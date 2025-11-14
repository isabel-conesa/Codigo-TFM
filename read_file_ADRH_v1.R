
################################################################################
##      EXTRACCION DATOS DEL ATLAS DE DISTRIBUCION DE RENTA DE HOGARES        ##
################################################################################


## DESCARGA DE PAQUETES NECESARIOS EN EL PROCESO 

# install.packages("data.table")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("httr")
# install.packages("rvest")
# install.packages("purrr")
# install.packages("sqldf")


library(data.table)
library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(purrr)


## Funcion para descargar datos de la web del INE
## En este caso, adaptada a datos del ADRH

read_file_ADRH <- function(province_name, 
                           year = NULL, 
                           indicators = NULL,
                           sep = ";", 
                           url_ADRH = "https://www.ine.es/dynt3/inebase/index.htm?padre=12385&capsel=7132",
                           timeout = 30,
                           verbose = TRUE) {
  
  # Validations
  stopifnot(length(province_name) == 1)
  if (!is.null(year)) stopifnot(is.character(year) | is.numeric(year))
  if (!is.null(indicators)) stopifnot(is.character(indicators))
  
  # Internal function for logging
  log_msg <- function(...) if (verbose) cat(..., "\n")
  
  # Optimized function to get CSV URL
  get_csv_url <- function(file_url) {
    file_id <- str_extract(file_url, "(?<=t=)\\d+")
    if (is.na(file_id)) return(NA_character_)
    paste0("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/", file_id, ".csv?nocab=1")
  }
  
  # Function to read and process web page
  read_webpage <- function(url) {
    tryCatch({
      response <- GET(url, 
                      add_headers('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'),
                      timeout(timeout))
      content(response, "text", encoding = "UTF-8") %>% read_html()
    }, error = function(e) {
      log_msg("Error reading page: ", e$message)
      NULL
    })
  }
  
  # Function to extract links from page
  extract_links <- function(page, base_URL, filter_text = NULL, text_patterns = NULL) {
    if (is.null(page)) return(data.frame())
    
    links_df <- page %>%
      html_nodes("a") %>%
      map_df(~{
        text_content <- html_text(.x, trim = TRUE) %>% 
          iconv(from = "UTF-8", to = "UTF-8", sub = "")
        data.frame(
          text = text_content,
          url = html_attr(.x, "href"),
          stringsAsFactors = FALSE
        )
      }) %>%
      filter(!is.na(url), url != "", text != "")
    
    # Apply filters if specified
    if (!is.null(filter_text)) {
      links_df <- links_df %>% filter(text == filter_text)
    }
    
    if (!is.null(text_patterns)) {
      links_df <- links_df %>% 
        filter(str_detect(text, paste(text_patterns, collapse = "|")))
    }
    
    # Build complete URLs
    links_df$complete_url <- ifelse(
      str_detect(links_df$url, "^https?://"),
      links_df$url,
      paste0(base_URL, links_df$url)
    )
    
    links_df
  }
  
  log_msg("Searching for province: ", province_name)
  
  # STEP 1: Get province URL
  province_page <- read_webpage(url_ADRH)
  province_links <- extract_links(province_page, 
                                  base_URL = "https://www.ine.es/dynt3/inebase/index.htm", 
                                  filter_text = province_name)
  
  if (nrow(province_links) == 0) {
    stop("Province '", province_name, "' not found")
  }
  
  province_url <- province_links$complete_url[1]
  log_msg("Found URL for ", province_name, ": ", province_url)
  
  # STEP 2: Get indicator links for the province
  indicators_page <- read_webpage(province_url)
  
  # Patterns to filter indicators (improved)
  indicator_patterns <- c('^Porcentaje', '^Distribución', '^Indicador', "^Índice de Gini")
  
  indicator_links <- extract_links(indicators_page, 
                                   base_URL = "https://www.ine.es", 
                                   text_patterns = indicator_patterns)
  
  if (nrow(indicator_links) == 0) {
    stop("No indicators found for province ", province_name)
  }
  
  # Filter specific indicators if requested
  if (!is.null(indicators)) {
    indicator_links <- indicator_links %>%
      filter(str_detect(text, paste(indicators, collapse = "|")))
    
    if (nrow(indicator_links) == 0) {
      stop("Specified indicators not found: ", paste(indicators, collapse = ", "))
    }
  }
  
  # Generate CSV URLs
  indicator_links$csv_url <- map_chr(indicator_links$complete_url, get_csv_url)
  indicator_links <- indicator_links %>% filter(!is.na(csv_url))
  
  log_msg("Found ", nrow(indicator_links), " indicators: ", 
          paste0(indicator_links$text, collapse = ", "))
  
  # Function to download and process a CSV
  download_and_process_csv <- function(csv_url, indicator_name) {
    tryCatch({
      log_msg("Downloading: ", indicator_name)
      
      # Download and clean blank lines
      response <- GET(csv_url, timeout(timeout))
      content_text <- content(response, "text", encoding = "UTF-8")
      lines <- unlist(str_split(content_text, "\r?\n"))
      clean_lines <- lines[lines != "" & str_trim(lines) != ""]
      clean_content <- paste(clean_lines, collapse = "\n")
      
      # Read data
      dt <- fread(text = clean_content, sep = sep, encoding = "UTF-8")
      
      # Filter by year if specified
      if (!is.null(year)) {
        if ("Periodo" %in% names(dt)) {
          dt <- dt[Periodo %in% as.character(year)]
        } else if ("Year" %in% names(dt)) {
          dt <- dt[Periodo %in% as.character(year)]
        }
      }
      
      # Add metadata
      attr(dt, "indicator") <- indicator_name
      attr(dt, "province") <- province_name
      attr(dt, "year") <- year
      
      log_msg("✓ ", indicator_name, " - ", nrow(dt), " rows")
      return(dt)
      
    }, error = function(e) {
      log_msg("✗ Error in ", indicator_name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Download all indicators
  log_msg("Downloading indicators...")
  files_list <- map2(indicator_links$csv_url, 
                     indicator_links$text, 
                     download_and_process_csv)
  
  # Remove null elements and assign names
  names(files_list) <- indicator_links$text
  files_list <- compact(files_list)
  
  # Add attributes to final result
  attr(files_list, "province") <- province_name
  attr(files_list, "year") <- year
  attr(files_list, "indicators") <- names(files_list)
  attr(files_list, "download_date") <- Sys.time()
  
  log_msg("Download completed: ", length(files_list), " of ", 
          nrow(indicator_links), " indicators successful")
  
  return(files_list)
}



################################################################################
##             PRUEBA DE LA FUNCION PARA LA PROVINCIA DE SORIA                ##
################################################################################

datos_soria.lst <- read_file_ADRH('Soria', 2022)


### Tratamiento de los datos del df, nos quedamos con lo que necesitamos ###

## Elimino las columnas que no necesito
df_total <- df_total %>%
  select(-"Indicadores de renta media y mediana",-"Periodo")


## Borro todas las filas para las que alguna de las columnas: 
##   distritos o secciones esten vacias
df_total <- df_total %>%
  filter(!(Secciones == "" | is.na(Secciones) |
             Distritos == "" | is.na(Distritos)))


## Lista de todas las variables descargadas, en el orden en que aparecen en la web
## del INE cuando lo descargamos desde allí directamente
indicadores_value <- c("Renta neta media por persona", "Renta neta media por hogar",  
                       "Media de la renta por unidad de consumo", "Mediana de la renta por unidad de consumo",
                       "Renta bruta media por persona", "Renta bruta media por hogar")

df_total <- df_total %>%
  mutate(Indicadores = rep(indicadores_value, length.out = n()))



###  Creación de nuevas variables para facilitar la integración de ficheros  ###

## Creo la variable código de sección (facilitarán los cruces de datos)

df_total <- df_total %>%
  mutate(CO_SEC = substr(Secciones, 8, 10),
         CO_DIS = substr(Distritos, 6, 7),
         CO_MUN = substr(Municipios, 3, 5),
         CO_PRO = substr(Municipios, 1, 2),
         NOM_MUN = substring(Municipios, 7),
         co_seccion = substr(Secciones, 1, 10))


## Filtro para quedarme con los datos de las variables objeto de estudio:

## 1. Renta neta media por persona
renta_neta_media_pers <- df_total %>%
  filter(Indicadores == "Renta neta media por persona")

## 1. Renta neta media por hogar
renta_neta_media_hogar<- df_total %>%
  filter(Indicadores == "Renta neta media por hogar") 


## Comprobamos el número de secciones censales que tenemos:
nrow(renta_neta_media_pers)

## Nos salen 217





