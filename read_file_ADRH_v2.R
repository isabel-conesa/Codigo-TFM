#' Download data from the Household Income Distribution Atlas (ADRH) from INE
#'
#' This function downloads data from the Household Income Distribution Atlas 
#' of the Spanish National Statistics Institute (INE) for a specific province.
#' It allows filtering by year and indicator types, and returns a list of 
#' data tables with the requested indicators.
#'
#' @param province_name Name of the province for which data will be downloaded.
#'   Must be a character of length 1. The name must exactly match the 
#'   denomination used by INE (e.g., 'Albacete', 'Madrid', 'Barcelona').
#' @param year Year or years to filter the data. Can be numeric or character.
#'   If NULL (default), all available years are downloaded.
#' @param indicators Character vector with the types of indicators to download.
#'   If NULL (default), all available indicators are downloaded.
#'   Common indicators include: 'Percentage', 'Distribution', 'Indicators', 
#'   'Gini Index'. The function uses pattern matching on indicator names.
#' @param sep Field separator for CSV files. Default is ";".
#' @param url_ADRH Base URL for the ADRH data portal. 
#'   Default is "https://www.ine.es/dynt3/inebase/index.htm?padre=12385&capsel=7132".
#' @param timeout Timeout in seconds for HTTP requests. Default is 30 seconds.
#' @param verbose Logical indicating whether to display progress messages. 
#'   Default is TRUE.
#'
#' @return A named list of data.table objects, where each element corresponds to 
#'   one indicator. Each data table contains the downloaded data with the 
#'   following attributes:
#'   \itemize{
#'     \item \code{indicator}: Name of the indicator
#'     \item \code{province}: Province name
#'     \item \code{year}: Requested years (if any)
#'   }
#'   The list itself has additional attributes:
#'   \itemize{
#'     \item \code{province}: Province name
#'     \item \code{year}: Requested years
#'     \item \code{indicators}: Names of successfully downloaded indicators
#'     \item \code{download_date}: Timestamp of download
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' albacete_data <- read_file_ADRH(province_name = 'Albacete', year = "2022")
#'
#' # With specific indicators
#' albacete_filtered <- read_file_ADRH(
#'   province_name = 'Albacete', 
#'   year = "2022",
#'   indicators = c("Percentage", "Distribution")
#' )
#'
#' # Multiple years
#' albacete_multi_year <- read_file_ADRH(
#'   province_name = 'Albacete', 
#'   year = c("2020", "2021", "2022")
#' )
#'
#' # All available years
#' albacete_complete <- read_file_ADRH(province_name = 'Albacete')
#'
#' # Advanced options
#' albacete_detailed <- read_file_ADRH(
#'   province_name = 'Albacete',
#'   year = "2022",
#'   indicators = c("Percentage", "Distribution"),
#'   verbose = TRUE,
#'   timeout = 60
#' )
#' }
#'
#' @export
#' @import data.table
#' @import dplyr
#' @import stringr
#' @import httr
#' @import rvest
#' @import purrr
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
  indicator_patterns <- c('^Porcentaje', '^Distribución', '^Indicadores', "^Índice de Gini")
  
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
  
  # MODIFIED FUNCTION: Improved CSV download and processing with better error handling
  download_and_process_csv <- function(csv_url, indicator_name) {
    tryCatch({
      log_msg("Downloading: ", indicator_name)
      
      # Download and clean blank lines
      response <- GET(csv_url, timeout(timeout))
      content_text <- content(response, "text", encoding = "UTF-8")
      lines <- unlist(str_split(content_text, "\r?\n"))
      clean_lines <- lines[lines != "" & str_trim(lines) != ""]
      clean_content <- paste(clean_lines, collapse = "\n")
      
      # SOLUCIÓN AL PROBLEMA: Múltiples estrategias para leer el CSV
      dt <- NULL
      
      # Estrategia 1: Intentar con fread con parámetros más permisivos
      try({
        dt <- fread(
          text = clean_content, 
          sep = sep, 
          encoding = "UTF-8",
          quote = "",           # Deshabilitar comillas
          fill = TRUE,          # Rellenar columnas faltantes
          blank.lines.skip = TRUE,  # Saltar líneas en blanco
          showProgress = FALSE
        )
      }, silent = TRUE)
      
      # Estrategia 2: Si falla, intentar con read.csv
      if (is.null(dt) || nrow(dt) == 0) {
        try({
          # Crear archivo temporal
          temp_file <- tempfile(fileext = ".csv")
          writeLines(clean_content, temp_file, useBytes = TRUE)
          
          dt <- fread(
            temp_file,
            sep = sep,
            encoding = "UTF-8",
            quote = "",
            fill = TRUE,
            blank.lines.skip = TRUE
          )
          
          # Limpiar archivo temporal
          unlink(temp_file)
        }, silent = TRUE)
      }
      
      # Estrategia 3: Si sigue fallando, usar readLines y manual parsing
      if (is.null(dt) || nrow(dt) == 0) {
        try({
          # Parseo manual básico
          data_lines <- clean_lines
          if (length(data_lines) > 1) {
            # Extraer encabezados
            headers <- str_split(data_lines[1], sep, simplify = TRUE)
            # Procesar datos
            data_list <- lapply(data_lines[-1], function(line) {
              str_split(line, sep, simplify = TRUE)
            })
            
            # Crear data.table manualmente
            if (length(data_list) > 0) {
              dt <- as.data.table(do.call(rbind, data_list))
              if (ncol(dt) == length(headers)) {
                setnames(dt, headers)
              }
            }
          }
        }, silent = TRUE)
      }
      
      # Verificar que se pudo leer algún dato
      if (is.null(dt) || nrow(dt) == 0) {
        log_msg("✗ Could not parse data for: ", indicator_name)
        return(NULL)
      }
      
      # Filter by year if specified
      if (!is.null(year)) {
        year_chr <- as.character(year)
        # Buscar columnas que puedan contener años
        year_cols <- c("Periodo", "Año", "Year", "Period", "Fecha")
        available_cols <- names(dt)
        
        for (col in year_cols) {
          if (col %in% available_cols) {
            dt <- dt[get(col) %in% year_chr]
            break
          }
        }
      }
      
      # Add metadata
      attr(dt, "indicator") <- indicator_name
      attr(dt, "province") <- province_name
      attr(dt, "year") <- year
      
      log_msg("✓ ", indicator_name, " - ", nrow(dt), " rows, ", ncol(dt), " columns")
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
