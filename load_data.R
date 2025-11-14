
# Instalar paquetes necesarios... --------------------------------------------


# install.packages("data.table")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("httr")
# install.packages("rvest")
# install.packages("purrr")
# install.packages("sqldf")

# install.packages("tidyr")

# install.packages("sf")


library(data.table)
library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(purrr)

library(tidyr)

library(sf)




# Datos ADRH --------------------------------------------------------------


## EXTRACCION DATOS DEL ATLAS DE DISTRIBUCION DE RENTA DE HOGARES ---------


## PRUEBA DE LA FUNCION PARA LA PROVINCIA DE SORIA ------------------------

datos_soria.lst <- read_file_ADRH('Soria', 2022)

## Hacer una copia de seguridad para no machacar
datos_soria_copia.lst <- datos_soria.lst

#######datos_soria.lst <- datos_soria_copia.lst


## Obtengo una lista con los nombres de los df de la lista que descargada
variables_ADRH.lst <- c(names(datos_soria.lst))

## Creo una lista con los nombres de las variables pivote
variables_pivote_ADRH.lst <- c("Indicadores de renta media y mediana"
                            ,"Distribución por fuente de ingresos"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Distribución de la renta por unidad de consumo"
                            ,"Índice de Gini y Distribución de la renta P80/P20"
                            ,"Indicadores demográficos")



### Tratamiento de los datos del df, nos quedamos con lo que necesitamos ###

## Defino una lista vacia que contendrá los datos finales
## La idea es pivotar la tabla, es decir, en lugar de tener una columna con los 
## nombres de las variables y valores para todas esas variables en otro campo, obtener
## una columna para cada variable con sus valores correspondientes

datos_soria_pivot.lst <- list()


## Itero sobre los df de la lista que he descargado: hago en todos lo mismo 

for(i in 1:length(variables_ADRH.lst)){
  
  ## Elimino las columnas que no necesito
  ## En este caso el campo Periodo
  datos_soria.lst[[ variables_ADRH.lst[i] ]] <- datos_soria.lst[[ variables_ADRH.lst[i] ]] %>%
    select(-"Periodo")
  
  
  ## Borro todas las filas para las que alguna de las columnas: 
  ##   distritos o secciones esten vacias
  datos_soria.lst[[ variables_ADRH.lst[i] ]] <- datos_soria.lst[[ variables_ADRH.lst[i] ]] %>%
    filter(!(Secciones == "" | is.na(Secciones) |
               Distritos == "" | is.na(Distritos)))
  
  ## Creo un df intermedio que facilita la escritura y lectura del código
  aux.df <- datos_soria.lst[[ variables_ADRH.lst[i] ]]
  
  ## Traspongo/pivoto los valores de la variable 
  datos_soria_pivot.lst[[ variables_ADRH.lst[i] ]]<- aux.df %>%
    pivot_wider(names_from = variables_pivote_ADRH.lst[i], values_from = Total)
  
  ## Defino las variables que utilizare para la integracion del tablón final
  datos_soria_pivot.lst[[ variables_ADRH.lst[i] ]] <-datos_soria_pivot.lst[[ variables_ADRH.lst[i] ]] %>%
    mutate(CO_SEC = substr(Secciones, 8, 10),
           CO_DIS = substr(Distritos, 6, 7),
           CO_MUN = substr(Municipios, 3, 5),
           CO_PRO = substr(Municipios, 1, 2),
           NOM_MUN = substring(Municipios, 7),
           co_seccion = substr(Secciones, 1, 10))
  
}


# Datos Censo -------------------------------------------------------------

## EXTRACCION DATOS DEL CENSO ---------------------------------------------

## PRUEBA DE LA FUNCION PARA LA PROVINCIA DE SORIA ------------------------
censo_soria.lst <- read_file_censo('Soria', 2022)


## Hacer una copia de seguridad para no machacar
censo_soria_copia.lst <- censo_soria.lst

#######censo_soria.lst <- censo_soria_copia.lst


## Obtengo una lista con los nombres de los df de la lista descargada
variables_CENSO.lst <- c(names(censo_soria.lst))

## Creo una lista con los nombres de las variables pivote
## ¡¡ EN ESTE CASO NO ES NECESARIO, PUES CADA DF DE LA LISTA CONTIENE UNA 
##    ÚNICA VARIABLE


### Tratamiento de los datos del df, nos quedamos con lo que necesitamos ###
for(i in 1:length(variables_CENSO.lst)){
  
  ## Elimino las columnas que no necesito
  ## En este caso el campo Periodo
  censo_soria.lst[[ variables_CENSO.lst[i] ]] <- censo_soria.lst[[ variables_CENSO.lst[i] ]] %>%
    select(-"Periodo")
  
  
  ## Borro todas las filas para las que alguna de las columnas: 
  ##   distritos o secciones esten vacias
  censo_soria.lst[[ variables_CENSO.lst[i] ]] <- censo_soria.lst[[ variables_CENSO.lst[i] ]] %>%
    filter(!(Secciones == "" | is.na(Secciones) |
               Municipios == "" | is.na(Municipios)))

  ## ¡¡ CAMBIAR NOMBRE DE LA COLUMNA TOTAL POR LOS NOMBRES DE LAS VARIABLES !!
  
  ## Creación de nuevas variables que servirán para la integración de las distintas
  ## fuentes de datos
  censo_soria.lst[[ variables_CENSO.lst[i] ]] <- censo_soria.lst[[ variables_CENSO.lst[i] ]] %>%
    mutate(CO_SEC = substr(Secciones, 8, 10),
           CO_DIS = substr(Secciones, 6, 7),
           CO_MUN = substr(Municipios, 3, 5),
           CO_PRO = substr(Municipios, 1, 2),
           NOM_MUN = substring(Municipios, 7),
           co_seccion = substr(Secciones, 1, 10))
  
}




# Informacion Secciones Censales (base)----------------------------------------


## LECTURA ARCHIVO .shp -------------------------------------------------------

## Ruta del archivo .shp
ruta <- '/Users/isabelconesahernandez/Documents/TFM/codigo-TFM/seccionado_2022/SECC_CE_20220101.shp'


## Lectura del archivo
datos_mapa <- st_read(ruta)

## Filtro por la provincia de Soria
mapa_soria <- datos_mapa %>%
  filter(CPRO == "42") 

## Comprobamos el número de secciones censales que tenemos:
nrow(mapa_soria)

## Nos salen 216




