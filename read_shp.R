
## LECTURA DEL ARCHIVO .shp

## Instalar paquetes necesarios

# install.packages("sf")
library(sf)


## Ruta del archivo .shp
ruta <- '/Users/isabelconesahernandez/Documents/TFM/SCRIPTS/seccionado_2022/SECC_CE_20220101.shp'

datos_mapa <- st_read(ruta)



## Filtro por la provincia de Soria
mapa_soria <- datos_mapa %>%
  filter(CPRO == "42") 

## Comprobamos el número de secciones censales que tenemos:
nrow(mapa_Soria)

## Nos salen 216


