#------------------------------------------------------------------------------#
# Proyecto:                   Presencia criminal 2007-2022
# Objetivo:                   Limpiar y unir bases 
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          20 de junio de 2023
# Última actualización:       20 de junio de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión 2023

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, zoo, ggtext, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}


# 1. Cargar datos --------------------------------------------------------------

# Bases
crudo1 <- read_xlsx(paste_inp("mapping_2.xlsx"))
crudo2 <- read_xlsx(paste_inp("2023. presencia_criminal.xlsx"))


# Limpieza de nombres de variables
g8 <- crudo1 %>% 
  group_by(year,state_name) %>% 
  unique() %>% 
  select("fuente" = "fuente",
         "anio"   = "year",
         "estado" = "state_name",
         "id"     = "state_code",
         "grupo"  = "group_stable")


# Se unen bases 
base_s <- crudo2 %>% bind_rows(g8)
