#------------------------------------------------------------------------------#
# Proyecto:                   Redes criminales 2020
# Objetivo:                   Cortejar y corroborar nombres
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          24 de junio de 2023
# Última actualización:       24 de junio de 2023
#------------------------------------------------------------------------------#

# Fuente: BACRIM 2020: aliados y rivales

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, zoo, ggtext, beepr, lubridate, stringr, janitor, stringi)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos --------------------------------------------------------------

# bacrim aliados y rivales 
crudo <- read_xlsx(paste_inp("bacrim_redes.24.06.23.xlsx"))

# 2. Procesamiento--------------------------------------------------------------
# 2.- Comparar los grupos ya identificados con aliados-rivales
colnames(crudo)

# Bases con nombres únicas
grupo <- crudo %>% 
  select(grupo)

aliado <- crudo %>% 
  select(aliado)

rival <- crudo %>% 
  select(rival)

# Lista de los nombres
v_grupo  <- unique(grupo$grupo)
v_aliado <- unique(aliado$aliado)
v_rival  <- unique(rival$rival)


# Se hace una  base de las listas anteriores y limpieza de texto
df_al_rv <- data.frame(v_aliado = v_rival)


# Se compara el nombre de los grupos con las listas pasadas y datas
df_al_rv <- unique(df_al_rv$v_aliado)
v_faltantes <- df_al_rv[!(df_al_rv %in% v_grupo)]

v_faltantes # Ninguno 


# Fin--------------------------------------------------------------------------- 
beepr::beep(9)







