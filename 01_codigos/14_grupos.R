#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Limpieza Monitor 2023
#
# Encargados:                 Alejandro Pocoroba y Erick Morales 
# Correos:                    alejandro.pocoroba@cide.edu
			      erick.morales@cide.edu 
# Fecha de creación:           de 2023
# Última actualización:        de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD enero-marzo 2023

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, stringr, zoo, ggtext, beepr, mxmaps)

library(mxmaps) # Para los mapas

# Limpiar espacio de trabajo 
rm(list=ls())

# 1. Cargar datos --------------------------------------------------------------
df_crudo <- read_xlsx(paste_inp("Monitor_PPD_01_02.23.xlsx"))

## 2.1. Funciones de limpieza --------------------------------------------------
# limpieza de las variables de interés
df_crudo <- df_crudo %>% 
  janitor::clean_names() %>% 
  rename("fecha_de_publicacion"  = "x1_2_1_fecha_publicacion",
         "enlace"                = "x1_2_2_enlace",
         "estado"                = "x1_3_2_estado",
         "municipio"             = "x1_3_3_municipio",
         "grupo_criminal"        = "x3_3_grupo_criminal",
         "alianza"               = "x3_3_1_alianza_grupo",
         "rival"                 = "x3_3_2_rival_grupo")  %>% 
  # Agregar variable de mes para facilitar filtros de fechas 
  mutate(mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  select(fecha_de_publicacion, enlace, estado, municipio,
         grupo_criminal, alianza, rival)


df_gc <- df_gc %>% 
  mutate(grupo1   = limpiar_grupos(grupo1),
         grupo2   = limpiar_grupos(grupo2),
         alianza1 = limpiar_grupos(alianza2),
         alianza2 = limpiar_grupos(alianza2),
         alianza3 = limpiar_grupos(alianza3),
         alianza4 = limpiar_grupos(alianza4),
         alianza5 = limpiar_grupos(alianza5),
         rival1   = limpiar_grupos(rival1),
         rival2   = limpiar_grupos(rival2),
         rival3   = limpiar_grupos(rival3))