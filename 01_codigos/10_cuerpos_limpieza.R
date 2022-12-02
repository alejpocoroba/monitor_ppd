#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Limpieza cuerpos
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          25 de noviembre de 2022
# Última actualización:       25 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

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

# Noviembre
m10 <- read_xlsx(paste_inp("Monitor2_PPD_noviembre25.11.xlsx"))

# Limpieza de datos 
cuerpos <- m10 %>% 
  janitor::clean_names() %>% 
  select(cuerpos_casilla = x2_2_5_cuerpo_s_localizado_s_restos_humanos, 
         forma_cuerpo    = x2_2_6_cuerpo_localizado_restos_humanos, 
         lugar_cuerpo    = x2_2_7_lugar_del_cuerpo_localizado_restos_humanos) %>% 
  filter(cuerpos_casilla == "TRUE") 

cuerpos2 <- data.frame(count(cuerpos, forma_cuerpo, sort = T) 
                       %>% drop_na()) %>% rename(frecuencia = n)

openxlsx::write.xlsx(cuerpos2, file = paste_out("formas_cuerpo.xlsx"), overwrite = T)            

cuerpos3 <- data.frame(count(cuerpos, lugar_cuerpo, sort = T) 
                       %>% drop_na()) %>% rename(frecuencia = n)

openxlsx::write.xlsx(cuerpos3, file = paste_out("lugar_cuerpo.xlsx"), overwrite = T)     

