#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento de la información
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          25 de junio de 2022
# Última actualización:       25 de junio de 2022
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

m1 <- read_xlsx(paste_inp("Monitor PPD.xlsx"))

# 2. Limpar datos --------------------------------------------------------------

colnames(m2)

m2 <- m1 %>% 
  janitor::clean_names() %>% 
# construcción de identificadores 
  mutate(n_hechos = 
           (numero_de_homicidios_total > 0) + 
           (numero_de_heridos_as_total > 0) + 
           (numero_de_detenidos_as_total > 0) +
           (narcomensaje) +
           (privacion_de_la_libertad) +
           !is.na(otras_actividades_ilicitas)) %>% 
# 2.1 Casos repetidos 
  distinct(fecha_de_publicacion, fecha_de_los_hechos, municipio, n_hechos, 
           autoridad_militar, autoridad_civil, 
           nombre_del_grupo_criminal_gc, cuerpo_s_localizado_s, 
           numero_de_homicidios_total, numero_de_detenidos_as_total, ataque_armado,
           politica_de_seguridad, lugar,
           .keep_all = TRUE) 

v_unicos <- unique(m2$id)

df_eliminados <- m1 %>% 
  filter(!Id %in% v_unicos)

table(df_eliminados$Responsable)
