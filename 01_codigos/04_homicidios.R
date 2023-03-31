#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Homicidios 
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          24 de febrero   de 2023
# Última actualización:       21 de marzo   de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, beepr)

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos---------------------------------------------------------------
df_hom_ene_feb <- read_xlsx(paste_inp("Monitor_PPD_01_02.23.xlsx"))
df_hom_marz <- read_xlsx(paste_inp("Monitor_PPD_03.23.xlsx"))

# 2. Limpieza de los datos------------------------------------------------------
## Datos de homicidios en octubre-----------------------------------------------

# Se pegan enero-febrero y marzo 2023
df_hom <- df_hom_ene_feb %>% 
  bind_rows(df_hom_marz) 

# Limpieza de los nombres & selección de variables
df_hom2 <- df_hom %>% 
  janitor::clean_names() %>% 
  select("publicacion" = "x1_2_1_fecha_publicacion",
         "estado"      = "x1_3_2_estado",
         "homicido"    = "x2_2_1_homicidios_total") %>% 
  # normalización de la fecha a mes 
  mutate(mes = lubridate::month(publicacion, label = TRUE))

# Numero total de homicidios: 3787
sum(df_hom2$homicido)

# Homicidios agrupados por fecha
homicidios_ <- df_hom2 %>% 
  select(publicacion, homicido) %>% 
  group_by(publicacion) %>% 
  summarise(total_homicidios = sum(homicido, na.rm = T))

# Verificación
sum(homicidios_$total_homicidios) # Homicidios: 3787

# Guardar base
openxlsx::write.xlsx(homicidios_, file = paste_out("homicidios_.xlsx"), overwrite = T)

