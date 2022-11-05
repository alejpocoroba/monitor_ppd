#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Total de homicidios
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          04 de noviembre de 2022
# Última actualización:       04 de noviembre de 2022
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
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza de los datos------------------------------------------------------
## Datos de homicidios en octubre-----------------------------------------------
# Fecha de los hechos 
homicidios_octubre <- df_monitor_amplio %>% 
  filter(fecha_de_los_hechos >= "2022-10-01") %>% 
  filter(fecha_de_los_hechos <= "2022-10-31")

sum(homicidios_octubre$numero_de_homicidios_total) # Homicidios: 1,500
sum(homicidios_octubre$numero_de_homicidios_hombre, na.rm = T) # Ho - Hombre: 1233
sum(homicidios_octubre$numero_de_homicidios_mujer, na.rm = T) # Ho - mujer: 149
# No identificados: 118 

homicidios_octubre2 <- homicidios_octubre %>% 
  select(fecha_de_los_hechos, numero_de_homicidios_total,
         numero_de_homicidios_hombre, numero_de_homicidios_mujer) %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T),
            hombres = sum(numero_de_homicidios_hombre, na.rm = T),
            mujer = sum(numero_de_homicidios_mujer, na.rm = T))

# Verificación
sum(homicidios_octubre2$total_homicidios) # Homicidios: 1,500
sum(homicidios_octubre2$hombres) # Ho - Hombre: 1233
sum(homicidios_octubre2$mujer) # Ho - mujer: 149

openxlsx::write.xlsx(homicidios_octubre2, file = paste_out("homicidios_octubre.xlsx"), overwrite = T)

# Fecha de publicación
homicidios_octubre3 <- df_monitor_amplio %>% 
  filter(fecha_de_publicacion >= "2022-10-01") %>% 
  filter(fecha_de_publicacion <= "2022-10-31")

sum(homicidios_octubre3$numero_de_homicidios_total) # Homicidios: 1881
sum(homicidios_octubre3$numero_de_homicidios_hombre, na.rm = T) # Ho - Hombre: 1561
sum(homicidios_octubre3$numero_de_homicidios_mujer, na.rm = T) # Ho - mujer: 191
# No identificados: 129

homicidios_octubre4 <- homicidios_octubre3 %>% 
  select(fecha_de_publicacion, numero_de_homicidios_total,
         numero_de_homicidios_hombre, numero_de_homicidios_mujer) %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T),
            hombres = sum(numero_de_homicidios_hombre, na.rm = T),
            mujer = sum(numero_de_homicidios_mujer, na.rm = T))

# Verificación
sum(homicidios_octubre4$total_homicidios) # Homicidios: 1881
sum(homicidios_octubre4$hombres) # Ho - Hombre: 1561
sum(homicidios_octubre4$mujer) # Ho - mujer: 191

openxlsx::write.xlsx(homicidios_octubre4, file = paste_out("homicidios_octubre.xlsx"), overwrite = T)


