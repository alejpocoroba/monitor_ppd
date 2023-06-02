#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Monitor para PPData
#
# Encargado:                  Alejandro Pocoroba 
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          30 de mayo de 2023
# Última actualización:       30 de mayo de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión 2022 y enero-marzo 2023

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

# bases de enero, febrer y marzo 
m1 <- read_xlsx(paste_inp("03.- FINAL2022_3.xlsx"))
m2 <- read_xlsx(paste_inp("03.- FINAL2023_3.xlsx"))

# 2. Procesamiento -------------------------------------------------------------
class(m1$CVEGEO_ent)
class(m2$CVEGEO_ent)

m2$CVEGEO_ent <- as.numeric(m2$CVEGEO_ent)
m2$CVEGEO_mun <- as.numeric(m2$CVEGEO_mun)
m2$CVEGEO     <- as.numeric(m2$CVEGEO)

# Se unen bases 
a <- m1 %>% bind_rows(m2)

# Se ordena base
colnames(a)

b <- a %>% 
  select("id":"CVEGEO",
         "homic_total", "homic_hombre", "homic_mujer", "homic_clasif1", "homic_clasif2",
         "cuerpos_localizados",
         "ataque1", "ataque2", "ataque3", "lugar_ataque1", "lugar_ataque2",
         "narcomensaje", 
         "autoridad1", "autoridad2", "autoridad3", "autoridad4", "autoridad5",
         "tipo_actividad_autoridad1", "tipo_actividad_autoridad2", "tipo_actividad_autoridad3")

# limpieza 
taxi <- c("chofer", "chofer/taxi", "taxi", "taxi/chofer")
conductor <- c("motociclista", "ciclista")

b <- b %>% 
  mutate(homic_clasif1 = 
           case_when(homic_clasif1    %in% taxi              ~ "taxi/chofer",
                     homic_clasif1    ==   homic_clasif1     ~ homic_clasif1), 
         homic_clasif2 = 
           case_when(homic_clasif2    %in% taxi              ~ "taxi/chofer",
                     homic_clasif2    ==   homic_clasif2     ~ homic_clasif2))

b <- b %>% 
  mutate(homic_clasif1 = 
           case_when(homic_clasif1    %in% conductor         ~ "motociclista/ciclista",
                     homic_clasif1    ==   homic_clasif1     ~ homic_clasif1), 
         homic_clasif2 = 
           case_when(homic_clasif2    %in% conductor         ~ "motociclista/ciclista",
                     homic_clasif2    ==   homic_clasif2     ~ homic_clasif2))
                     

# base limpia
openxlsx::write.xlsx(b, 
                     file = paste_out("monitor.2022_2023.xlsx"))


 





 