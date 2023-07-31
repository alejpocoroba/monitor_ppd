#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Reporte de desempeño: captura de notas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          01 de agosto de 2023
# Última actualización:       01 de agosto de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión abril - junio 2023

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
c <- read_xlsx(paste_inp("monitor_abril_2023.xlsx"))
b <- read_xlsx(paste_inp("monitor_junio_2023.xlsx"))
d <- read_xlsx(paste_inp("monitor_marzo_2023.xlsx"))

# Se unen bases 
a <- c %>% 
  bind_rows(b) %>% 
  bind_rows(d)

# 2. Procesamiento -------------------------------------------------------------

# Limpieza de nombres y variables de interés
crudo <- a %>% 
  janitor::clean_names() %>% 
  select(estado:observaciones)

# Tratamiento de fechas #
crudo$"fecha" <- as.Date(crudo$"fecha")
crudo$dia     <- format(crudo$"fecha", "%d")
crudo$mes     <- format(crudo$"fecha", "%m")
crudo$anio    <- format(crudo$"fecha", "%Y")

# Estado según su clave de Inegi 
crudo <- crudo %>% 
  mutate(
    claveEdo = str_sub(estado, -2, -1))

# Limpiar T/F por 1/0
crudo$violencia_armade_fuego <- as.character(crudo$violencia_armade_fuego)
crudo$homicidios <- as.character(crudo$homicidios)
crudo$heridxs <- as.character(crudo$heridxs)
crudo$genero <- as.character(crudo$genero)
crudo$presencia_criminal <- as.character(crudo$presencia_criminal)
crudo$actividades_delictivas <- as.character(crudo$actividades_delictivas)
crudo$detenciones <- as.character(crudo$detenciones)
crudo$acciones_gubernamentales <- as.character(crudo$acciones_gubernamentales)
crudo$politica_seguridad <- as.character(crudo$politica_seguridad)


limpiar_tf <- function(x){
  
  case_when(
    x == "FALSE" ~ "0",
    x == "TRUE"  ~ "1",
    x == x ~ x
  )
}

crudo <- crudo %>% 
  mutate_at(.vars = c("violencia_armade_fuego", "homicidios", "heridxs",
                      "genero", "presencia_criminal", "actividades_delictivas",
                      "detenciones", "acciones_gubernamentales", "politica_seguridad"),
            .funs = ~limpiar_tf(.))

# Limpieza de texto 
# minúsculas
crudo <- crudo %>% 
  mutate_at(.vars = c("otra", "observaciones"), 
            .funs = ~str_to_lower(.))

# acentos
crudo$otra          <- stri_trans_general(crudo$otra, "Latin-ASCII")
crudo$observaciones <- stri_trans_general(crudo$observaciones, "Latin-ASCII")

# Limpieza variable otra 
# separar valores 
crudo_otra1 <- crudo %>%
  mutate(otra = str_replace_all(otra, "/", ";"),
         otra = str_replace_all(otra, ":", ";")) %>% 
  separate(otra, sep = ";", c("otra1", "otra2","otra3"))

v_menoredad        <- c()
v_desparecidxs     <- c()
v_abusodeautoridad <- c("tortura policiaca")
v_incendio         <- c("quema de auto")
v_secuestro        <- c("levanton", "victima fue levantado", "robo y secuestro",
                        "secuestro de ninxs")
v_traflorfau       <- c("madera ilegal")


# 4. ID y orden 
df_act <- df_act %>% 
  mutate(id = 1:length(df_act$Mes))








