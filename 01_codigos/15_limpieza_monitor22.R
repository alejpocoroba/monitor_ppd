#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD enero-marzo 2022
# Objetivo:                   Limpieza: cuerpos y grupos criminales
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          02 de marzo de 2023
# Última actualización:       05 de abril de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

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

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1.- Cargar datos --------------------------------------------------------------
df_crudo <- read_xlsx(paste_inp("Monitor_PPD_01_02.23.xlsx"))

## 2.1. Funciones de limpieza --------------------------------------------------
# limpieza de las variables de interés
df_cuerpos <- df_crudo %>% 
  janitor::clean_names() %>% 
  rename("fecha_de_publicacion"     = "x1_2_1_fecha_publicacion",
         "enlace"                   = "x1_2_2_enlace",
         "estado"                   = "x1_3_2_estado",
         "municipio"                = "x1_3_3_municipio",
         "cuerpo_localizado"        = "x2_2_5_cuerpo_restos",
         "cuerpo_modo"              = "x2_2_6_cuerpo_restos_localizados",
         "cuerpo_lugar"             = "x2_2_7_lugar_cuerpo_restos")  %>% 
  # Agregar variable de mes para facilitar filtros de fechas 
  mutate(mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  select(fecha_de_publicacion, enlace, estado, municipio,
         cuerpo_localizado, cuerpo_modo, cuerpo_lugar)


# cambiar F/T por 0/1
df_cuerpos$cuerpo_localizado <- as.character(df_cuerpos$cuerpo_localizado)

df_cuerpos <- df_cuerpos %>%
  mutate(cuerpo_localizado = 
           case_when(cuerpo_localizado == "FALSE" ~ "0",
                     cuerpo_localizado == "TRUE"  ~ "1",
                     cuerpo_localizado == cuerpo_localizado ~ cuerpo_localizado))

# clasificaciones 

v_armadefuego <- c("huellas de tortura/ impacto de armas de fuego", "Impactos de arma de fuego", 
                   "Huellas de tortura / cuerpo maniatado", "impactos de arma de fuego; bolsa de plástico",
                   "impacto de arma de fuego; maniatado", "maniatado; impactos de bala", "impactos de arma de fuego; tortura",
                   "impactos de arma de fuego; calcinado")

v_tortura_maniatado <- c("maniatado", "Huellas de violencia; soga en el cuello", "huellas de violencia",
                         "colgado", "huellas de tortura; calcinado", "lesión en el cuello", "colgado; golpes", "ahorcado", "maniatado; encobijado") 

v_plastico_encobijado <- c("envuelto en una lona", "Bolsas de plástico / encobijado", 
                           "bolsas de plástico", "encobijado", "Tambo de agua, bolsa de plastico, huellas de violencia",
                           "estrangulado; encobijado", "bolsas de plástico; cuerpos en descomposición", "huellas de violencia; bolsa de plastico",
                           "bolsa de plástico; maniatado")

v_descomposicion <- c("restos óseos", "Osamenta", "Descomposición / restos óseos / enterrado / fosas", "restos humanos", "Descomposición",
                      "bolsa de plástico; fosa")

v_calcinado <- c("Calcinado")

v_desmembrado <- c("Desmembrado / alguna parte del cuerpo", "decapitado", "degollada", 
                   "desmembrado", "desmembrado; bolsa de plástico", "cabeza", "pie", "degollado", "desmembrado; encobijado", "maleta")

v_otro <- c("golpes", "encajuelados", "enterrado", "cubierto con material",
            "golpe en la cabeza", "arma blanca", "Otro", "estrangulado",
            "Heridas arma blanca", "Tambo de agua")

v_sinformación <- c("ojos vendados", "sin información", "cuerpo completo")


df_cuerpos <- df_cuerpos %>% 
  mutate(cuerpo_modo = 
           case_when(cuerpo_modo %in% v_armadefuego          ~ "Impactos de arma de fuego",
                     cuerpo_modo %in% v_tortura_maniatado    ~ "Huellas de tortura / cuerpo maniatado",
                     cuerpo_modo %in% v_plastico_encobijado  ~ "Bolsas de plástico / encobijado",
                     cuerpo_modo %in% v_descomposicion       ~ "Descomposición / restos óseos / enterrado / fosas",
                     cuerpo_modo %in% v_calcinado            ~ "Calcinado",
                     cuerpo_modo %in% v_desmembrado          ~ "Desmembrado / alguna parte del cuerpo",
                     cuerpo_modo %in% v_otro                 ~ "Otro",
                     cuerpo_modo %in% v_sinformación         ~ "Sin información",
                     cuerpo_modo == cuerpo_modo ~ cuerpo_modo))

unique(df_cuerpos$cuerpo_modo)























