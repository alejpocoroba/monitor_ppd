#------------------------------------------------------------------------------#
# Proyecto:                   BACRIM 2020
# Objetivo:                   Cortejar y corroborar nombres - actualización
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          27 de junio de 2023
# Última actualización:       27 de junio de 2023
#------------------------------------------------------------------------------#

# Fuente: BACRIM 2020

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
crudo <- read_xlsx(paste_inp("BACRIM2020-DB_a27.06.23.xlsx"))

# 2. Procesamiento--------------------------------------------------------------
# Separar los nombres de los grupos criminales en columnas independientes 
df_nombres <- crudo %>% 
  separate(aliados, sep = ";", c("aliado1", "aliado2", "aliado3",
                                 "aliado4", "aliado5", "aliado6",
                                 "aliado7", "aliado8", "aliado9",
                                 "aliado10")) %>% 
  separate(rivales, sep = ";", c("rival1", "rival2", "rival3",
                                 "rival4", "rival5"))


# formato largo 
# Aliados
df_aliados <- df_nombres %>% 
  select(id,grupo, estado, starts_with("aliado")) %>% 
  pivot_longer(
    cols = aliado1:aliado10,
    names_to = c("num_aliado"),
    values_to = "aliado") %>% 
  select(aliado) %>% 
  unique() %>% 
  drop_na()

# Rivales
df_rivales <- df_nombres %>% 
  select(id,grupo, estado, starts_with("rival")) %>% 
  pivot_longer(
    cols = rival1:rival5,
    names_to = c("num_rival"),
    values_to = "rival") %>% 
  select(rival) %>% 
  unique() %>% 
  drop_na()


# Bases con nombres únicas
grupo <- df_nombres %>% 
  select(grupo)

aliados <- df_aliados %>% 
  select(aliado)

rivales <- df_rivales %>% 
  select(rival)

# Lista de los nombres
v_grupo  <- unique(grupo$grupo)
v_aliado <- unique(aliados$aliado)
v_rival  <- unique(rivales$rival)


# Se hace una  base de las listas anteriores y limpieza de texto
df_al_rv <- data.frame(v_aliado = v_rival)


# Se compara el nombre de los grupos con las listas pasadas y datas
df_al_rv <- unique(df_al_rv$v_aliado)
v_faltantes <- df_al_rv[!(df_al_rv %in% v_grupo)]

v_faltantes # Ninguno 


# Fin--------------------------------------------------------------------------- 
beepr::beep(9)