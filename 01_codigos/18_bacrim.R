#------------------------------------------------------------------------------#
# Proyecto:                   Redes criminales 2020
# Objetivo:                   Transformar estructura de la base: largo
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          22 de junio de 2023
# Última actualización:       22 de junio de 2023
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
crudo <- read_xlsx(paste_inp("00.- bacrim_limpieza_b6.xlsx"))

# 2. Procesamiento--------------------------------------------------------------

# variables de interes
crudo1 <- crudo %>% 
  select(id, grupo, estado, aliados, rivales) %>% 
  mutate(weight = "0")

# Número valores de aliados y rivales
# aliados
aliados <- crudo1 %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    aliados = str_replace_all(aliados, ":", ";"), 
    # Contar número de puntos y comas (;)
    aliados = str_count(aliados, ";")) %>% 
    select(aliados) 

max(aliados$aliados, na.rm = T) # El máximo es 9

# rivales
rivales <- crudo1 %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    rivales = str_replace_all(rivales, ":", ";"), 
    # Contar número de puntos y comas (;)
    rivales = str_count(rivales, ";")) %>% 
  select(rivales) 

max(rivales$rivales, na.rm = T) # El máximo es 5

# Separar los nombres de los grupos criminales en columnas independientes 
df_nombres <- crudo1 %>% 
  separate(aliados, sep = ";", c("aliado1", "aliado2", "aliado3",
                                 "aliado4", "aliado5", "aliado6",
                                 "aliado7", "aliado8", "aliado9")) %>% 
  separate(rivales, sep = ";", c("rival1", "rival2", "rival3",
                                 "rival4", "rival5"))

# formato largo 
# Aliados
df_aliados <- df_nombres %>% 
  select(id,grupo, estado, starts_with("aliado")) %>% 
  pivot_longer(
    cols = aliado1:aliado9,
    names_to = c("num_aliado"),
    values_to = "aliado") %>% 
  rename("weight" = "num_aliado") %>% 
  select(id,grupo, estado, aliado, weight) %>% 
  mutate("weight" = "1") %>% 
  unique() %>% 
  drop_na()

# Rivales
df_rivales <- df_nombres %>% 
  select(id,grupo, estado, starts_with("rival")) %>% 
  pivot_longer(
    cols = rival1:rival5,
    names_to = c("num_rival"),
    values_to = "rival") %>% 
  rename("weight" = "num_rival") %>% 
  select(id,grupo, estado, rival, weight) %>% 
  mutate("weight" = "-1") %>% 
  unique() %>% 
  drop_na()

# Pegar bases
bacrim <- full_join(df_aliados, df_rivales, 
                    by = c("id", "grupo", "estado", "weight")) 

bacrim <- bacrim %>% 
  select(id, grupo, estado, aliado, rival, weight)


# Limpieza de nombres 
# Nombres similares 
nombres <- function(x){
  
  case_when(
    x  == "Cártel de Sinaloa / Los Chapitos"  ~ "Los Chapitos",
    x  == " Cártel de Sinaloa / Los Chapitos" ~ "Los Chapitos",
    x  == "Cártel de Sinaloa / Los Chapitos " ~ "Los Chapitos",
    x  == "Cártel del Golfo Nueva Era"        ~ "Nueva Era Cártel del Golfo",
    x  == " Cártel del Golfo Nueva Era"       ~ "Nueva Era Cártel del Golfo",
    x  == "Cártel del Golfo Nueva Era "       ~ "Nueva Era Cártel del Golfo",
    x  == x ~ x
  )
}

bacrim_vf <- bacrim %>% 
  mutate_at(.vars = c("grupo", "aliado", "rival"), 
            .funs = ~nombres(.))

# minúsculas
bacrim_redes <- bacrim_vf %>% 
  mutate_at(.vars = c("grupo", "estado", "aliado", "rival"), 
            .funs = ~str_to_lower(.))

# acentos
bacrim_redes$grupo  <- stri_trans_general(bacrim_redes$grupo, "Latin-ASCII")
bacrim_redes$estado <- stri_trans_general(bacrim_redes$estado, "Latin-ASCII")
bacrim_redes$aliado <- stri_trans_general(bacrim_redes$aliado, "Latin-ASCII")
bacrim_redes$rival  <- stri_trans_general(bacrim_redes$rival, "Latin-ASCII")

 # 3. Base limpía----------------------------------------------------------------
openxlsx::write.xlsx(bacrim_redes, 
                     file = paste_out("bacrim_redes.xlsx"))


# Nota: de forma manual se quitan los artículos en el nombre de los grupos


# Fin--------------------------------------------------------------------------- 
beepr::beep(9)










