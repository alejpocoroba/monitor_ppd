#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Limpieza M-2023
#
# Encargado:                  Alejandro Pocoroba y Erick Isaac Morales Sánchez
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          01 de mayo de 2023
# Última actualización:       02 de mayo de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión enero-marzo 2023

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

ruta_archivo <- "C:/Users/erick/OneDrive/Documentos/MONITOR PPD/monitor20230102.xlsx"

# 1. Cargar datos --------------------------------------------------------------

# bases de enero, febrer y marzo 
a <- read_xlsx(paste_inp("Monitor_PPD_01_02.23.xlsx"))
b <- read_xlsx(paste_inp("Monitor_PPD_03.23.xlsx"))

# Se unen bases 
m1 <- a %>% bind_rows(b)

# 2. Procesamiento -------------------------------------------------------------

# Limpiar nombres de microdatos
df_ <- m1 %>% 
  janitor::clean_names() 

# Tratamiento de fechas #
df_$"x1_2_1_fecha_publicacion" <- as.Date(df_$"x1_2_1_fecha_publicacion")
df_$dia <- format(df_$"x1_2_1_fecha_publicacion", "%d")
df_$mes <- format(df_$"x1_2_1_fecha_publicacion", "%m")
df_$anio <- format(df_$"x1_2_1_fecha_publicacion", "%Y")

# Estado y munici´pio según su clave de Inegi 
df_ <- separate(df_, "x1_3_2_estado", c("estado", "clave"), sep = "-")
df_ <- separate(df_, "x1_3_3_municipio", c("municipio", "claveEdo", "clavemun"), sep = "-")
df_ <- subset(df_, select = -c(clave))
df_$clavefinal <- paste0(df_$claveEdo, df_$clavemun) 

# Orden y renombrar las variables 
df_1 <- df_ %>%
  select("fecha_de_publicacion"   = "x1_2_1_fecha_publicacion",
         "Mes"                    = "mes", 
         "Dia"                    = "dia",
         "Anio"                   = "anio", 
         "fecha_hechos"           = "x1_3_1_fecha_hechos",
         "Enlace"                 = "x1_2_2_enlace",
         "titulo_de_la_nota"      = "x1_2_3_titulo_nota",
         "nombre_de_la_fuente"    = "x1_2_4_fuente",
         "enlace_otras_notas"     = "x1_2_5_enlaces_duplicados",
         "Estado"                 = "estado",
         "Municipio"              = "municipio",
         "CVEGEO_ent"             = "claveEdo",
         "CVEGEO_mun"             = "clavemun",
         "CVEGEO"                 = "clavefinal",
         "Lugar"                  = "x1_3_4_lugar",
         "grupo_criminal"         = "x3_3_grupo_criminal",
         "Alianza"                = "x3_3_1_alianza_grupo",
         "Rival"                  = "x3_3_2_rival_grupo",
         "Narcomensaje"           = "x3_2_1_narcomensaje",
         "contenido_narcomensaje" = "x3_2_2_contenido_narcomensaje",
         "homic_total"            = "x2_2_1_homicidios_total",
         "homic_hombre"           = "x2_2_2_homicidios_hombre",
         "homic_mujer"            = "x2_2_3_homicidios_mujer",
         "homic_clasif1"          = "x2_2_4_pertenece_homicidio",
         "cuerpos_localizados"    = "x2_2_5_cuerpo_restos",
         "cuerpos_modo1"          = "x2_2_6_cuerpo_restos_localizados",
         "cuerpos_lugar1"         = "x2_2_7_lugar_cuerpo_restos",
         "heridos_total"          = "x2_3_1_heridxs_total",
         "heridos_hombre"         = "x2_3_2_heridos_hombres",
         "heridos_mujeres"        = "x2_3_3_heridas_mujeres",
         "heridos_clasif1"        = "x2_3_4_pertenece_heridx",
         "ataque_armado_clean"    = "x2_1_1_ataque",
         "lugar_ataque_clean"     = "x2_1_2_lugar_ataque",
         "politica_de_seguridad"  = "politica_seguridad")
 
# 3. Limpiar de las variables -------------------------------------------------
### 3.1.-   Ataque--------------------------------------------------------------------
# Lugar 
# Persona (homicidios + heridxs)

# Cambiar FALSE/TRUE por 0 y 1 en Narcomensajes, Cuerpos y Poli de Seg

# Narcomensaje
df_1$Narcomensaje <- as.character(df_1$Narcomensaje)
df_1 <- df_1 %>%
  mutate(Narcomensaje = 
           case_when(Narcomensaje == "FALSE" ~ "0",
                     Narcomensaje == "TRUE"  ~ "1",
                     Narcomensaje == Narcomensaje ~ Narcomensaje))
# Cuerpos 
df_1$cuerpos_localizados <- as.character(df_1$cuerpos_localizados)
df_1 <- df_1 %>%
  mutate(cuerpos_localizados = 
           case_when(cuerpos_localizados == "FALSE" ~ "0",
                     cuerpos_localizados == "TRUE"  ~ "1",
                     cuerpos_localizados == cuerpos_localizados ~ cuerpos_localizados))

# Politica de seguridad 
df_1$politica_de_seguridad <- as.character(df_1$politica_de_seguridad)
df_1 <- df_1 %>%
  mutate(politica_de_seguridad = 
           case_when(politica_de_seguridad == "FALSE" ~ "0",
                     politica_de_seguridad == "TRUE"  ~ "1",
                     politica_de_seguridad == politica_de_seguridad ~ politica_de_seguridad))

# Cuerpos
df_1 <- df_1 %>% 
  mutate(cuerpos_modo1 = str_replace_all(cuerpos_modo1, ":", ";", ".")) %>% 
  separate(cuerpos_modo1, sep = ";", c("cuerpo_modo1", 
                                       "cuerpo_modo2"))





# Base en Excel 
write_xlsx(df_1, "monitor2023.xlsx")