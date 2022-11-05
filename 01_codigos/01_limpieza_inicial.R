#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento de la información
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          25 de junio de 2022
# Última actualización:       04 de noviembre de 2022
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

#junio
m1 <- read_xlsx(paste_inp("Monitor_junio/Monitor_PPD_junio1.xlsx"))
m2 <- read_xlsx(paste_inp("Monitor_junio/Monitor_PPD_junio2.xlsx"))

# julio
m3 <- read_xlsx(paste_inp("Monitor_PPD_julio.xlsx"))

# Agosto 
m4 <- read_xlsx(paste_inp("Monitor_PPD_agosto1.xlsx"))
m5 <- read_xlsx(paste_inp("Monitor_PPD_agosto2.xlsx"))
m6 <- read_xlsx(paste_inp("Monitor_PPD_agosto3.xlsx"))

# Septiembre
m7 <- read_xlsx(paste_inp("Monitor_PPD_septiembre1.xlsx"))
m8 <- read_xlsx(paste_inp("Monitor_PPD_septiembre2.xlsx"))

#Octubre
m9 <- read_xlsx(paste_inp("Monitor_PPD_octubre.xlsx"))

# 2. Limpar datos --------------------------------------------------------------

df_j <- m1 %>% 
  rename("1.2.2) Enlace" = "1.2.1) Enlace",
         "1.2.5) Nota complementaria/duplicada" = "1.2.5) Nota duplicada") %>% 
  mutate(
    `1.2.6) Enlaces de notas complementaria/duplicada` = 
      paste(`1.2.6) Enlace de nota duplicada`, `1.2.6.1) Enlaces de notas duplicadas`, 
            sep = ";")) %>% 
  select(-c(`1.2.6) Enlace de nota duplicada`, 
            `1.2.6.1) Enlaces de notas duplicadas`)) 

# Bases que tienen valores vacíos ocultos de origen
m5 <- m5 %>%
  filter(!is.na(Id)) 

m6 <- m6 %>%
  filter(!is.na(Id)) 

## 2.1. Pegar bases ------------------------------------------------------------
### 2.1.2 Verificar los nombres de las bases------------------------------------
### Base m1 tiene nombres de variables distintas al resto de las bases 

colnames(df_j)
colnames(m2)

nom_ms <- colnames(df_j) %in% colnames(m2)
nom_ms2 <- colnames(m2) %in% colnames(df_j)

sum(nom_ms2)
table(nom_ms)


colnames(df_j)[!nom_ms]
colnames(m2)[!nom_ms2]

### 2.1.2 Pegar todas las bases-------------------------------------------------
df_1 <- df_j %>% # base junio 1 
  bind_rows(m2) %>% # base junio 2
  bind_rows(m3) %>% # base julio
  bind_rows(m4) %>% # base agosto 1
  bind_rows(m5) %>% # base agosto 2
  bind_rows(m6) %>% # base agosto 3
  bind_rows(m7) %>% # base septiembre 1
  bind_rows(m8) %>% # base septiembre 2
  bind_rows(m9)

## 2.2. Homologación de variables-----------------------------------------------
df_2 <- df_1 %>% 
  janitor::clean_names() %>% 
  rename("datos_generales"      = "x1_datos_generales",
         "responsable"          = "x1_1_responsable",
         "sobre_la_nota"        = "x1_2_sobre_la_nota",
         "fecha_de_publicacion" = "x1_2_1_fecha_de_publicacion",
         "enlace"               = "x1_2_2_enlace",
         "titulo_de_la_nota"    = "x1_2_3_titulo_de_la_nota",
         "nombre_de_la_fuente"  = "x1_2_4_nombre_de_la_fuente",
         "nota_complementaria_duplicada"= "x1_2_5_nota_complementaria_duplicada",
         "enlaces_de_notas_complementaria_duplicada" = "x1_2_6_enlaces_de_notas_complementaria_duplicada",
         "hechos"               = "x1_3_hechos", 
         "fecha_de_los_hechos"  = "x1_3_1_fecha_de_los_hechos",
         "pais"                 = "x1_3_2_pais",
         "estado"               = "x1_3_3_estado",
         "municipio"            = "x1_3_4_municipio",
         "lugar"                = "x1_3_5_lugar",
         "grupos_criminales" = "x2_grupos_criminales",
         "grupos_criminales_gc" = "x2_1_grupos_criminales_gc", 
         "nombre_del_grupo_criminal_gc" = "x2_1_1_nombre_del_grupo_criminal_gc",
         "alianza_del_gc"       = "x2_1_2_alianza_del_gc",
         "rival_del_gc"         =  "x2_1_3_rival_del_gc",
         "actividades_delictivas" = "x3_actividades_delictivas",
         "muertos"                = "x3_1_muertos",
         "numero_de_homicidios_total" = "x3_1_1_numero_de_homicidios_total",
         "numero_de_homicidios_hombre"= "x3_1_2_numero_de_homicidios_hombre",
         "numero_de_homicidios_mujer" = "x3_1_3_numero_de_homicidios_mujer",
         "pertenece_a" = "x3_1_4_pertenece_a",
         "cuerpo_s_localizado_s" = "x3_1_5_cuerpo_s_localizado_s",
         "victimas" = "x3_2_victimas",
         "numero_de_heridos_as_total" = "x3_2_1_numero_de_heridos_as_total",
         "numero_de_heridos_hombres" = "x3_2_2_numero_de_heridos_hombres",
         "numero_de_heridas_mujeres" = "x3_2_3_numero_de_heridas_mujeres",
         "herido_a_pertenece_a" = "x3_2_4_herido_a_pertenece_a",
         "detenidos_as" = "x3_3_detenidos_as",
         "numero_de_detenidos_as_total" = "x3_3_1_numero_de_detenidos_as_total",
         "numero_de_detenidos_hombres" = "x3_3_2_numero_de_detenidos_hombres",
         "numero_de_detenidas_mujeres" = "x3_3_3_numero_de_detenidas_mujeres",
         "detenido_a_pertenece_a" = "x3_3_4_detenido_a_pertenece_a",
         "ataque_armado_t" = "x3_4_ataque_armado", 
         "ataque_armado" = "x3_4_1_ataque_armado",
         "otras_actividades_delictivas" = "x3_5_otras_actividades_delictivas",
         "trafico" = "x3_5_1_trafico",
         "privacion_de_la_libertad" = "x3_5_2_privacion_de_la_libertad",
         "numero_de_personas_privadas_de_su_libertad" = "x3_5_2_1_numero_de_personas_privadas_de_su_libertad",
         "narcomensaje" = "x3_5_3_narcomensaje",
         "contenido_del_narcomensaje" = "x3_5_3_1_contenido_del_narcomensaje",
         "otras_actividades_ilicitas" = "x3_5_4_otras_actividades_ilicitas",
         "presencia_internacional" = "x3_5_5_presencia_internacional",
         "presencia_no_violenta_t" = "x3_6_presencia_no_violenta", 
         "presencia_no_violenta" = "x3_6_1_presencia_no_violenta",
         "instituciones_de_seguridad" = "x4_instituciones_de_seguridad",
         "fuerzas_armadas" = "x4_1_fuerzas_armadas", 
         "autoridad_militar" = "x4_1_1_autoridad_militar",
         "tipo_de_actividad_militar" = "x4_1_2_tipo_de_actividad_militar",
         "fuerzas_de_seguridad" = "x4_2_fuerzas_de_seguridad",
         "autoridad_civil" = "x4_2_1_autoridad_civil",
         "tipo_de_actividad_civil" = "x4_2_2_tipo_de_actividad_civil",
         "politica_de_seguridad_y_de_drogas" = "x5_politica_de_seguridad_y_de_drogas",
         "politica_de_seguridad" = "x5_1_politica_de_seguridad",
         "politica_de_drogas" = "x5_2_politica_de_drogas") %>% 
  select(-c(`tipo_de_elemento`, `ruta_de_acceso`)) %>% 
# 2.1 Base modificada-----------------------------------------------------------
# Id nuevos, eliminación de repetidos y política de seguridad y drogas
# construcción de identificadores 
  mutate(n_hechos = 
           (numero_de_homicidios_total > 0) + 
           (numero_de_heridos_as_total > 0) + 
           (numero_de_detenidos_as_total > 0) +
           (narcomensaje) +
           (privacion_de_la_libertad) +
           !is.na(otras_actividades_ilicitas)) %>% 
# Casos repetidos 
  distinct(fecha_de_publicacion, fecha_de_los_hechos, municipio, n_hechos, 
           autoridad_militar, autoridad_civil, 
           nombre_del_grupo_criminal_gc, cuerpo_s_localizado_s, 
           numero_de_homicidios_total, numero_de_detenidos_as_total, ataque_armado,
           politica_de_seguridad, lugar,
           .keep_all = TRUE) 

# Controles de calidad 
# v_unicos <- unique(df_2$id)
# 
# df_eliminados <- df_1 %>%
#    filter(!Id %in% v_unicos)
# 
# table(df_eliminados$Responsable)

# Quitar variables política de seguridad y drogas e internacional
df_3 <- df_2 %>% 
  filter(!politica_de_seguridad == TRUE,
         !politica_de_drogas == TRUE,
         presencia_internacional == FALSE) %>% 
  select(-c(politica_de_seguridad, politica_de_drogas, presencia_internacional))


# 3. Guardar base sin modificar (amplia, hasta línea 160)-----------------------
df_monitor_amplio <- df_2 # hasta octubre 

openxlsx::write.xlsx(df_monitor_amplio, file = paste_out("Monitor_df_full.xlsx"), overwrite = T)
save(df_monitor_amplio, file = paste_out("df_monitor_amplio.Rdata"))

# 3.1 Estructura de la base modificada-----------------------------------------

# ¿Cuáles son las dimensiones de la base?
dim(df_3)

# ¿Cuántas observaciones hay por entidad?
table(df_3$estado)

# ¿Cuáles entidadades están registradas?
unique(df_3$estado)

# ¿Cuántas entidadades están registradas?
length(unique(df_3$estado))

# ¿En cuántas observaciones no se registró la entidad?
sum(is.na(df_3$estado))

# Ver observaciones defectusas 
df_na <- df_3 %>% 
  filter(is.na(estado))

# 3.2 Guardar base modificada------- -------------------------------------------
df_monitor <- df_3

save(df_monitor, file = paste_out("df_monitor.Rdata"))


                      

