#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento de la información
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          25 de junio de 2022
# Última actualización:       07 de febrero de 2023
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

### 2.1 Verificar los nombres de las bases------------------------------------

colnames(df_j)
colnames(m2)

nom_ms <- colnames(df_j) %in% colnames(m2)
nom_ms2 <- colnames(m2) %in% colnames(df_j)

sum(nom_ms2)
table(nom_ms)


colnames(df_j)[!nom_ms]
colnames(m2)[!nom_ms2]

### 2.2 Pegar todas las bases-------------------------------------------------
df_1 <- df_j %>% # base junio 1 
  bind_rows(m2) %>% # base junio 2
  bind_rows(m3) %>% # base julio
  bind_rows(m4) %>% # base agosto 1
  bind_rows(m5) %>% # base agosto 2
  bind_rows(m6) %>% # base agosto 3
  bind_rows(m7) %>% # base septiembre 1
  bind_rows(m8) %>% # base septiembre 2
  bind_rows(m9)     # base octubre 

## 2.3. Homologación de variables-----------------------------------------------

df_2 <- df_1 %>% 
  janitor::clean_names() %>% 
  rename("datos_generales"                            = "x1_datos_generales",
         "responsable"                                = "x1_1_responsable",
         "sobre_la_nota"                              = "x1_2_sobre_la_nota",
         "fecha_de_publicacion"                       = "x1_2_1_fecha_de_publicacion",
         "enlace"                                     = "x1_2_2_enlace",
         "titulo_de_la_nota"                          = "x1_2_3_titulo_de_la_nota",
         "nombre_de_la_fuente"                        = "x1_2_4_nombre_de_la_fuente",
         "nota_complementaria_duplicada"              = "x1_2_5_nota_complementaria_duplicada",
         "enlaces_de_notas_complementaria_duplicada"  = "x1_2_6_enlaces_de_notas_complementaria_duplicada",
         "hechos"                                     = "x1_3_hechos", 
         "fecha_de_los_hechos"                        = "x1_3_1_fecha_de_los_hechos",
         "pais"                                       = "x1_3_2_pais",
         "estado"                                     = "x1_3_3_estado",
         "municipio"                                  = "x1_3_4_municipio",
         "lugar"                                      = "x1_3_5_lugar",
         "grupos_criminales"                          = "x2_grupos_criminales",
         "grupos_criminales_gc"                       = "x2_1_grupos_criminales_gc", 
         "nombre_del_grupo_criminal_gc"               = "x2_1_1_nombre_del_grupo_criminal_gc",
         "alianza_del_gc"                             = "x2_1_2_alianza_del_gc",
         "rival_del_gc"                               =  "x2_1_3_rival_del_gc",
         "actividades_delictivas"                     = "x3_actividades_delictivas",
         "muertos"                                    = "x3_1_muertos",
         "numero_de_homicidios_total"                 = "x3_1_1_numero_de_homicidios_total",
         "numero_de_homicidios_hombre"                = "x3_1_2_numero_de_homicidios_hombre",
         "numero_de_homicidios_mujer"                 = "x3_1_3_numero_de_homicidios_mujer",
         "pertenece_a"                                = "x3_1_4_pertenece_a",
         "cuerpo_s_localizado_s"                      = "x3_1_5_cuerpo_s_localizado_s",
         "victimas"                                   = "x3_2_victimas",
         "numero_de_heridos_as_total"                 = "x3_2_1_numero_de_heridos_as_total",
         "numero_de_heridos_hombres"                  = "x3_2_2_numero_de_heridos_hombres",
         "numero_de_heridas_mujeres"                  = "x3_2_3_numero_de_heridas_mujeres",
         "herido_a_pertenece_a"                       = "x3_2_4_herido_a_pertenece_a",
         "detenidos_as"                               = "x3_3_detenidos_as",
         "numero_de_detenidos_as_total"               = "x3_3_1_numero_de_detenidos_as_total",
         "numero_de_detenidos_hombres"                = "x3_3_2_numero_de_detenidos_hombres",
         "numero_de_detenidas_mujeres"                = "x3_3_3_numero_de_detenidas_mujeres",
         "detenido_a_pertenece_a"                     = "x3_3_4_detenido_a_pertenece_a",
         "ataque_armado_t"                            = "x3_4_ataque_armado", 
         "ataque_armado"                              = "x3_4_1_ataque_armado",
         "otras_actividades_delictivas"               = "x3_5_otras_actividades_delictivas",
         "trafico"                                    = "x3_5_1_trafico",
         "privacion_de_la_libertad"                   = "x3_5_2_privacion_de_la_libertad",
         "numero_de_personas_privadas_de_su_libertad" = "x3_5_2_1_numero_de_personas_privadas_de_su_libertad",
         "narcomensaje"                               = "x3_5_3_narcomensaje",
         "contenido_del_narcomensaje"                 = "x3_5_3_1_contenido_del_narcomensaje",
         "otras_actividades_ilicitas"                 = "x3_5_4_otras_actividades_ilicitas",
         "presencia_internacional"                    = "x3_5_5_presencia_internacional",
         "presencia_no_violenta_t"                    = "x3_6_presencia_no_violenta", 
         "presencia_no_violenta"                      = "x3_6_1_presencia_no_violenta",
         "instituciones_de_seguridad"                 = "x4_instituciones_de_seguridad",
         "fuerzas_armadas"                            = "x4_1_fuerzas_armadas", 
         "autoridad_militar"                          = "x4_1_1_autoridad_militar",
         "tipo_de_actividad_militar"                  = "x4_1_2_tipo_de_actividad_militar",
         "fuerzas_de_seguridad"                       = "x4_2_fuerzas_de_seguridad",
         "autoridad_civil"                            = "x4_2_1_autoridad_civil",
         "tipo_de_actividad_civil"                    = "x4_2_2_tipo_de_actividad_civil",
         "politica_de_seguridad_y_de_drogas"          = "x5_politica_de_seguridad_y_de_drogas",
         "politica_de_seguridad"                      = "x5_1_politica_de_seguridad",
         "politica_de_drogas"                         = "x5_2_politica_de_drogas")  %>% 
  # Agregar variable de mes para facilitar filtros de fechas 
  mutate(mes = lubridate::month(fecha_de_publicacion, label = TRUE))

## 2.4. Filtrar periodo de interés ---------------------------------------------

# # Explorar variable de fecha de publicación 
# sum(is.na(df_2$fecha_de_publicacion)) # Hay 1 NAs - colum vacía
# table(df_2$mes) # Distribución por mes 
# 
# # ---- Antes del periodo de interés 
# df_antes <- df_2 %>% # notas antes de 01/06
#   filter(mes < "jun") # Todo antes de junio: 121 notas
# 
# table(df_antes$mes)
# 
# # ---- Después del periodo de interés
# df_despues <- df_2 %>% # notas después de octubre: 8 notas
#   filter(mes > "oct") # Todo antes de junio: 121 notas
# 
# table(df_despues$mes)

# ----- Periodo de interés
df_monitor_periodo <- df_2 %>% 
  # Dejar todo lo que sea posterior a mayo y previo a noviembre
  filter(mes > "may" & mes < "nov")  


# 3. Filtros de calidad --------------------------------------------------------

## 3.1. Temporalidad -----------------------------------------------------------

# Ver meses incluidos en la base: 
table(df_monitor_periodo$mes)

# # Ver que las dimensiones de las baes sean consistentes a través de los periodos 
# dim(df_2)
# dim(df_antes)
# dim(df_monitor_periodo)
# dim(df_despues)

## 3.2. IDs --------------------------------------------------------------------

# ---- Exploración de ID
# Ver frecuencia de los IDs (cada ID debería aparecer solo una vez)
View(table(df_monitor_periodo$id)) # Solo hay una instancia donde se repita

# Ver observaciones donde el ID se repite 
df_error <- df_monitor_periodo %>% 
  filter(id == 7592) # Se comprueba que son dos observaciones distintas en contenido

table(df_error$mes)

# Se identificó que el ID 63 no está entre los IDs usados 
v_ids <- unique(df_monitor_periodo$id) # Lista de IDs en la base
60 %in% v_ids # Por ejemplo, el ID 60 sí está presente
63 %in% v_ids # El ID 63 no está presente

# Adicional a este error, hay que notar que en la base procesada por Monse Lara
# y la Dra. Atuesta, la nota fue renombrada como 99. Dado que en esta base sí 
# hay un ID 99, la nota con ID 99 pasara a ser ID 63 y una de las notas 
# repetidas con ID 7592 pasará a ser ID 99, para que coincida con la base 
# de la Dra. Atuesta. 


# ---- Cambiamos el ID de una de las observaciones con ID repetido 
df_monitor_amplio <- df_monitor_periodo %>% 
  mutate(
    # Variable con ID 99 asignar ID 63
    id = if_else(id == 99, 63, id),
    # Variable repetida con ID 7592, asignar ID 99
    id = if_else(id == 7592 & mes == "sep", 99, id)) 

# ---- Repetir filtros de calidad

View(table(df_monitor_amplio$id)) # Solo hay una instancia donde se repita


# 4. Guardar base sin modificar ------------------------------------------------

# Renombrar 
monitor_jun_oct <- df_monitor_amplio

# ---- Guardar base 
openxlsx::write.xlsx(monitor_jun_oct, file = paste_out("monitor_jun_oct.xlsx"), overwrite = T)

save(monitor_jun_oct, file = paste_out("monitor_jun_oct.Rdata"))

# FIN. ------------------------------------------------------------------------- 
