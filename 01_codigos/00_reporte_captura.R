#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Reporte de desempeño: captura de notas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          25 de junio de 2022
# Última actualización:       01 de noviembre de 2022
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

# Bases que tienen valores ocultos de origen
m5 <- m5 %>%
  filter(!is.na(Id)) 

m6 <- m6 %>%
  filter(!is.na(Id)) 

# Se pegan todas las bases
df_pegada <- df_j %>% # base junio 1
  bind_rows(m2) %>%   # base junio 2
  bind_rows(m3) %>%   # base julio
  bind_rows(m4) %>%   # base agosto 1
  bind_rows(m5) %>%   # base agosto 2
  bind_rows(m6) %>%   # base agosto 3
  bind_rows(m7) %>%   # base septiembre 1
  bind_rows(m8) %>%   # base septiembre 2
  bind_rows(m9)       # base octubre 

# Limpiar nombres de microdatos
df_microdatos <- df_pegada %>% 
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
  # Mes de captura
  mutate(mes = lubridate::month(fecha_de_publicacion)) %>% 
  # Nombres de entidades
  mutate(
    id_entidad = str_extract_all(estado, "[:digit:]")
  )
  

df_responsable <- df_microdatos %>% 
  group_by(responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

df_responsable_mes <- df_microdatos %>% 
  group_by(mes, responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  group_by(mes) %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))


df_entidad<- df_microdatos %>% 
  group_by(estado) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))


# 3. Estadísticas de captura ---------------------------------------------------

# Total de notas capturadas
paste0("Total de notas capturadas: ", dim(df_microdatos)[1])

# Notas capturadas por mes
table(df_microdatos$mes)

# Notas capturadas por persona
table(df_microdatos$responsable)

# Notas capturadas por mes, por persona

ggplot(df_responsable_mes, 
       aes(x = mes, y = porcentaje, fill = responsable)) +
  geom_col()


ggplot(df_responsable_mes, 
       aes(x = mes, y = notas, fill = responsable)) +
  geom_col()


## 3.1 Total de notas capturadas por persona ---------------------------------------
ggplot(
  # Datos
  df_responsable,
  # Coordenadas
       aes(x = notas, y = reorder(responsable, notas))) +
  # Geoms
  geom_col() +
  geom_text(
    aes(label = paste0(notas, " (", porcentaje_text, ")")), 
    nudge_x = if_else(df_responsable$responsable == "AP", -130, 110), 
    color = if_else(df_responsable$responsable == "AP", "white", "black"), 
    ) +
  # Annotations
  annotate(
    "text", x = 1000, y = 1, 
    label = paste0("Total de observaciones capturadas: ", scales::comma(sum(df_responsable$notas)))
  ) +
  # Etiquetas
  labs( 
    title = "Total de observaciones capturadas para el Monitor PPD", 
    subtitle = "Por persona responsable", 
    x = "\nNúmero de observaciones capturadas", 
    y = "Persona responsable\n", 
    caption = paste0("Corte del Monitor-PPD a las 19:15 del día ", max(df_microdatos$fecha_de_publicacion))
    ) +
  # Escalas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema 
  theme_bw()

ggsave(file = paste_fig("00_captura_persona.png"))

## 3.2 Total de notas capturadas por persona y por mes -----------------------------
ggplot(
  # Datos
  df_responsable_mes,
  # Coordenadas
  aes(x = notas, y = reorder(responsable, notas))) +
  facet_wrap(~mes) +
  # Geoms
  geom_col() +
  geom_text(
    aes(label = paste0(notas, " (", porcentaje_text, ")")), size = 3,
    nudge_x = if_else(df_responsable_mes$responsable %in% c("AP", "LA"), -140, 120), 
    color = if_else(df_responsable_mes$responsable %in% c("AP", "LA"), "white", "black"), 
  ) +
  # Annotations
  # annotate(
  #   "text", x = 1000, y = 1, 
  #   label = paste0("Total de observaciones capturadas: ", scales::comma(sum(df_responsable_mes$notas)))
  # ) +
  # Etiquetas
  labs( 
    title = "Total de observaciones capturadas para el Monitor PPD", 
    subtitle = "Por mes y por persona responsable", 
    x = "\nNúmero de observaciones capturadas", 
    y = "Persona responsable\n", 
    caption = paste0("Corte del Monitor-PPD a las 19:15 del día ", max(df_microdatos$fecha_de_publicacion))
  ) +
  # Escalas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema 
  theme_bw()

ggsave(file = paste_fig("01_captura_persona_mes.png"))


## 3.3 Total de notas capturadas por persona por día--------------------------------
df_data <- df_microdatos %>% 
  group_by(fecha_de_publicacion, estado, responsable) %>% 
  summarise(
    total = n()
  ) %>% 
  filter(!is.na(estado)) %>% 
  drop_na()

ggplot(
  # Datos
  df_data %>% filter(fecha_de_publicacion>= as.Date("2022-06-01"), fecha_de_publicacion<= as.Date("2022-10-31")), 
  # Coordenadas 
       aes(x = fecha_de_publicacion, y = total, fill = responsable)) +
  facet_wrap(~estado, ncol = 8) +
  # Geoms
  geom_col() +
  # Etiquetas
  labs(
    title = "Total de observaciones capturadas para el Monitor PPD de junio a octubre", 
    subtitle = "Por estado, fecha de publicación y persona responsable", 
    fill = "Persona\nresponsable\n", 
    x = "Fecha de publicación", 
    y = "\nNúmero de observaciones capturadas", 
    caption = paste0("Monitor-PPD al 01/11/22 ")
  ) +
  # Escalas
  scale_fill_brewer(palette="Set2") +
  # Temas 
  theme_bw() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 30))
  


ggsave(file = paste_fig("03_captura_estado_fecha_persona.png"), 
       width = 10, height = 6)



## 3.4 Total de notas capturadas por persona por día por mes --------------------
df_data <- df_microdatos %>% 
  filter(mes == 6) %>% 
  group_by(fecha_de_publicacion, estado, responsable) %>% 
  summarise(
    total = n()
  ) %>% 
  filter(!is.na(estado)) %>% 
  drop_na()

ggplot(
  # Datos
  df_data, 
  # Coordenadas 
  aes(x = fecha_de_publicacion, y = total, fill = responsable)) +
  facet_wrap(~estado, ncol = 8) +
  # Geoms
  geom_col() +
  # Etiquetas
  labs(
    title = "Total de observaciones capturadas para el Monitor", 
    subtitle = "Por estado, fecha de publicación y persona responsable", 
    fill = "Persona\nresponsable\n", 
    x = "Fecha de publicación", 
    y = "\nNúmero de observaciones capturadas", 
    caption = paste0("Corte del Monitor-PPD al 15/08/22")
  ) +
  # Escalas
  scale_fill_brewer(palette="Set2") +
  # Tema
  theme_bw() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 30))

ggsave(file = paste_fig("02b_captura_estado_fecha_persona_junio.png"), 
       width = 10, height = 6)


# ---- Bucle

for(i in 6:10) {
  df_data <- df_microdatos %>% 
    filter(mes == i) %>% 
    group_by(fecha_de_publicacion, estado, responsable) %>% 
    summarise(
      total = n()
    ) %>% 
    filter(!is.na(estado)) %>% 
    drop_na()
  
  ggplot(
    # Datos
    df_data %>% filter(fecha_de_publicacion>= as.Date("2022-06-01"), fecha_de_publicacion<= as.Date("2022-10-31")), 
    # Coordenadas 
    aes(x = fecha_de_publicacion, y = total, fill = responsable)) +
    facet_wrap(~estado, ncol = 8) +
    # Geoms
    geom_col() +
    # Etiquetas
    labs(
      title = paste0("Total de observaciones capturadas para el Monitor en el mes ", i), 
      subtitle = "Por estado, fecha de publicación y persona responsable", 
      fill = "Persona\nresponsable\n", 
      x = "Fecha de publicación", 
      y = "\nNúmero de observaciones capturadas", 
      caption = paste0("Corte del Monitor-PPD al 01/11/22")
    ) +
    # Escalas
    scale_fill_brewer(palette="Set2") +
    # Tema
    theme_bw() +
    theme(
      legend.position = "top", 
      axis.text.x = element_text(angle = 30))

  ggsave(file = paste_fig(paste0("02b_captura_estado_fecha_persona_", i, ".png")), 
         width = 10, height = 6)
  
  }


