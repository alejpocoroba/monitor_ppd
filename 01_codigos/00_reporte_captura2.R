#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD versión 2
# Objetivo:                   Reporte de desempeño: captura de notas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          10 de noviembre de 2022
# Última actualización:       26 de enero de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión 2

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

# Noviembre
m10 <- read_xlsx(paste_inp("Monitor_PPD_noviembre.xlsx"))

# Diciembre
m11 <- read_xlsx(paste_inp("Monitor_PPD_diciembre.xlsx"))

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

# limpiar nombres de microdatos
df_microdatos1 <- df_pegada %>% 
  janitor::clean_names() %>% 
  select("responsable" = "x1_1_responsable",
         "publicacion" = "x1_2_1_fecha_de_publicacion",
         "estado"      = "x1_3_3_estado")

# m10 y m11 tiene estructura diferente
df_microdatos2 <- m10 %>% # noviembre
  janitor::clean_names() %>% 
  select("responsable" = "x1_1_responsable",
         "publicacion" = "x1_2_1_fecha_de_publicacion",
         "estado"      = "x1_3_3_estado")

df_microdatos4 <- m11 %>% # diciembre 19/01/23
  janitor::clean_names() %>% 
  select("responsable" = "x1_1_responsable",
         "publicacion" = "x1_2_1_fecha_de_publicacion",
         "estado"      = "x1_3_3_estado")

# Se pagan las bases de junio a diciembre 
df_microdatos3 <-  df_microdatos1 %>% 
  bind_rows(df_microdatos2)       %>% 
  bind_rows(df_microdatos4)       %>%
  # modificaciones sobre fecha: mes
  mutate(mes = lubridate::month(publicacion)) %>% 
  mutate(
    id_entidad = str_extract_all(estado, "[:digit:]"))

# 3. Cifras de captura ---------------------------------------------------
## Periodo junio a noviembre----
# Total de observaciones 
paste0("Número total de observaciones entre junio y diciembre de 2022: ", dim(df_microdatos3)[1])

# Total de obsveraciones por mes
table(df_microdatos3$mes)

# Total de observaciones por responsable 
table(df_microdatos3$responsable)

# Porcentajes

# Porcentaje por responsable 
df_responsable <- df_microdatos3 %>% 
  group_by(responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# Porcentje por responsable y mes
df_responsable_mes <- df_microdatos3 %>% 
  group_by(mes, responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  group_by(mes) %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# Porcentaje por estado
df_entidad<- df_microdatos3 %>% 
  group_by(estado) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# 4. Figuras-----

## Capturas general----
# procesamiento
df_data1 <- df_microdatos3 %>% 
  group_by(publicacion, estado, responsable) %>% 
  summarise(
    total = n()) %>% 
  filter(!is.na(estado)) %>% 
  drop_na()

# Por estado, publicación y responsable
ggplot(
  # Datos
  df_data1 %>% filter(publicacion>= as.Date("2022-06-01"), publicacion<= as.Date("2022-12-31")), 
  # Coordenadas 
  aes(x = publicacion, y = total, fill = responsable)) +
  facet_wrap(~estado, ncol = 8) +
  # Geoms
  geom_col() +
  # Etiquetas
  labs(
    title = "Total de observaciones capturadas para el Monitor PPD de junio a diciembre 2022", 
    subtitle = "Por estado, fecha de publicación y persona responsable", 
    fill = "Persona\nresponsable\n", 
    x = "Fecha de publicación", 
    y = "\nNúmero de observaciones capturadas", 
    caption = paste0("Fuente: Monitor-PPD al 26/01/23 ")
  ) +
  # Escalas
  scale_fill_brewer(palette="Set2") +
  # Temas 
  theme_bw() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 30))

ggsave(file = paste_fig("01_captura_general.png"), 
       width = 10, height = 6)

## Captura desagregado----
# Procesamiento 
for(i in 6:12) {
  df_data2 <- df_microdatos3 %>% 
    filter(mes == i) %>% 
    group_by(publicacion, estado, responsable) %>% 
    summarise(
      total = n()
    ) %>% 
    filter(!is.na(estado)) %>% 
    drop_na()

# Por día, mes, estado y responsable
  ggplot(
    # Datos
    df_data2 %>% filter(publicacion>= as.Date("2022-06-01"), publicacion<= as.Date("2022-12-31")), 
    # Coordenadas 
    aes(x = publicacion, y = total, fill = responsable)) +
    facet_wrap(~estado, ncol = 8) +
    # Geoms
    geom_col() +
    # Etiquetas
    labs(
      title = paste0("Total de observaciones capturadas por mes ", i), 
      subtitle = "Por estado, fecha de publicación y persona responsable", 
      fill = "Persona\nresponsable\n", 
      x = "Fecha de publicación", 
      y = "\nNúmero de observaciones capturadas", 
      caption = paste0("Fuente: Monitor-PPD al 26/01/23 ")
    ) +
    # Escalas
    scale_fill_brewer(palette="Set2") +
    # Tema
    theme_bw() +
    theme(
      legend.position = "top", 
      axis.text.x = element_text(angle = 30))
  
  ggsave(file = paste_fig(paste0("02_captura_estado_fecha_persona_", i, ".png")), 
         width = 10, height = 6)
  
}

# Fin 


