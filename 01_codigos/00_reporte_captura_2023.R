#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Reporte de desempeño: captura de notas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          19 de enero de 2023
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
m1 <- read_xlsx(paste_inp("Monitor_PPD_enero.26.xlsx"))

# 2. Procesamiento 

# limpiar nombres de microdatos
df_ <- m1 %>% 
  janitor::clean_names() %>% 
  select("responsable" = "x1_1_responsable",
         "publicacion" = "x1_2_1_fecha_publicacion",
         "estado"      = "x1_3_2_estado") %>% 
  # modificaciones sobre fecha: mes
  mutate(mes = lubridate::month(publicacion)) %>% 
  mutate(
    id_entidad = str_extract_all(estado, "[:digit:]"))

# 3. Cifras de captura ---------------------------------------------------
## Periodo junio a noviembre----
# Total de observaciones 
paste0("Número total de observaciones entre enero 2023: ", dim(df_)[1])

# Total de obsveraciones por mes
table(df_$mes)

# Total de observaciones por responsable 
table(df_$responsable)

# Porcentajes

# Porcentaje por responsable 
df_responsable <- df_ %>% 
  group_by(responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# Porcentje por responsable y mes
df_responsable_mes <- df_ %>% 
  group_by(mes, responsable) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  group_by(mes) %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# Porcentaje por estado
df_entidad<- df_ %>% 
  group_by(estado) %>% 
  summarise(notas = n()) %>% 
  ungroup() %>% 
  mutate(
    porcentaje = round(notas/sum(notas), 4), 
    porcentaje_text = scales::percent(porcentaje, accuracy = 0.01))

# 4. Figuras-----

## Capturas general----
# procesamiento
df_data1 <- df_ %>% 
  group_by(publicacion, estado, responsable) %>% 
  summarise(
    total = n()) %>% 
  filter(!is.na(estado)) %>% 
  drop_na()

# Por estado, publicación y responsable
ggplot(
  # Datos
  df_data1 %>% filter(publicacion>= as.Date("2023-01-01"), publicacion<= as.Date("2023-01-31")), 
  # Coordenadas 
  aes(x = publicacion, y = total, fill = responsable)) +
  facet_wrap(~estado, ncol = 8) +
  # Geoms
  geom_col() +
  # Etiquetas
  labs(
    title = "Total de observaciones capturadas para el Monitor PPD en enero 2023", 
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

ggsave(file = paste_fig("01_captura_general_2023.png"), 
       width = 10, height = 6)



# Fin 

