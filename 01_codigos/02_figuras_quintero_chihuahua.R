#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento de info para figuras
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          21 de agosto de 2022
# Última actualización:       21 de agosto de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("03_datos_limpios/"           , x)}
paste_fig <- function(x){paste0("04_figuras/quintero_chihuahua/", x)}

# 1. Cargar datos --------------------------------------------------------------

load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza ------------------------------------------------------------------

df_limpio <- df_monitor_amplio              %>% 
  # Renombrar
  mutate(year = year(fecha_de_los_hechos), 
         mes  = month(fecha_de_los_hechos),
         semana = week(fecha_de_los_hechos)
         ) %>% 
  select(estado, year, mes, semana, 
         fecha      = fecha_de_los_hechos, 
         homicidios = numero_de_homicidios_total, 
         detenidos  = numero_de_detenidos_as_total, 
         heridos    = numero_de_heridos_as_total, 
         privadas   = numero_de_personas_privadas_de_su_libertad) %>% 
  # Agrupar para sacar el total por día por entidad 
  group_by(estado, fecha) %>% 
  summarise(across(where(is.numeric), ~sum(., na.rm = T)))        %>% 
  # Dejar solo entidades de interés 
  filter(str_detect(estado, "Chihuahua") | str_detect(estado, "Sonora")) %>% 
  # Cambiar a formato largo 
  pivot_longer(
    cols      = c(homicidios:privadas), 
    names_to  = "evento", 
    values_to = "total") %>% 
  # Renombrar eventos 
  mutate(evento = case_when(
    evento == "homicidios" ~ "Homicidios", 
    evento == "detenidos"  ~ "Personas detenidas", 
    evento == "heridos"    ~ "Personas heridas", 
    evento == "privadas"   ~ "Personas privadas de la libertad"), 
    total = as.integer(total), 
    arresto_caro     = if_else(fecha == as.Date("2022-07-15"), 1, 0),
    asesinato_padres = if_else(fecha == as.Date("2022-06-20"), 1, 0),
    jueves_negro     = if_else(fecha == as.Date("2022-08-11"), 1, 0)) %>% 
  # filter(total != 0) %>% 
  glimpse()

df_limpio_semana <- df_limpio       %>% 
  group_by(estado, semana, evento)  %>% 
  summarise(total = sum(total))

# 3. Figuras -------------------------------------------------------------------

## 3.0. Configuración de figuras -----------------------------------------------

# ---- Tema
tema <-  theme_bw() +
  theme(
    text             = element_text(family = "Fira Sans", color = "black"),
    plot.title       = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
    plot.subtitle    = element_text(size = 10, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
    plot.caption     = element_text(hjust = .5, size = 6, family = "Fira Sans", color = "black"),
    panel.grid       = element_line(linetype = 2),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.title     = element_text(size = 8, face = "bold", family="Fira Sans"),
    legend.text      = element_text(size = 10, family="Fira Sans"),
    axis.title.x     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
    axis.title.y     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans", angle = 90),
    axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=1),
    axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
    strip.background = element_rect(fill="white", colour = NA),
    strip.text.x     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"))

# ---- Colores

# ---- Vectores de texto
v_caption <- "Fuente: Elaboracón propia con datos del Monitor-PPD 2022"

## 3.1. Sonora (Caro Quintero) -------------------------------------------------

# ---- Fecha de arresto de caro quintero
v_arresto <- unique(df_limpio$fecha[df_limpio$arresto_caro == 1])

# ---- Filtrar datos 
df_data <- df_limpio %>% filter(str_detect(estado, "Sonora"), 
                                !str_detect(evento, "detenidas"),
                                # !str_detect(evento, "heridas"), 
                                fecha >= as.Date("2022-06-01"))

# ---- Visualización 
ggplot(
  # Datos
  df_data, 
  # Coordenadas
       aes(x = fecha, y = total, group = evento, color = evento)) +
  facet_wrap(~evento, ncol = 1) +
  # Geoms
  geom_line() +
  geom_point() +
  geom_vline(xintercept = v_arresto, linetype = "dashed") +
  annotate("text", x = v_arresto + 100000, y = 14, 
           # label = if_else(df_data$evento == "Homicidios", "Arresto de Caro Quintero", NA_character_), 
           label = "Arresto de Caro Quintero", 
           size = 3, family = "Fira Sans", color = "#666666", hjust = 0) +
  # geom_vline(xintercept = as.Date("2022-07-15"), linetype = "dashed") +
  # Etiquetas
  labs(
    title    = "Eventos delictivos ocurridos en Sonora", 
    subtitle = "Diario y por tipo de evento\n", 
    x = "", 
    y = "Número total de eventos\n", 
    color = "", 
    caption = v_caption
  ) +
  # Escalas
  # scale_y_continuous(breaks = seq(0, 15, 2)) +
  # Tema
  tema +
  theme(legend.position = "none")

# ---- Guardar figura
ggsave(file = paste_fig("quintero.png"), 
       width = 6, height = 5)



## 3.2. Chihuahua --------------------------------------------------------------


# ---- Fecha de arresto de caro quintero
v_padres <- unique(df_limpio$fecha[df_limpio$asesinato_padres == 1])
v_jueves <- unique(df_limpio$fecha[df_limpio$jueves_negro     == 1])

# ---- Filtrar datos 
df_data <- df_limpio %>% filter(str_detect(estado, "Chihuahua"), 
                                !str_detect(evento, "privadas"),
                               # !str_detect(evento, "detenidas"),
                                !str_detect(evento, "heridas"),
                                fecha >= as.Date("2022-06-01")) %>% 
  # mutate(fecha = as.Date(fecha)) %>% 
  glimpse()

# ---- Visualización 
ggplot(
  # Datos
  df_data, 
  # Coordenadas
  aes(x = fecha, y = total, group = evento, color = evento)) +
  facet_wrap(~evento, scale = "free_y", ncol = 1) +
  # Geoms
  geom_line() +
  geom_point() +
  # Asesinato de sacerdotes
  geom_vline(xintercept = v_padres, linetype = "dashed") +
  annotate("text", x = v_padres + 100000, y = 15,
           # label = if_else(df_data$evento == "Homicidios", "Arresto de Caro Quintero", NA_character_),
           label = "Asesinato de\nsacerdotes",
           size = 3, family = "Fira Sans", color = "#666666", hjust = 0) +
  # Jueves negro
  geom_vline(xintercept = v_jueves, linetype = "dashed") +
  annotate("text", x = v_jueves + 100000, y = 15,
           # label = if_else(df_data$evento == "Homicidios", "Arresto de Caro Quintero", NA_character_),
           label = "Jueves negro\nen Ciudad Juárez",
           size = 3, family = "Fira Sans", color = "#666666", hjust = 0) +
  geom_vline(xintercept = as.Date("2022-07-15"), linetype = "dashed") +
  # Etiquetas
  labs(
    title    = "Homicidios ocurridos en Chihuahua", 
    subtitle = "Por día\n", 
    x = "", 
    y = "Número total de eventos\n", 
    color = "", 
    caption = v_caption
  ) +
  # Escalas
  # scale_y_continuous(breaks = seq(0, 15, 2)) +
  # scale_x_date(date_labels="%d", date_breaks = "1 day") +
  # Tema
  tema +
  theme(legend.position = "none")

# ---- Guardar figura
ggsave(file = paste_fig("chihuahua.png"), 
       width = 8, height = 6)

# FIN. -------------------------------------------------------------------------

# ily :)
