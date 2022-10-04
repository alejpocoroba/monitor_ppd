#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Sonora: antes y después de Quintero
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          26 de septiembre de 2022
# Última actualización:       29 de septiembre de 2022
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
paste_fig <- function(x){paste0("04_figuras/"           , x)}

# 1. Cargar datos --------------------------------------------------------------

load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza de datos ---------------------------------------------------------
## Base de datos de Sonora
df_sonora <- df_monitor_amplio %>% 
  filter(estado == "Sonora-26") 
  
## Datos del 1 al 14 de junio 
df_sonora_antes <- df_sonora %>% 
  filter(fecha_de_los_hechos >= '2022-06-01') %>% 
  filter(fecha_de_los_hechos <= '2022-06-14')

# Homicidios 
df_son_an_hom <- df_sonora_antes %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
  cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
  names_to  = "evento", 
  values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total))

# Frecuencia de heridos por Total, Hombres y Mujeres 
df_data_her <- df_son_an_her %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Heridxs 
df_son_an_her <- df_sonora_antes %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total))

# Frecuencia de heridas por Total, Hombres y Mujeres 
df_data_he <- df_son_an_hom %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Frecuencia de homicidios por municipio
df_son_an_hom_est <- df_sonora_antes %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total))

# Homicidios - localizado
df_son_ant_loc <- df_sonora_antes %>% 
  select(numero_de_homicidios_total, cuerpo_s_localizado_s) %>%   
  group_by(cuerpo_s_localizado_s) %>% 
  summarise(total = sum(numero_de_homicidios_total))

# Homicidios - ataque armado
df_son_ant_atack_ho <- df_sonora_antes %>% 
  select(numero_de_homicidios_total, ataque_armado) %>%   
  group_by(ataque_armado) %>% 
  summarise(total = sum(numero_de_homicidios_total))

# Heridas - ataque armado
df_son_ant_atack_he <- df_sonora_antes %>% 
  select(numero_de_heridos_as_total, ataque_armado) %>%   
  group_by(ataque_armado) %>% 
  summarise(total = sum(numero_de_heridos_as_total))

# Sin localizar
df_sonora_sloca <- df_sonora_antes %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Figuras 
df_figura_antes <- df_sonora_antes    %>% 
    select(estado, 
         fecha      = fecha_de_los_hechos, 
         homicidios = numero_de_homicidios_total, 
         heridos    = numero_de_heridos_as_total, 
         privadas   = numero_de_personas_privadas_de_su_libertad) %>% 
  # Agrupar para sacar el total por día por entidad 
  group_by(estado, fecha) %>% 
  summarise(across(where(is.numeric), ~sum(., na.rm = T)))        %>% 
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
    evento == "privadas"   ~ "Personas desaparecidas"), 
    total = as.integer(total))

# Tema
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

# Fuentes
v_caption <- "Fuente: Elaboracón propia con datos del Monitor-PPD 2022"

# Gráficas 
ggplot(
  # Datos
  df_figura_antes, 
  # Coordenadas
  aes(x = fecha, y = total, group = evento, color = evento)) +
  facet_wrap(~evento, ncol = 1) +
  # Geoms
  geom_line() +
  geom_point() +
  # Etiquetas
  labs(
    # title    = "Eventos delictivos ocurridos en Sonora", 
    subtitle = "Diario y por tipo de evento\n", 
    x = "", 
    y = "Número total de eventos\n", 
    color = "", 
    caption = v_caption
  ) +
  # Escalas
  scale_y_continuous(limits = c(0, 6.5)) +
  # Tema
  tema +
  theme(legend.position = "none")

ggsave(file = paste_fig("antes_quintero.png"), 
       width = 5, height = 4)

# Después de Quintero 
## Datos del 15 al 30 de junio 
df_sonora_des <- df_sonora %>% 
  filter(fecha_de_los_hechos >= '2022-06-15') %>% 
  filter(fecha_de_los_hechos <= '2022-06-30')

# Homicidios 
df_son_des_hom <- df_sonora_des %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total))

# Frecuencia de homicidios por Total, Hombres y Mujeres 
df_data_hom_des <- df_son_des_hom %>% 
  group_by(evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Heridxs 
df_son_des_her <- df_sonora_des %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total))

# Frecuencia de heridas por Total, Hombres y Mujeres 
df_data_he_des <- df_son_des_her %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Frecuencia de homicidios por municipio
df_son_des_hom_est <- df_sonora_des %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total))

# Homicidios - localizado
df_son_des_loc <- df_sonora_des %>% 
  select(numero_de_homicidios_total, cuerpo_s_localizado_s) %>%   
  group_by(cuerpo_s_localizado_s) %>% 
  summarise(total = sum(numero_de_homicidios_total))

# Homicidios - ataque armado
df_son_des_atack_ho <- df_sonora_des %>% 
  select(numero_de_homicidios_total, ataque_armado) %>%   
  drop_na(ataque_armado) %>% 
  group_by(ataque_armado) %>% 
  summarise(total = sum(numero_de_homicidios_total, na.rm = T))

# Heridas - ataque armado
df_son_des_atack_he <- df_sonora_des %>% 
  select(numero_de_heridos_as_total, ataque_armado) %>% 
  drop_na(ataque_armado) %>% 
  group_by(ataque_armado) %>% 
  summarise(total = sum(numero_de_heridos_as_total, na.rm = T))

# Sin localizar
df_sonora_sloca_des <- df_sonora_des %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Figuras 
df_figura_des <- df_sonora_des    %>% 
  select(estado, 
         fecha      = fecha_de_los_hechos, 
         homicidios = numero_de_homicidios_total, 
         heridos    = numero_de_heridos_as_total, 
         privadas   = numero_de_personas_privadas_de_su_libertad) %>% 
  # Agrupar para sacar el total por día por entidad 
  group_by(estado, fecha) %>% 
  summarise(across(where(is.numeric), ~sum(., na.rm = T)))        %>% 
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
    evento == "privadas"   ~ "Personas desaparecidas"), 
    total = as.integer(total))

# Tema
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

# Fuentes
v_caption <- "Fuente: Elaboracón propia con datos del Monitor-PPD 2022"

# Gráficas 
ggplot(
  # Datos
  df_figura_des, 
  # Coordenadas
  aes(x = fecha, y = total, group = evento, color = evento)) +
  facet_wrap(~evento, ncol = 1) +
  # Geoms
  geom_line() +
  geom_point() +
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
  scale_y_continuous(limits = c(0, 6.5)) +
  # Tema
  tema +
  theme(legend.position = "none")

ggsave(file = paste_fig("despues_quintero.png"), 
       width = 5, height = 4)
