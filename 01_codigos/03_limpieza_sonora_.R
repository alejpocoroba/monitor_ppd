#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Sonora: antes y después de Quintero
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          26 de septiembre de 2022
# Última actualización:       26 de octubre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------

# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, beepr)

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza de los datos------------------------------------------------------

## Datos de Sonora--------------------------------------------------------------
df_sonora <- df_monitor_amplio %>% 
  filter(estado == "Sonora-26")

### General---------------------------------------------------------------------
# Se depuran las fechas anteriores a 01/06/2022 y después a 01/09
df_sonora_gen <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-05-31") %>% 
  filter(fecha_de_los_hechos <= "2022-08-31")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_gen_h1 <- df_sonora_gen %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_gen_h2 <- df_sonora_gen_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_gen_h3 <- df_sonora_gen %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_gen_h4 <- df_sonora_gen %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_gen_h5 <- df_sonora_gen %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))
  
#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_gen_he1 <- df_sonora_gen %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_gen_he2 <- df_sonora_gen_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_gen_he3 <- df_sonora_gen %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_gen_he4 <- df_sonora_gen %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_gen_slo <- df_sonora_gen %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_gen_slo$total)


### Antes de la detención-------------------------------------------------------
# Datos del 01 al 14 de julio
df_sonora_ant <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-05-31") %>% 
  filter(fecha_de_los_hechos <= "2022-07-14")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_ant_h1 <- df_sonora_ant %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

hom_agre_ant <- df_sonora_ant %>% 
  filter(numero_de_homicidios_total >= "1") 

colnames(df_sonora_ant)

# Cuerpos
hom_cuerpos_ant <- hom_agre_ant %>% 
  select(numero_de_homicidios_total,  cuerpo_s_localizado_s) %>% 
  group_by(cuerpo_s_localizado_s) %>% 
  summarise(total = sum(numero_de_homicidios_total, na.rm = T))


hom_cuerpos_ant <- hom_agre_ant %>% 
  select(numero_de_homicidios_total,  cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE")

sum(hom_cuerpos_ant$numero_de_homicidios_total)

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_ant_h2 <- df_sonora_ant_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))


# Por municipio 3
df_sonora_ant_h3 <- df_sonora_ant %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_ant_h4 <- df_sonora_ant %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_ant_h5 <- df_sonora_ant %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_ant_he1 <- df_sonora_ant %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_ant_he2 <- df_sonora_ant_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_ant_he3 <- df_sonora_ant %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_ant_he4 <- df_sonora_ant %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_ant_slo <- df_sonora_ant %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_ant_slo$total)

### Después de la detención-------------------------------------------------------
# Datos del 15 de julio al 31 de agosto
df_sonora_des <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-07-14") %>% 
  filter(fecha_de_los_hechos <= "2022-08-31")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_des_h1 <- df_sonora_des %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_des_h2 <- df_sonora_des_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_des_h3 <- df_sonora_des %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_des_h4 <- df_sonora_des %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_des_h5 <- df_sonora_des %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_des_he1 <- df_sonora_des %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_des_he2 <- df_sonora_des_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Frecuencia por grupo criminal - agresión
df_gp_he <- df_sonora_des %>% 
  select(numero_de_heridos_as_total, herido_a_pertenece_a, fecha_de_los_hechos, ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  group_by(fecha_de_los_hechos, ataque_armado) %>% 
  summarise(total = sum(numero_de_heridos_as_total))
# Total de heridos por agresión: 31
sum(df_gp_he$total)

# Frecuencia por grupo criminal - enfrentamiento
df_gp_he <- df_sonora_des %>% 
  select(numero_de_heridos_as_total, herido_a_pertenece_a, fecha_de_los_hechos, 
         ataque_armado, autoridad_militar, autoridad_civil) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  group_by(fecha_de_los_hechos, ataque_armado) %>% 
  summarise(total = sum(numero_de_heridos_as_total))
# Total de heridos por enfrentamiento: 31
sum(df_gp_he$total)


colnames(df_sonora_des)


# Por municipio 3
df_sonora_des_he3 <- df_sonora_des %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_des_he4 <- df_sonora_des %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_des_slo <- df_sonora_des %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_des_slo$total)

### Figuras--------------------------------------------------------------------- 

# Base completa para gráficar 
df_limpio <- df_monitor_amplio              %>% 
  # Renombrar
  mutate(year = year(fecha_de_los_hechos), 
         mes  = month(fecha_de_los_hechos),
         semana = week(fecha_de_los_hechos)
  ) %>% 
  select(estado, year, mes, semana, 
         fecha      = fecha_de_los_hechos, 
         homicidios = numero_de_homicidios_total, 
         heridos    = numero_de_heridos_as_total, 
         privadas   = numero_de_personas_privadas_de_su_libertad) %>% 
  # Agrupar para sacar el total por día por entidad 
  group_by(estado, fecha) %>% 
  summarise(across(where(is.numeric), ~sum(., na.rm = T)))        %>% 
  # Dejar solo entidades de interés 
  filter(str_detect(estado, "Sonora")) %>% 
  # Cambiar a formato largo 
  pivot_longer(
    cols      = c(homicidios:privadas), 
    names_to  = "evento", 
    values_to = "total") %>% 
  # Renombrar eventos 
  mutate(evento = case_when(
    evento == "homicidios" ~ "Homicidios", 
    evento == "heridos"    ~ "Personas heridas", 
    evento == "privadas"   ~ "Personas no localizadas"), 
    total = as.integer(total), 
    arresto_caro     = if_else(fecha == as.Date("2022-07-15"), 1, 0)) %>% 
  # filter(total != 0) %>% 
  glimpse()

df_limpio_semana <- df_limpio       %>% 
  group_by(estado, semana, evento)  %>% 
  summarise(total = sum(total))

#### Configuración de figuras -----------------------------------------------

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

#### Sonora arresto de Caro Quintero -------------------------------------------

# ---- Fecha de arresto de caro quintero
v_arresto <- unique(df_limpio$fecha[df_limpio$arresto_caro == 1])

# ---- Filtrar datos 
df_data <- df_limpio %>% filter(str_detect(estado, "Sonora"), 
                                !str_detect(evento, "detenidas"),
                                # !str_detect(evento, "heridas"), 
                                fecha >= as.Date("2022-06-01"),
                                fecha <= as.Date("2022-08-31")) 

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
  # scale_x_date(breaks = unique(df_data$fecha)) +
  # Tema
  tema +
  theme(legend.position = "none")

# ---- Guardar figura
ggsave(file = paste_fig("quintero.png"), 
       width = 6, height = 5)