#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento de info para figuras
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          21 de julio de 2022
# Última actualización:       21 de julio de 2022
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
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos --------------------------------------------------------------
load(paste_inp("df_monitor.Rdata"))

# 2. Figuras -------------------------------------------------------------------

## 2.0. Configuración de figuras------------------------------------------------ 
v_caption <- "Fuente: Elaboracón propia con datos del Monitor-PPD 2022"

## 2.1. Homicidios por municipios----------------------------------------------- 
df_data1 <- df_monitor %>% 
  filter(estado == "Sonora-26") %>% 
  select(-c(id:sobre_la_nota), -enlace) %>% 
  filter(numero_de_homicidios_total>0) %>% 
  group_by(municipio) %>% 
  summarise(homicidio_total = sum(numero_de_homicidios_total, na.rm = T))

ggplot(df_data1, 
       aes(x = reorder(municipio, homicidio_total), y = homicidio_total)) + 
# Geoms
    geom_col() +
    geom_text(aes(label = homicidio_total), vjust = -0.3) +
# Etiqueta
labs(
  title = "Total de homicidios en Sonora",
  subtitle = "Por municipio junio 2022", 
  x = "Municipio",
  y = "Número de homicidios", 
  caption = v_caption) + 
# Escalas
# Tema 
theme_bw() + 
  theme(legend.position = "none")

ggsave(file = paste_fig("sonora_mup_homi.png"), 
       width = 10, height = 6)

# ---- Bucle

v_entidades <- unique(df_monitor$estado) 


for(i in 1:length(v_entidades)){
  df_data1 <- df_monitor %>% 
    filter(estado == v_entidades[i]) %>% 
    select(-c(id:sobre_la_nota), -enlace) %>% 
    filter(numero_de_homicidios_total>0) %>% 
    group_by(municipio) %>% 
    summarise(homicidio_total = sum(numero_de_homicidios_total, na.rm = T))
  
  ggplot(
    # Datos
    df_data1, 
    # Coordenadas
    aes(x = reorder(municipio, homicidio_total), y = homicidio_total)) + 
    coord_flip() +
    # Geoms
    geom_col() +
    geom_text(aes(label = homicidio_total), hjust = -0.3) +
    # Etiqueta
    labs(
      title = paste0("Total de homicidios en ", v_entidades[i]),
      subtitle = "Por municipio junio a 15 de agosto 2022", 
      x = "Municipio",
      y = "Número de homicidios", 
      caption = v_caption) + 
    # Escalas
    # Tema 
    theme_bw() + 
    theme(legend.position = "none")
  
  ggsave(file = paste_fig(paste0(v_entidades[i], "_mup_homi.png")), 
         width = 10, height = 6)
  
}
## 2.2. Homicidios por entidad--------------------------------------------------
df_data2 <- df_monitor %>% 
  select(-c(id:sobre_la_nota), -enlace) %>% 
  filter(numero_de_homicidios_total>0) %>% 
  group_by(estado) %>% 
  summarise(homicidio_total = sum(numero_de_homicidios_total, na.rm = T)) %>% 
  drop_na()


ggplot(df_data2, 
  aes(x = reorder(estado, homicidio_total), y = homicidio_total)) + 
  # Geoms
  geom_col() +
  geom_text(aes(label = homicidio_total), vjust = +0.4, hjust = -0.2) +
  coord_flip() +
  # Etiqueta
  labs(
    title = "Homicidios en México",
    subtitle = "Por estado de junio al 15 de agosto de 2022", 
    x = "Estado",
    y = "Número de homicidios", 
    caption = v_caption) + 
  # Escalas
  # Tema 
  theme_bw() + 
  theme(legend.position = "none")

ggsave(file = paste_fig("estados_homi.png"), 
       width = 10, height = 6)


## 2.2. Presencia criminal por entidad------------------------------------------ 

