#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Limpieza para R Markdown
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          09 de noviembre de 2022
# Última actualización:       14 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, 
  srvyr, lubridate, zoo, 
  ggtext, mxmaps, sf, 
  viridis, beepr)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Procesar datos------
df_crudo <- df_monitor_amplio %>% 
  # variables de interés
  select(publicacion = fecha_de_publicacion,
         estado, 
         grupo           = nombre_del_grupo_criminal_gc, 
         grupo_a         = alianza_del_gc, 
         grupo_r         = rival_del_gc,
         homicidio_t     = numero_de_homicidios_total, 
         homicidio_v     = numero_de_homicidios_hombre,
         homicidio_m     = numero_de_homicidios_mujer,
         heridos_t       = numero_de_heridos_as_total, 
         heridos_v       = numero_de_heridos_hombres,
         heridos_m       = numero_de_heridas_mujeres,
         detenidos_t     = numero_de_detenidos_as_total, 
         detenidos_v     = numero_de_detenidos_hombres,
         detenidos_m     = numero_de_detenidas_mujeres,
         ataque          = ataque_armado, 
         cuerpos         = cuerpo_s_localizado_s, 
         desaparecidos   = privacion_de_la_libertad, 
         desaparecidos_n = numero_de_personas_privadas_de_su_libertad,
         militar         = autoridad_militar, 
         militar_a       = tipo_de_actividad_militar,
         civil           = autoridad_civil, 
         civil_a         = tipo_de_actividad_civil) %>% 
         # fecha para el reporte: julio, agosto y septiembre 2022 
         filter(publicacion >= '2022-06-01') %>% 
         filter(publicacion <= '2022-08-31')

# 3. Homicidios 
# procesamiento de los datos
homi_mapa <- df_crudo %>% 
  # variables de interés
  select(estado, 
         homicidio_t, 
         homicidio_v,
         homicidio_m) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(homicidio_t, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  # datos geo
  left_join(mxstate.map)

# figura: mapa
cols <- RColorBrewer::brewer.pal(3,'Blues')[c(1,3)]

ggplot(
  # Datos
  homi_mapa,
  # Coordenadas
  aes(long, lat, group = group, fill = total_homicidios), size = 0, alpha = 0.9) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Homicidios violentos en México",
       subtitle = "De julio, agosto y septiembre del 2022\n",
       fill = "Número de homicidios", 
       caption = "Fuente: Monitor-PPD (2022)") +
  scale_fill_viridis_b(breaks = c(200, 300, 400, 500, 600)) +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))


