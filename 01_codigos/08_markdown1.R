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

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

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

# 3. Homicidios----
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

ggsave(file = paste_fig("map_homicidios.png"), 
       width = 10, height = 6)

# figura: tabla 
tabla_homi <- df_crudo %>% 
  select(homicidio_t, homicidio_v, homicidio_m, 
         heridos_t, heridos_v, heridos_m,       
         detenidos_t, detenidos_v, detenidos_m) %>% 
  summarize_all(~sum(., na.rm = T)) %>% 
  pivot_longer(
    cols = homicidio_t:detenidos_m, 
    names_to = c("evento", "sexo"),
    names_sep = "_", 
    values_to = "total") %>% 
  mutate(
      sexo = case_when(
       sexo == "t"  ~ "Total", 
       sexo == "v"  ~ "Hombre",
       sexo == "m"  ~ "Mujer"),
      evento = case_when(
       evento == "detenidos" ~ "Detenidos(as)",
       evento == "heridos"   ~ "Heridos(as)",
       T ~ "Homicidios")) %>% 
  group_by(evento, sexo) %>% 
  summarise(total = sum(total)) %>% 
  pivot_wider(
    names_from = sexo, 
    values_from = total) %>% 
  arrange(desc(evento)) %>% 
  rename(Evento = evento)

# install.packages("kableextra") - RMarkdown 

# 4. Tipos de homicidios----
# base - cuerpos
tip_cuerpo$cuerpos <- as.character(tip_cuerpo$cuerpos)

tip_cuerpo <- df_crudo %>% 
  select(homicidio_t, cuerpos) %>% 
  filter(cuerpos == "TRUE") %>% 
  mutate(cuerpos = case_when(
          cuerpos == "TRUE" ~ "cuerpos localizados")) %>% 
  group_by(cuerpos) %>% 
  summarise(total_cuerpos = sum(homicidio_t, na.rm = T)) %>% 
  rename(tipo_agresion = cuerpos,
         total_homicidios = total_cuerpos)

# base - ataque
tip_ataque <- df_crudo %>% 
  select(homicidio_t, 
         ataque,
         cuerpos) %>% 
  filter(homicidio_t == "1") %>% 
  filter(cuerpos == "FALSE") %>% 
  select(!cuerpos)

# Se categorizan las obversaciones
unique(tip_ataque2$ataque)

v_fuego <- c("agresión por policias", "agresión en penal", "ataque de motosicarios",
             "agresión; feminicidio", "agresión", "agresión en anexo", "discusión",
             "robo", "agresión en anexo", "agresión en casa", "robo de camioneta",
             "robo a casa", "robo; agresión", "intento de robo", 
             "venta de camioneta por facebook")

v_enfrenta <- c("enfrentamiento")

v_blanca <- c("ataque con arma blanca", "riña; arma blanca", 
              "agresión con arma blanca", "arma blanca")

v_golpes <- c("riña", "golpes", "golpeado")

v_otros <- c("bomba molotov", "quemada", "narcobloqueos")

tip_ataque2 <- tip_ataque %>% 
  mutate(
    ataque = case_when(
      ataque %in% v_fuego ~ "arma de fuego",
      ataque %in% v_enfrenta ~ "enfrentamiento",
      ataque %in% v_blanca ~ "arma blanca", 
      ataque %in% v_golpes ~ "golpes",
      ataque %in% v_otros ~ "otros",
      ataque == ataque ~ ataque)) %>% 
  group_by(ataque) %>% 
  summarise(total_homicidios = sum(homicidio_t)) %>% 
  drop_na(ataque) %>% 
  arrange(desc(total_homicidios)) %>% 
  rename(tipo_agresion = ataque)

# Base cuerpos + ataque2
tipo_hom <- tip_ataque2 %>% 
  bind_rows(tip_cuerpo) %>% 
  arrange(desc(total_homicidios))

# figura: gráfica 
ggplot(tipo_hom, 
       aes(x = reorder(tipo_agresion, total_homicidios), y = total_homicidios)) + 
       # Geoms
       geom_col() +
       geom_text(aes(label = total_homicidios), vjust = +0.4, hjust = -0.2) +
       coord_flip() +
       # Etiqueta
       labs(
            title = "Homicidios violentos en México",
            subtitle = "Por tipo de evento de junio a agosto de 2022", 
            x = "Tipo de evento",
            y = "Número de homicidios", 
            caption = "Fuente: Monitor-PPD (2022)") + 
       # Escalas
       # Tema 
       theme_bw() + 
       theme(legend.position = "none")

# 5. Grupos criminales----
df_gc <- df_crudo %>% 
  select(estado, grupo, grupo_a, grupo_r)
  
