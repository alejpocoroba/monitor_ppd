#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Mapas y tablas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          09 de noviembre de 2022
# Última actualización:       09 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, mxmaps, sf, beepr)


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
## Mapas ----

df_corto <- df_monitor_amplio %>% 
  # variables de interés
  select(estado, nombre_del_grupo_criminal_gc, alianza_del_gc, rival_del_gc,
         numero_de_homicidios_total, numero_de_homicidios_hombre,
         numero_de_homicidios_mujer, 
         numero_de_heridos_as_total, numero_de_heridos_hombres, 
         numero_de_heridas_mujeres,
         numero_de_detenidos_as_total, numero_de_detenidos_hombres, 
         numero_de_detenidas_mujeres,
         ataque_armado, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad,
         autoridad_militar, tipo_de_actividad_militar,
         autoridad_civil, tipo_de_actividad_civil) 

# Homicidios 
df_homicidios <- df_monitor_amplio %>% 
  # variables de interés
  select(estado, numero_de_homicidios_total, numero_de_homicidios_hombre,
         numero_de_homicidios_mujer) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(numero_de_homicidios_total, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  left_join(mxstate.map)

# Grupos criminales 
df_grupos <- df_corto %>% 
  # variables de interés
  select(estado, nombre_del_grupo_criminal_gc) %>% 
  group_by(estado, nombre_del_grupo_criminal_gc) %>% 
  summarize(total_grupos = n()) %>% 
  drop_na(nombre_del_grupo_criminal_gc) %>% 
  group_by(estado) %>% 
  summarize(total_grupos = n()) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  left_join(mxstate.map)

# Desaparecidos 
df_desparecidos <- df_corto %>% 
  # variables de interés
  select(estado, numero_de_personas_privadas_de_su_libertad) %>% 
  group_by(estado) %>% 
  summarize(total_desparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  left_join(mxstate.map)

#  Figura 
cols <- RColorBrewer::brewer.pal(3,'Blues')[c(1,3)]

ggplot(
  # Datos
  df_desparecidos,
  # Coordenadas
  aes(long, lat, group = group, fill = total_desparecidos)) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Personsas desaparecidas y/ sin localizar en México",
       subtitle = "Entre junio, julio y agosto del 2022\n",
       fill = "Número", 
       caption = "Fuente: Monitor-PPD (2022)") +
  scale_fill_gradient(low=cols[1],high=cols[2]) +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = NA)        
        )

ggsave(file = paste_fig("map_desaparecidos.png"), 
       width = 10, height = 6)
  
## Tablas----
df_corto1 <- df_monitor_amplio %>% 
  # variables de interés
  select(numero_de_homicidios_total, 
         numero_de_homicidios_hombre,
         numero_de_homicidios_mujer, 
         numero_de_heridos_total = numero_de_heridos_as_total,
         numero_de_heridos_hombres, 
         numero_de_heridas_mujeres,
         numero_de_detenidos_total = numero_de_detenidos_as_total,
         numero_de_detenidos_hombres, 
         numero_de_detenidas_mujeres) %>% 
  summarize_all(~sum(., na.rm = T))   %>% 
  rename_all(~stringr::str_replace(.,"^numero_de_","")) %>% 
  pivot_longer(
    cols = homicidios_total:detenidas_mujeres, 
    names_to = c("evento", "sexo"),
    names_sep = "_", 
    values_to = "total"
  ) %>% 
  mutate(
    # sexo = if_else(sexo == "as", "total", sexo)
    sexo = case_when(
      sexo == "as"    ~ "Total", 
      sexo == "total" ~ "Total", 
      str_detect(sexo, "hom") ~ "Hombres", 
      str_detect(sexo, "muj") ~ "Mujeres"), 
    evento = case_when(
      str_detect(evento, "det") ~ "Detenidos(as)" ,
      str_detect(evento, "her") ~ "Heridos(as)" ,
      T ~ "Homicidios"
          )
    ) %>% 
  group_by(evento, sexo) %>% 
  summarise(total = sum(total)) %>% 
  pivot_wider(
    names_from = sexo, 
    values_from = total
  ) %>% 
  arrange(desc(evento)) %>% 
  rename(Evento = evento)



install.packages("kableextra")

