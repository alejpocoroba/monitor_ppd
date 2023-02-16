#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Procesamiento para entrega 2022 
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          15 de febrero de 2023
# Última actualización:       15 de febrero de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, stringr, zoo, ggtext, beepr)

library(mxmaps) # Para los mapas

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# ---- Tema para gráficas 
# tema <- theme_bw() 

tema        <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    # plot.caption.position = "plot",
    text                  = element_text(family = "Source Sans Pro", color = "#6F7271"),
    plot.title            = element_text(family = "Source Sans Pro", color = "#6F7271",   size = 16,  face  = "bold",  margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Source Sans Pro", color = "#98989A",   size = 14,  margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Gotham", color = "#6F7271", size = 10,  hjust = 1),
    panel.grid            = element_line(linetype = 2),
    plot.margin           = margin(0, 2, 0, 1.5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size = 11, family = "Gotham", face   = "bold"),
    legend.text           = element_text(size = 11, family = "Gotham"),
    axis.title            = element_text(size = 11, family = "Gotham", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 11, family = "Gotham", color = "#6F7271", angle=0,  hjust=1),
    axis.text.x           = element_text(size = 11, family = "Gotham", color = "#6F7271", angle=90, hjust=1, vjust = 0.5),
    strip.text.x          = element_text(size = 11, family = "Gotham", face = "bold", color = "#6F7271"),
    strip.text.y          = element_text(size = 11, family = "Gotham", face = "bold", color = "#6F7271"),
    strip.background      = element_rect(fill = "white", color = NA))

v_caption <- "Fuente: Monitor- PPD (2022)\n"

# 1. Cargar datos --------------------------------------------------------------

df_crudo <- read_xlsx(paste_inp("base_monitorPPD_2sem2022.xlsx"))


# 2. Exploración de base -------------------------------------------------------

# View(table(df_crudo$id)) # Hay dos IDs con el número 99 
names(df_crudo)

# 3. Homicidios ----------------------------------------------------------------

# Seleccionar variables de interés
df_homicidios <- df_crudo %>% 
  mutate(
    mes     = lubridate::month(fecha_de_publicacion),
    year_m  = zoo::as.yearmon(paste0(anio, "-", mes)),
    mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  select(
    id, fecha = fecha_de_publicacion, year_m, anio, mes, pais, estado, municipio, 
    CVEGEO, starts_with("homic"), starts_with("cuerpos"), contains("ataque"), 
    homicidio, contains("arma")) %>% 
  # Filtrar para periodo de interés 
  filter(mes != "ene")


# Ver valores de variables de interés 
table(df_homicidios$homic_total)   # Variable numérica 
table(df_homicidios$homic_clasif1) # Variable categórica
table(df_homicidios$homic_clasif2) # Variable categórica
table(df_homicidios$homic_alias)   # Variable boleana
table(df_homicidios$homicidio)     # Variable binaria
table(df_homicidios$agresiónataque) # Variable boleana
table(df_homicidios$cuerpos_localizados)
table(df_homicidios$ataque_armado_clean) ### Variable categórica
table(df_homicidios$lugar_ataque_clean) # Variable vacía 
table(df_homicidios$portacion_trafico_armas)

## 3.1. General ----------------------------------------------------------------

#### Mapa de calor -------------------------------------------------------------

# procesamiento de los datos
df_map1 <- df_homicidios %>% 
  # variables de interés
  select(estado, 
         homic_total) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(homic_total, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  # datos geo
  left_join(mxstate.map, by = "region")

# figura: mapa
ggplot(
  # Datos
  df_map1,
  # Coordenadas
  aes(long, lat, group = group, fill = total_homicidios), size = 0, alpha = 0.9) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Homicidios violentos en México",
       subtitle = "Julio - Diciembre 2022\n",
       fill = "Número", 
       caption = v_caption) +
  # scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  scale_fill_gradient(low = "white", high = "#d62828", na.value = "grey", 
                      label = scales::comma_format()) +
  # Tema
  tema +
  # Quitar fondo al mapa
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA)) +
  # Ampliar largo de la barra 
  theme(legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1,"cm")) 


# Guardar
ggsave(file = paste_fig("reporte_2022/01a_homicidios_mapa.png"))

#### Barras --------------------------------------------------------------------

# procesamiento de los datos
df_barras1 <- df_homicidios %>% 
  # variables de interés
  select(estado, homic_total) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(homic_total, na.rm = T)) %>% 
  mutate(
    region = str_sub(estado, -2, -1), 
    estado = str_remove_all(estado, "[:digit:]"),
    estado = str_remove_all(estado, "-"), 
    )  %>%
  drop_na() %>% 
  filter(estado != "20", region != "20")  

# figura: mapa
ggplot(df_barras1, 
       aes(x = total_homicidios, y = reorder(estado, total_homicidios))) +
  geom_col(fill = "#d62828") +
  geom_text(aes(label = scales::comma(total_homicidios)), vjust = +0.4, 
            hjust = if_else(df_barras1$total_homicidios<max(df_barras1$total_homicidios), -0.2, 1.2), 
            color = if_else(df_barras1$total_homicidios<max(df_barras1$total_homicidios), "#6F7271", "white"), 
            family = "Gotham") +
  # Etiquetas
  labs(title = "Homicidios violentos en México",
       subtitle = "Julio - Diciembre 2022\n",
       x = "", 
       y = "", 
       caption = v_caption) +
  scale_x_continuous(label = scales::comma_format()) +
  tema +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
# Guardar
ggsave(file = paste_fig("reporte_2022/01b_homicidios_barras.png"))

#### Total nacional al mes -----------------------------------------------------


# Procesar datos 
df_plot1 <- df_homicidios %>% 
  filter(homic_total > 0) %>% 
  group_by(year_m, mes)   %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  filter(mes != "ene") 

# Serie de tiempo 
ggplot(
  # Datos
  df_plot1, 
  # Coordenadas
  aes(x = year_m, y = total, group = 1)) +
  # Geoms
  geom_line(size = 2, color = "#98989A") +
  geom_point(size = 3, color = "#6F7271") +
  geom_text(aes(label = scales::comma(total)), 
            vjust = if_else(df_plot1$mes %in% c("jul", "nov"), 1.7, -1.2),
            family = "Gotham") +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format(), limits = c(1300, 2000)) +
  # scale_x_date(breaks = c(1:6)) +
  zoo::scale_x_yearmon(breaks = seq(min(df_plot1$year_m), max(df_plot1$year_m), 1)) + 
  # Tema
  tema
 
# zoo::scale_x_yearqtr(breaks = seq(min(d$fecha_t), max(d$fecha_t),1), 

# Guardar
ggsave(file = paste_fig("reporte_2022/02_homicidios_serie.png"))


# zoo::scale_x_yearqtr(breaks = seq(min(d$fecha_t), max(d$fecha_t),1), 


# # Serie de tiempo 
# ggplot(
#   # Datos
#   df_plot1, 
#   # Coordenadas
#   aes(x = year_m, y = total)) +
#   # Geoms
#   geom_area(fill = "#d62828") +
#   # Etiquetas
#   labs(
#     title = "Homicidios violentos en México", 
#     subtitle = "Julio - Diciembre 2022\n", 
#     x = "", 
#     y = "", 
#     caption = v_caption
#   ) +
#   # Etiquetas 
#   scale_y_continuous(label = scales::comma_format(), limits = c(0, 2000)) +
#   # Tema
#   tema
# 
# # Guardar
# ggsave(file = paste_fig("reporte_2022/02_homicidios_serie.png"))



#### Total estatal al mes ------------------------------------------------------

# Procesar datos 
df_plot2 <- df_homicidios %>% 
  filter(homic_total > 0) %>% 
  group_by(estado, year_m, mes)   %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  filter(mes != "ene") %>% 
  drop_na() %>% 
  filter(estado != "20") %>% 
  mutate(
    estado = str_remove_all(estado, "[:digit:]"),
    estado = str_remove_all(estado, "-")
  )


# ---- Versión  vertical con eje y libre
# Serie de tiempo 
ggplot(
  # Datos
  df_plot2, 
  # Coordenadas
  aes(x = mes, y = total, group = 1)) +
  facet_wrap(~estado, scale = "free", ncol = 4) +
  # Geoms
  geom_line(size = 1.2, color = "#98989A") +
  geom_point(size = 2.2, color = "#6F7271") +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format()) +
  # Tema
  tema

# Guardar
ggsave(file = paste_fig("reporte_2022/03a_homicidios_serie_estados.png"), 
       width = 10, height = 12)


# ---- Versión  horizontal con eje y libre
# Serie de tiempo 
ggplot(
  # Datos
  df_plot2, 
  # Coordenadas
  aes(x = mes, y = total, group = 1)) +
  facet_wrap(~estado, scale = "free", ncol = 8, labeller = label_wrap_gen(width = 16)) +
  # Geoms
  geom_line(size = 1, color = "#98989A") +
  geom_point(size = 2, color = "#6F7271") +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format()) +
  # Tema
  tema

# Guardar
ggsave(file = paste_fig("reporte_2022/03b_homicidios_serie_estados.png"), 
       width = 15, height = 8)

# ---- Versión  horizontal con eje y libre
# Serie de tiempo 
ggplot(
  # Datos
  df_plot2, 
  # Coordenadas
  aes(x = mes, y = total, group = 1)) +
  facet_wrap(~estado, scale = "free_x", ncol = 8, labeller = label_wrap_gen(width = 16)) +
  # Geoms
  geom_line(size = 1, color = "#98989A") +
  geom_point(size = 2, color = "#6F7271") +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format()) +
  # Tema
  tema

# Guardar
ggsave(file = paste_fig("reporte_2022/03c_homicidios_serie_estados.png"), 
       width = 15, height = 8)



# ---- Versión  spaghetti
# Valores más altos en diciembre
df_top3 <- df_plot2 %>% 
  ungroup() %>% 
  filter(mes == "dic") %>% 
  arrange(desc(total)) %>% 
  slice(1:3) %>% 
  glimpse()

v_max <- df_top3$estado

# Serie de tiempo 
ggplot(
  # Datos
  df_plot2, 
  # Coordenadas
  aes(x = mes, y = total, group = estado)) +
  # Geoms
  geom_line(size = 1, color = "#98989A") +
  geom_point(size = 2, color = "#6F7271") +
  geom_text(
    aes(label = if_else((df_plot2$estado %in% v_max) & df_plot2$mes == "dic", 
                    estado, NA_character_)), family = "Gotham", color = "black", 
    nudge_x = 0.2, nudge_y = 8) +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format()) +
  # Tema
  tema

# Guardar
ggsave(file = paste_fig("reporte_2022/03d_homicidios_serie_estados.png"))




## 3.2 Homicidos por criterios -------------------------------------------------


#### Nacional ------------------------------------------------------------------

# PREGUNTA PARA MAILOBSKY: La variable ataque_armado_clean tiene algunos de los 
# eventos incluidos en la gráfica del reporte, pero no los incluye todos y 
# también incluye valores nuevos. 

v_eventos <- unique(df_homicidios$ataque_armado_clean) # 12 tipos de eventos

table(df_homicidios$ataque_armado_clean)


df_arma <- df_homicidios %>% 
  filter(ataque_armado_clean  == "agresión con arma de fuego")

table(df_arma$homic_total)

df_cuerpos <- df_homicidios     %>% 
  ungroup()                     %>% 
  summarise(total = sum(cuerpos_localizados, na.rm = TRUE)) %>% 
  mutate(evento = "Cuerpos localizados")
  
  
df_plot3 <- df_homicidios %>% 
  mutate(evento = ataque_armado_clean) %>% 
  group_by(evento) %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  # mutate(evento = if_else(total < 20, "Otros", evento)) %>% 
  group_by(evento) %>% 
  summarise(total = sum(total, na.rm = TRUE)) %>% 
  bind_rows(df_cuerpos) %>% 
  mutate(evento = str_to_title(evento))




# table(df_homicidios$cuerpos_localizados)

ggplot(
  # Datos 
  df_plot3, 
  # Coordenadas 
       aes(x = total, y = reorder(evento, total))) +
  # Geoms 
  geom_col() +
  geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
            hjust = if_else(df_plot3$total<max(df_plot3$total), -0.2, 1.2),
            color = if_else(df_plot3$total<max(df_plot3$total), "#6F7271", "white"), 
            family = "Gotham") +
            # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Por tipo de evento, Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema
  tema +
  theme(axis.text.x = element_text(angle = 0))


# Guardar
ggsave(file = paste_fig("reporte_2022/08a_homicidios_tipos.png"))

#### Estatal -------------------------------------------------------------------

# 
df_cuerpos_estados <- df_homicidios                         %>% 
  ungroup()                                                 %>% 
  group_by(estado)                                          %>% 
  summarise(total = sum(cuerpos_localizados, na.rm = TRUE)) %>% 
  mutate(evento = "Cuerpos localizados") %>% 
  filter(!is.na(estado), estado != "20") 


df_plot4 <- df_homicidios                                   %>% 
  mutate(evento = ataque_armado_clean)                      %>% 
  group_by(estado, evento)                                  %>% 
  summarise(total = sum(homic_total, na.rm = TRUE))         %>% 
  drop_na()                                                 %>% 
  ungroup()                                                 %>% 
  bind_rows(df_cuerpos_estados)                             %>% 
  mutate(evento = str_to_title(evento))                     %>% 
  filter(!is.na(estado), estado != "20")                    %>% 
  mutate(
    estado = str_remove_all(estado, "[:digit:]"))



# ggplot(
#   # Datos 
#   df_plot4, 
#   # Coordenadas 
#   aes(x = total, y = evento)) +
#   facet_wrap(~estado) +
#   # Geoms 
#   geom_col() +
#   geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
#             # hjust = if_else(df_plot3$total<max(df_plot3$total), -0.2, 1.2),
#             # color = if_else(df_plot3$total<max(df_plot3$total), "#6F7271", "white"), 
#             family = "Gotham") +
#   # Etiquetas
#   labs(
#     title = "Homicidios violentos en México", 
#     subtitle = "Por tipo de evento, Julio - Diciembre 2022\n", 
#     x = "", 
#     y = "", 
#     caption = v_caption
#   ) +
#   # Etiquetas 
#   scale_x_continuous(label = scales::comma_format()) +
#   # Tema
#   tema +
#   theme(axis.text.x = element_text(angle = 0))
# 
# ggsave(file = paste_fig("reporte_2022/08b_homicidios_tipos.png"), 
#        width = 15, height = 8)

ggplot(
  # Datos 
  df_plot4, 
  # Coordenadas 
  aes(x = total, y = estado)) +
  facet_wrap(~evento, ncol = 5, scales = "free_x", labeller = label_wrap_gen(width = 16)) +
  # Geoms 
  geom_col() +
  # geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
  #           family = "Gotham") +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Por tipo de evento, Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_x_continuous(label = scales::comma_format()) +
  scale_y_discrete(limits=rev) +
  # Tema
  tema +
  theme(axis.text.x = element_text(angle = 0))

ggsave(file = paste_fig("reporte_2022/08b_homicidios_tipos.png"), 
       width = 15, height = 15)


# 4. Grupos criminales ---------------------------------------------------------

# NOTA: LA base de grupos no está limpia porque no he limpiado los nombres, 
# pero tampoco he puesto los filtros de tiempo (que sea de junio a diciembre).

## 4.1. Separar grupos en distintas columnas -----------------------------------

# Identificar cuál es el máximo de grupos incluidos en una sola observación 
# dentro de la variable "grupo_criminal". Se hace contando los ";"
df_semicolon <- df_crudo %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    grupo_criminal = str_replace_all(grupo_criminal, ":", ";"), 
    # Contar número de puntos y comas (;)
    semicolon = str_count(grupo_criminal, ";")) %>% 
  select(id, semicolon) 

max(df_semicolon$semicolon, na.rm = T) # El máximo es 10 grupos criminales 

# Separar los nombres de los grupos criminales en columnas independientes 
df_nombres <- df_crudo %>% 
  mutate(grupo_criminal = str_replace_all(grupo_criminal, ":", ";")) %>% 
  separate(grupo_criminal, sep = ";", c("grupo1", 
                                        "grupo2", 
                                        "grupo3", 
                                        "grupo4", 
                                        "grupo5", 
                                        "grupo6", 
                                        "grupo7", 
                                        "grupo8",
                                        "grupo9", 
                                        "grupo10"))


## 4.2. Limpiar nombres --------------------------------------------------------

# ---- Agrupar todos los nombres distintos 
v_grupos <- unique(
  c(df_nombres$grupo1, df_nombres$grupo2, df_nombres$grupo3, df_nombres$grupo4, 
    df_nombres$grupo5, df_nombres$grupo6, df_nombres$grupo7, df_nombres$grupo8, 
    df_nombres$grupo9, df_nombres$grupo10))

# Hay muchos casos donde el nombre comienza con un espacio (" ")

# ---- Funciones de limpieza 

# Función para clasificar si es un grupo armado, autodefensas o un alias
clasificar_grupos <- function(x){

  # ---- Vectores con las clasificaciones
  v_gruposin <- c(
    "Sujetos armados", "Banda de homicidas en Perote", "célula delictiva", 
    "Narcomenudistas", "13 células delictivas", "Civiles Armados", "grupo armado", 
    "Sin especificar", "sin especificar")
  
  v_autodefensas <- c(
    "Columna Armada del General Pedro José Méndez", 
    'Columna Armada Pedro José Méndez',
    "Columna Armada Pedro José Méndez",
    "Resistencia Civil de Baja California", 
    "Autodefensa Fuerza Territorial Poblana", 
    "Fuerza Territorial Poblana", 
    "Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)",
    'Octavio L. alias "Tarzán" líder de autodefensas de Tamaulipas',
    " Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)")
  
  v_alias <- c(
    "Julio Adrián, también conocido como ‘El Mercy’", "‘El Pollo’",
    "‘El Smoll’", "El Panchito", "El Cholo", "Cipog-ez", "‘Zague’.",
    "Don Cabezón", "Oscar Ivan Maras", "el Payepas","El Damaso",
    "El Moyo", "El Jaibol", "“el Santa”", "‘Checo’", "Efraín, alias 'El Gato'",
    "“El Yogui”", "“El Kinkis”", "\"El Bocho\"", "El Vampi", 
    "“El Pillo”, apenas había tomado el poder en sustitución de recién capturado y peligroso líder narcomenudista “El Croquis”",
    "“El Bengala”", "El Ruso", "\"El Rana\" y \"El Gus\"", "La Zorra", 
    "El Colas","‘El Chuy’, ‘El Muerto’ y ‘El Jarro’", "El Grillo", 
    "“El Huevo”", "Fabián N alias \"El Pelón\"", "“La Pegui” y/o “La Sombra”",
    "‘El Carlitos’", "El Kaiman", "El Dany", "El Diablo", "\"El Negro\"",
    "El Komander", "“El Color\"", "“El Merla”", "“El Pelón”.", "el pistolas",
    "Cristian Alberto “N”, alias “El Bozo” o “El Alondra”", 
    "\"El Carnitas\"", "\"El Carolo\"", "el Cantinflas", "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"",
    "El Caimán", "La China", "el pepillo", "El Bolas", "El Bader", "El Tuerto", 
    "hermanos Barrera Estrada", "\"El Rata\"", "\"El Manzano\"", "“El Metales\"", 
    "“El Papirrín”", "El Perico", "“El Chavurras”", "El Cebollas", "Sierra 3", 
    "“El Pinky\"", "\"El Roy\"", "Leonardo León ‘N’, alias “El Vainilla”", 
    "El Dueñas", "‘El Chino’", "“El Mudo”", "El Tiko", "“El Parkita”", "\"El Hacha\"",
    "‘La Platita’ y ‘El Gargamel’", "Jesús Isaac “N”, alías “El Chamán”", "“El Pato”",
    "'El Lavadoras'", "“El Coco” y/o “El Tío”", "‘El Toro’", "“La Mariana” y/o “El Charly”",
    "La Má",  "El Toñín", "El Toñín", "El Richi", "Los Juanchos (líder El Pirata)",
    "Güicho el de Los Reyes", "El Tufito", "El Padrino Jr.", "el “Mulix”", "Toda la Sierra", 
    "líder huachicolero Antonio Martínez Fuentes, alias “El Toñín”.",  "El Gordo Fresa", "El Fantasma", 
    "El Pavón", "Guasón Poblano","El Guasón", "El Vago", "El Marrano", "El Trucha", 
    " “El Acople”", " El Fresa", "“El Camacho”")
  
  # Condicioneales para limpieza  
  case_when(
    x %in% v_gruposin     ~ "grupo armado", 
    x %in% v_autodefensas ~ "autodefensas", 
    x %in% v_alias        ~ "alias", 
    x  == x ~ x
  )
  
}

# Función para limpiar nombres de grupos armados

limpiar_grupos <- function(x){
  
  case_when(
    x  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
    x  == " La Línea" ~ "La Línea",
    x  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
    x  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
    x  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
    x  == "Los capetos"  ~ "Los Capetos",
    x  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
    x  == "Gente Nueva Salazar"  ~ "Los Salazar",
    x  == "La banda de “El Negro”"  ~ "Banda de El Negro",
    x  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
    x  == "Los salazar"  ~ "Los Salazar",
    x  == "Los Salazares"  ~ "Los Salazar", 
    x  == "Los Salazares"  ~ "Los Salazar",
    x  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
    x  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "Los pájaros sierra"  ~ "Pájaros Sierra",
    x  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
    x  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
    x  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
    x  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
    x  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
    x  == " El Bukanas"  ~ "Grupo de El Bukanas",
    x  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
    x  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
    x  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
    x  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
    x  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
    x  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
    x  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
    x  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
    x  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
    x  == "“Los Lampones\""  ~ "Los Lampones", 
    x  == "Unión Tepito"  ~ "La Unión Tepito",
    x  == "La familia Michoacana"  ~ "La Familia Michoacana",
    x  == " La Familia Michoacana"  ~ "La Familia Michoacana",
    x  == " Familia Michacana"  ~ "La Familia Michoacana",
    x  == " Familia Michoacana"  ~ "La Familia Michoacana",
    x  == "La empresa"  ~ "La Empresa",
    x  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
    x  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
    x  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
    x  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
    x  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
    x  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
    x  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
    x  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
    x  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
    x  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
    x  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
    x  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
    x  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
    x  == "Escorpiones" ~ "Grupo Scorpion",
    x  == "La Plaza" ~ "Cártel La Plaza",
    x  == "los viagras" ~ "Los Viagras",
    x  == " Los Viagras" ~ "Los Viagras",
    x  == "Los correa" ~ "Cártel de los Correa",
    x  == "Los Correa" ~ "Cártel de los Correa",
    x  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
    x  == " los caballeros templarios" ~ "Caballeros Templarios",
    x  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
    x  == x ~ x
  )
  
}

# ---- Recodificar nombres con funciones 
df_nombres_limpios <- df_nombres %>% 
  # Crear variables con tipo de clasificación 
  mutate(across(starts_with("grupo"), ~clasificar_grupos(.))) %>% 
  # Limpiar nombres específicos de grupos criminales
  mutate(across(starts_with("grupo"), ~limpiar_grupos(.)))  %>% 
  # Quitar espacios en blanco al inicio y final (función trimws)
  mutate(across(starts_with("grupo"), ~trimws(.))) %>% 
  # Convertir autodefensas, alias y guardia civil a NAs
  mutate(across(starts_with("grupo"), ~if_else(. %in% c("autodefensas", 
                                                        "alias", 
                                                        "Guardia Civil"), 
                                               NA_character_, .))) 

# ---- Agrupar todos los nombres distintos después de la limpieza
v_grupos_limpios <- unique(
  c(df_nombres_limpios$grupo1, df_nombres_limpios$grupo2, 
    df_nombres_limpios$grupo3, df_nombres_limpios$grupo4, 
    df_nombres_limpios$grupo5, df_nombres_limpios$grupo6, 
    df_nombres_limpios$grupo7, df_nombres_limpios$grupo8, 
    df_nombres_limpios$grupo9, df_nombres_limpios$grupo10))

## 4.3. Mapa -------------------------------------------------------------------

# ---- Contar grupos criminales distintos por entidadad federativa

# Quitar nombres dañados de entidades
df_estados <- df_nombres_limpios %>% 
  filter(!is.na(estado), estado != "20") %>% 
  # Filtrar periodo de tiempo 
  mutate(
    mes     = lubridate::month(fecha_de_publicacion),
    year_m  = zoo::as.yearmon(paste0(anio, "-", mes)),
    mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  filter(mes != "ene") 
  
# Guardar los nombres de todas las entidades
v_estados <- unique(df_estados$estado)

# Data frame vacío para pegar resultados 
df_grupos_entidad <- data.frame()

# Bucle de limpieza por estado 
for(i in 1:length(v_estados)){
  
  # Imprimir vuelta 
  print(paste0("Vuelta ", i, ": ", v_estados[i]))
  
  # Filtrar base y dejar solo una entidad
  df_entidad <- df_estados                  %>% 
    filter(estado == v_estados[i])
  
  # Guardar un vector con los nombres únicos de grupo por entidad
  v_grupos_estado <- unique(
    c(df_entidad$grupo1, df_entidad$grupo2, 
      df_entidad$grupo3, df_entidad$grupo4, 
      df_entidad$grupo5, df_entidad$grupo6, 
      df_entidad$grupo7, df_entidad$grupo8, 
      df_entidad$grupo9, df_entidad$grupo10)) 
  
  # Quitar valor NA
  v_grupos_estado <- v_grupos_estado[!is.na(v_grupos_estado)]
  
  # Guardar información en un data frame
  df_grupos_entidad <- df_grupos_entidad    %>% 
    bind_rows(data.frame(estado = v_estados[i], 
                         total_grupos = length(v_grupos_estado)))
  
}

# View(df_grupos_entidad)


# ---- Pegar datos geo 
df_mapa2 <- df_grupos_entidad               %>% 
  mutate(region = str_sub(estado, -2, -1))  %>% 
  left_join(mxstate.map, by = "region")

# ---- Graficar mapa 
ggplot(
  # Datos
  df_mapa2 %>% mutate(total_grupos = if_else(total_grupos == 0, NA_integer_, total_grupos)),
  # Coordenadas
  aes(long, lat, group = group, fill = total_grupos)) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Presencia criminal en México",
       subtitle = "Entre Julio - Diciembre 2022\n",
       fill = "Número de grupos criminales", 
       caption = "Fuente: Monitor-PPD (2022)") +
  # scale_fill_gradient(low=cols[1],high=cols[2]) +
  scale_fill_gradient(low = "#7FCCA1", high = "#193F29", na.value = "white", 
                      breaks = seq(0, 40, 10)) +
  # Tema
  tema +
  # Quitar fondo al mapa
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA)) +
  # Ampliar largo de la barra 
  theme(legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1,"cm")) 

ggsave(file = paste_fig("reporte_2022/04_grupos_mapa.png"))


## 4.4. Barras -----------------------------------------------------------------

# Gráfica
ggplot(
  # Datos
  df_grupos_entidad %>% 
    rename(total = total_grupos) %>% 
    mutate(estado = str_remove_all(estado, "[:digit:]")), 
       aes(x = total, y = reorder(estado, total))) +
  # Geoms
  geom_col(fill = "#193F29") +
  geom_text(aes(label = scales::comma(total)), vjust = 0.4, size = 3.5, 
            hjust = if_else(df_grupos_entidad$total<max(df_grupos_entidad$total), -0.4, 1.3),
            color = if_else(df_grupos_entidad$total<max(df_grupos_entidad$total), "#6F7271", "white"), 
            family = "Gotham") +
  # Etiquetas
  labs(title = "Presencia criminal en México",
       subtitle = "Entre Julio - Diciembre 2022\n",
       x = "\nNúmero de grupos criminales registrados",
       y = "", 
       caption = "Fuente: Monitor-PPD (2022)") +
  # Escalas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema 
  tema +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Guardar
ggsave(file = paste_fig("reporte_2022/05_grupos_barras.png"))


# 5. Alianzas y rivales --------------------------------------------------------


# 6. Desaparecidos -------------------------------------------------------------


#### Mapa ----------------------------------------------------------------------

table(df_crudo$desapariciónsecuestro)
table(df_crudo$numero_personas_desaparecidas)

df_desaparecidos <- df_crudo %>% 
  mutate(
    mes     = lubridate::month(fecha_de_publicacion),
    year_m  = zoo::as.yearmon(paste0(anio, "-", mes)),
    mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  # Filtrar meses de interés 
  filter(mes != "ene") %>% 
  select(estado, numero_personas_desaparecidas) %>% 
  group_by(estado) %>% 
  summarize(total_desparecidos = sum(numero_personas_desaparecidas, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1), 
         total_desparecidos = if_else(total_desparecidos == 0, NA_real_, total_desparecidos)) %>% 
  left_join(mxstate.map, by = "region")


#  Figura 
cols <- RColorBrewer::brewer.pal(3,'Blues')[c(1,3)]

ggplot(
  # Datos
  df_desaparecidos,
  # Coordenadas
  aes(long, lat, group = group, fill = total_desparecidos)) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Personsas desaparecidas y/o sin localizar en México",
       subtitle = "Entre Julio - Diciembre 2022\n",
       fill = "Número", 
       caption = "Fuente: Monitor-PPD (2022)") +
  # scale_fill_gradient(low=cols[1],high=cols[2]) +
  scale_fill_gradient(low = "#B6C5CE", high = "#182025", na.value = "white") +
  # Tema
  tema +
  # Quitar fondo al mapa
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA)) +
  # Ampliar largo de la barra 
  theme(legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1,"cm")) 

ggsave(file = paste_fig("reporte_2022/06_desapariciones_mapa.png"))

#### Barras --------------------------------------------------------------------

df_barras <- df_crudo                                               %>% 
  # variables de interés                    
  select(estado, numero_personas_desaparecidas)                     %>% 
  group_by(estado)                                                  %>% 
  summarize(total = sum(numero_personas_desaparecidas, na.rm = T))  %>% 
  drop_na()                                                         %>% 
  filter(estado != "20") %>% 
  mutate(estado = str_remove_all(estado, "[:digit:]"))

ggplot(
  # Datos 
  df_barras, 
  # Coordenadas
       aes(x = total, y = reorder(estado, total))) +
  # Geoms
  geom_col(fill = "#182025") +
  geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
            hjust = if_else(df_barras$total<max(df_barras$total), -0.2, 1.2),
            color = if_else(df_barras$total<max(df_barras$total), "#6F7271", "white"), 
            family = "Gotham") +
  # Etiquetas
  labs(title = "Personas desaparecidas y/o sin localizar en México",
       subtitle = "Entre Julio - Diciembre 2022\n",
       fill = "Número", 
       x = "", 
       y = "", 
       caption = "Fuente: Monitor-PPD (2022)") +
  # Escalas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema 
  tema +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
ggsave(file = paste_fig("reporte_2022/07_desapariciones_barras.png"))



# 7. Autoridades ---------------------------------------------------------------

# Juntar todas las autoridades registradas 
v_autoridades <- unique(
  c(df_crudo$autoridad1, df_crudo$autoridad2, df_crudo$autoridad3, 
    df_crudo$autoridad4, df_crudo$autoridad5))

# Lista de autoridades civiles

# Lista de autoridades militares 


# FIN. -------------------------------------------------------------------------

