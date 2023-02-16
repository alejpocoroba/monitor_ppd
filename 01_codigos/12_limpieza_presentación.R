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

# Tema para gráficas 
tema <- theme_bw() 

v_caption <- "Fuente: Monitor- PPD (2022)"

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
  filter(mes != "ene", mes != "jun")


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
       fill = "Número de homicidios", 
       caption = v_caption) +
  # scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  scale_fill_gradient(low = "white", high = "#d62828", na.value = "grey") +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))


# Guardar
ggsave(file = paste_fig("reporte_2022/01a_homicidios_mapa.png"))

#### Barras --------------------------------------------------------------------

# procesamiento de los datos
df_barras1 <- df_homicidios %>% 
  # variables de interés
  select(estado, homic_total) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(homic_total, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1))  %>%
  drop_na() %>% 
  filter(estado != "20")

# figura: mapa
ggplot(df_barras1, 
       aes(x = total_homicidios, y = reorder(estado, total_homicidios))) +
  geom_col(fill = "#d62828") +
  geom_text(aes(label = scales::comma(total_homicidios)), vjust = +0.4, 
            hjust = if_else(df_barras1$total_homicidios<max(df_barras1$total_homicidios), -0.2, 1.5)) +
  # Etiquetas
  labs(title = "Homicidios violentos en México",
       subtitle = "Julio - Diciembre 2022\n",
       x = "", 
       y = "", 
       caption = v_caption) +
  scale_x_continuous(label = scales::comma_format()) +
  tema
  
# Guardar
ggsave(file = paste_fig("reporte_2022/01b_homicidios_barras.png"))

#### Total nacional al mes -----------------------------------------------------

# Procesar datos 
df_plot1 <- df_homicidios %>% 
  filter(homic_total > 0) %>% 
  group_by(year_m, mes)   %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  filter(mes != "ene", mes != "jun")

# Serie de tiempo 
ggplot(
  # Datos
  df_plot1, 
  # Coordenadas
  aes(x = year_m, y = total, group = 1)) +
  # Geoms
  geom_line() +
  geom_point() +
  # Etiquetas
  labs(
    title = "Homicidios violentos en México", 
    subtitle = "Julio - Diciembre 2022\n", 
    x = "", 
    y = "", 
    caption = v_caption
  ) +
  # Etiquetas 
  scale_y_continuous(label = scales::comma_format(), limits = c(1400, 2000)) +
  # Tema
  tema
 
# Guardar
ggsave(file = paste_fig("reporte_2022/02_homicidios_serie.png"))



#### Total estatal al mes ------------------------------------------------------

# Procesar datos 
df_plot2 <- df_homicidios %>% 
  filter(homic_total > 0) %>% 
  group_by(estado, year_m, mes)   %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  filter(mes != "ene", mes != "jun") %>% 
  drop_na() %>% 
  filter(estado != "20")


# Serie de tiempo 
ggplot(
  # Datos
  df_plot2, 
  # Coordenadas
  aes(x = year_m, y = total, group = 1)) +
  facet_wrap(~estado, scale = "free_y") +
  # Geoms
  geom_line() +
  geom_point() +
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
ggsave(file = paste_fig("reporte_2022/03_homicidios_serie_estados.png"))


## 3.2 Homicidos por criterios -------------------------------------------------

# PREGUNTA PARA MAILOBSKY: La variable ataque_armado_clean tiene algunos de los 
# eventos incluidos en la gráfica del reporte, pero no los incluye todos y 
# también incluye valores nuevos. 

v_eventos <- unique(df_homicidios$ataque_armado_clean) # 12 tipos de eventos


df_plot3 <- df_homicidios %>% 
  mutate(evento = ataque_armado_clean) %>% 
  group_by(evento) %>% 
  summarise(total = sum(homic_total, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(evento = if_else(total < 20, "Otros", evento)) %>% 
  group_by(evento) %>% 
  summarise(total = sum(total, na.rm = TRUE)) 

# table(df_homicidios$cuerpos_localizados)

ggplot(
  # Datos 
  df_plot3, 
  # Coordenadas 
       aes(x = total, y = reorder(evento, total))) +
  # Geoms 
  geom_col() +
  geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
            hjust = if_else(df_plot3$total<max(df_plot3$total), -0.2, 1.5),
            color = if_else(df_plot3$total<max(df_plot3$total), "black", "white")) +
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
  tema


# Guardar
ggsave(file = paste_fig("reporte_2022/03_homicidios_tipos.png"))


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

# Agrupar todos los nombres distintos 
v_grupos <- unique(
  c(df_nombres$grupo1, df_nombres$grupo2, df_nombres$grupo3, df_nombres$grupo4, 
    df_nombres$grupo5, df_nombres$grupo6, df_nombres$grupo7, df_nombres$grupo8, 
    df_nombres$grupo9, df_nombres$grupo10))


# Hay muchos casos donde el nombre comienza con un espacio (" ")

## 4.3. Mapa -------------------------------------------------------------------


# 5. Desaparecidos -------------------------------------------------------------


#### Mapa ----------------------------------------------------------------------

table(df_crudo$desapariciónsecuestro)
table(df_crudo$numero_personas_desaparecidas)

df_desaparecidos <- df_crudo %>% 
  mutate(
    mes     = lubridate::month(fecha_de_publicacion),
    year_m  = zoo::as.yearmon(paste0(anio, "-", mes)),
    mes = lubridate::month(fecha_de_publicacion, label = TRUE)) %>% 
  # Filtrar meses de interés 
  filter(mes != "ene", mes != "jun") %>% 
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
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = NA))

ggsave(file = paste_fig("reporte_2022/06_desapariciones_mapa.png"))

#### Barras --------------------------------------------------------------------

df_barras <- df_crudo %>% 
  # variables de interés
  select(estado, numero_personas_desaparecidas) %>% 
  group_by(estado) %>% 
  summarize(total = sum(numero_personas_desaparecidas, na.rm = T))  %>% 
  drop_na() %>% 
  filter(estado != "20")

ggplot(
  # Datos 
  df_barras, 
  # Coordenadas
       aes(x = total, y = reorder(estado, total))) +
  # Geoms
  geom_col() +
  geom_text(aes(label = scales::comma(total)), vjust = +0.4, 
            hjust = if_else(df_barras$total<max(df_barras$total), -0.2, 1.5),
            color = if_else(df_barras$total<max(df_barras$total), "black", "white")) +
  # Etiquetas
  labs(title = "Personsas desaparecidas y/o sin localizar en México",
       subtitle = "Entre Julio - Diciembre 2022\n",
       fill = "Número", 
       x = "", 
       y = "", 
       caption = "Fuente: Monitor-PPD (2022)") +
  # Escalas 
  scale_x_continuous(label = scales::comma_format()) +
  # Tema 
  tema
  
ggsave(file = paste_fig("reporte_2022/07_desapariciones_barras.png"))


# 6. Autoridades ---------------------------------------------------------------

# Juntar todas las autoridades registradas 
v_autoridades <- unique(
  c(df_crudo$autoridad1, df_crudo$autoridad2, df_crudo$autoridad3, 
    df_crudo$autoridad4, df_crudo$autoridad5))

# Lista de autoridades civiles

# Lista de autoridades militares 


# FIN. -------------------------------------------------------------------------

