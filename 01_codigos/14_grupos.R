#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Grupos criminales 2022
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          02 de marzo de 2023
# Última actualización:       07 de marzo de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)


# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, stringr, zoo, ggtext, beepr, mxmaps)

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

# 1. Cargar datos --------------------------------------------------------------
df_crudo <- read_xlsx(paste_inp("base_monitorPPD_2sem2022.xlsx"))

## 2.1. Funciones de limpieza --------------------------------------------------
# interes: id, enlace, publicación, estado, municipio y grupos (ali y riv)
df_crudo <- df_crudo %>% 
  select(id, fecha_de_publicacion, enlace, estado, municipio,
         grupo_criminal, alianza, rival)

#---- Función para clasificar si es un grupo armado, autodefensas o un alias

clasificar_grupos <- function(x){
  
  # Vectores con las clasificaciones
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
    " Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)", "Guardia Civil")
  
  v_alias <- c(
    "Julio Adrián, también conocido como ‘El Mercy’", "‘El Pollo’",
    "‘El Smoll’", "El Panchito", "El Cholo", "Cipog-ez", "‘Zague’.",
    "Don Cabezón", "Oscar Ivan Maras", "el Payepas","El Damaso",
    "El Moyo", "El Jaibol", "“el Santa”", "‘Checo’", "Efraín, alias 'El Gato'",
    "“El Yogui”", "“El Kinkis”", "\"El Bocho\"", "El Vampi", 
    "“El Pillo”, apenas había tomado el poder en sustitución de recién capturado y peligroso líder narcomenudista “El Croquis”",
    "“El Bengala”", "El ruso", "El Ruso", "\"El Rana\" y \"El Gus\"", "La Zorra", 
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
    " “El Acople”", " El Fresa", "“El Camacho”", "Edilberto N., alias El Gavilán", "El Jaguar",
    "Héctor Zepeda Navarrete, alias “El Teto”, Germán Ramírez Sánchez “El Toro” y Ángel Custodio Cuevas Arredondo, “El Marino”.", "taxista El Lienzo")
  
  # Condicioneales para limpieza  
  case_when(
    x %in% v_gruposin     ~ "grupo armado", 
    x %in% v_autodefensas ~ "autodefensas", 
    x %in% v_alias        ~ "alias", 
    x  == x ~ x
  )
  
}

#---- Función para limpiar nombres de grupos armados

limpiar_grupos <- function(x){
  
  case_when(
    x  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
    x  == "La Línea o Cártel de Juárez" ~ "La Línea", 
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
    x  == "Cártel Lo Alemanes de Los Zetas" ~ "Cártel Los Alemanes",
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
    x  == "La familia michoacana" ~ "La Familia Michoacana",
    x  == "La familia michoacana" ~ "La Familia Michoacana",
    x  == " La familia michoacana" ~ "La Familia Michoacana",
    x  == "La empresa"  ~ "La Empresa",
    x  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
    x  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
    x  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
    x  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
    x  == "cártel de Tepalcatepec" ~ "Cártel de Tepalcatepec",
    x  == " cártel de Tepalcatepec" ~ "Cártel de Tepalcatepec",
    x  == "Escuadro de la Muerte" ~ "Escuadrón de la Muerte", 
    x  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
    x  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
    x  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
    x  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
    x  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
    x  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
    x  == "cártel de Sinaloa Facción Mayo Zambada" ~ "Cártel de Sinaloa", 
    x  == "Mayo Zambada" ~ "Cártel de Sinaloa",
    x  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
    x  == "cártel del pacífico" ~ "Cártel de Sinaloa",
    x  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
    x  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
    x  == "Escorpiones" ~ "Grupo Scorpion",
    x  == "La Plaza" ~ "Cártel La Plaza",
    x  == "los viagras" ~ "Los Viagras",
    x  == " Los Viagras" ~ "Los Viagras",
    x  == "Los correa" ~ "Cártel de los Correa",
    x  == "Los Correa" ~ "Cártel de los Correa",
    x  == "cártel de Los Correa" ~ "Cártel de los Correa",
    x  == "“El Tigre” Correa, líder máximo del cártel que lleva el apellidos de la familia" ~ "Cártel de los Correa", 
    x  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
    x  == " los caballeros templarios" ~ "Caballeros Templarios",
    x  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
    x  == "Específicamente el Grupo X que encabeza el líder delincuencial conocido como “El barbas”." ~ "Grupo X de El Barbas",
    x  == "Familia Caro Páez" ~ "Cártel de Caborca", 
    x  == "Gente del Tigre" ~ "Gente Nueva del Tigre",
    x  == "Gente Nueva de Sonora" ~ "Gente Nueva", 
    x  == "Gente Nuevo" ~ "Gente Nueva",
    x  == "Gente Nueva Los Paredes" ~ "Los Paredes",
    x  == "Los Artistas Asesinos" ~ "Artistas Asesinos",
    x  == "Tlacos del Cártel de la Sierra" ~ "Cártel de la Sierra",
    x  == " Cártel Sangre Nueva Zeta" ~ "Sangre Nueva Zeta",
    x  == x ~ x
  )
  
}

## 2.2. Limpiar ----------------------------------------------------------------

# ---- Separar columnas 
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
                                        "grupo10", 
                                        "grupo11"))  %>% 
  separate(rival, sep = ";", c("rival1", "rival2", "rival3")) %>% 
  separate(alianza, sep = ";", c("alianza1", "alianza2", "alianza3", "alianza4", 
                                 "alianza5", "alianza6")) 

## 4.2. Limpiar nombres --------------------------------------------------------
df_limpia <- df_nombres %>% 
  # Crear variables con tipo de clasificación 
  mutate(across(starts_with("grupo"), ~clasificar_grupos(.))) %>% 
  # Limpiar nombres específicos de grupos criminales
  mutate(across(starts_with("grupo"), ~limpiar_grupos(.)))  %>% 
  # Quitar espacios en blanco al inicio y final (función trimws)
  mutate(across(starts_with("grupo"), ~trimws(.))) %>% 
  # Quitar observaciones que solo tienen un punto (".")
  mutate(across(starts_with("grupo"), ~if_else(. == ".", NA_character_, .))) %>%          
  mutate(across(starts_with("grupo"), ~if_else(. == "na", NA_character_, .))) %>%          
  # Convertir autodefensas, alias y guardia civil a NAs
  mutate(across(starts_with("grupo"), ~if_else(. %in% c("autodefensas", 
                                                        "alias", 
                                                        "grupo armado"), 
                                               NA_character_, .)))  %>% 
  # Rivales
  mutate(across(starts_with("rival"), ~clasificar_grupos(.))) %>% 
  mutate(across(starts_with("rival"), ~limpiar_grupos(.)))  %>% 
  mutate(across(starts_with("rival"), ~trimws(.))) %>% 
  mutate(across(starts_with("rival"), ~if_else(. == ".", NA_character_, .))) %>%          
  mutate(across(starts_with("rival"), ~if_else(. == "na", NA_character_, .))) %>%          
  mutate(across(starts_with("rival"), ~if_else(. %in% c("autodefensas", 
                                                        "alias", 
                                                        "grupo armado"), 
                                               NA_character_, .)))  %>% 
  # Alianzas
  mutate(across(starts_with("alianza"), ~clasificar_grupos(.))) %>% 
  mutate(across(starts_with("alianza"), ~limpiar_grupos(.)))  %>% 
  mutate(across(starts_with("alianza"), ~trimws(.))) %>% 
  mutate(across(starts_with("alianza"), ~if_else(. == ".", NA_character_, .))) %>%          
  mutate(across(starts_with("alianza"), ~if_else(. == "na", NA_character_, .))) %>%          
  mutate(across(starts_with("alianza"), ~if_else(. %in% c("autodefensas", 
                                                          "alias", 
                                                          "grupo armado"), 
                                                 NA_character_, .)))

## 2.3. Revisar ----------------------------------------------------------------

# Mismo número de filas, hay más columnas porque grupos se dividió
dim(df_crudo)  
dim(df_limpia) 

# Comparar nombres (sucios vs limpios)

# ---- Agrupar todos los nombres distintos 
v_grupos <- unique(
  c(df_nombres$grupo1, df_nombres$grupo2 , df_nombres$grupo3 , df_nombres$grupo4, 
    df_nombres$grupo5, df_nombres$grupo6 , df_nombres$grupo7 , df_nombres$grupo8, 
    df_nombres$grupo9, df_nombres$grupo10, df_nombres$grupo11, 
    df_nombres$rival1, df_nombres$rival2, df_nombres$rival3,
    df_nombres$alianza1, df_nombres$alianza2, df_nombres$alianza3, 
    df_nombres$alianza4, df_nombres$alianza5, df_nombres$alianza6)) 

v_grupos_limpios <- unique(
  c(df_limpia$grupo1  , df_limpia$grupo2, 
    df_limpia$grupo3  , df_limpia$grupo4, 
    df_limpia$grupo5  , df_limpia$grupo6, 
    df_limpia$grupo7  , df_limpia$grupo8, 
    df_limpia$grupo9  , df_limpia$grupo10, 
    df_limpia$grupo11 , df_limpia$rival1, 
    df_limpia$rival2  , df_limpia$rival3, 
    df_limpia$alianza1, df_limpia$alianza2, 
    df_limpia$alianza3, df_limpia$alianza4, 
    df_limpia$alianza5, df_limpia$alianza6))

# 3. Estadística descriptiva ---------------------------------------------------

# Seleccionar variables de interés y pasarlo a formato largo 
df_larga <- df_limpia %>% 
  select(id, fecha = fecha_de_publicacion, estado, starts_with("grupo")) %>% 
  pivot_longer(cols = c(starts_with("grupo"), starts_with("alianza"), starts_with("rival")),
               names_to  = "num", 
               values_to = "grupo") %>% 
  drop_na(grupo)

# ---- Número de grupos: 125
length(unique(df_larga$grupo))

# ---- Lista de grupos 
lista_grupos2022 <- data.frame(unique(df_larga$grupo))

# Base de la lista de grupos 
openxlsx::write.xlsx(lista_grupos2022, 
                     file = paste_out("lista_grupos2022.xlsx"))

# Presencia en estados por grupo 
# Procesamiento
# Estados sin presencia: 6, 10, 15, 20, 27, 29, 31
estados_faltan <- data.frame("estado" = c("Colima-06", "Durango-10", "Estado de México-15", 
                                          "Oaxaca-20", "Tabasco-27", "Tlaxcala-29", "Yucatán-31"),
                             "total_grupos" =  NA_real_)

# Número de grupos por estado
df_grupos0 <- df_larga %>% 
  # variables de interés
  select(estado, grupo) %>% 
  group_by(estado, grupo) %>% 
  summarize(total_grupos = n()) %>% 
  drop_na(grupo) %>% 
  group_by(estado) %>% 
  summarize(total_grupos = n()) %>% 
  drop_na(estado)


# Se guarda la base del número de grupos por estado
openxlsx::write.xlsx(df_grupos0, 
                       file = paste_out("grupos_por_estado_2022.xlsx"))

# Mapa 
# Se agregan los datos geo   
df_grupos <- df_grupos0 %>% 
  bind_rows(estados_faltan) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  left_join(mxstate.map, by = "region")

# plot
ggplot(
  # Datos
  df_grupos,
  # Coordenadas
  aes(long, lat, group = group, fill = total_grupos)) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Presencia criminal en México",
       subtitle = "Junio - Diciembre 2022\n",
       fill = "Número", 
       caption = "Fuente: Monitor-PPD (2022)") +
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

# Se guarda la figura   
ggsave(file = paste_fig("map_grupos.png"), width = 10, height = 6)


# Grupos por estado 
df_grupos2 <- df_larga %>% 
  select(estado, grupo) %>% 
  group_by(estado, grupo) %>% 
  summarize(total_grupos = n()) %>% 
  drop_na(grupo) %>% 
  group_by(grupo) %>% 
  summarize(total_grupos = n())
  
openxlsx::write.xlsx(df_grupos2, 
                     file = paste_out("lista_grupos_estado_2022.xlsx"))
