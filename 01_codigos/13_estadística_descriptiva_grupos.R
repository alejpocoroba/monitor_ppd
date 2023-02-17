#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Estadística descriptiva grupos criminales
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          15 de febrero de 2023
# Última actualización:       16 de febrero de 2023
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

# 1. Cargar datos --------------------------------------------------------------

df_crudo <- read_xlsx(paste_inp("base_monitorPPD_2sem2022.xlsx"))

# 2. Limpiar nombres de grupos -------------------------------------------------

## 2.1. Funciones de limpieza --------------------------------------------------

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

#---- Función para limpiar nombres de grupos armados

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


#---- Función para clasificar autoridades
clasificar_autoridad <- function(x){
  
  v_civil <- c("municipal", "guardia civil", "guardia de proximidad", 
               "protección civil", "vial", "tránsito", "federal", 
               "investigación", "aduana", "ministerial", "fiscalía",
               "UECS", "AIC", "comisionado de seguridad", "PGJ", "estatal",
               "preventiva", "INM", "mando único", "SSC", "PGJE", "penal", "ANAM")
  
  v_militar <- c("sedena", "semar", "GN")
  
  v_ambos <- c("operativo blindaje", "BOMU", "BOI")
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
                                        "grupo10")) %>% 
  rename(grupo11 = rival, grupo12 = alianza)


## 4.2. Limpiar nombres --------------------------------------------------------

# ---- Recodificar nombres con funciones 
df_limpia <- df_nombres                                       %>% 
  # Crear variables con tipo de clasificación 
  mutate(across(starts_with("grupo"), ~clasificar_grupos(.))) %>% 
  # Limpiar nombres específicos de grupos criminales
  mutate(across(starts_with("grupo"), ~limpiar_grupos(.)))    %>% 
  # Quitar espacios en blanco al inicio y final (función trimws)
  mutate(across(starts_with("grupo"), ~trimws(.)))            %>%
  mutate(across(starts_with("grupo"), ~if_else(. == ".", NA_character_, .))) %>%          
  # Convertir autodefensas, alias y guardia civil a NAs
  mutate(across(starts_with("grupo"), ~if_else(. %in% c("autodefensas", 
                                                        "alias", 
                                                        "Guardia Civil"), 
                                               NA_character_, .)))  %>% 
  rename(rival = grupo11, alianza = grupo12)

## 2.3. Revisar ----------------------------------------------------------------

# Mismo número de filas, hay más columnas porque grupos se dividió
dim(df_crudo)  
dim(df_limpia) 

# Comparar nombres (sucios vs limpios)

# ---- Agrupar todos los nombres distintos 
v_grupos <- unique(
  c(df_nombres$grupo1, df_nombres$grupo2, df_nombres$grupo3, df_nombres$grupo4, 
    df_nombres$grupo5, df_nombres$grupo6, df_nombres$grupo7, df_nombres$grupo8, 
    df_nombres$grupo9, df_nombres$grupo10, df_nombres$grupo11, df_nombres$grupo12)) # Hay 297 grupos (versión sucia)

v_grupos_limpios <- unique(
  c(df_limpia$grupo1, df_limpia$grupo2, df_limpia$grupo3, df_limpia$grupo4, 
    df_limpia$grupo5, df_limpia$grupo6, df_limpia$grupo7, df_limpia$grupo8, 
    df_limpia$grupo9, df_limpia$grupo10, df_limpia$rival, df_limpia$alianza)) # Hay 131 grupos (sin contar NA)


## 2.4. Guardar nombres limpios ------------------------------------------------

openxlsx::write.xlsx(df_limpia, 
                     file = paste_out("base_monitorPPD_2sem2022_limpia.xlsx"))

# 3. Estadística descriptiva ---------------------------------------------------

# Seleccionar variables de interés y pasarlo a formato largo 
df_larga <- df_limpia                                                     %>% 
  select(id, fecha = fecha_de_publicacion, estado, starts_with("grupo"))  %>% 
  pivot_longer(cols = starts_with("grupo"), 
               names_to = "num", 
               values_to = "grupo")                                       %>% 
  drop_na(grupo)

# ---- Número de grupos 
length(unique(df_larga$grupo))

# ---- Lista de grupos 
unique(df_larga$grupo)

# ---- Frecuencia con la que se menciona cada grupo 
table(df_larga$grupo)

# ---- Los 20 grupos mencionados con más frecuencia 
df_top20 <- as.data.frame(table(df_larga$grupo))  %>% 
  arrange(desc(Freq))                             %>% 
  slice(1:20) 

print(df_top20) 

v_top20 <- df_top20$Var1 # Guardar grupos más mencionados

# ---- Presencia de grupos criminales en entidades federativas 
df_entidades <- df_larga                          %>% 
  filter(grupo %in% v_top20)                      %>% 
  drop_na(estado)

df_grupos_entidad <- as.data.frame(
  table(df_entidades$estado, df_entidades$grupo)) %>% 
  filter(Freq > 0)                                %>% 
  select(grupo = Var2, estado = Var1, menciones = Freq) %>% 
  arrange(grupo, desc(menciones))


df_grupos_conteo_entidad <- df_grupos_entidad %>% 
  group_by(grupo) %>% 
  summarise(num_estados = n())

# FIN. -------------------------------------------------------------------------
