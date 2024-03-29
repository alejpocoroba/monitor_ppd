#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Continuidad "12_limpieza_presentación"
#                             Lineas 1-41 y 270 a 310
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          15 de febrero de 2023
# Última actualización:       15 de febrero de 2023
#------------------------------------------------------------------------------#

unique(df_crudo$instit_deten2)
unique(df_crudo$instit_deten3)

# Fuente: Monitor PPD 

# Procesamiento de los nombres 
# Vectores

v_gruposin <- c("Sujetos armados", "Banda de homicidas en Perote", 
                "célula delictiva", "Narcomenudistas", "13 células delictivas",
                "Civiles Armados", "grupo armado", "Sin especificar", "sin especificar")

v_autodefensas <- c("Columna Armada del General Pedro José Méndez", 
                    'Columna Armada Pedro José Méndez',
                    "Columna Armada Pedro José Méndez",
                    "Resistencia Civil de Baja California", 
                    "Autodefensa Fuerza Territorial Poblana", 
                    "Fuerza Territorial Poblana", 
                    "Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)",
                    'Octavio L. alias "Tarzán" líder de autodefensas de Tamaulipas',
                    " Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)")

v_alias <- c("Julio Adrián, también conocido como ‘El Mercy’", "‘El Pollo’",
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
             
# Limpieza de nombres 
## 1.- generales

# Grupo 1    
df_nombres2 <- df_nombres %>% 
  mutate(
    grupo1 = case_when(
      grupo1 %in% v_gruposin ~ "grupo armado",
      grupo1 %in% v_autodefensas ~ "autodefensas",
      grupo1 %in% v_alias ~ "alias", 
      grupo1 == grupo1 ~ grupo1))

# Grupo 2
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo2 = case_when(
      grupo2 %in% v_gruposin ~ "grupo armado",
      grupo2 %in% v_autodefensas ~ "autodefensas",
      grupo2 %in% v_alias ~ "alias", 
      grupo2 == grupo2 ~ grupo2))

# Grupo 3
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo3 = case_when(
      grupo3 %in% v_gruposin ~ "grupo armado",
      grupo3 %in% v_autodefensas ~ "autodefensas",
      grupo3 %in% v_alias ~ "alias", 
      grupo3 == grupo3 ~ grupo3))

# Caso: Los Mexicles, Gente Nueva, Los Aztecas

# Grupo 4
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo4 = case_when(
      grupo4 %in% v_gruposin ~ "grupo armado",
      grupo4 %in% v_autodefensas ~ "autodefensas",
      grupo4 %in% v_alias ~ "alias", 
      grupo4 == grupo4 ~ grupo4))

# Grupo 5
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo5 = case_when(
      grupo5 %in% v_gruposin ~ "grupo armado",
      grupo5 %in% v_autodefensas ~ "autodefensas",
      grupo5 %in% v_alias ~ "alias", 
      grupo5 == grupo5 ~ grupo5))

# Grupo 6 
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo6 = case_when(
      grupo6 %in% v_gruposin ~ "grupo armado",
      grupo6 %in% v_autodefensas ~ "autodefensas",
      grupo6 %in% v_alias ~ "alias", 
      grupo6 == grupo6 ~ grupo6))

#Grupo 7
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo7 = case_when(
      grupo7 %in% v_gruposin ~ "grupo armado",
      grupo7 %in% v_autodefensas ~ "autodefensas",
      grupo7 %in% v_alias ~ "alias", 
      grupo7 == grupo7 ~ grupo7))

# Grupo 8
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo8 = case_when(
      grupo8 %in% v_gruposin ~ "grupo armado",
      grupo8 %in% v_autodefensas ~ "autodefensas",
      grupo8 %in% v_alias ~ "alias", 
      grupo8 == grupo8 ~ grupo8))

# Grupo 9
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo9 = case_when(
      grupo9 %in% v_gruposin ~ "grupo armado",
      grupo9 %in% v_autodefensas ~ "autodefensas",
      grupo9 %in% v_alias ~ "alias", 
      grupo9 == grupo9 ~ grupo9))

# Grupo 10
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo10 = case_when(
      grupo10 %in% v_gruposin ~ "grupo armado",
      grupo10 %in% v_autodefensas ~ "autodefensas",
      grupo10 %in% v_alias ~ "alias", 
      grupo10 == grupo10 ~ grupo10))

unique(df_nombres2$grupo10)

# Guardia civil + autodefensas = borrar
# Sin especificar: menciona grupo criminal pero no nombre

## 2.- Nombres de grupos criminales 
# Grupo 1
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo1 = case_when(
      grupo1  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo1  == " La Línea" ~ "La Línea",
      grupo1  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo1  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo1  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo1  == "Los capetos"  ~ "Los Capetos",
      grupo1  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo1  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo1  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo1  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo1  == "Los salazar"  ~ "Los Salazar",
      grupo1  == "Los Salazares"  ~ "Los Salazar", 
      grupo1  == "Los Salazares"  ~ "Los Salazar",
      grupo1  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo1  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo1  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo1  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo1  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo1  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo1  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo1  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo1  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo1  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo1  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo1  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo1  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo1  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo1  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo1  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo1  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo1  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo1  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo1  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo1  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo1  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo1  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo1  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo1  == "La empresa"  ~ "La Empresa",
      grupo1  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo1  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo1  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo1  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo1  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo1  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo1  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo1  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo1  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo1  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo1  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo1  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo1  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo1  == "Escorpiones" ~ "Grupo Scorpion",
      grupo1  == "La Plaza" ~ "Cártel La Plaza",
      grupo1  == "los viagras" ~ "Los Viagras",
      grupo1  == " Los Viagras" ~ "Los Viagras",
      grupo1  == "Los correa" ~ "Cártel de los Correa",
      grupo1  == "Los Correa" ~ "Cártel de los Correa",
      grupo1  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo1  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo1  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo1  == grupo1 ~ grupo1))

# Caso: [45] "Cártel de los Correa- La familia michoacana"  
# Caso: [111] "Cártel de Sinaloa. Cártel Jalisco Nueva Generación (CJNG)"
# Caso: [51] "pandilleros de La Villita y los rivales de El Cerrito" 

# Grupo 2 
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo2 = case_when(
      grupo2  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo2  == " La Línea" ~ "La Línea",
      grupo2  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo2  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo2  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo2  == "Los capetos"  ~ "Los Capetos",
      grupo2  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo2  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo2  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo2  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo2  == "Los salazar"  ~ "Los Salazar",
      grupo2  == "Los Salazares"  ~ "Los Salazar", 
      grupo2  == "Los Salazares"  ~ "Los Salazar",
      grupo2  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo2  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo2  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo2  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo2  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo2  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo2  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo2  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo2  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo2  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo2  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo2  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo2  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo2  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo2  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo2  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo2  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo2  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo2  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo2  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo2  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo2  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo2  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo2  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo2  == "La empresa"  ~ "La Empresa",
      grupo2  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo2  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo2  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo2  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo2  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo2  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo2  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo2  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo2  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo2  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo2  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo2  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo2  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo2  == "Escorpiones" ~ "Grupo Scorpion",
      grupo2  == "La Plaza" ~ "Cártel La Plaza",
      grupo2  == "los viagras" ~ "Los Viagras",
      grupo2  == " Los Viagras" ~ "Los Viagras",
      grupo2  == "Los correa" ~ "Cártel de los Correa",
      grupo2  == "Los Correa" ~ "Cártel de los Correa",
      grupo2  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo2  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo2  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo2  == grupo2 ~ grupo2))

# Grupo 3 
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo3 = case_when(
      grupo3  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo3  == " La Línea" ~ "La Línea",
      grupo3  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo3  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo3  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo3  == "Los capetos"  ~ "Los Capetos",
      grupo3  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo3  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo3  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo3  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo3  == "Los salazar"  ~ "Los Salazar",
      grupo3  == "Los Salazares"  ~ "Los Salazar", 
      grupo3  == "Los Salazares"  ~ "Los Salazar",
      grupo3  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo3  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo3  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo3  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo3  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo3  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo3  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo3  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo3  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo3  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo3  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo3  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo3  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo3  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo3  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo3  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo3  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo3  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo3  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo3  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo3  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo3  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo3  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo3  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo3  == "La empresa"  ~ "La Empresa",
      grupo3  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo3  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo3  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo3  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo3  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo3  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo3  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo3  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo3  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo3  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo3  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo3  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo3  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo3  == "Escorpiones" ~ "Grupo Scorpion",
      grupo3  == "La Plaza" ~ "Cártel La Plaza",
      grupo3  == "los viagras" ~ "Los Viagras",
      grupo3  == " Los Viagras" ~ "Los Viagras",
      grupo3  == "Los correa" ~ "Cártel de los Correa",
      grupo3  == "Los Correa" ~ "Cártel de los Correa",
      grupo3  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo3  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo3  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo3  == grupo3 ~ grupo3))

# Caso:  " Los Mexicles, Gente Nueva, Los Aztecas"

# Grupo 4 
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo4 = case_when(
      grupo4  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo4  == " La Línea" ~ "La Línea",
      grupo4  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo4  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo4  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo4  == "Los capetos"  ~ "Los Capetos",
      grupo4  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo4  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo4  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo4  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo4  == "Los salazar"  ~ "Los Salazar",
      grupo4  == "Los Salazares"  ~ "Los Salazar", 
      grupo4  == "Los Salazares"  ~ "Los Salazar",
      grupo4  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo4  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo4  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo4  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo4  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo4  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo4  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo4  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo4  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo4  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo4  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo4  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo4  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo4  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo4  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo4  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo4  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo4  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo4  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo4  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo4  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo4  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo4  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo4  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo4  == "La empresa"  ~ "La Empresa",
      grupo4  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo4  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo4  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo4  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo4  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo4  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo4  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo4  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo4  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo4  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo4  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo4  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo4  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo4  == "Escorpiones" ~ "Grupo Scorpion",
      grupo4  == "La Plaza" ~ "Cártel La Plaza",
      grupo4  == "los viagras" ~ "Los Viagras",
      grupo4  == " Los Viagras" ~ "Los Viagras",
      grupo4  == "Los correa" ~ "Cártel de los Correa",
      grupo4  == "Los Correa" ~ "Cártel de los Correa",
      grupo4  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo4  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo4  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo4  == grupo4 ~ grupo4))

# Grupo 5
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo5 = case_when(
      grupo5  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo5  == " La Línea" ~ "La Línea",
      grupo5  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo5  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo5  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo5  == "Los capetos"  ~ "Los Capetos",
      grupo5  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo5  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo5  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo5  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo5  == "Los salazar"  ~ "Los Salazar",
      grupo5  == "Los Salazares"  ~ "Los Salazar", 
      grupo5  == "Los Salazares"  ~ "Los Salazar",
      grupo5  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo5  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo5  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo5  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo5  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo5  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo5  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo5  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo5  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo5  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo5  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo5  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo5  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo5  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo5  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo5  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo5  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo5  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo5  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo5  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo5  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo5  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo5  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo5  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo5  == "La empresa"  ~ "La Empresa",
      grupo5  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo5  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo5  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo5  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo5  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo5  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo5  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo5  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo5  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo5  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo5  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo5  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo5  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo5  == "Escorpiones" ~ "Grupo Scorpion",
      grupo5  == "La Plaza" ~ "Cártel La Plaza",
      grupo5  == "los viagras" ~ "Los Viagras",
      grupo5  == " Los Viagras" ~ "Los Viagras",
      grupo5  == "Los correa" ~ "Cártel de los Correa",
      grupo5  == "Los Correa" ~ "Cártel de los Correa",
      grupo5  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo5  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo5  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo5  == grupo5 ~ grupo5))

# Del grupo 6 a 9 ya no era necesario limpiar, viene un solo nombre 

# Grupo 10
df_nombres2 <- df_nombres2 %>% 
  mutate(
    grupo10 = case_when(
      grupo10  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)" ~ "La Línea",
      grupo10  == " La Línea" ~ "La Línea",
      grupo10  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo10  == "Grupo de Huachicol del Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo10  == "El Pelón del Sur" ~ "Banda del Pelón del Sur",
      grupo10  == "Los capetos"  ~ "Los Capetos",
      grupo10  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo10  == "Gente Nueva Salazar"  ~ "Los Salazar",
      grupo10  == "La banda de “El Negro”"  ~ "Banda de El Negro",
      grupo10  == "banda delictiva de “Los Chinos”"  ~ "Los Chinos",
      grupo10  == "Los salazar"  ~ "Los Salazar",
      grupo10  == "Los Salazares"  ~ "Los Salazar", 
      grupo10  == "Los Salazares"  ~ "Los Salazar",
      grupo10  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
      grupo10  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo10  == "Los pájaros sierra"  ~ "Pájaros Sierra",
      grupo10  == " líder de Pájaros Sierra"  ~ "Pájaros Sierra",
      grupo10  == "Cártel Los Alemanes de Los Zetas" ~ "Cártel Los Alemanes", 
      grupo10  == "Cártel de Los Alemanes" ~ "Cártel Los Alemanes",
      grupo10  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo10  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo10  == " El Bukanas"  ~ "Grupo de El Bukanas",
      grupo10  == "'El Loco Téllez'"  ~ "Grupo de El Loco Téllez",
      grupo10  == "El Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo10  == " Célula Delictiva del Loco Téllez"  ~ "Grupo de El Loco Téllez",
      grupo10  == "gente de \"El Grillo\""  ~ "Banda de El Grillo",
      grupo10  == "banda de “Los Cucos”"  ~ "Banda de Los Cucos",
      grupo10  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo10  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo10  == " parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo10  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo10  == "“Los Lampones\""  ~ "Los Lampones", 
      grupo10  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo10  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo10  == " La Familia Michoacana"  ~ "La Familia Michoacana",
      grupo10  == " Familia Michacana"  ~ "La Familia Michoacana",
      grupo10  == " Familia Michoacana"  ~ "La Familia Michoacana",
      grupo10  == "La empresa"  ~ "La Empresa",
      grupo10  == "Los Mexicles o Los Aztecas"  ~ "Los Mexicles",
      grupo10  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo10  == "La unión de los fantasmas" ~ "La Unión de los Fantasmas",
      grupo10  == "Cártel del Abuelo Farías" ~ "Cártel de Tepalcatepec", 
      grupo10  == "“El Abuelo”" ~ "Cártel de Tepalcatepec",
      grupo10  == "Guardia Guerrerense, antes Templarios." ~ "Guardia Guerrerense",
      grupo10  == "‘Los Tarzanes’ o ‘Los Sinaloa’" ~ "Los Sinaloa",
      grupo10  == "\"La Reina de los Sinaloas\"" ~ "Los Sinaloa",
      grupo10  == "alias \"El Mayo\"" ~ "Cártel de Sinaloa",
      grupo10  == "grupo del Chapos Trini" ~ "Cártel de Sinaloa",
      grupo10  == "Los Chapos Trinis" ~ "Cártel de Sinaloa",
      grupo10  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
      grupo10  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
      grupo10  == "Escorpiones" ~ "Grupo Scorpion",
      grupo10  == "La Plaza" ~ "Cártel La Plaza",
      grupo10  == "los viagras" ~ "Los Viagras",
      grupo10  == " Los Viagras" ~ "Los Viagras",
      grupo10  == "Los correa" ~ "Cártel de los Correa",
      grupo10  == "Los Correa" ~ "Cártel de los Correa",
      grupo10  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
      grupo10  == " los caballeros templarios" ~ "Caballeros Templarios",
      grupo10  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
      grupo10  == grupo10 ~ grupo10))

# Alianzas y rivalidades 
# Alianzas
# Identificar cuál es el máximo de grupos incluidos en una sola observación 
# dentro de la variable "alianza". Se hace contando los ";"
df_ali <- df_nombres2 %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    alianza = str_replace_all(alianza, ":", ";"), 
    # Contar número de puntos y comas (;)
    df_ali = str_count(alianza, ";")) %>% 
  select(id, df_ali) 

max(df_ali$df_ali, na.rm = T) # El máximo es 5 alianzas

# Separar los nombres de "alianzas" en columnas independientes 
df_nombres2 <- df_nombres2 %>% 
  mutate(alianza = str_replace_all(alianza, ":", ";")) %>% 
  separate(alianza, sep = ";", c("alianza1", 
                                 "alianza2", 
                                 "alianza3", 
                                 "alianza4", 
                                 "alianza5")) 
# Rival
# Identificar cuál es el máximo de grupos incluidos en una sola observación 
# dentro de la variable "rival". Se hace contando los ";"
df_riv <- df_nombres2 %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    alianza = str_replace_all(rival, ":", ";"), 
    # Contar número de puntos y comas (;)
    df_riv = str_count(rival, ";")) %>% 
  select(id, df_riv) 

max(df_riv$df_riv, na.rm = T) # El máximo es 2 rivales

# Separar los nombres de "alianzas" en columnas independientes 
df_nombres2 <- df_nombres2 %>% 
  mutate(rival = str_replace_all(rival, ":", ";")) %>% 
  separate(rival, sep = ";", c("rival1", 
                               "rival2")) 

# Autoridad 
## Limpieza de nombres
colnames(df_crudo)
unique(df_crudo$autoridad5)

v_civil <- c("municipal", "guardia civil", "guardia de proximidad", 
             "protección civil", "vial", "tránsito", "federal", 
             "investigación", "aduana", "ministerial", "fiscalía",
             "UECS", "AIC", "comisionado de seguridad", "PGJ", "estatal",
             "preventiva", "INM", "mando único", "SSC", "PGJE", "penal", "ANAM")

v_militar <- c("sedena", "semar", "GN")

v_ambos <- c("operativo blindaje", "BOMU", "BOI")

# Autoridad 1 

df_autoridad <- df_crudo %>% 
  mutate(
    autoridad1 = case_when(
      autoridad1 %in% v_civil ~ "institución civil",
      autoridad1 %in% v_militar ~ "fuerzas armadas",
      autoridad1 %in% v_ambos ~ "Operativos Conjuntos", 
      autoridad1 == autoridad1 ~ autoridad1))

# Autoridad 2

df_autoridad <- df_autoridad %>% 
  mutate(
    autoridad2 = case_when(
      autoridad2 %in% v_civil ~ "institución civil",
      autoridad2 %in% v_militar ~ "fuerzas armadas",
      autoridad2 %in% v_ambos ~ "Operativos Conjuntos", 
      autoridad2 == autoridad2 ~ autoridad2))

# Autoridad 3

df_autoridad <- df_autoridad %>% 
  mutate(
    autoridad3 = case_when(
      autoridad3 %in% v_civil ~ "institución civil",
      autoridad3 %in% v_militar ~ "fuerzas armadas",
      autoridad3 %in% v_ambos ~ "Operativos Conjuntos", 
      autoridad3 == autoridad3 ~ autoridad3))

# Autoridad 4

df_autoridad <- df_autoridad %>% 
  mutate(
    autoridad4 = case_when(
      autoridad4 %in% v_civil ~ "institución civil",
      autoridad4 %in% v_militar ~ "fuerzas armadas",
      autoridad4 %in% v_ambos ~ "Operativos Conjuntos", 
      autoridad4 == autoridad4 ~ autoridad4))

# Autoridad 5

df_autoridad <- df_autoridad %>% 
  mutate(
    autoridad5 = case_when(
      autoridad5 %in% v_civil ~ "institución civil",
      autoridad5 %in% v_militar ~ "fuerzas armadas",
      autoridad5 %in% v_ambos ~ "Operativos Conjuntos", 
      autoridad5 == autoridad5 ~ autoridad5))