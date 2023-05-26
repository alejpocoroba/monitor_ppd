#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Limpieza enero-marzo 2023
#
# Encargado:                  Alejandro Pocoroba Erick Isaac Morales Sánchez
# Correo:                     alejandro.pocoroba@cide.edu
#                             erick.morales@cide.edu                                
# Fecha de creación:          01 de mayo de 2023
# Última actualización:       26 de mayo de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión enero-marzo 2023

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
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos --------------------------------------------------------------

# bases de enero, febrer y marzo 
m1 <- read_xlsx(paste_inp("m_ene_mar_23_su.xlsx"))

# 2. Procesamiento -------------------------------------------------------------

# Limpiar nombres de microdatos
df_ <- m1 %>% 
  janitor::clean_names() 

# Tratamiento de fechas #
df_$"x1_2_1_fecha_publicacion" <- as.Date(df_$"x1_2_1_fecha_publicacion")
df_$dia <- format(df_$"x1_2_1_fecha_publicacion", "%d")
df_$mes <- format(df_$"x1_2_1_fecha_publicacion", "%m")
df_$anio <- format(df_$"x1_2_1_fecha_publicacion", "%Y")

# Estado y municipio según su clave de Inegi 
df_ <- df_ %>% 
  mutate(
    claveEdo = str_sub(x1_3_3_municipio, -6, -5), 
    clavemun = str_sub(x1_3_3_municipio, -3, -1),
    clavefinal = str_remove_all(str_sub(x1_3_3_municipio, -6, -1), "[:punct:]"))

# Orden y renombrar las variables 
df_1 <- df_ %>%
  select("fecha_de_publicacion"   = "x1_2_1_fecha_publicacion",
         "Mes"                    = "mes", 
         "Dia"                    = "dia",
         "Anio"                   = "anio", 
         "fecha_hechos"           = "x1_3_1_fecha_hechos",
         "Enlace"                 = "x1_2_2_enlace",
         "titulo_de_la_nota"      = "x1_2_3_titulo_nota",
         "nombre_de_la_fuente"    = "x1_2_4_fuente",
         "enlace_otras_notas"     = "x1_2_5_enlaces_duplicados",
         "Estado"                 = "x1_3_2_estado",
         "Municipio"              = "x1_3_3_municipio",
         "CVEGEO_ent"             = "claveEdo",
         "CVEGEO_mun"             = "clavemun",
         "CVEGEO"                 = "clavefinal",
         "Lugar"                  = "x1_3_4_lugar",
         "grupo_criminal"         = "x3_3_grupo_criminal",
         "Alianza"                = "x3_3_1_alianza_grupo",
         "Rival"                  = "x3_3_2_rival_grupo",
         "actividad_grupo"        = "x3_1_actividad",
         "Narcomensaje"           = "x3_2_1_narcomensaje",
         "contenido_narcomensaje" = "x3_2_2_contenido_narcomensaje",
         "homic_total"            = "x2_2_1_homicidios_total",
         "homic_hombre"           = "x2_2_2_homicidios_hombre",
         "homic_mujer"            = "x2_2_3_homicidios_mujer",
         "homic_clasif1"          = "x2_2_4_pertenece_homicidio",
         "cuerpos_localizados"    = "x2_2_5_cuerpo_restos",
         "cuerpos_modo1"          = "x2_2_6_cuerpo_restos_localizados",
         "cuerpos_lugar1"         = "x2_2_7_lugar_cuerpo_restos",
         "heridos_total"          = "x2_3_1_heridxs_total",
         "heridos_hombre"         = "x2_3_2_heridos_hombres",
         "heridos_mujeres"        = "x2_3_3_heridas_mujeres",
         "heridos_clasif1"        = "x2_3_4_pertenece_heridx",
         "ataque_armado_clean"    = "x2_1_1_ataque",
         "lugar_ataque_clean"     = "x2_1_2_lugar_ataque",
         "politica_de_seguridad"  = "politica_seguridad")
 
# 3. Limpiar de las variables --------------------------------------------------

### 3.1 Fuente------------------------------------------------------------------
# Limpiar nombres de la fuente
#Vectores para los acentos
origen <- c("?", "?", "?", "?", "?", "?", "?", "?", "?", "?")

# Vector de caracteres de destino
destino <- c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")

# Aplicar la funci?n chartr() para eliminar acentos
df_1$nombre_de_la_fuente <- chartr(paste(origen, collapse = ""), 
                                   paste(destino, collapse = ""), 
                                   df_1$nombre_de_la_fuente)
# Quitar articulos que se repiten
df_1$nombre_de_la_fuente <- gsub("^el\\s+", "", df_1$nombre_de_la_fuente, 
                                 ignore.case = TRUE)

### 3.2 Narcomensajes, Cuerpos y Seg--------------------------------------------
# Cambiar FALSE/TRUE por 0 y 1

# Narcomensaje
df_1$Narcomensaje <- as.character(df_1$Narcomensaje)
df_1 <- df_1 %>%
  mutate(Narcomensaje = 
           case_when(Narcomensaje == "FALSE" ~ "0",
                     Narcomensaje == "TRUE"  ~ "1",
                     Narcomensaje == Narcomensaje ~ Narcomensaje))
# Cuerpos 
df_1$cuerpos_localizados <- as.character(df_1$cuerpos_localizados)
df_1 <- df_1 %>%
  mutate(cuerpos_localizados = 
           case_when(cuerpos_localizados == "FALSE" ~ "0",
                     cuerpos_localizados == "TRUE"  ~ "1",
                     cuerpos_localizados == cuerpos_localizados ~ cuerpos_localizados))

# Politica de seguridad 
df_1$politica_de_seguridad <- as.character(df_1$politica_de_seguridad)
df_1 <- df_1 %>%
  mutate(politica_de_seguridad = 
           case_when(politica_de_seguridad == "FALSE" ~ "0",
                     politica_de_seguridad == "TRUE"  ~ "1",
                     politica_de_seguridad == politica_de_seguridad ~ politica_de_seguridad))

### 3.3. Grupos criminales------------------------------------------------------
#base inicia df_1 y termina con df_gc

# ---- Separar columnas 
df_semicolon <- df_1 %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    grupo_criminal = str_replace_all(grupo_criminal, ":", ";"), 
    # Contar número de puntos y comas (;)
    grupo_criminal = str_count(grupo_criminal, ";")) %>% 
  select(grupo_criminal) 

max(df_semicolon$grupo_criminal, na.rm = T) # El máximo es 1 grupos criminales 

# Separar los nombres de los grupos criminales en columnas independientes 
df_gc <- df_1 %>% 
  mutate(grupo_criminal = str_replace_all(grupo_criminal, ":", ";")) %>% 
  separate(grupo_criminal, sep = ";", c("grupo1", 
                                        "grupo2"))  %>% 
  separate(Alianza, sep = ";", c("alianza1", "alianza2", "alianza3", "alianza4", 
                                 "alianza5")) %>%
  separate(Rival, sep = ";", c("rival1", "rival2"))

# Limpiar nombres

limpiar_grupos <- function(x){
  
  case_when(
    x  == " La Línea" ~ "La Línea",
    x  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
    x  == " Artistas Asesinos" ~ "Los Artistas Asesinos",
    x  == " Los Doblados" ~ "Los Artistas Asesinos",
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
    x  == "Salazar" ~ "Los Salazar",
    x  == "Cártel Jalisco"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == " El Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "cártel Jalisco Nueva Generación" ~ "Cártel Jalisco Nueva Generación (CJNG)", 
    x  == "Cártel Jalisco Nueva Generación"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
    x  == "CJGN" ~ "Cártel Jalisco Nueva Generación (CJNG)",
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
    x  == "cártel del pacífico" ~ "Cártel de Sinaloa",
    x  == "Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
    x  == "La Chapiza"   ~ "Cártel de Sinaloa / Los Chapitos",
    x  == " Chapiza"     ~ "Cártel de Sinaloa / Los Chapitos",
    x  == " Cártel de Sinaloa / Los Chapitos" ~ "Cártel de Sinaloa / Los Chapitos",
    x  == "grupo El Jale Azul / El Sargento Huracán" ~ "Grupo El Jale Azul",
    x  == "La Plaza" ~ "Cártel La Plaza",
    x  == " La Plaza" ~ "Cártel La Plaza",
    x  == "los viagras" ~ "Los Viagras",
    x  == " Los Viagras" ~ "Los Viagras",
    x  == "Los correa" ~ "Cártel de los Correa",
    x  == "Los Correa" ~ "Cártel de los Correa",
    x  == " Célula delictiva del Mamer" ~ "Grupo del Mamer",
    x  == " los caballeros templarios" ~ "Caballeros Templarios",
    x  == " Los Caballeros Templarios" ~ "Caballeros Templarios",
    x  == "GDC" ~ "Grupo del Chaparrito",
    x  == "la mera verga" ~ "La Mera Verga",
    x  == "Los Ciclones" ~ "Los Ciclones",
    x  == "Grupo Escorpión" ~ "Grupo Escorpión",
    x  == "Escorpiones" ~ "Grupo Escorpión",
    x  == " Los Ciclones" ~ "Los Ciclones",
    x  == "grupo del 13" ~ "Grupo del 13",
    x  == "banda delictiva de “El Malverde”" ~ "Banda de El Malverde",
    x  == "Nueva alianza" ~ "Nueva Alianza",
    x  == " Los Rusos" ~ "Los Rusos",
    x  == "cártel de Cancún" ~ "Cártel de Cancún",
    x  == "Los maseros" ~ "Los Maseros",
    x  == x ~ x
  )
}

df_gc <- df_gc %>% 
  mutate_at(.vars = c("grupo1", "grupo2", "alianza1", "alianza2", "alianza3",
                      "alianza4", "alianza5", "rival1", "rival2"), 
            .funs = ~limpiar_grupos(.))

unique(df_gc$grupo1)
unique(df_gc$grupo2)
unique(df_gc$alianza1)
unique(df_gc$alianza2)
unique(df_gc$alianza3)
unique(df_gc$alianza4)
unique(df_gc$alianza5)
unique(df_gc$rival1)
unique(df_gc$rival2)

### 3.4. Persona (homicidios + heridxs)-----------------------------------------
# homicidios 
df_phcolon <- df_gc %>% 
mutate(
  # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
  homic_clasif1 = str_replace_all(homic_clasif1, ":", ";"), 
  # Contar número de puntos y comas (;)
  homic_clasif1 = str_count(homic_clasif1, ";")) %>% 
  select(homic_clasif1)

max(df_phcolon$homic_clasif1, na.rm = T) # El máximo es 2

df_p <- df_gc %>% 
  mutate(homic_clasif1 = str_replace_all(homic_clasif1, ":", ";")) %>% 
  separate(homic_clasif1, sep = ";", c("homic_clasif1", 
                                       "homic_clasif2"))

unique(df_p$homic_clasif1)
unique(df_p$homic_clasif2)

# herdxs 
df_phecolon <- df_gc %>% 
mutate(
  # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
  heridos_clasif1 = str_replace_all(heridos_clasif1, ":", ";"), 
  # Contar número de puntos y comas (;)
  heridos_clasif1 = str_count(heridos_clasif1, ";")) %>% 
  select(heridos_clasif1)

max(df_phecolon$heridos_clasif1, na.rm = T) # El máximo es 1

df_p <- df_p %>% 
  mutate(heridos_clasif1 = str_replace_all(heridos_clasif1, ":", ";")) %>% 
  separate(heridos_clasif1, sep = ";", c("herido_clasif1", 
                                         "herido_clasif2"))

unique(df_p$herido_clasif1)
unique(df_p$herido_clasif2)

# clasificación - vector 

v_adulto_mayor <- c("adultos mayores", "tercera edad")

v_campesino <- c("campesino", "cultivador de aguacate")

v_ex_fuerzas_de_seguridad <- c("ex policía", "exmilitar", "Exagente de FGE",
                               "ex militar", "expolicía")

v_ex_funcionario_público <- c("exdirigente de partido","Exfuncionario público", 
                              "ex-síndico", "ex diputado", "Exdirigente del PRD",
                              "ex-alcalde", "ex alcalde y colaboradores",
                              "ex funcionario")

v_fseguridad <- c(" Policía Penitenciaria", "comandante de policía municipal", 
                  "elemento de la Guardia Nacional", "Guardia Nacional", "marino", 
                  "Militar", "militar", "policia", "policía auxiliar", 
                  "policía bancaria", "policía de tránsito", "Policía estatal", 
                  "polícia estatal", "Policía Estatal Preventiva (PEP)", 
                  "policía estatal", "policía fiscalia", "policía investigación", 
                  "policía ministerial", "policía municipal", "polícia transito", 
                  "policía y bombero", "un municipal", "dos estatales", " un militar",
                  "Policía Penitenciaria.", "funcionario público", 
                  "elemento de la Guardia Nacional ", "un policía", " un ministerial",
                  " dos estatales", "agente AIC-UECS",
                  "Comandante de la Policía de investigación", "policía comunitario",
                  "dos policias", "polícia", "Policía Metropolitana",
                  "agente de la FGE", "autoridad policial",
                  "guardia nacional", "dos policías", "agente", "semar", 
                  "agentes de tránsito", " exagente de la FGE",
                  " Guardia Nacional", "un militar", " un militar")

v_fpublico <- c("comisariado", "comisariado ejidal", 
                "comisario suplente de la comunidad", "delegado", 
                "empleado municipal", "Fiscalía Estatal", 
                "funcionario público", 
                "representante de Bienes Comunales de comunidad indígena", 
                "servidor público municipal", "síndico", 
                " delegado de la comunidad", "sindicalistas", "cfe", "comisario",
                "líder de unión nacional de transportistas del cambio", "líder cañero")

v_menor <- c("menor de edad", "un menor de edad", "2 menor de edad", 
             "1 menor de edad", " 2 menores de edad", " un menor de edad",
             " 2 menor de edad", "menor de edad", " 1 menor de edad",
             "una menor de edad", "dos menores")

v_moto <- c("motociclista", "motociclistas", "mototaxista", "motocilista")

v_pepenador <- c("pepenador")

v_criminal <- c("agresor", "cuatro sicarios", 
                "exintegrante de grupo criminal", "Gatilleros", "grupo criminal", 
                "huachicoleros", "ladrón", 
                "líder narcomenudista Abraham de Jesús H. C., alias El Croquis", 
                "Narcomenudista", "presunto agresor", "presunto criminal", 
                "Salvador Navarro Peñaloza El Zarco", "alias El Yofos",
                "Sicario", " 6 grupo criminal", " sicario", " dos delincuentes",
                " asaltante", " grupo del crimen organizado", "ocho sicarios",
                "delincuente", "asaltante", "narcodistribuidor", "presunto ladrón",
                "presunto asaltante", "miembros de célula delictiva", 
                "presunto delincuente", "presunto narcomenudista", "pandilla")

v_general <- c("desaparecidos", "deparecido", "Ex presidiario", "familia", 
               "hermano de edil", "hijo de dueño de negocio", 
               "Integrantes de Ronda comunitaria", "menonitas", "MULT", 
               "padre e hijo", "pareja", "pasajero", "Sacerdote", 
               "Sin especificar", "sin especificar", " desaparecido", " desparecido",
               " pasajero", " desaparecida", "desaparecido", "estudiante", "Miembro de los Loreto",
               "Hermano de expresidente municipal", "cliente de negocio", "ciclista",
               "exconvicto", "Cholo", "civil", "talamontes", " dos hermanos", "pasajero de taxi",
               "civíl", " civil", " acompañante")

v_lgbt <- c("transexual", "población lgbt")

v_ppl <- c("persona privada de la libertad", "presidiario", 
           "privado de la libertad", "recluso", " ppl", " secuestrado",
           "interno centro de rehabilitación", "detenido", "secuestrados",
           "secuestrado", "personas privadas de la libertad")

v_profesionista <- c("abogado", "abogado", "arquitecto", "doctor", 
                     "empresario", "enfermera", "gerente", 
                     "hotelero", "Profesor", "profesora", "periodista",
                     "Hotelero", "maestro", "profesor", "maestra",
                     "profesional de la salud")

v_sprivada <- c("custodio centro de rehabilitación", "custodios", 
                "guardaespaldas", "guardia de seguridad", "velador",
                "custodio", "escolta de autodefensa", " escolta")

v_calle <- c("condición de calle", "indigente", "persona en situación de calle")

v_taxi <- c("chofer de aplicación", "conductor de plataforma", 
            "taxista", "taxista", "trailero", "transportista", " chofer",
            " taxista", "uber", "chofer", "líder de taxistas",
            "taxi/chofer", "un taxista")

v_trabajador <- c("albañil", "ama de casa", "artesano", 
                  "ayudante de hojalatería", "bolillero", "Carnicero", 
                  "checador", "contratista", "despachadores de gasolina", 
                  "dueño de autolavado", "dueño de bar", "dueño de local", 
                  "empleado de tienda", "ganadero", "jardinero", "jornalero", 
                  "lavacoches", "líder de transportistas y de comerciantes", 
                  "limpiaparabrisas", "mecánico", "mesero", "obrero", "panadero", 
                  "pescadores", "Propietario de negocio", "talachero", 
                  "tamalero", "taquero", "trabajador", "empleado de hotel",
                  "vendedora", "despachador", "barbero", "empleado", "comerciante",
                  "repartidor", "vendedora ambulante", "beisbolista", 
                  "scort", "capataz", "músico", "chatarrero", "Integrante grupo de Danza",
                  "repartidor de agua", "empleado de autolavado", "dueño", 
                  "carpintero", "Dueño de negocio", "vendedor de pan",
                  " empleado de bar", " empleada de comercio")

v_migrante <- c("migrante")

# Limpiar nombres
df_p <- df_p %>%
mutate(homic_clasif1 = 
   case_when(homic_clasif1 %in% v_adulto_mayor               ~ "adulto mayor",
             homic_clasif1 %in% v_campesino                  ~ "campesino",
             homic_clasif1 %in% v_ex_fuerzas_de_seguridad    ~ "ex fuerzas de seguridad",
             homic_clasif1 %in% v_ex_funcionario_público     ~ "ex funcionario público",
             homic_clasif1 %in% v_fseguridad                 ~ "fuerzas de seguridad",
             homic_clasif1 %in% v_fpublico                   ~ "funcionario público",
             homic_clasif1 %in% v_menor                      ~ "menor de edad",
             homic_clasif1 %in% v_moto                       ~ "motociclista",
             homic_clasif1 %in% v_pepenador                  ~ "pepenador",
             homic_clasif1 %in% v_criminal                   ~ "población criminal",
             homic_clasif1 %in% v_general                    ~ "población en general",
             homic_clasif1 %in% v_lgbt                       ~ "población lgbt",
             homic_clasif1 %in% v_ppl                        ~ "población privada de la libertad",
             homic_clasif1 %in% v_profesionista              ~ "profesionista",
             homic_clasif1 %in% v_sprivada                   ~ "seguridad privada",
             homic_clasif1 %in% v_calle                      ~ "situación de calle",
             homic_clasif1 %in% v_ppl                        ~ "población privada de la libertad",
             homic_clasif1 %in% v_taxi                       ~ "taxi/chofer",
             homic_clasif1 %in% v_trabajador                 ~ "trabajador",
             homic_clasif1 %in% v_migrante                   ~ "migrante",
             homic_clasif1 ==   homic_clasif1                ~ homic_clasif1),
  homic_clasif2 =
    case_when(homic_clasif2 %in% v_adulto_mayor             ~ "adulto mayor",
              homic_clasif2 %in% v_campesino                ~ "campesino",
              homic_clasif2 %in% v_ex_fuerzas_de_seguridad  ~ "ex fuerzas de seguridad",
              homic_clasif2 %in% v_ex_funcionario_público   ~ "ex funcionario público",
              homic_clasif2 %in% v_fseguridad               ~ "fuerzas de seguridad",
              homic_clasif2 %in% v_fpublico                 ~ "funcionario público",
              homic_clasif2 %in% v_menor                    ~ "menor de edad",
              homic_clasif2 %in% v_moto                     ~ "motociclista",
              homic_clasif2 %in% v_pepenador                ~ "pepenador",
              homic_clasif2 %in% v_criminal                 ~ "población criminal",
              homic_clasif2 %in% v_general                  ~ "población en general",
              homic_clasif2 %in% v_lgbt                     ~ "población lgbt",
              homic_clasif2 %in% v_ppl                      ~ "población privada de la libertad",
              homic_clasif2 %in% v_profesionista            ~ "profesionista",
              homic_clasif2 %in% v_sprivada                 ~ "seguridad privada",
              homic_clasif2 %in% v_calle                    ~ "situación de calle",
              homic_clasif2 %in% v_ppl                      ~ "población privada de la libertad",
              homic_clasif2 %in% v_taxi                     ~ "taxi/chofer",
              homic_clasif2 %in% v_trabajador               ~ "trabajador",
              homic_clasif2 %in% v_migrante                 ~ "migrante",
              homic_clasif2 ==   homic_clasif2              ~ homic_clasif2),
 herido_clasif1 =
    case_when(herido_clasif1 %in% v_adulto_mayor             ~ "adulto mayor",
              herido_clasif1 %in% v_campesino                ~ "campesino",
              herido_clasif1 %in% v_ex_fuerzas_de_seguridad  ~ "ex fuerzas de seguridad",
              herido_clasif1 %in% v_ex_funcionario_público   ~ "ex funcionario público",
              herido_clasif1 %in% v_fseguridad               ~ "fuerzas de seguridad",
              herido_clasif1 %in% v_fpublico                 ~ "funcionario público",
              herido_clasif1 %in% v_menor                    ~ "menor de edad",
              herido_clasif1 %in% v_moto                     ~ "motociclista",
              herido_clasif1 %in% v_pepenador                ~ "pepenador",
              herido_clasif1 %in% v_criminal                 ~ "población criminal",
              herido_clasif1 %in% v_general                  ~ "población en general",
              herido_clasif1 %in% v_lgbt                     ~ "población lgbt",
              herido_clasif1 %in% v_ppl                      ~ "población privada de la libertad",
              herido_clasif1 %in% v_profesionista            ~ "profesionista",
              herido_clasif1 %in% v_sprivada                 ~ "seguridad privada",
              herido_clasif1 %in% v_calle                    ~ "situación de calle",
              herido_clasif1 %in% v_ppl                      ~ "población privada de la libertad",
              herido_clasif1 %in% v_taxi                     ~ "taxi/chofer",
              herido_clasif1 %in% v_trabajador               ~ "trabajador",
              herido_clasif1 %in% v_migrante                 ~ "migrante",
              herido_clasif1 ==   herido_clasif1             ~ herido_clasif1),
  herido_clasif2 =
    case_when(herido_clasif2 %in% v_adulto_mayor             ~ "adulto mayor",
              herido_clasif2 %in% v_campesino                ~ "campesino",
              herido_clasif2 %in% v_ex_fuerzas_de_seguridad  ~ "ex fuerzas de seguridad",
              herido_clasif2 %in% v_ex_funcionario_público   ~ "ex funcionario público",
              herido_clasif2 %in% v_fseguridad               ~ "fuerzas de seguridad",
              herido_clasif2 %in% v_fpublico                 ~ "funcionario público",
              herido_clasif2 %in% v_menor                    ~ "menor de edad",
              herido_clasif2 %in% v_moto                     ~ "motociclista",
              herido_clasif2 %in% v_pepenador                ~ "pepenador",
              herido_clasif2 %in% v_criminal                 ~ "población criminal",
              herido_clasif2 %in% v_general                  ~ "población en general",
              herido_clasif2 %in% v_lgbt                     ~ "población lgbt",
              herido_clasif2 %in% v_ppl                      ~ "población privada de la libertad",
              herido_clasif2 %in% v_profesionista            ~ "profesionista",
              herido_clasif2 %in% v_sprivada                 ~ "seguridad privada",
              herido_clasif2 %in% v_calle                    ~ "situación de calle",
              herido_clasif2 %in% v_ppl                      ~ "población privada de la libertad",
              herido_clasif2 %in% v_taxi                     ~ "taxi/chofer",
              herido_clasif2 %in% v_trabajador               ~ "trabajador",
              herido_clasif2 %in% v_migrante                 ~ "migrante",
              herido_clasif2 ==   herido_clasif2             ~ herido_clasif2))

unique(df_p$homic_clasif1)
unique(df_p$homic_clasif2)

unique(df_p$herido_clasif1)
unique(df_p$herido_clasif2)

### 3.5. Cuerpos----------------------------------------------------------------

# separar valores con más opciones
df_semicolon0 <- df_p %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    cuerpos_modo1 = str_replace_all(cuerpos_modo1, ":", ";"), 
    # Contar número de puntos y comas (;)
    semicolon0 = str_count(cuerpos_modo1, ";")) %>% 
  select(semicolon0) 

max(df_semicolon0$semicolon0, na.rm = T) # El máximo es 2

df_c <- df_p %>% 
  mutate(cuerpos_modo1 = str_replace_all(cuerpos_modo1, ":", ";")) %>% 
  separate(cuerpos_modo1, sep = ";", c("cuerpo_modo1", 
                                       "cuerpo_modo2"))

unique(df_c$cuerpo_modo1)
unique(df_c$cuerpo_modo2)

# clasificación - vector 
v_armadefuego <- c("Impactos de arma de fuego", "impacto de arma de fuego",
                   "impactos de arma de fuego", " impacto de armas de fuego",
                   " impactos de bala", " impacto de arma de fuego",
                   " impactos de arma de fuego")

v_tortura_maniatado <- c("golpe en la cabeza", "arma blanca", "huellas de tortura",
                         "huellas de violencia", "Huellas de tortura / cuerpo maniatado",
                         "colgado", "golpes", "maniatado", "estrangulado", "Huellas de violencia",
                         "Heridas arma blanca", "ahorcado", "lesión en el cuello",
                         "huellas de asfixia", "heridas arma blanca", "Huella de tortura",
                         " tortura", " colgados", " maniatado", " soga en el cuello", " golpes")

v_plastico_encobijado <- c(" bolsa de plastico", " bolsa de plástico", " bolsas plastico",
                           "Bolsas de plástico / encobijado", "bolsas de plástico",
                           "encobijado", "cubierto con material", "bolsa de plástico",
                           "envuelto en una lona", " encobijado")

v_descomposicion <- c("Descomposición / restos óseos / enterrado / fosas",
                      "restos óseos", "Descomposición", "Osamenta", "enterrado",
                      "semienterrado", "fosa", " fosa", " cuerpos en descomposición",
                      "descomposición")

v_calcinado <- c("calcinado", " calcinado", "Calcinado", " Calcinado")

v_desmembrado <- c("Desmembrado / alguna parte del cuerpo", "decapitado",
                   "degollada", "cabeza","desmembrado", "pie", "degollado",
                   "maleta", "encajuelados", "mutilado", "en costal",
                   "extremidad en hielera", "extremidad", "encajuelado",
                   "entambados", "enmaletado", " hieleras", "restos humanos")

v_otro <- c("Tambo de agua", "Otro", "desnudo", "tinaco", "tambo de agua")

v_sinformación <- c("ojos vendados", "sin información", "cuerpo completo", " cabeza de animal")

# Limpiar nombres

df_c <- df_c %>% 
  mutate(cuerpo_modo1 = 
           case_when(cuerpo_modo1 %in% v_armadefuego          ~ "Impactos de arma de fuego",
                     cuerpo_modo1 %in% v_tortura_maniatado    ~ "Huellas de tortura / cuerpo maniatado",
                     cuerpo_modo1 %in% v_plastico_encobijado  ~ "Bolsas de plástico / encobijado",
                     cuerpo_modo1 %in% v_descomposicion       ~ "Descomposición / restos óseos / enterrado / fosas",
                     cuerpo_modo1 %in% v_calcinado            ~ "Calcinado",
                     cuerpo_modo1 %in% v_desmembrado          ~ "Desmembrado / alguna parte del cuerpo",
                     cuerpo_modo1 %in% v_otro                 ~ "Otro",
                     cuerpo_modo1 %in% v_sinformación         ~ "Sin información",
                     cuerpo_modo1 ==   cuerpo_modo1           ~ cuerpo_modo1),
  cuerpo_modo2 = 
           case_when(cuerpo_modo2 %in% v_armadefuego          ~ "Impactos de arma de fuego",
                     cuerpo_modo2 %in% v_tortura_maniatado    ~ "Huellas de tortura / cuerpo maniatado",
                     cuerpo_modo2 %in% v_plastico_encobijado  ~ "Bolsas de plástico / encobijado",
                     cuerpo_modo2 %in% v_descomposicion       ~ "Descomposición / restos óseos / enterrado / fosas",
                     cuerpo_modo2 %in% v_calcinado            ~ "Calcinado",
                     cuerpo_modo2 %in% v_desmembrado          ~ "Desmembrado / alguna parte del cuerpo",
                     cuerpo_modo2 %in% v_otro                 ~ "Otro",
                     cuerpo_modo2 %in% v_sinformación         ~ "Sin información",
                     cuerpo_modo2 ==   cuerpo_modo2           ~ cuerpo_modo2))

unique(df_c$cuerpo_modo1)
unique(df_c$cuerpo_modo2)

### 3.6. Cuerpos localizado-----------------------------------------------------

unique(df_c$cuerpos_lugar1)

# separar valores 
df_semicolon1 <- df_c %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    cuerpos_lugar1 = str_replace_all(cuerpos_lugar1, ":", ";"), 
    # Contar número de puntos y comas (;)
    semicolon1 = str_count(cuerpos_lugar1, ";")) %>% 
  select(semicolon1) 

max(df_semicolon1$semicolon1, na.rm = T) # El máximo es 1

df_c <- df_c %>% 
  mutate(cuerpos_lugar1 = str_replace_all(cuerpos_lugar1, ":", ";")) %>% 
  separate(cuerpos_lugar1, sep = ";", c("cuerpo_lugar1", 
                                        "cuerpo_lugar2"))

unique(df_c$cuerpo_lugar1)
unique(df_c$cuerpo_lugar2)

# clasificación - vector 
v_adomicilio <- c("afuera de domicilio")

v_aestablecimiento <- c("afuera de escuela", "afuera de negocio", 
                        "afuera de oficina", "afuera de un panteón", 
                        "afuera de bar", "afuera de mercado", "afuera de velorio",
                        "transmisores de cámaras de seguridad",
                        "afuera de establecimiento",
                        "detrás de panteón", "fuera de negocio")

v_baldio <- c("basurero", "Calle", "campo", "Ejido",
              "finca", "Lote baldío", "obra negra", "paraje", 
              "pozo", "rancho", "río", "terreno", "terreo baldío", "zona boscosa",
              "terreno baldío", "terracería", "Barranco", "brecha",
              "lugar abandonado", "escombros", "tapias", "maleza",
              "zona deshabitada", "fosa clandestina",
              "terreno baldió", "matorrales", "zona cerril" )

v_banco <- c("banco", "cajero")

v_barbe <- c("barbería")

v_calle <- c("calle", "camino", "camino de terracería", " calle", 
             "17 calle Poniente y Venustiano Carranza de la colonia 1 de Mayo, a unos 800 metros de la base cangrejo de la Secretaría de Seguridad Pública Municipal.",
             "debajo de un puente", "socavon", "zanja", "tambo", "despoblado", "caja de cartón", "puente", " hielera")

v_campo <- c("campo de fútbol", "canchas de fútbol", "Unidad deportiva",
             "estadio de béisbol")

v_carretera <- c("carretera", "vías del tren", "entronque", "Carretera", "vías de tren",
                 "carretera", " carretera")

v_iglesia <- c("iglesia", "terreno de templo", "seminario")

v_domicilio <- c("casa", "Domicilio", "jacal", "vivienda", "vivienda de comerciante", "obra", "cuartería",
                 "vivienda abandonada", "domicilio en obra negra", "casa abandonada", "casa de seguridad",
                 " vivienda abandonada")

v_estacionamiento <- c("estacionamiento", "estacionamiento de bar", "estacionamiento supermercado")

v_fiesta <- c("fiesta", "salón de fiestas")

v_gas <- c("gasolinera", " gasera", "gasolineria")

v_hosp <- c("hospital", "clínica")

v_gub <- c("Centro penitenciario", "cereso", "escuela", "estación de bomberos",
           "estación de tren", "institución pública", "ministerio público", "oficina de gobierno", "comandancia",
           "retén", "afuera de campo militar", "afuera de cereso", "relleno sanitario")

v_local <- c("centro comercial", "comercio", "comercio", "gasera", "comercio", "purificadora de agua", 
             "local comercial", "sitio de taxis", "Taller de Herrería", "trituradora", "verificentro", " purificadora de agua")

v_nego <- c("base de taxis", "bodega","cabañas", "carpintería", "expendio", "funeraria", 
            "hotel", "ladrilleras", "motel", "Negocio", "negocio", "laboratorio", "recicladora", 
            " laboratorio", "farmacia", "plaza de ganado", "pizzería", "corral",
            "Billar", "forrajera", "caballerizas", "negocio", "parque ecológico")

v_parque <- c("área verde", "huerta", "jardín", "Parque", "predio rústico", "vivero", 
              "sembradío", "predio", "sierra", "cerro", "parcela", "Nopalera", "barranca",
              "zona rural", "montaña", "parque", "reserva", "bosque", "jardinera", "monte")    

v_comida <-c("cafetería", "restaurante", "taquería", "cenaduría")

v_entretenimiento <- c("carrera de caballos", "casino", "local de maquinitas", "negocio de maquinas tragamodenas", 
                       "negocio de maquinitas", "palenque", "playa", "Rodeo", "jaripeo", "plaza principal", "plaza",
                       "plaza comercial", "quinta")

v_meca <- c("autolavado", "refaccionaria", "taller mecánico", "taller de motos", "taller mecánico", " refaccionaria")

v_taxi <- c("Central camionera", "transporte público", " taxi", " taller mecánico", "taxi")

v_abarrotes <- c("carnicería", "mercado", "negocio de comida", "negocio: fruteria", " fruteria", "oxxo", "puesto ambulante",
                 "tianguis", "tienda", "tortillería")

v_vehiculo <- c("tráiler", "vehículo", "vehiculo", " vehículo", " motocicleta", " autobús", "auto", 
                "agresión a vehículo", "vagón de tren", "coche", "vehículo en llamas", "Camioneta",
                "cajuela", "carro", "encajuelado", "Dentro de un auto", "remolcador",
                " vehículo calcinado", "vehículo")

v_alcohol <- c("bar", "bar/pulquería", "Cantina", "comercio", "depósito de cerveza", 
               "depósito de cerveza", "vinateria", " depósito de cerveza")

v_consumo <- c("albergue", "anexo", "campo de cultivo", 
               "centro de rehabilitación", "punto de venta de droga", " picadero",
               "picadero", "centro rehabilitación")

v_agua <- c("drenaje", "canal de aguas negras", "cisterna", "a orilla de un río",
            "A orillas de un río", "deposito de agua", "laguna", "orilla de río",
            "mar", "arroyo", "Dren", "canal de riego", "Canal", "Canal de aguas negras",
            "aguas negras", "planta tratadora de agua", "alcantarilla", "Laguna",
            "fondo de un pozo", "noria", "manglar", "pozo de agua", " pozo de agua")

v_otro1 <- c("propiedad privada", "Rancho", "obra en construcción", "Obra negra",
             "construcción", "cuerpo de agua")


# Limpiar nombres 
df_c <- df_c %>% 
  mutate(cuerpo_lugar1 = 
           case_when(cuerpo_lugar1 %in% v_adomicilio        ~ "afuera de domicilio",
                     cuerpo_lugar1 %in% v_aestablecimiento  ~ "afuera de establecimiento",
                     cuerpo_lugar1 %in% v_baldio            ~ "baldío/maleza/terreno",
                     cuerpo_lugar1 %in% v_banco             ~ "banco",
                     cuerpo_lugar1 %in% v_barbe             ~ "barbería/salón de belleza/estética",
                     cuerpo_lugar1 %in% v_calle             ~ "calle",
                     cuerpo_lugar1 %in% v_campo             ~ "campo deportivo",
                     cuerpo_lugar1 %in% v_carretera         ~ "carretera",
                     cuerpo_lugar1 %in% v_iglesia           ~ "comunidad/religión",
                     cuerpo_lugar1 %in% v_domicilio         ~ "domicilio",
                     cuerpo_lugar1 %in% v_estacionamiento   ~ "estacionamiento",
                     cuerpo_lugar1 %in% v_fiesta            ~ "fiesta/salón de fiestas",
                     cuerpo_lugar1 %in% v_gas               ~ "gasolinera",
                     cuerpo_lugar1 %in% v_hosp              ~ "hospital/consultorio",
                     cuerpo_lugar1 %in% v_gub               ~ "instancia gubernamental",
                     cuerpo_lugar1 %in% v_local             ~ "local comercial",
                     cuerpo_lugar1 %in% v_nego              ~ "negocio",
                     cuerpo_lugar1 %in% v_parque            ~ "parque/jardín/espacio público",
                     cuerpo_lugar1 %in% v_comida            ~ "puesto de comida/restaurante/taquería",
                     cuerpo_lugar1 %in% v_entretenimiento   ~ "sitio de entretenimiento",
                     cuerpo_lugar1 %in% v_meca              ~ "taller mecánico/vulcanizadora/lavacoches",
                     cuerpo_lugar1 %in% v_taxi              ~ "taxi/transporte público",
                     cuerpo_lugar1 %in% v_abarrotes         ~ "tienda de abarrotes/mercado/venta de comida",
                     cuerpo_lugar1 %in% v_vehiculo          ~ "vehículo",
                     cuerpo_lugar1 %in% v_alcohol           ~ "venta de alcohol",
                     cuerpo_lugar1 %in% v_consumo           ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     cuerpo_lugar1 %in% v_agua              ~ "río/tuberia/canales de agua",
                     cuerpo_lugar1 %in% v_otro1             ~ "otro",
                     cuerpo_lugar1 ==   cuerpo_lugar1       ~ cuerpo_lugar1), 
         cuerpo_lugar2 = 
           case_when(cuerpo_lugar2 %in% v_adomicilio        ~ "afuera de domicilio",
                     cuerpo_lugar2 %in% v_aestablecimiento  ~ "afuera de establecimiento",
                     cuerpo_lugar2 %in% v_baldio            ~ "baldío/maleza/terreno",
                     cuerpo_lugar2 %in% v_banco             ~ "banco",
                     cuerpo_lugar2 %in% v_barbe             ~ "barbería/salón de belleza/estética",
                     cuerpo_lugar2 %in% v_calle             ~ "calle",
                     cuerpo_lugar2 %in% v_campo             ~ "campo deportivo",
                     cuerpo_lugar2 %in% v_carretera         ~ "carretera",
                     cuerpo_lugar2 %in% v_iglesia           ~ "comunidad/religión",
                     cuerpo_lugar2 %in% v_domicilio         ~ "domicilio",
                     cuerpo_lugar2 %in% v_estacionamiento   ~ "estacionamiento",
                     cuerpo_lugar2 %in% v_fiesta            ~ "fiesta/salón de fiestas",
                     cuerpo_lugar2 %in% v_gas               ~ "gasolinera",
                     cuerpo_lugar2 %in% v_hosp              ~ "hospital/consultorio",
                     cuerpo_lugar2 %in% v_gub               ~ "instancia gubernamental",
                     cuerpo_lugar2 %in% v_local             ~ "local comercial",
                     cuerpo_lugar2 %in% v_nego              ~ "negocio",
                     cuerpo_lugar2 %in% v_parque            ~ "parque/jardín/espacio público",
                     cuerpo_lugar2 %in% v_comida            ~ "puesto de comida/restaurante/taquería",
                     cuerpo_lugar2 %in% v_entretenimiento   ~ "sitio de entretenimiento",
                     cuerpo_lugar2 %in% v_meca              ~ "taller mecánico/vulcanizadora/lavacoches",
                     cuerpo_lugar2 %in% v_taxi              ~ "taxi/transporte público",
                     cuerpo_lugar2 %in% v_abarrotes         ~ "tienda de abarrotes/mercado/venta de comida",
                     cuerpo_lugar2 %in% v_vehiculo          ~ "vehículo",
                     cuerpo_lugar2 %in% v_alcohol           ~ "venta de alcohol",
                     cuerpo_lugar2 %in% v_consumo           ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     cuerpo_lugar1 %in% v_agua              ~ "río/tuberia/canales de agua",
                     cuerpo_lugar1 %in% v_otro1             ~ "otro",
                     cuerpo_lugar2 ==   cuerpo_lugar2       ~ cuerpo_lugar2))

unique(df_c$cuerpo_lugar1)
unique(df_c$cuerpo_lugar2)

### 3.7. Ataque-----------------------------------------------------------------
# separar valores  
df_semicolon2 <- df_c %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    ataque_armado_clean = str_replace_all(ataque_armado_clean, ":", ";"), 
    # Contar número de puntos y comas (;)
    semicolon2 = str_count(ataque_armado_clean, ";")) %>% 
  select(semicolon2) 

max(df_semicolon2$semicolon2, na.rm = T) # El máximo es 3

df_ata <- df_c %>% 
  mutate(ataque_armado_clean = str_replace_all(ataque_armado_clean, ":", ";")) %>% 
  separate(ataque_armado_clean, sep = ";", c("ataque1", 
                                             "ataque2",
                                             "ataque3"))

unique(df_ata$ataque1)
unique(df_ata$ataque2)
unique(df_ata$ataque3)

# clasificación - vector 

va_blanca <- c("arma blanca","asalto con arma blanca", "asfixia", 
              "degollación", "lapidación", "estrangulación", "Poncha llantas",
              "Ponchallantas", "martillazos", " arma blanca", " arma blanca")

va_fuego <- c("arma de fuego", "disparos a casa habitación", "disparos al aire", "detonaciones",
             "disparos",  "agresión", "agresión a comercio", "agresión a vehiculo",
             "agresión a vivienda", "agresión a vivienda; agresión a vehículo", 
             "arma de fuego; agresión a policías", "arma de fuego; secuetro",
             "asalto con arma de fuego", "Bala perdida", "disparos (en general)", 
             "disparos a automóvil", "disparos a cámaras de vigilancia", 
             "disparos a comercio", "fachada baleada", "presunto feminicidio",
             "billar", "secuestro armado", "levantón armado", 
             "agresión a vehículo", " arma de fuego", " agresión a policías",
             " agresión a vehículo", " agresión a policías")

va_balacera <- c("balacera", "tiroteo")

va_enfrentamiento <- c("enfrentamiento")

va_incendio <- c("bomba molotov", "incendio de casa", "incendio de auto", 
                "incendio de local", "incendio de vehículo", "arma explosiva",
                "incendio", "incendio provocado", "Narcobloqueos", "bomba",
                "incendiado", " incendiado")

va_fseguridad <- c("desalojo", "persecución")

va_persecucion <- c("persecución")

va_rina <- c("golpes", "riña", "golpes/riña", "riña; arma blanca", 
            "riña; arma de fuego", "riña; arma de fuego; agresión a policías",
            "linchamiento")

va_otro <- c("robo", "asalto", "narcotizado", "robo de autos", 
             "robo de vehículo", " atropellamiento intencional")

# Limpiar nombres 
df_ata <- df_ata %>% 
  mutate(ataque1 = 
           case_when(ataque1 %in% va_blanca             ~ "agresión con arma blanca",
                     ataque1 %in% va_fuego              ~ "agresión con arma de fuego",
                     ataque1 %in% va_balacera           ~ "balacera",
                     ataque1 %in% va_enfrentamiento     ~ "enfrentamiento",
                     ataque1 %in% va_incendio           ~ "incendio/explosión",
                     ataque1 %in% va_fseguridad         ~ "intervención fuerzas de seguridad",
                     ataque1 %in% va_persecucion        ~ "persecución",
                     ataque1 %in% va_rina               ~ "riña",
                     ataque1 %in% va_otro               ~ "otro",
                     ataque1 ==   ataque1               ~ ataque1), 
         ataque2 = 
           case_when(ataque2 %in% va_blanca             ~ "agresión con arma blanca",
                     ataque2 %in% va_fuego              ~ "agresión con arma de fuego",
                     ataque2 %in% va_balacera           ~ "balacera",
                     ataque2 %in% va_enfrentamiento     ~ "enfrentamiento",
                     ataque2 %in% va_incendio           ~ "incendio/explosión",
                     ataque2 %in% va_fseguridad         ~ "intervención fuerzas de seguridad",
                     ataque2 %in% va_persecucion        ~ "persecución",
                     ataque2 %in% va_rina               ~ "riña",
                     ataque2 %in% va_otro               ~ "otro",
                     ataque2 ==   ataque2               ~ ataque2), 
         ataque3 = 
           case_when(ataque3 %in% va_blanca             ~ "agresión con arma blanca",
                     ataque3 %in% va_fuego              ~ "agresión con arma de fuego",
                     ataque3 %in% va_balacera           ~ "balacera",
                     ataque3 %in% va_enfrentamiento     ~ "enfrentamiento",
                     ataque3 %in% va_incendio           ~ "incendio/explosión",
                     ataque3 %in% va_fseguridad         ~ "intervención fuerzas de seguridad",
                     ataque3 %in% va_persecucion        ~ "persecución",
                     ataque3 %in% va_rina               ~ "riña",
                     ataque3 %in% va_otro               ~ "otro",
                     ataque3 ==   ataque3               ~ ataque3))


unique(df_ata$ataque1)
unique(df_ata$ataque2)
unique(df_ata$ataque3)

# Corrección de valores duplicados 
df_ata <- df_ata %>% 
  mutate(ataque3 =ifelse(ataque3=="agresión con arma de fuego",NA, ataque3))


### 3.7. Lugar de ataque--------------------------------------------------------

# separar valores con más opciones
df_semicolon3 <- df_ata %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    lugar_ataque_clean = str_replace_all(lugar_ataque_clean, ":", ";"), 
    # Contar número de puntos y comas (;)
    lugar_ataque_clean = str_count(lugar_ataque_clean, ";")) %>% 
  select(lugar_ataque_clean) 

max(df_semicolon3$lugar_ataque_clean, na.rm = T) # El máximo es 1

df_lu <- df_ata %>% 
  mutate(lugar_ataque_clean = str_replace_all(lugar_ataque_clean, ":", ";")) %>% 
  separate(lugar_ataque_clean, sep = ";", c("lugar_ataque1", 
                                            "lugar_ataque2"))

unique(df_lu$lugar_ataque1)
unique(df_lu$lugar_ataque2)

# clasificación - vector 
v_adomicilio2 <- c("afuera de domicilio")

v_aestablecimiento2 <- c("afuera de escuela", "afuera de negocio", 
                        "afuera de oficina", "afuera de un panteón", 
                        "afuera de bar", "afuera de mercado", "afuera de velorio",
                        "transmisores de cámaras de seguridad")

v_baldio2 <- c("basurero", "Calle", "campo", "canal de aguas negras", "Ejido",
              "finca", "Lote baldío", "montaña", "obra negra", "paraje", 
              "pozo", "rancho", "río", "terreno", "terreo baldío", "zona boscosa")

v_banco2 <- c("banco", "cajero")

v_barbe2 <- c("barbería")

v_calle2 <- c("calle", "camino", "camino de terracería", " calle")
             

v_campo2 <- c("campo de fútbol", "canchas de fútbol", "Unidad deportiva")

v_carretera2 <- c("carretera", "vías del tren")

v_iglesia2 <- c("iglesia")

v_domicilio2 <- c("casa", "Domicilio", "jacal", "vivienda", "vivienda de comerciante", "obra", "cuartería")

v_estacionamiento2 <- c("estacionamiento", "estacionamiento de bar", "estacionamiento supermercado")

v_fiesta2 <- c("fiesta", "salón de fiestas")

v_gas2 <- c("gasolinera", " gasera", "gasolineria")

v_hosp2 <- c("hospital", "clínica")

v_gub2 <- c("Centro penitenciario", "cereso", "escuela", "estación de bomberos",
           "estación de tren", "institución pública", "ministerio público", "oficina de gobierno", "comandancia",
           "retén", "afuera de campo militar", "afuera de cereso")

v_local2 <- c("centro comercial", "comercio", "comercio", "gasera", "comercio", "purificadora de agua", 
             "local comercial", "sitio de taxis", "Taller de Herrería", "trituradora", "verificentro", " purificadora de agua")

v_nego2 <- c("base de taxis", "bodega","cabañas", "carpintería", "expendio", "funeraria", 
            "hotel", "ladrilleras", "motel", "Negocio", "negocio", "laboratorio", "recicladora", 
            " laboratorio", "farmacia", "plaza de ganado", "pizzería", "corral",
            "Billar", "forrajera", "caballerizas")

v_parque2 <- c("área verde", "huerta", "jardín", "Parque", "a orilla de un río")    

v_comida2 <-c("cafetería", "restaurante", "taquería", "cenaduría")

v_entretenimiento2 <- c("carrera de caballos", "casino", "local de maquinitas", "negocio de maquinas tragamodenas", 
                       "negocio de maquinitas", "palenque", "playa", "Rodeo", "jaripeo", "plaza principal", "plaza",
                       "plaza comercial")

v_meca2 <- c("autolavado", "refaccionaria", "taller mecánico", 
             "taller de motos", "taller mecánico", " refaccionaria", "vulcanizadora")

v_taxi2 <- c("Central camionera", "transporte público", " taxi", " taller mecánico")

v_abarrotes2 <- c("carnicería", "mercado", "negocio de comida", "negocio: fruteria", " fruteria", "oxxo", "puesto ambulante",
                 "tianguis", "tienda", "tortillería")

v_vehiculo2 <- c("tráiler", "vehículo", "vehiculo", " vehículo", " motocicleta", " autobús", "auto", "agresión a vehículo")

v_alcohol2 <- c("bar", "bar/pulquería", "Cantina", "comercio", "depósito de cerveza", 
               "depósito de cerveza", "vinateria", " depósito de cerveza")

v_consumo2 <- c("albergue", "anexo", "campo de cultivo", 
               "centro de rehabilitación", "punto de venta de droga", " picadero",
               "picadero", "centro rehabilitación")


# Limpiar nombres 
df_lu <- df_lu %>% 
  mutate(lugar_ataque1 = 
           case_when(lugar_ataque1 %in% v_adomicilio2         ~ "afuera de domicilio",
                     lugar_ataque1 %in% v_aestablecimiento2   ~ "afuera de establecimiento",
                     lugar_ataque1 %in% v_baldio2             ~ "baldío/maleza/terreno",
                     lugar_ataque1 %in% v_banco2              ~ "banco",
                     lugar_ataque1 %in% v_barbe2              ~ "barbería/salón de belleza/estética",
                     lugar_ataque1 %in% v_calle2              ~ "calle",
                     lugar_ataque1 %in% v_campo2              ~ "campo deportivo",
                     lugar_ataque1 %in% v_carretera2          ~ "carretera",
                     lugar_ataque1 %in% v_iglesia2            ~ "comunidad/religión",
                     lugar_ataque1 %in% v_domicilio2          ~ "domicilio",
                     lugar_ataque1 %in% v_estacionamiento2    ~ "estacionamiento",
                     lugar_ataque1 %in% v_fiesta2             ~ "fiesta/salón de fiestas",
                     lugar_ataque1 %in% v_gas2                ~ "gasolinera",
                     lugar_ataque1 %in% v_hosp2               ~ "hospital/consultorio",
                     lugar_ataque1 %in% v_gub2                ~ "instancia gubernamental",
                     lugar_ataque1 %in% v_local2              ~ "local comercial",
                     lugar_ataque1 %in% v_nego2               ~ "negocio",
                     lugar_ataque1 %in% v_parque2             ~ "parque/jardín/espacio público",
                     lugar_ataque1 %in% v_comida2             ~ "puesto de comida/restaurante/taquería",
                     lugar_ataque1 %in% v_entretenimiento2    ~ "sitio de entretenimiento",
                     lugar_ataque1 %in% v_meca2               ~ "taller mecánico/vulcanizadora/lavacoches",
                     lugar_ataque1 %in% v_taxi2               ~ "taxi/transporte público",
                     lugar_ataque1 %in% v_abarrotes2          ~ "tienda de abarrotes/mercado/venta de comida",
                     lugar_ataque1 %in% v_vehiculo2           ~ "vehículo",
                     lugar_ataque1 %in% v_alcohol2            ~ "venta de alcohol",
                     lugar_ataque1 %in% v_consumo2            ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     lugar_ataque1 ==   lugar_ataque1         ~ lugar_ataque1), 
         lugar_ataque2 = 
           case_when(lugar_ataque2 %in% v_adomicilio2         ~ "afuera de domicilio",
                     lugar_ataque2 %in% v_aestablecimiento2   ~ "afuera de establecimiento",
                     lugar_ataque2 %in% v_baldio2             ~ "baldío/maleza/terreno",
                     lugar_ataque2 %in% v_banco2              ~ "banco",
                     lugar_ataque2 %in% v_barbe2              ~ "barbería/salón de belleza/estética",
                     lugar_ataque2 %in% v_calle2              ~ "calle",
                     lugar_ataque2 %in% v_campo2              ~ "campo deportivo",
                     lugar_ataque2 %in% v_carretera2          ~ "carretera",
                     lugar_ataque2 %in% v_iglesia2            ~ "comunidad/religión",
                     lugar_ataque2 %in% v_domicilio2          ~ "domicilio",
                     lugar_ataque2 %in% v_estacionamiento2    ~ "estacionamiento",
                     lugar_ataque2 %in% v_fiesta2             ~ "fiesta/salón de fiestas",
                     lugar_ataque2 %in% v_gas2                ~ "gasolinera",
                     lugar_ataque2 %in% v_hosp2               ~ "hospital/consultorio",
                     lugar_ataque2 %in% v_gub2                ~ "instancia gubernamental",
                     lugar_ataque2 %in% v_local2              ~ "local comercial",
                     lugar_ataque2 %in% v_nego2               ~ "negocio",
                     lugar_ataque2 %in% v_parque2             ~ "parque/jardín/espacio público",
                     lugar_ataque2 %in% v_comida2             ~ "puesto de comida/restaurante/taquería",
                     lugar_ataque2 %in% v_entretenimiento2    ~ "sitio de entretenimiento",
                     lugar_ataque2 %in% v_meca2               ~ "taller mecánico/vulcanizadora/lavacoches",
                     lugar_ataque2 %in% v_taxi2               ~ "taxi/transporte público",
                     lugar_ataque2 %in% v_abarrotes2          ~ "tienda de abarrotes/mercado/venta de comida",
                     lugar_ataque2 %in% v_vehiculo2           ~ "vehículo",
                     lugar_ataque2 %in% v_alcohol2            ~ "venta de alcohol",
                     lugar_ataque2 %in% v_consumo2            ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     lugar_ataque2 ==   lugar_ataque2         ~ lugar_ataque2))

unique(df_lu$lugar_ataque1)
unique(df_lu$lugar_ataque2)

### 3.8 Actividad-----------------------------------------------------------------
# separar valores con más opciones
df_actsemicolon <- df_lu %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    actividad_grupo = str_replace_all(actividad_grupo, ":", ";"), 
    # Contar número de puntos y comas (;)
    actividad_grupo = str_count(actividad_grupo, ";")) %>% 
  select(actividad_grupo) 

max(df_actsemicolon$actividad_grupo, na.rm = T) # El máximo es 1

df_act <- df_lu %>% 
  mutate(actividad_grupo = str_replace_all(actividad_grupo, ":", ";")) %>% 
  separate(actividad_grupo, sep = ";", c("actividad_grupo1", 
                                         "actividad_grupo2"))

unique(df_act$actividad_grupo1)
unique(df_act$actividad_grupo2)


# clasificación - vector 

vact_agresion <- c("Agresiones", "Desalojo", "disparos")

vact_extorison <- c("cobro de piso", "halcón",
                    "extorsión", " cobro de piso")

vact_enfrentamiento <- c("Enfrentamiento")

vact_noviolenta <- c("narcofiesta", "Presencia no violenta", 
                     "Entrega de juguetes", "préstamos",
                     "entrega de secuestradores")

vact_amenaza <- c("toque de queda", "amenaza")

vact_secuestro <- c("secuestro","Tráfico de personas")

vact_psinespecificar <- c("Presencia (sin especificar)", "presencia",
                          "presencia (varios)")

vact_labycamp <- c("laboratorio","campamento", "reclutamiento")

vact_fseguridad <- c("desmantelamiento cédula criminal", "detención",
                     "Decomiso")

vact_coru_rob <- c("lavado de dinero", "robo", "contubernio")

vact_sustancias <- c("Sustancias ilícitas", " tráfico de sustancias",
                     "  tráfico de sustancias")

vact_armas <- c("tráfico de armas")  

# Limpiar nombres 
df_act <- df_act %>% 
  mutate(actividad_grupo1 = 
           case_when(actividad_grupo1 %in% vact_agresion        ~ "agresión",
                     actividad_grupo1 %in% vact_extorison       ~ "vigilancia/extorsión",
                     actividad_grupo1 %in% vact_enfrentamiento  ~ "enfrentamiento",
                     actividad_grupo1 %in% vact_noviolenta      ~ "presencia no violenta",
                     actividad_grupo1 %in% vact_amenaza         ~ "amenazas",
                     actividad_grupo1 %in% vact_secuestro       ~ "privación de la libertad",
                     actividad_grupo1 %in% vact_psinespecificar ~ "presencia sin especificar",
                     actividad_grupo1 %in% vact_labycamp        ~ "laboratorios/campamento/reclutamiento",
                     actividad_grupo1 %in% vact_fseguridad      ~ "intervenciones gubernamentales",
                     actividad_grupo1 %in% vact_coru_rob        ~ "corrupción/robo",
                     actividad_grupo1 %in% vact_sustancias      ~ "sustancias ilegales",
                     actividad_grupo1 %in% vact_armas           ~ "tráfico de armas",
                     actividad_grupo1 ==   actividad_grupo1     ~ actividad_grupo1), 
         actividad_grupo2 = 
           case_when(actividad_grupo2 %in% vact_agresion        ~ "agresión",
                     actividad_grupo2 %in% vact_extorison       ~ "vigilancia/extorsión",
                     actividad_grupo2 %in% vact_enfrentamiento  ~ "enfrentamiento",
                     actividad_grupo2 %in% vact_noviolenta      ~ "presencia no violenta",
                     actividad_grupo2 %in% vact_amenaza         ~ "amenazas",
                     actividad_grupo2 %in% vact_secuestro       ~ "privación de la libertad",
                     actividad_grupo2 %in% vact_psinespecificar ~ "presencia sin especificar",
                     actividad_grupo2 %in% vact_labycamp        ~ "laboratorios/campamento/reclutamiento",
                     actividad_grupo2 %in% vact_fseguridad      ~ "intervenciones gubernamentales",
                     actividad_grupo2 %in% vact_coru_rob        ~ "corrupción/robo",
                     actividad_grupo2 %in% vact_sustancias      ~ "sustancias ilegales",
                     actividad_grupo2 %in% vact_armas           ~ "tráfico de armas",
                     actividad_grupo2 ==   actividad_grupo2     ~ actividad_grupo2)) 

unique(df_act$actividad_grupo1)
unique(df_act$actividad_grupo2)

# 4. ID y orden 
df_act <- df_act %>% 
 mutate(id = 1:length(df_act$Mes))

m_2023 <- df_act %>%
  select("id"                     = "id",
         "fecha_de_publicacion"   = "fecha_de_publicacion",
         "mes"                    = "Mes", 
         "dia"                    = "Dia",
         "anio"                   = "Anio", 
         "fecha_hechos"           = "fecha_hechos",
         "enlace"                 = "Enlace",
         "titulo_de_la_nota"      = "titulo_de_la_nota",
         "nombre_de_la_fuente"    = "nombre_de_la_fuente",
         "enlace_otras_notas"     = "enlace_otras_notas",
         "estado"                 = "Estado",
         "municipio"              = "Municipio",
         "CVEGEO_ent"             = "CVEGEO_ent",
         "CVEGEO_mun"             = "CVEGEO_mun",
         "CVEGEO"                 = "CVEGEO",
         "lugar"                  = "Lugar",
         "grupo1"                 = "grupo1",
         "grupo2"                 = "grupo2",
         "alianza1"               = "alianza1",
         "alianza2"               = "alianza2",
         "alianza3"               = "alianza3",
         "alianza4"               = "alianza4",
         "alianza5"               = "alianza5",
         "rival1"                 = "rival1", 
         "rival2"                 = "rival2",
         "actividad_grupo1"       = "actividad_grupo1",
         "actividad_grupo2"       = "actividad_grupo2",
         "narcomensaje"           = "Narcomensaje",
         "contenido_narcomensaje" = "contenido_narcomensaje",
         "homic_total"            = "homic_total",
         "homic_hombre"           = "homic_hombre",
         "homic_mujer"            = "homic_mujer",
         "homic_clasif1"          = "homic_clasif1",
         "homic_clasif2"          = "homic_clasif2",
         "cuerpos_localizados"    = "cuerpos_localizados",
         "cuerpo_modo1"           = "cuerpo_modo1",
         "cuerpo_modo2"           = "cuerpo_modo2",
         "cuerpo_lugar1"          = "cuerpo_lugar1",
         "cuerpo_lugar2"          = "cuerpo_lugar2",
         "heridos_total"          = "heridos_total",
         "heridos_hombre"         = "heridos_hombre",
         "heridos_mujeres"        = "heridos_mujeres",
         "herido_clasif1"         = "herido_clasif1",
         "herido_clasif2"         = "herido_clasif2",
         "ataque1"                = "ataque1",
         "ataque2"                = "ataque2",
         "ataque3"                = "ataque3",
         "lugar_ataque1"          = "lugar_ataque1",
         "lugar_ataque2"          = "lugar_ataque2",
         "politica_de_seguridad"  = "politica_de_seguridad")

# Detalles
# Reemplazar mayusculas
m_2023 <- m_2023 %>% 
  mutate_at(.vars = c("nombre_de_la_fuente", 
                      "actividad_grupo1", "actividad_grupo2",
                      "contenido_narcomensaje", "homic_clasif1",
                      "homic_clasif2", "cuerpo_modo1", "cuerpo_modo2",
                      "cuerpo_lugar1", "cuerpo_lugar2", "herido_clasif1",
                      "herido_clasif2", "ataque1", "ataque2", "ataque3",
                      "lugar_ataque1", "lugar_ataque2"), 
            .funs = ~str_to_lower(.))


# 4. Base limpía----------------------------------------------------------------
openxlsx::write.xlsx(m_2023, 
                     file = paste_out("m_ene_mar_23_lim.xlsx"))


# Fin--------------------------------------------------------------------------- 
beepr::beep(9)





  

