#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Limpieza M-2023
#
# Encargado:                  Alejandro Pocoroba y Erick Isaac Morales Sánchez
# Correo:                     alejandro.pocoroba@cide.edu
#                             erick.morales@cide.edu                                
# Fecha de creación:          01 de mayo de 2023
# Última actualización:       02 de mayo de 2023
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
m1 <- read_xlsx(paste_inp("m_ene_mar_23_s.xlsx"))

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
df_ <- separate(df_, "x1_3_2_estado", c("estado", "clave"), sep = "-")
df_ <- separate(df_, "x1_3_3_municipio", c("municipio", "claveEdo", "clavemun"), sep = "-")
df_ <- subset(df_, select = -c(clave))
df_$clavefinal <- paste0(df_$claveEdo, df_$clavemun) 

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
         "Estado"                 = "estado",
         "Municipio"              = "municipio",
         "CVEGEO_ent"             = "claveEdo",
         "CVEGEO_mun"             = "clavemun",
         "CVEGEO"                 = "clavefinal",
         "Lugar"                  = "x1_3_4_lugar",
         "grupo_criminal"         = "x3_3_grupo_criminal",
         "Alianza"                = "x3_3_1_alianza_grupo",
         "Rival"                  = "x3_3_2_rival_grupo",
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
### 3.1 Cambiar FALSE/TRUE por 0 y 1 en Narcomensajes, Cuerpos y Poli de Seg

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

### 3.2. Grupos criminales------------------------------------------------------
#base inicia df_1 y termina con df_gc

### 3.3. Persona (homicidios + heridxs)-----------------------------------------
#base inicia df_gc y termina con df_p

### 3.4. Cuerpos----------------------------------------------------------------
#base inicia df_p y termina con df_c

df_p <- df_1 #por el momento

# separar valores con más opciones
df_semicolon <- df_p %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    cuerpos_modo1 = str_replace_all(cuerpos_modo1, ":", ";"), 
    # Contar número de puntos y comas (;)
    semicolon = str_count(cuerpos_modo1, ";")) %>% 
  select(semicolon) 

max(df_semicolon$semicolon, na.rm = T) # El máximo es 2

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


### 3.5. Ataque-----------------------------------------------------------------
#base inicia df_c y termina con df_ata


### 3.6. Lugar de ataque--------------------------------------------------------
#base inicia df_ata y termina con df_lu

df_lu <- df_c # por el momento

# separar valores con más opciones
df_semicolon2 <- df_lu %>% 
  mutate(
    # Reemplazar casos donde haya dos puntos (:) por punto y coma (;)
    lugar_ataque_clean = str_replace_all(lugar_ataque_clean, ":", ";"), 
    # Contar número de puntos y comas (;)
    lugar_ataque_clean = str_count(lugar_ataque_clean, ";")) %>% 
  select(lugar_ataque_clean) 

max(df_semicolon2$lugar_ataque_clean, na.rm = T) # El máximo es 1

df_lu <- df_lu %>% 
  mutate(lugar_ataque_clean = str_replace_all(lugar_ataque_clean, ":", ";")) %>% 
  separate(lugar_ataque_clean, sep = ";", c("lugar_ataque1", 
                                            "lugar_ataque2"))

unique(df_lu$lugar_ataque1)
unique(df_lu$lugar_ataque2)

# clasificación - vector 
v_adomicilio <- c("afuera de domicilio")

v_aestablecimiento <- c("afuera de escuela", "afuera de negocio", 
                        "afuera de oficina", "afuera de un panteón", 
                        "afuera de bar", "afuera de mercado", "afuera de velorio",
                        "transmisores de cámaras de seguridad")

v_baldio <- c("basurero", "Calle", "campo", "canal de aguas negras", "Ejido",
              "finca", "Lote baldío", "montaña", "obra negra", "paraje", 
              "pozo", "rancho", "río", "terreno", "terreo baldío", "zona boscosa")

v_banco <- c("banco", "cajero")

v_barbe <- c("barbería")

v_calle <- c("calle", "camino", "camino de terracería", " calle", 
             "17 calle Poniente y Venustiano Carranza de la colonia 1 de Mayo, a unos 800 metros de la base cangrejo de la Secretaría de Seguridad Pública Municipal.")

v_campo <- c("campo de fútbol", "canchas de fútbol", "Unidad deportiva")

v_carretera <- c("carretera", "vías del tren")

v_iglesia <- c("iglesia")

v_domicilio <- c("casa", "Domicilio", "jacal", "vivienda", "vivienda de comerciante", "obra", "cuartería")

v_estacionamiento <- c("estacionamiento", "estacionamiento de bar", "estacionamiento supermercado")

v_fiesta <- c("fiesta", "salón de fiestas")

v_gas <- c("gasolinera", " gasera", "gasolineria")

v_hosp <- c("hospital", "clínica")

v_gub <- c("Centro penitenciario", "cereso", "escuela", "estación de bomberos",
           "estación de tren", "institución pública", "ministerio público", "oficina de gobierno", "comandancia",
           "retén", "afuera de campo militar", "afuera de cereso")

v_local <- c("centro comercial", "comercio", "comercio", "gasera", "comercio", "purificadora de agua", 
             "local comercial", "sitio de taxis", "Taller de Herrería", "trituradora", "verificentro", " purificadora de agua")

v_nego <- c("base de taxis", "bodega","cabañas", "carpintería", "expendio", "funeraria", 
            "hotel", "ladrilleras", "motel", "Negocio", "negocio", "laboratorio", "recicladora", 
            " laboratorio", "farmacia", "plaza de ganado", "pizzería", "corral",
            "Billar", "forrajera", "caballerizas")

v_parque <- c("área verde", "huerta", "jardín", "Parque", "a orilla de un río")    

v_comida <-c("cafetería", "restaurante", "taquería", "cenaduría")

v_entretenimiento <- c("carrera de caballos", "casino", "local de maquinitas", "negocio de maquinas tragamodenas", 
                       "negocio de maquinitas", "palenque", "playa", "Rodeo", "jaripeo", "plaza principal", "plaza",
                       "plaza comercial")

v_meca <- c("autolavado", "refaccionaria", "taller mecánico", "taller de motos", "taller mecánico", " refaccionaria")

v_taxi <- c("Central camionera", "transporte público", " taxi", " taller mecánico")

v_abarrotes <- c("carnicería", "mercado", "negocio de comida", "negocio: fruteria", " fruteria", "oxxo", "puesto ambulante",
                 "tianguis", "tienda", "tortillería")

v_vehiculo <- c("tráiler", "vehículo", "vehiculo", " vehículo", " motocicleta", " autobús", "auto", "agresión a vehículo")

v_alcohol <- c("bar", "bar/pulquería", "Cantina", "comercio", "depósito de cerveza", 
               "depósito de cerveza", "vinateria", " depósito de cerveza")

v_consumo <- c("albergue", "anexo", "campo de cultivo", 
               "centro de rehabilitación", "punto de venta de droga", " picadero",
               "picadero", "centro rehabilitación")

# Limpiar nombres 
df_lu <- df_lu %>% 
  mutate(lugar_ataque1 = 
           case_when(lugar_ataque1 %in% v_adomicilio        ~ "afuera de domicilio",
                     lugar_ataque1 %in% v_aestablecimiento  ~ "afuera de establecimiento",
                     lugar_ataque1 %in% v_baldio            ~ "baldío/maleza/terreno",
                     lugar_ataque1 %in% v_banco             ~ "banco",
                     lugar_ataque1 %in% v_barbe             ~ "barbería/salón de belleza/estética",
                     lugar_ataque1 %in% v_calle             ~ "calle",
                     lugar_ataque1 %in% v_campo             ~ "campo deportivo",
                     lugar_ataque1 %in% v_carretera         ~ "carretera",
                     lugar_ataque1 %in% v_iglesia           ~ "comunidad/religión",
                     lugar_ataque1 %in% v_domicilio         ~ "domicilio",
                     lugar_ataque1 %in% v_estacionamiento   ~ "estacionamiento",
                     lugar_ataque1 %in% v_fiesta            ~ "fiesta/salón de fiestas",
                     lugar_ataque1 %in% v_gas               ~ "gasolinera",
                     lugar_ataque1 %in% v_hosp              ~ "hospital/consultorio",
                     lugar_ataque1 %in% v_gub               ~ "instancia gubernamental",
                     lugar_ataque1 %in% v_local             ~ "local comercial",
                     lugar_ataque1 %in% v_nego              ~ "negocio",
                     lugar_ataque1 %in% v_parque            ~ "parque/jardín/espacio público",
                     lugar_ataque1 %in% v_comida            ~ "puesto de comida/restaurante/taquería",
                     lugar_ataque1 %in% v_entretenimiento   ~ "sitio de entretenimiento",
                     lugar_ataque1 %in% v_meca              ~ "taller mecánico/vulcanizadora/lavacoches",
                     lugar_ataque1 %in% v_taxi              ~ "taxi/transporte público",
                     lugar_ataque1 %in% v_abarrotes         ~ "tienda de abarrotes/mercado/venta de comida",
                     lugar_ataque1 %in% v_vehiculo          ~ "vehículo",
                     lugar_ataque1 %in% v_alcohol           ~ "venta de alcohol",
                     lugar_ataque1 %in% v_consumo           ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     lugar_ataque1 ==   lugar_ataque1       ~ lugar_ataque1), 
         lugar_ataque2 = 
           case_when(lugar_ataque2 %in% v_adomicilio        ~ "afuera de domicilio",
                     lugar_ataque2 %in% v_aestablecimiento  ~ "afuera de establecimiento",
                     lugar_ataque2 %in% v_baldio            ~ "baldío/maleza/terreno",
                     lugar_ataque2 %in% v_banco             ~ "banco",
                     lugar_ataque2 %in% v_barbe             ~ "barbería/salón de belleza/estética",
                     lugar_ataque2 %in% v_calle             ~ "calle",
                     lugar_ataque2 %in% v_campo             ~ "campo deportivo",
                     lugar_ataque2 %in% v_carretera         ~ "carretera",
                     lugar_ataque2 %in% v_iglesia           ~ "comunidad/religión",
                     lugar_ataque2 %in% v_domicilio         ~ "domicilio",
                     lugar_ataque2 %in% v_estacionamiento   ~ "estacionamiento",
                     lugar_ataque2 %in% v_fiesta            ~ "fiesta/salón de fiestas",
                     lugar_ataque2 %in% v_gas               ~ "gasolinera",
                     lugar_ataque2 %in% v_hosp              ~ "hospital/consultorio",
                     lugar_ataque2 %in% v_gub               ~ "instancia gubernamental",
                     lugar_ataque2 %in% v_local             ~ "local comercial",
                     lugar_ataque2 %in% v_nego              ~ "negocio",
                     lugar_ataque2 %in% v_parque            ~ "parque/jardín/espacio público",
                     lugar_ataque2 %in% v_comida            ~ "puesto de comida/restaurante/taquería",
                     lugar_ataque2 %in% v_entretenimiento   ~ "sitio de entretenimiento",
                     lugar_ataque2 %in% v_meca              ~ "taller mecánico/vulcanizadora/lavacoches",
                     lugar_ataque2 %in% v_taxi              ~ "taxi/transporte público",
                     lugar_ataque2 %in% v_abarrotes         ~ "tienda de abarrotes/mercado/venta de comida",
                     lugar_ataque2 %in% v_vehiculo          ~ "vehículo",
                     lugar_ataque2 %in% v_alcohol           ~ "venta de alcohol",
                     lugar_ataque2 %in% v_consumo           ~ "venta/distribución/consumo sustancias/centro de rehabilitación",
                     lugar_ataque2 ==   lugar_ataque2       ~ lugar_ataque2))

unique(df_lu$lugar_ataque1)
unique(df_lu$lugar_ataque2)





  

