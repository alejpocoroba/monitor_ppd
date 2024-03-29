#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD 2023
# Objetivo:                   Reporte de desempeño: captura de notas
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          01 de agosto de 2023
# Última actualización:       01 de agosto de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD versión abril - junio 2023

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Librerías 
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, zoo, ggtext, beepr, lubridate, stringr, janitor, stringi)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("02_datos_crudos/" , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}


# 1. Cargar datos --------------------------------------------------------------
c <- read_xlsx(paste_inp("monitor_abril_2023.xlsx"))
b <- read_xlsx(paste_inp("monitor_junio_2023.xlsx"))
d <- read_xlsx(paste_inp("monitor_marzo_2023.xlsx"))

# Se unen bases 
a <- c %>% 
  bind_rows(b) %>% 
  bind_rows(d)

# 2. Procesamiento -------------------------------------------------------------

# Limpieza de nombres y variables de interés
crudo <- a %>% 
  janitor::clean_names() %>% 
  select(estado:observaciones)

# Tratamiento de fechas #
crudo$"fecha" <- as.Date(crudo$"fecha")
crudo$dia     <- format(crudo$"fecha", "%d")
crudo$mes     <- format(crudo$"fecha", "%m")
crudo$anio    <- format(crudo$"fecha", "%Y")

# Estado según su clave de Inegi 
crudo <- crudo %>% 
  mutate(
    claveEdo = str_sub(estado, -2, -1))

### 2.1 Limpieza de texto-------------------------------------------------------
# Limpiar T/F por 1/0
crudo$violencia_armada_fuego <- as.character(crudo$violencia_armada_fuego)
crudo$homicidios <- as.character(crudo$homicidios)
crudo$heridxs <- as.character(crudo$heridxs)
crudo$genero <- as.character(crudo$genero)
crudo$presencia_criminal <- as.character(crudo$presencia_criminal)
crudo$actividades_delictivas <- as.character(crudo$actividades_delictivas)
crudo$detenciones <- as.character(crudo$detenciones)
crudo$acciones_gubernamentales <- as.character(crudo$acciones_gubernamentales)
crudo$politica_seguridad <- as.character(crudo$politica_seguridad)

limpiar_tf <- function(x){
  
  case_when(
    x == "FALSE" ~ "0",
    x == "TRUE"  ~ "1",
    x == x ~ x
  )
}

crudo <- crudo %>% 
  mutate_at(.vars = c("violencia_armada_fuego", "homicidios", "heridxs",
                      "genero", "presencia_criminal", "actividades_delictivas",
                      "detenciones", "acciones_gubernamentales", "politica_seguridad"),
            .funs = ~limpiar_tf(.))

# Minúsculas
crudo <- crudo %>% 
  mutate_at(.vars = c("otra", "observaciones"), 
            .funs = ~str_to_lower(.))

# Acentos
crudo$otra          <- stri_trans_general(crudo$otra, "Latin-ASCII")
crudo$observaciones <- stri_trans_general(crudo$observaciones, "Latin-ASCII")

# Limpieza variable "otra" 
# separar valores 
df_crudo <- crudo %>%
  mutate(otra = str_replace_all(otra, "/", ";"),
         otra = str_replace_all(otra, ":", ";")) %>% 
  separate(otra, sep = ";", c("otra1", "otra2"))

unique(df_crudo$otra1)
unique(df_crudo$otra2)
unique(df_crudo$observaciones)

# Grupos sociales 
v_menoredad        <- c("menor lesionadx", "menor de edad implicado",
                        "menor de edad implicadx", "violencia a infantes",
                        "menores implicadxs", "menor", "menor de edad",
                        "menores de edad involucradxs", " menores",
                        "menores", " menores involucradxs", " menor de edad implicado",
                        " menores heridxs", " menor de edad", " menores de edad",
                        "menor de edad", "menor de edad herido",
                        "menor de edad heridio con arma blanca",
                        "menor de edad detenido", "menor de edad implicado\r\n",
                        "menor de edad implicado")

v_taxista         <- c("taxistas \r\n", "taxista", "taxistas\r\n",
                       "taxista \r\n", "taxista\r\n" )


v_autodefensas    <- c("autodefensas")

# Autoridades 
v_detautoridad     <- c("detencion de autoridad", "detencion de policias",
                        "detencion a autoridad", "detencion autoridad", "detencion a policias")

v_abusodeautoridad <- c("tortura policiaca", "tortura por parte de autoridad",
                        "extorsion de autoridad", "abuso en contra de migrantes",
                        "abuso policial", "tortura policiaca", "abuso de autoridad",
                        "autoridad", "abuso de autoridad")

v_agresionauto     <- c("agresion a autoridad", "agresion a  autoridad", "neutralizacion policiaca",
                        "asesinato de autoridad", "asesinato autoridad", "recuento de policias asesinados",
                        "policias", "dos agentes de la guardia nacional resultaron heridos de gravedad luego de que fueron atacados a balazos",
                        "policia", "agresion a autoridad\r\n" )

v_actiauto         <- c("erradicacion", "allaneamiento", "capacitacion ddhh", "capacitacion",
                        "capacitacion a autoridad", "platicas de prevencion",
                        "agresion a autoridad ", "actividad semar",
                        "comercio ilicito")

# Actividad delictiva
v_persecucion      <- c("persecucion")

v_desparecidxs     <- c("desaparecidxs", "cuerpo de desaparecidxs", "cuerpo desaparecidxs",
                        "desapariciones", "desaparecido", "menor de edad desaparecido",
                        "menores desaparecidxs", "desaparecidos", "desaparecidx",
                        "cuerpo de desaparrecidx", "posible cuerpo de desaparecidx",
                        "desaparicion forzada", "cuerpo de desaparecidx",
                        "colectivos de busqueda", "desaparecidxs\r\n",
                        "cuerpo de desaparecidx\r\n")

v_secuestro        <- c("levanton", "victima fue levantado", "secuestro", " secuestro",
                        "secuestro de ninxs", "secuestro de ninos", 
                        "robo y secuestro", "secuestro reportero", "secuestro de ninxs",
                        "secuestro", "victima fue levantado", "sustraccion de menor",
                        "sustraccion de menores", "secuestro virtual",
                        " secuestro", "levanton\r\n")

v_fosas            <- c("fosas clandestinas", "fosa clandestina", "fosa")

v_incendio         <- c("quema de auto", "incendio domicilio", 
                        "incendio vehiculos", "incendio",
                        "quema de negocio", "explposion", "incendio vivienda",
                        "quemaduras", "incendio de negocio", "quema de auto",
                        "explosion", "bombas caseras", "agresion con explosivos",
                        " incendio vehiculo", "agresion con explosivos", 
                        " incendio vehiculo", "agresion con explosivos",
                        "ataque con granadas", "detonacion de artefacto explosivo.",
                        "amenaza de bomba")

v_robo             <- c(" robo", "robo", "asalto", "atraco", "vehiculo robado",
                        "robo", "robo", "robo")

v_corrupcion       <- c("corrupcion")

v_fraude           <- c("patrullas falsas", "fraude cibernetico", "estafa", "fraude",
                        "billetes falsos")

v_extorsion        <- c(" cobro de piso", "cobro de piso", "extorsion",
                        "prestamistas gota a gota")

v_sustancias       <- c("medicamento controlado", "politica de drogas", "fentanilo",
                        "sustancias", "contrabando de cigarros", "bebe intoxicado fentanilo")

v_traflorfau       <- c("madera ilegal", "tala clandestina", "madera clandestina",
                        "madera ilegal", "tala clandestina" )

v_huachicol        <- c("huachicoleo", "huachicol", "control de pozos petroleros",
                        "huachicoleo")

v_personas         <- c("trafico de migrantes", "trafico de personas",
                        "traficantes de humanos")

v_narcoactividades <- c("narcobloqueo", "narcobloqueos", 
                        "narcolaboratorio", "campamento", 
                        "narcocampamento", "narco-polleria")

v_narcomensaje    <- c("narcomensajes", "narcomensaje", "mensaje mediante grabacion",
                       "mensaje", "narcomensaje", "mensaje",
                       "narco mensaje \"limpieza social\"", "narcomanta",
                       "narco manta \r\n", "arias cartulinas con mensajes amenazantes.", 
                       "narcomensaje\r\n", "narcomensaje \r\n",
                       "narcomanta \r\n", "cuerpo  con mensaje", "mensaje",
                       "mensaje mediante grabacion", "narco mensaje")

v_amaneza          <- c("amenazas")

# Violencia familiar y género 
v_genero           <- c("intento de violacion", "posible feminicidio",
                        "violacion", "presunto feminicidio", "violencia familiar",
                        "violencia de genero", "violencia de genero0",
                        " violencia genero", "familiar", "genero",
                        "   violencia familiar", "violencia familiar/ violencia genero\r\n",
                        "posible feminicidio", "violencia familiar", "violencia familiar\r\n",
                        "violencia familiar/ violencia genero")

# Agresiones contra prensa
v_periodistas      <- c("agresion a reporteros", "acciones libertad de prensa")

# Agresión en general 
v_agresion         <- c("agresion a vehiculo", "agresion", "agresion a vivienda",
                        "ataque a vehiculo", "disparos vivienda", "disparos negocios",
                        " agresion a vehiculo", "disparos negocios",
                        "disparos negocios", "disparos vivienda\r\n")

# Lugares
v_centro           <- c("centro de rehabilitacion")

v_penitenciario    <- c("situacion en sistema penitenciario", "motin centro penitenciario",
                        "cuerpo detenido", "muerto detenido")

nombres_otra <- function(x) {
 
   case_when(
    x %in% v_menoredad        ~  "menor de edad",
    x %in% v_autodefensas     ~  "autodefensas",
    x %in% v_detautoridad     ~  "detencion de autoridades",
    x %in% v_abusodeautoridad ~  "abuso de autoridad",
    x %in% v_agresionauto     ~  "agresion contra autoridades",
    x %in% v_actiauto         ~  "actividad de las autoridades",
    x %in% v_persecucion      ~  "persecucion",
    x %in% v_desparecidxs     ~  "desaparecidos/as",
    x %in% v_secuestro        ~  "secuestro",
    x %in% v_fosas            ~  "fosas clandestinas",
    x %in% v_incendio         ~  "incendio/explosivos",
    x %in% v_robo             ~  "robo/asaltos",
    x %in% v_corrupcion       ~  "corrupcion", 
    x %in% v_fraude           ~  "fraude",
    x %in% v_extorsion        ~  "extorsion",
    x %in% v_sustancias       ~  "sustancias",
    x %in% v_traflorfau       ~  "trafico de flora/fauna",
    x %in% v_huachicol        ~  "huachicol",
    x %in% v_personas         ~  "trafico de personas",
    x %in% v_narcoactividades ~  "narcoactividades",
    x %in% v_amaneza          ~  "amanezas",
    x %in% v_genero           ~  "violencia familiar/genero",
    x %in% v_periodistas      ~  "agresion contra la prensa",
    x %in% v_agresion         ~  "agresiones",
    x %in% v_centro           ~  "centro de rehabilitacion",
    x %in% v_penitenciario    ~  "centro penitenciario",
    x %in% v_narcomensaje     ~  "narcomensaje",
    x == x ~ x
  )
}

df_crudo_li <- df_crudo %>% 
  mutate_at(.vars = c("otra1", "otra2", "observaciones"),
            .funs = ~nombres_otra(.))

# Valores similares en columnas distintas en la misma observacion
df_crudo_li2 <- df_crudo_li %>% 
  mutate(otra2 = if_else(otra2 == otra1, NA_character_, otra2))

# Detalles
# ID y orden
df_limpio <- df_crudo_li2 %>% 
  mutate(id = 1:length(df_crudo_li$mes)) %>% 
  select(id, estado, claveEdo, fecha,  mes, anio, titulo_nota, enlace, enlace_relacionado,
        violencia_armada_fuego, homicidios, heridxs, genero, presencia_criminal,
         actividades_delictivas, detenciones, acciones_gubernamentales, politica_seguridad,
         otra1, otra2) %>% 
  rename("heridos_as" = "heridxs") %>% 
  filter(mes != "07")
# se deja fuera observaciones por indicaciones de LA 08/08/23

# 3. Base limpia----------------------------------------------------------------
# Microdatos:
# openxlsx::write.xlsx(df_limpio, 
#                     file = paste_out("monitor.abril_junio2023.xlsx"))

# 4. Base panel
df_limpio$violencia_armada_fuego   <- as.numeric(df_limpio$violencia_armada_fuego)
df_limpio$homicidios               <- as.numeric(df_limpio$homicidios)
df_limpio$heridos_as               <- as.numeric(df_limpio$heridos_as)
df_limpio$genero                   <- as.numeric(df_limpio$genero)
df_limpio$presencia_criminal       <- as.numeric(df_limpio$presencia_criminal)
df_limpio$actividades_delictivas   <- as.numeric(df_limpio$actividades_delictivas)
df_limpio$detenciones              <- as.numeric(df_limpio$detenciones)
df_limpio$acciones_gubernamentales <- as.numeric(df_limpio$acciones_gubernamentales)
df_limpio$politica_seguridad       <- as.numeric(df_limpio$politica_seguridad)

# por mes 
df_dp_m <- df_limpio %>% 
  group_by(mes, estado, claveEdo) %>% 
  summarise(violencia_armada_fuego   = sum(violencia_armada_fuego, na.rm = TRUE),
            homicidios               = sum(homicidios, na.rm = TRUE),
            heridos_as               = sum(heridos_as, na.rm = TRUE),
            genero                   = sum(genero, na.rm = TRUE),
            presencia_criminal       = sum(presencia_criminal, na.rm = TRUE),
            actividades_delictivas   = sum(actividades_delictivas, na.rm = TRUE),
            detenciones              = sum(detenciones, na.rm = TRUE),
            acciones_gubernamentales = sum(acciones_gubernamentales, na.rm = TRUE),
            politica_seguridad       = sum(politica_seguridad, na.rm = TRUE)) 

# de largo a ancho
df_dp_m2 <- df_dp_m %>%
  pivot_longer(cols = -c(mes,estado, claveEdo), names_to = "tipo", values_to = "total") %>% 
  pivot_wider(names_from = c(tipo, mes), values_from = total) 

# 5. Base limpia----------------------------------------------------------------
# openxlsx::write.xlsx(df_dp_m2, 
#                     file = paste_out("ppd.monitor.abril_junio2023.xlsx"))