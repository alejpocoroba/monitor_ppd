#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Detenciones y sustancias
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          14 de diciembre de 2022
# Última actualización:       12 de enero de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD: junio a noviembre 2022

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, ggrepel, beepr)

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp1 <- function(x){paste0("03_datos_limpios/", x)}
paste_inp2 <- function(x){paste0("02_datos_crudos/" , x)}
paste_fig  <- function(x){paste0("04_figuras/"      , x)}
paste_out  <- function(x){paste0("03_datos_limpios/", x)}


# 1. Cargar datos---------------------------------------------------------------
load(paste_inp1("df_monitor_amplio.Rdata")) # junio a octubre 
df_nov <- read_xlsx(paste_inp2("Monitor_PPD_noviembre.xlsx")) # noviembre

# 2. Procesamiento--------------------------------------------------------------
# Junio - octubre
unique(df_monitor_amplio$trafico)

v_sustancias         <- c("Cocaina", "sustancias", "cocaina" )
v_sustancias_delitos <- c("armas; sustancias", "sustancias; armas; ropa táctica",
                          "sustancias; armas", "sustancias ; armas ; ropa táctica",
                          "sustancias; armas; placas policiales apócrifas",
                          "sustancias; animales", "sustancias; personas",
                          "sustancias; armas; uniformes", "armas; drogas",
                          "sustancias; vehículos", "armas; vehículos; sustancias",
                          "armas: sustancias")


df_detenciones <- df_monitor_amplio %>% 
  select(id, 
         publicacion                  = fecha_de_publicacion,
         enlace,
         titulo                       = titulo_de_la_nota,
         fecha_hechos                 = fecha_de_los_hechos,
         estado,
         municipio,
         lugar, 
         detenidos_t                  = numero_de_detenidos_as_total,
         detenidos_h                  = numero_de_detenidos_hombres,
         detenidos_m                  = numero_de_heridas_mujeres,
         detenido_pertenece           = detenido_a_pertenece_a,
         trafico, 
         otra_actividad               = otras_actividades_ilicitas,
         ataque                       = ataque_armado,
         secuestro                    = privacion_de_la_libertad,
         narcomensaje, 
         militar                      = autoridad_militar,
         militar_actividad            = tipo_de_actividad_militar,
         civil                        = autoridad_civil,
         civil_actividad              = tipo_de_actividad_civil,
         politica_seguridad           = politica_de_seguridad,
         grupo_criminal               = nombre_del_grupo_criminal_gc,
         homicidios_t                 = numero_de_homicidios_total,
         heridos_t                    = numero_de_heridos_as_total) %>% 
  mutate(trafico = 
           case_when(trafico %in% v_sustancias ~ "sustancias",
                     trafico %in% v_sustancias_delitos ~ "sustancias_delitos",
                     trafico == trafico ~ trafico)) %>% 
  filter(trafico %in% c("sustancias", "sustancias_delitos"))

# Guardar base
openxlsx::write.xlsx(df_detenciones, file = paste_out("detenciones6_10.xlsx"), overwrite = T)

# Noviembre 
df_detenciones2 <- df_nov %>% 
  janitor::clean_names() %>% 
  select(id, 
         publicacion            = x1_2_1_fecha_de_publicacion,
         enlace                 = x1_2_2_enlace,
         titulo                 = x1_2_3_titulo_de_la_nota,
         fecha_hechos           = x1_3_1_fecha_de_los_hechos,
         estado                 = x1_3_3_estado,
         municipio              = x1_3_4_municipio, 
         lugar                  = x1_3_5_lugar, 
         detenidos_t            = x3_1_1_numero_de_detenidos_as_total,
         detenidos_h            = x3_1_2_numero_de_detenidos_hombres,
         detenidos_m            = x2_3_3_numero_de_heridas_mujeres,
         detenido_pertenece     = x3_1_4_detenido_a_pertenece_a_ocupacion,
         actividad_delictiva    = x4_1_1_actividades_delictivas, 
         institucion_detiene    = x3_1_5_institucion_que_detiene,
         ataque                 = x2_1_1_ataque, 
         secuestro              = x4_1_2_privacion_de_la_libertad,
         narcomensaje           = x5_1_1_narcomensaje,  
         contenido_narcomensaje = x5_1_2_contenido_del_narcomensaje,
         autoridad              = x6_1_autoridad,
         autoridad_actividad    = x6_2_tipo_de_actividad_de_la_autoridad,
         politica_seguridad     = x7_1_politica_de_seguridad, 
         grupo_criminal         = x5_1_3_nombre_del_grupo_criminal_gc,
         alianza_grupo          = x5_1_4_alianza_del_gc, 
         rival_grupo            = x5_1_5_rival_del_gc,
         homicidios_t           = x2_2_1_numero_de_homicidios_total,
         homicidios_h           = x2_2_2_numero_de_homicidios_hombre,
         homicidios_m           = x2_2_3_numero_de_homicidios_mujer,
         homicidio_pertenece    = x2_2_4_pertenece_a_ocupacion,
         cuerpo_localizado      = x2_2_6_cuerpo_localizado_restos_humanos,
         heridos_t              = x2_3_1_numero_de_heridos_as_total,
         heridos_h              = x2_3_2_numero_de_heridos_hombres,
         heridos_m              = x2_3_3_numero_de_heridas_mujeres,
         herido_pertenece       = x2_3_4_herido_a_pertenece_a_ocupacion)

unique(df_detenciones2$actividad_delictiva)

v_sustancias11         <- c("sustancias ilegales", "tráfico de sustancias",
                            "posesión de sustancias")
v_sustancias_delitos11 <- c("sustancias ilegales; portación de armas", 
                            "tráfico de armas; tráfico de sustancias",
                            "huachicol; sustancias ilícitas", 
                            "trafico de sustancias; portación de armas",
                            "robo; tráfico de sustancias", 
                            "portación de armas; secuestro; sustancias ilegales",
                            "sustancias ilegales; billetes falsos", 
                            "enfrentamiento; portación de armas; sustancias ilegales",
                            "armas. sustancias ilegales", "portación de armas; sustancias ilegales",
                            "sustancias ilegales; préstamos fraudulentos gota a gota")

df_detenciones2 <- df_detenciones2 %>% 
  mutate(actividad_delictiva = 
         case_when(actividad_delictiva %in% v_sustancias11 ~ "sustancias",
                   actividad_delictiva %in% v_sustancias_delitos11 ~ "sustancias_delitos",
                   actividad_delictiva == actividad_delictiva ~ actividad_delictiva)) %>% 
  filter(actividad_delictiva %in% c("sustancias", "sustancias_delitos"))

# Guardar base
openxlsx::write.xlsx(df_detenciones2, file = paste_out("detenciones11.xlsx"), overwrite = T)
















