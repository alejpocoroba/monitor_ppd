#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Ciudad Juárez
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          08 de noviembre de 2022
# Última actualización:       11 de febrero de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

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
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}
paste_inp2 <- function(x){paste0("02_datos_crudos/" , x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("monitor_jun_oct.Rdata")) # junio - octubre

nov <- read_xlsx(paste_inp2("Monitor_PPD_noviembre.xlsx")) # noviembre
dic <- read_xlsx(paste_inp2("Monitor_PPD_diciembre10.02.xlsx")) # diciembre

# 2. Limpieza de los datos------------------------------------------------------
## Datos de Chihuahua-----------------------------------------------------------

#limpieza junio - octubre
df_chi1 <- monitor_jun_oct %>% 
  filter(estado == "Chihuahua-08")

# Limpieza en el nombre de Juárez
v_juarez <- c("Juárez-07-048", "Juárez-16-046")
V_arma   <- c("agresión", "arma de fuego", "arma de fuego; golpes")

df_chi2 <- df_chi1 %>% 
  mutate(municipio = case_when(municipio %in% v_juarez  ~ "Juárez-08-037",
                               municipio == municipio ~ municipio)) 

## Datos de Ciudad Juárez-----
df_juarez1 <- df_chi2 %>% 
  filter(municipio == "Juárez-08-037") %>% 
  filter(fecha_de_publicacion > "2022-06-01") %>% 
  filter(fecha_de_publicacion < "2022-10-31") %>% 
  # variables de interés
  select(-c("datos_generales":"fecha_de_publicacion", 
            "nombre_de_la_fuente":"hechos",
            "pais":"municipio",
            "numero_de_detenidos_as_total":"detenido_a_pertenece_a",
            "grupos_criminales":"grupos_criminales_gc",
            "actividades_delictivas": "muertos",
            "victimas", "detenidos_as", "ataque_armado_t", 
            "otras_actividades_delictivas", "presencia_internacional",
            "presencia_no_violenta_t", "presencia_no_violenta",
            "instituciones_de_seguridad": "fuerzas_armadas", 
            "fuerzas_de_seguridad", "politica_de_seguridad_y_de_drogas",
            "politica_de_drogas"))

colnames(df_juarez1)

# noviembre 
df_juarez2 <- nov %>% 
  janitor::clean_names() %>%
  select("id"                           = "id",
         "enlace"                       = "x1_2_2_enlace",
         "titulo_de_la_nota"            = "x1_2_3_titulo_de_la_nota",
         "estado"                       = "x1_3_3_estado",
         "municipio"                    = "x1_3_4_municipio",
         "fecha_de_publicacion"         = "x1_2_1_fecha_de_publicacion",
         "fecha_de_los_hechos"          = "x1_3_1_fecha_de_los_hechos", 
         "lugar"                        = "x1_3_5_lugar",
         "nombre_del_grupo_criminal_gc" = "x5_1_3_nombre_del_grupo_criminal_gc",
         "alianza_del_gc"               = "x5_1_4_alianza_del_gc",
         "rival_del_gc"                 = "x5_1_5_rival_del_gc",
         "numero_de_homicidios_total"   = "x2_2_1_numero_de_homicidios_total",
         "numero_de_homicidios_hombre"  = "x2_2_2_numero_de_homicidios_hombre",
         "numero_de_homicidios_mujer"   = "x2_2_3_numero_de_homicidios_mujer",
         "pertenece_a"                  = "x2_2_4_pertenece_a_ocupacion",
         "cuerpo_s_localizado_s"        = "x2_2_5_cuerpo_s_localizado_s_restos_humanos",
         "numero_de_heridos_as_total"   = "x2_3_1_numero_de_heridos_as_total",
         "numero_de_heridos_hombres"    = "x2_3_2_numero_de_heridos_hombres",
         "numero_de_heridas_mujeres"    = "x2_3_3_numero_de_heridas_mujeres",
         "herido_a_pertenece_a"         = "x3_1_4_detenido_a_pertenece_a_ocupacion",
         "ataque_armado"                = "x2_1_1_ataque",
         "privacion_de_la_libertad"     = "x4_1_2_privacion_de_la_libertad",
         "narcomensaje"                 = "x5_1_1_narcomensaje",
         "contenido_del_narcomensaje"   = "x5_1_2_contenido_del_narcomensaje",
         "actividades_ilicitas"         = "x4_1_1_actividades_delictivas",
         "autoridad"                    = "x6_1_autoridad",
         "tipo_de_actividad"            = "x6_2_tipo_de_actividad_de_la_autoridad",
         "politica_de_seguridad"        = "x7_1_politica_de_seguridad") %>% 
  filter(estado == "Chihuahua-08") %>% 
  mutate(municipio = case_when(municipio %in% v_juarez  ~ "Juárez-08-037",
                               municipio == municipio ~ municipio)) %>% 
  filter(municipio == "Juárez-08-037") %>% 
  select(-c("estado", "municipio"))

# diciembre
df_juarez3 <- dic %>% 
  janitor::clean_names() %>%
  select("id"                           = "id",
         "enlace"                       = "x1_2_2_enlace",
         "titulo_de_la_nota"            = "x1_2_3_titulo_de_la_nota",
         "estado"                       = "x1_3_3_estado",
         "municipio"                    = "x1_3_4_municipio",
         "fecha_de_los_hechos"          = "x1_3_1_fecha_de_los_hechos", 
         "fecha_de_publicacion"         = "x1_2_1_fecha_de_publicacion",
         "lugar"                        = "x1_3_5_lugar",
         "nombre_del_grupo_criminal_gc" = "x5_1_3_nombre_del_grupo_criminal_gc",
         "alianza_del_gc"               = "x5_1_4_alianza_del_gc",
         "rival_del_gc"                 = "x5_1_5_rival_del_gc",
         "numero_de_homicidios_total"   = "x2_2_1_numero_de_homicidios_total",
         "numero_de_homicidios_hombre"  = "x2_2_2_numero_de_homicidios_hombre",
         "numero_de_homicidios_mujer"   = "x2_2_3_numero_de_homicidios_mujer",
         "pertenece_a"                  = "x2_2_4_pertenece_a_ocupacion",
         "cuerpo_s_localizado_s"        = "x2_2_5_cuerpo_s_localizado_s_restos_humanos",
         "numero_de_heridos_as_total"   = "x2_3_1_numero_de_heridos_as_total",
         "numero_de_heridos_hombres"    = "x2_3_2_numero_de_heridos_hombres",
         "numero_de_heridas_mujeres"    = "x2_3_3_numero_de_heridas_mujeres",
         "herido_a_pertenece_a"         = "x3_1_4_detenido_a_pertenece_a_ocupacion",
         "ataque_armado"                = "x2_1_1_ataque",
         "privacion_de_la_libertad"     = "x4_1_2_privacion_de_la_libertad",
         "narcomensaje"                 = "x5_1_1_narcomensaje",
         "contenido_del_narcomensaje"   = "x5_1_2_contenido_del_narcomensaje",
         "actividades_ilicitas"         = "x4_1_1_actividades_delictivas",
         "autoridad"                    = "x6_1_autoridad",
         "tipo_de_actividad"            = "x6_2_tipo_de_actividad_de_la_autoridad",
         "politica_de_seguridad"        = "x7_1_politica_de_seguridad") %>% 
  filter(estado == "Chihuahua-08") %>% 
  mutate(municipio = case_when(municipio %in% v_juarez  ~ "Juárez-08-037",
                               municipio == municipio ~ municipio)) %>% 
  filter(municipio == "Juárez-08-037") %>% 
  select(-c("estado", "municipio"))

df_juarez4 <- df_juarez2 %>% 
  bind_rows(df_juarez3)

# variables de interés: grupos, homicidio, cuerpos, herido, ataque 
a <- df_juarez1 %>% 
  select(id:ataque_armado, politica_de_seguridad)

b <- df_juarez4 %>% 
  select(id:ataque_armado, politica_de_seguridad)

df_juarez5 <- a %>% 
  bind_rows(b) %>% 
  filter(politica_de_seguridad == "FALSE") %>% 
  mutate(ataque_armado = 
           case_when(ataque_armado %in% V_arma  ~ "agresión",
                                   ataque_armado == ataque_armado ~ ataque_armado)) 

# Guardar base de Juárez
openxlsx::write.xlsx(df_juarez5, 
                     file = paste_out("juarez.xlsx"), overwrite = T)

#### Generalidades-------------------------------------------------------------

# Totales
sum(df_juarez5$numero_de_homicidios_total, na.rm = T) # de homicidios: 528
sum(df_juarez5$numero_de_heridos_as_total, na.rm = T) # de heridos: 203

sum(df_juarez5$numero_de_homicidios_hombre, na.rm = T)
sum(df_juarez5$numero_de_homicidios_mujer, na.rm = T)

sum(df_juarez5$numero_de_heridos_hombres, na.rm = T)
sum(df_juarez5$numero_de_heridas_mujeres, na.rm = T)

# Ataque armado 
unique(df_juarez5$ataque_armado)
table(df_juarez5$ataque_armado) # agre 315; enfren: 6; arma blanca 13

# 1) Agresión  
agresion <- df_juarez5 %>% 
  filter(ataque_armado == "agresión")
table(agresion$pertenece_a) 
table(agresion$herido_a_pertenece_a) 

# 2) Enfrentamiento  
enfrentamiento <- df_juarez5 %>% 
  filter(ataque_armado == "enfrentamiento")
table(enfrentamiento$pertenece_a)
table(enfrentamiento$herido_a_pertenece_a) 

# Presencia criminal 
grupos <- df_juarez5 %>% 
  select(fecha_de_publicacion, 
         nombre_del_grupo_criminal_gc, alianza_del_gc, rival_del_gc,
         numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s,
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado)


#### Homicidios----------------------------------------------------------------

##### Total de homicidios----
# 1) Total homicidios - base general
df_homicidios_d1 <- df_juarez5 %>% 
  select(fecha_de_publicacion, numero_de_homicidios_total) %>%
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_d1)

# Total: 528 
sum(df_homicidios_d1$total_homicidios, na.rm = T)

mean(df_homicidios_d1$total_homicidios)


###### Cuerpos localizados -----

# 1) Homicidios - cuerpos localizados: general
df_cuerpos_d1 <- df_juarez5 %>% 
  select(fecha_de_publicacion, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_d1)

# Total 
sum(df_cuerpos_d1$total_homicidios, na.rm = T) # cuerpos localizados: 223

###### Agresión -----

# 1) Homicidios - agresión: en general 
df_agre_d1 <- df_juarez5 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_agre_d1$total_homicidios) # homicidios por agresión: 271

mean(df_agre_d1$total_homicidios)

###### Enfrentamientos -----

# 1) Homicidios - enfrentamiento: en general
df_enfren_d1 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_d1)
# Total
sum(df_enfren_d1$total_homicidios) # homicidios en enfrentamiento: 1

###### Otras -----

# 1) Homicidios - arma blanca
df_otro_d1 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "ataque con arma blanca") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_otro_d1$total_homicidios) # arma blanca: 0

# 2) Homicidios - bomba molotv
df_otro_d2 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "bomba molotov") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_otro_d2$total_homicidios) # bomba molotov: 1

# 3) Homicidios - riña
df_otro_d3 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "riña") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_otro_d3$total_homicidios) # riña: 2

# 4) Homicidios - golpeados
df_otro_d4 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "golpeado") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_otro_d4$total_homicidios) # golpeado: 1

# 5) Homicidios - golpes
df_otro_d5 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "golpes") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_otro_d5$total_homicidios) # golpes: 1

#### Heridxs--------------------------------------------------------------

##### Total de heridos----
# 1) Total heridos - base general
df_heridxs_d1 <- df_juarez1 %>% 
  select(fecha_de_publicacion, numero_de_heridos_as_total,
         numero_de_heridos_hombres, numero_de_heridas_mujeres) %>%
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_d1)

# Total: 138
sum(df_heridxs_d1$total_heridos, na.rm = T)

###### Agresión -----

# 1) heridos - agresión: en general 
df_heridxs_d2 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d2$total_heridos) # heridos por agresión: 105

mean(df_heridxs_d2$total_homicidios)

###### Enfrentamientos -----

# 1) heridos - enfrentamiento: en general
df_heridxs_d3 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_d3)
# Total
sum(df_heridxs_d3$total_heridos) # heridos en enfrentamiento: 1

# heridos - armas de fuego (agresió+ enfrentamiento)
df_heridxs_dos <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total,
         numero_de_heridos_hombres, 
         numero_de_heridas_mujeres,
         ataque_armado) %>%
 # filter(ataque_armado == "enfrentamiento") 1 hombre herido %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(ataque_armado) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T),
            total_hombr = sum(numero_de_heridos_hombres, na.rm = T),
            total_mujer = sum(numero_de_heridas_mujeres, na.rm = T))


sum(df_heridxs_dos$numero_de_heridos_hombres, na.rm = T)
sum(df_heridxs_dos$numero_de_heridas_mujeres, na.rm = T)

###### Otras -----

# 1) Heridos - arma blanca
df_heridxs_d4 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "ataque con arma blanca") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d4$total_heridos) # arma blanca: 0

# 2) Heridos - bomba molotv
df_heridxs_d5 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "bomba molotov") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d5$total_heridos) # bomba molotov: 1

# 3) Homicidios - riña
df_heridxs_d6 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "riña") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d6$total_heridos) # riña: 21

# 4) Homicidios - golpeados
df_heridxs_d7 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "golpeado") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d7$total_heridos) # golpeado: 0

# 5) Homicidios - golpes
df_heridxs_d8 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         numero_de_heridos_as_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "golpes") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total
sum(df_heridxs_d8$total_heridos) # golpes: 0

#### Desparecidxs---------------------------------------------------------------

# 1) Desaparecidos: general 
df_desaparecidos1 <- df_juarez1 %>% 
  select(fecha_de_publicacion, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad) %>% 
  filter(privacion_de_la_libertad == "TRUE") %>% 
  filter(numero_de_personas_privadas_de_su_libertad >= "1") %>% 
  group_by(fecha_de_publicacion) %>% 
  summarise(total_desaparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T))
sum(df_desaparecidos1$total_desaparecidos) # desaparecidos: 21
View(df_desaparecidos1)

# 3. Figuras--------------------------------------------------------------------

# Homicidios mensual 

fecha <- df_homicidios_d1 %>% 
  mutate(mes = lubridate::month(fecha_de_publicacion), 
         dia = fecha_de_publicacion,
         semana = lubridate::week(fecha_de_publicacion)) %>% 
  group_by(mes) %>% 
  summarize(mes_total = sum(total_homicidios)) %>% 
  mutate(mes = case_when(
    mes == "6"  ~ "Junio",
    mes == "7"  ~ "Julio",
    mes == "8"  ~ "Agosto",
    mes == "9"  ~ "Septiembre",
    mes == "10" ~ "Octubre",
    mes == "11" ~ "Noviembre",
    mes == "12" ~ "Diciembre")) %>% 
  mutate(mes = factor(
    mes, levels = c("Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")))

ggplot(fecha, aes(x = mes, y = mes_total, group = 1)) +
  geom_point()+
  geom_line() +
  ggrepel::geom_text_repel(aes(label = mes_total), hjust = -0.1, color = "black") +
  labs(title = "Homicidios violentos en Ciudad Juárez",
       subtitle = "De junio a diciembre de 2022\n",
       y = "Número de homicidios\n",
       x = "\nMes",
       caption = "Fuente: Elaboración propia con base en el Monitor-PPD (2022)")+
  guides(color = "none") +
  theme_light() +
  theme(legend.position = "none")


# Guardar figura
# ggsave(file = paste_fig(paste0("homicidio_juarez", ".png")),  width = 10, height = 6)
  
# Homicidios día

fecha_dia <- df_homicidios_d1 %>% 
  mutate(mes = lubridate::month(fecha_de_los_hechos), 
         dia = fecha_de_los_hechos,
         semana = lubridate::week(fecha_de_los_hechos)) 

ggplot(fecha_dia, aes(x = dia, y = total_homicidios, group = 1)) +
  geom_point()+
  geom_line() +
  ggrepel::geom_text_repel(aes(label = total_homicidios), hjust = -0.1, color = "black") +
  labs(title = "Homicidios violentos en Ciudad Juárez",
       subtitle = "Entre junio y octubre de 2022\n",
       y = "Número de homicidios\n",
       x = "\n Día",
       caption = "Fuente: Elaboración propia con base en el Monitor-PPD (2022)")+
  guides(color = "none") +
  theme_light()

# Homicidios semana

fecha_semana <- df_homicidios_d1 %>% 
  mutate(mes = lubridate::month(fecha_de_los_hechos), 
         dia = fecha_de_los_hechos,
         semana = lubridate::week(fecha_de_los_hechos)) %>% 
  group_by(semana) %>% 
  summarize(total_semana = sum(total_homicidios))

ggplot(fecha_semana, aes(x = semana, y = total_semana, group = 1)) +
  geom_point()+
  geom_line() +
  ggrepel::geom_text_repel(aes(label = total_semana), hjust = -0.1, color = "black") +
  labs(title = "Homicidios violentos en Ciudad Juárez",
       subtitle = "Entre junio y octubre de 2022\n",
       y = "Número de homicidios\n",
       x = "\n Semana",
       caption = "Fuente: Elaboración propia con base en el Monitor-PPD (2022)")+
  guides(color = "none") +
  theme_light()

