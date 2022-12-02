#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Sonora: antes y después de Quintero
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          26 de octubre de 2022
# Última actualización:       01 de diciembre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, beepr)

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza de los datos------------------------------------------------------
## Datos de Sonora--------------------------------------------------------------
df_sonora <- df_monitor_amplio %>% 
  filter(estado == "Sonora-26") %>% 
  filter(fecha_de_los_hechos >= "2022-06-01") %>% 
  filter(fecha_de_los_hechos <= "2022-08-31")

# 3. Efectos de la detención----------------------------------------------------

## Antes de la detención-----
### 01 de junio al 14 de julio----
df_sonora_antes <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-06-01") %>% 
  filter(fecha_de_los_hechos <= "2022-07-14")

# openxlsx::write.xlsx(df_sonora_antes, file = paste_out("sonora_a.xlsx"), overwrite = T)

# Variables de interés: 
# homicidio, heridos, pertenece + ataque + autoridad (mili y civi)
df_ataque_antes <- df_sonora_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s,
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado,
         autoridad_civil, autoridad_militar)

# Totales
sum(df_ataque_antes$numero_de_homicidios_total, na.rm = T) # de homicidios: 117
sum(df_ataque_antes$numero_de_heridos_as_total, na.rm = T) # de heridos: 31

# Ataque armado 
unique(df_ataque_antes$ataque_armado)
table(df_ataque_antes$ataque_armado) # agresion 61; enfren: 9

# 1) Agresión  
agresion_a <- df_ataque_antes %>% 
  filter(ataque_armado == "agresión")
table(agresion_a$pertenece_a)
table(agresion_a$herido_a_pertenece_a)

# 2) Enfrentamiento  
enfrentamiento_a <- df_ataque_antes %>% 
  filter(ataque_armado == "enfrentamiento")
table(enfrentamiento_a$pertenece_a)
table(enfrentamiento_a$herido_a_pertenece_a)

# Presencia criminal 
grupos_a <- df_sonora_antes %>% 
  select(municipio, fecha_de_los_hechos,
         nombre_del_grupo_criminal_gc, alianza_del_gc, rival_del_gc, 
         numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s,
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado,
         autoridad_civil, autoridad_militar)

table(grupos_a$nombre_del_grupo_criminal_gc)
table(grupos_a$alianza_del_gc)
table(grupos_a$rival_del_gc)

#### Homicidios--------------------------------------------------------------

##### Total de homicidios----
# 1) Total homicidios - base general
df_homicidios_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_a1)

# 2) Total homicidios - fecha de los hechos 
df_homicidios_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_a2)

# 3) Total homicidios - municipal 
df_homicidios_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_a3)

##### Cuerpos localizados -----

# 1) Homicidios - cuerpos localizados: general
df_cuerpos_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_a1)

# Total 
sum(df_cuerpos_a1$total_homicidios, na.rm = T) # cuerpos localizados: 56

# 2) H - cuerpos localizados: fecha de los hechos
df_cuerpos_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_a2)

# 3) H - cuerpos localizados: municipios
df_cuerpos_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_a3)

##### Agresión -----

# 1) Homicidios - agresión: en general 
df_agre_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_agre_a1$total_homicidios) # homicidios por agresión: 50 

# 2) H - agresión: fecha de los hechos 
df_agre_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_agre_a2)

# 3) H - agresión: municipios
df_agre_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_agre_a3)

##### Enfrentamientos -----

# 1) Homicidios - enfrentamiento: en general
df_enfren_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_a1)
# Total
sum(df_enfren_a1$total_homicidios) # homicidios en enfrentamiento: 8

# 2) Homicidios - enfrentamiento: fecha de los hechos
df_enfren_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_a2)

# 3) Homicidios - enfrentamiento: municipios
df_enfren_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_a3)

#### Heridxs--------------------------------------------------------------

# 1) Total heridxs: general
df_heridxs_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_a1)
# Total
sum(df_heridxs_a1$total_heridxs) # heridxs: 31 

# 2) Heridxs - fecha de los hechos 
df_heridxs_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_a2)

# 3) Heridxs - municipios
df_heridxs_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(municipio) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_a3)

##### Agresión -----

# 1) Heridxs - agresión: en general  
df_agre_he_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he_a1)
# Total
sum(df_agre_he_a1$total_heridos) # total heridos - agresión: 21

# 2) He - agresión: fecha de hechos  
df_agre_he_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he_a2)

# 3) He - agresión: municipios 
df_agre_he_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he_a3)

##### Enfrentamiento -----

# 1) Heridxs - enfrentamiento: en general  
df_enfren_he_a1 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total 
sum(df_enfren_he_a1$total_heridos) # total heridos - enfrentamiento: 5
View(df_enfren_he_a1)

# 2) Heridxs - enfrentamiento: fecha de hechos
df_enfren_he_a2 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
View(df_enfren_he_a2)

# 3) Heridxs - enfrentamiento: municipios
df_enfren_he_a3 <- df_ataque_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
View(df_enfren_he_a3)

#### Desparecidxs---------------------------------------------------------------

# 1) Desaparecidos: general 
df_desaparecidos_a1 <- df_sonora_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad) %>% 
  filter(privacion_de_la_libertad == "TRUE") %>% 
  filter(numero_de_personas_privadas_de_su_libertad >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_desaparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T))
sum(df_desaparecidos_a1$total_desaparecidos) # desaparecido: 116
View(df_desaparecidos_a1)

# 2) Desaparecidos - fecha de hechos
df_desaparecidos_a2 <- df_sonora_antes %>% 
  select(municipio, fecha_de_los_hechos, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad) %>% 
  filter(privacion_de_la_libertad == "TRUE") %>% 
  filter(numero_de_personas_privadas_de_su_libertad >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_desaparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T))











## Después de la detención------------------------------------------------------
### 15 de julio al 31 de agosto----
df_sonora_des <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-07-14") %>% 
  filter(fecha_de_los_hechos <= "2022-08-31")

View(df_sonora_des)

# openxlsx::write.xlsx(df_sonora_des, file = paste_out("sonora_d.xlsx"), overwrite = T)

# Variables de interés: 
# homicidio, heridos, pertenece + ataque + autoridad (mili y civi)
df_ataque_desp <- df_sonora_des %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s,
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado,
         autoridad_civil, autoridad_militar)

# Totales
sum(df_ataque_desp$numero_de_homicidios_total) # de homicidios: 131
sum(df_ataque_desp$numero_de_heridos_as_total) # de heridos: 40

# Ataque armado 
unique(df_ataque_desp$ataque_armado)
table(df_ataque_desp$ataque_armado) # agresion 75; enfren: 5; blanca: 1

# 1) Agresión  
agresion <- df_ataque_desp %>% 
  filter(ataque_armado == "agresión")
table(agresion$pertenece_a) # ho - comerciante 1, civil y militar 1
table(agresion$herido_a_pertenece_a) # he - colateral 1, menor de edad 2 y periodista 1

# 2) Enfrentamiento  
enfrentamiento <- df_ataque_desp %>% 
  filter(ataque_armado == "enfrentamiento")
table(enfrentamiento$pertenece_a) # ho - grupo criminal: 3
table(enfrentamiento$herido_a_pertenece_a) # he - militar 1, civil 1 y criminal 1

# Presencia criminal 
grupos <- df_sonora_des %>% 
  select(municipio, fecha_de_los_hechos, nombre_del_grupo_criminal_gc,
         numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s,
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado,
         autoridad_civil, autoridad_militar)



#### Homicidios--------------------------------------------------------------

##### Total de homicidios----
# 1) Total homicidios - base general
df_homicidios_d1 <- df_ataque_desp %>% 
    select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
    filter(numero_de_homicidios_total >= "1") %>% 
    group_by(fecha_de_los_hechos, municipio) %>% 
    summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_d1)

# 2) Total homicidios - fecha de los hechos 
df_homicidios_d2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_d2)

# 3) Total homicidios - municipal 
df_homicidios_d3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total) %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_homicidios_d3)

##### Cuerpos localizados -----

# 1) Homicidios - cuerpos localizados: general
df_cuerpos_d1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_d1)

# Total 
sum(df_cuerpos_d1$total_homicidios, na.rm = T) # cuerpos localizados: 66

# 2) H - cuerpos localizados: fecha de los hechos
df_cuerpos_d2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_d2)

# 3) H - cuerpos localizados: municipios
df_cuerpos_d3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_homicidios_total, pertenece_a, cuerpo_s_localizado_s) %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_cuerpos_d3)

##### Agresión -----

# 1) Homicidios - agresión: en general 
df_agre_d1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_agre_d1$total_homicidios) # homicidios por agresión: 59 


# 2) H - agresión: fecha de los hechos 
df_agre_d2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_agre_d2)

mean(df_agre_d2$total_homicidios)

# 3) H - agresión: municipios
df_agre_d3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_agre_d3)

##### Enfrentamientos -----

# 1) Homicidios - enfrentamiento: en general
df_enfren_d1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_d1)
# Total
sum(df_enfren_d1$total_homicidios) # homicidios en enfrentamiento: 6

# 2) Homicidios - enfrentamiento: fecha de los hechos
df_enfren_d2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_d2)

# 3) Homicidios - enfrentamiento: municipios
df_enfren_d3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_homicidios_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
view(df_enfren_d3)

##### Arma blanca -----

# 1) Homicidios - arma blanca: general
df_arblanca_d1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_homicidios_total, pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "ataque con arma blanca") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_homicidios = sum(numero_de_homicidios_total, na.rm = T))
# Total
sum(df_arblanca_d1$total_homicidios) # arma blanca: 0

#### Heridxs--------------------------------------------------------------

# 1) Total heridxs: general
df_heridxs_d1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_d1)
# Total
sum(df_heridxs_d1$total_heridxs) # heridxs: 40 

# 2) Heridxs - fecha de los hechos 
df_heridxs_d2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_d2)

# 3) Heridxs - municipios
df_heridxs_d3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, numero_de_heridos_as_total) %>% 
  group_by(municipio) %>% 
  summarise(total_heridxs = sum(numero_de_heridos_as_total, na.rm = T))
view(df_heridxs_d3)

##### Agresión -----

# 1) Heridxs - agresión: en general  
df_agre_he1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he1)
# Total
sum(df_agre_he1$total_heridos) # total heridos - agresión: 31

# 2) He - agresión: fecha de hechos  
df_agre_he2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he2)

# 3) He - agresión: municipios 
df_agre_he3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "agresión") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
view(df_agre_he3)

##### Enfrentamiento -----

# 1) Heridxs - enfrentamiento: en general  
df_enfren_he1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
# Total 
sum(df_enfren_he1$total_heridos) # total heridos - enfrentamiento: 8
View(df_enfren_he1)

# 2) Heridxs - enfrentamiento: fecha de hechos
df_enfren_he2 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
View(df_enfren_he2)

# 3) Heridxs - enfrentamiento: municipios
df_enfren_he3 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "enfrentamiento") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))
View(df_enfren_he3)

##### Arma blanca -----

# Heridxs - ataque con arma blanca: en general
df_armablanca_he1 <- df_ataque_desp %>% 
  select(municipio, fecha_de_los_hechos, 
         numero_de_heridos_as_total, herido_a_pertenece_a,
         ataque_armado) %>% 
  filter(ataque_armado == "ataque con arma blanca") %>% 
  filter(numero_de_heridos_as_total >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_heridos = sum(numero_de_heridos_as_total, na.rm = T))

sum(df_armablanca_he1$total_heridos) # total heridos - ataque con arma blanca: 1

#### Desparecidxs---------------------------------------------------------------

# 1) Desaparecidos: general 
df_desaparecidos1 <- df_sonora_des %>% 
  select(municipio, fecha_de_los_hechos, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad) %>% 
  filter(privacion_de_la_libertad == "TRUE") %>% 
  filter(numero_de_personas_privadas_de_su_libertad >= "1") %>% 
  group_by(fecha_de_los_hechos, municipio) %>% 
  summarise(total_desaparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T))
sum(df_desaparecidos1$total_desaparecidos) # desaparecido:98
View(df_desaparecidos1)

# 2) Desaparecidos - fecha de hechos
df_desaparecidos2 <- df_sonora_des %>% 
  select(municipio, fecha_de_los_hechos, 
         privacion_de_la_libertad, numero_de_personas_privadas_de_su_libertad) %>% 
  filter(privacion_de_la_libertad == "TRUE") %>% 
  filter(numero_de_personas_privadas_de_su_libertad >= "1") %>% 
  group_by(fecha_de_los_hechos) %>% 
  summarise(total_desaparecidos = sum(numero_de_personas_privadas_de_su_libertad, na.rm = T))

