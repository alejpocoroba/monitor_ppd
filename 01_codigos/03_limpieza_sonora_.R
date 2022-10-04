#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Sonora: antes y después de Quintero
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          26 de septiembre de 2022
# Última actualización:       03 de octubre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------

# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, srvyr, lubridate, zoo, ggtext, beepr)

# Silenciar msj de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Limpiar espacio de trabajo 
rm(list = ls ())

# Funciones con direcciones de las carpetas
paste_inp <- function(x){paste0("03_datos_limpios/", x)}
paste_fig <- function(x){paste0("04_figuras/"      , x)}

# 1. Cargar datos---------------------------------------------------------------
load(paste_inp("df_monitor_amplio.Rdata"))

# 2. Limpieza de los datos------------------------------------------------------

## Datos de Sonora--------------------------------------------------------------
df_sonora <- df_monitor_amplio %>% 
  filter(estado == "Sonora-26")

### General---------------------------------------------------------------------
# Se depuran las fechas anteriores a 01/06/2022
df_sonora_gen <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-06-01")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_gen_h1 <- df_sonora_gen %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_gen_h2 <- df_sonora_gen_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_gen_h3 <- df_sonora_gen %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_gen_h4 <- df_sonora_gen %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_gen_h5 <- df_sonora_gen %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))
  
#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_gen_he1 <- df_sonora_gen %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_gen_he2 <- df_sonora_gen_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_gen_he3 <- df_sonora_gen %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_gen_he4 <- df_sonora_gen %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_gen_slo <- df_sonora_gen %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_gen_slo$total)


### Antes de la detención-------------------------------------------------------
# Datos del 01 al 14 de junio
df_sonora_ant <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-06-01") %>% 
  filter(fecha_de_los_hechos <= "2022-06-14")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_ant_h1 <- df_sonora_ant %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_ant_h2 <- df_sonora_ant_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_ant_h3 <- df_sonora_ant %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_ant_h4 <- df_sonora_ant %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_ant_h5 <- df_sonora_ant %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_ant_he1 <- df_sonora_ant %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_ant_he2 <- df_sonora_ant_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_ant_he3 <- df_sonora_ant %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_ant_he4 <- df_sonora_ant %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_ant_slo <- df_sonora_ant %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_ant_slo$total)

### Después de la detención-------------------------------------------------------
# Datos del 01 al 14 de junio
df_sonora_des <- df_sonora %>% 
  filter(fecha_de_los_hechos >= "2022-06-15") %>% 
  filter(fecha_de_los_hechos <= "2022-06-30")

#### Homicidios-----------------------------------------------------------------
# por fecha 1
df_sonora_des_h1 <- df_sonora_des %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de homicidios - total, mujer, hombre 2
df_sonora_des_h2 <- df_sonora_des_h1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_des_h3 <- df_sonora_des %>% 
  select(municipio, numero_de_homicidios_total) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Homicidios localizados 4
df_sonora_des_h4 <- df_sonora_des %>% 
  filter(cuerpo_s_localizado_s == "TRUE") %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, cuerpo_s_localizado_s) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(cuerpo_s_localizado_s, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 5
df_sonora_des_h5 <- df_sonora_des %>% 
  select(numero_de_homicidios_total:numero_de_homicidios_mujer, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_homicidios_total:numero_de_homicidios_mujer), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Heridxs--------------------------------------------------------------------
# por fecha 1
df_sonora_des_he1 <- df_sonora_des %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, fecha_de_los_hechos) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(fecha_de_los_hechos, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Frecuencia de heridxs - total, mujer, hombre 2
df_sonora_des_he2 <- df_sonora_des_he1 %>% 
  group_by(evento) %>% 
  summarise(total = sum(total))

# Por municipio 3
df_sonora_des_he3 <- df_sonora_des %>% 
  select(municipio, numero_de_heridos_as_total) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

# Ataque armado 4
df_sonora_des_he4 <- df_sonora_des %>% 
  select(numero_de_heridos_as_total:numero_de_heridas_mujeres, ataque_armado) %>% 
  pivot_longer(
    cols      = c(numero_de_heridos_as_total:numero_de_heridas_mujeres), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(ataque_armado, evento) %>% 
  summarise(total = sum(total, na.rm = T))

#### Sin localizar--------------------------------------------------------------
df_sonora_des_slo <- df_sonora_des %>% 
  select(municipio, numero_de_personas_privadas_de_su_libertad) %>% 
  pivot_longer(
    cols      = c(numero_de_personas_privadas_de_su_libertad), 
    names_to  = "evento", 
    values_to = "total") %>% 
  group_by(municipio, evento) %>% 
  summarise(total = sum(total, na.rm = T))

sum(df_sonora_des_slo$total)

### Figuras 


