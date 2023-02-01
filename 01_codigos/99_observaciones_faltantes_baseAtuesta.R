#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Comparación de bases
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          01 de febrero de 2023
# Última actualización:       01 de febrero de 2023
#------------------------------------------------------------------------------#

# Fuente: Monitor PPD 

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

# Cargar base limpiada con código (provine de Share Point [SP])
load(paste_out("monitor_jun_oct.Rdata"))

# Cargar base de Dra. Atuesta (LA)
df_la<- read_xlsx(paste_inp("base1_oct2022.xlsx"))

# 2. Comparaciones -------------------------------------------------------------

## 2.1. Longitudes de los ID ---------------------------------------------------

# Ver dimensiones de las dos bases 
dim(monitor_jun_oct) # Aquí hay 15,573 observaciones 
dim(df_la)         # Aquí son 12,978

# El objetivo es encontrar las observaciones faltantes 
dim(monitor_jun_oct)[1] - dim(df_la)[1] # Diferencia de 2,595


# ---- Comprobar que todos los IDs estén en la base grande 
View(table(df_la$id)) # No hay ningún ID repetido
View(table(monitor_jun_oct$id))

# Extraer ids
v_ids_la <- unique(df_la$id)
v_ids_sp <- unique(monitor_jun_oct$id)

63 %in% v_ids_la
63 %in% v_ids_sp

99 %in% v_ids_la
99 %in% v_ids_sp

# Tienen la misma longitud que la base
length(v_ids_la)
length(v_ids_sp)


sum(!(v_ids_la %in% v_ids_sp)) # Hay 8 IDs en la base en la base chica que no están en la grande 
sum(!(v_ids_sp %in% v_ids_la)) # Hay 2063 IDs en la base en la base grande que no están en la chica

v_ids_totales <- unique(c(v_ids_la, v_ids_sp))
length(v_ids_totales)
length(v_ids_totales) - 8-2063

## 2.2. IDs y notas ------------------------------------------------------------

# El ID 99 está nombrando a dos notas distintas en cada base. Esto implica que 
# no podemos tener la certeza de que los IDs sea una variable útil para 
# empatar las dos bases. 

# Debido a esto, hay que identificar si hay un caso adicional al ID 99 en el que 
# se estén refiriendo a notas distintas. 

# Guardar versiones reducidas de la base(ID, fecha y titulo)
df_notas_la <- df_la %>% select(id, fecha_de_publicacion, titulo_de_la_nota)
df_notas_sp <- monitor_jun_oct %>% select(id, fecha_de_publicacion, titulo_de_la_nota) 

# Unir bases para cotejar IDs
df_compara <- df_notas_la %>% 
  full_join(df_notas_sp, by = "id") 

dim(df_compara)

# Revisión de caso: ¿Son los títulos del ID 1 iguales en las dos bases?
v_title_la <- df_compara$titulo_de_la_nota.x[1]
v_title_sp <- df_compara$titulo_de_la_nota.y[1]

identical(v_title_la, v_title_sp) # Sí son iguales

# ---- Loop para revisar casos idénticos 
v_registro_id <- c()
v_total <- 0

for(i in 1:dim(df_compara)[1]){
  print(i)
  
  v_title_la <- df_compara$titulo_de_la_nota.x[i]
  v_title_sp <- df_compara$titulo_de_la_nota.y[i]
  
  v_identical <- identical(v_title_la, v_title_sp)
  
  # print(v_identical)
  
  if(isFALSE(v_identical)){
    v_registro_id <- c(v_registro_id, df_compara$id[i])
    v_total <- v_total + 1
  }
  
}

# ---- Extraer los casos donde el ID de la base chica nombra a notas distintas entre las bases 
df_distintos <- df_compara %>% 
  filter(id %in% v_registro_id)

# Están reflejadas las 8 notas de la base chica que no están en la grande 
# También las 2593 notas de la base grande que no están en la chica 
# Se encuentran 2 casos adicionales que, según R, tienen títulos distintos,
# pero (al leerlas) se confirma que son la misma nota. 

# 3. Pegar bases ---------------------------------------------------------------

## 3.1. Base con todas las observaciones ---------------------------------------

# Una vez verificada la calidad de la información, armamos la base unificada 
# con todas las observaciones que hay en las dos bases (sin repeticiones). 

# Extraer observaciones de la base chica que faltan en la base grande 


sum(!(v_ids_la %in% v_ids_sp)) # Hay 8 IDs en la base en la base chica que no están en la grande 

v_faltantes_grande <- df_la$id[!(v_ids_la %in% v_ids_sp)] # Extraer los IDs 

df_faltantes_grande <- df_la %>% 
  filter(id %in% v_faltantes_grande)

table(df_faltantes_grande$fecha_de_los_hechos) # Son las ocho notas de noviembre

# No es necesaria incluir estas observaciones dado que estarán en el filtro 
# de noviembre. 


## 3.2. Notas faltantes en la base de Atuesta ----------------------------------

sum(!(v_ids_sp %in% v_ids_la)) # 2,603 observaciones

v_faltantes <- monitor_jun_oct$id[!(v_ids_sp %in% v_ids_la)] # Extraer los IDs 
length(v_faltantes)


# Filtrar y dejar solo observaciones que le faltan a la base de LA
df_faltantes <- monitor_jun_oct %>% 
  filter(id %in% v_faltantes)


dim(df_faltantes)

# 4. Guardar base --------------------------------------------------------------

openxlsx::write.xlsx(df_faltantes, 
                     file = paste_out("df_faltantes.xlsx"), overwrite = T)


# FIN. -------------------------------------------------------------------------
