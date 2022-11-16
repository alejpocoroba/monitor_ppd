#------------------------------------------------------------------------------#
# Proyecto:                   Monitor PPD
# Objetivo:                   Limpieza para R Markdown
#
# Encargado:                  Alejandro Pocoroba
# Correo:                     alejandro.pocoroba@cide.edu
# Fecha de creación:          09 de noviembre de 2022
# Última actualización:       15 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente: Monitor-PPD (2022)

# 0. Configuración inicial------------------------------------------------------
# Liberías
require(pacman)
p_load(
  readxl, tidyverse, dplyr, stringr,
  srvyr, lubridate, zoo, 
  ggtext, mxmaps, sf, 
  viridis, beepr)

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

# 2. Procesar datos------
df_crudo <- df_monitor_amplio %>% 
  # variables de interés
  select(publicacion = fecha_de_publicacion,
         estado, 
         grupo           = nombre_del_grupo_criminal_gc, 
         grupo_a         = alianza_del_gc, 
         grupo_r         = rival_del_gc,
         homicidio_t     = numero_de_homicidios_total, 
         homicidio_v     = numero_de_homicidios_hombre,
         homicidio_m     = numero_de_homicidios_mujer,
         heridos_t       = numero_de_heridos_as_total, 
         heridos_v       = numero_de_heridos_hombres,
         heridos_m       = numero_de_heridas_mujeres,
         detenidos_t     = numero_de_detenidos_as_total, 
         detenidos_v     = numero_de_detenidos_hombres,
         detenidos_m     = numero_de_detenidas_mujeres,
         ataque          = ataque_armado, 
         cuerpos         = cuerpo_s_localizado_s, 
         desaparecidos   = privacion_de_la_libertad, 
         desaparecidos_n = numero_de_personas_privadas_de_su_libertad,
         militar         = autoridad_militar, 
         militar_a       = tipo_de_actividad_militar,
         civil           = autoridad_civil, 
         civil_a         = tipo_de_actividad_civil) %>% 
  # fecha para el reporte: julio, agosto y septiembre 2022 
  filter(publicacion >= '2022-06-01') %>% 
  filter(publicacion <= '2022-08-31')

# 3. Homicidios----
# procesamiento de los datos
homi_mapa <- df_crudo %>% 
  # variables de interés
  select(estado, 
         homicidio_t, 
         homicidio_v,
         homicidio_m) %>% 
  group_by(estado) %>% 
  summarize(total_homicidios = sum(homicidio_t, na.rm = T)) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  # datos geo
  left_join(mxstate.map)

# figura: mapa
ggplot(
  # Datos
  homi_mapa,
  # Coordenadas
  aes(long, lat, group = group, fill = total_homicidios), size = 0, alpha = 0.9) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Homicidios violentos en México",
       subtitle = "De julio, agosto y septiembre del 2022\n",
       fill = "Número de homicidios", 
       caption = "Fuente: Monitor-PPD (2022)") +
  scale_fill_viridis_b(breaks = c(200, 300, 400, 500, 600)) +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))

# ggsave(file = paste_fig("map_homicidios.png"), width = 10, height = 6)

# figura: tabla 
tabla_homi <- df_crudo %>% 
  select(homicidio_t, homicidio_v, homicidio_m, 
         heridos_t, heridos_v, heridos_m,       
         detenidos_t, detenidos_v, detenidos_m) %>% 
  summarize_all(~sum(., na.rm = T)) %>% 
  pivot_longer(
    cols = homicidio_t:detenidos_m, 
    names_to = c("evento", "sexo"),
    names_sep = "_", 
    values_to = "total") %>% 
  mutate(
    sexo = case_when(
      sexo == "t"  ~ "Total", 
      sexo == "v"  ~ "Hombre",
      sexo == "m"  ~ "Mujer"),
    evento = case_when(
      evento == "detenidos" ~ "Detenidos(as)",
      evento == "heridos"   ~ "Heridos(as)",
      T ~ "Homicidios")) %>% 
  group_by(evento, sexo) %>% 
  summarise(total = sum(total)) %>% 
  pivot_wider(
    names_from = sexo, 
    values_from = total) %>% 
  arrange(desc(evento)) %>% 
  rename(Evento = evento)

# install.packages("kableextra") - RMarkdown 

# 4. Tipos de homicidios-------------------------------------------------------
# base - cuerpos
tip_cuerpo$cuerpos <- as.character(tip_cuerpo$cuerpos)

tip_cuerpo <- df_crudo %>% 
  select(homicidio_t, cuerpos) %>% 
  filter(cuerpos == "TRUE") %>% 
  mutate(cuerpos = case_when(
    cuerpos == "TRUE" ~ "cuerpos localizados")) %>% 
  group_by(cuerpos) %>% 
  summarise(total_cuerpos = sum(homicidio_t, na.rm = T)) %>% 
  rename(tipo_agresion = cuerpos,
         total_homicidios = total_cuerpos)

# base - ataque
tip_ataque <- df_crudo %>% 
  select(homicidio_t, 
         ataque,
         cuerpos) %>% 
  filter(homicidio_t == "1") %>% 
  filter(cuerpos == "FALSE") %>% 
  select(!cuerpos)

# Homogenizar 
unique(tip_ataque2$ataque)

v_fuego <- c("agresión por policias", "agresión en penal", "ataque de motosicarios",
             "agresión; feminicidio", "agresión", "agresión en anexo", "discusión",
             "robo", "agresión en anexo", "agresión en casa", "robo de camioneta",
             "robo a casa", "robo; agresión", "intento de robo", 
             "venta de camioneta por facebook")

v_enfrenta <- c("enfrentamiento")

v_blanca <- c("ataque con arma blanca", "riña; arma blanca", 
              "agresión con arma blanca", "arma blanca")

v_golpes <- c("riña", "golpes", "golpeado")

v_otros <- c("bomba molotov", "quemada", "narcobloqueos")

tip_ataque2 <- tip_ataque %>% 
  mutate(
    ataque = case_when(
      ataque %in% v_fuego ~ "arma de fuego",
      ataque %in% v_enfrenta ~ "enfrentamiento",
      ataque %in% v_blanca ~ "arma blanca", 
      ataque %in% v_golpes ~ "golpes",
      ataque %in% v_otros ~ "otros",
      ataque == ataque ~ ataque)) %>% 
  group_by(ataque) %>% 
  summarise(total_homicidios = sum(homicidio_t)) %>% 
  drop_na(ataque) %>% 
  arrange(desc(total_homicidios)) %>% 
  rename(tipo_agresion = ataque)

# Base cuerpos + ataque2
tipo_hom <- tip_ataque2 %>% 
  bind_rows(tip_cuerpo) %>% 
  arrange(desc(total_homicidios))

# figura: gráfica 
ggplot(tipo_hom, 
       aes(x = reorder(tipo_agresion, total_homicidios), y = total_homicidios)) + 
  # Geoms
  geom_col() +
  geom_text(aes(label = total_homicidios), vjust = +0.4, hjust = -0.2) +
  coord_flip() +
  # Etiqueta
  labs(
    title = "Homicidios violentos en México",
    subtitle = "Por tipo de evento de junio a agosto de 2022", 
    x = "Tipo de evento",
    y = "Número de homicidios", 
    caption = "Fuente: Monitor-PPD (2022)") + 
  # Escalas
  # Tema 
  theme_bw() + 
  theme(legend.position = "none")

# 5. Grupos criminales----
df_gc <- df_crudo %>% 
  select(estado, grupo, grupo_a, grupo_r)

### grupo
df_gc2 <- df_gc %>% 
  select(estado, grupo) %>% 
  drop_na()

table(df_gc2$grupo)
unique(df_gc2$grupo)

v_gruposin <- c("Sujetos armados", "Banda de homicidas en Perote", 
                "célula delictiva", "Sin especificar", "sin especificar")

v_autodefensas <- c("Columna Armada del General Pedro José Méndez", 
                    'Columna Armada Pedro José Méndez',
                    "Resistencia Civil de Baja California", 
                    "Autodefensa Fuerza Territorial Poblana", 
                    "Fuerza Territorial Poblana", 
                    "Unión de Pueblos y Organizaciones del Estado de Guerrero (UPOEG)",
                    'Octavio L. alias "Tarzán" líder de autodefensas de Tamaulipas')

v_nombres <- c("Julio Adrián, también conocido como ‘El Mercy’", "‘El Pollo’",
               "‘El Smoll’", "El Panchito", "El Cholo", "Cipog-ez", "‘Zague’.",
               "Don Cabezón", "Oscar Ivan Maras", "el Payepas","El Damaso",
               "El Moyo", "El Jaibol", "“el Santa”", "‘Checo’", "Efraín, alias 'El Gato'",
               "“El Yogui”", "“El Kinkis”", "\"El Bocho\"", "El Vampi", 
               "“El Pillo”, apenas había tomado el poder en sustitución de recién capturado y peligroso líder narcomenudista “El Croquis”",
               "“El Bengala”", "El Ruso", "\"El Rana\" y \"El Gus\"", "La Zorra", 
               "El Colas","‘El Chuy’, ‘El Muerto’ y ‘El Jarro’", "El Grillo", 
               "“El Huevo”", "Fabián N alias \"El Pelón\"", "“La Pegui” y/o “La Sombra”",
               "‘El Carlitos’", "El Kaiman", "El Dany", "El Diablo", "\"El Negro\"",
               "El Komander", "“El Color\"", "“El Merla”", "“El Pelón”.")

# Grupos criminales identificados
df_gc3 <- df_gc2 %>% 
  mutate(
    grupo = case_when(
      grupo %in% v_gruposin ~ "grupo armado",
      grupo %in% v_autodefensas ~ "autodefensas",
      grupo %in% v_nombres ~ "nombres", 
      grupo == grupo ~ grupo)) %>% 
  filter(!(grupo %in% c("grupo armado", "autodefensas", "nombres")))

unique(df_gc3$grupo)

df_gc4 <- df_gc3 %>% 
  mutate(otros = case_when(
    str_detect(grupo, "Cártel de los Correa-") ~ "La familia michoacana",
    str_detect(grupo, "La Villita") ~ "los rivales de El Cerrito",
    str_detect(grupo, "Los Gueros de Morelos;") ~ "Los Guichos de Morelos",
    str_detect(grupo, "Los Mexicles o Los Aztecas;") ~ "Cártel de Juárez",
    str_detect(grupo, "Los Tlacos; Los Ardillos") ~ "Los Ardillos")) 

df_gc5 <- df_gc4 %>% 
  mutate(
    grupo = case_when(
      grupo  == "‘La Línea’ o Nuevo Cártel de Juárez (NCDJ)"  ~ "La Línea",
      grupo  == "Grupo Delta/Gente Nueva"  ~ "Grupo Delta",
      grupo  == "Los capetos"  ~ "Los Capetos",
      grupo  == "Gente Nueva de los Salazar"  ~ "Los Salazar",
      grupo  == "Los Salazares"  ~ "Los Salazar", 
      grupo  == "CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo  == "el Cantinflas; CJNG"  ~ "Cártel Jalisco Nueva Generación (CJNG)",
      grupo  == "Los Salazares"  ~ "Los Salazar",
      grupo  == "banda \"La bolsa negra\""  ~ "Grupo La Bolsa Negra",
      grupo  == "'El Víbora', presunto lugarteniente de 'El Bukanas'"  ~ "Grupo de El Bukanas",
      grupo  == "'El Loco Téllez'"  ~ "grupo de El Loco Téllez",
      grupo  == "Cártel Arellano Félix"  ~ "Cártel de Los Arellano Félix",
      grupo  == "Juan Antonio \"N\", alias \"Mares\" o \"Tambor\"; parte de una célula de integrantes remanentes del CSRDL"  ~ "Cártel de Santa Rosa de Lima",
      grupo  == "Cártel de H. Matamoros"  ~ "Cártel del Golfo",
      grupo  == "“Los Lampones\""  ~ "Los Lampones",
      grupo  == "Unión Tepito"  ~ "La Unión Tepito",
      grupo  == "La familia Michoacana"  ~ "La Familia Michoacana",
      grupo  == "La empresa"  ~ "La Empresa",
      grupo  == "Cártel de los Correa- La familia michoacana"  ~ "Cártel de los Correa",
      grupo  == "pandilleros de La Villita y los rivales de El Cerrito"  ~ "Banda de La Villita",
      grupo  == "Los Gueros de Morelos; Los Guichos de Morelos"  ~ "Los Gueros de Morelos",
      grupo  == "Los Mexicles o Los Aztecas; Cártel de Juárez"  ~ "Los Mexicles",
      grupo  == "Los Tlacos; Los Ardillos"  ~ "Tlacos del Cártel de la Sierra",
      grupo  == "banda de Los Platanitos"  ~ "Banda de Los Platanitos",
      grupo == grupo ~ grupo))

unique(df_gc5$grupo)

df_gc6 <- df_gc5 %>% 
  select(!grupo) %>% 
  drop_na() %>% 
  mutate(
    otros = case_when(
      otros  == "La familia michoacana"  ~ "La Familia Michoacana",
      otros  == "los rivales de El Cerrito"  ~ "Banda de El Cerrito",
      otros == otros ~ otros)) %>% 
  rename(grupo = otros) %>% 
  bind_rows(df_gc5) %>% 
  select(!otros)

### grupo a
df_gca <- df_gc %>% 
  select(estado, grupo_a) %>% 
  drop_na()

df_gca <- df_gca[-4,]

unique(df_gca$grupo_a)

df_gca1 <- df_gca %>% 
  mutate(otros = case_when(
    str_detect(grupo_a, "Los Aztecas; Los Hermanos Mayores; La Línea o Cártel de Juárez") ~ "Los Aztecas"),
    otros2 = case_when(
      str_detect(grupo_a, "Los Aztecas; Los Hermanos Mayores; La Línea o Cártel de Juárez") ~ "Los Hermanos Mayores"),
    otros3 = case_when(
      str_detect(grupo_a, "Los Aztecas; Los Hermanos Mayores; La Línea o Cártel de Juárez") ~ "La Línea")) %>% 
  mutate(
    grupo_a = case_when(
      grupo_a  == "Los Aztecas; Los Hermanos Mayores; La Línea o Cártel de Juárez"  ~ "Cártel de Juárez",
      grupo_a  == "La familia michoacana"  ~ "La Familia Michoacana",
      grupo_a  == "Los Salazar; Gente Nueva"  ~ "Los Salazar",
      grupo_a  == "cártel del pacífico"  ~ "Cártel de Sinaloa",
      grupo_a == grupo_a ~ grupo_a))

df_1 <- df_gca1 %>% 
  select(estado, otros) %>% 
  rename(grupo_a = otros) %>% 
  drop_na()

df_2 <- df_gca1 %>% 
  select(estado, otros2) %>% 
  rename(grupo_a = otros2) %>% 
  drop_na()

df_3 <- df_gca1 %>% 
  select(estado, otros3) %>% 
  rename(grupo_a = otros3) %>% 
  drop_na()

df_gca2 <- df_gca1 %>% 
  bind_rows(df_1) %>% 
  bind_rows(df_2) %>%
  bind_rows(df_3) %>%
  select(!c(otros, otros2, otros3)) %>% 
  rename(grupo = grupo_a)

### grupo r
df_gcr <- df_gc %>% 
  select(estado, grupo_r) %>% 
  drop_na()

df_gcr <- df_gcr[-3,]

unique(df_gcr$grupo_r)

df_gcr1 <- df_gcr %>% 
  mutate(otros = case_when(
    str_detect(grupo_r, "Gente Nuevo; Cártel de Sinaloa") ~ "Cártel de Sinaloa",
    str_detect(grupo_r, "Los Chapos; Los Artistas Asesinos; Cártel de Sinaloa") ~ "Los Artistas Asesinos"),
    otros2 = case_when(
      str_detect(grupo_r, "Los Chapos; Los Artistas Asesinos; Cártel de Sinaloa") ~ "Cártel de Sinaloa")) %>% 
  mutate(
    grupo_r = case_when(
      grupo_r  == "Pájaros sierra"  ~ "Pájaros Sierra",
      grupo_r  == "Gente Nuevo; Cártel de Sinaloa"  ~ "Los Salazar",
      grupo_r  == "Los Chapos; Los Artistas Asesinos; Cártel de Sinaloa"  ~ "Cártel de Sinaloa / Los Chapitos",
      grupo_r == grupo_r ~ grupo_r))

df_1r <- df_gcr1 %>% 
  select(estado, otros) %>% 
  rename(grupo_r = otros) %>% 
  drop_na()

df_2r <- df_gcr1 %>% 
  select(estado, otros2) %>% 
  rename(grupo_r = otros2) %>% 
  drop_na()

df_gcr2 <- df_gcr1 %>% 
  bind_rows(df_1r) %>% 
  bind_rows(df_2r) %>%
  select(!c(otros, otros2)) %>% 
  rename(grupo = grupo_r)

# base gc + g_a + g_r
df_gc_full <- df_gc6 %>% 
  bind_rows(df_gca2) %>% 
  bind_rows(df_gcr2)

length(unique(df_gc_full$grupo)) 

# figura: mapa 
# procesamiento 
df_grupos <- df_gc_full %>% 
  # variables de interés
  select(estado, grupo) %>% 
  group_by(estado, grupo) %>% 
  summarize(total_grupos = n()) %>% 
  drop_na(grupo) %>% 
  group_by(estado) %>% 
  summarize(total_grupos = n()) %>% 
  mutate(region = str_sub(estado, -2, -1)) %>% 
  left_join(mxstate.map)

# mapa 
ggplot(
  # Datos
  df_grupos,
  # Coordenadas
  aes(long, lat, group = group, fill = total_grupos)) +
  coord_map() +
  geom_polygon(color = "black", size = .2, show.legend = T, alpha = 1) +
  labs(title = "Presencia criminal en México",
       subtitle = "Entre julio, agosto y septiembre del 2022\n",
       fill = "Número", 
       caption = "Fuente: Monitor-PPD (2022)") +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = NA))

# 6. Autoridad-----------------------------------------------------------------
