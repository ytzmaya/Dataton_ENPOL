#------------------------------------------------------------------------------#
# Proyecto:              ENPOL 2021 (Tema 3. Tortura y fabricación de culpables)
# Objetivo:              Procesamiento inicial de microdatos 
#
# Encargadas:            Adriana Ortega (INTR)  | Regina I. Medina (INTR)
# Correo:                aortega@intersecta.org | rmedina@intersecta.org
# Fecha de creación:     06 de diciembre de 2021
# Última actualización:  06 de diciembre de 2021
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/programas/enpol/2016/

# Notas: 
# - Al actualizar con datos 2021: revisar mismo nombre de variables en delitos

# Dudas:
# - ¿Qué hacemos con las personas que no saben qué delito fue?
# - ¿Se clasifican por separado los delitos de personas procesadas y de 
#   personas sentenciadas? 


# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(foreign, tidyverse, dplyr, srvyr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "02_datos_crudos/ENPOL/2016/"
out_data    <- "03_datos_limpios/03_tortura/"
out_figs    <- "04_figuras/03_tortura/"


# 1. Cargar datos --------------------------------------------------------------

df_raw1 <- read.dbf(paste0(inp, "ENPOL_SOC"      , ".dbf"))
df_raw2 <- read.dbf(paste0(inp, "ENPOL_SEC2_3_4" , ".dbf"))
df_raw3 <- read.dbf(paste0(inp, "ENPOL_SEC_5_6"  , ".dbf"))
df_raw4 <- read.dbf(paste0(inp, "ENPOL_SEC7_1"   , ".dbf"))
df_raw5 <- read.dbf(paste0(inp, "ENPOL_SEC7_2"   , ".dbf"))
df_raw6 <- read.dbf(paste0(inp, "ENPOL_SEC8_9_10", ".dbf"))

dim(df_raw1) # Todas las bases tienen el mismo número de observaciones (58,127)

# 2. Procesar datos ------------------------------------------------------------

## 2.1. Unificación de bases ---------------------------------------------------

# Variables que conforman la llave personal 
v_llave <- c("ID_PER", "CVE_ENT", "NOM_ENT", "CEN_INT", "NOM_INT", 
    "RESUL_VIV", "SEXO", "FUERO")

# Varibales muestrales 
v_expansion <- c("EST_DIS", "FPC", "FAC_PER")

df_unida <- df_raw1                                     %>% 
    full_join(df_raw2, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw3, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw4, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw5, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw6, by = c(v_llave, v_expansion))    %>% 
    mutate(year = 2016)
    
dim(df_unida) # Las mismas 58,127 observaciones

## 2.2. Clasificación de variables binarias ------------------------------------

# Clasificación de PPO vigente en 2021

# Observaciones donde la persona no sabe o no respondió sobre el delito
table(df_unida$P5_8_98)
table(df_unida$P5_8_99)


# Clasificación de variables binarias (ppo, abuso de fuerza y tortura)
df_binaria <- df_unida                                  %>%
    mutate(
        # Delito que amerita PPO (vigente en el año 2021)
        ppo = case_when(
            # No cometió ningún delito que amerite PPO (procesados y sentenciados)
            !(P5_29_2   == 1 | P5_29_10 == 1 | P5_29_11 == 1 | P5_29_15 == 1 | P5_29_22 == 1) ~ 0,
            !(P5_8_2    == 1 | P5_8_10  == 1 | P5_8_11  == 1 | P5_8_15  == 1 | P5_8_22  == 1) ~ 0,
            # Cometió algún delito que amerite PPO (procesados y sentenciados)
             (P5_29_2   == 1 | P5_29_10 == 1 | P5_29_11 == 1 | P5_29_15 == 1 | P5_29_22 == 1) ~ 1,
             (P5_8_2    == 1 | P5_8_10  == 1 | P5_8_11  == 1 | P5_8_15  == 1 | P5_8_22  == 1) ~ 1,
            # No sabe (no tomar en cuenta)
              P5_29_98  == 1 ~ NA_real_, 
              P5_8_98   == 1 ~ NA_real_, 
            # No responde (no tomar en cuenta)
              P5_29_99  == 1 ~ NA_real_,
              P5_8_99   == 1 ~ NA_real_), 
        # Arrestos con abuso de fuerza
        uso_fuerza = case_when(
            # Sin abuso de fuerza 
            !(P3_8_1    == 1 | P3_8_2 == 1 | P3_8_3 == 1 | P3_8_4 == 1 | 
              P3_8_5    == 1 | P3_8_6 == 1 | P3_8_7 == 1 | P3_8_8 == 1) ~ 0, 
            # Casos en donde se usó algún tipo de abuso de fuerza
             (P3_8_1    == 1 | P3_8_2 == 1 | P3_8_3 == 1 | P3_8_4 == 1 | 
              P3_8_5    == 1 | P3_8_6 == 1 | P3_8_7 == 1 | P3_8_8 == 1) ~ 1, 
            # En todas responde que no sabe o no responde
             (P3_8_1 %in% c(8, 9) & P3_8_2 %in% c(8, 9)  & P3_8_3 %in% c(8, 9) & 
              P3_8_4 %in% c(8, 9) & P3_8_5 %in% c(8, 9)  & P3_8_6 %in% c(8, 9) & 
              P3_8_7 %in% c(8, 9) & P3_8_8 %in% c(8, 9)) ~ NA_real_), 
        # Indicador 1 de tortura y malos tratos
        tortura1 = case_when(
            # Casos donde no hubo tortura
            !(P3_12_1 == 1 | P3_12_2 == 1 | P3_12_3 == 1 | P3_12_4 == 1 | 
              P3_12_5 == 1 | P3_12_6 == 1 | P3_12_7 == 1 | P3_12_8 == 1 | 
              P3_12_9 == 1) ~ 0,
            # Casos en donde hubo tortura
             (P3_12_1 == 1 | P3_12_2 == 1 | P3_12_3 == 1 | P3_12_4 == 1 | 
              P3_12_5 == 1 | P3_12_6 == 1 | P3_12_7 == 1 | P3_12_8 == 1 | 
              P3_12_9 == 1) ~ 1,
            # Casos en donde siempre respondió no sé o no respondió 
             (P3_12_1 %in% c(8, 9) & P3_12_2 %in% c(8, 9) & 
              P3_12_3 %in% c(8, 9) & P3_12_4 %in% c(8, 9) &
              P3_12_5 %in% c(8, 9) & P3_12_6 %in% c(8, 9) & 
              P3_12_7 %in% c(8, 9) & P3_12_8 %in% c(8, 9) & 
              P3_12_9 %in% c(8, 9)) ~ NA_real_),
        # Indicador 1 de tortura y malos tratos
        tortura2 = case_when(
            # Casos donde no hubo tortura
            !(P3_13_1 == 1 | P3_13_2 == 1 | P3_13_3 == 1 | P3_13_4 == 1 | 
              P3_13_5 == 1 | P3_13_6 == 1 | P3_13_7 == 1 | P3_13_8 == 1 | 
                                                           P3_13_9 == 1) ~ 0,
            # Casos en donde hubo tortura
             (P3_13_1 == 1 | P3_13_2 == 1 | P3_13_3 == 1 | P3_13_4 == 1 | 
              P3_13_5 == 1 | P3_13_6 == 1 | P3_13_7 == 1 | P3_13_8 == 1 | 
                                                           P3_13_9 == 1) ~ 1,
            # Casos en donde siempre respondió no sé o no respondió 
             (P3_13_1 %in% c(8, 9) & P3_13_2 %in% c(8, 9)  & 
              P3_13_3 %in% c(8, 9) & P3_13_4 %in% c(8, 9)  &
              P3_13_5 %in% c(8, 9) & P3_13_6 %in% c(8, 9)  & 
              P3_13_7 %in% c(8, 9) & P3_13_8 %in% c(8, 9)  & 
                                     P3_13_9 %in% c(8, 9)) ~ NA_real_),
        #Juntar las dos variables de tortura 
        tortura = ifelse(tortura1 == 1 | tortura2 == 1, 1, 0)) 

# Revisar que las variables nuevas estén correctas
        # table(df_binaria$ppo)
        # sum(is.na(df_binaria$ppo))
        # 
        # table(df_binaria$uso_fuerza)
        # sum(is.na(df_binaria$uso_fuerza))
        # 
        # table(df_binaria$tortura1)
        # sum(is.na(df_binaria$tortura1))
        # 
        # table(df_binaria$tortura2)
        # sum(is.na(df_binaria$tortura2))
        # 
        # table(df_binaria$tortura)
        # sum(is.na(df_binaria$tortura))

    
## 2.3. Diseño de encuesta -----------------------------------------------------

# Revisar el diseño para que tenga todos los parámetros bien especificados

# options(survey.lonely.psu="adjust")

# df_encuesta <- df_binaria       %>%
#     mutate_all(~as.numeric(.))  %>%
#     as_survey_design(
#         ids = ID_PER, strata = EST_DIS, fpc = FPC, weights = FAC_PER)


# Diseño de encuesta solo con ids y con pesos
df_encuesta <- df_binaria                   %>%
    mutate_all(~as.numeric(.))              %>%
    as_survey_design(
        ids = ID_PER, weights = FAC_PER)    

# Cambiar nombre de variables y categorías
df_enpol <- df_encuesta                     %>% 
    rename(sexo = SEXO)                     %>% 
    mutate(
        sexo = ifelse(sexo == 1, 
            "Hombre", "Mujer"), 
        ppo  = ifelse(ppo  == 1, 
            "Delito que amerita PPO", "Delito que no amerita PPO"), 
        uso_fuerza = ifelse(uso_fuerza == 1, 
            "Con uso excesivo de la fuerza", "Sin uso excesivo de la fuerza"), 
        tortura = ifelse(tortura == 1, 
            "Con tortura o malos tratos", "Sin tortura o malos tratos")
    )


## 2.4. Gráficas (cruces estadísticos) -----------------------------------------

# Tema para gráficas 
# Formato para gráficas
v_formato <- ".png"

### 2.4.1. Delitos con prisión preventiva oficiosa -----------------------------

# Síntesis de datos 
df_ppo <- df_enpol                          %>%
    srvyr::group_by(sexo, ppo)              %>% 
    srvyr::summarise(
        prop_ppo = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    mutate(ppo = as.factor(ppo)) 

# names(df_ppo)

# Etiquetas de texto
v_title     <- "Población privada de su libertad en México"
v_subtitle  <- "Por delito que amerita prisión prentiva oficiosa y por sexo"
v_caption   <- "Fuente: ENPOL 2021. Datos procesados por Intersecta."
v_porcent   <- "Porcentaje"
v_sexo      <- "Sexo"


# Visualización 
ggplot(df_ppo, 
    #Datos
    aes(x = sexo, y = prop_ppo, fill = ppo)) +
    geom_col() +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        caption  = v_caption,
        y        = v_porcent, 
        x        = "",         
        fill     = "") +
    # Diseño 
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top")


# Guardar 
ggsave(file = paste0(out_figs, "01_enpol2016_ppo_sexo", v_formato), 
    width = 6, height = 4)

### 2.4.2. Uso de la fuerza ----------------------------------------------------

df_fuerza <- df_enpol                       %>%
    srvyr::group_by(sexo, ppo, uso_fuerza)  %>% 
    srvyr::summarise(
        prop_usof = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    filter(!is.na(ppo))

# names(df_fuerza)

ggplot(df_fuerza, 
    aes(x = ppo, y = prop_usof, fill = uso_fuerza)) +
    facet_grid(~sexo) +
    geom_col()


### 2.4.3. Tortura y malos tratos ----------------------------------------------

df_tortura <- df_enpol                      %>%
    srvyr::group_by(sexo, ppo, tortura)     %>% 
    srvyr::summarise(
        prop_tortura = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    filter(!is.na(ppo))


ggplot(df_tortura, 
    aes(x = ppo, y = prop_tortura, fill = tortura)) +
    facet_grid(~sexo) +
    geom_col()


# 3. Guardar datos -------------------------------------------------------------

# FIN. -------------------------------------------------------------------------
