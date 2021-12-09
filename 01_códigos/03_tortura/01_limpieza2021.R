#------------------------------------------------------------------------------#
# Proyecto:              ENPOL 2021 (Tema 3. Tortura y fabricación de culpables)
# Objetivo:              Procesamiento inicial de microdatos de 2021
#
# Encargadas:            Adriana Ortega (INTR)  | Regina I. Medina (INTR)
# Correo:                aortega@intersecta.org | rmedina@intersecta.org
# Fecha de creación:     06 de diciembre de 2021
# Última actualización:  08 de diciembre de 2021
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
p_load(googledrive, googlesheets4, foreign, tidyverse, dplyr, srvyr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "02_datos_crudos/ENPOL/2016/"
out_data    <- "03_datos_limpios/03_tortura/"
out_figs    <- "04_figuras/03_tortura/"

# 1. Cargar datos --------------------------------------------------------------

#--- Importar ids de archivos de ENPOL2021 de la carpeta de drive
df_files <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) 

#--- Importar primera base de ENPOL2021 (prueba)
temp    <- tempfile(fileext = ".csv")
dl      <- drive_download(as_id(df_files$id[1]), path = temp, overwrite = TRUE)
df_raw1 <- read_csv(dl$local_path, locale = locale(encoding = "latin1"))

#--- Importar bases en bucle 
for(i in 1:7){
    temp    <- tempfile(fileext = ".csv")
    
    dl <- drive_download(as_id(df_files$id[i]), path = temp, overwrite = TRUE)
    
    df_csv <- read_csv(dl$local_path, locale = locale(encoding = "latin1"))
    
    assign(paste0("df_raw", i), df_csv)
}

dim(df_raw1) # Todas las bases tienen el mismo número de observaciones (61,449)

# 2. Procesar datos ------------------------------------------------------------

## 2.1. Unificación de bases ---------------------------------------------------

# Variables que conforman la llave personal 
v_llave     <- c("ID_PER" , "CVE_ENT", "NOM_ENT", "CEN_INT", 
                 "NOM_INT", "SEXO"   , "FUERO")

# Varibales muestrales 
v_expansion <- c("EST_DIS", "FPC", "FAC_PER")

# Unir bases
df_unida <- df_raw1                                     %>% 
    full_join(df_raw2, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw3, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw4, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw5, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw6, by = c(v_llave, v_expansion))    %>% 
    full_join(df_raw7, by = c(v_llave, v_expansion))    %>% 
    mutate(year = 2021)

dim(df_unida) # Las mismas 61,449 observaciones


df_mujeres <- df_unida %>% filter(SEXO == 2) 

## 2.2. Clasificación de variables binarias ------------------------------------

# Variables de interés vigentes en 2021

# Observaciones donde la persona no sabe o no respondió sobre el delito
table(df_unida$P5_8_98)
table(df_unida$P5_8_99)

# Personas sentenciadas (P5_3)

# Delitos con PPO                Procesadas | Sentenciadas
# - Robo a casa habitación       (P5_11_02) | (P5_31_02)
# - Homicidio doloso             (P5_11_12) | (P5_31_12)
# - Portación ilegal de armas    (P5_11_13) | (P5_31_13)
# - Secuestro y secuestro exprés (P5_11_17) | (P5_31_17)
# - Violación sexual             (P5_11_18) | (P5_31_18)
# - Delincuencia organizada      (P5_11_20) | (P5_31_20)
# - Privación de la libertad     (P5_11_23) | (P5_31_23)


# Abuso de la fuerza en el arresto (pregunta 3.13)
# - Aplicar fuerza física            (P3_13_02)
# - Esposar                          (P3_13_03)
# - Arma contundente                 (P3_13_04)
# - Arma no letal                    (P3_13_05)
# - Sustancia química                (P3_13_06)
# - Amenazó con arma de fuego        (P3_13_07)
# - Lesión menor                     (P3_13_08)
# - Lesión grave                     (P3_13_09)
# - Lesión mortal                    (P3_13_10)
# - Disparó con arma de fuego        (P3_13_11)
# - Hirió con arma de fuego          (P3_13_12)
       
# Violencia psicológica (3.17)
# - Amenaza de cargos falsos         (P3_17_01)
# - Amenaza de matarlo               (P3_17_02)
# - Amenaza de daño                  (P3_17_03)
# - Amenaza de daño a la familia     (P3_17_04)
# - Otras amenazas                   (P3_17_05)
# - Presión para denunciar a alguien (P3_17_06)
# - Incomunicación                   (P3_17_07)
# - Paseo en automóvil               (P3_17_08)
# - Daño a su familia                (P3_17_09)
# - Vendaron ojos                    (P3_17_11)

# Violencia física (pregunta 3.18)
# - Ataron                           (P3_18_01)
# - Asfixia                          (P3_18_02)
# - Tehuacán                         (P3_18_03)
# - Golpes y patadas                 (P3_18_04)
# - Golpes con objetos               (P3_18_05)
# - Quemaduras                       (P3_18_06)
# - Descagas eléctricas              (P3_18_07)
# - Aplastamiento                    (P3_18_08)
# - Lesiones arma blanca             (P3_18_09)
# - Encajar agujas                   (P3_18_10)
# - Lesiones arma de fuego           (P3_18_11)
# - Otra agresión física             (P3_18_15)

# Violencia sexual (pregunta 3.17 y 3.18)
# - Desvestir                        (P3_17_10)
# - Acoso sexual                     (P3_18_12)
# - Lesiones en genitales            (P3_18_13)
# - Violación sexual                 (P3_18_14)


# Clasificación de variables binarias (ppo, abuso de fuerza y tortura)
df_binaria <- df_unida                                  %>%
    mutate(
        # Sentencia 
        sentencia = ifelse(P5_3 %in% c(2, 3), 1, 0), 
        # Delito que amerita PPO (vigente en el año 2021)
        ppo = case_when(
            # No cometió ningún delito que amerite PPO (procesados y sentenciados)
            !(P5_11_02 == 1 | P5_11_12 == 1 | P5_11_13 == 1 | P5_11_17 == 1 | 
              P5_11_18 == 1 | P5_11_20 == 1 | P5_11_23 == 1 ) ~ 0,
            !(P5_31_02 == 1 | P5_31_12 == 1 | P5_31_13 == 1 | P5_31_17 == 1 | 
              P5_31_18 == 1 | P5_31_20 == 1 | P5_31_23 == 1 ) ~ 0, 
            # Cometió algún delito que amerite PPO (procesados y sentenciados)
             (P5_11_02 == 1 | P5_11_12 == 1 | P5_11_13 == 1 | P5_11_17 == 1 | 
              P5_11_18 == 1 | P5_11_20 == 1 | P5_11_23 == 1 ) ~ 1,
             (P5_31_02 == 1 | P5_31_12 == 1 | P5_31_13 == 1 | P5_31_17 == 1 | 
              P5_31_18 == 1 | P5_31_20 == 1 | P5_31_23 == 1 ) ~ 1, 
            # No sabe (no tomar en cuenta)
              P5_11_98 == 1 ~ NA_real_, 
              P5_31_98 == 1 ~ NA_real_, 
            # No responde (no tomar en cuenta)
              P5_11_99 == 1 ~ NA_real_,
              P5_31_99 == 1 ~ NA_real_), 
        # Arrestos con abuso de fuerza
        uso_fuerza = case_when(
            # Sin abuso de fuerza 
            !(P3_13_02 == 1 | P3_13_03 == 1 | P3_13_04 == 1 | P3_13_05 == 1 | 
              P3_13_06 == 1 | P3_13_07 == 1 | P3_13_08 == 1 | P3_13_09 == 1 | 
              P3_13_10 == 1 | P3_13_11 == 1 | P3_13_12 == 1) ~ 0, 
            # Casos en donde se hubo algún tipo de abuso de fuerza
             (P3_13_02 == 1 | P3_13_03 == 1 | P3_13_04 == 1 | P3_13_05 == 1 | 
              P3_13_06 == 1 | P3_13_07 == 1 | P3_13_08 == 1 | P3_13_09 == 1 | 
              P3_13_10 == 1 | P3_13_11 == 1 | P3_13_12 == 1) ~ 1, 
            # En todas responde que no sabe o no responde
             (P3_13_02 %in% c(8, 9) & P3_13_03 %in% c(8, 9) & 
              P3_13_04 %in% c(8, 9) & P3_13_05 %in% c(8, 9) & 
              P3_13_06 %in% c(8, 9) & P3_13_07 %in% c(8, 9) & 
              P3_13_08 %in% c(8, 9) & P3_13_09 %in% c(8, 9) & 
              P3_13_10 %in% c(8, 9) & P3_13_11 %in% c(8, 9) & 
              P3_13_12 %in% c(8, 9) & P3_13_07 %in% c(8, 9) & 
              P3_13_08 %in% c(8, 9)) ~ NA_real_), 
        # Violencia psicológica 
        violencia_psic = case_when(
            # Casos donde no hubo violencia psicológica
            !(P3_17_01 == 1 | P3_17_02 == 1 | P3_17_03 == 1 | P3_17_04 == 1 | 
              P3_17_05 == 1 | P3_17_06 == 1 | P3_17_07 == 1 | P3_17_08 == 1 | 
              P3_17_09 == 1 | P3_17_11 == 1) ~ 0,
            # Casos donde sí hubo violencia psicológica
             (P3_17_01 == 1 | P3_17_02 == 1 | P3_17_03 == 1 | P3_17_04 == 1 | 
              P3_17_05 == 1 | P3_17_06 == 1 | P3_17_07 == 1 | P3_17_08 == 1 | 
              P3_17_09 == 1 | P3_17_11 == 1) ~ 1,
            # Casos donde no sabe o no respondió 
             (P3_17_01 %in% c(8, 9) & P3_17_02 %in% c(8, 9) & 
              P3_17_03 %in% c(8, 9) & P3_17_04 %in% c(8, 9) & 
              P3_17_05 %in% c(8, 9) & P3_17_06 %in% c(8, 9) & 
              P3_17_07 %in% c(8, 9) & P3_17_08 %in% c(8, 9) & 
              P3_17_09 %in% c(8, 9) & P3_17_11 %in% c(8, 9)) ~ NA_real_), 
        # Indicador de violencia física
        violencia_fisica = case_when(
            # Casos donde no hubo violencia física
            !(P3_18_01 == 1 | P3_18_02 == 1 | P3_18_03 == 1 | P3_18_04 == 1 | 
              P3_18_05 == 1 | P3_18_06 == 1 | P3_18_07 == 1 | P3_18_08 == 1 | 
              P3_18_09 == 1 | P3_18_10 == 1 | P3_18_11 == 1 | P3_18_15 == 1) ~ 0,
            # Casos donde sí hubo violencia física
             (P3_18_01 == 1 | P3_18_02 == 1 | P3_18_03 == 1 | P3_18_04 == 1 | 
              P3_18_05 == 1 | P3_18_06 == 1 | P3_18_07 == 1 | P3_18_08 == 1 | 
              P3_18_09 == 1 | P3_18_10 == 1 | P3_18_11 == 1 | P3_18_15 == 1) ~ 1, 
            # Casos donde no sabe o no respondió 
             (P3_18_01 %in% c(8, 9) & P3_18_02 %in% c(8, 9) &
              P3_18_03 %in% c(8, 9) & P3_18_04 %in% c(8, 9) & 
              P3_18_05 %in% c(8, 9) & P3_18_06 %in% c(8, 9) & 
              P3_18_07 %in% c(8, 9) & P3_18_08 %in% c(8, 9) & 
              P3_18_09 %in% c(8, 9) & P3_18_10 %in% c(8, 9) & 
              P3_18_11 %in% c(8, 9) & P3_18_15 %in% c(8, 9) ~ NA_real_)),
        # Indicador de violencia sexual 
        violencia_sexual = case_when(
            !(P3_17_10 == 1 |  P3_18_12 == 1 |  P3_18_13 == 1 | P3_18_14 == 1) ~ 0,  
             (P3_17_10 == 1 |  P3_18_12 == 1 |  P3_18_13 == 1 | P3_18_14 == 1) ~ 1, 
             (P3_17_10 %in% c(8, 9) & P3_18_12 %in% c(8, 9) &
              P3_18_13 %in% c(8, 9) & P3_18_14 %in% c(8, 9)) ~ NA_real_), 
        #Juntar las dos variables de tortura
        violencia = ifelse(
            violencia_psic == 1 | violencia_fisica == 1 | violencia_sexual == 1,
            1, 0)) 

# Revisar que las variables nuevas estén correctas
# table(df_binaria$sentencia)
# sum(is.na(df_binaria$sentencia))
# 
# table(df_binaria$ppo)
# sum(is.na(df_binaria$ppo))
# 
# table(df_binaria$uso_fuerza)
# sum(is.na(df_binaria$uso_fuerza))
# 
# table(df_binaria$violencia_psic)
# sum(is.na(df_binaria$violencia_psic))
# 
# table(df_binaria$violencia_fisica)
# sum(is.na(df_binaria$violencia_fisica))
# 
# table(df_binaria$violencia_sexual)
# sum(is.na(df_binaria$violencia_sexual))


## 2.3. Diseño de encuesta -----------------------------------------------------

# Aplicar el diseño muestral 
df_encuesta <- df_binaria                   %>%
    as_survey_design(
        ids = ID_PER, strata = EST_DIS, fpc = FPC, weights = FAC_PER)

# Cambiar nombre de variables y categorías
df_enpol <- df_encuesta                     %>% 
    rename(sexo = SEXO)                     %>% 
    mutate(
        sexo = ifelse(sexo == 1, 
            "Hombres", "Mujeres"), 
        sentencia = ifelse(sentencia == 1, 
            "Con sentencia", "Sin sentencia"), 
        ppo  = ifelse(ppo  == 1, 
            "Delito que amerita PPO", "Delito que no amerita PPO"), 
        uso_fuerza = ifelse(uso_fuerza == 1, 
            "Con uso de la fuerza", "Sin uso de la fuerza"), 
        violencia_psic = ifelse(violencia_psic == 1, 
            "Con violencia psicológica", "Sin violencia psicológica"),
        violencia_fisica = ifelse(violencia_fisica == 1, 
            "Con violencia física", "Sin violencia física"),
        violencia_sexual = ifelse(violencia_sexual == 1, 
            "Con violencia sexual", "Sin violencia sexual"),
        violencia = ifelse(violencia == 1, 
            "Con algún tipo de violencia", "Sin ningún tipo de violencia"))


## 2.4. Gráficas (cruces estadísticos) -----------------------------------------

# Tema para gráficas 
tema <-  theme_linedraw() +
    theme(text = element_text(family = "Avenir Next Condensed", color = "black"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Avenir Next Condensed", color = "black"),
        plot.subtitle = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Avenir Next Condensed"),
        plot.caption = element_text(hjust = .5, size = 9, family = "Avenir Next Condensed", color = "black"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10, face = "bold", family="Avenir Next Condensed"),
        legend.text = element_text(size = 10, family="Avenir Next Condensed"),
        axis.title = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Avenir Next Condensed"),
        axis.text.y = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5),
        axis.text.x = element_text(size = 8, family="Avenir Next Condensed", angle=90, hjust=.5),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"),
        strip.text.y = element_text(size=9, family = "Avenir Next Condensed"))

# Colores del tema para policy briefs del CIDE (hecho por Anabel)
c5  <- c("#006535", "#BBD2CD", "#F59F79","#4e79a7", "#92A39D")
c4  <- c("#006535", "#BBD2CD", "#F59F79", "#92A39D") # Verde, gris, verde grisáceo y naranja
c3  <- c("#006535", "#F59F79", "#92A39D")
c2  <- c("#006535", "#F59F79")  
c1  <- c("#006535")


# Formato para gráficas
v_formato <- ".png"

### 2.4.1. Delitos con prisión preventiva oficiosa -----------------------------

# Etiquetas de texto
v_title     <- "Población privada de su libertad en México"
v_subtitle  <- "Por delitos que ameritan prisión prentiva oficiosa y por sexo"
v_caption   <- "
Fuente: Encuesta Nacional de Población Privada de la Libertad (ENPOL 2021). Datos procesados por Intersecta.
Nota: Los delitos incluidos en aquellos que ameritan prisión preventiva oficiosa (PPO) son: 
robo a casa habitación, homicidio doloso, portación ilegal de armas, secuestro, violación sexual, 
delincuencia organizada y privación de la libertad."
v_porcent   <- "Porcentaje"
v_sexo      <- "Sexo"

# ----- Por sexo 
df_ppo_count <- df_enpol                    %>% 
    group_by(sexo, ppo)                     %>% 
    srvyr::survey_count(vartype = c("ci"))  %>% 
    mutate(total = round(n))

# Síntesis de datos 
df_ppo_perct <- df_enpol                    %>%
    filter(!is.na(ppo))                     %>% 
    srvyr::group_by(sexo, ppo)              %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    mutate(ppo = factor(
        ppo, levels = c("Delito que amerita PPO", "Delito que no amerita PPO"))) 

# View(df_ppo)
# names(df_ppo)

# Visualización 
ggplot(df_ppo_perct, 
    #Datos
    aes(x = sexo, y = prop, fill = reorder(ppo, desc(ppo)))) +
    geom_col() +
    geom_label(aes(label = scales::percent(prop)), 
        position = position_stack(1), fill = "white") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "01_enpol2021_ppo_sexo", v_formato), 
    width = 7, height = 6)


# ----- Por estatus y por sexo
# Síntesis de datos 
df_ppo2 <- df_enpol                         %>%
    filter(!is.na(ppo))                     %>% 
    srvyr::group_by(sexo, sentencia, ppo)   %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    mutate(ppo = as.factor(ppo), sentencia = as.factor(sentencia)) %>% 
    mutate(ppo = factor(
        ppo, levels = c("Delito que amerita PPO", "Delito que no amerita PPO"))) 

# names(df_ppo)

# Etiquetas de texto
v_title     <- "Población privada de su libertad en México"
v_subtitle  <- "Por delitos que ameritan prisión prentiva oficiosa y por sexo"
v_porcent   <- "Porcentaje"
v_sexo      <- "Sexo"

# Visualización 
ggplot(df_ppo2, 
    #Datos
    aes(x = sexo, y = prop, fill = reorder(ppo, desc(ppo)))) +
    facet_grid(~sentencia) +
    # Geom
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_label(aes(label = scales::percent(prop)), 
        position = position_stack(1), fill = "white") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "02_enpol2021_ppo_sexo_sentencia", v_formato), 
    width = 7, height = 6)


### 2.4.2. Uso de la fuerza ----------------------------------------------------

# # Síntesis de datos 
# df_fuerza <- df_enpol                       %>%
#     srvyr::group_by(sexo, ppo, uso_fuerza)  %>% 
#     srvyr::summarise(
#         prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
#     filter(!is.na(ppo))
# 
# # names(df_fuerza)
# 
# # Etiquetas de texto
# v_title     <- "Población privada de su libertad en México"
# v_subtitle  <- "Por delito que amerita prisión prentiva oficiosa y por sexo"
# v_caption   <- "Fuente: ENPOL 2021. Datos procesados por Intersecta."
# v_porcent   <- "Porcentaje"
# v_sexo      <- "Sexo"
# 
# 
# # Visualización 
# ggplot(df_fuerza, 
#     aes(x = ppo, y = prop, fill = uso_fuerza)) +
#     facet_grid(~sexo) +
#     geom_col()
# 
# 
# # Guardar 
# ggsave(file = paste0(out_figs, "03_enpol2021_ppo_sexo_sentencia", v_formato), 
#     width = 6, height = 4)


### 2.4.3. Violencia -----------------------------------------------------------

# ---- Algún tipo de violencia
# Síntesis de datos 
df_data <- df_enpol                             %>%
    filter(!is.na(violencia))                   %>% 
    filter(!is.na(ppo))                         %>%
    srvyr::group_by(sexo, ppo, violencia)       %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) 

# Etiquetas de texto
v_title     <- "Personas privadas de su libertad arrestadas con violencia"
v_subtitle  <- "Por delitos que ameritan prisión prentiva oficiosa y por sexo"

# Visualización 
ggplot(df_data, 
    # Datos
    aes(x = ppo, y = prop, fill = reorder(violencia, desc(violencia)))) +
    facet_grid(~sexo) +
    # Geoms
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_label(aes(label = scales::percent(prop)), 
        position = position_stack(1), fill = "white") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "04_enpol2021_violencia_sexo", v_formato), 
    width = 7, height = 6)


# ---- Violencia psicológica   
# Síntesis de datos 
df_data <- df_enpol                             %>%
    filter(!is.na(violencia_psic))              %>% 
    srvyr::group_by(sexo, ppo, violencia_psic)  %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    filter(!is.na(ppo))

# Etiquetas de texto
v_title     <- "Personas privadas de su libertad arrestadas con violencia psicológica"
v_subtitle  <- "Por delito que amerita prisión prentiva oficiosa y por sexo"

# Visualización 
ggplot(df_data, 
    aes(x = ppo, y = prop, fill = reorder(violencia_psic, desc(violencia_psic)))) +
    facet_grid(~sexo) +
    #Geoms
    geom_col()+
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_label(aes(label = scales::percent(prop)), 
        position = position_stack(1), fill = "white") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "05_enpol2021_violencia_psicológica", v_formato), 
    width = 7, height = 6)

# ---- Violencia física    
# Síntesis de datos 
df_data <- df_enpol                                 %>%
    filter(!is.na(violencia_fisica))                %>% 
    srvyr::group_by(sexo, ppo, violencia_fisica)    %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    filter(!is.na(ppo))

# Etiquetas de texto
v_title     <- "Personas privadas de su libertad arrestadas con violencia física"
v_subtitle  <- "Por delitos que ameritan prisión prentiva oficiosa y por sexo"

# Visualización 
ggplot(df_data, 
    aes(x = ppo, y = prop, fill = reorder(violencia_fisica, desc(violencia_fisica)))) +
    facet_grid(~sexo) +
    # Geoms
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_label(aes(label = scales::percent(prop)), 
        position = position_stack(1), fill = "white") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "06_enpol2021_violencia_física", v_formato), 
    width = 7, height = 6)


# ---- Violencia sexual    
# Síntesis de datos 
df_data <- df_enpol                                                 %>%
    filter(!is.na(violencia_sexual))                                %>% 
    srvyr::group_by(sexo, ppo, violencia_sexual)                    %>% 
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.99)) %>% 
    filter(!is.na(ppo))

# Etiquetas de texto
v_title     <- "Personas privadas de su libertad arrestadas con violencia sexual"
v_subtitle  <- "Por delitos que ameritan prisión prentiva oficiosa y por sexual"

# Visualización 
ggplot(df_data, 
    aes(x = ppo, y = prop, fill = reorder(violencia_sexual, desc(violencia_sexual)))) +
    facet_grid(~sexo) +
    #Geoms
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_label(aes(label = scales::percent(prop)), 
    position = position_stack(1), fill = "white") +
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
    scale_fill_manual(values = c(c5[2], c5[1])) +
    theme(legend.position = "top")

# Guardar 
ggsave(file = paste0(out_figs, "07_enpol2021_violencia_sexual", v_formato), 
    width = 7, height = 6)

# FIN. -------------------------------------------------------------------------