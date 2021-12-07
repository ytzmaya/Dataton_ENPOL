#------------------------------------------------------------------------------#
# Proyecto:              ENPOL 2021 (Tema 3. Tortura y fabricación de culpables)
# Objetivo:              Limpieza inicial de preguntas relevantes al tema 3
#
# Encargadas:            Adriana Ortega (INTR)  | Regina I. Medina (INTR)
# Correo:                aortega@intersecta.org | rmedina@intersecta.org
# Fecha de creación:     06 de diciembre de 2021
# Última actualización:  06 de diciembre de 2021
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/programas/enpol/2016/

# Notas: 
# - Falta terminar el procesamiento previo

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, lubridate, zoo, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "02_datos_crudos/03_tortura/"
out_data    <- "03_datos_limpios/03_tortura/"
out_figs    <- "04_figuras/03_tortura/"

# 1. Cargar datos --------------------------------------------------------------

# 2. Procesar datos ------------------------------------------------------------

# 3. Guardar datos -------------------------------------------------------------

# FIN. -------------------------------------------------------------------------
