#
# Author: jrr, oe, mp
# Maintainers: OE
# Dataton ENPOL 2021
# ------------------------------------ 
#

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googledrive, foreign, janitor, spatstat, here)



# ----- ENPOL
tmp <- tempfile(fileext = ".csv")

files <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "SOC"))

enpol <- drive_download(as_id(files$id),
                        path = tmp,
                        overwrite = T)

enpol<- read_csv(enpol$local_path, locale = locale(encoding = "latin1"))


escolaridad_enpol <- enpol %>% 
  clean_names() %>% 
  select(sexo, p1_18_n, fac_per, p1_3) %>% 
  filter(p1_3 %in% 18:97) %>% 
  mutate(p1_18_n = case_when(p1_18_n %in% c("00","01","02") ~ "Básico (Primaria, preescolar o ninguno)", 
                             p1_18_n %in% c("03", "04") ~ "Secundaria o estudios técnicos con primaria terminada", 
                             p1_18_n %in% c("06", "07") ~ "Preparatoria, bachillerato o estudios técnicos con secundaria terminada", 
                             p1_18_n %in% c("08") ~ "Licenciatura, normal de licenciatura o estudios técnicos con preparatoria terminada",
                             p1_18_n %in% c("09") ~ "Posgrado (Doctorado, maestría o especialidad",
                             p1_18_n %in% c("05") ~ "Otro (Normal con primaria o secundaria terminada)",
                             is.na(p1_18_n) ~ "No aplica",
                             T ~ "No especificado")) %>% 
  group_by(sexo, p1_18_n) %>% 
  summarise(total_escolaridad = sum(fac_per, na.rm = T)) %>% 
  group_by(sexo) %>% 
  mutate(pob = sum(total_escolaridad, na.rm =T), 
         pct_escolaridad = (total_escolaridad/pob)*100) %>% 
mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
       source = "ENPOL 2021")

write_csv(escolaridad_enpol, "/Users/jorge/Desktop/R/Dataton_ENPOL/01_códigos/output/escolaridad_enpol_2021.csv") 



# DONE. 
