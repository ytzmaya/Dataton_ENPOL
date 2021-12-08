#
# Author: JRR, MP
# Maintainers: JRR, OE
# Dataton ENPOL 2021
# ------------------------------------ 
#


# Paquetes and download ---------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googledrive, foreign, janitor, spatstat, here)

# ---- Censo
tmp <- tempfile(fileext = ".zip")

files <- drive_ls(as_id("14_hMR1u7ndp62O_KiHWZgYthL-cdOquN")) %>% 
  filter(str_detect(name, "Personas"))

censo <- drive_download(as_id(files$id),
                        path = tmp,
                        overwrite = T)

censo_edad <- read_csv(censo$local_path, locale = locale(encoding = "latin1"))

rm(files, censo)

dist_censo <- censo_edad %>% 
  clean_names() %>% 
  select(sexo, edad, factor) %>% 
  filter(edad %in% 18:130) %>% 
  mutate(rangos_edad = case_when(edad %in% 18:29 ~ 18,
                                 edad %in% 30:39 ~ 30,
                                 edad %in% 40:49 ~ 40,
                                 edad %in% 50:59 ~ 50,
                                 edad %in% 60:69 ~ 60,
                                 edad %in% 70:79 ~ 70,
                                 edad %in% 80:89 ~ 80,
                                 edad %in% 90:97 ~ 90)) %>% 
  group_by(sexo, rangos_edad) %>% 
  summarise(total = sum(factor, na.rm=T)) %>% 
    mutate(den=sum(total, na.rm=T)) %>%
    ungroup() %>%
    mutate(per=round((total/den)*100, 1)) %>%
    na.omit() %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         source = "Censo 2020")


write_csv(dist_censo, "/Users/jorge/Desktop/R/Dataton_ENPOL/01_códigos/output/rangos_edad_censo.csv") 



# ESCOLARIDAD -------------------------------------------------------------
pct_escolaridad <- censo_edad %>% 
  clean_names() %>%
  select(nivacad, edad, sexo, factor) %>% 
  mutate(nivacad = case_when(nivacad %in% c("00","01","02") ~ "Básico (Primaria, preescolar o ninguno)", 
                             nivacad %in% c("03","06") ~ "Secundaria o estudios técnicos con primaria terminada", 
                             nivacad %in% c("04","05","07") ~ "Preparatoria, bachillerato o estudios técnicos con secundaria terminada", 
                             nivacad %in% c("08", "10", "11") ~ "Licenciatura, normal de licenciatura o estudios técnicos con preparatoria terminada",
                             nivacad %in% c("12", "13", "14") ~ "Posgrado (Doctorado, maestría o especialidad",
                             nivacad %in% c("09") ~ "Otro (Normal con primaria o secundaria terminada)",
                             is.na(nivacad) ~ "No aplica",
                             T ~ "No especificado"),
         rango_edad = case_when(edad >= 18 & edad !=9999 ~ "18 y más",
                                edad %in% 0:17 ~ "0 a 17", 
                                T ~ "No especificado"), 
         sexo = case_when(sexo == 1 ~ "Hombre",
                          sexo == 3 ~ "Mujer", 
                          T ~ "No especificado")) %>%
  filter(nivacad != "No aplica", rango_edad == "18 y más") %>%
  group_by(sexo, nivacad) %>% 
  summarise(total_escolaridad = sum(factor, na.rm = T)) %>% 
  group_by(sexo) %>% 
  mutate(pob = sum(total_escolaridad, na.rm =T), 
         pct_escolaridad = (total_escolaridad/pob)*100)

write_csv(pct_escolaridad, "/Users/jorge/Desktop/R/Dataton_ENPOL/01_códigos/output/escolaridad_censo_2020.csv") 





# DONE.  
  