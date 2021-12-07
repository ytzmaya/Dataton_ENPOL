#
# Author: OE
# Maintainers: OE
# Dataton ENPOL 2021
# ------------------------------------ 
#

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
  group_by(sexo, edad) %>% 
  summarise(total = sum(factor, na.rm=T)) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         source = "Censo 2020")

mediana_censo <- censo_edad %>% 
  clean_names() %>% 
  select(sexo, edad, factor) %>% 
  filter(edad %in% 18:130) %>%  
  group_by(sexo) %>% 
  summarise(mediana_censo = weighted.median(edad, factor)) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         source = "Censo 2020")

# ----- ENPOL
tmp <- tempfile(fileext = ".zip")

files <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "SOC"))

enpol <- drive_download(as_id(files$id),
                        path = tmp,
                        overwrite = T)

enpol_edad <- read_csv(enpol$local_path, locale = locale(encoding = "latin1"))

rm(files, enpol)

dist_enpol <- enpol_edad %>% 
  clean_names() %>% 
  select(sexo, p1_3, fac_per) %>% 
  filter(p1_3 %in% 18:97) %>% 
  group_by(sexo, p1_3) %>% 
  summarise(total = sum(fac_per, na.rm=T)) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         source = "ENPOL 2021") %>% 
  rename(edad = p1_3)

mediana_enpol <- enpol_edad %>% 
  clean_names() %>% 
  select(sexo, p1_3, fac_per) %>% 
  filter(p1_3 %in% 18:97) %>%  
  group_by(sexo) %>% 
  summarise(mediana_enpol = weighted.median(p1_3, fac_per)) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         source = "ENPOL 2021")

edades <- bind_rows(dist_censo,
                    dist_enpol) %>% 
  left_join(mediana_censo) %>% 
  left_join(mediana_enpol) %>%
  mutate(medianas = ifelse(!is.na(mediana_censo), mediana_censo, mediana_enpol))


ggplot(data = edades, aes(x = edad, y = total)) +
  geom_col(fill = "#011F6D") +
  geom_vline(aes(xintercept=medianas, color = source), size = 1, color ="#FF0000") +
  facet_grid(source~sexo, scales = "free_y") + 
  theme_minimal() +
  theme(legend.position = "top")
  



