# Author: JRR, MP
# Maintainer(s): 
#
# Copyright:2021, Data Cívica, GPL v2 or later
# ===============================================
#


## niv acad, escolari, escoacum, alfabet, hlengua, qdialec_inali, perte_indigena, conact, ocupacion_c,
# sittra, ingtrem, sexo, edad, lugar nacim



# Agrupar por ESCOLARIDAD, ESTUDIOS TÉCNICOS COMO = "Estudios técnicos de primaria a preparatoria" 
# 18 a 29


# Paquetes ----------------------------------------------------------------
if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, foreign, googledrive, janitor, here, 
               stringr, stringi, gargle, R.utils, data.table, sf, utils, purrr, patchwork)


# Files and download ------------------------------------------------------

### censo 2021 
files_censo <- drive_ls(as_id("14_hMR1u7ndp62O_KiHWZgYthL-cdOquN")) %>% 
  filter(str_detect(name, "Personas"))


files_out <- list(ingresos_plot = here("Desktop", "R", "Dataton_ENPOL", "01_códigos", "output", "ingresos_plot."))
devices <- c("png", "svg")


# Read data  --------------------------------------------------------------

# niv acad, escolari, escoacum, alfabet, hlengua, qdialec_inali, perte_indigena, conact, ocupacion_c,
# sittra, ingtrem, sexo, edad, lugar nacim, discapacidad


### censo 2020
tmp <- tempfile(fileext = ".csv")

dl <- drive_download(as_id(files_censo$id), 
                     path = tmp, 
                     overwrite = TRUE) 



censo <- read_csv(dl$local_path) 

p1 <- censo %>% 
  select(INGTRMEN, 
         SEXO, EDAD, FACTOR) %>% 
  clean_names() %>% 
  filter(edad %in% 18:130) %>% 
  mutate(ingtrmen = case_when(ingtrmen %in% 1:2999 ~ "Menos de $3,000",
                            ingtrmen %in% 3000:5500 ~ "De $3,000 a $5,500",
                            ingtrmen %in% 5501:7500 ~ "De $5,501 a $7,500",
                            ingtrmen %in% 7501:9000 ~ "De $7,501 a $9,000",
                            ingtrmen %in% 9001:11000 ~ "De $9,001 a $11,000",
                            ingtrmen > 11000 ~ "Más de $11,000",
                            ingtrmen < 1 ~ "No recibía ingresos",
                            T ~ "No especificado"
                            ),
         ingtrmen = factor(ingtrmen, 
                           levels = c("No especificado",
                                      "No recibía ingresos",
                                      "Más de $11,000",
                                      "De $9,001 a $11,000",
                                      "De $7,501 a $9,000",
                                      "De $5,501 a $7,500",
                                      "De $3,000 a $5,500",
                                      "Menos de $3,000"
                                      )),
         sexo = case_when(sexo %in% 1 ~ "Hombre", 
                          sexo %in% 3 ~ "Mujer")) %>% 
  select(-edad) %>% 
  group_by(sexo, ingtrmen) %>% 
  summarise(total = sum(factor, na.rm=T)) %>% 
  mutate(den=sum(total, na.rm=T)) %>%
  ungroup() %>%
  mutate(per=round((total/den)*100, 1)) %>%
  na.omit() %>% 
  ggplot(data =., aes(sexo, ingtrmen, fill = per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje") +
  geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        #  axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(position = "top") +
  labs(title="CENSO 2020",
       subtitle = "por sexo",
       x="", y="", fill="")

p1

rm(dl, tmp)
# Enpol -------------------------------------------------------------------


###  enpol 2021
inp_files_socio <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "2_3"))



### Enpol 2021 sociodem
tmp <- tempfile(fileext = ".csv")


dl <- drive_download(as_id(inp_files_socio$id), 
                     path = tmp, 
                     overwrite = TRUE) 


enpol_2_3 <- read_csv(dl$local_path) %>% 
  select(ID_PER, SEXO, P2_15, FAC_PER) 


p2 <- enpol_2_3 %>% 
  mutate(P2_15 = case_when(P2_15== 1 ~ "Menos de $3,000",
                           P2_15 == 2 ~ "De $3,000 a $5,500",
                           P2_15 == 3 ~ "De $5,501 a $7,500",
                           P2_15 == 4 ~ "De $7,501 a $9,000",
                           P2_15 == 5 ~ "De $9,001 a $11,000",
                           P2_15 == 6 ~ "Más de $11,000",
                           P2_15 == 7 ~ "No recibía ingresos",
                           T ~ "No especificado"),
         P2_15 = factor(P2_15, 
                           levels = c("No especificado",
                                                 "No recibía ingresos",
                                                 "Más de $11,000",
                                                 "De $9,001 a $11,000",
                                                 "De $7,501 a $9,000",
                                                 "De $5,501 a $7,500",
                                                 "De $3,000 a $5,500",
                                                 "Menos de $3,000")),
                        sexo = case_when(SEXO %in% 1 ~ "Hombre", 
                                         SEXO %in% 2 ~ "Mujer")) %>% 
  select(-ID_PER) %>% 
  group_by(sexo,  P2_15) %>% 
  summarise(total = sum(FAC_PER, na.rm=T)) %>% 
  mutate(den=sum(total, na.rm=T)) %>%
  ungroup() %>%
  mutate(per=round((total/den)*100, 1)) %>%
  na.omit() %>% 
  ggplot(data =., aes(sexo,  P2_15, fill = per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje") +
  geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        #  axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(position = "top") +
  labs(title="ENPOL 2021",
       subtitle = "por sexo",
       x="", y="", fill="")


p2                        
  

rm(dl, tmp)


# Join plots --------------------------------------------------------------

p1 + p2 +
  plot_annotation(
    title = "Ingresos mensuales de la población en México vs personas privadas de la libertad previo a su detención"
  ) &
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5))


walk(devices, ~ ggsave(filename = file.path(paste0(files_out$ingresos_plot, .x)),
                       device = .x, width = 24, height = 14))


# DONE. 

                                