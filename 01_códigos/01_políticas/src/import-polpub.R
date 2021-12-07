#
# Author: OE
# Maintainers: OE
# Dataton ENPOL 2021
# ------------------------------------ 
#

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googledrive, foreign, janitor, here)


# ----- 2016
tmp1 <- tempfile(fileext = ".dbf")
tmp2 <- tempfile(fileext = ".dbf")
tmp3 <- tempfile(fileext = ".dbf")

enpol16_5 <- drive_ls(as_id("1qw7cAiBTYj8NEa4DJFoezzpNxrOqVb0z")) %>% 
  filter(str_detect(name, "ENPOL_SEC_5_6.dbf"))

enpol16_7 <- drive_ls(as_id("1qw7cAiBTYj8NEa4DJFoezzpNxrOqVb0z")) %>% 
  filter(str_detect(name, "ENPOL_SEC7_1.dbf"))

enpol16_10 <- drive_ls(as_id("1qw7cAiBTYj8NEa4DJFoezzpNxrOqVb0z")) %>% 
  filter(str_detect(name, "ENPOL_SEC8_9_10.dbf"))

dl5 <- drive_download(as_id(enpol16_5$id),
                       path = tmp1,
                       overwrite = T)

dl7 <- drive_download(as_id(enpol16_7$id),
                      path = tmp2,
                      overwrite = T)

dl10 <- drive_download(as_id(enpol16_10$id),
                       path = tmp3,
                       overwrite = T)

enpol2016 <- left_join(read.dbf(dl5$local_path, as.is = T) %>% 
                         clean_names %>% 
                         select(id_per, sexo, p5_3, p5_4_a, starts_with("p5_8")),
                       read.dbf(dl7$local_path, as.is = T) %>% 
                         clean_names %>% 
                         select(id_per, starts_with("p7_29"), p7_30, starts_with("p7_31"), starts_with("p7_32")),
                       by = "id_per") %>% 
  left_join(read.dbf(dl10$local_path, as.is = T) %>% 
              clean_names %>% 
              select(id_per, starts_with("p9_"), starts_with("p10_"), fac_per),
            by = "id_per")

# ---- 2021 
tmp1 <- tempfile(fileext = ".csv")
tmp2 <- tempfile(fileext = ".csv")
tmp3 <- tempfile(fileext = ".csv")

enpol21_5 <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "ENPOL2021_5.csv"))

enpol21_7 <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "ENPOL2021_7.csv"))

enpol21_10 <- drive_ls(as_id("1TigvyIp4QZmkSoGR58bekibx3bTQ7fhb")) %>% 
  filter(str_detect(name, "ENPOL2021_8_9_10_11.csv"))

dl5 <- drive_download(as_id(enpol21_5$id),
                      path = tmp1,
                      overwrite = T)

dl7 <- drive_download(as_id(enpol21_7$id),
                      path = tmp2,
                      overwrite = T)

dl10 <- drive_download(as_id(enpol21_10$id),
                       path = tmp3,
                       overwrite = T)

enpol2021 <- left_join(read_csv(dl5$local_path) %>% 
                         clean_names %>% 
                         select(id_per, sexo, p5_3, p5_4_a, starts_with("p5_11")),
                       read_csv(dl7$local_path) %>% 
                         clean_names %>% 
                         select(id_per, p7_25, p7_26, p7_27, starts_with("p7_28"), p7_29),
                       by = "id_per") %>%  
  left_join(read_csv(dl10$local_path) %>% 
              clean_names %>% 
              select(id_per, starts_with("p9_"), starts_with("p10_"), fac_per),
            by = "id_per") %>% 
   mutate(p5_4_a = as.integer(p5_4_a),
          sentencia_rango = case_when(p5_4_a %in% 1:5 ~ "1 a 5 años",
                                     p5_4_a %in% 6:10 ~ "6 a 10 años",
                                     p5_4_a %in% 11:15 ~ "11 a 15 años",
                                     p5_4_a %in% 16:20 ~ "16 a 20 años",
                                     p5_4_a > 20 ~ "Más de 20 años"),
           sentencia_rango = factor(sentencia_rango, levels = c("1 a 5 años", "6 a 10 años",
                                                               "11 a 15 años", "16 a 20 años",
                                                               "Más de 20 años")))

rm(list=ls()[! ls() %in% c("enpol2016", "enpol2021")])

# ----- Analysis

# ----- expectativas 2021

tempo <- bind_rows(enpol2021 %>% 
                     group_by(sexo, p10_5_1) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Encontrar trabajo") %>% 
                     ungroup() %>% 
                     filter(p10_5_1 == 1) %>% 
                     select(-p10_5_1),
                   enpol2021 %>% 
                     group_by(sexo, p10_5_2) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Continuar estudiando") %>% 
                     ungroup() %>% 
                     filter(p10_5_2 == 1) %>% 
                     select(-p10_5_2),
                   enpol2021 %>% 
                     group_by(sexo, p10_5_3) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Reencontrar a sus amigos") %>% 
                     ungroup() %>% 
                     filter(p10_5_3 == 1) %>% 
                     select(-p10_5_3),
                   enpol2021 %>% 
                     group_by(sexo, p10_5_4) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Reintegrarse a su familia") %>% 
                     ungroup() %>% 
                     filter(p10_5_4 == 1) %>% 
                     select(-p10_5_4)) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         var = factor(var, levels = c("Encontrar trabajo",
                                      "Continuar estudiando",
                                      "Reencontrar a sus amigos",
                                      "Reintegrarse a su familia")))

ggplot(data = tempo, aes(x = fct_rev(var), y = pct, fill = sexo)) +
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 1)) +
  labs(title = "¿Considera que el haber estado en un centro penitenciario afecte sus posibilidades de...?",
       y = "Porcentaje de PPLs", x = "", fill = "",
       caption = "Fuente: Elaboración propia con datos de la ENPOL 2021, pregunta P10.5") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.x = element_blank())

ggsave(here("01_códigos/01_políticas/output/expectativas-sexo.svg"), width = 10, height = 10)

# Reincidencias

tempo <- enpol2021 %>% 
  group_by(sexo, p9_1, p10_5_1) %>% 
  summarise(total = round(sum(fac_per, na.rm = T))) %>% 
  ungroup() %>% 
  group_by(sexo, p9_1) %>% 
  mutate(pct = round(total/sum(total)*100, 1),
         var = "Encontrar trabajo") %>% 
  ungroup() %>% 
  filter(p10_5_1 == 1 & p9_1 %in% 1:2) %>% 
  select(-p10_5_1)


tempo <- bind_rows(enpol2021 %>% 
                     group_by(sexo, p9_1, p10_5_1) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo, p9_1) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Encontrar trabajo") %>% 
                     ungroup() %>% 
                     filter(p10_5_1 == 1 & p9_1 %in% 1:2) %>% 
                     select(-p10_5_1),
                   enpol2021 %>% 
                     group_by(sexo, p9_1, p10_5_2) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo, p9_1) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Continuar estudiando") %>% 
                     ungroup() %>% 
                     filter(p10_5_2 == 1 & p9_1 %in% 1:2) %>% 
                     select(-p10_5_2),
                   enpol2021 %>% 
                     group_by(sexo, p9_1, p10_5_3) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo, p9_1) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Reencontrar a sus amigos") %>% 
                     ungroup() %>% 
                     filter(p10_5_3 == 1 & p9_1 %in% 1:2) %>% 
                     select(-p10_5_3),
                   enpol2021 %>% 
                     group_by(sexo, p9_1, p10_5_4) %>% 
                     summarise(total = round(sum(fac_per, na.rm = T))) %>% 
                     ungroup() %>% 
                     group_by(sexo, p9_1) %>% 
                     mutate(pct = round(total/sum(total)*100, 1),
                            var = "Reintegrarse a su familia") %>% 
                     ungroup() %>% 
                     filter(p10_5_4 == 1 & p9_1 %in% 1:2) %>% 
                     select(-p10_5_4)
                   ) %>% 
  mutate(sexo = ifelse(sexo==1, "Hombres", "Mujeres"),
         var = factor(var, levels = c("Encontrar trabajo",
                                      "Continuar estudiando",
                                      "Reencontrar a sus amigos",
                                      "Reintegrarse a su familia")),
         p9_1 = factor(p9_1, levels = c(1,2), labels = c("Reincidentes", "No reincidentes")))

ggplot(data = tempo, aes(x = p9_1, y = sexo, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = paste0(pct, "%"))) +
  facet_wrap(~var) +
  labs(title = "¿Considera que el haber estado en un centro penitenciario afecte sus posibilidades de...?",
       subtitle = "Por sexo y reincidencia",
       x = "", y = "",
       caption = "Fuente: Elaboración propia con datos de la ENPOL 2021, pregunta P10.5 y P9.1") +
  scale_fill_continuous(low = "#e5f5e0", high  = "#31a354") +
  coord_fixed() +
  theme_minimal() +
  theme(text = element_text(size = 18, face = "bold"),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.position = "none")

ggsave(here("01_códigos/01_políticas/output/expectativas-sexo-reincid.svg"), width = 10, height = 10)

# ---- reinsidencia 2016 vs 2021

tempo <- bind_rows(
  enpol2016 %>% 
    mutate(across(everything(), as.numeric)) %>% 
    group_by(sexo, p10_6) %>% 
    summarise(total = sum(as.integer(fac_per), na.rm = T)) %>% 
    ungroup() %>% 
    group_by(sexo) %>% 
    mutate(pct = round(total/sum(total)*100, 1),
           year = 2016),
enpol2021 %>% 
  group_by(sexo, p10_7) %>% 
  summarise(total = sum(as.integer(fac_per), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo) %>% 
  mutate(pct = round(total/sum(total)*100, 1),
         year = 2021) %>% 
  rename(p10_6 = p10_7)
) 
  
  
