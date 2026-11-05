# ============================================================
# 02b_riego_flores.R
# Riego de las flores sumado al riego de los demás cultivos
# Requiere: 02_riego.R (sup_regada, sup) y 01b_flores.R
# Agrega: pct_riego_flores
# ============================================================
library(haven)
library(tidyverse)

setwd("D:/Documents/GITHUB/rendimiento_por_h")
dir_espac <- "D:/Downloads/1_BDD_ESPAC_2025_SPSS/1_BDD_ESPAC_2025_SPSS"

fp0 <- read_sav(file.path(dir_espac, "fpnac2025.sav"))
ft0 <- read_sav(file.path(dir_espac, "ftnac2025.sav"))

riego_fl <- bind_rows(
  fp0 %>% transmute(provincia = as.character(ual_prov), sup_cos = fp_k707ha,
                    regada = fp_superf_regada_ha, fexp = fact_exp_fin),
  ft0 %>% transmute(provincia = as.character(ual_prov), sup_cos = ft_k721ha,
                    regada = ft_superf_regada_ha, fexp = fact_exp_fin)) %>%
  filter(!is.na(provincia), !is.na(sup_cos), sup_cos > 0) %>%
  mutate(regada_c = pmin(replace_na(regada, 0), sup_cos)) %>%
  group_by(provincia) %>%
  summarise(reg_fl = sum(regada_c * fexp), cos_fl = sum(sup_cos * fexp), .groups = "drop")

base_modelo <- readRDS("datos_modelo_2025.rds")
stopifnot(all(c("sup_regada", "sup", "pct_riego") %in% names(base_modelo)))

datos <- base_modelo %>%
  select(-any_of(c("reg_fl", "cos_fl", "pct_riego_flores"))) %>%
  left_join(riego_fl, by = "provincia") %>%
  mutate(across(c(reg_fl, cos_fl), ~ replace_na(.x, 0)),
         pct_riego_flores = 100 * (sup_regada + reg_fl) / (sup + cos_fl))

saveRDS(datos,   "datos_modelo_2025.rds")
write_csv(datos, "datos_modelo_2025.csv")

datos %>% filter(provincia %in% c("17", "05", "10")) %>%
  select(provincia_nom, pct_riego, pct_riego_flores)