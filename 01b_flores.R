# ============================================================
# 01b_flores.R
# Flores (fpnac + ftnac), ESPAC 2025: suma superficie y valor a la productividad
# Agrega: sup_flores, valor_flores, sup_total, valor_total, prod_usd_ha_flores
# Precio por tallo: exportaciones de flores 2025 (BCE) / tallos exportados (ESPAC)
# ============================================================
library(haven)
library(tidyverse)

setwd("D:/Documents/GITHUB/rendimiento_por_h")
dir_espac <- "D:/Downloads/1_BDD_ESPAC_2025_SPSS/1_BDD_ESPAC_2025_SPSS"

valor_export_usd <- 1045e6        # exportaciones de flores 2025 (BCE, citado por Expreso)

fp0 <- read_sav(file.path(dir_espac, "fpnac2025.sav"))
ft0 <- read_sav(file.path(dir_espac, "ftnac2025.sav"))

# ---- 1. Registros de flores ----
flores <- bind_rows(
  fp0 %>% transmute(provincia = as.character(ual_prov),
                    sup_cos = fp_k707ha, tallos = fp_k709, pct_int = fp_venmerint,
                    fexp = fact_exp_fin),
  ft0 %>% transmute(provincia = as.character(ual_prov),
                    sup_cos = ft_k721ha, tallos = ft_k723, pct_int = ft_venmerint,
                    fexp = fact_exp_fin)) %>%
  filter(!is.na(provincia), provincia != "")

# ---- 2. Tope de tallos por hectárea (p99) y expansión ----
flores <- flores %>%
  mutate(tallos_ha = if_else(sup_cos > 0, tallos / sup_cos, NA_real_),
         tope      = quantile(tallos_ha, 0.99, na.rm = TRUE),
         extremo   = !is.na(tallos_ha) & tallos_ha > tope,
         tallos2   = if_else(extremo, tope * sup_cos, tallos),
         pct_int   = pmin(replace_na(pct_int, 0), 100),
         sup_exp    = sup_cos * fexp,
         tallos_exp = tallos2 * fexp,
         tallos_int = tallos_exp * pct_int / 100)

# ---- 3. Precio implícito por tallo y valor ----
precio_tallo <- valor_export_usd / sum(flores$tallos_int, na.rm = TRUE)
cat("Precio implícito por tallo (USD):", round(precio_tallo, 3),
    "| superficie de flores (ha):", round(sum(flores$sup_exp, na.rm = TRUE)),
    "| registros con tope:", sum(flores$extremo, na.rm = TRUE), "\n")

flores <- flores %>% mutate(valor_usd = tallos_exp * precio_tallo)

flores_prov <- flores %>%
  group_by(provincia) %>%
  summarise(n_flores     = n(),
            sup_flores   = sum(sup_exp,   na.rm = TRUE),
            valor_flores = sum(valor_usd, na.rm = TRUE), .groups = "drop")

# ---- 4. Productividad con flores ----
prod_base <- if (file.exists("datos_modelo_2025.rds")) {
  readRDS("datos_modelo_2025.rds")
} else {
  readRDS("productividad_usd_ha_2025.rds")
}
stopifnot(all(c("valor", "sup", "prod_usd_ha", "provincia_nom") %in% names(prod_base)))

datos <- prod_base %>%
  select(-any_of(c("n_flores", "sup_flores", "valor_flores", "sup_total",
                   "valor_total", "prod_usd_ha_flores"))) %>%
  left_join(flores_prov, by = "provincia") %>%
  mutate(across(c(n_flores, sup_flores, valor_flores), ~ replace_na(.x, 0)),
         sup_total   = sup + sup_flores,
         valor_total = valor + valor_flores,
         prod_usd_ha_flores = valor_total / sup_total)

saveRDS(flores_prov, "flores_prov_2025.rds")
saveRDS(datos,       "datos_modelo_2025.rds")
write_csv(datos,     "datos_modelo_2025.csv")

# ---- 5. Revisión ----
datos %>% transmute(provincia_nom, n_flores, sup_flores = round(sup_flores),
                    sin_flores = round(prod_usd_ha), con_flores = round(prod_usd_ha_flores)) %>%
  arrange(desc(con_flores)) %>% print(n = 25)