# ============================================================
# 02_riego.R
# Riego provincial (% de superficie cosechada con riego), ESPAC 2025
# ============================================================
library(haven)
library(tidyverse)

# ---- 0. Rutas y catálogo ----
dir_espac <- "D:/Downloads/1_BDD_ESPAC_2025_SPSS/1_BDD_ESPAC_2025_SPSS"

prov_nombres <- c("01"="Azuay","02"="Bolívar","03"="Cañar","04"="Carchi","05"="Cotopaxi",
                  "06"="Chimborazo","07"="El Oro","08"="Esmeraldas","09"="Guayas","10"="Imbabura",
                  "11"="Loja","12"="Los Ríos","13"="Manabí","14"="Morona Santiago","15"="Napo",
                  "16"="Pastaza","17"="Pichincha","18"="Tungurahua","19"="Zamora Chinchipe",
                  "20"="Galápagos","21"="Sucumbíos","22"="Orellana","23"="Santo Domingo","24"="Santa Elena")

# ---- 1. Leer ESPAC ----
ct0 <- read_sav(file.path(dir_espac, "ctnac2025.sav"))
cp0 <- read_sav(file.path(dir_espac, "cpnac2025.sav"))

# los nombres de superficie regada vienen cortados, se buscan por patrón
nm_ct <- names(ct0)[str_detect(names(ct0), "^ct_superf_reg")]
nm_cp <- names(cp0)[str_detect(names(cp0), "^cp_superf_reg")]
stopifnot(length(nm_ct) == 1, length(nm_cp) == 1)

# ---- 2. Registros: vacío en superficie regada = no riega (0) ----
riego_reg <- bind_rows(
  ct0 %>% transmute(provincia = as.character(ual_prov), sup_cos = ct_k511ha,
                    regada = .data[[nm_ct]], fexp = fact_exp_fin),
  cp0 %>% transmute(provincia = as.character(ual_prov), sup_cos = cp_k411ha,
                    regada = .data[[nm_cp]], fexp = fact_exp_fin)) %>%
  filter(!is.na(provincia), provincia != "", !is.na(sup_cos), sup_cos > 0) %>%
  mutate(regada   = replace_na(regada, 0),
         regada_c = pmin(regada, sup_cos))      # la regada no supera a la cosechada

# ---- 3. Agregar por provincia ----
riego_prov <- riego_reg %>%
  group_by(provincia) %>%
  summarise(sup_regada = sum(regada_c * fexp),
            sup_cos    = sum(sup_cos  * fexp), .groups = "drop") %>%
  mutate(pct_riego     = 100 * sup_regada / sup_cos,
         provincia_nom = unname(prov_nombres[provincia])) %>%
  filter(!is.na(provincia_nom)) %>%
  arrange(desc(pct_riego))

# ---- 4. Unir con los datos del modelo y guardar ----
base_modelo <- if (file.exists("datos_modelo_2025.rds")) {
  readRDS("datos_modelo_2025.rds")
} else {
  readRDS("productividad_usd_ha_2025.rds")
}

datos <- base_modelo %>%
  select(-any_of(c("pct_riego", "sup_regada"))) %>%      # evita columnas duplicadas
  left_join(select(riego_prov, provincia, pct_riego, sup_regada), by = "provincia")

saveRDS(riego_prov, "riego_prov_2025.rds")
saveRDS(datos,      "datos_modelo_2025.rds")
write_csv(datos,    "datos_modelo_2025.csv")

# ---- 5. Revisiones (opcionales) ----
riego_reg %>% summarise(pct_exceden   = round(100 * mean(regada > sup_cos), 1),
                        pct_con_riego = round(100 * mean(regada > 0), 1))
riego_prov %>% select(provincia_nom, pct_riego, sup_regada) %>% print(n = 25)
names(datos)