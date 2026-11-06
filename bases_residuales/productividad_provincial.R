# ============================================================
# Productividad agrícola provincial (USD/ha), ESPAC 2025
# ============================================================
library(haven)
library(tidyverse)
library(readxl)

# ---- 0. Rutas (cambiar solo aquí) ----
dir_espac <- "D:/Downloads/1_BDD_ESPAC_2025_SPSS/1_BDD_ESPAC_2025_SPSS"
f_precios <- "D:/Downloads/precios-productor-ponderado.xlsx"

# ---- 1. Catálogos ----
prov_nombres <- c("01"="Azuay","02"="Bolívar","03"="Cañar","04"="Carchi","05"="Cotopaxi",
                  "06"="Chimborazo","07"="El Oro","08"="Esmeraldas","09"="Guayas","10"="Imbabura",
                  "11"="Loja","12"="Los Ríos","13"="Manabí","14"="Morona Santiago","15"="Napo",
                  "16"="Pastaza","17"="Pichincha","18"="Tungurahua","19"="Zamora Chinchipe",
                  "20"="Galápagos","21"="Sucumbíos","22"="Orellana","23"="Santo Domingo","24"="Santa Elena")

prov_sipa <- c("01"="AZUAY","02"="BOLIVAR","03"="CAÑAR","04"="CARCHI","05"="COTOPAXI",
               "06"="CHIMBORAZO","07"="EL ORO","08"="ESMERALDAS","09"="GUAYAS","10"="IMBABURA",
               "11"="LOJA","12"="LOS RIOS","13"="MANABÍ","14"="MORONA SANTIAGO","15"="NAPO",
               "16"="PASTAZA","17"="PICHINCHA","18"="TUNGURAHUA","19"="ZAMORA CHINCHIPE",
               "20"="GALAPAGOS","21"="SUCUMBIOS","22"="ORELLANA",
               "23"="SANTO DOMINGO DE LOS TSÁCHILAS","24"="SANTA ELENA")

# Grupo de cultivo según el nombre en el SIPA (el orden da prioridad)
pat_sipa <- c(
  ARROZ = "^Arroz Cáscara",            MAIZ_DURO = "^Maíz Duro Seco",
  MAIZ_SUAVE_CHOCLO = "^Maíz Suave Choclo", MAIZ_SUAVE_SECO = "^Maíz Suave Seco",
  PAPA = "^Papa Superchola",           CACAO = "^Cacao .*Almendra Seca",
  BANANO = "^Banano Cavendish",
  PLATANO = "^Plátano (Barraganete|Dominico|Hartón)$",
  YUCA = "^Yuca$",                     PALMA = "^Palma De Aceite",
  CANA_OTROS = "^Caña.*Otros Usos",    CANA_AZUCAR = "^Caña De Azúcar para",
  CAFE = "^Café Arábigo Grano Oro",    LIMON = "^Limón Sutil",
  MARACUYA = "^Maracuyá",              PINA = "^Piña",
  PITAHAYA = "^Pitahaya",              TOMATE_ARBOL = "^Tomate De Árbol",
  TOMATE_RINON = "^Tomate Riñón",      ARVEJA_TIERNA = "^Arveja Tierna",
  HABA_TIERNA = "^Haba Tierna",        NARANJILLA = "^Naranjilla",
  FRUTILLA = "^Frutilla",              BROCOLI = "^Brócoli",
  AGUACATE = "^Aguacate",              PALMITO = "^Palmito",
  ARANDANO = "^Arandano",              FREJOL_SECO = "^Fréjol Seco",
  SOYA = "^Soya",                      NARANJA = "^Naranja$",
  CEBOLLA_BLANCA = "^Cebolla En Rama")

# Mismo grupo según el nombre del cultivo en la ESPAC
pat_espac <- c(
  ARROZ = "^ARROZ",                    MAIZ_DURO = "^MA[IÍ]Z DURO",
  MAIZ_SUAVE_CHOCLO = "^MA[IÍ]Z SUAVE CHOCLO", MAIZ_SUAVE_SECO = "^MA[IÍ]Z SUAVE SECO",
  PAPA = "^PAPA",                      CACAO = "^CACAO",
  BANANO = "^BANANO",                  PLATANO = "^PL[ÁA]TANO",
  YUCA = "^YUCA",                      PALMA = "^PALMA",
  CANA_OTROS = "OTROS USOS",           CANA_AZUCAR = "^CA[ÑN]A",
  CAFE = "^CAF[ÉE]",                   LIMON = "^LIM[ÓO]N",
  MARACUYA = "^MARACUY",               PINA = "^PI[ÑN]A",
  PITAHAYA = "^PITAHAYA",              TOMATE_ARBOL = "^TOMATE DE [ÁA]RBOL",
  TOMATE_RINON = "^TOMATE RI[ÑN]ON",   ARVEJA_TIERNA = "^ARVEJA TIERNA",
  HABA_TIERNA = "^HABA TIERNA",        NARANJILLA = "^NARANJILLA",
  FRUTILLA = "^FRUTILLA",              BROCOLI = "^BR[ÓO]COLI",
  AGUACATE = "^AGUACATE",              PALMITO = "^PALMITO",
  ARANDANO = "^AR[ÁA]NDANO",           FREJOL_SECO = "^FR[ÉE]JOL SECO",
  SOYA = "^SOYA",                      NARANJA = "^NARANJA",
  CEBOLLA_BLANCA = "^CEBOLLA BLANCA")

asignar_grupo <- function(x, patrones) {
  out <- rep(NA_character_, length(x))
  for (i in rev(seq_along(patrones))) out[str_detect(x, patrones[[i]])] <- names(patrones)[i]
  out
}

# ---- 2. Base ESPAC (producción con tope de rendimiento p99) ----
ct <- read_sav(file.path(dir_espac, "ctnac2025.sav")) %>%
  transmute(provincia = as.character(ual_prov),
            cultivo   = str_squish(as.character(as_factor(ct_nclavr))),
            sup_cos = ct_k511ha, prod_t = ct_prod, fexp = fact_exp_fin,
            tipo = "transitorio")

cp <- read_sav(file.path(dir_espac, "cpnac2025.sav")) %>%
  transmute(provincia = as.character(ual_prov),
            cultivo   = str_squish(as.character(as_factor(cp_nclavr))),
            sup_cos = cp_k411ha, prod_t = cp_prod, fexp = fact_exp_fin,
            tipo = "permanente")

base <- bind_rows(ct, cp) %>%
  filter(!is.na(provincia), provincia != "") %>%
  mutate(sup_exp = sup_cos * fexp,
         rend    = if_else(sup_cos > 0, prod_t / sup_cos, NA_real_)) %>%
  group_by(cultivo) %>%
  mutate(tope = quantile(rend, 0.99, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(extremo  = !is.na(rend) & rend > tope & !str_detect(cultivo, "OTROS"),
         prod_t2  = if_else(extremo, tope * sup_cos, prod_t),
         prod_exp0 = prod_t * fexp,      # producción original expandida
         prod_exp  = prod_t2 * fexp)     # producción con tope (la que se usa)

# ---- 3. Precios SIPA 2025 (USD/t) ----
sipa <- read_excel(f_precios, sheet = "Nacional", range = cell_cols("A:I"))
names(sipa) <- c("anio","mes","provincia","producto","presentacion",
                 "equiv","unidad","precio_usd","precio_usd_kg")

sipa25 <- sipa %>%
  filter(as.character(anio) == "2025") %>%
  mutate(usd_t = as.numeric(precio_usd_kg) * 1000,
         grupo = asignar_grupo(producto, pat_sipa)) %>%
  filter(!is.na(grupo), is.finite(usd_t))

precio_prov <- sipa25 %>% group_by(provincia, grupo) %>%
  summarise(p_prov = mean(usd_t), .groups = "drop")
precio_nac  <- precio_prov %>% group_by(grupo) %>%
  summarise(p_nac = mean(p_prov), .groups = "drop")

# ---- 4. Precio por registro: provincial > nacional > medio del tipo ----
base2 <- base %>%
  mutate(prov_sipa = unname(prov_sipa[provincia]),
         grupo     = asignar_grupo(cultivo, pat_espac)) %>%
  left_join(precio_prov, by = c("prov_sipa" = "provincia", "grupo")) %>%
  left_join(precio_nac,  by = "grupo")

p_tipo <- base2 %>%
  filter(!is.na(p_prov) | !is.na(p_nac), !is.na(prod_exp)) %>%
  group_by(tipo) %>%
  summarise(p_tipo = weighted.mean(coalesce(p_prov, p_nac), prod_exp),
            .groups = "drop")

base2 <- base2 %>%
  left_join(p_tipo, by = "tipo") %>%
  mutate(precio_usd_t = coalesce(p_prov, p_nac, p_tipo),
         fuente = case_when(!is.na(p_prov) ~ "provincial",
                            !is.na(p_nac)  ~ "nacional",
                            TRUE           ~ "imputado"),
         valor_usd = prod_exp * precio_usd_t)

# ---- 5. Productividad provincial (USD por ha cosechada) ----
prod_prov <- base2 %>%
  group_by(provincia) %>%
  summarise(valor = sum(valor_usd, na.rm = TRUE),
            sup   = sum(sup_exp,  na.rm = TRUE), .groups = "drop") %>%
  mutate(prod_usd_ha   = valor / sup,
         provincia_nom = unname(prov_nombres[provincia])) %>%
  filter(!is.na(provincia_nom)) %>%
  arrange(desc(prod_usd_ha))

# ---- 6. Guardar ----
saveRDS(prod_prov, "productividad_usd_ha_2025.rds")
saveRDS(base2,     "base2_espac_precios_2025.rds")

# ---- 7. Revisiones (opcionales) ----
prod_prov %>% select(provincia_nom, prod_usd_ha, sup) %>% print(n = 25)

base2 %>% group_by(fuente) %>%                      # cobertura de precios
  summarise(sup = sum(sup_exp, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round(100 * sup / sum(sup), 1))

base2 %>% filter(fuente == "imputado") %>%          # cultivos imputados
  group_by(cultivo) %>% summarise(sup = sum(sup_exp, na.rm = TRUE)) %>%
  arrange(desc(sup)) %>% print(n = 15)

base2 %>% filter(extremo) %>% count(cultivo, sort = TRUE) %>% print(n = 10)

sens <- function(k) {                               # sensibilidad del precio imputado
  base2 %>%
    mutate(v = if_else(fuente == "imputado", valor_usd * k, valor_usd)) %>%
    group_by(provincia) %>%
    summarise(r = sum(v, na.rm = TRUE) / sum(sup_exp, na.rm = TRUE), .groups = "drop")
}
prod_prov %>%
  left_join(sens(0.5), by = "provincia") %>% rename(r_mitad = r) %>%
  left_join(sens(1.5), by = "provincia") %>% rename(r_150 = r) %>%
  mutate(var_baja = round(100 * (r_mitad / prod_usd_ha - 1), 1),
         var_alta = round(100 * (r_150  / prod_usd_ha - 1), 1)) %>%
  select(provincia_nom, prod_usd_ha, var_baja, var_alta) %>% print(n = 25)