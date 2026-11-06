# ============================================================
# 03_credito.R
# Crédito agrícola 2025 por provincia, con y sin flores
# Fuente: SIPA (BanEcuador + CFN; Superintendencias de Bancos y de EPS)
# Requiere: datos_modelo_2025.rds con 'sup' y 'sup_total' (01 y 01b_flores.R)
# Agrega:   credito_con_flores, credito_flores, credito_sin_flores,
#           credito_ha_sin_flores (sobre superficie sin flores),
#           credito_ha_con_flores (sobre superficie con flores)
# ============================================================
library(readxl)
library(tidyverse)
library(stringi)

setwd("D:/Documents/GITHUB/rendimiento_por_h")

f_pub  <- "D:/Downloads/credito-publico-agropecuario.xlsx"
f_priv <- "D:/Downloads/credito-privado-agropecuario.xlsx"
anio_ref <- "2025"

prov_nombres <- c("01"="Azuay","02"="Bolívar","03"="Cañar","04"="Carchi","05"="Cotopaxi",
                  "06"="Chimborazo","07"="El Oro","08"="Esmeraldas","09"="Guayas","10"="Imbabura",
                  "11"="Loja","12"="Los Ríos","13"="Manabí","14"="Morona Santiago","15"="Napo",
                  "16"="Pastaza","17"="Pichincha","18"="Tungurahua","19"="Zamora Chinchipe",
                  "20"="Galápagos","21"="Sucumbíos","22"="Orellana","23"="Santo Domingo","24"="Santa Elena")

norm <- function(x) stri_trans_general(str_to_upper(str_squish(x)), "Latin-ASCII")
prov_cod <- setNames(names(prov_nombres), norm(prov_nombres))

# ---- 1. Leer (la tabla empieza tras 5 filas de notas) ----
pub <- read_excel(f_pub, sheet = 1, skip = 5, col_types = "text")
names(pub) <- c("anio","mes","provincia","canton","sector","subsector",
                "prod_orig","prod_hom","genero","operaciones","valor_usd","institucion")

priv <- read_excel(f_priv, sheet = 1, skip = 5, col_types = "text")
names(priv) <- c("anio","mes","subsistema","provincia","canton","sector","subsector",
                 "prod_orig","prod_hom","genero","operaciones","valor_usd","fuente")

# ---- 2. Unir y limpiar ----
cred <- bind_rows(
  pub  %>% transmute(anio, provincia, sector, prod_hom, valor_txt = valor_usd, origen = "publico"),
  priv %>% transmute(anio, provincia, sector, prod_hom, valor_txt = valor_usd, origen = "privado")) %>%
  mutate(valor = suppressWarnings(as.numeric(valor_txt)))

cred %>% filter(is.na(valor)) %>% count(anio, valor_txt)     # solo deben ser encabezados sueltos

cred <- cred %>%
  filter(anio == anio_ref, !is.na(valor)) %>%
  mutate(sector_n = norm(sector), prod_n = norm(prod_hom),
         pn = norm(provincia),
         pn = if_else(str_detect(pn, "^SANTO DOMINGO"), "SANTO DOMINGO", pn),
         provincia = unname(prov_cod[pn]))

cred %>% count(sector_n, origen)                  # debe aparecer AGRICOLA en los dos
cred %>% filter(is.na(provincia)) %>% count(pn)   # debe salir vacío

# ---- 3. Agrícola por provincia: con y sin flores ----
credito_prov <- cred %>%
  filter(!is.na(provincia), sector_n == "AGRICOLA") %>%
  group_by(provincia) %>%
  summarise(credito_con_flores = sum(valor),
            credito_flores     = sum(valor[str_detect(prod_n, "^FLOR")]),
            credito_sin_flores = credito_con_flores - credito_flores,
            .groups = "drop")

# ---- 4. Unir y dividir cada crédito entre su propia superficie ----
base_modelo <- readRDS("datos_modelo_2025.rds")
stopifnot(all(c("sup", "sup_total") %in% names(base_modelo)))

datos <- base_modelo %>%
  select(-any_of(c("publico", "privado", "credito_total", "credito_ha", "credito_ha_flores",
                   setdiff(names(credito_prov), "provincia"),
                   "credito_ha_con_flores", "credito_ha_sin_flores"))) %>%
  left_join(credito_prov, by = "provincia") %>%
  mutate(across(c(credito_con_flores, credito_flores, credito_sin_flores), ~ replace_na(.x, 0)),
         credito_ha_sin_flores = credito_sin_flores / sup,
         credito_ha_con_flores = credito_con_flores / sup_total)

saveRDS(credito_prov, "credito_prov_2025.rds")
saveRDS(datos,        "datos_modelo_2025.rds")
write_csv(datos,      "datos_modelo_2025.csv")

# ---- 5. Revisiones ----
datos %>% transmute(provincia_nom, credito_flores = round(credito_flores),
                    sin_flores = round(credito_ha_sin_flores),
                    con_flores = round(credito_ha_con_flores)) %>%
  arrange(desc(con_flores)) %>% print(n = 25)
names(datos)