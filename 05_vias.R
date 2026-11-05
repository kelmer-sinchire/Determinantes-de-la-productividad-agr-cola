# ============================================================
# 05_vias.R
# Densidad de red vial estatal por provincia (km por 100 km2), MTOP 2024
# Variables: km_100km2 (total), km_100km2_arterial, km_100km2_colectora
# Requiere: datos_modelo_2025.rds (scripts anteriores)
# Guarda:   vias_prov_2025.rds, datos_modelo_2025.rds y datos_modelo_2025.csv
# ============================================================
library(readxl)
library(tidyverse)
library(stringi)

setwd("D:/Documents/GITHUB/rendimiento_por_h")
f_vial <- "D:/Downloads/mtop_rve_km_2024.xls (1).xls"

# TRUE descuenta el tramo "Puente Internacional Rumichaca - Entrada Tulcán" (Carchi, 2,4 km),
# que parece repetir al tramo "Rumichaca - Entrada Tulcán". FALSE usa la cifra oficial completa.
descontar_carchi <- TRUE

prov_nombres <- c("01"="Azuay","02"="Bolívar","03"="Cañar","04"="Carchi","05"="Cotopaxi",
                  "06"="Chimborazo","07"="El Oro","08"="Esmeraldas","09"="Guayas","10"="Imbabura",
                  "11"="Loja","12"="Los Ríos","13"="Manabí","14"="Morona Santiago","15"="Napo",
                  "16"="Pastaza","17"="Pichincha","18"="Tungurahua","19"="Zamora Chinchipe",
                  "20"="Galápagos","21"="Sucumbíos","22"="Orellana","23"="Santo Domingo","24"="Santa Elena")

norm <- function(x) stri_trans_general(str_to_upper(str_squish(x)), "Latin-ASCII")
prov_cod <- setNames(names(prov_nombres), norm(prov_nombres))
a_codigo <- function(x) {
  pn <- norm(x)
  pn <- if_else(str_detect(pn, "^SANTO DOMINGO"), "SANTO DOMINGO", pn)
  unname(prov_cod[pn])
}

# ---- 1. Kilómetros por provincia y jerarquía, solo 2024 ----
vial <- read_excel(f_vial, sheet = 1, col_types = "text") %>% select(1:9)
names(vial) <- c("anio","provincia","tramo","codigo","jerarquia","km","ancho","carriles","estado")

vial24 <- vial %>%
  mutate(km = suppressWarnings(as.numeric(km))) %>%
  filter(anio == "2024", !is.na(km))

if (descontar_carchi) {
  n0 <- nrow(vial24)
  vial24 <- vial24 %>%
    filter(!(norm(provincia) == "CARCHI" &
               str_detect(norm(tramo), "^PUENTE INTERNACIONAL RUMICHACA - ENTRADA TULCAN")))
  cat("Tramos descontados por solape (Carchi):", n0 - nrow(vial24), "\n")
}

cat("Tramos 2024:", nrow(vial24), "| km totales:", round(sum(vial24$km)), "\n")
vial24 %>% count(jerarquia = str_to_upper(str_squish(jerarquia)))   # ARTERIAL y COLECTORA

km_prov <- vial24 %>%
  mutate(provincia = a_codigo(provincia),
         jer = str_to_upper(str_squish(jerarquia))) %>%
  group_by(provincia) %>%
  summarise(km_vial      = sum(km),
            km_arterial  = sum(km[jer == "ARTERIAL"]),
            km_colectora = sum(km[jer == "COLECTORA"]),
            n_tramos     = n(), .groups = "drop")

km_prov %>% filter(is.na(provincia))           # debe salir vacío

# ---- 2. Área de cada provincia (INEC, capa oficial de cantones) ----
url_area <- paste0("https://services7.arcgis.com/iFGeGXTAJXnjq0YN/ArcGIS/rest/services/",
                   "Cantones_del_Ecuador/FeatureServer/0/query?where=1%3D1",
                   "&outFields=DPA_DESPRO,AREA_KM2&returnGeometry=false&f=json")

area_cant <- jsonlite::fromJSON(url_area)$features$attributes
stopifnot(nrow(area_cant) > 100)
cat("Cantones leídos:", nrow(area_cant), "\n")

area_prov <- area_cant %>%
  mutate(provincia = a_codigo(DPA_DESPRO)) %>%
  filter(!is.na(provincia)) %>%
  group_by(provincia) %>%
  summarise(area_km2 = sum(AREA_KM2, na.rm = TRUE), .groups = "drop")

# ---- 3. Densidad vial ----
vias_prov <- km_prov %>%
  inner_join(area_prov, by = "provincia") %>%
  mutate(km_100km2           = 100 * km_vial      / area_km2,
         km_100km2_arterial  = 100 * km_arterial  / area_km2,
         km_100km2_colectora = 100 * km_colectora / area_km2)

# ---- 4. Unir con los datos del modelo y guardar ----
datos <- readRDS("datos_modelo_2025.rds") %>%
  select(-any_of(setdiff(names(vias_prov), "provincia"))) %>%
  left_join(vias_prov, by = "provincia")

saveRDS(vias_prov, "vias_prov_2025.rds")
saveRDS(datos,     "datos_modelo_2025.rds")
write_csv(datos,   "datos_modelo_2025.csv")

# ---- 5. Revisiones ----
datos %>% transmute(provincia_nom, km_vial = round(km_vial), km_arterial = round(km_arterial),
                    km_colectora = round(km_colectora), area_km2 = round(area_km2),
                    km_100km2 = round(km_100km2, 1)) %>%
  arrange(desc(km_100km2)) %>% print(n = 25)

cat("Provincias con datos viales:", sum(!is.na(datos$km_100km2)), "de", nrow(datos), "\n")
cat("Área total de las provincias (km2):", round(sum(area_prov$area_km2)), "\n")
names(datos)