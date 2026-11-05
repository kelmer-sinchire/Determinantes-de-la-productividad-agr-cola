# ============================================================
# 07_base_estudio.R
# Base final del estudio (23 provincias, 2025) + diccionario de variables
# Requiere: datos_modelo_2025.rds con las columnas de los scripts 01 a 05
# Guarda:   modelo_2025.rds, modelo_2025.csv, diccionario_variables_2025.csv
# ============================================================
library(tidyverse)

setwd("D:/Documents/GITHUB/rendimiento_por_h")
datos <- readRDS("datos_modelo_2025.rds")

necesarias <- c("provincia", "provincia_nom",
                "prod_usd_ha_flores", "prod_usd_ha",
                "pct_riego_flores", "pct_riego",
                "credito_ha_con_flores", "credito_ha_sin_flores",
                "km_100km2", "km_100km2_arterial", "km_100km2_colectora",
                "pobreza_rural", "educ_rural")
faltan <- setdiff(necesarias, names(datos))
if (length(faltan)) stop("Faltan columnas: ", paste(faltan, collapse = ", "),
                         ". Corre antes los scripts 01 a 05.")

# ---- 1. Base del estudio ----
# Versión principal = con flores. Versión de chequeo = sin flores (sufijo _sf).
modelo <- datos %>%
  transmute(provincia, provincia_nom,
            productividad_usd_ha    = prod_usd_ha_flores,
            riego_pct               = pct_riego_flores,
            credito_usd_ha          = credito_ha_con_flores,
            productividad_usd_ha_sf = prod_usd_ha,
            riego_pct_sf            = pct_riego,
            credito_usd_ha_sf       = credito_ha_sin_flores,
            vias_km_100km2          = km_100km2,
            vias_arterial_km_100km2 = km_100km2_arterial,
            vias_colectora_km_100km2 = km_100km2_colectora,
            pobreza_rural_pct       = pobreza_rural,
            escolaridad_rural_anios = educ_rural) %>%
  arrange(provincia)

stopifnot(nrow(modelo) == 23)
cat("Valores faltantes por variable:\n"); print(colSums(is.na(modelo)))

# ---- 2. Diccionario ----
diccionario <- tribble(
  ~variable, ~descripcion, ~unidad, ~rol, ~que_explica, ~relacion_esperada, ~fuente, ~nota,
  "provincia", "Código de provincia", "Código de 2 dígitos", "Identificador", "—", "—", "INEC (DPA)", "Galápagos no está en la ESPAC: son 23 provincias",
  "provincia_nom", "Nombre de la provincia", "Texto", "Identificador", "—", "—", "INEC (DPA)", "",
  "productividad_usd_ha", "Valor bruto de la producción agrícola por hectárea cosechada, incluidas flores", "USD por ha cosechada", "Dependiente (principal)", "Rendimiento económico de la tierra agrícola de la provincia", "—", "ESPAC 2025 (INEC); precios al productor 2025 (SIPA-MAG); flores con precio por tallo calibrado con exportaciones del BCE", "Producción topada en el p99 de rendimiento por cultivo; precio provincial, luego nacional, luego medio del tipo",
  "productividad_usd_ha_sf", "Igual que la anterior, sin flores", "USD por ha cosechada", "Dependiente (chequeo)", "Igual que la anterior", "—", "ESPAC 2025 (INEC); SIPA-MAG", "Solo cultivos de cpnac y ctnac",
  "riego_pct", "Porcentaje de la superficie cosechada que tiene riego, incluidas flores", "% (0 a 100)", "Independiente", "Disponibilidad de agua controlada: estabiliza y eleva el rendimiento", "Positiva", "ESPAC 2025 (INEC)", "Vacío en la pregunta se trata como sin riego; la superficie regada no supera a la cosechada. Posible causalidad inversa",
  "riego_pct_sf", "Igual que la anterior, sin flores", "% (0 a 100)", "Independiente (chequeo)", "Igual que la anterior", "Positiva", "ESPAC 2025 (INEC)", "",
  "credito_usd_ha", "Crédito agrícola otorgado en 2025 (público y privado) por hectárea cosechada, incluidas flores", "USD por ha cosechada", "Independiente", "Acceso a financiamiento para insumos, semillas y tecnología", "Positiva", "SIPA: BanEcuador y CFN; Superintendencia de Bancos y SEPS. Superficie de la ESPAC", "Es un flujo del año, no saldo. En Pichincha puede reflejar la sede de las instituciones",
  "credito_usd_ha_sf", "Igual que la anterior, sin flores", "USD por ha cosechada", "Independiente (chequeo)", "Igual que la anterior", "Positiva", "SIPA; ESPAC 2025", "Crédito sin flores sobre superficie sin flores",
  "vias_km_100km2", "Kilómetros de red vial estatal por cada 100 km² de provincia (arterial más colectora)", "km por 100 km²", "Independiente", "Accesibilidad a mercados e insumos y menor costo de transporte", "Positiva", "MTOP, inventario 2024; áreas oficiales del INEC", "No incluye caminos vecinales. Usar una sola de las tres medidas viales por regresión",
  "vias_arterial_km_100km2", "Solo vías arteriales (troncales)", "km por 100 km²", "Independiente (alternativa)", "Conexión con los grandes centros", "Positiva", "MTOP 2024; INEC", "",
  "vias_colectora_km_100km2", "Solo vías colectoras (recogen la producción zonal)", "km por 100 km²", "Independiente (alternativa)", "Acceso desde las zonas productivas a las troncales", "Positiva", "MTOP 2024; INEC", "Zamora Chinchipe y Pastaza no tienen colectoras",
  "pobreza_rural_pct", "Porcentaje de la población rural en pobreza por ingresos", "% de personas", "Control socioeconómico", "Capacidad de inversión y condiciones de vida de los productores", "Negativa", "ENEMDU anual 2025 (INEC)", "Estimada con factor de expansión",
  "escolaridad_rural_anios", "Años promedio de estudio de las personas rurales de 25 años o más", "Años", "Control (capital humano)", "Adopción de técnicas y gestión de la finca", "Positiva", "ENEMDU anual 2025 (INEC)", "Años calculados desde nivel de instrucción y año aprobado")

stopifnot(setequal(names(modelo), diccionario$variable))

# ---- 3. Guardar ----
saveRDS(modelo, "modelo_2025.rds")
write_excel_csv(modelo, "modelo_2025.csv")
write_excel_csv(diccionario, "diccionario_variables_2025.csv")

# ---- 4. Revisión rápida ----
summary(modelo %>% select(-provincia, -provincia_nom))
modelo %>% select(productividad_usd_ha, riego_pct, credito_usd_ha, vias_km_100km2,
                  pobreza_rural_pct, escolaridad_rural_anios) %>%
  cor(use = "complete.obs") %>% round(2)