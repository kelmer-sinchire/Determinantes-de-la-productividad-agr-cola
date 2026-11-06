# ============================================================
# 07_base_estudio.R
# Base final del estudio (23 provincias, 2025) + diccionario de variables
# Requiere: datos_modelo_2025.rds generado con los scripts 01 a 05
# Guarda:   modelo_2025.rds, modelo_2025.csv, diccionario_variables_2025.csv
# ============================================================
library(tidyverse)

setwd("D:/Documents/GITHUB/rendimiento_por_h")
datos <- readRDS("datos_modelo_2025.rds")

necesarias <- c("provincia", "provincia_nom",
                "prod_usd_ha_flores", "pct_riego_flores", "credito_ha_con_flores",
                "km_100km2", "km_100km2_arterial", "km_100km2_colectora",
                "pobreza_rural", "educ_rural")
faltan <- setdiff(necesarias, names(datos))
if (length(faltan)) stop("Faltan columnas: ", paste(faltan, collapse = ", "),
                         ". Corre antes los scripts 01 a 05.")

# ---- 1. Base del estudio ----
modelo <- datos %>%
  transmute(provincia, provincia_nom,
            productividad_usd_ha     = prod_usd_ha_flores,
            riego_pct                = pct_riego_flores,
            credito_usd_ha           = credito_ha_con_flores,
            vias_km_100km2           = km_100km2,
            vias_arterial_km_100km2  = km_100km2_arterial,
            vias_colectora_km_100km2 = km_100km2_colectora,
            pobreza_rural_pct        = pobreza_rural,
            escolaridad_rural_anios  = educ_rural) %>%
  arrange(provincia)

stopifnot(nrow(modelo) == 23)
cat("Valores faltantes por variable:\n"); print(colSums(is.na(modelo)))

# ---- 2. Diccionario ----
diccionario <- tribble(
  ~variable, ~descripcion, ~unidad, ~rol, ~que_explica, ~relacion_esperada, ~fuente, ~nota,
  "provincia", "Código de provincia", "Código de 2 dígitos", "Identificador", "—", "—", "INEC (DPA)", "Galápagos no está en la ESPAC: son 23 provincias",
  "provincia_nom", "Nombre de la provincia", "Texto", "Identificador", "—", "—", "INEC (DPA)", "",
  "productividad_usd_ha", "Valor bruto de la producción por hectárea cosechada, incluidas flores", "USD por ha cosechada", "Dependiente", "Rendimiento económico de la tierra agrícola de la provincia", "—", "ESPAC 2025 (INEC); precios al productor 2025 (SIPA-MAG); flores con precio por tallo calibrado con exportaciones del BCE", "Producción topada en el p99 de rendimiento por cultivo; precio provincial, luego nacional, luego medio del tipo",
  "riego_pct", "Porcentaje de la superficie cosechada con riego, incluidas flores", "% (0 a 100)", "Independiente", "Disponibilidad de agua controlada: estabiliza y eleva el rendimiento", "Positiva", "ESPAC 2025 (INEC)", "Vacío en la pregunta se trata como sin riego; la regada no supera a la cosechada. Posible causalidad inversa",
  "credito_usd_ha", "Crédito agrícola otorgado en 2025 (público y privado) por hectárea cosechada, incluidas flores", "USD por ha cosechada", "Independiente", "Acceso a financiamiento para insumos, semillas y tecnología", "Positiva", "SIPA: BanEcuador y CFN; Superintendencia de Bancos y SEPS; superficie de la ESPAC", "Es un flujo del año, no saldo. En Pichincha puede reflejar la sede de las instituciones",
  "vias_km_100km2", "Kilómetros de red vial estatal (arterial más colectora) por cada 100 km² de provincia", "km por 100 km²", "Independiente", "Accesibilidad a mercados e insumos y menor costo de transporte", "Positiva", "MTOP, inventario 2024; áreas oficiales del INEC", "No incluye caminos vecinales. Es la suma de las dos siguientes",
  "vias_arterial_km_100km2", "Solo vías arteriales (troncales)", "km por 100 km²", "Independiente (alternativa)", "Conexión con los grandes centros de consumo", "Positiva", "MTOP 2024; INEC", "Ya está contenida en vias_km_100km2",
  "vias_colectora_km_100km2", "Solo vías colectoras (recogen la producción de cada zona)", "km por 100 km²", "Independiente (alternativa)", "Acceso desde las zonas productivas a las troncales", "Positiva", "MTOP 2024; INEC", "Zamora Chinchipe y Pastaza no tienen colectoras. Ya está contenida en vias_km_100km2",
  "pobreza_rural_pct", "Porcentaje de la población rural en pobreza por ingresos", "% de personas", "Control", "Capacidad de inversión y condiciones de vida de los productores", "Negativa", "ENEMDU anual 2025 (INEC)", "Estimada con factor de expansión",
  "escolaridad_rural_anios", "Años promedio de estudio de las personas rurales de 25 años o más", "Años", "Control", "Adopción de técnicas y gestión de la finca", "Positiva", "ENEMDU anual 2025 (INEC)", "Años calculados desde nivel de instrucción y año aprobado")

stopifnot(setequal(names(modelo), diccionario$variable))

# ---- 3. Guardar ----
saveRDS(modelo, "modelo_2025.rds")
write_excel_csv(modelo, "modelo_2025.csv")
write_excel_csv(diccionario, "diccionario_variables_2025.csv")

# ---- 4. Revisión rápida ----
summary(modelo %>% select(-provincia, -provincia_nom))
modelo %>% select(-provincia, -provincia_nom) %>% cor(use = "complete.obs") %>% round(2)