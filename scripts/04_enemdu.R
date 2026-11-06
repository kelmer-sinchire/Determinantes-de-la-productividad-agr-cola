# ============================================================
# 04_enemdu.R
# Pobreza rural y escolaridad rural por provincia, ENEMDU anual 2025
# Requiere: datos_modelo_2025.rds y la base ENEMDU (.sav o .dta) en la carpeta del proyecto
# Agrega:   n_rural, n_adultos, pobreza_rural, pobreza_ext_rural, educ_rural, sec_comp_rural
# ============================================================
library(haven)
library(tidyverse)
library(stringi)

setwd("D:/Documents/GITHUB/rendimiento_por_h")

# ---- 1. Leer la base ENEMDU de la carpeta ----
f_enemdu <- list.files(pattern = "(?i)enemdu.*\\.(dta|sav)$")
print(f_enemdu)
stopifnot(length(f_enemdu) == 1)

enemdu <- switch(tolower(tools::file_ext(f_enemdu)),
                 dta = read_dta(f_enemdu), sav = read_sav(f_enemdu))

nrow(enemdu)
enemdu %>% count(periodo, mes) %>% print(n = 15)       # deben estar los 12 meses

# ---- 2. Variables auxiliares ----
rural <- str_detect(as.character(as_factor(enemdu$area)), regex("rural", ignore_case = TRUE))
if (!any(rural, na.rm = TRUE)) rural <- as.numeric(enemdu$area) == 2

cod_de <- function(x, patron, defecto) {
  lab <- attr(x, "labels")
  cod <- if (is.null(lab)) numeric(0) else unname(lab[str_detect(names(lab), patron)])
  if (length(cod) != 1) defecto else cod
}
cod_pobre <- cod_de(enemdu$pobreza,  regex("^pobre$",       ignore_case = TRUE), 1)
cod_ext   <- cod_de(enemdu$epobreza, regex("^pobre extrem", ignore_case = TRUE), 1)
cat("Código pobre:", cod_pobre, "| código pobre extremo:", cod_ext, "\n")

nivel <- str_squish(stri_trans_general(str_to_lower(as.character(as_factor(enemdu$p10a))), "Latin-ASCII"))
p10b  <- as.numeric(enemdu$p10b)

# años de escolaridad por nivel de instrucción
anios <- case_when(
  str_detect(nivel, "ningun|alfabet|jardin")        ~ 0,
  str_detect(nivel, "primaria")                      ~ pmin(p10b, 6),
  str_detect(nivel, "basica")                        ~ pmin(p10b, 10),
  str_detect(nivel, "secundaria")                    ~ 6 + pmin(p10b, 6),
  str_detect(nivel, "media|bachillerato")            ~ 10 + pmin(p10b, 3),
  str_detect(nivel, "superior no|tecnic|tecnolog")   ~ 13 + pmin(p10b, 3),
  str_detect(nivel, "superior|univers")              ~ 13 + pmin(p10b, 7),
  str_detect(nivel, "post|maestr|doctor")            ~ 18 + pmin(p10b, 3),
  TRUE ~ NA_real_)

enem <- enemdu %>%
  transmute(provincia = sprintf("%02d", as.integer(prov)),
            rural     = rural,
            fexp      = fexp,
            edad      = p03,
            nivel     = nivel,
            anios     = anios,
            pobre     = if_else(is.na(pobreza),  NA_real_, as.numeric(as.numeric(pobreza)  == cod_pobre)),
            pobre_ext = if_else(is.na(epobreza), NA_real_, as.numeric(as.numeric(epobreza) == cod_ext)),
            adulto    = edad >= 25) %>%
  mutate(anios_ad = if_else(adulto, anios, NA_real_),
         sec_ad   = if_else(adulto & !is.na(anios), as.numeric(anios >= 12), NA_real_))

# conversión a años (adultos de 25 años o más): ningún nivel debe quedar sin años
enem %>% filter(adulto) %>%
  group_by(nivel) %>%
  summarise(n = n(), anios_prom = round(mean(anios, na.rm = TRUE), 1),
            sin_anios = sum(is.na(anios)), .groups = "drop") %>% print(n = 20)

# ---- 3. Indicadores rurales por provincia (con factor de expansión) ----
enemdu_prov <- enem %>%
  filter(rural, !is.na(provincia)) %>%
  group_by(provincia) %>%
  summarise(n_rural           = n(),
            n_adultos         = sum(adulto, na.rm = TRUE),
            pobreza_rural     = 100 * weighted.mean(pobre,     fexp, na.rm = TRUE),
            pobreza_ext_rural = 100 * weighted.mean(pobre_ext, fexp, na.rm = TRUE),
            educ_rural        = weighted.mean(anios_ad, fexp, na.rm = TRUE),
            sec_comp_rural    = 100 * weighted.mean(sec_ad, fexp, na.rm = TRUE),
            .groups = "drop")

# ---- 4. Unir con los datos del modelo y guardar ----
datos <- readRDS("datos_modelo_2025.rds") %>%
  select(-any_of(setdiff(names(enemdu_prov), "provincia"))) %>%
  left_join(enemdu_prov, by = "provincia")

saveRDS(enemdu_prov, "enemdu_prov_2025.rds")
saveRDS(datos,       "datos_modelo_2025.rds")
write_csv(datos,     "datos_modelo_2025.csv")

# ---- 5. Revisiones ----
datos %>% select(provincia_nom, n_rural, pobreza_rural, educ_rural, sec_comp_rural) %>%
  arrange(desc(pobreza_rural)) %>% mutate(across(where(is.double), ~ round(.x, 1))) %>% print(n = 25)

enem %>% filter(rural) %>%
  summarise(pobreza_rural_nacional = round(100 * weighted.mean(pobre, fexp, na.rm = TRUE), 1))

cat("Provincias sin datos de pobreza o educación:",
    sum(is.na(datos$pobreza_rural) | is.na(datos$educ_rural)), "\n")
names(datos)