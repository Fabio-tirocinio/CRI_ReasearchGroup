# =============================================================================
# SEZIONE 1 — Librerie e caricamento dati (invariata dall'originale)
# =============================================================================
library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(stats)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(sf)
library(viridisLite)
library(viridis)

MEF <- read_excel("Desktop/Materiale per ASMBI/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.xlsx")
ISTAT <- read.csv("~/Desktop/Materiale per ASMBI/POSAS_2022_it_Comuni 16.51.05.csv", sep=";")

MEF_clean <- MEF %>%
  select(
    -matches("Anno di imposta"),
    -matches("Reddito da fabbricati"),
    -matches("Reddito da partecipazione"),
    -matches("Reddito di spettanza dell'imprenditore"),
    -matches("Reddito imponibile"),
    -matches("Imposta netta"),
    -matches("Trattamento spettante"),
    -matches("Reddito imponibile addizionale"),
    -matches("Addizionale regionale"),
    -matches("Addizionale comunale"),
  )

ISTAT <- ISTAT %>%
  mutate(Comune = str_to_upper(Comune)) %>%
  mutate(
    fascia_eta = case_when(
      Età %in% 0:14   ~ "eta_0_14",
      Età %in% 15:24  ~ "eta_15_24",
      Età %in% 25:44  ~ "eta_25_44",
      Età %in% 45:64  ~ "eta_45_64",
      Età >= 65       ~ "eta_65_plus"
    )
  ) %>%
  group_by(Codice.comune, Comune, fascia_eta) %>%
  summarise(
    Maschi = sum(Totale.maschi, na.rm = TRUE),
    Femmine = sum(Totale.femmine, na.rm = TRUE),
    Totale = sum(Totale, na.rm = TRUE),
    .groups = "drop"
  )

ISTAT_wide <- ISTAT %>%
  pivot_wider(
    names_from = fascia_eta,
    values_from = c(Maschi, Femmine, Totale),
    values_fill = 0
  )

MEF_clean <- MEF_clean %>%
  rename(Codice.comune = `Codice Istat Comune`)

merged_data <- ISTAT_wide %>%
  inner_join(MEF_clean, by = "Codice.comune")

# Stima autonomi mancanti (celle oscurate MEF)
merged_data <- merged_data %>%
  mutate(
    Autonomi_stimati = `Numero contribuenti` -
      coalesce(`Reddito da pensione - Frequenza`, 0) -
      coalesce(`Reddito da lavoro dipendente e assimilati - Frequenza`, 0),
    Autonomi_privacy_flag = if_else(Autonomi_stimati > 0 & Autonomi_stimati < 4, TRUE, FALSE),
    `Reddito da lavoro autonomo (stimato) - Frequenza` = if_else(
      is.na(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`) &
        Autonomi_privacy_flag == TRUE,
      Autonomi_stimati,
      `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`
    )
  )


# =============================================================================
# PATCH 1 (NUOVO) — Merge dell'istruzione
# =============================================================================
quota_istruzione<- read.csv("~/Desktop/Materiale per ASMBI/quota_istruzione_comuni.csv")

merged_data <- merged_data %>%
  left_join(
    quota_istruzione %>%
      select(Codice.comune, quota_istruzione_0_Base,
             quota_istruzione_1_Medio, quota_istruzione_2_Alto),
    by = "Codice.comune"
  )

n_mancanti <- sum(is.na(merged_data$quota_istruzione_0_Base))
cat("Comuni senza corrispondenza istruzione:", n_mancanti, "su", nrow(merged_data), "\n")

# =============================================================================
# NESSUNA ESCLUSIONE (coerente con la tesi): lo script R originale non
# escludeva comuni per dati mancanti — M_i gestisce i buchi con na.rm=TRUE
# (media sulle componenti disponibili), P_i con l'imputazione automatica di
# PCA() sulla media di colonna (l'avviso "Missing values are imputed" è il
# comportamento atteso, non un errore). L'esclusione a 6.573 righe era stata
# fatta SOLO nel notebook Python separato, per il modello di regressione sul
# reddito (Capitolo 4) — non per M_i/P_i/C_i. Usiamo comunque la colonna
# "stimata" (non quella grezza) per ridurre i buchi dove possibile (es.
# Ingria, Autonomi_stimati=1, recuperato), lasciando che na.rm/PCA gestiscano
# il resto (es. Briga Alta, Autonomi_stimati=5, fuori dalla fascia 1-3).


# =============================================================================
# SEZIONE 2 — M_i, Indice a media semplice (versione DEFINITIVA, con log-transform,
# corrispondente a riga 424+ dell'originale — quella che hai validato "risultati
# migliori" rispetto alla prima bozza) + istruzione aggiunta (PATCH 2)
# =============================================================================
minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(NA_real_, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

merged_data_norm <- merged_data %>%
  mutate(
    n_contrib_log = log1p(`Numero contribuenti`),
    dip_log = log1p(`Reddito da lavoro dipendente e assimilati - Frequenza`),
    pens_log = log1p(`Reddito da pensione - Frequenza`),
    # Colonna stimata (Sezione 1): dopo il filtro corretto sopra, qui non ci
    # sono più NaN — recupera correttamente casi come Ingria (Autonomi_stimati=1)
    auto_log = log1p(`Reddito da lavoro autonomo (stimato) - Frequenza`)
  ) %>%
  mutate(
    n_contrib_norm = minmax_norm(n_contrib_log),
    dip_norm = minmax_norm(dip_log),
    pens_norm = minmax_norm(pens_log),
    auto_norm = minmax_norm(auto_log),
    # NUOVO: istruzione, già in [0,1], nessun log/minmax necessario
    edu_base_norm = quota_istruzione_0_Base,
    edu_alto_norm = quota_istruzione_2_Alto
  ) %>%
  mutate(
    n_contrib_inv = 1 - n_contrib_norm,
    dip_inv = 1 - dip_norm,
    pens_inv = 1 - pens_norm,
    auto_inv = 1 - auto_norm,
    # DECISIONE (coerente coi risultati Shapley: 0_Base aumenta il rischio,
    # 2_Alto lo diminuisce): edu_base NON invertita, edu_alto invertita
    edu_base_inv = edu_base_norm,
    edu_alto_inv = 1 - edu_alto_norm
  )

merged_data_norm <- merged_data_norm %>%
  mutate(
    n_comps_valid = rowSums(!is.na(select(., n_contrib_inv, dip_inv, pens_inv,
                                          auto_inv, edu_base_inv, edu_alto_inv)))
  )

merged_data_norm <- merged_data_norm %>%
  rowwise() %>%
  mutate(
    indice_privacy_simple = mean(c_across(c(n_contrib_inv, dip_inv, pens_inv,
                                            auto_inv, edu_base_inv, edu_alto_inv)),
                                 na.rm = TRUE)
  ) %>%
  ungroup()

rng_idx <- range(merged_data_norm$indice_privacy_simple, na.rm = TRUE)
merged_data_norm <- merged_data_norm %>%
  mutate(indice_privacy_simple_norm01 = (indice_privacy_simple - rng_idx[1]) / (rng_idx[2] - rng_idx[1]))

merged_data_norm <- merged_data_norm %>%
  mutate(
    low_info_flag = n_comps_valid < 5,  # aggiornato da <3: ora 6 componenti totali
    small_comune_flag = `Numero contribuenti` < 100
  )

# Controlli rapidi (facoltativi, tenuti dall'originale)
top20 <- merged_data_norm %>%
  arrange(desc(indice_privacy_simple)) %>%
  select(Codice.comune, Comune, `Numero contribuenti`, n_comps_valid, indice_privacy_simple) %>%
  head(20)
top20

merged_data_norm %>%
  filter(Comune == "BRIGA ALTA") %>%
  select(Comune, `Numero contribuenti`, n_comps_valid, indice_privacy_simple)

# =============================================================================
# SEZIONE 3 — P_i, Indice PCA (versione definitiva, riga 579+) + istruzione (PATCH 3)
# =============================================================================
merged_data_pca <- merged_data %>%
  mutate(
    n_contrib_log = log1p(`Numero contribuenti`),
    dip_log = log1p(`Reddito da lavoro dipendente e assimilati - Frequenza`),
    pens_log = log1p(`Reddito da pensione - Frequenza`),
    # Colonna stimata, coerente con la Sezione 2 (M_i)
    auto_log = log1p(`Reddito da lavoro autonomo (stimato) - Frequenza`),
    edu_base_log = quota_istruzione_0_Base,
    edu_alto_log = 1 - quota_istruzione_2_Alto
  )

z_vars <- merged_data_pca %>%
  select(n_contrib_log, dip_log, pens_log, auto_log, edu_base_log, edu_alto_log) %>%
  scale(center = TRUE, scale = TRUE)

z_vars_df <- as.data.frame(z_vars)
pca_res <- PCA(z_vars_df, scale.unit = FALSE, ncp = 4, graph = FALSE)

pca_var <- pca_res$eig
pca_var  # controlla la varianza spiegata da PC1 (era 92.67% nella versione a 4 variabili;
# con l'istruzione aggiunta il numero cambierà, controllalo)

merged_data_pca$indice_pca <- pca_res$ind$coord[, 1]
merged_data_pca$indice_pca <- 1 - merged_data_pca$indice_pca  # coerenza: 1 = rischio alto

rng_idx <- range(merged_data_pca$indice_pca, na.rm = TRUE)
merged_data_pca$indice_pca_norm01 <- (merged_data_pca$indice_pca - rng_idx[1]) / (rng_idx[2] - rng_idx[1])

merged_data_pca <- merged_data_pca %>%
  mutate(small_comune_flag = `Numero contribuenti` < 100)


# =============================================================================
# SEZIONE 4 — R_i (RISCRITTO, PATCH 4): pesi Shapley invece di feature importance
# =============================================================================
weights <- read.csv("~/Desktop/Materiale per ASMBI/feature_importances_SHAP_ri.csv")
weights_region <- read.csv("~/Desktop/Materiale per ASMBI/shap_medio_per_categoria.csv") %>%
  filter(variabile == "Region")

df <- read.csv("~/Desktop/Materiale per ASMBI/dataset_comunale_per_R.csv")

# --- Proxy comunali per le feature individuali senza colonna diretta ---
df <- df %>%
  mutate(
    Quota_Femmine = (Femmine_eta_0_14 + Femmine_eta_15_24 + Femmine_eta_25_44 +
                       Femmine_eta_45_64 + Femmine_eta_65_plus) /
      (Maschi_eta_0_14 + Maschi_eta_15_24 + Maschi_eta_25_44 +
         Maschi_eta_45_64 + Maschi_eta_65_plus +
         Femmine_eta_0_14 + Femmine_eta_15_24 + Femmine_eta_25_44 +
         Femmine_eta_45_64 + Femmine_eta_65_plus),
    Eta_media_stimata = (
      (Maschi_eta_0_14 + Femmine_eta_0_14) * 7 +
        (Maschi_eta_15_24 + Femmine_eta_15_24) * 20 +
        (Maschi_eta_25_44 + Femmine_eta_25_44) * 34 +
        (Maschi_eta_45_64 + Femmine_eta_45_64) * 54 +
        (Maschi_eta_65_plus + Femmine_eta_65_plus) * 75
    ) / (Maschi_eta_0_14 + Maschi_eta_15_24 + Maschi_eta_25_44 + Maschi_eta_45_64 +
           Maschi_eta_65_plus + Femmine_eta_0_14 + Femmine_eta_15_24 +
           Femmine_eta_25_44 + Femmine_eta_45_64 + Femmine_eta_65_plus)
  )



df <- df %>%
  rename(
    Employee_Income_Amount = Reddito_da_lavoro_dipendente_e_assimilati___Ammontare_in_euro,
    Employee_Income_Frequency = Reddito_da_lavoro_dipendente_e_assimilati___Frequenza,
    Pension_Income_Amount = Reddito_da_pensione___Ammontare_in_euro,
    Pension_Income_Frequency = Reddito_da_pensione___Frequenza
  )

df <- df %>%
  mutate(
    Reddito_medio_stimato = (Employee_Income_Amount + Pension_Income_Amount) /
      pmax(Employee_Income_Frequency + Pension_Income_Frequency, 1)
  )
df <- df %>%
  left_join(
    quota_istruzione %>% select(Codice.comune, quota_istruzione_0_Base, quota_istruzione_2_Alto),
    by = "Codice.comune"
  )

scale_if_needed <- function(x) {
  if (sd(x, na.rm = TRUE) > 0.01) return(scale(x)[, 1]) else return(x)
}

# --- Region: lookup diretto del valore SHAP medio (non scaling generico) ---
region_lookup <- setNames(weights_region$shap_medio, weights_region$categoria)
df$Regione_shap_diretto <- region_lookup[df$Regione]  # verifica nome colonna regione in df!
df$Regione_shap_diretto[is.na(df$Regione_shap_diretto)] <- mean(weights_region$shap_medio)
peso_region <- weights$importance_norm[weights$feature == "Region"]

# --- Education: media delle due quote scalate, stesso verso di M_i/P_i ---
df$edu_z <- scale_if_needed(df$quota_istruzione_0_Base) -
  scale_if_needed(df$quota_istruzione_2_Alto)
peso_education <- weights$importance_norm[weights$feature == "Education_Macro"]

# --- Componenti numeriche standard ---
df$Numero_contribuenti_z <- scale_if_needed(df$Numero_contribuenti)
df$Reddito_medio_stimato_z <- scale_if_needed(df$Reddito_medio_stimato)
df$Eta_media_stimata_z <- scale_if_needed(df$Eta_media_stimata)
df$Quota_Femmine_z <- scale_if_needed(df$Quota_Femmine)

# --- Assemblaggio finale di R_i ---
df$Indice_RF_privacy <- 0

w <- weights$importance_norm[weights$feature == "Numero_contribuenti"]
df$Indice_RF_privacy <- df$Indice_RF_privacy + w * (-df$Numero_contribuenti_z)  # invertito: comune piccolo = rischio alto

w <- weights$importance_norm[weights$feature == "Total_Gross_Income"]
df$Indice_RF_privacy <- df$Indice_RF_privacy + w * df$Reddito_medio_stimato_z  # non invertito: Shapley reddito+ = rischio+

w <- weights$importance_norm[weights$feature == "Age"]
df$Indice_RF_privacy <- df$Indice_RF_privacy + w * (-df$Eta_media_stimata_z)  # invertito: Shapley età+ = rischio-

w <- weights$importance_norm[weights$feature == "Gender"]
df$Indice_RF_privacy <- df$Indice_RF_privacy + w * df$Quota_Femmine_z  # segnale debole, non invertito

df$Indice_RF_privacy <- df$Indice_RF_privacy + peso_education * df$edu_z
df$Indice_RF_privacy <- df$Indice_RF_privacy + peso_region * df$Regione_shap_diretto

df$Indice_RF_privacy_norm <- (df$Indice_RF_privacy - min(df$Indice_RF_privacy, na.rm = TRUE)) /
  (max(df$Indice_RF_privacy, na.rm = TRUE) - min(df$Indice_RF_privacy, na.rm = TRUE))

df %>% arrange(desc(Indice_RF_privacy_norm)) %>% select(Comune, Indice_RF_privacy_norm) %>% head(10)
df %>% arrange(Indice_RF_privacy_norm) %>% select(Comune, Indice_RF_privacy_norm) %>% head(10)


# =============================================================================
# SEZIONE 5 — Assemblaggio di C_i (invariata dall'originale, righe 789-885)
# =============================================================================
comparison_df <- merged_data_norm %>%
  mutate(Codice.comune = as.character(Codice.comune)) %>%
  select(Codice.comune, Comune, indice_privacy_simple) %>%
  left_join(
    merged_data_pca %>%
      mutate(Codice.comune = as.character(Codice.comune)) %>%
      select(Codice.comune, indice_pca_norm01),
    by = "Codice.comune"
  ) %>%
  left_join(
    df %>%
      mutate(Codice.comune = as.character(Codice.comune)) %>%
      select(Codice.comune, Indice_RF_privacy_norm),
    by = "Codice.comune"
  )

missing_rf <- setdiff(merged_data_norm$Codice.comune, df$Codice.comune)
cat("Comuni senza corrispondenza in R_i:", length(missing_rf), "\n")

comparison_df$Indice_composito_misto <- NA
for (i in 1:nrow(comparison_df)) {
  w_simple <- 0.45
  w_pca <- 0.45
  w_rf <- 0.10
  rf_val <- comparison_df$Indice_RF_privacy_norm[i]
  if (is.na(rf_val)) {
    w_simple <- 0.5
    w_pca <- 0.5
    rf_val <- 0
  }
  comparison_df$Indice_composito_misto[i] <-
    w_simple * comparison_df$indice_privacy_simple[i] +
    w_pca * comparison_df$indice_pca_norm01[i] +
    w_rf * rf_val
}

# Top/bottom 10 — primo controllo dei risultati nuovi
comparison_df %>% arrange(desc(Indice_composito_misto)) %>% select(Comune, Indice_composito_misto) %>% head(10)
comparison_df %>% arrange(Indice_composito_misto) %>% select(Comune, Indice_composito_misto) %>% head(10)

# Valori per i 5 comuni piccoli, per la tabella nel paper (Sezione Geospatial)
comparison_df %>%
  filter(Comune %in% c("BRIGA ALTA", "MORTERONE", "PEDESINA", "MACRA", "INGRIA")) %>%
  select(Comune, Indice_composito_misto)

comparison_df %>%
  filter(Comune %in% c("BRIGA ALTA", "MORTERONE", "PEDESINA", "MACRA", "INGRIA")) %>%
  select(Comune, indice_privacy_simple, indice_pca_norm01, Indice_RF_privacy_norm, Indice_composito_misto)


# =============================================================================
# TEST DI SENSIBILITÀ — pesi alternativi (0.40/0.40/0.20), a confronto con
# quelli originali della tesi (0.45/0.45/0.10)
# =============================================================================
comparison_df$Indice_composito_alt <- NA
for (i in 1:nrow(comparison_df)) {
  w_simple_alt <- 0.40
  w_pca_alt <- 0.40
  w_rf_alt <- 0.20
  rf_val <- comparison_df$Indice_RF_privacy_norm[i]
  if (is.na(rf_val)) {
    w_simple_alt <- 0.5
    w_pca_alt <- 0.5
    rf_val <- 0
  }
  comparison_df$Indice_composito_alt[i] <-
    w_simple_alt * comparison_df$indice_privacy_simple[i] +
    w_pca_alt * comparison_df$indice_pca_norm01[i] +
    w_rf_alt * rf_val
}

# --- Confronto 1: i 5 comuni piccoli, pesi originali vs alternativi ---
comparison_df %>%
  filter(Comune %in% c("BRIGA ALTA", "MORTERONE", "PEDESINA", "MACRA", "INGRIA")) %>%
  select(Comune, Indice_composito_misto, Indice_composito_alt)

# --- Confronto 2: la classifica generale cambia? (correlazione di rango) ---
cor(comparison_df$Indice_composito_misto, comparison_df$Indice_composito_alt,
    method = "spearman", use = "complete.obs")

# --- Confronto 3: la top 20 assoluta resta la stessa? ---
top20_originale <- comparison_df %>% arrange(desc(Indice_composito_misto)) %>% pull(Comune) %>% head(20)
top20_alt <- comparison_df %>% arrange(desc(Indice_composito_alt)) %>% pull(Comune) %>% head(20)
cat("Comuni in comune tra le due top 20:", length(intersect(top20_originale, top20_alt)), "su 20\n")


# =============================================================================
# SEZIONE 6 — Mappe finali (versione DEFINITIVA, righe 2209-2274 dell'originale,
# "SCRIPT DEFINITIVO MAPPE - INDICE COMPOSITO MISTO (Per Tesi)")
# Nessuna modifica necessaria: legge da comparison_df, che ora ha il nuovo Ci
# =============================================================================
comuni_sf <- st_read("/Users/aleangeli/Desktop/Materiale per ASMBI/Limiti01012022/Com01012022/Com01012022_WGS84.shp")
province_sf <- st_read("/Users/aleangeli/Desktop/Materiale per ASMBI/Limiti01012022//ProvCM01012022/ProvCM01012022_WGS84.shp")
regioni_sf <- st_read("/Users/aleangeli/Desktop/Materiale per ASMBI/Limiti01012022/Reg01012022/Reg01012022_WGS84.shp")

comuni_sf <- comuni_sf %>% mutate(PRO_COM = sprintf("%06d", as.numeric(PRO_COM)))
comparison_df <- comparison_df %>% mutate(Codice.comune = sprintf("%06d", as.numeric(Codice.comune)))
regioni_sf <- regioni_sf %>% mutate(COD_REG = as.character(COD_REG))
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))

comuni_map_misto <- comuni_sf %>%
  left_join(comparison_df %>% select(Codice.comune, Comune, Indice_composito_misto),
            by = c("PRO_COM" = "Codice.comune")) %>%
  filter(!is.na(Indice_composito_misto))

df_prov <- comuni_sf %>%
  st_drop_geometry() %>%
  left_join(comparison_df %>% select(Codice.comune, Indice_composito_misto),
            by = c("PRO_COM" = "Codice.comune")) %>%
  group_by(COD_PROV) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE))

df_reg <- comuni_sf %>%
  st_drop_geometry() %>%
  left_join(comparison_df %>% select(Codice.comune, Indice_composito_misto),
            by = c("PRO_COM" = "Codice.comune")) %>%
  group_by(COD_REG) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE)) %>%
  mutate(COD_REG = as.character(COD_REG))

province_map_misto <- province_sf %>% left_join(df_prov, by = "COD_PROV")
regioni_map_misto <- regioni_sf %>% left_join(df_reg, by = "COD_REG")

# A. Comunale (pagina intera, legenda in alto) — QUESTA è la mappa nazionale
#    per la Sezione "Geospatial Risk Analysis" del paper
plot_comunale_misto <- ggplot() +
  geom_sf(data = comuni_map_misto, aes(fill = Indice_composito_misto), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Privacy Risk Level",
                       breaks = c(0, 0.5, 1), labels = c("Low (0.0)", "0.5", "High (1.0)"),
                       guide = guide_colorbar(direction = "horizontal", barwidth = 12, barheight = 0.5,
                                              title.position = "top", title.hjust = 0.5)) +
  labs(title = "Mixed Privacy Risk Index: Municipal Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "top")


plot_comunale_misto

plot_provinciale_misto <- ggplot() +
  geom_sf(data = province_map_misto, aes(fill = Indice_composito_misto), color = "white", size = 0.1) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0.3, 1), name = "Privacy Risk Level",
                       guide = guide_colorbar(direction = "horizontal", barwidth = 12, barheight = 0.5,
                                              title.position = "top", title.hjust = 0.5)) +
  labs(title = "Mixed Privacy Risk Index: Provincial Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "top")

plot_regionale_misto <- ggplot() +
  geom_sf(data = regioni_map_misto, aes(fill = Indice_composito_misto), color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0.5, 1), name = "Privacy Risk Level",
                       guide = guide_colorbar(direction = "horizontal", barwidth = 12, barheight = 0.5,
                                              title.position = "top", title.hjust = 0.5)) +
  labs(title = "Mixed Privacy Risk Index: Regional Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "top")

print(plot_comunale_misto)
print(plot_provinciale_misto)
print(plot_regionale_misto)

# Salvataggio per il paper (LaTeX)
# Salvataggio corretto per ASMBI (Formato EPS e 800 DPI)
ggsave("/Users/aleangeli/Desktop/Materiale per ASMBI/ci_map_national.eps", 
       plot_comunale_misto, width = 10, height = 8, dpi = 800, device = "eps")

ggsave("/Users/aleangeli/Desktop/Materiale per ASMBI/ci_map_provincial.eps", 
       plot_provinciale_misto, width = 8, height = 6, dpi = 800, device = "eps")

ggsave("/Users/aleangeli/Desktop/Materiale per ASMBI/ci_map_regional.eps", 
       plot_regionale_misto, width = 8, height = 6, dpi = 800, device = "eps")

# Esportazione dati finali
write.csv(comparison_df, "/Users/aleangeli/Desktop/Materiale per ASMBI/Ci_finale_tutti_comuni.csv", row.names = FALSE)
cat("✅ Script completato. File esportati: mappe PNG + Ci_finale_tutti_comuni.csv\n")

