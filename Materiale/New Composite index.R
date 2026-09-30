#####################  DATA CLEANING AND DATA MANIPULATION ##################### 

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

# importazione dataset ministero delle finanza

MEF <-read_excel("Desktop/Tirocinio/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.xlsx")
View(MEF)  


# importazione dataset Demografiche istat

ISTAT <- read.csv("~/Desktop/Tirocinio/POSAS_2022_it_Comuni.csv", sep=";")

View(ISTAT)


# preparazione datset MEF

# seleziono solo le varibaili di interesse 

summarize(MEF)

colnames(MEF)

# Elimina solo le colonne dei redditi che non ti interessano
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

# Controllo
glimpse(MEF_clean)

View(MEF_clean)


glimpse(ISTAT)

# Trasforma i nomi dei comuni in maiuscolo
ISTAT<- ISTAT %>%
  mutate(Comune = str_to_upper(Comune))

# 1. Creo le classi
ISTAT<- ISTAT%>%
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


head(ISTAT)

# 2. Porto le fasce d'età da riga a colonna
ISTAT_wide <- ISTAT %>%
  pivot_wider(
    names_from = fascia_eta,
    values_from = c(Maschi, Femmine, Totale),
    values_fill = 0
  )

head(ISTAT_wide)

View(MEF_clean)

glimpse(MEF_clean)

# controllo NA in MEF_clean

sapply(MEF_clean, function(x) sum(is.na(x)))

# Seleziona solo le colonne di reddito
col_redditi <- grep("Reddito", names(MEF_clean), value = TRUE)

# Filtra i record che hanno almeno un valore diverso da 0 e non NA
MEF_clean_filtered <- MEF_clean[rowSums(is.na(MEF_clean[col_redditi]) | MEF_clean[col_redditi] == 0) < length(col_redditi), ]

sapply(MEF_clean_filtered, function(x) sum(is.na(x)))

# non ci sono sttai cambiamenti quindi continuo a usare MEF_clean 

glimpse(MEF_clean)

glimpse(ISTAT_wide)


# Rinomino la colonna per avere lo stesso nome in entrambi i dataset
MEF_clean <- MEF_clean %>%
  rename(Codice.comune = `Codice Istat Comune`)

# Merge dei due dataset
merged_data <- ISTAT_wide %>%
  inner_join(MEF_clean, by = "Codice.comune")

View(merged_data)

glimpse(merged_data)

#  ISTAT_wide ha 7904 osservazioni e MEF_clean ne ha 7905

colnames(MEF_clean)

colnames(ISTAT_wide)

# Trova i codici mancanti in ISTAT_wide
missing_code <- setdiff(MEF_clean$Codice.comune, ISTAT_wide$Codice.comune)

# Mostra il comune corrispondente in MEF_clean
MEF_clean %>% filter(Codice.comune %in% missing_code)

# Regione denominata Mancante/errata quindi il merge è andato a buon fine 


# creo la variabile per stimare il numero di autonomi nei piccoli comuni 

# lavoro fatto in excel con inferenza logica (magari tengo questo su R)

merged_data <- merged_data %>%
  mutate(
    # Stima del numero di autonomi mancanti
    Autonomi_stimati = `Numero contribuenti` -
      coalesce(`Reddito da pensione - Frequenza`, 0) -
      coalesce(`Reddito da lavoro dipendente e assimilati - Frequenza`, 0),
    
    # Se la differenza è compresa tra 1 e 3, probabile oscuramento per privacy
    Autonomi_privacy_flag = if_else(Autonomi_stimati > 0 & Autonomi_stimati < 4, TRUE, FALSE),
    
    # Sostituzione dei NA solo se plausibilmente oscurati
    `Reddito da lavoro autonomo (stimato) - Frequenza` = if_else(
      is.na(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`) &
        Autonomi_privacy_flag == TRUE,
      Autonomi_stimati,
      `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`
    )
  )

# controllo quanti comuni ora hanno anche il nuemro di lavoratori autonomi aggiornato 

sum(merged_data$Autonomi_privacy_flag, na.rm = TRUE)

View(merged_data)

# analisi 5 comuni piccoli da collegare con analisi dinamica e grafiche fatte nel file zonal statistics 

# Filtrare i comuni di interesse
comuni_piccoli <- merged_data %>%
  filter(Comune %in% c("BRIGA ALTA", "MORTERONE", "PEDESINA", "MACRA", "INGRIA")) %>%
  select(Comune, Totale_eta_0_14:Totale_eta_65_plus, `Numero contribuenti`,
         `Reddito da lavoro dipendente e assimilati - Frequenza`,
         `Reddito da pensione - Frequenza`,
         `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`,
         Autonomi_privacy_flag) %>%
  mutate(percentuale_contribuenti = `Numero contribuenti` / 
           (Totale_eta_0_14 + Totale_eta_15_24 + Totale_eta_25_44 + Totale_eta_45_64 + Totale_eta_65_plus))

# Visualizzare i risultati
View(comuni_piccoli)


# commento Monterone (guarda quaderno blu con lavoro Lido) lo stesso anche per Pedesina


colnames(merged_data)

# analizzo le varibaili con nuemero di contirenti e poi frequenza per i vari redditi

# controllo NA nelle frequenze dei redditi 

merged_data %>%
  summarise(
    NA_dipendenti = sum(is.na(`Reddito da lavoro dipendente e assimilati - Frequenza`)),
    NA_pensione = sum(is.na(`Reddito da pensione - Frequenza`)),
    NA_autonomi = sum(is.na(`Reddito da lavoro autonomo (stimato) - Frequenza`))
  )

# come mi immaginavo erano tutte negli autonomi

# imputiamo i valori dei 16 comuni calcoalti prima nella frequanza dei lavoratori autonomi 

merged_data %>%
  filter(Autonomi_privacy_flag == TRUE) %>%
  select(Comune, 
         `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`,
         Autonomi_stimati,
         Autonomi_privacy_flag)

merged_data %>%
  summarise(
    NA_dipendenti = sum(is.na(`Reddito da lavoro dipendente e assimilati - Frequenza`)),
    NA_pensione = sum(is.na(`Reddito da pensione - Frequenza`)),
    NA_autonomi = sum(is.na(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`))
  )

# 1324 record hanno ancora valori NA nella frequenza dei redditi autonomi 

# Filtra i comuni con NA nei redditi autonomi
NA_autonomi_records <- merged_data %>%
  filter(is.na(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`))

# Visualizza i primi record o tutti
View(NA_autonomi_records)

summary(NA_autonomi_records)

# ci sono anche comuni piccoli nel dataset con gli NA analizzo meglio questo dataset per vedere come muovermi

# Numero di comuni con meno di 100 contribuenti
comuni_sotto_100 <- merged_data %>% 
  filter(`Numero contribuenti` < 100) %>% 
  nrow() 

comuni_sotto_100

summary(merged_data$`Numero contribuenti`)

#analisi Briga Alta

merged_data %>% 
  filter(Comune == "BRIGA ALTA") %>% 
  select(`Numero contribuenti`, `Reddito da lavoro dipendente e assimilati - Frequenza`, 
         `Reddito da pensione - Frequenza`, 
         `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)

# mancano 5 contribuenti e possono essere da 1 a 3 autonomi 


# analisi Moncenisio

merged_data %>% 
  filter(Comune == "MONCENISIO") %>% 
  select(`Numero contribuenti`, `Reddito da lavoro dipendente e assimilati - Frequenza`, 
         `Reddito da pensione - Frequenza`, 
         `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)

# mancano 9 contribuenti. Gli autonomi sono da 1-3 


# selezioniamo le variabili originali
vars_freq <- merged_data %>%
  select(`Numero contribuenti`,
         `Reddito da lavoro dipendente e assimilati - Frequenza`,
         `Reddito da pensione - Frequenza`,
         `Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)

# calcolo matrice di correlazione ignorando i NA
corr_matrix <- cor(vars_freq, use = "pairwise.complete.obs")

##################### INDICE COMPOSITO #####################

# analisi delle varibiabili che potrebbero essere id mio interesse nel calcolo dei vari indici 

############ media semplice ############

# variabili scelte: numeor contirbuenti, reddito da lavoro dipendente (freq), reddito da lavoro pensione (freq), reddito da lavoro autonomo (freq)

vars_freq <- c(
  "Numero contribuenti",
  "Reddito da lavoro dipendente e assimilati - Frequenza",
  "Reddito da pensione - Frequenza",
  "Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza"
)

# controllo NA

merged_data %>%
  summarise(across(all_of(vars_freq), ~ sum(is.na(.)), .names = "NA_{col}"))


# funzione min-max

minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(NA_real_, length(x)))    # se costante, ritorna NA per evitare divisione per 0
  }
  (x - rng[1]) / (rng[2] - rng[1])
}


merged_data_norm <- merged_data %>%
  mutate(
    n_contrib_norm = minmax_norm(`Numero contribuenti`),
    dip_norm = minmax_norm(`Reddito da lavoro dipendente e assimilati - Frequenza`),
    pens_norm = minmax_norm(`Reddito da pensione - Frequenza`),
    auto_norm = minmax_norm(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)
  ) %>%
  # Inversione: valori bassi = rischio alto
  mutate(
    n_contrib_inv = 1 - n_contrib_norm,
    dip_inv = 1 - dip_norm,
    pens_inv = 1 - pens_norm,
    auto_inv = 1 - auto_norm
  )


merged_data_norm <- merged_data_norm %>%
  mutate(
    n_comps_valid = rowSums(!is.na(select(., n_contrib_inv, dip_inv, pens_inv, auto_inv)))
  )

# creazione indice composito 

merged_data_norm <- merged_data_norm %>%
  rowwise() %>%
  mutate(
    indice_privacy_simple = mean(c_across(c(n_contrib_inv, dip_inv, pens_inv, auto_inv)), na.rm = TRUE)
  ) %>%
  ungroup()

# normalizzazione 0-1

rng_idx <- range(merged_data_norm$indice_privacy_simple, na.rm = TRUE)
if (diff(rng_idx) > 0) {
  merged_data_norm <- merged_data_norm %>%
    mutate(indice_privacy_simple_norm01 = (indice_privacy_simple - rng_idx[1]) / (rng_idx[2] - rng_idx[1]))
} else {
  merged_data_norm <- merged_data_norm %>%
    mutate(indice_privacy_simple_norm01 = NA_real_)
}

# secondo ISTAT r OECD bisogna creare degli indicatori di affidabilità (indicatori di controllo qualitativo del dataset)

# controllo quante delle 4 componenti sono valide (cioè non NA)
# se un comune ha meno di 3 variabli valide l'indice è poco affidabile 
# low_into_flag = TRUE òìindice è poco robusto -> risulatti dell'indice vanno calcolati con cautela 
# low_into_flag = FALSE l'indice è calcolato su abbastanza dati 

# small_comune_flag identifica i comuni piccoli quidni con meno di 100 contibuenti

merged_data_norm <- merged_data_norm %>%
  mutate(
    low_info_flag = n_comps_valid < 3,
    small_comune_flag = `Numero contribuenti` < 100
  )

top20 <- merged_data_norm %>%
  arrange(desc(indice_privacy_simple_norm01)) %>%
  select(Codice.comune, Comune, `Numero contribuenti`, n_comps_valid, indice_privacy_simple_norm01) %>%
  head(20)

top20

summary_stats <- summary(merged_data_norm$indice_privacy_simple_norm01)

summary_stats

View(merged_data_norm)


# tutto 0-1 priviamo qualcosa di diverso 


##################### INDICE COMPOSITO #####################

# Variabili di interesse
vars_freq <- c(
  "Numero contribuenti",
  "Reddito da lavoro dipendente e assimilati - Frequenza",
  "Reddito da pensione - Frequenza",
  "Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza"
)

# 1. Controllo NA
merged_data %>%
  summarise(across(all_of(vars_freq), ~ sum(is.na(.)), .names = "NA_{col}"))

# 2. Funzione min-max robusta
minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

# 3. Log-transform + min-max normalization
merged_data_norm <- merged_data %>%
  mutate(
    n_contrib_log = log1p(`Numero contribuenti`),
    dip_log = log1p(`Reddito da lavoro dipendente e assimilati - Frequenza`),
    pens_log = log1p(`Reddito da pensione - Frequenza`),
    auto_log = log1p(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)
  ) %>%
  mutate(
    n_contrib_norm = minmax_norm(n_contrib_log),
    dip_norm = minmax_norm(dip_log),
    pens_norm = minmax_norm(pens_log),
    auto_norm = minmax_norm(auto_log)
  ) %>%
  # Inversione: valori bassi = rischio alto
  mutate(
    n_contrib_inv = 1 - n_contrib_norm,
    dip_inv = 1 - dip_norm,
    pens_inv = 1 - pens_norm,
    auto_inv = 1 - auto_norm
  )

# 4. Conteggio componenti valide
merged_data_norm <- merged_data_norm %>%
  mutate(
    n_comps_valid = rowSums(!is.na(select(., n_contrib_inv, dip_inv, pens_inv, auto_inv)))
  )

# 5. Calcolo indice composito (media semplice)
merged_data_norm <- merged_data_norm %>%
  rowwise() %>%
  mutate(
    indice_privacy_simple = mean(c_across(c(n_contrib_inv, dip_inv, pens_inv, auto_inv)), na.rm = TRUE)
  ) %>%
  ungroup()

# 6. Flag per affidabilità e comuni piccoli
merged_data_norm <- merged_data_norm %>%
  mutate(
    low_info_flag = n_comps_valid < 3,
    small_comune_flag = `Numero contribuenti` < 100
  )

# 7. Controlli rapidi
top20 <- merged_data_norm %>%
  arrange(desc(indice_privacy_simple)) %>%
  select(Codice.comune, Comune, `Numero contribuenti`, n_comps_valid, indice_privacy_simple) %>%
  head(20)

summary_stats <- summary(merged_data_norm$indice_privacy_simple)

# 8. Output per verifica
list(
  top20 = top20,
  summary_stats = summary_stats
)

# 9. Visualizzazione finale
View(merged_data_norm)


ggplot(merged_data_norm, aes(x = indice_privacy_simple)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of the Privacy Risk Composite Index",
    x = "Privacy Risk Index (simple mean)",
    y = "Number of Municipalities"
  ) +
  theme_minimal()

# Scatter plot: index vs number of taxpayers
ggplot(merged_data_norm, aes(x = `Numero contribuenti`, y = indice_privacy_simple)) +
  geom_point(alpha = 0.6, color = "#404080") +
  scale_x_log10() +  # log scale for number of taxpayers
  labs(
    title = "Privacy Risk Index vs Number of Taxpayers",
    x = "Number of Taxpayers (log scale)",
    y = "Privacy Risk Index"
  ) +
  theme_minimal()


# selezioniamo i top 20 e bottom 20
top15_comuni <- merged_data_norm %>%
  arrange(desc(indice_privacy_simple)) %>%
  slice_head(n = 15) %>%
  mutate(Category = "Top 15 (Highest Risk)")

bottom15_comuni <- merged_data_norm %>%
  arrange(indice_privacy_simple) %>%
  slice_head(n = 15) %>%
  mutate(Category = "Bottom 15 (Lowest Risk)")

# combiniamo
plot_data <- bind_rows(top15_comuni, bottom15_comuni)

# ordiniamo per indice per il grafico
plot_data <- plot_data %>%
  arrange(Category, indice_privacy_simple) %>%
  mutate(Comune = factor(Comune, levels = Comune))

# grafico

ggplot(plot_data, aes(x = Comune, y = indice_privacy_simple, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_x") +
  scale_fill_manual(values = c("Top 15 (Highest Risk)" = "#FF6B6B", "Bottom 15 (Lowest Risk)" = "#4E79A7")) +
  labs(
    title = "Privacy Risk Distribution: Simple Mean Index",
    x = "Municipality",
    y = "Privacy Risk Index",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Legenda rimossa
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "plain", size = 10) # Rende il titolo sopra le colonne più leggibile
  )

# con questa tipoligia di analisi inserendo il log ho ottentuo dei risultati migliori rispetto a quello precedente

# l'idea è provare a creare gli altri indici compositi e poi fare un'analisi grafica italiana per vedere le differenze anche
# a livello spaziale 




















############ PCA ############

##################### INDICE COMPOSITO PCA #####################

vars_freq <- c(
  "Numero contribuenti",
  "Reddito da lavoro dipendente e assimilati - Frequenza",
  "Reddito da pensione - Frequenza",
  "Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza"
)

# 2. Log-transform (per ridurre asimmetria)
merged_data_pca <- merged_data %>%
  mutate(
    n_contrib_log = log1p(`Numero contribuenti`),
    dip_log = log1p(`Reddito da lavoro dipendente e assimilati - Frequenza`),
    pens_log = log1p(`Reddito da pensione - Frequenza`),
    auto_log = log1p(`Reddito da lavoro autonomo (comprensivo dei valori nulli) - Frequenza`)
  )

z_vars <- merged_data_pca %>%
  select(n_contrib_log, dip_log, pens_log, auto_log) %>%
  scale(center = TRUE, scale = TRUE)

# Trasformiamo in data frame per FactoMineR
z_vars_df <- as.data.frame(z_vars)


#i NA vengono imputati automaticamente con la media
pca_res <- PCA(z_vars_df, scale.unit = FALSE, ncp = 4, graph = FALSE)

# Varianza spiegata da ogni componente
pca_var <- pca_res$eig

pca_var

# la PC1 spiega il 92,67% della varianza totale quindi basta questa


loadings_pc1 <- pca_res$var$coord[,1]
print(loadings_pc1)

# 7. Creazione dell'indice PCA usando PC1
merged_data_pca$indice_pca <- pca_res$ind$coord[,1]

# 8. Inversione logica per mantenere coerenza con media semplice
# Vogliamo che indice vicino a 1 = rischio alto (comuni piccoli)
merged_data_pca$indice_pca <- 1 - merged_data_pca$indice_pca


rng_idx <- range(merged_data_pca$indice_pca, na.rm = TRUE)
merged_data_pca$indice_pca_norm01 <- (merged_data_pca$indice_pca - rng_idx[1]) / (rng_idx[2] - rng_idx[1])


merged_data_pca <- merged_data_pca %>%
  mutate(
    small_comune_flag = `Numero contribuenti` < 100
  )

# 1. Selezione e preparazione dati (15 comuni)
top15_pca <- merged_data_pca %>%
  arrange(desc(indice_pca_norm01)) %>%
  slice_head(n = 15) %>%
  mutate(Category = "Top 15 (Highest Risk)")

bottom15_pca <- merged_data_pca %>%
  arrange(indice_pca_norm01) %>%
  slice_head(n = 15) %>%
  mutate(Category = "Bottom 15 (Lowest Risk)")

# Combiniamo i dati
plot_data_pca <- bind_rows(top15_pca, bottom15_pca) %>%
  mutate(Category = factor(Category, levels = c( "Bottom 15 (Lowest Risk)","Top 15 (Highest Risk)")))

# Ordinamento dei comuni (per avere le barre ordinate dentro ogni facet)
plot_data_pca <- plot_data_pca %>%
  group_by(Category) %>%
  mutate(Comune = fct_reorder(Comune, indice_pca_norm01)) %>%
  ungroup()

# 2. Grafico PCA
ggplot(plot_data_pca, aes(x = Comune, y = indice_pca_norm01, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_x") +
  scale_fill_manual(values = c("Top 15 (Highest Risk)" = "#FF6B6B", 
                               "Bottom 15 (Lowest Risk)" = "#4E79A7")) +
  labs(
    title = "Privacy Risk Distribution: PCA Index",
    x = "Municipality",
    y = " Privacy Risk Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "plain", size = 10),
    plot.title = element_text(face = "plain", size = 12)
  )
##################### INDICE COMPOSITO FEATURE IMPORTANCE WEIGHTS #####################

str(merged_data)

dataset_rf <- merged_data

write.csv(dataset_rf, "dataset_random_forest_completo.csv", row.names = FALSE)

# Messaggio di conferma
cat("✅ File esportato con successo: dataset_random_forest_completo.csv\n")

str(dataset_rf)

View(dataset_rf)

# IMPORTAZIONE FILE GOOGLE COLBA FEATURE IMPORTANCE RANDOM FOREST

weights <- read.csv("~/Desktop/Tirocinio/feature_importances_RF.csv")
View(weights)

df <- read.csv("~/Desktop/Tirocinio/dataset_comunale_per_R.csv")
View(df)

setdiff(weights$feature, names(df))

# ---- 2. Controlla feature presenti ----
missing_feats <- setdiff(weights$feature, names(df))
if(length(missing_feats) > 0){
  stop("Le seguenti feature NON sono presenti nel dataset comunale: ", paste(missing_feats, collapse = ", "))
} else {
  message("Tutte le feature sono presenti nel dataset comunale.")
}

weights_mean <- weights %>%
  group_by(feature) %>%
  summarise(importance_norm = mean(importance_norm, na.rm = TRUE))

# ---- 4. Standardizza le feature se necessario ----
scale_if_needed <- function(x) {
  if(sd(x, na.rm = TRUE) > 0.01) {
    return(scale(x)[,1])
  } else {
    return(x)
  }
}

for(f in weights_mean$feature){
  df[[paste0(f, "_z")]] <- scale_if_needed(df[[f]])
} 

df$Indice_RF_corr <- 0
for(i in 1:nrow(weights_mean)){
  f <- weights_mean$feature[i]
  w <- weights_mean$importance_norm[i]
  
  # Inverti il segno per le feature “positivamente correlate con popolazione”
  if(f == "Numero_contribuenti") {
    df$Indice_RF_corr <- df$Indice_RF_corr + w * (- df[[paste0(f, "_z")]])
  } else {
    df$Indice_RF_corr <- df$Indice_RF_corr + w * df[[paste0(f, "_z")]]
  }
}

# check 

# Top 10 comuni con indice più alto
df %>% arrange(desc(Indice_RF_corr)) %>% select(Comune, Indice_RF_corr) %>% head(10)

# Top 10 comuni con indice più basso
df %>% arrange(Indice_RF_corr) %>% select(Comune, Indice_RF_corr) %>% head(10)


# Normalizza tra 0 e 1
df$Indice_RF_corr_norm <- (df$Indice_RF_corr - min(df$Indice_RF_corr, na.rm=TRUE)) /
  (max(df$Indice_RF_corr, na.rm=TRUE) - min(df$Indice_RF_corr, na.rm=TRUE))

# Top 10 comuni con indice più alto
df %>% arrange(desc(Indice_RF_corr_norm )) %>% select(Comune, Indice_RF_corr_norm ) %>% head(10)

# Top 10 comuni con indice più basso
df %>% arrange(Indice_RF_corr_norm ) %>% select(Comune, Indice_RF_corr_norm ) %>% head(10)


# errore in quanto le città grandi hanno valori troppo alte rispetto a quelle piccola 
# dovrebbe essere il contrario

pop_features <- c("Numero_contribuenti",
                  "Maschi_eta_0_14","Maschi_eta_15_24","Maschi_eta_25_44",
                  "Maschi_eta_45_64","Maschi_eta_65_plus",
                  "Femmine_eta_0_14","Femmine_eta_15_24","Femmine_eta_25_44",
                  "Femmine_eta_45_64","Femmine_eta_65_plus")

for(f in pop_features){
  df[[paste0(f, "_z_inv")]] <- - df[[paste0(f, "_z")]]
}

df$Indice_RF_privacy <- 0
for(i in 1:nrow(weights_mean)){
  f <- weights_mean$feature[i]
  w <- weights_mean$importance_norm[i]
  if(f %in% pop_features){
    df$Indice_RF_privacy <- df$Indice_RF_privacy + w * df[[paste0(f, "_z_inv")]]
  } else {
    df$Indice_RF_privacy <- df$Indice_RF_privacy + w * df[[paste0(f, "_z")]]
  }
}

# Normalizza tra 0 e 1
df$Indice_RF_privacy_norm <- (df$Indice_RF_privacy - min(df$Indice_RF_privacy, na.rm=TRUE)) /
  (max(df$Indice_RF_privacy, na.rm=TRUE) - min(df$Indice_RF_privacy, na.rm=TRUE))


df %>% arrange(desc(Indice_RF_privacy_norm)) %>% select(Comune, Indice_RF_privacy_norm) %>% head(10)
df %>% arrange(Indice_RF_privacy_norm) %>% select(Comune, Indice_RF_privacy_norm) %>% head(10)

# li vorrei un po' meno uniformi 

# provo a creare anche delle feature relative come la % della popolazione ecc.

############# confornto primi tre indici #############

# faccio questo prima di implementare il mio indice composito misto in
# modo da vedere come si sono comportati gli indici

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

# Controlla i primi record
head(comparison_df,10)

summary(comparison_df)


missing_rf <- setdiff(merged_data_norm$Codice.comune, df$Codice.comune)
length(missing_rf)
head(missing_rf, 20)


# ------------------------
# 1. Selezione e preparazione dati (15 comuni e ordine corretto)
top15_rf <- comparison_df %>% 
  arrange(desc(Indice_RF_privacy_norm)) %>% 
  slice_head(n = 15) %>% 
  mutate(Category = "Top 15 (Highest Risk)")

bottom15_rf <- comparison_df %>% 
  arrange(Indice_RF_privacy_norm) %>% 
  slice_head(n = 15) %>% 
  mutate(Category = "Bottom 15 (Lowest Risk)")

# Combiniamo e impostiamo il fattore per l'ordine (Bottom a sinistra, Top a destra)
plot_data_rf <- bind_rows(top15_rf, bottom15_rf) %>%
  mutate(Category = factor(Category, levels = c("Bottom 15 (Lowest Risk)", "Top 15 (Highest Risk)")))

# Ordinamento dei comuni (per avere le barre ordinate dentro ogni facet)
plot_data_rf <- plot_data_rf %>%
  group_by(Category) %>%
  mutate(Comune = fct_reorder(Comune, Indice_RF_privacy_norm)) %>%
  ungroup()

# 2. Grafico RF
ggplot(plot_data_rf, aes(x = Comune, y = Indice_RF_privacy_norm, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_x") +
  scale_fill_manual(values = c("Bottom 15 (Lowest Risk)" = "#4E79A7", 
                               "Top 15 (Highest Risk)" = "#FF6B6B")) +
  labs(
    title = "Privacy Risk Distribution: Random Forest Index",
    x = "Municipality",
    y = "Privacy Risk Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "plain", size = 10),
    plot.title = element_text(face = "plain", size = 12)
  )
##################### INDICE COMPOSITO MISTO #####################

# Essendo che l'indice composito creato con le feature importance è 
# pressochè identico in tutti i comuni gli attribuisco un peso inferiore
# 45,45,20


comparison_df$Indice_composito_misto <- NA

for(i in 1:nrow(comparison_df)){
  w_simple <- 0.45
  w_pca <- 0.45
  w_rf <- 0.10
  
  rf_val <- comparison_df$Indice_RF_privacy_norm[i]
  
  # Se RF NA, redistribuisci peso tra Simple e PCA
  if(is.na(rf_val)){
    w_simple <- 0.5
    w_pca <- 0.5
    rf_val <- 0
  }
  
  comparison_df$Indice_composito_misto[i] <- 
    w_simple * comparison_df$indice_privacy_simple[i] +
    w_pca * comparison_df$indice_pca_norm01[i] +
    w_rf * rf_val
}

view(comparison_df)

# Top 10 comuni con indice composito misto più alto
comparison_df %>%
  arrange(desc(Indice_composito_misto)) %>%
  select(Comune, Indice_composito_misto) %>%
  head(10)

# Top 10 comuni con indice composito misto più basso
comparison_df %>%
  arrange(Indice_composito_misto) %>%
  select(Comune, Indice_composito_misto) %>%
  head(10)


# 1. Preparazione dati (15 comuni per parte)
top15_misto <- comparison_df %>% 
  arrange(desc(Indice_composito_misto)) %>% 
  slice_head(n = 15) %>% 
  mutate(Category = "Top 15 (Highest Risk)")

bottom15_misto <- comparison_df %>% 
  arrange(Indice_composito_misto) %>% 
  slice_head(n = 15) %>% 
  mutate(Category = "Bottom 15 (Lowest Risk)")

# Unione e impostazione ordine (Bottom a sinistra, Top a destra)
plot_data_misto <- bind_rows(bottom15_misto, top15_misto) %>%
  mutate(Category = factor(Category, levels = c("Bottom 15 (Lowest Risk)", "Top 15 (Highest Risk)")))

# Ordinamento interno ai facet
plot_data_misto <- plot_data_misto %>%
  group_by(Category) %>%
  mutate(Comune = fct_reorder(Comune, Indice_composito_misto)) %>%
  ungroup()

# 2. Grafico coordinato
ggplot(plot_data_misto, aes(x = Comune, y = Indice_composito_misto, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_x") +
  # Usiamo i tuoi colori: #4E79A7 (Blu) e #FF6B6B (Rosso)
  scale_fill_manual(values = c("Bottom 15 (Lowest Risk)" = "#4E79A7", 
                               "Top 15 (Highest Risk)" = "#FF6B6B")) +
  labs(
    title = "Privacy Risk Distribution: Composite Mixed Index",
    x = "Municipality",
    y = "Privacy Risk Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "plain", size = 10),
    plot.title = element_text(face = "plain", size = 12, hjust = 0.5)
  )

















##################### RAPPRESENTAZIONI GRAFICHE #####################

# import shape file 

# shape comune 

comuni_sf <- st_read("/Users/aleangeli/Desktop/Tirocinio/Limiti01012022/Com01012022/Com01012022_WGS84.shp")

# shape provincia 

province_sf <- st_read("/Users/aleangeli/Desktop/Tirocinio/Limiti01012022/ProvCM01012022/ProvCM01012022_WGS84.shp")

# shape regione 

regioni_sf <- st_read("/Users/aleangeli/Desktop/Tirocinio/Limiti01012022/Reg01012022/Reg01012022_WGS84.shp")

names(comuni_sf)

names(merged_data_norm)

names(merged_data_pca)

str(comuni_sf$COD_CM)com

str(merged_data_norm$Codice.comune)

head(comuni_sf$COD_UTS)

head(comuni_sf$PRO_COM)

##################### rappresentazione comunale media semplice ##################### 

# Convertiamo entrambi in character (assicurandoci che gli zeri iniziali siano corretti)
comuni_sf <- comuni_sf %>%
  mutate(PRO_COM = sprintf("%04d", PRO_COM))

merged_data_norm <- merged_data_norm %>%
  mutate(Codice.comune = sprintf("%04d", Codice.comune))

# Join tra shapefile e dati dell'indice

comuni_map <- comuni_sf %>%
  left_join(
    merged_data_norm %>% select(Codice.comune, Comune, indice_privacy_simple),
    by = c("PRO_COM" = "Codice.comune")
  )

comuni_map <- st_as_sf(comuni_map, sf_column_name = "geometry")

# Assicurati che COD_REG sia character per uniformità
regioni_sf <- regioni_sf %>% 
  mutate(COD_REG = as.character(COD_REG))

comuni_map <- st_make_valid(comuni_map)

# Rimuovi eventuali righe con indice NA (solo per il plot)
comuni_map_plot <- comuni_map %>% filter(!is.na(indice_privacy_simple))

# Uniforma CRS con regioni
st_crs(regioni_sf) <- st_crs(comuni_map_plot)

# Plot
comuni_media_semplice = ggplot() +
  geom_sf(data = comuni_map_plot, aes(fill = indice_privacy_simple), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Municipality",
    subtitle = "Based on the simple composite index"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

comuni_media_semplice

##################### rappresentazione comunale per l'indice PCA ##################### 

# Assicurati che l'indice PCA sia presente in merged_data_pca
names(merged_data_pca)

merged_data_pca <- merged_data_pca %>%
  mutate(Codice.comune = as.character(Codice.comune))

# Unione dati con lo shapefile dei comuni
comuni_map_pca <- comuni_sf %>%
  left_join(
    merged_data_pca %>% select(Codice.comune, Comune, indice_pca_norm01),
    by = c("PRO_COM" = "Codice.comune")
  )

# Controllo rapido
summary(comuni_map_pca$indice_pca_norm01)

# Converti COD_REG in character per uniformità
regioni_sf <- regioni_sf %>% 
  mutate(COD_REG = as.character(COD_REG))

# Unione con i comuni già mappati
# Questo serve solo se vuoi colorare i comuni e sovrapporre i contorni regionali
ggplot() +
  geom_sf(data = comuni_map_pca, aes(fill = indice_pca_norm01), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Municipality (PCA)",
    subtitle = "With regional boundaries",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

##################### rappresentazione provinciale  media semplice ##################### 

indice_privacy_prov <- merged_data_norm %>%
  group_by(`Sigla Provincia`) %>%
  summarise(indice_privacy_media = mean(indice_privacy_simple, na.rm = TRUE))

str(province_sf$SIGLA)

# Converti in character per sicurezza
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))

# Join dati con shapefile
province_map <- province_sf %>%
  left_join(indice_privacy_prov, by = c("SIGLA" = "Sigla Provincia"))

st_crs(regioni_sf) <- st_crs(province_map)

ggplot() +
  geom_sf(data = province_map, aes(fill = indice_privacy_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Province",
    subtitle = "Based on the simple composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

##########tesi ##########
ggplot() +
  geom_sf(data = province_map, aes(fill = indice_privacy_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    na.value = "grey90",
    name = "Privacy Risk Index",
    # Rimuovendo limits, i colori torneranno quelli originali brillanti
    breaks = c(min(province_map$indice_privacy_media, na.rm = TRUE), 
               median(province_map$indice_privacy_media, na.rm = TRUE), 
               max(province_map$indice_privacy_media, na.rm = TRUE)),
    labels = c("Low", "Medium", "High"),
    guide = guide_colourbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  labs(
    title = "Privacy Risk Index based on Simple Mean: Province Level",
    caption = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title.position = "top",
    plot.title = element_text(hjust = 0.5, face = "plain")
  )
##################### rappresentazione provinciale  PCA ##################### 

indice_pca_prov <- merged_data_pca %>%
  group_by(`Sigla Provincia`) %>%
  summarise(indice_pca_media = mean(indice_pca_norm01, na.rm = TRUE))

# Converti in character per sicurezza
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))

# Join dei dati con lo shapefile
province_map_pca <- province_sf %>%
  left_join(indice_pca_prov, by = c("SIGLA" = "Sigla Provincia"))


st_crs(regioni_sf) <- st_crs(province_map_pca)

ggplot() +
  geom_sf(data = province_map_pca, aes(fill = indice_pca_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Province",
    subtitle = "Based on the mean of the PCA index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



##################### rappresentazione regionale  media semplice #####################

# Rinomina la colonna per semplicità
merged_data_norm <- merged_data_norm %>%
  rename(Codice_Istat_Regione = `Codice Istat Regione`)

# Calcola l'indice medio per regione
indice_regionale <- merged_data_norm %>%
  group_by(Codice_Istat_Regione) %>%
  summarise(indice_simple_media = mean(indice_privacy_simple, na.rm = TRUE))

# Assicurati che il codice regione sia character
regioni_sf <- regioni_sf %>% mutate(COD_REG = as.character(COD_REG))

indice_regionale <- indice_regionale %>%
  mutate(Codice_Istat_Regione = as.character(Codice_Istat_Regione))

# Join tra shapefile e dati aggregati
mappa_regioni_simple <- regioni_sf %>%
  left_join(indice_regionale, by = c("COD_REG" = "Codice_Istat_Regione"))

# Plot
ggplot() +
  geom_sf(data = mappa_regioni_simple, aes(fill = indice_simple_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Region",
    subtitle = "Based on the simple composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


##################### rappresentazione regionale  PCA ##################### 

# Aggrega l'indice PCA per regione
indice_regionale_pca <- merged_data_pca %>%
  group_by(`Codice Istat Regione`) %>%
  summarise(indice_pca_media = mean(indice_pca_norm01, na.rm = TRUE)) %>%
  mutate(`Codice Istat Regione` = as.character(`Codice Istat Regione`))  # uniforma il tipo

# Join con lo shapefile delle regioni
mappa_regioni_pca <- regioni_sf %>%
  left_join(indice_regionale_pca, by = c("COD_REG" = "Codice Istat Regione"))

# Plot
ggplot(mappa_regioni_pca) +
  geom_sf(aes(fill = indice_pca_media), color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Region",
    subtitle = "Based on the PCA composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

######################### rappresentazione RF #########################


# Assicuriamoci che il codice sia in formato character
comuni_sf <- comuni_sf %>% mutate(PRO_COM = sprintf("%04d", PRO_COM))
df <- df %>% mutate(Codice.comune = sprintf("%04d", Codice.comune))

# Join con indice RF
comuni_map_rf <- comuni_sf %>%
  left_join(df %>% select(Codice.comune, Comune, Indice_RF_privacy_norm),
            by = c("PRO_COM" = "Codice.comune")) %>%
  filter(!is.na(Indice_RF_privacy_norm))

# Uniforma CRS
st_crs(comuni_map_rf) <- st_crs(regioni_sf)

# Plot
ggplot() +
  geom_sf(data = comuni_map_rf, aes(fill = Indice_RF_privacy_norm), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0,1),
                       name = "Privacy Risk Index (RF)") +
  labs(title = "Privacy Risk Index (Random Forest) by Municipality",
       caption = "Fonte: ISTAT + elaborazione propria") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Unisci l'indice RF ai comuni
comuni_rf <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_RF_privacy_norm),
    by = c("PRO_COM" = "Codice.comune")
  )

# Unisci l'indice RF ai comuni
comuni_rf <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_RF_privacy_norm),
    by = c("PRO_COM" = "Codice.comune")
  )

# Escludi comuni con NA e aggrega per provincia
df_prov_rf <- comuni_rf %>%
  st_drop_geometry() %>% 
  filter(!is.na(Indice_RF_privacy_norm)) %>%
  group_by(COD_PROV) %>%
  summarise(Indice_RF_privacy_norm = mean(Indice_RF_privacy_norm, na.rm = TRUE))

# Assicura che i codici provinciali siano character
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))
df_prov_rf <- df_prov_rf %>% mutate(COD_PROV = as.character(COD_PROV))

# Join con shapefile province
province_map_rf <- province_sf %>%
  left_join(df_prov_rf, by = c("SIGLA" = "COD_PROV"))


ggplot() +
  geom_sf(data = province_map_rf, aes(fill = Indice_RF_privacy_norm), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0,1),
                       name = "Indice RF") +
  labs(
    title = "Indice Random Forest – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )











##################### Rpprensentazione indice composito misto #####################


comuni_sf <- comuni_sf %>% 
  mutate(PRO_COM = sprintf("%06d", as.numeric(PRO_COM)))

comparison_df <- comparison_df %>% 
  mutate(Codice.comune = sprintf("%06d", as.numeric(Codice.comune)))

comuni_map_misto <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  ) %>%
  filter(!is.na(Indice_composito_misto))

regioni_sf <- st_transform(regioni_sf, st_crs(comuni_sf))

ggplot() +
  geom_sf(data = comuni_map_misto, aes(fill = Indice_composito_misto), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0,1),
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Comunale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################provinciale#########

# Assicuriamoci che PRO_COM e Codice.comune siano stati uniformati come nel passo precedente

names(comuni_sf)
df_prov <- comuni_sf %>%
  st_drop_geometry() %>%   # ⬅⬅⬅ elimina le geometrie prima dell'aggregazione
  left_join(
    comparison_df %>% select(Codice.comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  ) %>%
  group_by(COD_PROV) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE))


province_map_misto <- province_sf %>%
  left_join(df_prov, by = c("COD_PROV"))

ggplot() +
  geom_sf(data = province_map_misto, 
          aes(fill = Indice_composito_misto), 
          color = "black", size = 0.2) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0.3,1),
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


########## regionale ########
names(regioni_sf)
names(comparison_df)

comuni_con_misto <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  )

df_reg_misto <- comuni_con_misto %>%
  st_drop_geometry() %>%       # rimuove geometria per evitare problemi
  group_by(COD_REG) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE))


regioni_sf <- regioni_sf %>%
  mutate(COD_REG = as.character(COD_REG))

df_reg_misto <- df_reg_misto %>%
  mutate(COD_REG = as.character(COD_REG))


regioni_map_misto <- regioni_sf %>%
  left_join(df_reg_misto, by = "COD_REG")

ggplot() +
  geom_sf(data = regioni_map_misto,
          aes(fill = Indice_composito_misto),
          color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       limits = c(0.3,1),
                       name = "Indice Composito Misto") +
  labs(
    title = "Indice Composito Misto – Livello Regionale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################

################ Provinciale ################

province_map_misto <- province_sf %>%
  left_join(df_prov, by = c("COD_PROV"))

ggplot() +
  geom_sf(data = province_map_misto, 
          aes(fill = Indice_composito_misto), 
          color = "black", size = 0.2) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0.3,1), # <-- qui
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################ Regionale ################

regioni_map_misto <- regioni_sf %>%
  left_join(df_reg_misto, by = "COD_REG")

ggplot() +
  geom_sf(data = regioni_map_misto,
          aes(fill = Indice_composito_misto),
          color = "black", size = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0.5,1), # <-- qui
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Regionale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# sistemazione per tesi: ##################### rappresentazione comunale media semplice ##################### 

# Convertiamo entrambi in character (assicurandoci che gli zeri iniziali siano corretti)
comuni_sf <- comuni_sf %>%
  mutate(PRO_COM = sprintf("%04d", PRO_COM))

merged_data_norm <- merged_data_norm %>%
  mutate(Codice.comune = sprintf("%04d", Codice.comune))

# Join tra shapefile e dati dell'indice

comuni_map <- comuni_sf %>%
  left_join(
    merged_data_norm %>% select(Codice.comune, Comune, indice_privacy_simple),
    by = c("PRO_COM" = "Codice.comune")
  )

comuni_map <- st_as_sf(comuni_map, sf_column_name = "geometry")

# Assicurati che COD_REG sia character per uniformità
regioni_sf <- regioni_sf %>% 
  mutate(COD_REG = as.character(COD_REG))

comuni_map <- st_make_valid(comuni_map)

# Rimuovi eventuali righe con indice NA (solo per il plot)
comuni_map_plot <- comuni_map %>% filter(!is.na(indice_privacy_simple))

# Uniforma CRS con regioni
st_crs(regioni_sf) <- st_crs(comuni_map_plot)

# Plot
comuni_media_semplice = ggplot() +
  geom_sf(data = comuni_map_plot, aes(fill = indice_privacy_simple), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Municipality",
    subtitle = "Based on the simple composite index"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

comuni_media_semplice

ggplot() +
  geom_sf(data = comuni_map_plot, aes(fill = indice_privacy_simple), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    na.value = "grey90",
    name = "Privacy Risk Index",
    # Mappiamo i valori su Low, Medium, High basandoci sui dati reali
    breaks = c(min(comuni_map_plot$indice_privacy_simple, na.rm = TRUE), 
               median(comuni_map_plot$indice_privacy_simple, na.rm = TRUE), 
               max(comuni_map_plot$indice_privacy_simple, na.rm = TRUE)),
    labels = c("Low", "Medium", "High"),
    # Guide forzata per mantenere la legenda in alto e orizzontale
    guide = guide_colourbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  labs(
    title = "Privacy Risk Index by Municipality",
    subtitle = "Based on the simple composite index",
    caption = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    # Posizionamento legenda
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

##################### rappresentazione comunale per l'indice PCA ##################### 

# Assicurati che l'indice PCA sia presente in merged_data_pca
names(merged_data_pca)

merged_data_pca <- merged_data_pca %>%
  mutate(Codice.comune = as.character(Codice.comune))

# Unione dati con lo shapefile dei comuni
comuni_map_pca <- comuni_sf %>%
  left_join(
    merged_data_pca %>% select(Codice.comune, Comune, indice_pca_norm01),
    by = c("PRO_COM" = "Codice.comune")
  )

# Controllo rapido
summary(comuni_map_pca$indice_pca_norm01)

# Converti COD_REG in character per uniformità
regioni_sf <- regioni_sf %>% 
  mutate(COD_REG = as.character(COD_REG))

# Unione con i comuni già mappati
# Questo serve solo se vuoi colorare i comuni e sovrapporre i contorni regionali
ggplot() +
  geom_sf(data = comuni_map_pca, aes(fill = indice_pca_norm01), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Municipality (PCA)",
    subtitle = "With regional boundaries",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

##################### rappresentazione provinciale  media semplice ##################### 

indice_privacy_prov <- merged_data_norm %>%
  group_by(`Sigla Provincia`) %>%
  summarise(indice_privacy_media = mean(indice_privacy_simple, na.rm = TRUE))

str(province_sf$SIGLA)

# Converti in character per sicurezza
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))

# Join dati con shapefile
province_map <- province_sf %>%
  left_join(indice_privacy_prov, by = c("SIGLA" = "Sigla Provincia"))

st_crs(regioni_sf) <- st_crs(province_map)

ggplot() +
  geom_sf(data = province_map, aes(fill = indice_privacy_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Province",
    subtitle = "Based on the simple composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Assicuriamoci che la legenda sia coerente: 
# Usiamo 'magma' che è il trend standard per i rischi (dal nero/viola al giallo)
# Il giallo acceso indicherà il "Massimo Rischio", come nel tuo screenshot.

ggplot() +
  geom_sf(data = province_map, aes(fill = indice_privacy_media), color = "white", size = 0.1) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    na.value = "grey90",
    name = "Privacy Risk Index",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = 15, 
      barheight = 1
    )
  ) +
  labs(
    title = "Composite Privacy Risk Index",
    subtitle = "Provincial Level Aggregation",
    caption = "Source: ISTAT and MEF data, author's elaboration."
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top", # Legenda sotto per dare spazio alla mappa
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

##################### rappresentazione provinciale  PCA ##################### 

indice_pca_prov <- merged_data_pca %>%
  group_by(`Sigla Provincia`) %>%
  summarise(indice_pca_media = mean(indice_pca_norm01, na.rm = TRUE))

# Converti in character per sicurezza
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))

# Join dei dati con lo shapefile
province_map_pca <- province_sf %>%
  left_join(indice_pca_prov, by = c("SIGLA" = "Sigla Provincia"))


st_crs(regioni_sf) <- st_crs(province_map_pca)

ggplot() +
  geom_sf(data = province_map_pca, aes(fill = indice_pca_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Province",
    subtitle = "Based on the mean of the PCA index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



##################### rappresentazione regionale  media semplice #####################

# Rinomina la colonna per semplicità
merged_data_norm <- merged_data_norm %>%
  rename(Codice_Istat_Regione = `Codice Istat Regione`)

# Calcola l'indice medio per regione
indice_regionale <- merged_data_norm %>%
  group_by(Codice_Istat_Regione) %>%
  summarise(indice_simple_media = mean(indice_privacy_simple, na.rm = TRUE))

# Assicurati che il codice regione sia character
regioni_sf <- regioni_sf %>% mutate(COD_REG = as.character(COD_REG))

indice_regionale <- indice_regionale %>%
  mutate(Codice_Istat_Regione = as.character(Codice_Istat_Regione))

# Join tra shapefile e dati aggregati
mappa_regioni_simple <- regioni_sf %>%
  left_join(indice_regionale, by = c("COD_REG" = "Codice_Istat_Regione"))

# Plot
ggplot() +
  geom_sf(data = mappa_regioni_simple, aes(fill = indice_simple_media), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index") +
  labs(
    title = "Privacy Risk Index by Region",
    subtitle = "Based on the simple composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


##################### rappresentazione regionale  PCA ##################### 

# Aggrega l'indice PCA per regione
indice_regionale_pca <- merged_data_pca %>%
  group_by(`Codice Istat Regione`) %>%
  summarise(indice_pca_media = mean(indice_pca_norm01, na.rm = TRUE)) %>%
  mutate(`Codice Istat Regione` = as.character(`Codice Istat Regione`))  # uniforma il tipo

# Join con lo shapefile delle regioni
mappa_regioni_pca <- regioni_sf %>%
  left_join(indice_regionale_pca, by = c("COD_REG" = "Codice Istat Regione"))

# Plot
ggplot(mappa_regioni_pca) +
  geom_sf(aes(fill = indice_pca_media), color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90",
                       name = "Privacy Risk Index (PCA)") +
  labs(
    title = "Privacy Risk Index by Region",
    subtitle = "Based on the PCA composite index",
    caption = "Source: ISTAT + elaboration"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

######################### rappresentazione RF #########################


# Assicuriamoci che il codice sia in formato character
comuni_sf <- comuni_sf %>% mutate(PRO_COM = sprintf("%04d", PRO_COM))
df <- df %>% mutate(Codice.comune = sprintf("%04d", Codice.comune))

# Join con indice RF
comuni_map_rf <- comuni_sf %>%
  left_join(df %>% select(Codice.comune, Comune, Indice_RF_privacy_norm),
            by = c("PRO_COM" = "Codice.comune")) %>%
  filter(!is.na(Indice_RF_privacy_norm))

# Uniforma CRS
st_crs(comuni_map_rf) <- st_crs(regioni_sf)

# Plot
ggplot() +
  geom_sf(data = comuni_map_rf, aes(fill = Indice_RF_privacy_norm), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0,1),
                       name = "Privacy Risk Index (RF)") +
  labs(title = "Privacy Risk Index (Random Forest) by Municipality",
       caption = "Fonte: ISTAT + elaborazione propria") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Unisci l'indice RF ai comuni
comuni_rf <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_RF_privacy_norm),
    by = c("PRO_COM" = "Codice.comune")
  )

# Unisci l'indice RF ai comuni
comuni_rf <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_RF_privacy_norm),
    by = c("PRO_COM" = "Codice.comune")
  )

# Escludi comuni con NA e aggrega per provincia
df_prov_rf <- comuni_rf %>%
  st_drop_geometry() %>% 
  filter(!is.na(Indice_RF_privacy_norm)) %>%
  group_by(COD_PROV) %>%
  summarise(Indice_RF_privacy_norm = mean(Indice_RF_privacy_norm, na.rm = TRUE))

# Assicura che i codici provinciali siano character
province_sf <- province_sf %>% mutate(SIGLA = as.character(SIGLA))
df_prov_rf <- df_prov_rf %>% mutate(COD_PROV = as.character(COD_PROV))

# Join con shapefile province
province_map_rf <- province_sf %>%
  left_join(df_prov_rf, by = c("SIGLA" = "COD_PROV"))


ggplot() +
  geom_sf(data = province_map_rf, aes(fill = Indice_RF_privacy_norm), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0,1),
                       name = "Indice RF") +
  labs(
    title = "Indice Random Forest – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )











##################### Rpprensentazione indice composito misto #####################


comuni_sf <- comuni_sf %>% 
  mutate(PRO_COM = sprintf("%06d", as.numeric(PRO_COM)))

comparison_df <- comparison_df %>% 
  mutate(Codice.comune = sprintf("%06d", as.numeric(Codice.comune)))

comuni_map_misto <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  ) %>%
  filter(!is.na(Indice_composito_misto))

regioni_sf <- st_transform(regioni_sf, st_crs(comuni_sf))

ggplot() +
  geom_sf(data = comuni_map_misto, aes(fill = Indice_composito_misto), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0,1),
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Comunale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################provinciale#########

# Assicuriamoci che PRO_COM e Codice.comune siano stati uniformati come nel passo precedente

names(comuni_sf)
df_prov <- comuni_sf %>%
  st_drop_geometry() %>%   # ⬅⬅⬅ elimina le geometrie prima dell'aggregazione
  left_join(
    comparison_df %>% select(Codice.comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  ) %>%
  group_by(COD_PROV) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE))


province_map_misto <- province_sf %>%
  left_join(df_prov, by = c("COD_PROV"))

ggplot() +
  geom_sf(data = province_map_misto, 
          aes(fill = Indice_composito_misto), 
          color = "black", size = 0.2) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0.3,1),
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


########## regionale ########
names(regioni_sf)
names(comparison_df)

comuni_con_misto <- comuni_sf %>%
  left_join(
    comparison_df %>% select(Codice.comune, Indice_composito_misto),
    by = c("PRO_COM" = "Codice.comune")
  )

df_reg_misto <- comuni_con_misto %>%
  st_drop_geometry() %>%       # rimuove geometria per evitare problemi
  group_by(COD_REG) %>%
  summarise(Indice_composito_misto = mean(Indice_composito_misto, na.rm = TRUE))


regioni_sf <- regioni_sf %>%
  mutate(COD_REG = as.character(COD_REG))

df_reg_misto <- df_reg_misto %>%
  mutate(COD_REG = as.character(COD_REG))


regioni_map_misto <- regioni_sf %>%
  left_join(df_reg_misto, by = "COD_REG")

ggplot() +
  geom_sf(data = regioni_map_misto,
          aes(fill = Indice_composito_misto),
          color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       limits = c(0.3,1),
                       name = "Indice Composito Misto") +
  labs(
    title = "Indice Composito Misto – Livello Regionale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################

################ Provinciale ################

province_map_misto <- province_sf %>%
  left_join(df_prov, by = c("COD_PROV"))

ggplot() +
  geom_sf(data = province_map_misto, 
          aes(fill = Indice_composito_misto), 
          color = "black", size = 0.2) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0.3,1), # <-- qui
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Provinciale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


################ Regionale ################

regioni_map_misto <- regioni_sf %>%
  left_join(df_reg_misto, by = "COD_REG")

ggplot() +
  geom_sf(data = regioni_map_misto,
          aes(fill = Indice_composito_misto),
          color = "black", size = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0.5,1), # <-- qui
    name = "Indice Composito Misto"
  ) +
  labs(
    title = "Indice Composito Misto – Livello Regionale",
    caption = "Fonte: ISTAT + elaborazione propria"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# sistemazione per tesi 


theme_map_thesis <- function() {
  theme_void() + 
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain", margin = margin(b = 10)),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}


library(ggplot2)
library(sf)
library(dplyr)

# --- TEMA COMUNE PER COERENZA ---
theme_map_thesis <- function(legend_pos = "right") {
  theme_void() + 
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain", margin = margin(b = 10)),
      legend.position = legend_pos,
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

# --- TEMA COORDINATO ---
theme_misto_thesis <- function(legend_pos = "top") {
  theme_void() + 
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain", margin = margin(b = 10)),
      legend.position = legend_pos,
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

# =========================================================
# 1. MAPPA COMUNALE (Pagina Intera)
# =========================================================
comuni_misto_plot <- ggplot() +
  geom_sf(data = comuni_map_misto, aes(fill = Indice_composito_misto), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    limits = c(0, 1),
    # Rinominiamo il titolo per chiarezza immediata
    name = "Privacy Risk Level", 
    # Usiamo 'breaks' per forzare la visualizzazione di etichette chiare
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("Low (0.0)", "0.25", "0.5", "0.75", "High (1.0)"),
    guide = guide_colorbar(
      direction = "horizontal", 
      barwidth = 15,    # Leggermente più larga per leggibilità
      barheight = 0.6,  # Leggermente più alta
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  labs(title = "Mixed Privacy Risk Index: Municipal Level") +
  theme_misto_thesis(legend_pos = "top") +
  theme(
    # Aumentiamo lo spazio tra titolo e grafico
    plot.title = element_text(size = 14, hjust = 0.5, face = "plain", margin = margin(b = 20))
  )

comuni_misto_plot

# =========================================================
# 2. MAPPA PROVINCIALE (Confronto)
# =========================================================
# Assicurati che province_map_misto sia già unito ai dati aggregati
provinciale_misto_plot <- ggplot() +
  geom_sf(data = province_map_misto, aes(fill = Indice_composito_misto), color = "white", linewidth = 0.1) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Risk Index") +
  labs(title = "Mixed Privacy Risk Index: Provincial Level") +
  theme_misto_thesis(legend_pos = "right")

# =========================================================
# 3. MAPPA REGIONALE (Appendice)
# =========================================================
regionale_misto_plot <- ggplot() +
  geom_sf(data = regioni_map_misto, aes(fill = Indice_composito_misto), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Risk Index") +
  labs(title = "Mixed Privacy Risk Index: Regional Level") +
  theme_misto_thesis(legend_pos = "right")

# --- VISUALIZZA ---
print(comuni_misto_plot)

##################### PROVE TESI #############################

# Tema personalizzato per la tesi
theme_thesis_map <- function(legend_pos = "top") {
  theme_void() + 
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain", margin = margin(b = 10)),
      legend.position = legend_pos,
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Funzione per mappe comunali (Legenda in alto)
plot_comunale <- function(map_data, var_name, map_title) {
  ggplot() +
    geom_sf(data = map_data, aes(fill = {{var_name}}), color = NA) +
    geom_sf(data = regioni_sf, fill = NA, color = "black", linewidth = 0.4) +
    scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Risk Index") +
    labs(title = map_title) +
    theme_thesis_map(legend_pos = "top")
}


# Funzione per mappe provinciali/regionali (Legenda a destra)
plot_compact <- function(map_data, var_name, map_title) {
  ggplot() +
    geom_sf(data = map_data, aes(fill = {{var_name}}), color = "black", linewidth = 0.2) +
    geom_sf(data = regioni_sf, fill = NA, color = "black", linewidth = 0.4) +
    scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Risk Index") +
    labs(title = map_title) +
    theme_thesis_map(legend_pos = "right")
}

# ==============================================================================
# SCRIPT DEFINITIVO MAPPE - INDICE COMPOSITO MISTO (Per Tesi)
# ==============================================================================

# Assicuriamoci che i codici siano sempre allineati (formato 6 cifre)
comuni_sf <- comuni_sf %>% mutate(PRO_COM = sprintf("%06d", as.numeric(PRO_COM)))
comparison_df <- comparison_df %>% mutate(Codice.comune = sprintf("%06d", as.numeric(Codice.comune)))

# 1. PREPARAZIONE DATI (Comunale, Provinciale, Regionale)
comuni_map_misto <- comuni_sf %>%
  left_join(comparison_df %>% select(Codice.comune, Indice_composito_misto), 
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

# Join Geografici
province_map_misto <- province_sf %>% left_join(df_prov, by = "COD_PROV")
regioni_map_misto <- regioni_sf %>% mutate(COD_REG = as.character(COD_REG)) %>% left_join(df_reg, by = "COD_REG")

# ==============================================================================
# 2. GENERAZIONE PLOT (Stile Coerente)
# ==============================================================================

# A. Comunale (Piena Pagina - Legenda in alto)
plot_comunale_misto <- ggplot() +
  geom_sf(data = comuni_map_misto, aes(fill = Indice_composito_misto), color = NA) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 1), name = "Privacy Risk Level",
                       breaks = c(0, 0.5, 1), labels = c("Low (0.0)", "0.5", "High (1.0)"),
                       guide = guide_colorbar(direction = "horizontal", barwidth = 12, barheight = 0.5, title.position = "top", title.hjust = 0.5)) +
  labs(title = "Mixed Privacy Risk Index: Municipal Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 14), legend.position = "top")

# B. Provinciale (Compatto - Legenda a destra)
plot_provinciale_misto <- ggplot() +
  geom_sf(data = province_map_misto, aes(fill = Indice_composito_misto), color = "white", size = 0.1) +
  geom_sf(data = regioni_sf, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0.3, 1), name = "Risk Index") +
  labs(title = "Mixed Privacy Risk Index: Provincial Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "right")

# C. Regionale (Compatto - Legenda a destra)
plot_regionale_misto <- ggplot() +
  geom_sf(data = regioni_map_misto, aes(fill = Indice_composito_misto), color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0.5, 1), name = "Risk Index") +
  labs(title = "Mixed Privacy Risk Index: Regional Level") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "right")

# Visualizza per verificare
print(plot_comunale_misto)
print(plot_provinciale_misto)
print(plot_regionale_misto)












