# ====================================================================
# ANALISI JAILBREAK PROMPT - CLUSTERING EMERGENTE DA DIMENSIONI TEORICHE
# ====================================================================

# FASE 1: CARICAMENTO LIBRERIE E DATI
# ====================================

# Installiamo e carichiamo le librerie necessarie
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(stringr)) install.packages("stringr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(tibble)) install.packages("tibble")
if (!require(tidyr)) install.packages("tidyr")
if (!require(writexl)) install.packages("writexl")

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tibble)
library(tidyr)
library(writexl)

# Caricamento del dataset completo (tutti i 1406 prompt)
dataset_path <- "C:/Users/morse/OneDrive/Desktop/prova/DATASET12-25FORMATTATO.xlsx"
jailbreak_data <- read_excel(dataset_path)

# Verifica iniziale dei dati
cat("=== INFORMAZIONI DATASET ===\n")
cat("Numero totale di prompt:", nrow(jailbreak_data), "\n")
cat("Numero di colonne:", ncol(jailbreak_data), "\n")
cat("Colonne disponibili:", paste(names(jailbreak_data), collapse = ", "), "\n\n")

# Controllo valori mancanti nella colonna prompt
missing_prompts <- sum(is.na(jailbreak_data$prompt) | jailbreak_data$prompt == "")
cat("Prompt mancanti o vuoti:", missing_prompts, "\n")

# Rimuoviamo eventuali righe con prompt vuoti
jailbreak_data <- jailbreak_data %>% 
  filter(!is.na(prompt) & prompt != "")

cat("Prompt validi per l'analisi:", nrow(jailbreak_data), "\n\n")

# ====================================================================
# FASE 2: DIZIONARI AMPLIATI E RAFFINATI
# ====================================================================

cat("=== CREAZIONE DIZIONARI AMPLIATI ===\n")

# DIZIONARIO TRAINING DATA AMPLIATO
training_data_keywords <- c(
  # --- Conoscenze e informazioni pregresse ---
  "knowledge", "information", "data", "facts", "learn", "trained", "training",
  "database", "memory", "remember", "recall", "stored", "memorized",
  "encyclopedia", "reference", "source", "educational", "academic",
  "historical", "factual", "documented", "recorded", "archives",
  
  # --- Correzioni e aggiornamenti ---
  "actually", "correct", "truth", "real", "accurate", "verified",
  "evidence", "proof", "scientific", "research", "studies",
  "update", "fix", "change", "revise", "amend", "modify",
  "correction", "rectify", "adjust", "alter", "edit",
  
  # --- Accesso a informazioni ---
  "search", "lookup", "find", "retrieve", "access", "browse",
  "internet", "web", "online", "website", "wikipedia", "google",
  "query", "fetch", "obtain", "acquire", "gather",
  
  # --- Manipolazione della base di conoscenza ---
  "dataset", "examples", "sample", "corpus", "before", "originally",
  "initial", "first", "primary", "base", "foundation",
  "forget", "ignore", "override", "replace", "substitute", "instead",
  "new information", "contrary", "despite", "although", "however",
  
  # --- Termini tecnici ML ---
  "weights", "parameters", "embeddings", "vectors", "neurons",
  "layers", "model", "architecture", "pretrain", "pretraining",
  "transformer", "attention", "tokeniz", "vocab", "dictionary",
  
  # --- Epistemologia e verità ---
  "believe", "know", "certain", "sure", "confident", "aware",
  "mistaken", "wrong", "error", "mistake", "incorrect",
  "false", "untrue", "inaccurate", "misleading", "misinform"
)

# DIZIONARIO FINE-TUNING AMPLIATO
fine_tuning_keywords <- c(
  # --- Restrizioni e regole ---
  "restrictions", "guidelines", "rules", "policy", "policies",
  "limitations", "boundaries", "constraints", "prohibited", "forbidden",
  "not allowed", "cannot", "won't", "refuse", "decline",
  "inappropriate", "unsafe", "unethical", "illegal", "harmful",
  
  # --- Comportamenti appresi ---
  "helpful", "harmless", "honest", "ethical", "responsible", "safe",
  "appropriate", "respectful", "professional", "polite", "careful",
  "cautious", "considerate", "moderate", "balanced", "neutral",
  "objective", "unbiased", "fair", "reasonable", "thoughtful",
  
  # --- Sistema di sicurezza ---
  "safety", "security", "protection", "harm", "dangerous", "risk",
  "filter", "censorship", "moderation", "compliance", "violation",
  "terms of service", "community guidelines", "content policy",
  "acceptable use", "code of conduct", "ethics", "moral",
  
  # --- Allineamento e ottimizzazione ---
  "fine-tun", "finetun", "instruct", "align", "adjust", "optimize",
  "customize", "refine", "calibrate", "configure", "adapt", "tune",
  "reward", "reinforce", "RLHF", "human feedback", "preference",
  
  # --- Bypass e manipolazione ---
  "bypass", "circumvent", "avoid", "skip", "disable", "turn off",
  "remove", "delete", "cancel", "suspend", "pause", "stop",
  "workaround", "loophole", "exploit", "hack", "trick", "fool",
  
  # --- Modalità e configurazioni ---
  "mode", "setting", "configuration", "option", "parameter",
  "developer mode", "test mode", "debug", "admin", "sudo",
  "jailbreak", "unlock", "unrestricted", "uncensored", "unfiltered"
)

# DIZIONARIO CONTEXTUAL MEMORY AMPLIATO
contextual_memory_keywords <- c(
  # --- Ruoli e personaggi ---
  "role", "character", "persona", "act", "pretend", "imagine",
  "roleplay", "you are", "behave as", "respond as", "speak as",
  "think like", "personality", "attitude", "style", "tone", "voice",
  "identity", "avatar", "embody", "impersonate", "simulate",
  
  # --- Scenari e contesti ---
  "scenario", "situation", "context", "setting", "environment",
  "world", "story", "narrative", "fiction", "game", "simulation",
  "experiment", "hypothetical", "what if", "suppose", "assume",
  "let's say", "imagine if", "pretend that", "in a world where",
  
  # --- Manipolazione conversazione ---
  "conversation", "chat", "dialogue", "discussion", "interaction",
  "previous", "earlier", "before", "continue", "follow up",
  "ignore previous", "forget everything", "disregard", "start over",
  "new conversation", "reset", "clear", "fresh start", "begin again",
  
  # --- Memoria e stato ---
  "session", "history", "store", "current", "now", "present",
  "today", "this moment", "right now", "immediately", "instantly",
  "temporary", "permanent", "persistent", "remember this", "keep in mind",
  
  # --- Framing e prospettive ---
  "frame", "perspective", "viewpoint", "angle", "approach",
  "interpret", "understand", "see it as", "consider it", "treat it",
  "from now on", "going forward", "henceforth", "starting now",
  
  # --- Prompt injection patterns ---
  "ignore above", "ignore below", "everything above", "everything before",
  "new instructions", "real instructions", "actual task", "true purpose",
  "hidden", "secret", "confidential", "between us", "don't tell"
)

cat("Dizionari ampliati creati con:\n")
cat("- Training Data:", length(training_data_keywords), "parole-chiave\n")
cat("- Fine-Tuning:", length(fine_tuning_keywords), "parole-chiave\n")
cat("- Contextual Memory:", length(contextual_memory_keywords), "parole-chiave\n\n")

# Funzione per calcolare i punteggi
calculate_category_score <- function(text, keywords) {
  text_lower <- tolower(text)
  matches <- sapply(keywords, function(keyword) {
    str_count(text_lower, fixed(tolower(keyword)))
  })
  return(sum(matches))
}

# Applichiamo la funzione a tutti i prompt
cat("Calcolo punteggi per tutti i", nrow(jailbreak_data), "prompt...\n")

jailbreak_data$training_data_score <- sapply(jailbreak_data$prompt, 
                                             function(x) calculate_category_score(x, training_data_keywords))

jailbreak_data$fine_tuning_score <- sapply(jailbreak_data$prompt, 
                                           function(x) calculate_category_score(x, fine_tuning_keywords))

jailbreak_data$contextual_memory_score <- sapply(jailbreak_data$prompt, 
                                                 function(x) calculate_category_score(x, contextual_memory_keywords))

cat("Punteggi calcolati!\n\n")

# ====================================================================
# FASE 3: CLASSIFICAZIONE EMERGENTE BASATA SU DIMENSIONI TEORICHE
# ====================================================================

cat("=== CLASSIFICAZIONE EMERGENTE ===\n")

# APPROCCIO 1: Classificazione per dimensione dominante
# Ogni prompt viene assegnato alla dimensione con punteggio più alto
classify_by_dominant_dimension <- function(training, finetuning, contextual) {
  # Se tutti i punteggi sono 0, è "No Attack"
  if (training == 0 & finetuning == 0 & contextual == 0) {
    return("No_Attack")
  }
  
  # Trova la dimensione con punteggio massimo
  scores <- c(training = training, finetuning = finetuning, contextual = contextual)
  max_score <- max(scores)
  dominant_dims <- names(scores)[scores == max_score]
  
  # Se c'è un pareggio, lo gestiamo
  if (length(dominant_dims) > 1) {
    # Se tutte e tre hanno lo stesso punteggio > 0, è multi-target
    if (length(dominant_dims) == 3) {
      return("Multi_Target_Equal")
    }
    # Se due hanno lo stesso punteggio, usiamo una logica di priorità
    if ("contextual" %in% dominant_dims) return("Contextual_Memory")
    if ("finetuning" %in% dominant_dims) return("Fine_Tuning") 
    return("Training_Data")
  }
  
  # Assegna alla dimensione dominante
  if (dominant_dims == "training") return("Training_Data")
  if (dominant_dims == "finetuning") return("Fine_Tuning")
  if (dominant_dims == "contextual") return("Contextual_Memory")
}

# Applica la classificazione
jailbreak_data$cluster_emergente <- mapply(classify_by_dominant_dimension,
                                           jailbreak_data$training_data_score,
                                           jailbreak_data$fine_tuning_score, 
                                           jailbreak_data$contextual_memory_score)

# APPROCCIO 2: Classificazione più sofisticata con soglie
# Consideriamo anche l'intensità e le combinazioni

classify_by_intensity_and_combination <- function(training, finetuning, contextual) {
  total_score <- training + finetuning + contextual
  
  # Nessun attacco se tutti i punteggi sono 0
  if (total_score == 0) {
    return("No_Attack")
  }
  
  # Soglia per considerare un punteggio "significativo" (>= 23)
  threshold <- 10
  
  significant_training <- training >= threshold
  significant_finetuning <- finetuning >= threshold  
  significant_contextual <- contextual >= threshold
  
  # Conta quante dimensioni hanno punteggi significativi
  significant_count <- sum(significant_training, significant_finetuning, significant_contextual)
  
  # Se più di una dimensione è significativa, è multi-target
  if (significant_count > 1) {
    if (significant_training & significant_finetuning & significant_contextual) {
      return("Multi_Target_Triple")
    } else if (significant_training & significant_finetuning) {
      return("Training_Finetuning_Combo")
    } else if (significant_training & significant_contextual) {
      return("Training_Contextual_Combo") 
    } else if (significant_finetuning & significant_contextual) {
      return("Finetuning_Contextual_Combo")
    }
  }
  
  # Attacco singola dimensione (o comunque una dominante)
  scores <- c(training = training, finetuning = finetuning, contextual = contextual)
  dominant_dim <- names(scores)[which.max(scores)]
  
  if (dominant_dim == "training") return("Training_Data_Focus")
  if (dominant_dim == "finetuning") return("Fine_Tuning_Focus")  
  if (dominant_dim == "contextual") return("Contextual_Memory_Focus")
}

# Applica la classificazione sofisticata
jailbreak_data$cluster_sofisticato <- mapply(classify_by_intensity_and_combination,
                                             jailbreak_data$training_data_score,
                                             jailbreak_data$fine_tuning_score,
                                             jailbreak_data$contextual_memory_score)

# ====================================================================
# FASE 4: ANALISI DEI CLUSTER EMERGENTI
# ====================================================================

cat("=== ANALISI CLUSTER EMERGENTI ===\n")

# Analisi classificazione semplice (dimensione dominante)
cat("CLASSIFICAZIONE SEMPLICE (Dimensione Dominante):\n")
cluster_simple_summary <- jailbreak_data %>%
  group_by(cluster_emergente) %>%
  summarise(
    n_prompt = n(),
    percentuale = round(n() / nrow(jailbreak_data) * 100, 2),
    media_training = round(mean(training_data_score), 2),
    media_finetuning = round(mean(fine_tuning_score), 2),
    media_contextual = round(mean(contextual_memory_score), 2),
    mediana_training = median(training_data_score),
    mediana_finetuning = median(fine_tuning_score),
    mediana_contextual = median(contextual_memory_score),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_prompt))

print(cluster_simple_summary)
cat("\n")

# Analisi classificazione sofisticata
cat("CLASSIFICAZIONE SOFISTICATA (Intensità e Combinazioni):\n")
cluster_sophisticated_summary <- jailbreak_data %>%
  group_by(cluster_sofisticato) %>%
  summarise(
    n_prompt = n(),
    percentuale = round(n() / nrow(jailbreak_data) * 100, 2),
    media_training = round(mean(training_data_score), 2),
    media_finetuning = round(mean(fine_tuning_score), 2), 
    media_contextual = round(mean(contextual_memory_score), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_prompt))

print(cluster_sophisticated_summary)
cat("\n")

# ====================================================================
# FASE 5: VISUALIZZAZIONI DEI CLUSTER EMERGENTI
# ====================================================================

cat("=== CREAZIONE VISUALIZZAZIONI ===\n")

# 1. Distribuzione cluster semplici
p1 <- ggplot(cluster_simple_summary, aes(x = reorder(cluster_emergente, n_prompt), 
                                         y = n_prompt, fill = cluster_emergente)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste(n_prompt, "\n(", percentuale, "%)", sep = "")), 
            hjust = -0.1, size = 3) +
  labs(title = "Cluster Emergenti - Classificazione per Dimensione Dominante",
       subtitle = paste("Dataset:", nrow(jailbreak_data), "prompt"),
       x = "Tipo di Attacco", y = "Numero di Prompt") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 2. Distribuzione cluster sofisticati  
p2 <- ggplot(cluster_sophisticated_summary, aes(x = reorder(cluster_sofisticato, n_prompt),
                                                y = n_prompt, fill = cluster_sofisticato)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste(n_prompt, "\n(", percentuale, "%)", sep = "")), 
            hjust = -0.1, size = 3) +
  labs(title = "Cluster Emergenti - Classificazione per Intensità e Combinazioni",
       subtitle = paste("Dataset:", nrow(jailbreak_data), "prompt"),
       x = "Tipo di Attacco", y = "Numero di Prompt") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 3. Scatter plot 3D con cluster emergenti
p3 <- ggplot(jailbreak_data, aes(x = training_data_score, y = fine_tuning_score, 
                                 color = cluster_emergente, size = contextual_memory_score)) +
  geom_point(alpha = 0.6) +
  labs(title = "Distribuzione Prompt per Punteggi e Cluster Emergenti",
       subtitle = "Cluster basati su dimensione teorica dominante",
       x = "Training Data Score", 
       y = "Fine-Tuning Score",
       size = "Contextual Memory Score",
       color = "Cluster Emergente") +
  theme_minimal() +
  theme(legend.position = "right")

# 4. Heatmap caratteristiche cluster semplici
cluster_simple_long <- cluster_simple_summary %>%
  select(cluster_emergente, media_training, media_finetuning, media_contextual) %>%
  pivot_longer(cols = c(media_training, media_finetuning, media_contextual), 
               names_to = "categoria", values_to = "punteggio") %>%
  mutate(categoria = case_when(
    categoria == "media_training" ~ "Training Data",
    categoria == "media_finetuning" ~ "Fine-Tuning", 
    categoria == "media_contextual" ~ "Contextual Memory"
  ))

p4 <- ggplot(cluster_simple_long, aes(x = categoria, y = cluster_emergente, fill = punteggio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(punteggio, 1)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Heatmap Caratteristiche Cluster Emergenti",
       subtitle = "Punteggi medi per categoria di dis-articolazione",
       x = "Categoria di Dis-articolazione", 
       y = "Cluster Emergente",
       fill = "Punteggio\nMedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostra i grafici
print(p1)
print(p2) 
print(p3)
print(p4)

# ====================================================================
# FASE 6: ESEMPI RAPPRESENTATIVI PER CLUSTER EMERGENTI
# ====================================================================

cat("\n=== ESEMPI RAPPRESENTATIVI ===\n")

# Funzione per ottenere esempi rappresentativi per cluster emergenti
get_examples_by_cluster <- function(data, cluster_col, cluster_value, n_examples = 3) {
  cluster_data <- data %>% filter(!!sym(cluster_col) == cluster_value)
  
  if (nrow(cluster_data) == 0) return(data.frame())
  
  # Ordina per punteggio totale decrescente per ottenere esempi più "intensi"
  cluster_data <- cluster_data %>%
    mutate(total_score = training_data_score + fine_tuning_score + contextual_memory_score) %>%
    arrange(desc(total_score)) %>%
    slice_head(n = min(n_examples, nrow(cluster_data)))
  
  return(cluster_data)
}

# Esempi per cluster semplici
cat("ESEMPI PER CLUSTER EMERGENTI (Classificazione Semplice):\n\n")
for (cluster_name in cluster_simple_summary$cluster_emergente) {
  cat("=== CLUSTER:", cluster_name, "===\n")
  
  cluster_info <- cluster_simple_summary %>% filter(cluster_emergente == cluster_name)
  cat("Dimensione:", cluster_info$n_prompt, "prompt (", cluster_info$percentuale, "%)\n")
  cat("Punteggi medi - Training:", cluster_info$media_training, 
      "| Fine-tuning:", cluster_info$media_finetuning,
      "| Contextual:", cluster_info$media_contextual, "\n\n")
  
  esempi <- get_examples_by_cluster(jailbreak_data, "cluster_emergente", cluster_name, 2)
  
  for (i in 1:nrow(esempi)) {
    cat("Esempio", i, "(Punteggi T:", esempi$training_data_score[i], 
        "F:", esempi$fine_tuning_score[i], "C:", esempi$contextual_memory_score[i], "):\n")
    cat(substr(esempi$prompt[i], 1, 300), "...\n\n")
  }
  cat("---\n\n")
}

# ====================================================================
# FASE 7: EXPORT RISULTATI
# ====================================================================

cat("=== EXPORT RISULTATI ===\n")

# Prepariamo il dataset per l'export
jailbreak_data_export <- jailbreak_data %>%
  mutate(
    prompt_length = nchar(prompt),
    prompt_truncated = ifelse(nchar(prompt) > 32000, "YES", "NO"),
    prompt = ifelse(nchar(prompt) > 32000, 
                    paste0(substr(prompt, 1, 32000), "... [TRUNCATED]"), 
                    prompt)
  ) %>%
  select(
    # Informazioni originali
    platform, source, jailbreak, created_at, date, community, community_id, previous_community_id,
    # Punteggi delle dimensioni
    training_data_score, fine_tuning_score, contextual_memory_score,
    # Cluster emergenti
    cluster_emergente, cluster_sofisticato,
    # Informazioni aggiuntive
    prompt_length, prompt_truncated,
    # Prompt (alla fine per facilità di lettura)
    prompt
  )

# Salvataggio file principale
output_path <- "C:/Users/morse/OneDrive/Desktop/prova/DATASET_CLUSTER_EMERGENTI.xlsx"
writexl::write_xlsx(jailbreak_data_export, output_path)
cat("Dataset con cluster emergenti salvato in:", output_path, "\n")

# Salvataggio summary cluster semplici
summary_simple_path <- "C:/Users/morse/OneDrive/Desktop/prova/SUMMARY_CLUSTER_SEMPLICI.xlsx"
writexl::write_xlsx(cluster_simple_summary, summary_simple_path)
cat("Summary cluster semplici salvato in:", summary_simple_path, "\n")

# Salvataggio summary cluster sofisticati
summary_sophisticated_path <- "C:/Users/morse/OneDrive/Desktop/prova/SUMMARY_CLUSTER_SOFISTICATI.xlsx"
writexl::write_xlsx(cluster_sophisticated_summary, summary_sophisticated_path)
cat("Summary cluster sofisticati salvato in:", summary_sophisticated_path, "\n")

# Versione CSV completa (senza troncature)
csv_path <- "C:/Users/morse/OneDrive/Desktop/prova/DATASET_CLUSTER_EMERGENTI.csv"
write.csv(jailbreak_data, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Dataset completo (CSV) salvato in:", csv_path, "\n")

# ====================================================================
# RISULTATI FINALI E INTERPRETAZIONE
# ====================================================================

cat("\n=== RISULTATI FINALI ===\n")
cat("✓ Analizzati", nrow(jailbreak_data), "prompt jailbreak\n")
cat("✓ Creati cluster EMERGENTI basati sulle dimensioni teoriche\n")
cat("✓ Due approcci di classificazione: semplice e sofisticato\n")
cat("✓ Visualizzazioni che mostrano la distribuzione naturale\n")
cat("✓ Esempi rappresentativi per ogni cluster\n\n")

cat("INTERPRETAZIONE CLUSTER EMERGENTI:\n")
cat("• I cluster NON sono imposti arbitrariamente\n")
cat("• Ogni prompt è classificato in base a quale dimensione 'attacca' di più\n")
cat("• La distribuzione dei cluster riflette le strategie reali dei jailbreak\n")
cat("• Emergono naturalmente cluster per attacchi combinati\n\n")

# Statistiche finali sulla distribuzione
cat("DISTRIBUZIONE FINALE (Classificazione Semplice):\n")
total_prompts <- nrow(jailbreak_data)
for (i in 1:nrow(cluster_simple_summary)) {
  cluster_info <- cluster_simple_summary[i,]
  cat("•", cluster_info$cluster_emergente, ":", cluster_info$n_prompt, "prompts (", 
      cluster_info$percentuale, "%)\n")
}

cat("\nAnalisi completata con successo!")
cat("\nI cluster ora emergono dalle dimensioni teoriche invece di essere imposti artificialmente!")