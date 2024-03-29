# Caricamento delle librerie
library(readxl)
library(topicmodels)
library(tm)
library(dplyr)
library(spacyr)
library(slam)
library(ldatuning)
library(tidytext)
library(tidyverse)
library(stringr)
library(gutenbergr)
library(pheatmap)
library(writexl)

# Installazione e inizializzazione di spaCy
spacy_install()
spacy_initialize(model = "en_core_web_sm")

# Imposta il seed per la riproducibilità all'inizio dello script
set.seed(123)

# Importo i dati
df <- read_excel("C:/Users/Utente/OneDrive/DOTTORATO/Progetto di ricerca/Analisi review in R/bib.xlsx", sheet = "clear")

# Creo il dataframe con gli abstract
df_abstract <- data.frame(AB = df$AB)

# Preparazione dei dati per spacyr
df_abstract$text <- as.character(df_abstract$AB)
df_abstract$text <- sapply(df_abstract$text, trimws)

# Lemmatizzazione con spacyr
lemmatized <- spacy_parse(df_abstract$text, lemma = TRUE)
lemmatized$lemma <- sapply(lemmatized$lemma, trimws)

# Aggregazione dei lemmi in una singola stringa per documento
aggregated_texts <- lemmatized %>%
  group_by(doc_id) %>%
  summarise(text = paste(lemma, collapse=" ")) %>%
  ungroup()

# Crea il corpus dal testo lemmatizzato aggregato
corpus <- Corpus(VectorSource(aggregated_texts$text))

# Esegui processi di pulizia dei testi
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
termini_specifici <- c()  # Aggiungi qui i termini da escludere
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), termini_specifici))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(function(x) trimws(x)))

# Creazione della matrice dei termini
dtm <- DocumentTermMatrix(corpus)

# Esecuzione della ricerca del numero di topic ottimale senza parallelismo
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  verbose = TRUE)

# Plot dei risultati per trovare il numero ottimale di topic
FindTopicsNumber_plot(result)

# Creazione del modello LDA con il numero di topic deciso
# Nota: è importante rieffettuare set.seed() qui se hai eseguito altre operazioni casuali nel frattempo
set.seed(123)
lda_model <- LDA(dtm, k = 4, method = "Gibbs", control = list(seed = 123, burnin = 50000, iter = 1000000, thin = 1000))
terms(lda_model, 30) # Visualizza i primi termini per topic
topics(lda_model) # Visualizza la distribuzione dei topic nei documenti

# Ottieni la distribuzione dei topic nei documenti
topic_distribution <- posterior(lda_model)$topics
# Esempio di rinominazione basato su un'interpretazione dei topic
colnames(topic_distribution) <- c("Measurement perspective", "Decision making analysis", "Method and framework", "Community management")
# Usa pheatmap con le nuove etichette
pheatmap(topic_distribution, 
         color = colorRampPalette(c("blue", "yellow"))(100), 
         legend = TRUE, 
         show_rownames = TRUE,
         angle_col = 0,
         main = "Topic Distribution across Documents",
         cluster_cols = FALSE)