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
spacy_install()

# Importo i dati
df <- read_excel("C:/Users/Utente/OneDrive/DOTTORATO/Progetto di ricerca/Analisi review in R/bib.xlsx", sheet = "clear")
# Creo il dataframe con gli abstract
df_abstract <- data.frame(AB = df$AB)
# Inizializza spaCy specificando il modello linguistico inglese
spacy_initialize(model = "en_core_web_sm")
# Preparazione dei dati per spacyr
df_abstract$text <- as.character(df_abstract$AB)
df_abstract$text <- sapply(df_abstract$text, trimws)
# Lemmatizzazione con spacyr
lemmatized <- spacy_parse(df_abstract$text, lemma = TRUE)
lemmatized$lemma <- sapply(lemmatized$lemma, trimws)
# Aggregazione dei lemmi in una singola stringa per documento
lemmatized$text <- lemmatized$lemma
aggregated_texts <- lemmatized %>%
  group_by(doc_id) %>%
  summarise(text = paste(text, collapse=" ")) %>%
  ungroup()
# Crea il corpus dal testo lemmatizzato aggregato
corpus <- Corpus(VectorSource(aggregated_texts$text))

# Esegui processi si pulizia dei testi
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
termini_specifici <- c()  # Aggiungi qui i termini da escludere
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), termini_specifici))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) trimws(x)))

# Creazione della matrice dei termini
dtm <- DocumentTermMatrix(corpus)

# Creazione del modello di topic
set.seed(123) # Per la riproducibilità dei risultati
lda_model <- LDA(dtm, k = 4, method = "Gibbs", control = list(seed = 123, 
                                                              burnin = 50000, iter = 1000000, thin = 1000))
terms(lda_model, 30) # Visualizza i primi termini
topics(lda_model) # Visualizza la collocazione

# Visualizzazione
library(LDAvis)
# serVis(json_lda)
json_lda <- createJSON(phi = posterior(lda_model)$terms, 
                       theta = posterior(lda_model)$topics, 
                       doc.length = rowSums(as.matrix(dtm)), 
                       vocab = colnames(as.matrix(dtm)), 
                       term.frequency = colSums(as.matrix(dtm)))
serVis(json_lda)

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
