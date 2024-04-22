# Carico le librerie necessarie
library(readxl)
library(tm)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(topicmodels)
library(LDAvis)
library(reshape2)
library(pheatmap)
library(spacyr)
library(writexl)

# Installazione e inizializzazione di spaCy
spacy_install()
spacy_initialize(model = "en_core_web_sm")

# Importo i dati
df <- read_excel("C:/Users/Utente/OneDrive/DOTTORATO/Systematic literature review/Review protocol files/bib.xlsx", sheet = "bib")
abstracts <- data.frame(AB = df$AB)

# Lemmatizzazione con spacyr
seed = 123
lemmatized <- spacy_parse(abstracts$AB, lemma = TRUE)

# Aggregazione dei lemmi in una singola stringa per documento
seed = 123
aggregated_texts <- lemmatized %>%
  group_by(doc_id) %>%
  summarise(text = paste(lemma, collapse=" ")) %>%
  ungroup()

# Crea il corpus dal testo lemmatizzato aggregato
seed = 123
docs <- Corpus(VectorSource(aggregated_texts$text))

# Aggiunge parole specifiche alla lista delle stopwords ("paper", "research", "study", "case", "literature", "article", "findings")
seed = 123
custom_stopwords <- c(stopwords("english"), "paper", "article", "research")

# Pre-processing
seed = 123
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, custom_stopwords)  # Usa la lista aggiornata qui
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))

# Verifica il testo trasformato per i primi documenti
inspect(docs[1:5])

# Genero la dtm
seed = 123
dtm <- DocumentTermMatrix(docs)

# Lda
set.seed(123)
lda_model <- LDA(dtm, k = 5, method = "Gibbs", set.seed(123))

# Esplorazione
seed = 123
topics <- terms(lda_model, 30) # Ottiene le prime 10 parole per ogni topic
topics

# Assegnazione dei topic ai documenti (corretto)
doc_topics <- posterior(lda_model)$topics

# Visualizzazioni

# SerVis(json_lda)
seed = 123
json_lda <- createJSON(phi = posterior(lda_model)$terms, 
                       theta = posterior(lda_model)$topics, 
                       doc.length = rowSums(as.matrix(dtm)), 
                       vocab = colnames(as.matrix(dtm)), 
                       term.frequency = colSums(as.matrix(dtm)))
serVis(json_lda)
serVis(json_lda, out.dir = 'C:/Users/Utente/OneDrive/DOTTORATO/Systematic literature review/R analysis/docs', open.browser = TRUE)

# Heatmap con probabilità documento-topic
topic_distribution <- as.matrix(doc_topics)
pheatmap(topic_distribution, 
         color = colorRampPalette(c("blue", "yellow"))(100), 
         legend = TRUE, 
         show_rownames = TRUE,
         angle_col = 0,
         main = "Topic Distribution across Documents",
         cluster_cols = FALSE)

# Converti topic_distribution in un dataframe
df_topic_distribution <- as.data.frame(topic_distribution)
# Aggiungi una colonna con i numeri dei documenti
# Puoi usare rownames() per ottenere i numeri dei documenti se sono stati definiti come tali
df_topic_distribution$Document_Number <- rownames(df_topic_distribution)
# Riordina il dataframe per mettere la colonna dei numeri dei documenti come prima colonna
df_topic_distribution <- df_topic_distribution[, c(ncol(df_topic_distribution), 1:(ncol(df_topic_distribution)-1))]
# Converti le probabilità in percentuali, se necessario
df_topic_distribution[,2:6] <- df_topic_distribution[,2:6] * 100
# Rinomina le colonne per chiarezza
colnames(df_topic_distribution) <- c("Document_Number", "Topic 1 (%)", "Topic 2 (%)", "Topic 3 (%)", "Topic 4 (%)", "Topic 5 (%)")
# Visualizza le prime righe della tabella per verifica
head(df_topic_distribution)
