################################ TOPIC MODELING ################################
### Abbildung 5-7: Topic Modeling des Neuen Pitaval
### Vgl. Weitin, T. (2021). Quantitative Semantik. 
### In: Digitale Literaturgeschichte. Digitale Literaturwissenschaft. 
### https://doi.org/10.1007/978-3-662-63663-3_6
#------------------------------------------------------------------------------#

### Fuehrt ein Topic Modeling fuer ein Korpus durch.
###
### Ordnerstrukturen:
###
### data
###   |_stoplist_gesamt.txt
###
### corpora
###   |_DerNeuePitaval
###
###  results 
###   |_[Ergebnisordner werden automatisch erstellt]


#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

options(java.parameters = "-Xmx1024m") 
# muss definiert werden, bevor Packages geladen werden

#install.packages("gtools")
#install.packages("mallet")
#install.packages("tokenizers")
#install.packages("wordcloud")
#install.packages("xlsx")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("reshape")
#install.packages("tibble")

library(gtools)
library(mallet)
library(tokenizers)
library(wordcloud)
library(xlsx)
library(ggplot2)
library(dplyr)
library(reshape)
library(tibble)

#------------------------------------------------------------------------------#
## Funktionen -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

# makeFlexTextChunks()
# 
# Die Funktion makeFlexTextChunks() teilt Character-Vektoren variabel in Chunks
# auf.
# @param doc_object Text als Character-Vektor (character)
# @param chunk_size Anzahl bzw. Laenge der Chunks (numeric)
# @param percentage Soll chunk_size die Anzahl oder Laenge der Chunks festlegen
#                   (logical)
# @return in Chunks aufgeteilter Text (matrix) 

makeFlexTextChunks <- function (doc_object, chunk_size = 10, percentage = TRUE) {
  if (is.character(text_v) == FALSE) {
    stop("Die Texte wurden nicht korrekt eingelesen!")
  }
  if (is.numeric(chunk_size) == FALSE) {
    stop("Die Variable chunk_size muss numerisch sein!")
  }
  if (is.logical(percentage) ==FALSE) {
    stop("Die Variable percentage muss TRUE oder FALSE sein!")
  } 
  # Die Texte werden in einen einzigen Character-Vektor ohne Zeilenbrueche 
  # umgewandelt.
  words_v <- paste(doc_object, collapse=" ")
  # Umwandlung in Kleinbuchstaben
  words_lower_v <- tolower(words_v)
  # Interpunktionszeichen etc. werden durch Leerzeichen ersetzt
  words_regex_v <- gsub("[^[:alnum:][:space:]'-]", " ", words_lower_v)
  # Character-Vektor wird nach Leerzeichen aufgeteilt
  words_l <- strsplit(words_regex_v, "\\s+")
  word_v <- unlist(words_l)
  # Die Positionen der Woerter werden in einer Liste gespeichert
  x_i <- seq_along(word_v)
  # Wenn percentage == TRUE, ist die Laenge der Chunks variabel; 
  # chunk_size definiert in diesem Fall die Anzahl der Chunks
  if (percentage == TRUE) {
    # Maximale Laenge der Chunks festlegen
    max_length <- length(word_v)/chunk_size
    chunks_l <- split(word_v, ceiling(x_i/max_length)) 
    # ceiling --> aufrunden
  # Wenn percentage == FALSE, ist die Anzahl der Chunks variabel; chunk_size
  # definiert in diesem Fall die Laenge der Chunks
  } else {
    # Ueberpruefen, ob der Text laenger als chunk_size ist
    if (length(word_v) > chunk_size) {
      chunks_l <- split(word_v, ceiling(x_i/chunk_size))
      # Ueberpruefung des letzten Chunks:
      # Ist der letzte Chunk kuerzer als die Haelfte der definierten chunk_size,
      # werden die Woerter des letzten Chunks zum vorletzten Chunk hinzugefuegt 
      # und der letzte Chunk geloescht.
      if (length(chunks_l[[length(chunks_l)]]) <= chunk_size/2) {
        chunks_l[[length(chunks_l)-1]] <- c(chunks_l[[length(chunks_l)-1]],
                                            chunks_l[[length(chunks_l)]])
        chunks_l[[length(chunks_l)]] <- NULL
      }
    } else {
      # Wenn der Text kuerzer als die chunk_size ist, ist der gesamte Text ein
      # Chunk
      chunks_l <- split (word_v, ceiling (x_i/chunk_size))
    }
  }
  chunks_l <- lapply(chunks_l, paste, collapse = " ")
  chunks_m <- do.call(rbind, chunks_l)
  return (chunks_m)
}

#------------------------------------------------------------------------------#
## Parameter ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

num_topics <- 100
iteration <- 10000
chunk_size <- 500
num_topic_words <- 100

filename <- paste0("TM_",
                   chunk_size,
                   "chunksize_",
                   num_topics,
                   "numtopics_",
                   iteration,
                   "iteration_",
                   num_topic_words,
                   "twords_")

corpus <- "DerNeuePitaval"

# Dateipfade anpassen
path <- "...\\corpora"
path_results <-  "...\\results"
path_data <-  "...\\data"

path_corpus <- paste0(path, "\\", corpus)
output_dir <- paste0(path_results, "\\TopicModeling\\", corpus, "\\", num_topics, 
                     "Topics")

if(!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}


#------------------------------------------------------------------------------#
## Preprocessing --------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd(path_corpus)

corpus_files_v <- dir(path = path_corpus, "\\.txt$")
len <- length(corpus_files_v)
corpus_files_v <- mixedsort(corpus_files_v)

# Aufteilung der Texte in Chunks
topic_m <- NULL
texttitle <- rep("", length(corpus_files_v))
for (i in 1:length(corpus_files_v)) {
  # Texte werden in Absaetzen aufgeteilt eingelesen
  text_v <- scan(file.path(path_corpus, corpus_files_v[i]),
                 what = "character", sep = "\n" , encoding = "UTF-8", quiet = TRUE)
  chunk_m <- makeFlexTextChunks(text_v, chunk_size, percentage = FALSE)
  # Dateiendungen werden geloescht um Textnamen zu erhalten
  textname <- gsub("\\..*","", corpus_files_v[i]) 
  # Unterstriche koennen zu Problemen fuehren, wenn die Chunks wieder kombiniert
  # werden - sie werden deshalb entfernt.
  textname <- gsub("_", "", textname) 
  texttitle[i] <- paste(textname, collapse = " ")
  segments_m <- cbind(paste(textname, segment = 1:nrow(chunk_m), sep="_"), 
                      chunk_m)
  # alle Chunks werden in einer Matrix gesammelt
  topic_m <- rbind(topic_m, segments_m)
}

# Umwandlung in ein Dataframe
documents <- as.data.frame(topic_m, stringsAsFactors = FALSE)
colnames(documents) <- c("id", "text")

# Einlesen der Stopword-Liste
stopwords <- paste0(path_data ,"\\stoplist_gesamt.txt")

#------------------------------------------------------------------------------#
## Initiierung des Topic Model Trainer ----------------------------------------#
#------------------------------------------------------------------------------#

mallet_instances <- mallet.import(documents$id,
                                  documents$text,
                                  stopwords,
                                  FALSE,
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# Erstellung des Trainers mit festgelegter Anzahl von Topics
topic_model <- MalletLDA(num_topics)

# Textdaten werden geladen
topic_model$loadDocuments(mallet_instances)

# Alle vorkommenden Types werden inklusive Haeufigkeit abgespeichert
vocabulary <- topic_model$getVocabulary()
word_freqs <- mallet.word.freqs(topic_model)
write.table(word_freqs, paste0(output_dir, "\\", filename, 
                              "_wordfreqs.csv"), dec = ".", sep = " ")

#------------------------------------------------------------------------------#
## Training -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Verfeinerung der Hyperparameter: optimization interval, burn-in iterations
topic_model$setAlphaOptimization(20, 80)

# Training des Models & Festlegung der Iterationsanzahl
topic_model$train(iteration)

# Waehlt das passenste Topic fuer jedes Type
topic_model$maximize(10)


#------------------------------------------------------------------------------#
## Wortgewichte ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Erstellung einer Matrix, in der jede Reihe ein Topic und jede Spalte ein 
# Type ist
topic_words_m <- mallet.topic.words(topic_model,
                                    smoothed = TRUE,
                                    normalized = TRUE)
colnames(topic_words_m) <- vocabulary

# Extrahieren der Top Words und Wortgewichte
topic_word_weight <- NULL
for (i in 1: num_topics) {
  topic_top_words <- mallet.top.words(topic_model,
                                      topic_words_m[i,], num_topic_words)
  topic_wrd <- cbind(paste("Topic ", i), rbind(topic_top_words$words))
  topic_wght <- cbind(paste("Word Weights Topic", i), 
                      rbind(topic_top_words$weights))
  topic_ww <- rbind(topic_wrd, topic_wght)
  topic_word_weight <- rbind(topic_word_weight, topic_ww)
}

topic_word_weight <- t(topic_word_weight)
topic_word_weight_df <- as.data.frame(topic_word_weight, 
                                      stringsAsFactors = FALSE)
colnames(topic_word_weight_df) <- topic_word_weight_df[1,]
colnames_df <- colnames(topic_word_weight_df)
topic_word_weight_df <- topic_word_weight_df[-1,]

# Jede zweite Spalte wird in eine Spalte von numerischen Werten umgewandelt
for (i in 1:ncol(topic_word_weight_df)) {
  if (i %% 2){
    next
  }
  topic_word_weight_df[, i] <- as.numeric(topic_word_weight_df[, i])
}

write.table(topic_word_weight_df,
           paste0(output_dir, "\\", filename, 
                  "_wordweight.csv"), dec = ".", sep = " ")

# Berechnung der normalisierten Wortgewichte
# Erstellung einer Liste, in der die Summen der Wortgewichte gespeichert werden
ls <- list()
for (i in 1:ncol(topic_word_weight_df)) {
  if (class(topic_word_weight_df[,i]) == "character"){
    ls[i] <- 0
  } else {
    ls[i] <- sum(topic_word_weight_df[,i])
  }
}

# Liste wird als Reihe zum Dataframe hinzugefuegt
topic_word_weight_df[num_topic_words+1,] <- ls

# Summe der Wortgewichte wird zu Normalisierung der Werte verwendet
normalised_word_weight_df <- topic_word_weight_df
for (i in 1:ncol(normalised_word_weight_df)) {
  if (i %% 2 == 0) {
    normalised_word_weight_df[,i] <- (normalised_word_weight_df[,i]/
                                        normalised_word_weight_df
                                      [num_topic_words+1,i])*100
  }
}

write.table(normalised_word_weight_df,
            paste0(output_dir, "\\", filename, 
                   "_normalised_wordweight.csv"), dec = ".", sep = " ")

#------------------------------------------------------------------------------#
## Topic-Text-Gewichte --------------------------------------------------------#
#------------------------------------------------------------------------------#

# Dataframe von Topic-Chunk-Gewichten (Spalten = Topics, Reihen = Chunks)
doc_topics_m <- mallet.doc.topics(topic_model,
                                  smoothed = TRUE,
                                  normalized = TRUE)
doc_topics_df <- as.data.frame(doc_topics_m)

# Extraktion der Text- und Chunknamen
file_ids_v <- documents[,1]
file_id_l <- strsplit(as.character(file_ids_v), "_")
file_chunk_id_l <- lapply(file_id_l, rbind)
file_chunk_id_m <- do.call(rbind, file_chunk_id_l)

# Kombination der Textnamen mit den Topic-Chunk-Gewichten
doc_topics_df <- cbind(file_chunk_id_m[,1], doc_topics_df)

# Berechnung des Mittelwerts der Chunks eines Texts
doc_topic_means_df <- aggregate(doc_topics_df[, 2:ncol(doc_topics_df)],
                                list(doc_topics_df[,1]),
                                mean)

# Urspruengliche Reihenfolge der Texte herstellen
doc_topic_means_order_df <- doc_topic_means_df[order(order(texttitle)),]

colnames(doc_topic_means_order_df) <- c(c("Text"), sprintf("Topic%s", 
                                                           1:num_topics))

# Unterstriche wieder einfuegen
doc_topic_means_order_df$Text <- gsub("([A-Z][a-z]+?)([A-Z].+)(\\d{4})", 
                                      "\\1_\\2_\\3", 
                                      doc_topic_means_order_df$Text, 
                                      perl = TRUE)

write.table(doc_topic_means_order_df,
           paste0(output_dir, "\\", filename, "topics-in-docs.csv"),
           row.names = FALSE, dec = ".", sep = " ")

#------------------------------------------------------------------------------#
## Wordclouds -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Wordclouds mit Top 25 Woertern
new_pdf <- paste0(output_dir, "\\Wordclouds_allTopics_25.pdf")
my_plots <- vector(num_topics, mode='list')

for(i in 1:num_topics){
  topic_top_words <- mallet.top.words(topic_model,
                                      topic_words_m[i,], 25)
  
  wordcloud(topic_top_words$term,
            topic_top_words$weight,
            scale = c(1,.25),
            min.freq = 1, max.words = 100, rot.per=0,
            random.order=FALSE)
  my_plots[[i]] <- recordPlot()
  
}

graphics.off()

pdf(new_pdf, 4, 4, onefile=TRUE)

for (my_plot in my_plots) {
  replayPlot(my_plot)
}
graphics.off()

# Wordclouds mit Top 100 Woertern
new_pdf1 <- paste0(output_dir, "\\Wordclouds_allTopics_100.pdf")
my_plots <- vector(num_topics, mode='list')

for(i in 1:num_topics){
  topic_top_words <- mallet.top.words(topic_model,
                                      topic_words_m[i,], 100)
  
  wordcloud(topic_top_words$term,
            topic_top_words$weight,
            scale = c(1,.19),
            min.freq = 1, max.words = 100, rot.per=0,
            random.order=FALSE,
            use.r.layout=FALSE)
  my_plots[[i]] <- recordPlot()
  
}

graphics.off()

pdf(new_pdf1, 4, 4, onefile=TRUE)

for (my_plot in my_plots) {
  replayPlot(my_plot)
}
graphics.off()


#------------------------------------------------------------------------------#
## Profile --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Darstellung der Topic-Verteilung in den jeweiligen Texten als Balkendiagramme
# ein Diagramm pro Text
file <- paste0(output_dir, "\\", filename, "topics-in-docs.csv")

topicswithnames <- read.table(file, sep = " ", dec = ".", header = TRUE)
names <- topicswithnames[,1]
topicswith_nonames <- topicswithnames[,2:length(topicswithnames)]

pdf_topics_texts <- paste0(output_dir, "\\TopicsPerText_", corpus, ".pdf")
topics_text_plots <- list()

for (i in 1:len){
  name <- gsub("Bd(\\d+)(18\\d{2})(\\d+)(.+?)(\\d+)", 
               'Topic-Verteilung in "\\4" (Band \\1, \\2)', 
               names[i])
  name <- gsub("(-|_)", " ", name)
  name <- gsub("([a-z])([A-Z])", "\\1 \\2", name)
  subrow <- as.data.frame(t(
    as.matrix(topicswith_nonames[i, 1:length(topicswith_nonames)])))
  subrow$Topic <- as.factor(gsub("Topic(\\d+)", "\\1", rownames(subrow)))
  subrow$Topic <- reorder(subrow$Topic, c(1:num_topics))
  rownames(subrow) <- NULL
  colnames(subrow) <- c("Gewicht", "Topics")
  plot <- ggplot(subrow, aes(x = Gewicht, y = Topics)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.title.y = element_blank(),
                plot.title = element_text(hjust = 0.5)) +
          coord_flip() +
          labs(title = name)
  topics_text_plots[[i]] <- plot
}

graphics.off()

pdf(pdf_topics_texts, 20, 15, onefile=TRUE)
for (my_plot in topics_text_plots) {
  print(my_plot)
}
graphics.off()

# Darstellung der Anteile der Texte an jedem Topic als Balkendiagramme
# ein Diagramm pro Topic
pdf_texts_topics <- paste0(output_dir, "\\TextsPerTopic_", corpus, ".pdf")
texts_topic_plots <- list()
dates <- as.numeric(gsub("Bd\\d+(18\\d{2})(\\d{1,2}).+", "\\1\\2", names))
year <- as.numeric(gsub("(\\d{4})(\\d{1,2})", "\\1", dates))
num <- as.numeric(gsub("(\\d{4})(\\d{1,2})", "\\2", dates))
breaks <- c("1842", "1845", "1850", "1855", "1860", "1865", "1870", "1875", 
            "1880", "1886", "1890")

for (i in 1:length(topicswith_nonames)){
  label <- paste0("Anteile aller Texte an Topic ", i)
  subcolumn <- as.data.frame(
    topicswith_nonames[1:nrow(topicswith_nonames), i])
  subcolumn$Jahr <- as.factor(year)
  subcolumn$Text <- as.factor(num)
  subcolumn$Jahr <- reorder(subcolumn$Jahr, year)
  subcolumn$Text <- reorder(subcolumn$Text, num)
  subcolumn$ID <- as.factor(names)
  subcolumn$ID <- reorder(subcolumn$ID, year)
  ids <- subcolumn$ID[subcolumn$Text==1 & subcolumn$Jahr %in% breaks]
  remove <- c(2,4,6)
  ids <- ids[-remove]
  select <- subcolumn$Jahr[subcolumn$ID %in% ids]
  
  colnames(subcolumn) <- c("Gewicht", "Jahr", "Text", "ID")
  plot <- ggplot(subcolumn, aes(x = ID, y = Gewicht)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 30),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(breaks = ids,
                     labels = select) +
    labs(title = label)
  texts_topic_plots[[i]] <- plot
}

graphics.off()

pdf(pdf_texts_topics, 50, 25, onefile=TRUE)
for (my_plot in texts_topic_plots) {
  print(my_plot)
}
graphics.off()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#