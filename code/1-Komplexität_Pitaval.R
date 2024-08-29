#################### LESBARKEIT & LEXIKALISCHE DIVERSITÄT  I ####################
#------------------------------------------------------------------------------#

### Erhebung und Visualisierung
## Abb. 1: Wiener Sachtextformel 4 des Neuen Pitaval ueber saemtliche Baende

#------------------------------------------------------------------------------#

### Ordnerstrukturen:
###
### corpora
###     |_DerNeuePitaval
### results
###     |_[Ergebnisordner wird automatisch erstellt]

#------------------------------------------------------------------------------#
## Packages -------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Installieren der erforderlichen Programme
#install.packages("Rtools")
#install.packages("readtext")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("ggsci")
#install.packages("grid")
#install.packages("gtools")
#install.packages("quanteda.textstats")
#install.packages("quanteda")

# Laden der erforderlichen Packages
library(readtext)
library(ggplot2)
library(tidyverse)
library(ggsci)
library(grid)
library(gtools)
library(quanteda.textstats)
library(quanteda)

#------------------------------------------------------------------------------#
## Parameter und Pfade---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Visualisierungstyp
theme_set(theme_light())

# Dateipfade anpassen
path_corpus <- "...\\corpora\\DerNeuePitaval"
path_results <- "...\\results"

# Ergebnis-Unterordner erstellen
path_results <- paste0(path_results, "\\Komplexitaet")
if(!dir.exists(path_results)){
  dir.create(path_results, recursive = TRUE)
}

# Einlesen des Korpus
text <- readtext(path_corpus)
text_corpus <- corpus(text)

#------------------------------------------------------------------------------#
## Textstatistiken mit quanteda.textstats--------------------------------------#
#------------------------------------------------------------------------------#

# Berechnung des Lesbarkeitsindex (Neue Wiener Sachtextformeln 4)
# vgl. https://quanteda.io/reference/textstat_readability.html
readability <- textstat_readability(text_corpus, measure ="nWS.4")

# Berechnen der lexikalischen Vielfalt (Type-Token-Ratio)
# vgl. https://quanteda.io/reference/textstat_lexdiv.html?q=textstat%20_%20lexdiv
all_freqs <- tokens(text_corpus, remove_punct = TRUE,
                    remove_numbers = TRUE)
measures <- textstat_lexdiv(all_freqs, measure = "TTR")

# Zusammenfuehren der Werte und Berechnung der Mittelwerte
combined_measures <- merge(readability, measures, by = "document")
combined_measures$text <- as.factor(gsub("(.+?)_.+", "\\1", 
                                         combined_measures$document))

agg <- aggregate(combined_measures$nWS.4, list(combined_measures$text), 
                 mean)

combined_measures <- merge(combined_measures, agg, by.x = "text", 
                           by.y = "Group.1")

agg_TTR <- aggregate(combined_measures$TTR, list(combined_measures$text), 
                     mean)

combined_measures <- merge(combined_measures, agg_TTR, by.x = "text", 
                           by.y = "Group.1")

# Berechnung des Lesbarkeitsdurchschnitts aller Fallgeschichten eines Bandes
avg <- 
  combined_measures %>%
  summarize(avg = mean(nWS.4, na.rm = TRUE)) %>%
  pull(avg)

combined_measures$id <- as.factor(gsub("(Bd\\d+_\\d+_\\d+).+", "\\1", 
                                       combined_measures$document))
df_sorted <- combined_measures[mixedorder(as.character(combined_measures$id)),]
df_sorted$id <- factor(df_sorted$id, levels = df_sorted$id)
df_sorted$text <- factor(df_sorted$text, levels = unique(df_sorted$text), 
                         ordered = TRUE)

#------------------------------------------------------------------------------#
## Visualisierung der Daten----------------------------------------------------#
#------------------------------------------------------------------------------#

# Grafik: Balkendiagramm des nWs-Index der Fallgeschichten nach Baenden
# Abbildung der Durchschnittswerte pro Band
# helle Datenpunkte fuer Einzeltexte, dunkle Datenpunkte fuer Baende 
# Groeße der Datenpunkte nach Type-Token-Ratio der Fallgeschichten skaliert

df_sorted %>%
  ggplot(aes(text, nWS.4, color = text)) +   
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_hline(aes(yintercept = avg), color = "gray70", size = 0.6) +
  geom_jitter(aes(size = TTR), alpha = 0.25, width = 0.2) +   
  scale_size(range = c(0, 5)) +
  guides(size = guide_legend()) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_segment(aes(x = text, xend = text,
                   y = avg, yend = x.x), size = 0.8) 

ggsave(file.path(path_results, "nWS4_score_Pitaval.png"), width = 50, 
       height = 25, units = "cm", dpi = 300)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
