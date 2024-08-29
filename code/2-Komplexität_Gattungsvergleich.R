################## LESBARKEIT & LEXIKALISCHE DIVERSITÄT  II ####################
#------------------------------------------------------------------------------#

### Erhebung und Visualisierung
## Abb. 2: Gattungsvergleich Deutscher Novellenschatz, Gartenlaube, Neuer Pitaval

#------------------------------------------------------------------------------#

### Ordnerstrukturen:
###
### corpora
###     |_Vergleich_Pitaval-Gartenlaube-Novellenschatz
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
path_corpus <- "...\\corpora\\Vergleich_Pitaval-Gartenlaube-Novellenschatz"
path_results <- "...\\results"

# erstellter Ergebnis-Unterordner
path_results <- paste0(path_results, "\\Komplexitaet")
if(!dir.exists(path_results)){
  dir.create(path_results, recursive = TRUE)
}

text <- readtext(path_corpus)
text_corpus <- corpus(text)

#------------------------------------------------------------------------------#
## Textstatistiken mit quanteda.textstats--------------------------------------#
#------------------------------------------------------------------------------#

# Berechnung der Lesbarkeit: Flesch und Neue Wiener Sachtextformel
readability <- textstat_readability(text_corpus,measure = c("Flesch", "nWS.4"))
all_freqs <- tokens(text_corpus, remove_punct = TRUE,
                    remove_numbers = TRUE)

# Lexikalische Diversitaet berechnen: 
# "TTR" = Type-Token-Ratio, "R" = Guiraud’s Root TTR,"D" = Simpson’s D
lex_measures <- textstat_lexdiv(all_freqs, measure = c("TTR", "R", "D"))

# Die Werte zusammenfuehren
combined_measures <- merge(readability, lex_measures, by = "document")


#------------------------------------------------------------------------------#
## Visualisierung der Daten----------------------------------------------------#
#------------------------------------------------------------------------------#

# Grafik: Neue Wiener Sachtextformel und TTR
# Punktdiagramm: Neue Wiener Sachtextformel der einzelnen Texte
# Groeße der Datenpunkte nach Type-Token-Ratio der Fallgeschichten skaliert
plot <- ggplot(combined_measures, aes(x = document, y = nWS.4, size = TTR)) +
  geom_point(alpha = 0.7) + 
  ylim(0, 15) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "white"))

# Grafik speichern
ggsave(file.path(path_results, "NWS_scores_Gattungsvergleich.png"), plot, 
       width = 10, height = 6, units = "in", dpi = 300)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#