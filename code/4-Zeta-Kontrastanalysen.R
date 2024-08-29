######################### ZETA_KONTRASTANALYSEN ################################

### Abbildung 4:Kontrastanalyse Gartenlaube (Temme) vs. Neuer Pitaval
### Vgl. Weitin, T. (2021). Textklassifikation durch maschinelles Lernen. 
### In: Digitale Literaturgeschichte. Digitale Literaturwissenschaft. 
### https://doi.org/10.1007/978-3-662-63663-3_5
#------------------------------------------------------------------------------#


########### Schritt 1: TRAININGS- UND TESTSETS (OPPOSE / CLASSIFY) #############
#------------------------------------------------------------------------------#

### Erstellt mehrere zufaellig aufgeteilte Oppose und Classify Sets fuer 
### Klassifikationen. Dabei wird sichergestellt, dass fuer die Untersuchungen 
### relevante Texte immer in den Testsets des Classify Sets sind.
###
### Ordnerstrukturen:
###
### corpora
###        |_Gartenlaube_morethan1000words
###        |_Pitaval_morethan1000words
### results
###        |_[Pitaval_Gartenlaube_Oppose wird automatisch erstellt]
###        |_[Ergebnisordner werden automatisch erstellt]
### code
###     |_0-generalFunctions.R

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

`%nin%` = Negate(`%in%`)

# Dateipfade anpassen
path_corpora <- "...\\corpora"
path_results <- "...\\results"

oppose_dir <- paste0(path_results, "\\Pitaval_Gartenlaube_Oppose")
if(!dir.exists(oppose_dir)){
  dir.create(oppose_dir)
}

#------------------------------------------------------------------------------#
## Datenaufteilung: Gartenlaube-Temme (corpus_g)-------------------------------#
#------------------------------------------------------------------------------#

corpus_g <- paste0(path_corpora, "\\Gartenlaube_morethan1000words")

filelist_g <- list.files(path = corpus_g, pattern = NULL, full.names = TRUE, 
                         recursive = FALSE)
len_filelist_g <- length(filelist_g)

proportion_oppose_g <- trunc(len_filelist_g/2)
proportion_classify_g <- len_filelist_g - proportion_oppose_g
proportion_classify_train_g <- trunc(proportion_classify_g/100 * 80)
proportion_classify_test_g <- proportion_classify_g - proportion_classify_train_g
proportion_primary_g <- trunc(proportion_oppose_g/100 * 80)
proportion_test_g <- proportion_oppose_g - proportion_primary_g

#------------------------------------------------------------------------------#
## Datenaufteilung: Der Neue Pitaval (corpus_p)--------------------------------#
#------------------------------------------------------------------------------#

corpus_p <- paste0(path_corpora, "\\Pitaval_morethan1000words")

filelist_p <- list.files(path = corpus_p, pattern = NULL, full.names = TRUE,
                         recursive = FALSE)
len_filelist_p <- length(filelist_p)

proportion_oppose_p <- trunc(len_filelist_p/2)
proportion_classify_p <- len_filelist_p - proportion_oppose_p
proportion_classify_train_p <- trunc(proportion_classify_p/100 * 80)
proportion_classify_test_p <- proportion_classify_p - proportion_classify_train_p
proportion_secondary_p <- trunc(proportion_oppose_p/100 * 80)
proportion_test_p <- proportion_oppose_p - proportion_secondary_p

#------------------------------------------------------------------------------#
## Datenaufteilung: Seeds -----------------------------------------------------#
#------------------------------------------------------------------------------#

# Seeds sorgen dafür, dass Algorithmen mit Zufallselementen konsistente und 
# reproduzierbare Ergebnisse liefern
# Die Textanalysen werden unter kontrollierten Bedingungen durchgefuehrt 
seeds <- 4

for (i in 1:seeds) {
  seed <- i
  set.seed(seed)
  # Erstellung der Ordner
  oppose_dir_i <- paste0(oppose_dir, "\\seed", seed)
  oppose <- paste0(oppose_dir_i, "\\oppose")
  classify <- paste0(oppose_dir_i, "\\classify")
  primary_dir <- paste0(oppose, "\\primary_set")
  secondary_dir <- paste0(oppose, "\\secondary_set") 
  test_dir <- paste0(oppose, "\\test_set")
  testing_set <- paste0(classify, "\\testing")
  training_set <- paste0(classify, "\\training")
  split_dir <- c(oppose_dir_i, oppose, classify, primary_dir, secondary_dir, 
                 test_dir, testing_set, training_set)
  for (dir in split_dir){
    if(!dir.exists(dir)){
      dir.create(dir)
    }
  }

  ##############################################################################
  
  # Testdaten aus der Gartenlaube (Temme) extrahieren
  classify_test_g <- sample.int(length(filelist_g), proportion_classify_test_g, 
                                replace = FALSE)
  classify_test_ids_g <- filelist_g[classify_test_g]
  
  for (i in 1:length(classify_test_ids_g)) {
    file.copy(classify_test_ids_g[i], testing_set)
  }
  
  spare_ids_g <- which(filelist_g %nin% classify_test_ids_g)
  spare_g <- filelist_g[spare_ids_g]
  
  # Trainingsdaten aus der Gartenlaube (Temme) extrahieren
  classify_train_g <- sample.int(length(spare_g), proportion_classify_train_g, 
                                 replace = FALSE)
  classify_train_ids_g <- spare_g[classify_train_g]
  
  for (i in 1:length(classify_train_ids_g)) {
    file.copy(classify_train_ids_g[i], training_set)
  }
  
  spare_ids_g <- which(spare_g %nin% classify_train_ids_g)
  spare_g <- spare_g[spare_ids_g]
  
  # Oppose
  oppose_primary <- sample.int(length(spare_g), proportion_primary_g, 
                               replace = FALSE)
  oppose_primary_ids_g <- spare_g[oppose_primary]
  
  for (i in 1:length(oppose_primary_ids_g)) {
    file.copy(oppose_primary_ids_g[i], primary_dir)
  }
  
  spare_ids_g <- which(spare_g %nin% oppose_primary_ids_g)
  spare_g <- spare_g[spare_ids_g]
  
  for (i in 1:length(spare_g)) {
    file.copy(spare_g[i], test_dir)
  }
  
  ##############################################################################
  
  # Testdaten aus dem Der Neue Pitaval extrahieren
  classify_test_p <- sample.int(length(filelist_p), proportion_classify_test_p, 
                                replace = FALSE)
  classify_test_ids_p <- filelist_p[classify_test_p]
  
  for (i in 1:length(classify_test_ids_p)) {
    file.copy(classify_test_ids_p[i], testing_set)
  }
  
  spare_ids_p <- which(filelist_p %nin% classify_test_ids_p)
  spare_p <- filelist_p[spare_ids_p]
  
  # Trainingsdaten aus dem Der Neue Pitaval extrahieren
  classify_train_p <- sample.int(length(spare_p), proportion_classify_train_p, 
                                 replace = FALSE)
  classify_train_ids_p <- spare_p[classify_train_p]
  
  for (i in 1:length(classify_train_ids_p)) {
    file.copy(classify_train_ids_p[i], training_set)
  }
  
  spare_ids_p <- which(spare_p %nin% classify_train_ids_p)
  spare_p <- spare_p[spare_ids_p]
  
  # Oppose
  oppose_secondary <- sample.int(length(spare_p), proportion_secondary_p, 
                                 replace = FALSE)
  oppose_secondary_ids_p <- spare_p[oppose_secondary]
  
  for (i in 1:length(oppose_secondary_ids_p)) {
    file.copy(oppose_secondary_ids_p[i], secondary_dir)
  }
  
  spare_ids_p <- which(spare_p %nin% oppose_secondary_ids_p)
  spare_p <- spare_p[spare_ids_p]
  
  for (i in 1:length(spare_p)) {
    file.copy(spare_p[i], test_dir)
  }

}

################################################################################
################################################################################

#################### Schritt 2: CLASSIFY / OPPOSE ##############################
#------------------------------------------------------------------------------#

### Fuehrt zuerst eine Oppose-Funktion durch, die Woerter in zwei Listen 
### zurueckgibt, die in der ersten von zwei Gruppen von Texten besonders 
### ueber- und unterrepraesentiert sind. 
###
### Ordnerstrukturen:
###
### corpora
###     |_[Pitaval_Gartenlaube_Oppose wird automatisch erstellt]
###         |_[seed1 wird automatisch erstellt]
###             |_[classify wird automatisch erstellt]
###             |_[oppose wird automatisch erstellt]
### results
###     |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

#install.packages(stylo)
library(stylo)

# Dateipfad anpassen
source("...\\code\\0-generalFunctions.R")

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Ergebnis-Unterordner erstellen
path_results <- paste0(path_results, "\\Pitaval_Gartenlaube_Kontrastanalysen")
if (!dir.exists(path_results)) {
  dir.create(path_results, dir)
}

#------------------------------------------------------------------------------#
## Funktionen -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Parameter der Oppose-Analyse
# Betrachtung und Visualisierung stilometrischer Unterschiede 
# zwischen zwei Korpora auf Wortlistenbasis
call_oppose <- function(){
  oppose(gui = FALSE,
         oppose.method = "craig.zeta",
         primary.corpus.dir = "primary_set",
         secondary.corpus.dir = "secondary_set",
         test.corpus.dir = "test_set",
         text.slice.length = 1000,
         text.slice.overlap = 10,
         corpus.format="plain",
         corpus.lang = "German",
         encoding = "UTF-8",
         write.pdf.file = TRUE,
         write.png.file = TRUE)
}

# Parameter der Klassifizierung
# Textklassifizierung auf Basis stilometrischer Merkmale
# mit Support Vector Machines (svm)
call_classify <- function(){ 
  classify( gui = FALSE,
            training.corpus.dir = "training",
            test.corpus.dir = "testing",
            corpus.format = "plain",
            corpus.lang = "German",
            analyzed.features = "w",
            mfw.min = wordcount,
            mfw.max = wordcount,
            ngram.size = 1,
            use.existing.wordlist = TRUE,
            features = wordlist,
            classification.method = "svm",
            culling.of.all.samples = TRUE,
            z.scores.of.all.samples = FALSE,
            reference.wordlist.of.all.samples = FALSE,
            distance.measure="delta",
            svm.kernel = "linear",
            svm.degree = 3,
            svm.coef0 = 0,
            svm.cost = 1,
            k.value = 1,
            l.value = 0)
}


for (i in 1:seeds) 
  {
  
  #----------------------------------------------------------------------------#
  ## Oppose -------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  
  # Durchfuehrung der Analyse
  # Fuer jeden Seed werden bevorzugte und vermiedene Woerter der Korpora 
  # erhoben, gespeichert und visualisiert
  oppose_dir_i <- paste0(oppose_dir, "\\seed", i, "\\oppose")
  path_seeds <-  paste0(path_results,"\\seed", i)
  wd <- paste0(path_seeds, "\\oppose")       
  setwd(oppose_dir_i)
  
  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
  }
  
  oppose_results <- call_oppose()
  
  scores_avoided <- oppose_results$words.avoided.scores
  write.csv(scores_avoided, "scores_avoided.csv")
  scores_preferred <- oppose_results$words.preferred.scores
  write.csv(scores_preferred, "scores_preferred.csv")
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(wd, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (preferred) ------------------------------------------#
  #----------------------------------------------------------------------------#
  
  # Klassifizierung der Korpora I
  # Verwendung der bevorzugten Worte aus der Oppose-Analyse
  
  wordlist <- as.vector(oppose_results$words.preferred)
  wordcount <- length(wordlist)
  
  classify_dir_i <- paste0(oppose_dir, "\\seed", i, "\\classify")
  setwd(classify_dir_i)
  
  results_preferred <- call_classify()
  analyzed_features <- results_preferred$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_preferred <- paste0(path_results, "\\seed", i, "\\results_preferred")
  
  if (!dir.exists(path_results_preferred)) {
    dir.create(path_results_preferred, recursive = TRUE)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_preferred, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (avoided) --------------------------------------------#
  #----------------------------------------------------------------------------#
  
  # Klassifizierung der Korpora II
  # Verwendung der vermiedenen Worte aus der Oppose-Analyse 
  
  wordlist <- as.vector(oppose_results$words.avoided)
  wordcount <- length(wordlist)
  
  results_avoided <- call_classify()
  analyzed_features <- results_avoided$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_avoided <- paste0(path_results, "\\seed", i, "\\results_avoided")
  
  if (!dir.exists(path_results_avoided)) {
    dir.create(path_results_avoided, recursive = TRUE)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_avoided, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (merged) ---------------------------------------------#
  #----------------------------------------------------------------------------#
  
  # Klassifizierung der Korpora  III
  # mit kombinierter Wortlisten (vermieden und bevorzugt)
  
  wordlist <- as.vector(c(oppose_results$words.avoided, 
                          oppose_results$words.preferred))
  wordcount <- length(wordlist)
  
  results_merged <- call_classify()
  analyzed_features <- results_merged$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_merged <- paste0(path_results, "\\seed", i, "\\results_merged")
  
  if (!dir.exists(path_results_merged)) {
    dir.create(path_results_merged, dir)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_merged, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#