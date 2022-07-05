library(stylo)
library(grepel)
library(xlsx)
library(quanteda)
library(quanteda.textstats)
library(koRpus.lang.en)
library(koRpus.lang.fr)
library(koRpus.lang.it)
library(readtext)
library(stringr)
library(ggplot2)
library(ggrepel)
library(tm.plugin.koRpus)


# Cluster analysis with Stylo ### -------------------------------------

# Cluster analysis in Stylo for each language-corpus (the MFW is set to 600, the full wordlist. Yet the features
# analyzed are in reality much less (only those in the wordlist, selected manually as described (cf. Methodology)))

# Cluster analysis of Lahiri
# Using preprocessed corpora

res_EN_Lahiri = stylo(gui = FALSE, mfw.min = 600, mfw.max = 600, culling.min = 0, culling.max = 0,
                      parsed.corpus= LahENcorp, save.analyzed.features = FALSE, dendrogram.layout.horizontal = TRUE,
                      write.png.file = FALSE, display.on.screen = TRUE, use.existing.wordlist = TRUE)

res_IT_Lahiri = stylo(gui = FALSE, mfw.min = 600, mfw.max = 600, culling.min = 0, culling.max = 0,
                      parsed.corpus= LahITcorp, save.analyzed.features = FALSE, dendrogram.layout.horizontal = TRUE,
                      write.png.file = FALSE, display.on.screen = TRUE, use.existing.wordlist = TRUE, save.analyzed.features = TRUE)

# See features actually used in the cluster analysis to make sure only the selected features were used
res_EN_Lahiri$features.actually.used
res_IT_Lahiri$features.actually.used



# Cluster analysis of Beckett

res_EN_Beckett = stylo(gui = FALSE, mfw.min = 600, mfw.max = 600, culling.min = 0, culling.max = 0,
                      parsed.corpus= BeckettENcorp, save.analyzed.features = FALSE, dendrogram.layout.horizontal = TRUE,
                      write.png.file = FALSE, display.on.screen = TRUE, use.existing.wordlist = TRUE)

res_FR_Beckett = stylo(gui = FALSE, mfw.min = 600, mfw.max = 600, culling.min = 0, culling.max = 0,
                      parsed.corpus= BeckettFRcorp, save.analyzed.features = FALSE, dendrogram.layout.horizontal = TRUE,
                      write.png.file = FALSE, display.on.screen = TRUE, use.existing.wordlist = TRUE, save.analyzed.features = TRUE)

# See features actually used in the cluster analysis to make sure only the selected features were used
res_EN_Beckett$features.actually.used
res_FR_Beckett$features.actually.used


# Classification of Lahiri  with NSC ---------------------------------

summary(LahENcorp[1:14]) # Inspecting

# So to perform classification of language-groups in the Lahiri corpus according to function word-use, 
# I first load the file of selected features among the 600 MFF. The 600 MFF can be found in the file generated 
# when performing the cluster analysis in Stylo (as above). I subsequently edit that list to include only function words, 
# grammatical content words, and punctuation marks (cf. methodology). 
# This word-list with corresponding frequencies will be the basis for the classification analysis.

file = #YOURPATH# # Fetching edited word-list file
wordlist <- c(file, paste(readLines(file))) # Loading the entire word-list of 600 MFF as a list-object.
summary(wordlist) # Inspect
words <- wordlist[!grepl("#",unlist(wordlist))] # Deleting the manually excluded features (preceded by #) from list
summary(words) # Inspecting that only the selected features remain

# I create a document-term matrix as a basis for the classification (for both training- and test-set), in that way, we can
# run the classifier on just the selected features among the 600 MFF loaded above.
freq.list <- make.frequency.list(words, head = 600) # Creating frequency table using only the frequencies of the 600 MFF
                                                   
word.frequencies <- make.table.of.frequencies(corpus = LahENcorp, features = freq.list) # Document-term matrix of corpus and 
                                                                                              # matching frequencies.

# Now creating the sets for training and testing the classifier (cf. methodology)
training_set <- word.frequencies[c(1,2,3,6,8,9,11,12,14),1:250] # Selecting training set (at least one sample of each class)
dim(training_set) # Double-check that it is the right no. of texts
testing_set <- word.frequencies[-c(1,2,3,6,8,9,11,12,14),1:250] # Selecting testing set (simply the total corpus, subtracting the training set)
dim(testing_set) # Double-check ...

# Performing classification in Stylo, selecting the NSC classifier, cross-validation rounds set to 100.
res = classify(gui = FALSE, training.frequencies = training_set, test.frequencies = testing_set,
               classification.method = "nsc", mfw.min = 600, mfw.max = 600, cv.folds = 100, show.features = TRUE)

# Now to results
cf <- table(res$predicted, res$expected) # Create confusion matrix to see no. of mis-attributions for each class (predicted/expected)
cf
res$cross.validation.summary # Inspect the no. of correctly guessed samples in each cross-validation iteration
performance.measures(res) # Inspect accuracy, precision, recall, and avg.f-score
performance_measures_EN <- as.data.frame(performance.measures(res)) # Saving as df for later compilation

res$distinctive.features # See distinctive features for centroids in the NSC classification
write.xlsx(format(res$distinctive.features, digits = 2), ##)

# For more info on misclassifications, use crossv() (a simpler version of classify(), but more controllable). Only full corpus (word.frequencies) 
# is necessary since it may perform "leave one out" (whereby all samples will be used as test-samples at least once)
crossv_results = crossv(training.set = word.frequencies, classification.method = "nsc", cv.folds = 100, cv.mode = "leaveoneout")
crossv_results$misclassified # See the specific samples misclassified

# Now all this again but for the corpus in Italian...


# Since the Italian corpus is smaller than the English, I do the classification looking only at two classes
# (and not distinguishing between fiction and nonfiction as in the classification of the English corpus); so 
# that we classify works originally written in Italian and words originally written in English

# Loading corpus with the appropriate IDs
corpus_Lahiri_it = load.corpus.and.parse(files = "all", 
                                         corpus.dir = ##YOURPATH##, markup.type = "plain",
                                           corpus.lang = "Italian", splitting.rule = ("[ \t\n]+"),
                                         # preserving punctuation-marks
                                         sampling = "no.sampling",features = "w", ngram.size = 1, 
                                         preserve.case = FALSE, encoding = "UTF-8")
summary(corpus_Lahiri_it[1:8]) # Inspecting

# Again, first loading the file of selected features among the 600 MFF in Italian.
# This word-list with corresponding frequencies will be the basis for the classification analysis.

file = ##YOURPATH## # Fetching edited word-list file
  wordlist <- c(file, paste(readLines(file))) # Loading the entire word-list of 600 MFF as a list-object.
summary(wordlist) # Inspect
words <- wordlist[!grepl("#",unlist(wordlist))] # Deleting the manually excluded features (preceded by #) from list
summary(words) # Inspecting that only the selected features remain

# I create a document-term matrix as a basis for the classification (for both training- and test-set), in that way, we can
# run the classifier on just the selected features among the 600 MFF loaded above.
freq.list <- make.frequency.list(words, head = 600) # Creating frequency table using only the frequencies of the 600 MFF

word.frequencies <- make.table.of.frequencies(corpus = corpus_Lahiri_it, features = freq.list) # Document-term matrix of corpus and 
# matching frequencies.

# Now creating the sets for training and testing the classifier (cf. methodology)
training_set <- word.frequencies[c(1,2,5,6,7),1:287] # Selecting training set (at least one sample of each class)
dim(training_set) # Double-check that it is the right no. of texts
testing_set <- word.frequencies[-c(1,2,5,6,7),1:287] # Selecting testing set (simply the total corpus, subtracting the training set)
dim(testing_set) # Double-check ...

# Performing classification through Stylo, selecting the NSC classifier, cross-validation rounds set to 100.
res = classify(gui = FALSE, training.frequencies = training_set, test.frequencies = testing_set,
               classification.method = "nsc", mfw.min = 600, mfw.max = 600, cv.folds = 100, show.features = TRUE)

# Now to results
cf <- table(res$predicted, res$expected) # Create confusion matrix to see no. of mis-attributions for each class (predicted/expected)
cf
res$cross.validation.summary # Inspect the no. of correctly guessed samples in each cross-validation iteration
performance.measures(res) # Inspect accuracy, precision, recall, and avg.f-score
performance_measures_IT <- as.data.frame(performance.measures(res)) # Saving as df for later compilation

res$distinctive.features # See distinctive features for centroids in the NSC classification
write.csv(format(res$distinctive.features, digits = 2), "/Users/palleb/Desktop/Lahiri analysis/IT_dist_features", row.names = TRUE)

# For more info on misclassifications, use crossv() (a simpler version of classify(), but more controllable). Only full corpus (word.frequencies) 
# is necessary since it may perform "leave one out" (whereby all samples will be used as test-samples at least once)
crossv_results = crossv(training.set = word.frequencies, classification.method = "nsc", cv.folds = 100)
crossv_results$misclassified # See the specific samples misclassified


# Compiling performance measures in each language-analysis
# Fecthing performance measures and combining them in a table
all_performance <- rbind(Italian = performance_measures_IT, English = performance_measures_EN)
write.xlsx(format(all_performance, digits = 3), "/Users/palleb/Desktop/Lahiri analysis/NSCperformance_all.xlsx", row.names = TRUE)





# Classification of Beckett with NSC --------------------------------------

# Loading corpus with the appropriate IDs, since we only want to look at early and middle texts here (ID in filename)
corpus_Beckett_french <- load.corpus.and.parse(files = "all", 
                                               corpus.dir = "FR_corpus", markup.type = "plain",
                                               corpus.lang = "french", splitting.rule = ("[ \t\n]+"), # preserving punctuation-marks
                                               sampling = "no.sampling",features = "w",ngram.size = 1, 
                                               preserve.case = FALSE, encoding = "UTF-8")
summary(corpus_Beckett_french[]) # Inspect

# So to perform classification of language-groups in Beckett's corpus according to function word-use, 
# Again, loading the file of selected features among the 600 MFF in French this time

file = "#Beckett_FR_wordlist_600MFF.txt" # Fetching word-list file
wordlist_FR_600MFF <- c(file, paste(readLines(file))) # Loading the word-list of selected features among 600 MFF as list.
summary(wordlist_FR_600MFF) # Inspect
words_FR_600MFF <- wordlist_FR_600MFF[!grepl("#",unlist(wordlist_FR_600MFF))] # Deleting excluded features (preceded by #) from list
summary(words_FR_600MFF)

# I create a document-term matrix as a basis for the classification (for both training- and test-set), in that way, we can
# run the classifier on just the selected features among the 600 MFF loaded above.
freq.list_FR <- make.frequency.list(words_FR_600MFF, head = 600) # Creating frequency table using only the frequencies of the 
# selected features of the 600 MFF from word-list
word.frequencies_french <- make.table.of.frequencies(corpus = corpus_Beckett_french, features = freq.list_FR) # Document-term matrix of corpus and 
# matching frequencies.
training_set_french <- word.frequencies_french[c(1,2),1:337] # Selecting training set (at least one sample of each class)
dim(training_set_french) # Double-check that it is the right no. of texts
testing_set_french <- word.frequencies_french[-c(1,2,),1:337] # Selecting testing set (simply the total corpus, subtracting the training set)
dim(testing_set_french) # Double-check ...

res_Beckett_french <- classify(gui = FALSE, training.frequencies = training_set, test.frequencies = testing_set,
                               classification.method = "nsc", mfw.min = 600, mfw.max = 600, cv.folds = 100, show.features = TRUE) # Perform NSC classification
cf_french <- table(res_Beckett_french$predicted, res_Beckett_french$expected) # Create confusion matrix to see no. of mis-attributions for each class (predicted/expected)
cf_french
res_Beckett_french$cross.validation.summary # Inspect the no. of correctly guessed samples in each cross-validation iteration
perf_Beckett_french <- performance.measures(res_Beckett_french) # Inspect accuracy, precision, recall, and avg.f-score
res_Beckett_french$distinctive.features # See distinctive features for centroids in the NSC classification

# For more detailed info on misclassifications, use crossv() (a simpler version of classify(), but more controllable). Only full corpus (word.frequencies) 
# is necessary since crossv can perform "leave one out" (whereby all samples will be used as test-samples at least once)
crossv_res_Beckett_french <- crossv(training.set = word.frequencies, classification.method = "nsc", cv.folds = 100, cv.mode = "leaveoneout")
crossv_res_Beckett_french$misclassified # See the specific samples misclassified
crossv_res_Beckett_french$confusion_matrix # Again, confusion matrix for the "leave one out" procedure


## Calculate distinctive features for ONLY early English contra middle French-period texts
earlymiddle_training_set_french <- word.frequencies_french[c(4,9,14,16),1:337] # Selecting training set (at least one sample of each class)
dim(earlymiddle_training_set_french) # Inspect
earlymiddle_testing_set_french <- word.frequencies_french[c(2,6,10,17,18),1:337] # Selecting testing set (simply the total corpus, subtracting the training set)
dim(earlymiddle_testing_set_french) # Inspect
res_earlymiddle_french <- classify(gui = FALSE, training.frequencies = earlymiddle_training_set_french, test.frequencies = earlymiddle_testing_set_french,
                                   classification.method = "nsc", mfw.min = 600, mfw.max = 600, cv.folds = 500, show.features = TRUE) # Perform NSC classification
ealrymiddle_Beckett_perf_french <- performance.measures(res_earlymiddle_french) # See avg.accuracy, avg.precision, avg.recall, and avg.f
ealrymiddle_Beckett_perf_french
cf_earlymiddle_french <- table(res_earlymiddle_french$predicted, res_earlymiddle_french$expected) # Again, confusion matrix to see mis-attributions
cf_earlymiddle_french # Inspect

res_earlymiddle_french$distinctive.features
write.csv(format(res_earlymiddle_french$distinctive.features, digits = 2), "/Users/palleb/Desktop/Beckett_NSC_features_FR", row.names = TRUE)

## Combining results from analyses in both languages (English/French)

comb_perf_Beckett <- cbind(perf_Beckett_english, perf_Beckett_french)
comb_earlymiddle_perf_Beckett <- cbind(earlymiddle_Beckett_perf_english, earlymiddle_dist_features_french)

cf_english
cf_french

# Combining distinctive features in each analysis for whole corpus on only early- and middle-period works
comb_dist_features_Beckett <- cbind(res_Beckett_english$distinctive.features, res_Beckett_french$distinctive.features)
comb_earlymiddle_dist_features_Beckett <- cbind(earlymiddle_dist_features_english, earlymiddle_dist_features_french)



# Analysis of vocabulary (with oppose() in Stylo) ---------------------

# Selecting language-groups (filenames are preceded by their language of composition ID)
IT_eng <- LahENcorp[grep("It", names(LahENcorp))] # Getting just the works composed in English
EN_eng <- LahENcorp[grep("En", names(LahENcorp))] # Getting just the works composed in Italian

eng_results <- oppose(gui = FALSE, primary.corpus = IT_eng, secondary.corpus = EN_eng, custom.graph.title= "Vocabulary", 
       zeta.filter.threshold = 0.2, text.slice.length = 2000)
EN_eng_100_preferred <-  eng_results$words.preferred[1:100]
IT_eng_100_preferred <-  eng_results$words.avoided[1:100]
eng_results$words.avoided
eng_results$words.preferred
eng_results$words.avoided.scores
eng_results$words.preferred.scores

IT_italian <- Lah_ITcorp[grep("It", names(Lah_ITcorp))] # Getting just the works composed in English
EN_italian <- Lah_ITcorp[grep("En", names(Lah_ITcorp))] # Getting just the works composed in Italian
ital_results <- oppose(gui = FALSE, primary.corpus = IT_italian, secondary.corpus = EN_italian, custom.graph.title= "Vocabulary", 
       zeta.filter.threshold = 0.2, text.slice.length = 2000)

EN_italian_100_preferred <- ital_results$words.preferred[1:100] # Saving distinctive features
IT_italian_100_preferred <- ital_results$words.avoided[1:100]
ital_results$words.avoided
ital_results$words.preferred
ital_results$words.avoided.scores
ital_results$words.preferred.scores



### And for Beckett (looking at only early and middle works)

Beckett_oppose_eng_corpus = load.corpus.and.parse(files = "all", 
                                                  corpus.dir = "class_corpus", markup.type = "plain",
                                                  corpus.lang = "English.all", 
                                                  sampling = "no.sampling",features = "w",ngram.size = 1, 
                                                  preserve.case = FALSE, encoding = "UTF-8")

early_EN <- Beckett_oppose_eng_corpus[grep(c("earlyEN_"), names(Beckett_oppose_eng_corpus))] # Getting just the early works composed in English
summary(early_EN)
middle_FR <- Beckett_oppose_eng_corpus[grep("middleFr_", names(Beckett_oppose_eng_corpus))] # Getting just the middle-period works composed in French
summary(middle_FR)


Zeta_eng_results = oppose(gui = TRUE, primary.corpus = early_EN, secondary.corpus = middle_FR, 
                          custom.graph.title= "Vocabulary", zeta.filter.threshold = 0.2)
# Slice lenght = 1800, threshold = 0.2 !!!

EN_eng_100_preferred = Zeta_eng_results$words.preferred[1:100]
FR_eng_100_preferred = Zeta_eng_results$words.avoided[1:100]
Zeta_eng_results$words.avoided
Zeta_eng_results$words.preferred
Zeta_eng_results$words.avoided.scores
Zeta_eng_results$words.preferred.scores

summary(early_EN)
summary(middle_FR)

# Same in French

setwd(##)
Beckett_oppose_fr_corpus = load.corpus.and.parse(files = "all", 
                                                   corpus.dir = "class_corpus", markup.type = "plain",
                                                   corpus.lang = "French", 
                                                   sampling = "no.sampling",features = "w",ngram.size = 1, 
                                                   preserve.case = FALSE, encoding = "UTF-8")
  
early_EN <- Beckett_oppose_fr_corpus[grep(c("earlyEn_"), names(Beckett_oppose_fr_corpus))] # Getting just the early works composed in English
summary(early_EN)
middle_FR <- Beckett_oppose_fr_corpus[grep("middleFr_", names(Beckett_oppose_fr_corpus))] # Getting just the middle-period works composed in French
summary(middle_FR)
  
  
Zeta_fre_results = oppose(gui = TRUE, primary.corpus = early_EN, secondary.corpus = middle_FR, 
                            custom.graph.title= "Vocabulary", zeta.filter.threshold = 0.2)
  


# Oppose in entire corpus, French contrasted to English texts
setwd(##)
Beckett_oppose_fr_corpus = load.corpus.and.parse(files = "all", 
                                                     corpus.dir = "FR_corpus", markup.type = "plain",
                                                     corpus.lang = "French", 
                                                     sampling = "no.sampling",features = "w",ngram.size = 1, 
                                                     preserve.case = FALSE, encoding = "UTF-8")
EN <- Beckett_oppose_fr_corpus[grep(c("En_"), names(Beckett_oppose_fr_corpus))] # Getting just the early works composed in English
summary(EN)
FR <- Beckett_oppose_fr_corpus[grep("Fr_", names(Beckett_oppose_fr_corpus))] # Getting just the middle-period works composed in French
summary(FR)
Zeta_fre_results = oppose(gui = TRUE, primary.corpus = EN, secondary.corpus = FR, 
                              custom.graph.title= "Vocabulary", zeta.filter.threshold = 0.2)
    
    

# Vocabulary Diversity MSTTR in quanteda ### ---------------------------------------

# ## MSTTR Lahiri English analysis quanteda ## -------------------------------------------

folder_english = ##YOURPATH##

# Collecting filenames
file_names_english <- list.files(folder_english)

# Collecting texts
files_english <- dir(folder_english, full.names = TRUE)
Lahiri_texts_english <- c()
for (f in files_english) {
  Lahiri_texts_english <- c(Lahiri_texts_english, paste(readLines(f), collapse = "\n"))
}
summary(Lahiri_texts_english)

# Collecting years (last letters in filename)
years <- c()
for (y in file_names_english){
  tmp = str_sub(file_names_english, start = -8, end = -5)
  years <- c(tmp)
}
years

# Collecting language of composition (first letters in filename)
language <- c()
for (y in file_names_english){
  tmp = str_sub(file_names_english, start = 1, end = 2)
  language <- c(tmp)
}
language
# Collecting genre (secondary set of letters in filename)
genre <- c()
for (y in file_names_english){
  tmp = str_sub(file_names_english, start = 4, end = 6)
  genre <- c(tmp)
}
genre

# Preparing corpus with variables for Quanteda analysis
corpus_Lahiri_english <- corpus(Lahiri_texts_english) # Creating corpus.
docnames(corpus_Lahiri_english) <- file_names_english # Adding filenames
corpus_Lahiri_english$year <- years # Adding year variable
corpus_Lahiri_english$language <- language # Adding language of composition variable
corpus_Lahiri_english$genre <- genre # Adding genre variable
docvars(corpus_Lahiri_english) # Inspect year/language/genre variables
print(corpus_Lahiri_english) # Inspect corpus

# Tokenizing corpus & subsets
tokenized_Lahiri_corpus_english <- tokens(corpus_Lahiri_english, stem = TRUE, language = "en")
tokenized_IT_Lahiri_english <- tokens(corpus_subset(corpus_Lahiri_english, language %in% "It"))
tokenized_EN_Lahiri_english <- tokens(corpus_subset(corpus_Lahiri_english, language %in% "En"))

# Calculating MSTTR scores
corpus_Lahiri_MSTTR_english <- textstat_lexdiv(tokenized_Lahiri_corpus_english, measure = "MSTTR", MSTTR_segment = 100)
EN_Lahiri_MSTTR_english <- textstat_lexdiv(tokenized_EN_Lahiri_english, measure = "MSTTR", MSTTR_segment = 100)
IT_Lahiri_MSTTR_english <- textstat_lexdiv(tokenized_IT_Lahiri_english, measure = "MSTTR", MSTTR_segment = 100)

# Calculate MSTTR mean, max. value, and min. value for each language
meanmaxmin_Lahiri_EN_english <- c(mean = mean(EN_Lahiri_MSTTR_english$MSTTR),max = max(EN_Lahiri_MSTTR_english$MSTTR), min = min(EN_Lahiri_MSTTR_english$MSTTR))
meanmaxmin_Lahiri_IT_english <- c(mean = mean(IT_Lahiri_MSTTR_english$MSTTR),max = max(IT_Lahiri_MSTTR_english$MSTTR), min = min(IT_Lahiri_MSTTR_english$MSTTR))

# Combine MSTTR mean, max, and min. values for each language in table
table_combined_Lahiri_meanmaxmin_english <- cbind(EN_texts_English = meanmaxmin_Lahiri_EN_english, IT_texts_English = meanmaxmin_Lahiri_IT_english)
table_combined_Lahiri_meanmaxmin_english

# MSTTR for whole corpus, each text individually with data details
detailed_Lahiri_MSTTRscores_english <- cbind(corpus_Lahiri_MSTTR_english, year = corpus_Lahiri_english$year, language = corpus_Lahiri_english$language, genre = corpus_Lahiri_english$genre)


## Bonus
# Plot the scores of the whole corpus by year to check if there is a chronological trend
ggplot(data=corpus_MSTTR_english, aes(corpus_Lahiri_english$year, MSTTR, color=corpus_Lahiri_english$language, shape = corpus_Lahiri_english$genre)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = docnames(corpus_Lahiri_english)))+ labs(x = "year of publication", y = "TTR")

# If not, I simply use all_MSTTR_meanmaxmin_scores in thesis. 



# ## MSTTR Lahiri Italian analysis quanteda ## --------------------------------------------
folder_italian = ##YOURPATH##

# Collecting filenames
file_names_italian <- list.files(folder_italian)

# Collecting texts
files_italian <- dir(folder_italian, full.names = TRUE)
Lahiri_texts_italian <- c()
for (f in files_italian) {
  Lahiri_texts_italian <- c(Lahiri_texts_italian, paste(readLines(f), collapse = "\n"))
}
summary(Lahiri_texts_italian)

# Collecting years (last letters in filename)
years <- c()
for (y in file_names_italian){
  tmp = str_sub(file_names_italian, start = -8, end = -5)
  years <- c(tmp)
}
years

# Collecting language of composition (first letters in filename)
language <- c()
for (y in file_names_italian){
  tmp = str_sub(file_names_italian, start = 1, end = 2)
  language <- c(tmp)
}
language

# Collecting genre (secondary set of letters in filename)
genre <- c()
for (y in file_names_italian){
  tmp = str_sub(file_names_italian, start = 4, end = 6)
  genre <- c(tmp)
}
genre

# Preparing corpus with variables for analysis with Quanteda
corpus_Lahiri_italian <- corpus(Lahiri_texts_italian) # Creating corpus.
docnames(corpus_Lahiri_italian) <- file_names_italian # Adding filenames
corpus_Lahiri_italian$year <- years # Adding year variable
corpus_Lahiri_italian$language <- language # Adding language of composition variable
corpus_Lahiri_italian$genre <- genre # Adding genre variable
docvars(corpus_Lahiri_italian) # Inspect year/language/genre variables
print(corpus_Lahiri_italian) # Inspect corpus

# Tokenizing corpus & subsets
tokenized_Lahiri_corpus_italian <- tokens(corpus_Lahiri_italian)
tokenized_IT_Lahiri_italian <- tokens(corpus_subset(corpus_Lahiri_italian, language %in% "It"))
tokenized_EN_Lahiri_italian <- tokens(corpus_subset(corpus_Lahiri_italian, language %in% "En"))

# Calculating MSTTR scores
corpus_Lahiri_MSTTR_italian <- textstat_lexdiv(tokenized_Lahiri_corpus_italian, measure = "MSTTR", MSTTR_segment = 100)
EN_Lahiri_MSTTR_italian <- textstat_lexdiv(tokenized_EN_Lahiri_italian, measure = "MSTTR", MSTTR_segment = 100)
IT_Lahiri_MSTTR_italian <- textstat_lexdiv(tokenized_IT_Lahiri_italian, measure = "MSTTR", MSTTR_segment = 100)

# Calculate MSTTR mean, max. value, and min. value for each language
meanmaxmin_Lahiri_EN_italian <- c(mean = mean(EN_Lahiri_MSTTR_italian$MSTTR),max = max(EN_Lahiri_MSTTR_italian$MSTTR), min = min(EN_Lahiri_MSTTR_italian$MSTTR))
meanmaxmin_Lahiri_IT_italian <- c(mean = mean(IT_Lahiri_MSTTR_italian$MSTTR),max = max(IT_Lahiri_MSTTR_italian$MSTTR), min = min(IT_Lahiri_MSTTR_italian$MSTTR))

# Combine MSTTR mean, max, and min. values for each language in table
table_combined_Lahiri_meanmaxmin_italian <- cbind(EN_texts_Italian = meanmaxmin_Lahiri_EN_italian, IT_texts_Italian = meanmaxmin_Lahiri_IT_italian)
table_combined_Lahiri_meanmaxmin_italian

# MSTTR for whole corpus, each text individually with data
detailed_Lahiri_MSTTRscores_italian <- cbind(corpus_Lahiri_MSTTR_italian, year = corpus_Lahiri_italian$year, language = corpus_Lahiri_italian$language, genre = corpus_Lahiri_italian$genre)
detailed_Lahiri_MSTTRscores_italian

## Bonus
# Plot the scores of the whole corpus by year to check if there is a chronological trend
ggplot(data=corpus_MSTTR_italian, aes(corpus_texts$year, MSTTR, color=corpus_texts$language, shape = corpus_texts$genre)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = docnames(corpus_Lahiri_italian)))+ labs(x = "year of publication", y = "TTR")
# If not, I simply use all_MSTTR_meanmaxmin_scores in thesis.





# ## Combining Lahiri MSTTR results quanteda## --------------------------------

# Combining tables of MSTTR mean, max, and min-tables (English/Italian analysis)
all_Lahiri_meanmaxmin_scores <- cbind(table_combined_Lahiri_meanmaxmin_italian, table_combined_Lahiri_meanmaxmin_english)
write.xlsx(format(all_lahiri_meanmaxmin_scores, digits = 2), "/Users/palleb/Desktop/Lahiri analysis/table_Lahiri_MSTTR_meanmaxmin_all.xlsx", row.names = TRUE)

## Combining detailed MSTTR data (English/Italian)
detailed_Lahiri_MSTTRscores_all <- rbind(detailed_Lahiri_MSTTRscores_italian, detailed_Lahiri_MSTTRscores_english)
detailed_Lahiri_MSTTRscores_all
write.xlsx(format(detailed_Lahiri_MSTTRscores_all, digits = 2), "/Users/palleb/Desktop/Lahiri analysis/table_Lahiri_MSTTRscores_detailed.xlsx", row.names = TRUE)
# Detailed MSTTR data  for each author and each language analysis is ultimately joined in table, and uploaded on Github repository Bilingual_writing







# ## MSTTR Beckett English analysis ## -------------------------------------------

folder_english = "/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett English Oeuvre/EN_corpus"

# Collecting filenames
file_names_english <- list.files(folder_english)

# Collecting texts
files_english <- dir(folder_english, full.names = TRUE)
Beckett_texts_english <- c()
for (f in files_english) {
  Beckett_texts_english <- c(Beckett_texts_english, paste(readLines(f), collapse = "\n"))
}
summary(Beckett_texts_english)

# Collecting years (last letters in filename)
years <- c()
for (y in file_names_english){
  tmp = str_sub(file_names_english, start = -8, end = -5)
  years <- c(tmp)
}
years

# Collecting language of composition (first letters in filename)
language <- c()
for (y in file_names_english){
  tmp = str_sub(file_names_english, start = 1, end = 2)
  language <- c(tmp)
}
language

# Preparing corpus with variables for Quanteda analysis
corpus_Beckett_english <- corpus(Beckett_texts_english) # Creating corpus.
docnames(corpus_Beckett_english) <- file_names_english # Adding filenames
corpus_Beckett_english$year <- years # Adding year variable
corpus_Beckett_english$language <- language # Adding language of composition variable
docvars(corpus_Beckett_english) # Inspect year/language variables
print(corpus_Beckett_english) # Inspect corpus

# Tokenizing corpus & subsets DOES THIS WORK
tokenized_Beckett_corpus_english <- tokens(corpus_Beckett_english, stem = TRUE, language = "en")
tokenized_FR_Beckett_english <- tokens(corpus_subset(corpus_Beckett_english, language %in% "Fr"))
tokenized_EN_Beckett_english <- tokens(corpus_subset(corpus_Beckett_english, language %in% "En"))

# Calculating MSTTR scores
corpus_Beckett_MSTTR_english <- textstat_lexdiv(tokenized_Beckett_corpus_english, measure = "MSTTR", MSTTR_segment = 100)
EN_Beckett_MSTTR_english <- textstat_lexdiv(tokenized_EN_Beckett_english, measure = "MSTTR", MSTTR_segment = 100)
FR_Beckett_MSTTR_english <- textstat_lexdiv(tokenized_FR_Beckett_english, measure = "MSTTR", MSTTR_segment = 100)

# Calculate MSTTR mean, max. value, and min. value for each language
meanmaxmin_Beckett_EN_english <- c(mean = mean(EN_Beckett_MSTTR_english$MSTTR),max = max(EN_Beckett_MSTTR_english$MSTTR), min = min(EN_Beckett_MSTTR_english$MSTTR))
meanmaxmin_Beckett_FR_english <- c(mean = mean(FR_Beckett_MSTTR_english$MSTTR),max = max(FR_Beckett_MSTTR_english$MSTTR), min = min(FR_Beckett_MSTTR_english$MSTTR))
# Combine MSTTR mean, max, and min. values for each language in table
table_combined_Beckett_meanmaxmin_english <- cbind(EN_texts_English = meanmaxmin_Beckett_EN_english, FR_texts_English = meanmaxmin_Beckett_FR_english)
table_combined_Beckett_meanmaxmin_english # Inspect

# MSTTR for whole corpus, each text individually with details
detailed_Beckett_MSTTRscores_english <- cbind(corpus_Beckett_MSTTR_english, year = corpus_Beckett_english$year, language = corpus_Beckett_english$language)
detailed_Beckett_MSTTRscores_english # Inspect

## Bonus
# Plot the scores of the whole corpus by year to check if there is a chronological trend
ggplot(data=corpus_Beckett_MSTTR_english, aes(corpus_Beckett_english$year, MSTTR, color=corpus_Beckett_english$language)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = docnames(corpus_Beckett_english)))+ labs(x = "year of composition-start", y = "MSTTR")

# If not, I simply use all_MSTTR_meanmaxmin_scores in thesis. 



# ## MSTTR Becket French analysis ## --------------------------------------------
folder_french = "/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett French Oeuvre/Fr_Corpus"

# Collecting filenames
file_names_french <- list.files(folder_french)

# Collecting texts
files_french <- dir(folder_french, full.names = TRUE)
Beckett_texts_french <- c()
for (f in files_french) {
  Beckett_texts_french <- c(Beckett_texts_french, paste(readLines(f), collapse = "\n"))
}
summary(Beckett_texts_french)

# Collecting years (last letters in filename)
years <- c()
for (y in file_names_french){
  tmp = str_sub(file_names_french, start = -8, end = -5)
  years <- c(tmp)
}
years

# Collecting language of composition (first letters in filename)
language <- c()
for (y in file_names_french){
  tmp = str_sub(file_names_french, start = 1, end = 2)
  language <- c(tmp)
}
language


# Preparing corpus with variables for analysis with Quanteda
corpus_Beckett_french <- corpus(Beckett_texts_french) # Creating corpus.
docnames(corpus_Beckett_french) <- file_names_french # Adding filenames
corpus_Beckett_french$year <- years # Adding year variable
corpus_Beckett_french$language <- language # Adding language of composition variable
docvars(corpus_Beckett_french) # Inspect year/language variables
print(corpus_Beckett_french) # Inspect corpus

# Tokenizing corpus & subsets
tokenized_corpus_Beckett_french <- tokens(corpus_Beckett_french)
tokenized_FR_Beckett_french <- tokens(corpus_subset(corpus_Beckett_french, language %in% "Fr"))
tokenized_EN_Beckett_french <- tokens(corpus_subset(corpus_Beckett_french, language %in% "En"))

# Calculating MSTTR scores
corpus_Beckett_MSTTR_french <- textstat_lexdiv(tokenized_corpus_Beckett_french, measure = "MSTTR", MSTTR_segment = 100)
EN_Beckett_MSTTR_french <- textstat_lexdiv(tokenized_EN_Beckett_french, measure = "MSTTR", MSTTR_segment = 100)
FR_Beckett_MSTTR_french <- textstat_lexdiv(tokenized_FR_Beckett_french, measure = "MSTTR", MSTTR_segment = 100)

# Calculate MSTTR mean, max. value, and min. value for each language
meanmaxmin_Beckett_EN_french <- c(mean = mean(EN_Beckett_MSTTR_french$MSTTR),max = max(EN_Beckett_MSTTR_french$MSTTR), min = min(EN_Beckett_MSTTR_french$MSTTR))
meanmaxmin_Beckett_FR_french <- c(mean = mean(FR_Beckett_MSTTR_french$MSTTR),max = max(FR_Beckett_MSTTR_french$MSTTR), min = min(FR_Beckett_MSTTR_french$MSTTR))
# Combine MSTTR mean, max, and min. values for each language in table
table_combined_Beckett_meanmaxmin_french <- cbind(EN_texts_french = meanmaxmin_Beckett_EN_french, FR_texts_french = meanmaxmin_Beckett_FR_french)
table_combined_Beckett_meanmaxmin_french # Inspect

# MSTTR for whole corpus, each text individually with details
detailed_Beckett_MSTTRscores_french <- cbind(corpus_Beckett_MSTTR_french, year = corpus_Beckett_french$year, language = corpus_Beckett_french$language)
detailed_Beckett_MSTTRscores_french # Inspect

## Bonus
# Plot the scores of the whole corpus by year to check if there is a chronological trend
ggplot(data=corpus_Beckett_MSTTR_french, aes(corpus_Beckett_french$year, MSTTR, color=corpus_Beckett_french$language)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = docnames(corpus_Beckett_french)))+ labs(x = "year of composition-start", y = "MSTTR")
# If not, I simply use all_MSTTR_meanmaxmin_scores in thesis.




# ## Combining Beckett MSTTR results quanteda ## --------------------------------

# Combining tables of MSTTR mean, max, and min-tables (English/Italian analysis)
all_Beckett_meanmaxmin_scores <- cbind(table_combined_Beckett_meanmaxmin_french, table_combined_Beckett_meanmaxmin_english)
write.xlsx(format(all_Beckett_meanmaxmin_scores, digits = 2), "/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett_analysis_June/table_Beckett_MSTTR_meanmaxmin_all.xlsx", row.names = TRUE)

## Combining detailed MSTTR data (English/Italian)
detailed_Beckett_MSTTRscores_all <- rbind(detailed_Beckett_MSTTRscores_french, detailed_Beckett_MSTTRscores_english)
detailed_Beckett_MSTTRscores_all
write.xlsx(format(detailed_Beckett_MSTTRscores_all, digits = 2), "/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett_analysis_June/table_Beckett_MSTTRscores_detailed.xlsx", row.names = TRUE)
# Detailed MSTTR data  for each author and each language analysis is ultimately joined in table, and uploaded on Github repository Bilingual_writing





# Vocabulary Diversity MSTTR in koRpus, all authors all languages ---------------------------------
# This is just performed two times for each language (Fr/En/It/En)

# Load files from folder x
folder = ##YOURPATH##
  filenames <- list.files(folder)
texts <- c()
files <- dir(folder, full.names = TRUE)
for (f in files) {
  texts <- c(texts, paste(readLines(f), collapse = "\n"))
}

# Collecting years (last 4 letters in filename)
years <- c()
for (y in filenames){
  tmp = str_sub(filenames, start = -8, end = -5)
  years <- c(tmp)
}
# Collecting language of composition (first 2 letters in filename)
language <- c()
for (y in filenames){
  tmp = str_sub(filenames, start = 1, end = 2)
  language <- c(tmp)
}

# Preparing corpus with variables for Quanteda analysis
corpus <- corpus(texts) # Creating corpus.
docnames(corpus) <- filenames # Adding filenames
corpus$year <- years # Adding year variable
corpus$language <- language # Adding language of composition variable
# corpus$filenames <- filenames

# Tokenizing corpus & subsets
tokenized_corpus <- tokens(corpus)
tokenized_EN <- tokens(corpus_subset(corpus, language %in% "En"))
if ("Fr" %in% language){
  tokenized_FR <- tokens(corpus_subset(corpus, language %in% "Fr"))
} else {
  tokenized_IT <- tokens(corpus_subset(corpus, language %in% "It"))
}

# Calculating MSTTR scores
corpus_MSTTR <- textstat_lexdiv(tokenized_corpus, measure = "MSTTR", MSTTR_segment = 100)
EN_MSTTR <- textstat_lexdiv(tokenized_EN, measure = "MSTTR", MSTTR_segment = 100)
if (exists("tokenized_FR")){
  FR_MSTTR <- textstat_lexdiv(tokenized_FR, measure = "MSTTR", MSTTR_segment = 100)
} else {
  IT_MSTTR <- textstat_lexdiv(tokenized_IT, measure = "MSTTR", MSTTR_segment = 100)
}

# Calculate MSTTR mean, max. value, and min. value for each language
meanmaxmin_EN <- c(mean = mean(EN_MSTTR$MSTTR), max = max(EN_MSTTR$MSTTR), min = min(EN_MSTTR$MSTTR))
if (exists("FR_MSTTR")){
  meanmaxmin_FR <- c(mean = mean(FR_MSTTR$MSTTR),max = max(FR_MSTTR$MSTTR), min = min(FR_MSTTR$MSTTR))
} else {
  meanmaxmin_IT <- c(mean = mean(IT_MSTTR$MSTTR),max = max(IT_MSTTR$MSTTR), min = min(IT_MSTTR$MSTTR))
}

# Combine MSTTR mean, max, and min. values for each language in table
if (exists("meanmaxmin_FR")){
  table_meanmaxmin <- cbind(EN_texts = meanmaxmin_EN, FR_texts = meanmaxmin_EN)
} else {
  table_meanmaxmin <- cbind(EN_texts = meanmaxmin_EN, IT_texts = meanmaxmin_IT)
}

# MSTTR for whole corpus, each text individually with details
detailed_MSTTR <- cbind(corpus_MSTTR, year = corpus$year, language = corpus$language)

## Bonus
# Plot the scores of the whole corpus by year to check if there is a chronological trend
chron_plot <- ggplot(data=corpus_MSTTR, aes(corpus$year, MSTTR, color=corpus$language)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = docnames(corpus)))+ labs(x = "year of composition-start", y = "MSTTR")

# This is to double check that the subsets were as they should be
if (exists("tokenized_FR")){
  list_of_subsets <- cbind(docnames(tokenized_EN), docnames(tokenized_FR))
} else {
  list_of_subsets <- cbind(docnames(tokenized_EN), docnames(tokenized_IT))
}



# Sentence length ---------------------------------------------------------
# Reading corpus (from .txt files, directory)
folder = ##YOURPATH##
  file_names <- list.files(folder)
files <- dir(folder, full.names = TRUE)
text <- c()
for (f in files) {
  text <- c(text, paste(readLines(f), collapse = "\n"))
}
summary(text)

# Tokennizing texts in corpus
sentences <- tokenize_sentences(text)
length(sentences[[5]]) # Just to double check
sentence_words <- sapply(sentences, tokenize_words)

# Load metadata file (with file-names)
metadata <- read_csv2(sprintf(##Path to metadata csv##))
metadata
  
# To compute words per sentence
words_per_sentence <- c()
for (i in 1:nrow(metadata)){
words_per_sentence[[i]] <- sapply(sentence_words[[i]], length)
}
words_per_sentence # See output
  
# Compute median and mean words per sentence for all files. I compute median just to make sure mean words
# per sentence scores are not excessively higher (which would indicate parsing errors)
median_words_per_sentence <- sapply(words_per_sentence, median)
median_words_per_sentence
mean_words_per_sentence <- sapply(words_per_sentence, mean)
mean_words_per_sentence
# If all fine:
  
## Combine data in .xlsx file
# Add the total word-counts
words <- tokenize_words(text)
wordcount <- sapply(words, length)
  
# Add year (last letters in file-names)
years <- c()
for (y in file_names){
  tmp = str_sub(file_names, start = -8, end = -5)
  years <- c(tmp)
}
  
# Combine data
final <- cbind(median_words_per_sentence, mean_words_per_sentence, wordcount, years)
final
  
# Add row-names (file-names)
rownames(final) <- file_names # Name columns
  
# Write xlsx
#write.xlsx(final, "/Users/palleb/Desktop/Lahiri analysis/FINAL_EN.xlsx", row.names = TRUE)
  
# Intermediate step, opening final.xlsx in excel and double-checking results. Added column "abbr_title" with 
# an abbrieviated title (per text), and language of composition for easy visualization in subsequent plotting.   # Save as .CSV
  
# Plotting results (with ggplot)
library(ggrepel)

# Read csv data
mydata <- read_csv2(sprintf(##))
mydata
  
# Plot median words per sentence to year
#shape = genre
ggplot(data=mydata, aes(year, median_words_per_sentence, color=language, label = abbr_title, size = wordcount)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = abbr_title), size = 3) + labs(x = "year of publication", y = "median words per sentence")
# geom_text(nudge_y = .02) + labs(x = "median words per sentence", y = "start-year of composition") # Add this if overplotted for ease
  
# Plot mean words per sentence to  year
ggplot(data=mydata, aes(start_date, mean_words_per_sentence, color=language, label = abbr_title, size = wordcount)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = abbr_title), size = 3) + labs(x = "year of publication", y = "mean words per sentence")
  
ggsave(plot = last_plot(), filename = "finalmean_FR", device = "png", width = 6, height = 4, dpi = 600, 
       path = ##)
           
         

         