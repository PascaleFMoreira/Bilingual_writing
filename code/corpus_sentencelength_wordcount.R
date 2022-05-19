library(tidyverse)
library(tokenizers)
library(tm)
library(stringr)

# Code for computing corpus data, sentence-length, wordcount.
# Code is run twice, one for each language-corpus (setting the appropriate folder path)

# Reading corpus (from .txt files, directory)
folder = "/Users/palleb/Desktop/sentences/FR_corpus"
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
sentence_words # Check

# Load metadata
metadata <- read_csv2(sprintf("##MYDATA##"))
metadata

# To compute words per sentence
words_per_sentence <- c()
for (i in 1:nrow(metadata)){
  words_per_sentence[[i]] <- sapply(sentence_words[[i]], length)
}
words_per_sentence # See output

# Compute median and mean words per sentence for all files. I compute median just to make sure mean words
# per sentence results are not excessively higher (indicates parsing errors)
median_words_per_sentence <- sapply(words_per_sentence, median)
median_words_per_sentence
mean_words_per_sentence <- sapply(words_per_sentence, mean)
mean_words_per_sentence

# Add the total wordcounts as follows:
words <- tokenize_words(text)
wordcount <- sapply(words, length)

# Combine data
final <- cbind(median_words_per_sentence, mean_words_per_sentence, wordcount)
final
# Add row-names (file-names)
file_names <- list.files(folder)
rownames(final) <- file_names # Name columns
final
# Write xlsx
write.xlsx(final, "##PATHNAME##", row.names = TRUE)

# Intermediate step, opening final.xlsx in excel and double-checking results. Added column "abbr_title" with 
# an abbrieviated titles (per text) for easy visualization in subsequent plotting.

# Plotting results (with ggplot)
# Read xlsx
library(ggrepel)
mydata <- read_csv2(sprintf("##MYDATA.csv2##"))
mydata

# Plot median words per sentence to start date
ggplot(data=mydata, aes(start_date, median_words_per_sentence, color=language, label = abbr_title, size = wordcount)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = abbr_title, alpha = 0.05))+ labs(x = "start-year of composition", y = "median words per sentence")
# geom_text(nudge_y = .02) + labs(x = "median words per sentence", y = "start-year of composition")
# Plot mean words per sentence to start date
ggplot(data=mydata, aes(start_date, mean_words_per_sentence, color=language, label = abbr_title, size = wordcount)) + 
  geom_point() + ggrepel::geom_text_repel(aes(label = abbr_title, alpha = 0.05)) +labs(x = "start-year of composition", y = "mean words per sentence")

