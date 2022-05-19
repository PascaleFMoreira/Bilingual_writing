library(tidyverse)
library(tokenizers)

text <- paste("Now, I understand that because it's an election season",
              "expectations for what we will achieve this year are low .",
              "But, Mister Speaker, I appreciate the constructive approach.", "All is lost.")

sentences <- tokenize_sentences(text)
sentence_words <- tokenize_words(sentences[[1]])
sentence_words

sentence_length = list()
for (i in sentence_words){
  sentence_length = sapply(sentence_words[[i]], length)
}

# To see how many sentences are in text
length(sentence_words)

# To see how many words are in each sentence
sapply(sentence_words, length)


file = "/Users/palleb/Desktop/sentences/noter copy.txt"
text = paste(readLines(file), collapse = "\n")
words <- tokenize_words(text)
length(words[[1]])

sentences <- tokenize_sentences(text[[1]])
sentence_words <- tokenize_words(sentences)
sentence_words



## Use on directory of files:

input_loc = "/Users/palleb/Desktop/sentences/corpus"
files <- dir(input_loc, full.names = TRUE)
texts <- c()
for (f in files) {
  texts <- c(texts, paste(readLines(f), collapse = "\n"))
}


## Metadata?
metadata <- read_csv(sprintf(sep = ";", "/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/metadata_beckett.csv"))
metadata

# # See total number of words per text 
# words <- tokenize_words(text)
# sapply(words, length)

# Split each text into sentences
sentences <- tokenize_sentences(texts)

# Tokennize words for each sentence
sentence_words <- sapply(sentences, tokenize_words)

length <- sapply(sentences, length)

# file_list = list.files()

sentence_length = list()
for (i in texts){
  sentence_length[[i]] = sapply(sentence_words[[i]], length)
}


