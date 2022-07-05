library(stylo)
library(xlsx)

# Preprocessing corpora in each language and keeping punctuation marks withing the 600 MFF

# Preprocessing corpus Beckett English  --------
#Preprocessing corpus Beckett_EN - including punctuation marks in MFW

#Set wd to corpus folder
setwd()
#Standardizing miscallenous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("('+|‘+|‛)", '’', quot)
  writeLines(quot1, con=file)
}

#Putting space around all punctuation marks in order to include them in 
# examined features (MFW) - yet excluding apostrophes: ' or ’) 
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

# back to wd
setwd("../")

#stylo(splitting.rule = ("[ \t\n]+"))
#Or rather, saving parsed corpus to variable, changing stylo default splitting.rule to using only 
#space as splitting.rule (includes punctuation marks)
BeckettENcorp = load.corpus.and.parse(files = "all", corpus.dir = "En_Corpus", markup.type = "plain",
                                     corpus.lang = "English.all", splitting.rule = ("[ \t\n]+"), 
                                     sampling = "no.sampling",features = "w",ngram.size = 1, 
                                     preserve.case = FALSE, encoding = "UTF-8")




# Same for Beckett French ---------------------------------

#Preprocessing corpus - including punctuation marks in MFW
setwd()

#Standardizing miscellaneous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

#Putting space around all punctuation marks in order to include them in 
# examined features (MFW):: tx2=gsub("(\\.+|[[:punct:]])", " \\1 ", tx)
# but excluding apostrophes (' or ’)
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

#Now back to wd
setwd("../")

BeckettFRcorp = load.corpus.and.parse(files = "all", corpus.dir = "Fr_Corpus", markup.type = "plain",
                                     corpus.lang = "French", splitting.rule = ("[ \t\n]+"), 
                                     sampling = "no.sampling",features = "w",ngram.size = 1, 
                                     preserve.case = FALSE, encoding = "UTF-8")



# Wordcount EN--------------------------------------------
# word count for books in Beckett English corpus and save as file
wordcount_EN = data.frame()
for(b in BeckettENcorp){
  tmp = length(b)
  wordcount_EN= rbind(wordcount_EN, tmp)
}

# make titles the rownames of data frame
rownames(wordcount_EN)=names(BeckettENcorp)

# write results to xlsx file
write.xlsx(wordcount_EN,"/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett_wordcount_EN.xlsx", 
           row.names = TRUE)


# Wordcount FR --------------------------------------------
# same as for EN corpus
wordcount_FR = data.frame()
for(b in BeckettFRcorp){
  tmp = length(b)
  wordcount_FR= rbind(wordcount_FR, tmp)
}

# make titles the rownames of data frame
rownames(wordcount_FR)=names(BeckettFRcorp)
# write results to xlsx file
write.xlsx(wordcount_FR,"/Users/palleb/Library/Mobile Documents/com~apple~CloudDocs/Thesis Bilingual Creativity/Samuel Beckett/Beckett_wordcount_FR.xlsx",
           row.names = TRUE)





# Preprocessing corpus Lahiri Italian -----------------

setwd() # Set wd to corpus folder

#Standardizing miscellaneous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

# Putting space around all punctuation marks in order to include them in 
# examined features (MFW); but excluding apostrophes (' or ’)
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

setwd("../") #  Now back to wd

LahITcorp = load.corpus.and.parse(files = "all", corpus.dir = "IT_corpus", markup.type = "plain",
                                   corpus.lang = "Italian", splitting.rule = ("[ \t\n]+"), 
                                   sampling = "no.sampling",features = "w",ngram.size = 1, 
                                   preserve.case = FALSE, encoding = "UTF-8")



# Same for Lahiri English ----------------

setwd() # Set wd to corpus folder

#Standardizing miscallenous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

#Putting space around all punctuation marks in order to include them in 
# examined features (MFW); but excluding apostrophes ' or ’) 
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

setwd("../") # Now back to wd

#stylo(splitting.rule = ("[ \t\n]+"))
#Or rather, saving parsed corpus to variable, changing stylo default splitting.rule to using only 
#space as splitting.rule (includes punctuation marks)
LahENcorp = load.corpus.and.parse(files = "all", corpus.dir = "EN_corpus", markup.type = "plain",
                                   corpus.lang = "English.all", splitting.rule = ("[ \t\n]+"), 
                                   sampling = "no.sampling",features = "w",ngram.size = 1, 
                                   preserve.case = FALSE, encoding = "UTF-8")


# Wordcount IT ------------------------------------------------------------
wordcount_IT = data.frame()
for(b in LahITcorp_p){
  tmp = length(b)
  wordcount_IT= rbind(wordcount_IT, tmp)
}

# make titles the rownames of data frame
rownames(wordcount_IT)=names(LahITcorp_p)
wordcount_IT

# Wordcount EN ------------------------------------------------------------

wordcount_EN <- data.frame()
for(b in LahENcorp){
  tmp <- length(b)
  wordcount_EN <- rbind(wordcount_EN, tmp)
}

# make titles the rownames of data frame
rownames(wordcount_EN) <- names(Lah_ENcorp)
wordcount_EN


summary(LahENcorp)
summary(LahITcorp)


