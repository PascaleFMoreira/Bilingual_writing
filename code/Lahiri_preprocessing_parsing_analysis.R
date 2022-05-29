#Preprocessing corpus Lahiri_IT - including punctuation marks in MFW
#Set wd to corpus folder
setwd("##YOURPATH##")

#Standardizing miscallenous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

#Putting space around all punctuation marks in order to include them in 
# examined features (MFW) in Stylo
# but excluding apostrophes ' or ’) 
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

#Now back to wd
setwd("../")

#stylo(splitting.rule = ("[ \t\n]+"))
#Or rather, saving parsed corpus to variable, changing stylo default splitting.rule to using only 
#space as splitting.rule (includes punctuation marks)
LahiriITcorp = load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type = "plain",
                                     corpus.lang = "Italian", splitting.rule = ("[ \t\n]+"), 
                                     sampling = "no.sampling",features = "w",ngram.size = 1, 
                                     preserve.case = FALSE, encoding = "UTF-8")
stylo(parsed.corpus=LahiriITcorp)


#---------------------------------

#Same for English corpus
#Preprocessing corpus Lahiri_IT - including punctuation marks in MFW
#Set wd to corpus folder
setwd("##YOURPATH##")

#Standardizing miscellaneous quotation marks to "
filelist=list.files()
for(file in filelist){
  quot=readLines(file)
  quot1=gsub("(«+|»+|”+|“)", '\"', quot)
  writeLines(quot1, con=file)
}

#Putting space around all punctuation marks in order to include them in 
# examined features (MFW) in Stylo
# but excluding apostrophes (' or ’)
filelist=list.files()
for(file in filelist){
  tx=readLines(file)
  tx2=gsub("\\s*(\\.+|[[:punct:]])(?<!\\b['’]\\b)\\s*", " \\1 ", tx, perl=TRUE)
  writeLines(tx2, con=file)
}

#Now back to wd
setwd("../")

LahiriENcorp = load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type = "plain",
                                     corpus.lang = "English.all", splitting.rule = ("[ \t\n]+"), 
                                     sampling = "no.sampling",features = "w",ngram.size = 1, 
                                     preserve.case = FALSE, encoding = "UTF-8")
stylo(parsed.corpus=LahiriENcorp)


#---------------------------------

summary(LahiriENcorp)
summary(LahiriITcorp)
