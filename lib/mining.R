# Import necessary libraries
library(tm)
library(wordcloud)
library(dplyr)
library(tidytext)

# Relative path points to the local folder
folder.path="../data/InauguralSpeeches/"

# get the list of file names
speeches=list.files(path = folder.path, pattern = "*.txt")

# Truncate file names so it is only showing "FirstLast-Term"
prez.out=substr(speeches, 6, nchar(speeches)-4)

# Create a vector NA's equal to the length of the number of speeches
length.speeches=rep(NA, length(speeches))

# Create a corpus
ff.all<-Corpus(DirSource(folder.path))

# Use tm_map to strip all spaces to a single space, to lower case, remove filler words, empty strings and punctuation.
ff.all<-tm_map(ff.all, stripWhitespace)
ff.all<-tm_map(ff.all, content_transformer(tolower))
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))
ff.all<-tm_map(ff.all, removeWords, c("can", "may", "upon", "shall", "will", "must", ""))
# ff.all<-tm_map(ff.all, gsub, pattern = "free", replacement = "asdf")
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)
ff.all<-tm_map(ff.all, stemDocument)

# tdm.all =  a Term Document Matrix
tdm.all<-TermDocumentMatrix(ff.all)

#Bar Plot
termFrequency <- rowSums(as.matrix(tdm.all))
termFrequency <- subset(termFrequency, termFrequency>=150)
library(ggplot2)
barplot(termFrequency)


