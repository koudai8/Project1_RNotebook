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
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)

# tdm.all =  a Term Document Matrix
tdm.all<-TermDocumentMatrix(ff.all)

# tdm.tidy = a dataframe with three columns: words, document, count, over all words and speeches
tdm.tidy=tidy(tdm.all)

# tdm.overall = a tibble with two columns: word, count, over all speeches
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))

# Create a wordcloud using all words from all speeches, with font size ranging from 5 to 0.5, max number of words on the plot = 100,
# words below frequency of one will be omitted, plot the words in decreasing frequency, 30% of words have horizontal orientation,
# use R for collision detection, choose color based on frequency, and choose color based on colorbrewer color spectrum.
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=1000,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=TRUE,
          random.color=False,
          colors=brewer.pal(9,"Blues"))

# Construct a document-term matrix using a corpus, and tf-idf (term frequenicy - inverse document frequency), 
# which accounts for the frequency of the word in the document, offsetted by the frequency of the word in the entire corpus
dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))


# Tidy a DocumentTermMatrix into a three-column data frame: term{},and value (with zeros missing), with one-row-per-term-per-document.
ff.dtm=tidy(dtm)

# Stem words in a text document using Porter's stemming algorithm (The line below was originally commented out)
#ff.all<-tm_map(ff.all, stemDocument)


# Loop through all documents to create wordcloud for each
for(i in 1:length(speeches)){
  
 # Stem words in a text document using Porter's stemming algorithm (The line below was originally commented out)
 #crude=stemDocument(ff.all[[i]])
 
 # Create a corpus for ith document
 crude=Corpus(VectorSource(ff.all[[i]]))
 
 # Create a term-document matrix for ith document with words greater than 3 letters
 tdm <- TermDocumentMatrix(crude[1], list(wordLengths=c(3, Inf)))
 
 # Creates a nx2 matrix with term and their frequency in ith document
 m <- as.matrix(tdm)
 
 # Create a Named Num in Descending order with names being the terms
 v <- sort(rowSums(m),decreasing=TRUE)
 
 # Create a data frame from v
 d <- data.frame(word = names(v),freq=v)
  
  # This allows the output of wordcloud to go to a folder instead of displayed in a plot
  png(paste("../output/", prez.out[i], ".png", sep=""),
      width=300, height=300)
  
  # 
  wordcloud(ff.dtm$term[ff.dtm$document==speeches[i]],
            ff.dtm$count[ff.dtm$document==speeches[i]],
              
            # Font size range from 0.5 to 5
            scale=c(5,0.5),
              
            # Plot max 200 words on the plot
            max.words=200,
              
            # Words below this frequency are omitted
            min.freq=1,
            
            # Size of words are in descending order with respect to frequency
              random.order=FALSE,
              
            # 0% are horizontal
            rot.per=0,
            
            # Use C++ for collision detection
              use.r.layout=FALSE,
              
            # Use color based on frequency (darker --> more frequent)
            random.color=FALSE,
              
            # Use colors from this spectrum of 10 blue colors
            colors=brewer.pal(10,"Blues"), 
            
            # No idea...
            main=prez.out[i])
  
  # This command turns off graphical device and concludes plotting (for the ith wordcloud)
  dev.off()
  
  }