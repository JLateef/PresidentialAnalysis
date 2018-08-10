
#set working directory, and import necessary packages
setwd('C:/Users/JLateef/Downloads')
require(devtools)
install_github("lchiffon/wordcloud2")
install.packages("wordcloud2", repos = "https://github.com/lchiffon/wordcloud2")
library(wordcloud2)
library(rJava)
library(png)
library(knitr)
library(tm)
library(qdap)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(SnowballC)
library(shiny)

#suppress warning messages
options(warn = -1)

source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')


#import Obama's inauguration address, and load it into a volatile corpus 
text <- readLines("C:/Users/JLateef/Documents/Obama.txt")

obama_corpus <- VCorpus(VectorSource(text))

ocorpus <- VCorpus(obama_corpus)

content(obama_corpus[[14]])

inspect(obama_corpus)

#remove stopwords from the corpus
stopwords("en")


removeWords(obama_corpus, stopwords("en"))


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

obama_corpus <- tm_map(obama_corpus, toSpace, "/")

obama_corpus <- tm_map(obama_corpus, toSpace, "@")

obama_corpus <- tm_map(obama_corpus, toSpace, "\\|")

inspect(obama_corpus)

obama_corpus[[66]][1]



#automated function that cleans our corpus
cleaned_corpus <- function(obama_corpus)  {
  
  # Remove punctuations
  obama_corpus <- tm_map(obama_corpus, removePunctuation)
  
  obama_corpus <- tm_map(obama_corpus, tolower)
  
  
  obama_corpus <- tm_map(obama_corpus, PlainTextDocument)
  
  #add more stop words that are irrelevant to our analysis
  new_stops <- c("(Applause)", "(applause)", "applause", "will", "ago", "pat", "ask", "khe", "came", "act", "icy", "till", stopwords("english"))
  
  obama_corpus <- tm_map(obama_corpus, removeWords, new_stops)
  
  
  # Eliminate extra white spaces
  obama_corpus <- tm_map(obama_corpus, stripWhitespace)
  
  
}

return(obama_corpus)


#cleaned obama corpus

cleanobama <- cleaned_corpus(obama_corpus)

#turn our corpus into a term document matrix
dtm <- TermDocumentMatrix(cleanobama)

m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)

head(d, 10)

#cleaned_corpus(dtm)

#set the seed for possible reproduction of code
set.seed(2008)

#form the word cloud. I used wordcloud2 as well because it's cooler to me :)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(d, color = "random-light", backgroundColor = "grey")




#do the same as we did above, but with Trump's address
text2 <- readLines("C:/Users/JLateef/Documents/Trump.txt")

trump_corpus <- VCorpus(VectorSource(text2))

tcorpus <- VCorpus(trump_corpus)

content(trump_corpus[[14]])

inspect(trump_corpus)

stopwords("en")

removeWords(trump_corpus, stopwords("en"))


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

trump_corpus <- tm_map(trump_corpus, toSpace, "/")

trump_corpus <- tm_map(trump_corpus, toSpace, "@")

trump_corpus <- tm_map(trump_corpus, toSpace, "\\|")

inspect(trump_corpus)

trump_corpus[[66]][1]


cleaned_corpus <- function(trump_corpus)  {
  
  # Remove punctuations
  trump_corpus <- tm_map(trump_corpus, removePunctuation)
  
  trump_corpus <- tm_map(trump_corpus, tolower)
  
  trump_corpus <- tm_map(trump_corpus, PlainTextDocument)
  
  new_stops <- c("america", "make", "again", "great", "will", "let", "died", "now", "back", "right", "every", "talk", "day", "must", "like", "one", "yes", "would've", "should've", "could've", stopwords("english"))
  
  trump_corpus <- tm_map(trump_corpus, removeWords, new_stops)
  
  
  
  
  # Eliminate extra white spaces
  trump_corpus <- tm_map(trump_corpus, stripWhitespace)
  
  
}

return(trump_corpus)


#cleaned trump corpus
cleantrump <- cleaned_corpus(trump_corpus)

dttm <- TermDocumentMatrix(cleantrump)

mm <- as.matrix(dttm)

vv <- sort(rowSums(mm),decreasing=TRUE)

dd <- data.frame(word = names(vv),freq=vv)

head(dd, 10)
#cleaned_corpus(dttm)


#seed for reproducing code
set.seed(1234)
wordcloud(words = dd$word, freq = dd$freq, min.freq = 2,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(dd, color = "random-light", backgroundColor = "grey")








