#use this function to split the text of tweets 
#for a specific user into words and compute
#word frequenceies

TweetTokenize<- function(TweetsText, Stemmed = FALSE)
{
  #library(tm)
  
  #Data cleaning
  ####################################################
  #Remove mentions
  TweetsText = gsub("@([^ ]*)", " ", TweetsText)
  #Remove Retweet tag
  TweetsText = gsub("^RT", " ", TweetsText)
  #Remove links
  TweetsText = gsub("http(s?)([^ ]*)", " ", TweetsText, ignore.case = T)
  #remove html '&amp'
  TweetsText = gsub("&amp", "and", TweetsText)
  #Remove unrecognized emojies
  TweetsText = gsub("<(.*)>", " ", TweetsText, ignore.case = T)
  #Remove hashTag symbol
  TweetsText = gsub("#", " ", TweetsText)
  #remove collapsed whitespace
  TweetsText = stripWhitespace(TweetsText)
  
  #Compute frequency
  ctrl = list( tokenize = strsplit_space_tokenizer,
    tolower = TRUE,
    removePunctuation = TRUE,
    stopwords = TRUE,
    stemming = Stemmed
    #,dictionary = what to leave and what to delete
    )
    
    #Find term frequency
    FreqTable = termFreq(TweetsText, control = ctrl)
    FreqTable = as.data.frame(FreqTable)
    #make the extrcted words a separate col
    Term = as.character(row.names(FreqTable))
    colnames(FreqTable) = "Freq"
    Freq = as.integer(FreqTable$Freq)
    #compose final result
    FreqTable = data.frame(Term, Freq)

}
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))