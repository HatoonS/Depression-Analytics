#use this function to split the text of tweets 
#for a specific user into words and compute
#word frequenceies

TweetFreq<- function(TweetsText, Stemmed = FALSE)
{
  library(tm)
  library(tm.lexicon.GeneralInquirer)#::sentements score
  library(sentimentr)#::get_sentiment
  
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
  #remove capitalization
  TweetsText = tolower(TweetsText)
  #remove stop words
  TweetsText = removeWords(TweetsText, stopwords("english"))
  #remove collapsed whitespace
  TweetsText = stripWhitespace(TweetsText)
  #remove Punctuation
  TweetsText = removePunctuation(TweetsText, preserve_intra_word_dashes = TRUE)
  
  #convert all tweets of a user into one document
  text =  paste(TweetsText, sep = '', collapse = '')
  
  #Compute frequency
  ctrl = list( tokenize = strsplit_space_tokenizer,
               tolower = TRUE,
               removePunctuation = TRUE,
               stopwords = TRUE,
               stemming = Stemmed
               #,dictionary = what to leave and what to delete
  )
  
  #Find term frequency
  FreqTable = termFreq(text, control = ctrl)
  
  posTerms = tm_term_score(text, terms_in_General_Inquirer_categories("Positiv"))
  NegTerms = tm_term_score(FreqTable, terms_in_General_Inquirer_categories("Negativ"))
  
  
  text2 =  paste(TweetsText, sep = '', collapse = '')
  text3 =  paste(TweetsText, sep = '', collapse = '')
  
  new_Text = rbind(text, text2, text3)
  
  corpus <- VCorpus(VectorSource(new_Text))
  dtm <- as.TermDocumentMatrix(corpus)
  dtm2 <- removeSparseTerms(dtm, 0.1)
  freq <- colSums(as.matrix(dtm))
 
  
  
    
    #Find term frequency
    FreqTable = termFreq(TweetsText, control = ctrl)
    tdm = TermDocumentMatrix(FreqTable)
    
    
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