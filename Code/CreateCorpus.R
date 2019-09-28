
CreateCorpus <- function(TimeStamp, ignore = NULL)
{
  #TimeStamp = "02052018"
  library(tm)   #::Corpus, freqyency
  library(dplyr)#::select()
  
  #Read User Account Data
  FileName = paste("Data/UsersData_", TimeStamp, ".csv", sep = "")
  UsersData = read.csv(file = FileName, stringsAsFactors = FALSE)
  
  n = length(UsersData$Num) #no. of users
  
  #Read tweet text for all users
  AllTweets = data.frame(user=numeric(), text=character(), stringsAsFactors = FALSE)
  for(i in 1: n)
  {
    Num = UsersData$Num[[i]] #ids may not be serial!!
    
    if(Num %in% ignore)
      next
    
    #Read Prepared tweets from file
    ###################################
    FileName = paste("Data/Tweets/TweetData_user", Num , "_", TimeStamp,".csv", sep = "")
    TweetsData = read.csv(file = FileName, stringsAsFactors = FALSE)
    
    #convert all tweets of a user into one document
    TweetsText =  paste(TweetsData$Tweet, sep = '', collapse = '')
    perUser = data.frame(user=Num, text=TweetsText)
    #add to result
    AllTweets = rbind(AllTweets, perUser)
  }
  AllTweets$text = as.character(AllTweets$text)
  
  #data cleaning
  #########################
  #1st round
  TweetsText = cleanTweetText(AllTweets$text)
  Tweets_corpus = Corpus(VectorSource(TweetsText))
  #2nd round
  Tweets_corpus = tm_map(Tweets_corpus, content_transformer(tolower))
  Tweets_corpus = tm_map(Tweets_corpus, removePunctuation)
  Tweets_corpus = tm_map(Tweets_corpus, stripWhitespace)
  
  Tweets_corpus #return
  
}
cleanTweetText <- function(TweetsText){
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
  
}
