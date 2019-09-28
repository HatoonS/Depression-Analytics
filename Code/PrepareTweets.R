PrepareTweets<- function(TimeStamp)
{
 #TimeStamp = "02052018"

  library(rtweet, quietly = TRUE)#::read_tweet_csv
  library(dplyr, quietly = TRUE)#::select()
  library(stringr, quietly = TRUE)#::replace()()
  
  #read user names
  users = read.csv("Data/users.csv")
  users$user = as.character(users$user)
  
  n = length(users$id) #no. of users
  for(i in 1: n)
  {
    user = users$user[[i]]
    
    #Read Raw tweets from file
    FileName = paste("Data/RawTweets/TweetData_user", i , "_", TimeStamp,".csv", sep = "")
    RawTweets = read_twitter_csv(file = FileName)

    #extract needed data only
    #Extracting details..
    UserTweets <- select(RawTweets, 
                         Tweetid = "id",
                         Tweet = "text",
                         WhenUTC = "created",
                         Retweeted = "retweetCount",
                         Likes = "favoriteCount", 
                         IsRetweet ="isRetweet",
                         IsReply = "replyToUID")
    


    #compute extra info
    ########################################################
    UserTweets$IsRetweet = as.numeric(UserTweets$IsRetweet)
    UserTweets$IsReply = ifelse(is.na(UserTweets$IsReply), 0, 1)
    
    #Hashtag info
    #############################
    # split words
    words_list = strsplit(UserTweets$Tweet, " ")
    # how many hashtags per tweet
    hashCount = sapply(words_list, function(x) length(grep("#", x)))
    #update data frame with hash count
    UserTweets = mutate(UserTweets, hashCount= hashCount)
    
    #Mentions Info
    #############################
    # how many mentions per tweet
    mentionCount = sapply(words_list, function(x) length(grep("@", x)))
    #update data frame with mentions count
    UserTweets = mutate(UserTweets, mentionCount= mentionCount)
    
    #Emoji Usage
    #################################
    emoticons<- read.csv("emoji_dictionary.csv", header = T)#read imojies list
    emos = str_replace_all(emoticons$Name," ", "")
    emojiCount = sapply(words_list, function(x){sum(x %in% emos)})
    #update data frame with mentions count
    UserTweets = mutate(UserTweets, emojiCount = emojiCount)
    
    
    #Writing extracted tweet details to file
    ####################################
    FileName = paste("Data/Tweets/TweetData_user", i,"_", TimeStamp,".csv", sep = "")
    write.csv(UserTweets, file= FileName ,row.names=FALSE, na="0")
    
    print(paste("user (", i, ")"," tweets Prepared successfully !!"))
    
  }
}