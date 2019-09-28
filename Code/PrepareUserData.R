#global data
pTerms = read.table("negative-words.txt", header = F, stringsAsFactors = F)[, 1]
nTerms =read.table("positive-words.txt", header = F, stringsAsFactors = F)[, 1]

PrepareUserData <- function(TimeStamp)
{
  #has to be called after PrepareTweets
  #TimeStamp = "02052018"
  
  library(plyr, quietly = TRUE)#::join()
  library(dplyr, quietly = TRUE)#::select()
  library(lubridate, quietly = TRUE)#::date()
  library(chron, quietly = TRUE)#::times()
  library(rtweet, quietly = TRUE) #::read_tweeter_csv()
  library(stringr, quietly = TRUE) #::replace_all()
  library(sentimentr)#::sentiment()

  #Read row users data from disk
  ####################################
  FileName = paste("Data/raw_usersData_", TimeStamp, ".csv", sep = "" )
  UsersData = read_twitter_csv(file = FileName)
  
  #First part of gathered data
  Part1 = UsersData %>% select(
    #internal id
    Num = Serial_no,
    #Twitter id
    id = id,
    #twitter name
    userName = screen_name, 
    #classification target
    Class = Class,
    #The number of followers this account currently has.
    Number_of_followers = followers_count,
    #The number of users this account is following
    Following = friends_count, 
    #Total number of posts
    Total_number_of_posts = statuses_count,
    #user Location
    Location = location,
    #user UTC offset
    UTCoffset  = utc_offset
  )
  #replace NA by 0
  Part1$UTCoffset[is.na(Part1$UTCoffset)] <- 0
  
  print("users Raw data Read successfuly...")
  
  #Second part of gathered data
  #Read from Prepared Tweet Data
  Part2 = data.frame()
  n = length(Part1$Num) #no. of users
  
  for(i in 1:n)
  {
    userNum = Part1$Num[[i]] 
    UTCoffset = Part1$UTCoffset[[i]]
    perUser = getTweetStats(userNum, UTCoffset)
    Part2 = rbind(Part2, perUser)
    print(paste("done with user ", i))
  }
  
  print("users tweet summary data Read successfuly...")
  
  userData = join(Part1, Part2, by="Num", type = "left")
  print("done merging parts...")
  
  FileName = paste("Data/UsersData_", TimeStamp, ".csv", sep = "")
  write.csv(userData, file= FileName ,row.names=FALSE, na="0")
  
}

getTweetStats <- function(userNum, UTCoffset)
{
  
  #Read tweet details from file
  ####################################
  FileName = paste("Data/Tweets/TweetData_user", userNum,"_", TimeStamp,".csv", sep = "")
  UserTweets = read.csv(file= FileName, stringsAsFactors = FALSE)
  
  #Average_Posts_Per_day
  ####################################
  y <- data.frame(days = as.numeric(lubridate::date(UserTweets$WhenUTC)))
  Average_Posts_Per_day = ceiling(mean(plyr::count(y, vars = "days")[[2]])) 
  
  #Time of posts (Majority of posts classified as Morning, Afternoon, evening, night)
  ####################################
  #adjust time zone using UTCoffset user info
  Times = chron::times(substr(UserTweets$WhenUTC, 12,19))
  #divide the times of the day into labeled segments
  breaks <- c(0,6, 12, 17, 22, 24) / 24 # times are internally fractions of a day
  labels <- c("night1", "morning", "afternoon", "evening", "night")
  ind <- cut(Times, breaks, labels, include.lowest = TRUE)
  ind <- str_replace_all(ind,"night1", "night")
  #get the name of the most frequent time of the day
  Time_of_posts = names(sort(table(ind), decreasing=TRUE)[1])
  
  #Total no. of replies, to others
  ####################################
  Total_number_of_replies = length(na.omit(UserTweets$IsReply))
  
  #Total number of hashtags
  ##################################
  Total_number_of_hashtags = sum(UserTweets$hashCount)
  
  #total number of mentions
  ###################################
  Total_number_of_mentions = sum(UserTweets$mentionCount)
  
  #Total number retweets : made by the user!!
  ###################################
  #Total_retweets = sum(sapply(userDetails$text, function(x) if_else(startsWith(x, "RT "), 1, 0)))
  Total_retweets = sum(UserTweets$IsRetweet, na.rm=TRUE)
  
  #Total number retweeted : by others!!
  ###################################
  Total_number_retweeted = sum(UserTweets$Retweeted, na.rm=TRUE)
  
  #Total number emojies 
  ###################################
  Total_number_emoji = sum(UserTweets$emojiCount, na.rm=TRUE)
  
  #sentiment score for each tweet then
  #average sentiment accross all tweets
  ################################################
  #Upweight the negative values in a vector while 
  #also downweighting the zeros in a vector. 
  #Useful for small text chunks with several sentences in
  #which some one states a negative sentence but then 
  # uses the social convention of several positive 
  #sentences in an attempt to negate the impact of the negative.
  #The affective state isn't a neutral but a 
  # slightly lessened negative state.
  ##################################################
  sentiment =  sentimentr::sentiment(get_sentences(as.character(UserTweets$Tweet)))
  MixedSntmScore = sentimentr::average_weighted_mixed_sentiment(sentiment$sentiment)
  AvgSntmScore = sentimentr::average_downweighted_zero(sentiment$sentiment)

  #count positive and negative words
  #######################################
  Terms = strsplit_space_tokenizer(UserTweets$Tweet)
  NoPosTerms = sum(as.numeric(Terms %in% pTerms))
  NoNegTerms = sum(as.numeric(Terms %in% nTerms))
  
  #Result
  #######################################
  perUser = data.frame(Num = userNum,
                       Average_Posts_Per_day= Average_Posts_Per_day, 
                       Time_of_posts = Time_of_posts,
                       Total_number_of_replies = Total_number_of_replies,
                       Total_number_of_hashtags = Total_number_of_hashtags, 
                       Total_number_of_mentions = Total_number_of_mentions,
                       Total_number_emojies = Total_number_emoji,
                       Total_number_of_retweets = Total_retweets, 
                       Total_number_retweeted = Total_number_retweeted,
                       MixedSntmScore = MixedSntmScore,
                       AvgSntmScore = AvgSntmScore,
                       Total_Num_Pos_Terms = NoPosTerms,
                       Total_Num_Neg_Terms = NoNegTerms)
  
}
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))