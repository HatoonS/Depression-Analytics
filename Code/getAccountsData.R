getAccountsData<- function(TimeStamp){
  #Read account measures as_is
  
  #Read User Account Data
  FileName = paste("Data/UsersData_", TimeStamp, ".csv", sep = "")
  UsersData = read.csv(file = FileName, stringsAsFactors = FALSE)
  
  UsersData$Class = as.factor(UsersData$Class)
  UsersData$Time_of_posts = as.factor(UsersData$Time_of_posts)
  
  UsersData = UsersData %>%
    select(Class,
           Number_of_followers,
           Following,
           Total_number_of_posts,
           Average_Posts_Per_day,
           Time_of_posts,
           Total_number_of_replies,
           Total_number_of_hashtags,
           Total_number_of_mentions,
           Total_number_emojies,
           Total_number_of_retweets,
           Total_number_retweeted,
           Total_Num_Pos_Terms,
           Total_Num_Neg_Terms)
  
  #return
  UsersData
  
}