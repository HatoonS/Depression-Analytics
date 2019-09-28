getRawTweets<- function(){
  
  #read user names
  users = read.csv("Data/users.csv")
  users$user = as.character(users$user)
  
  #connect to Twitter API
  authorize(1)
  
  #repeat for each user in list of users
  n = length(users$id) #no. of users
  for(i in 1:n)
  {
    id = users$id[[i]]
    user = users$user[[i]]
    userTl = userTimeline(user ,n=3200, includeRts = TRUE)
    
    if( is.data.frame(userTl) && nrow(userTl) == 0){
      print(paste("user ", user, "(", id, ")"," tweets are not accessible"))
      return
    }
    userTweets <- twListToDF(userTl)
    
    #Detect & Replace Emojies
    #################################
    userTweets$text = iconv(userTweets$text, from = "latin1", to = "ascii", sub = "byte")#encode emojies
    emoticons <- read.csv("emoji_dictionary.csv", header = T)
    library(DataCombine)
    userTweets <- DataCombine::FindReplace(data = userTweets, Var = "text", 
                                           replaceData = emoticons,
                                           from = "R_Encoding", to = "Name", 
                                           exact = FALSE)
    
    
    #Dump "All" tweet details to file
    ####################################
    FileName = paste("Data/RawTweets/", "TweetData_user", id , "_", format(Sys.Date(), "%d%m%Y"),".csv", sep = "")
    write_as_csv(userTweets, file_name = FileName)
    print(paste("user (", id, ")"," tweets fetched successfully !!"))
    
    
  }
  
}