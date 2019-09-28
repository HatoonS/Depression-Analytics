TokenizeAllTweets<-function(TimeStamp, Stemmed=FALSE)
{
  #TimeStamp = "02052018"
  library(plyr) #::join()
  library(tm)   #::TweetTokenize()
  
  users = read.csv("Data/users.csv")

  n = length(users$Serial_no) #no. of users
  result = data.frame()
  
  for(i in 1: n)
  {
    Num = users$Serial_no[[i]] #ids may not be serial!!
    
    #Read Prepared tweets from file
    ###################################
    FileName = paste("Data/Tweets/TweetData_user", Num , "_", TimeStamp,".csv", sep = "")
    TweetsData = read.csv(file = FileName)
    
    #Tokenize & compute Freq
    ##############################
    FreqTable = TweetTokenize(TweetsData$Tweet, Stemmed = Stemmed)
    #rename frequency col to indicate user
    colnames(FreqTable) <- c("Term", paste("user", Num))

    #Merge into result
    ##############################
    result = suppressMessages(join(result, FreqTable, type="full"))
    print(paste("user (", Num, ")"," tweets Tokenized successfully !!"))
    
    
  }#end of loop
  
  
  # #transpose result to match required format
  # Terms = result$Term #keep aside
  # result = result[ , !(names(result) %in% "Term")]
  # result = t(result)
  # colnames(result) <- Terms
  
  #Writing tokenized tweet frequenceies to file
  ####################################
  x = ifelse(Stemmed, "", "non-")
  FileName = paste("Data/TokenFreq_", x,"Stemmed_", TimeStamp,".csv", sep = "")
  write.csv(result, file= FileName ,row.names=TRUE, na="0")
  
  
}