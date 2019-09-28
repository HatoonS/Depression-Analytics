authorize<- function(code=1)
{
  #####################################
  #Authentication for twitter API 
  #####################################
  app_name = "Depression Analytics"
  api_key <- "AvD0JpFL8c4wHnfJK1R4o33iK" 
  api_secret <- "VHsSf5M1zSSMCF351tAy8RtLB9phGxX4UGDuW6XNDI5BsusUuI" 
  token <- "4150033126-lfyWVq5zKLWEgFGxFRaeV3Z1NT3jFYvv2DtxBMn" 
  token_secret <- "8wnPfiP0LzB8odrXzQroAh9tQlZJB86LV10YuZhMZlsts" 
  
  if (code == 1){
    #authonticate using TwitteR package
    library(twitteR)
    setup_twitter_oauth(api_key, api_secret, token, token_secret)
  }
  else if (code == 2){
    #authonticate using Rtweet package
    library(rtweet)
    library(httr)
    twitter_token <- create_token(
      app = app_name,
      consumer_key = api_key,
      consumer_secret = api_secret)
    
  }
    
}
