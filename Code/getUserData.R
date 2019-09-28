#connect to twitter
#crawling user account details
#save in a csv file with a time stamp!!
getRawUsersData<- function(){
  
  #read user names
  users = read.csv("Data/users.csv", stringsAsFactors = FALSE)
 
  #####################################
  # crawl Twitter for account info
  #####################################
  #create a token that authenticates access to tweets
  token <- authorize(code =2)
 
  UsersData = data.frame()
  for ( i in users$Serial_no){
  
    user = users$user[[i]]
    #get the user profile
    userReq = tw_api_get_user(userName = user, twitter_token = token)
    if(i==185){ #"User not found."
      next
    } 
    #format as data frame
    userFrame = format_user_content(userReq)
    userFrame$Serial_no = users$Serial_no[[i]]
    userFrame$Class = users$Class[[i]]
    
    #update the result
    UsersData = rbind(UsersData, userFrame)
    print(paste("user (", i, ")"," profile fetched successfully !!"))
  }
  
  #dump row data to file
  ####################################
  FileName = paste("Data/raw_usersData_", format(Sys.Date(), "%d%m%Y"),".csv", sep = "")
  write_as_csv(UsersData, file_name = FileName)
  
  #Update list of users
  ###############################
  users = data.frame(Serial_no = c(1:length(UsersData$id)), id = UsersData$id, user = UsersData$screen_name)
  write.csv(users, "Data/users.csv")
  
   
}
tw_api_get_user <- function(twitter_token, userName)
{
  q = "https://api.twitter.com/1.1/users/show.json"
  req <- GET(q, config(token=twitter_token),query=list(user_id=NULL, screen_name=userName, include_entities=TRUE))
  req <- content(req)
  
}
format_user_content<- function(userReq){
  
  # Nullable elements
  time_zone   <- userReq$time_zone
  utc_offset  <- userReq$utc_offset
  description <- userReq$description
  location    <- userReq$location
  
  userFrame <- data.frame(
    stringsAsFactors = FALSE,
    id                      = userReq$id,
    name                    = userReq$name,
    screen_name             = userReq$screen_name,
    contributors_enabled    = userReq$contributors_enabled,
    created_at              = strptime(userReq$created_at,'%a %b %d %T +0000 %Y',tz = 'UTC'),
    default_profile         = userReq$default_profile,
    default_profile_image   = userReq$default_profile_image,
    description             = ifelse(is.null(description),NA,description),
    favourites_count        = userReq$favourites_count,
    followers_count         = userReq$followers_count,
    friends_count           = userReq$friends_count,
    geo_enabled             = userReq$geo_enabled,
    is_translator           = userReq$is_translator,
    lang                    = userReq$lang,
    listed_count            = userReq$listed_count,
    location                = ifelse(is.null(location),NA,location),
    profile_image_url       = userReq$profile_image_url,
    profile_image_url_https = userReq$profile_image_url_https,
    protected               = userReq$protected,
    statuses_count          = userReq$statuses_count,
    time_zone               = ifelse(is.null(time_zone),NA,time_zone),
    utc_offset              = ifelse(is.null(utc_offset),NA,utc_offset),
    verified                = userReq$verified
  )
}
  