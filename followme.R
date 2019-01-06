# Author: MHGottlieb
# Date: 2019-01-06
# --------------
# Description:
# Messing around with the follow-function
# 
# --------------

# Dependencies ------------------------------------------------------------

  library(rtweet)
  library(dplyr)

# Token -------------------------------------------------------------------

  consumer_key <- readLines("Keychain/consumer_key.txt", warn=FALSE)
  consumer_secret <- readLines("Keychain/secret_key.txt", warn=FALSE)

  token <- create_token(
    app = "Helge",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret)

# getting data ------------------------------------------------------------

  user <- "MHGottlieb"
  
  # users currently followed
  follows_now <- get_friends(user) 
  follows_now <- lookup_users(follows_now$user_id)[,c(1,4)] #just user id and @handle
  follows_now <- cbind(follows_now, date=Sys.Date(), following=TRUE)
  
  # users followed before
  follows_old <- readRDS
  
  
  temp <- merge(follows_old, follows_now, by="user_id", all.x=TRUE)
  
  follows <- cbind(
    user_id=temp$user_id,
    screen_name=temp$screen_name.x,
    date=temp$date.x,
    followed_before=temp$still_following,
    following_now=!is.na(temp$following),
    following_base=temp$following,
    follow_from_R=FALSE,
    
    )
  
  lists <- c("dkgreen", "EUDK", "dkmedia", "politik")
  owner_user <- c("karmel80", "karmel80", "karmel80",  "BayMads")
  out <- NULL
  
  
  
  for(i in 1:length(lists)){
    temp <- lists_members(slug=lists[i], owner_user=owner_user[i])
    out <- unique(rbind(out, temp))
    message(paste(i, "/", length(lists)))
  }
  
  out <- out[c(1,3, )]
  
    
  
  list <- lists_members(slug="dkgreen", owner_user="karmel80", n=5000)
  
  
  
  
  
  