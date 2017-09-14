rdacb.getusers <- function(groups, apikey) {
  users <- as.data.frame(NULL)
  for (i in 1:nrow(groups)) {
    url <- paste0("https://api.meetup.com/2/members?group_id=",groups$id[i],"&only=id&key=",apikey)
    while (url != "") {
      temp <- fromJSON(RCurl::httpGET(url))
      if (class(temp$results) == "data.frame") {
        test <- cbind(group_id=groups$id[i],temp$results)
        users <- rbind(users,test)
      }
      url <- temp$meta$`next`
    }
    print(paste0("Fetched members for group ", i))
  }
  u <- data.frame(group_id=users$group_id,user_id=users$id)
}