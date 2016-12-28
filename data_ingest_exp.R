# This file serves as an experimetnal workspace
# I don't intend this to be included in the final package

# Fix package load to be safer on machines that may not have packages installed
library(twitteR)
library(ROAuth)

# Source keys from separate file ignored by git, don't want to share they in public github repo
source("keys.R")

# Establish OAuth handshake with Twitter API
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

handles <- c("@kevinrose", "@tferriss")

# Function to pull tweets using searchTwitter from the twitteR package
# Outputs tidy data frame with each column being a tweet attribute
# Accepts search query in form of string, n number of tweets
# Does not handle re-tweets of quereryed handle NEEDS TO BE RESOLVED
tweetsIngest <- function(query, n) {
  raw_tweets <- searchTwitter(query, n, resultType = "recent")
  tidy_tweets <- do.call("rbind", lapply(raw_tweets, as.data.frame))
  return(tidy_tweets)
}

twitterMultiQuery <- function(querys, n) {
  for (each in querys) {
    if (!exists("tweets_master")) {
      tweets_master <- tweetsIngest(each, n)
    } else {
      tweets_master <- rbind(tweets_master, tweetsIngest(each, n))
    }
  }
  return(tweets_master)
}

big_list_of_tweets <- twitterMultiQuery(handles, 100)
