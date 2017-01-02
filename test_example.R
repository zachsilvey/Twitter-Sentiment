source("data_ingest_exp.R")
source("tweet_scoring_exp.R")

handles <- c("@verizon", "@at&t", "@t-mobile", "@sprint")

good_text <- scan('positive-words.txt',
                  what = 'character', comment.char = ';')
bad_text <- scan('negative-words.txt',
                 what = 'character', comment.char = ';')

scored <- handles %>%
  twitterMultiQuery(100) %>%
  prepTweets() %>%
  scoreDF(good_text, bad_text)

