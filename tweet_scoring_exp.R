library(stringr)

prepTweets <- function(df) {
  cleanTweets <- df$text
  cleanTweets <- gsub('[[:punct:]]', '', cleanTweets)
  cleanTweets <- gsub('[[:cntrl:]]', '', cleanTweets)
  cleanTweets <- gsub('\\d+', '', cleanTweets)
  cleanTweets <- iconv(enc2utf8(cleanTweets), sub = "byte")
  cleanTweets <- tolower(cleanTweets)
  df <- cbind(df, cleanText = cleanTweets)
}

# The intention of this function is to take a single charcter vector as input and then output a numeric value
# The function could then be used to score a vector of tweets using an apply function
scoreTweet <- function(x, pos_lex, neg_lex) {
  # score positive words
  # score negative words
  # sum score
  # return score, if both pos and neg are 0 return NA
  word.list = str_split(x, '\\s+')
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  pos.matches = sum(!is.na(match(words, pos_lex)))
  neg.matches = sum(!is.na(match(words, neg_lex)))
  if (pos.matches == 0 & neg.matches == 0) {
    return(NA)
  } else {
    return(pos.matches - neg.matches)
  }
}

scoreDF <- function(df, pos_lex, neg_lex) {
  scores <- lapply(df$cleanText, scoreTweet, pos_lex = pos_lex, neg_lex = neg_lex)
  return(cbind(df, scores = unlist(scores)))
}
