

getTweetText <- function(key){
  api_key <- 'U4lbZK3ZHMgVXq91RklD1FoJA'
  api_secret <- '4Nz4kKaE2dNarz5QDW6GxbyWkN20hzrDys1SoBA8lc2WOV9mQl'
  access_token <- '378123503-ST03hW8LIvAVheX8FUn4IGi2HNL4K9IrycnvOPk1'
  access_token_secret <- 'fqJKqeilWaw2EhARm5CLrka22mC1XQnfJ8xPPZDiB1SGm'

  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

  tweets <- searchTwitter(key, n = 100)
  tweets_text <-  unlist(lapply(tweets, function(x){ x$getText() }))

  # clean up sentences with R's regex-driven global substitute, gsub():
  tweets_text <- gsub('[[:punct:]]', '', tweets_text)
  tweets_text <- gsub('[[:cntrl:]]', '', tweets_text)
  tweets_text <- gsub('\\d+', '', tweets_text)
  # removing garbage
  tweets_text <- iconv(tweets_text, "UTF-8", "ASCII", sub = "")
  # and convert to lower case:
  tweets_text <- tolower(tweets_text)
  return(tweets_text)
}

getSentiment <- function(text){
  return(get_nrc_sentiment(text))
}

renderChart <- function(sent_df, key){
  sentimentTotals <- data.frame(colSums(sent_df))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  plot <- ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
          geom_bar(aes(fill = sentiment), stat = "identity") +
          theme(legend.position = "none") +
          xlab("Sentiment") + ylab("Total Count") + ggtitle(paste("Total Sentiment Score for Last 100 Tweets:", key))
  return(plot)
}
#

#renderChart(getSentiment(getTweetText("India")))
#pos <- scan('/home/sampurn/lords/wordbanks/positive-words.txt', what='character', comment.char=';')#

#neg <- scan('/home/sampurn/lords/wordbanks/negative-words.txt', what='character', comment.char=';')
#score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
#{
#  require(plyr)
#  require(stringr)
#  
#  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
#  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
#  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
#    
#    # clean up sentences with R's regex-driven global substitute, gsub():
#    sentence <- gsub('[[:punct:]]', '', sentence)
#    sentence <- gsub('[[:cntrl:]]', '', sentence)
#    sentence <- gsub('\\d+', '', sentence)
#    # and convert to lower case:
#    sentence <- tolower(sentence)
#    
#    # split into words. str_split is in the stringr package
#    word.list <- str_split(sentence, '\\s+')
#    # sometimes a list() is one level of hierarchy too much
#    words <- unlist(word.list)
#    
#    # compare our words to the dictionaries of positive & negative terms
#    
#    neg.matches <- match(words, neg.words)
#    pos.matches <- match(words, pos.words)
#    
#    # match() returns the position of the matched term or NA
#    # we just want a TRUE/FALSE:
#    pos.matches <- !is.na(pos.matches)
#    neg.matches <- !is.na(neg.matches)
#    
#    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#    score <- sum(pos.matches) - sum(neg.matches)
#    
#    return(score)
#  }, pos.words, neg.words, .progress=.progress )
#  
#  scores.df <- data.frame(score=scores, text=sentences)
#  return(scores.df)
#}#

#analysis <- score.sentiment(tweets_text, pos, neg)
