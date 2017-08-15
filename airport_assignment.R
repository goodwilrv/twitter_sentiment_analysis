## Install and load required libaries.
if (!require("pacman")) install.packages("pacman");
library(pacman)
p_load(sentimentr)
library(ROAuth)
library(stringi)
library(twitteR)
library(tm)
library(RCurl)
library(qdap)

library(stringr)
library(xlsxjars)
library(xlsx)
library(tm)
library(dplyr)
library(stringi)


## Credential details from twitter Authentication App.
api_key = "XXXXXXXXXXXXXXXXXXXXXXXXXX"
api_secret = "XXXXXXXXXXXXXXXXXXXXXXX"
access_token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

## Do the  authentication with the above strings. Enter 2 input if it asks for an option.
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


per_airport_tweet <- 3000
total_user_timeline <- 1300



extractAndSaveTweets <- function(SearchStringList, no_tweet,path) {
  ## Search for the tweets which contains searchString. n is maximum number of tweets.
  
  allCompanyTweetDf <- data.frame()
  no_companies <- length(SearchStringList)
  
  for (i in 1:no_companies) {
    cat("\n Extracting tweets for: ",SearchStringList[i])
    x1 <- tolower(SearchStringList[i])
    tweets <- searchTwitter(x1,n= no_tweet, lang = "en")
    
    ## Convert to data frame,
    cat("\n number of tweets extracted: ",length(tweets))
    if(length(length(tweets)) > 0) {
      tweetdf <- twListToDF(tweets)
      aboutCompany <- rep(companiesList[i],nrow(tweetdf))
      
      tweetdf <- cbind(tweetdf,aboutCompany)
      cat("Number of rows in allCompanyTweetDf Before: ",nrow(allCompanyTweetDf))
      allCompanyTweetDf <- rbind(allCompanyTweetDf,tweetdf)
      cat("Number of rows in allCompanyTweetDf After: ",nrow(allCompanyTweetDf))
    } else {
      cat("No tweet avaliable for: ",SearchStringList[i])
    }
    
    Sys.sleep(15)
  }
  
  write.csv(allCompanyTweetDf,path)
  
}

## Create corpus of the tweets and keep ready for text analytics techniques.
tm_preprocess <- function(df,stopWord){
  corpus=Corpus(VectorSource(df))
  corpus=tm_map(corpus,removePunctuation)
  
  # Convert to lower-case
  corpus=tm_map(corpus,tolower)
  
  
  corpus=tm_map(corpus,function(x) removeWords(x,c(stopwords("english"),stopWord)))
  
  # convert corpus to a Plain Text Document
  corpus=tm_map(corpus,removeNumbers)
  
  corpus=tm_map(corpus,PlainTextDocument)
  
  #corpus=tm_map(corpus,stemDocument)
  
  cat("tm_preprocess before returning.")
  return(corpus)
}

## Calculate the sentiments of the tweets.
getSentiment <- function(topicTweetdf) {
  
  
  no_of_tweets <- nrow(topicTweetdf)
  
  raw_tweets <- topicTweetdf$text
  # tweetdf <- topicTweetdf
  
  Topicdf_Sent <- topicTweetdf
  
  # Topicdf_Sent= Topicdf_Sent[(!duplicated(Topicdf_Sent$text)),]
  Topicdf_Sent$text <- gsub("http[[:alnum:][:punct:]]*", "", Topicdf_Sent$text)
  
 
  
  # 
  removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
  # 
  # 
  # topicTweetdf <- tweetdf
  
  Topicdf_Sent$text <- removeURL(Topicdf_Sent$text)
  
  Topicdf_Sent$text = gsub("[^[:alnum:] ]", "",Topicdf_Sent$text)
  
  # library(RSentiment)
  
  score_list <- c()
  # list_mentions <- c()
  no_tweets <- nrow(Topicdf_Sent)
  
  
  for(i in 1:no_tweets) {
    tweet <- Topicdf_Sent$text[i]
    print(tweet)
	
	## Not useing the qdap.polarity() function as advised by author of qdap package.
	
    # list_score <- qdap::polarity(tweet ,polarity.frame = qdapDictionaries::key.pol, constrain = FALSE,
    #                              negators = qdapDictionaries::negation.words,
    #                              amplifiers = qdapDictionaries::amplification.words,
    #                              deamplifiers = qdapDictionaries::deamplification.words,
    #                              question.weight = 0, amplifier.weight = 0.8, n.before = 2,
    #                              n.after = 2, rm.incomplete = FALSE, digits = 3)
    list_score <- sentimentr::sentiment(tweet)
    
    
    score_list <- append(score_list,list_score$sentiment)
  }
  
  
  Topicdf_Sent <- data.frame(Topicdf_Sent,score_list)
  # Topicdf_Sent <- data.frame(Topicdf_Sent,list_mentions)
  
  Topicdf_Sent$Raw_sentiment_Score <- Topicdf_Sent$score_list
  
  Topicdf_Sent$score_list[score_list < 0] <- -1
  Topicdf_Sent$score_list[score_list > 0] <- +1
  Topicdf_Sent$score_list[score_list == 0] <- 0
  
  # length(Topicdf_Sent$list_mentions)
  
  
  
  topicTweetdf <- cbind(topicTweetdf,Topicdf_Sent$score_list,Topicdf_Sent$Raw_sentiment_Score)
  
  
  
  # topicTweetdf$TweetCount <- topicTweetdf$retweetCount + 1
  # topicTweetdf$Effective_sentiment <- topicTweetdf$`Topicdf_Sent$score_list` * topicTweetdf$TweetCount
  # 
  # topicTweetdf$Mentions <- topicTweetdf$TweetCount * topicTweetdf$`Topicdf_Sent$list_mentions`
  
  
  names(topicTweetdf)[12]<-"SCreen_Name"
  names(topicTweetdf)[2]<-"text"
  names(topicTweetdf)[18]<-"company"
  names(topicTweetdf)[20]<-"Sentiment(Raw Score)"
  names(topicTweetdf)[19]<-"Sentiment(Pos/Neg/Neut.)"
  
  names(topicTweetdf)[6]<-"Date_Created"
  
  library(xlsx)
  
  write.xlsx(topicTweetdf[,c(12,2,18,19,6,20)],file=final_excel_path,sheetName = "Sentiment_sheet")
  
}


Working_Directory = "C:\\IIM Cal\\shiny_start\\Twitter_Product\\"
setwd(Working_Directory)

tweets_fileName = paste("airport","tweets",sep = "_")
csv_path = paste(Working_Directory,tweets_fileName)
time_stamp <- paste(format(Sys.time(), "%Y-%m-%d %H-%M"), "csv", sep = ".")
Sentiment_csv_path <- paste(csv_path,time_stamp,sep = "")

tweets_fileName = "airport_Sentiment"
excel_path = paste(Working_Directory,tweets_fileName)
time_stamp <- paste(format(Sys.time(), "%Y-%m-%d %H-%M"), "xlsx", sep = ".")
final_excel_path <- paste(excel_path,time_stamp,sep = "")


# searchStringList <- c("fly2ohare+wifi|chicagoairport+wifi|ohare+wifi|chicago airport+wifi","csiamumbai+wifi|mumbaiairport+wifi|mumbai_airport+wifi|csia+wifi|gvk+wifi|mumbai airport+wifi","blrairport+wifi|bangalore airport+wifi|bial+wifi","hyderabadairport+wifi|rgiahyd+wifi|rgia+wifi|hyderabad airport+wifi")

companiesList = c("New Delhi","Chicago","London","Dubai","Sydney")
searchStringList <- c("delhi+airport+wifi|gmr+wifi|delhi_airport+wifi","fly2ohare|chicago+airport+wifi","heathrow+wifi|londonairport+wifi|london+airport+wifi","DubaiAirports|dubai+airport+wifi","SydneyAirport|sydney+airport+wifi")

# searchStringList <- c("fly2ohare|chicagoairport|ohare|chicago airport","heathrow|londonairport|london airport","DubaiAirports|dubai airport","SydneyAirport|sydney airport")

extractAndSaveTweets(searchStringList,per_airport_tweet,Sentiment_csv_path)



getSentiment(read.csv(Sentiment_csv_path,header = TRUE))


