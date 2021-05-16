

library(twitteR)
library(RCurl)

require(twitteR)
require(RCurl)
install.packages("ROAuth")
library("ROAuth")
library(tm)
install.packages("NLP")
library(NLP)
library(syuzhet)
install.packages("SnowballC")
library("SnowballC")
install.packages("stringi")
library(stringi)
install.packages("topicmodels")
library(topicmodels)

consumer_key<-'OCffMUjbE3wLJzDTRahvwvcuU' 
consumer_secret<-'Dzwp08V7vT5yDKwgytYvTcAuTmyICvn24DxuGZqdoAm24BAa2j'
access_token<-'966262247029747712-1DtOlEobre32tzKEpsM306KqROBdwyZ'
access_secret<-'VE8qCuGaEirf5ha1oRdDhDybfDdJy5Vd6NyBSbmzbWX81'

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

ElonMusk_tweet<-searchTwitter("Elon Musk",n=100,lang = 'en')
ElonMusk_tweet



str(ElonMusk_tweet)

library(tm)

class(ElonMusk_tweet)
library(wordcloud)

ElonMusk_text<-sapply(ElonMusk_tweet, function(x) x$getText())


ElonMusk_corpus<-Corpus(VectorSource(ElonMusk_text))

ElonMusk_corpus

inspect(ElonMusk_corpus[1])


# Data Cleansing
x1 <- tm_map(ElonMusk_corpus, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])
x1 <- tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords, c(stopwords('english'),'Musk','Elon','ElonMusk'))
inspect(x1)

#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1)
# Term document matrix
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
dtm <- t(tdm)
tdm <- as.matrix(tdm)

inspect(dtm[1:7,1:5])

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 10)
w_sub
windows()
barplot(w_sub, las=3, col = rainbow(20))

x1 <- tm_map(x1, removeWords, c("Musk","Elon","ElonMusk"))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 10)
w_sub

library(wordcloud)
windows()
wordcloud(words = names(w_sub), freq = w_sub,colours=rainbow(10)) 
#word cloud with only subset of words

class(w_sub)
w_sub2 <- data.frame(w_sub)

w_sub2$words <- rownames(w_sub2)
w_sub2

#Column headers
colnames(w_sub2)<- c("freq","word")
w_sub2

###Sorting
w_sub2 <- w_sub2[order(-w_sub2$freq),]

# Column order is important for word cloud2
w_sub2<-w_sub2[, c("word", "freq")]

stopwords = c("Elon", "Musk", "ElonMusk")

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 10)
w_sub

install.packages("wordcloud2")
library(wordcloud2)
# The results!
# wordcloud2(TDMasDF, size = 0.4,  figPath="./liberty.png", backgroundColor = 'black', fontFamily="Loma")
# The one above uses a custom shape but it does not keep the x/y proportions
wordcloud2(w_sub2, size = 0.5,  shape="star", backgroundColor = 'black', fontFamily="Loma")

wordcloud2(w_sub2, size = 0.5,  shape="triangle", backgroundColor = 'black', fontFamily="Loma")

#####Sentiment analysis############

library(tm)
library("syuzhet")

library(lubridate)

library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

ElonMusk_txt <- as.character(ElonMusk_tweet)

x <- get_nrc_sentiment(ElonMusk_txt)

head(x)
View(Elontweet_txt)


ElonMusk_tweet<-searchTwitter("ElonMusk",n=100,lang = 'en')
ElonMusk_tweet

ElonMusk_text<-sapply(ElonMusk_tweet, function(x) x$getText())

ElonMusk_corpus<-Corpus(VectorSource(ElonMusk_text))

# Data Cleansing
x1 <- tm_map(ElonMusk_corpus, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])
x1 <- tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords, c(stopwords('english'),'Musk','Elon','ElonMusk'))
inspect(x1)

#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1)


setwd(file.choose())
read.csv(file.choose())
read.csv(file.choose())
opinion.lexicon.pos <- scan("positive.txt",what='character',comment.char = ';')
opinion.lexicon.neg <- scan("negative.txt",what='character',comment.char = ';')

library(stringr)
jj<-str_split(x1,pattern="\\s+")

lapply(jj,function(x) (sum(!is.na(match(x,opinion.lexicon.pos)))))

lapply(jj,function(x) (sum(!is.na(match(x,opinion.lexicon.neg)))))

##Sentiment score
score <-unlist(lapply(jj,function(x) (sum(!is.na(match(x,opinion.lexicon.pos)))-sum(!is.na(match(x,opinion.lexicon.neg))))))
score
mean(score)
sd(score)

##########################################################################
str(x1)

TweetsDF <- twListToDF(ElonMusk_tweet)
dim(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

EMtweet <- read.csv(file.choose(),header = T)

tweets <-iconv(Ntweet$text,"ASCII","UTF-8",sub="")

View(tweets)               
getwd()
s<-get_nrc_sentiment(tweets)
s
tweets[97]
get_nrc_sentiment('noble')
get_nrc_sentiment('fair')
get_nrc_sentiment('true')
get_nrc_sentiment('speak')

####Barplot

barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab='count',
        main='Sentiment score for Elon Musk tweets'
        )


##https://www.youtube.com/watch?v=otoXeVPhT7Q
###http://rstudio-pubs-static.s3.amazonaws.com/283881_efbb666d653a4eb3b0c5e5672e3446c6.html


