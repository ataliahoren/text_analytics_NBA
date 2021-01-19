#' Title: NBA Case
#' Purpose: pre and post covid - how tweets' content has changed and what Nike can take out of it?
#' NAME: Atalia Horenshtien
#' Date: Jan 20 2021
#' 

# Pre processing: 

## 1. setup

### working directory
setwd("~/Documents/MsBA/textAnalytics/Cases/text_analytics_NBA/data")

### Load the needed libraries
library(tm)
library(qdap)
library(plyr)
library(pbapply)
library(stringr)
library(stringi)

### Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

remove_UTF8_dots <- function(x){
  y <- iconv(x, "UTF-8", "ASCII", ".")
  y <- gsub("[[:punct:]]+", ' ', y)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(remove_UTF8_dots))
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

## 2. read and filter the data

### read the data 

#### join - Nov - Tweets pre covid-19, when no one thought about a potential pandemic, Aug after the pandemic
pre_covid_data <- read.csv('B_Nov2019.csv', header=TRUE) #October 22nd was the opening
post_covid_data = read.csv('K_Aug2020.csv', header=TRUE) #July 30 – August 14, 2020. (completion of regular season with "seeding" games)

#### count number of obs
befor_filter_pre_covid_data = nrow(pre_covid_data)
befor_filter_post_covid_data = nrow(post_covid_data)

### filter the data 

#### the top 3 NBA teams I will focus on, based on social media popolarity
top3_NBA_teams <- c('Los Angeles Lakers', 'Golden State Warriors', 'Chicago Bulls')

#### Create custom stop words
stops <- c(stopwords('SMART'), 'nba', tryTolower(top3_NBA_teams), unlist(strsplit(tryTolower(top3_NBA_teams), " "), use.names=FALSE), 
           'basketball', 'amp', 'game', 'games', 'team', 'sports', 'team', 'teams', 'play', 'playing', 'player', 'points', 'fans', 'move',
           'time', 'years', 'year', 'thinking', 'the', 'season')

#### filter the data for only the top top10_NBA_teams
pre_covid_data <- subset(pre_covid_data, team %in% top3_NBA_teams)
post_covid_data <- subset(post_covid_data, team %in% top3_NBA_teams)

#### ~8% of the posts are with the top 10 MbA team before and after covid.
nrow(pre_covid_data)/befor_filter_pre_covid_data
nrow(post_covid_data)/befor_filter_post_covid_data

#### choose sample Tweets to reduce number of obs
set.seed(1234)
pre_covid_data = pre_covid_data[sample(nrow(pre_covid_data), 5000), ]
post_covid_data = post_covid_data[sample(nrow(post_covid_data), 5000), ]

#### Vector Corpus; omit the meta data
pre_covid_data <- VCorpus(VectorSource(pre_covid_data$text))
post_covid_data <- VCorpus(VectorSource(post_covid_data$text))

## 3. Clean up the data

pre_covid_data <- cleanCorpus(pre_covid_data, stops)
post_covid_data <- cleanCorpus(post_covid_data, stops)

## 4. reorganized the data 

### Extract the cleaned text 
pre_covid_data <- unlist(pblapply(pre_covid_data, content))
post_covid_data <- unlist(pblapply(post_covid_data, content))

### Collapse each into a single "subject" ie a single document
pre_covid_data <- paste(pre_covid_data, collapse = ' ')
post_covid_data <- paste(post_covid_data, collapse = ' ')

### Combine the subject documents into a corpus of *2* documents
allTweets <- c(pre_covid_data, post_covid_data)
allTweets <- VCorpus((VectorSource(allTweets)))

pre_covid <- VCorpus((VectorSource(pre_covid_data)))
post_covid <- VCorpus((VectorSource(post_covid_data)))

### structure the data
tweetTDM  <- TermDocumentMatrix(allTweets)
tweetTDMm <- as.matrix(tweetTDM)

pre_covidTDM  <- TermDocumentMatrix(pre_covid)
post_covidTDM  <- TermDocumentMatrix(post_covid)

pre_covidTDMm <- as.matrix(pre_covidTDM)
post_covidTDMm <- as.matrix(post_covidTDM)

### Make sure order is the same as the c(objA, objB) 
colnames(tweetTDMm) <- c('pre_covid', 'post_covid')

# Visualization:

## Load the needed libraries
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

## Visualization 1. barplot of the most frequent words - all tweets

### frequency analysis
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

###  Remove the row attributes meta family
rownames(tweetFreq) <- NULL

### top frequent word
### sunset for a smaller data to plot based on condition
topWords <- subset(tweetFreq, tweetFreq$frequency >= 200) 
topWords  <- topWords[order(topWords$frequency, decreasing= T),]
head(topWords, 15)

###change frequency to include only the top 15 wwords
topWords <- subset(tweetFreq, tweetFreq$frequency >= 268) 
topWords  <- topWords[order(topWords$frequency, decreasing= F),]

###  Chg to factor for ggplot
topWords$word <- factor(topWords$word, levels=unique(as.character(topWords$word))) 

### visualization - Simple barplot
ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#FF4500') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Visualization 2. barplot of the most frequent words - pre_covid tweets

### frequency analysis
tweetSums <- rowSums(pre_covidTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

###  Remove the row attributes meta family
rownames(tweetFreq) <- NULL

### top frequent word
### sunset for a smaller data to plot based on condition
topWords <- subset(tweetFreq, tweetFreq$frequency >= 100) 
topWords  <- topWords[order(topWords$frequency, decreasing= T),]
head(topWords, 15)

###change frequency to include only the top 15 wwords
topWords <- subset(tweetFreq, tweetFreq$frequency >= 141) 
topWords  <- topWords[order(topWords$frequency, decreasing= F),]

###  Chg to factor for ggplot
topWords$word <- factor(topWords$word, levels=unique(as.character(topWords$word))) 

### visualization - Simple barplot
ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#008000') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Visualization 3. barplot of the most frequent words - post_covid tweets

### frequency analysis
tweetSums <- rowSums(post_covidTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

###  Remove the row attributes meta family
rownames(tweetFreq) <- NULL

### top frequent word
### sunset for a smaller data to plot based on condition
topWords <- subset(tweetFreq, tweetFreq$frequency >= 100) 
topWords  <- topWords[order(topWords$frequency, decreasing= T),]
head(topWords, 15)

###change frequency to include only the top 15 wwords
topWords <- subset(tweetFreq, tweetFreq$frequency >= 218) 
topWords  <- topWords[order(topWords$frequency, decreasing= F),]

###  Chg to factor for ggplot
topWords$word <- factor(topWords$word, levels=unique(as.character(topWords$word))) 

### visualization - Simple barplot
ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#8B0000') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Visualization 4. common cloud - what is mutual pre and post covid?

### Make a common cloud
commonality.cloud(tweetTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(3.5,0.25))

## Visualization 5. comparison cloud - what has changed pre and post covid?

### Make a comparison cloud
comparison.cloud(tweetTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(tweetTDMm),"Accent"),
                 scale=c(3,0.1))

#end