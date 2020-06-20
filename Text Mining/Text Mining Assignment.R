#Extracted the tweets from Twitter and performed wordcloud and Sentimental analysis on the tweets.
library("twitteR") # Provides access to Twitter API
library("ROAuth") # Provides authentication via OAuth to server of their choice
library(base64enc) # Provides handling tools to perform base64 encoding
library(httpuv) # Provides support for handling HTTP and WebSocket requests
library(tm) # Helps to perform Text mining operations
library(wordcloud) # Visualizes the text in the form of wordclouds
library(wordcloud2)
library(syuzhet) # Helps in performing sentimental analysis
library(lubridate) # Helps in performing time and date analysis
library(ggplot2) 
library(scales)
library(reshape2) # Transforms the data from wide to long.
library(dplyr)

cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',                
                         accessURL='https://api.twitter.com/oauth/access_token',                 
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Accessing Token Secret of Twitter to perform analysis
setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret

Tweets <- userTimeline('Twitter', n = 1000,includeRts = T) #Using 1000 tweets and which includes retweets too
TweetsDF <- twListToDF(Tweets) #Converting the tweets in the form of list
dim(TweetsDF) # Rows and columns
View(TweetsDF)
setwd("F:\\Datasets") 
write.csv(TweetsDF, "Tweets.csv",row.names = F)
getwd()

Twitter <- read.csv(file.choose())
str(Twitter)

# Build Corpus and DTM/TDM
corpus <- Twitter$text
corpus <- Corpus(VectorSource(corpus)) # All tweets into one corpus(group of tweets/documents)
inspect(corpus[1:20]) # Taking the first 20 tweets and performing analysis

# Performing cleansing on first 20 tweets
corpus <- tm_map(corpus,tolower) # Converting the tweets into lowercase
inspect(corpus[1:20])
corpus <- tm_map(corpus,removePunctuation) #Removing all punctuations
inspect(corpus[1:20])
corpus <- tm_map(corpus,removeNumbers) # Removing all numbers
inspect(corpus[1:20])
corpus_clean<-tm_map(corpus,stripWhitespace) # Stripping off the extra whitespaces
inspect(corpus[1:20])
cleanset<-tm_map(corpus,removeWords, stopwords('english')) # Removing set of stopwords
inspect(cleanset[1:20])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL)) # Removing the URL links
inspect(cleanset[1:20])

# Even after removing stopwords, we need to look at the data if there are any stopwords as mentioned below and remove them
cleanset<-tm_map(cleanset,removeWords, c('twitter','going','know','following','consider','best','far','least','can',
                                         'ok','okay','like','now','keep','perhaps','take','even','isnt','youre',
                                         'shouldnt','used','thats','saying','said'))
cleanset <- tm_map(cleanset, gsub,pattern = 'tweets', replacement = 'tweet') # stemming function
inspect(cleanset[1:20])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:20])

#Term Document Matrix :
# Converting the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset) # A matrix containing cleanset data with terms in rows and documents in columns
tdm
# The matrix contains 2298 terms and 999 documents
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm) # Matrix representation of tdm data
tdm[1:10,1:20] 
# Bar Plot 
w <- rowSums(tdm)  # Helps in finding how many times a particular word has been used.
w <- subset(w, w>= 20) # Pullout words that is been used more than 20 times.
barplot(w, las = 2, col = rainbow(50))
# The word tweet has appeared highest number of times and is considered as the highest frequency. This implies
# that Twitter is more concerned about tweet.
w <- sort(rowSums(tdm), decreasing = TRUE) # Sorting of words in decreasing order.
set.seed(1)

wordcloud(words = names(w), freq = w,max.words = 50,random.order = F, min.freq =  3, colors = brewer.pal(8, 'Dark2'), scale = c(5,0.3),rot.per = 0.6)
w <- data.frame(names(w),w) # Displaying how many times words are repeated
colnames(w) <- c('word','freq')
installed.packages("utf8")
library(utf8)
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 
letterCloud(w,word = "F",frequency(5), size=1)

# Sentiment Analysis for tweets: 
Twitterdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(Twitterdata$text)
class(tweets)

# Obtaining Sentimental scores 
Senti <- get_nrc_sentiment(tweets)
head(Senti)
tweets[4]
get_nrc_sentiment('Irritating') # Negative word
get_nrc_sentiment('Optimist') # Positive word

# Barplot
barplot(colSums(s), las = 2.5, col = rainbow(10), ylab = 'Count',main= 'Sentiment scores for Twitter Tweets')




##Extracted the Customer reviews for Oneplus phone from Amazon and performed wordcloud and Sentimental analysis on the reviews.
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Amazon Reviews #
aurl <- "https://www.amazon.in/gp/product/B07DJHXTLJ?pf_rd_p=649eac15-05ce-45c0-86ac-3e413b8ba3d4&pf_rd_r=K1972DRE0N8QAF5KYQMC/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pagenumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

setwd("F:\\Datasets")
write.table(amazon_reviews,"Oneplus.txt",row.names = F)
Oneplus_Lap <- read.delim('Oneplus.TXT')

str(Oneplus_Lap)
View(Oneplus_Lap)

# Build Corpus and DTM/TDM
library(tm)
corpus <- Oneplus_Lap[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('laptop','can','n','going','know','following','consider','best','far','least','can',
                                         'ok','okay','like','now','keep','perhaps','take','even','isnt','youre',
                                         'shouldnt','used','thats','saying','said','will','t','n','ill',"the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond","will","also","can", "time", 
                                         "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", 
                                         "over", "only", "north", "past", "twin", "while","then",'nnnnnnnnn','cant','go','ever','else','never',))
cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
# the barplot pulls both Computer and Machine as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
#  there are 383 words and 79 documents(Reviews) in this TDM
# Sparsity is 85% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 
w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 30) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
windows()
wordcloud(words = names(w), freq = w,max.words = 250,random.order = F,min.freq =  3, colors = brewer.pal(8, 'Dark2'),scale = c(5,0.3), rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 
letterCloud(w,word = 'Am',frequency(5), size=1)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Importing File 
Amazon_reviews <- read.delim('Oneplus.TXT')
reviews <- as.character(Amazon_reviews[-1,])
class(reviews)

# Obtaining Sentimental scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('Beautiful') # Positive word
get_nrc_sentiment('callous') # Negative word
# Barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10), ylab = 'Count',main= 'Sentiment scores for Amazon Reviews -Oneplus Phone')





##Extracted the User reviews from IMDB on the movie "Baahubali"and performed wordcloud and Sentimental analysis for the reviews.
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt4849438/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
setwd("F:\\Datasets")
write.table(IMDB_reviews,"Baahubali.txt",row.names = F)
Baahubali <- read.delim('Baahubali.txt')
str(Baahubali)
View(Baahubali)

# Build Corpus and DTM/TDM
library(tm)
corpus <- Baahubali[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
# the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 
w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

library(wordcloud)# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
windows()
wordcloud(words = names(w), freq = w, max.words = 250,random.order = F,min.freq =  3,colors = brewer.pal(8, 'Dark2'),scale = c(5,0.3),rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 
letterCloud(w,word = 'A',frequency(5), size=1)

# Sentiment Analysis for Baahubali movie reviews:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
# install.packages("syuzhet")# Read File 
IMDB_reviews <- read.delim('Baahubali.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtaining Sentimental scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('powerful') # A positive word with different emotions
get_nrc_sentiment('trash') #1 Negative word

# Barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10), ylab = 'Count',main= 'Sentiment scores for IMDB Reviews for Baahubali')
