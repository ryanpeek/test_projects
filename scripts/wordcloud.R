##Install Packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
##Load Require Library
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(lubridate)
library(readr)
##Read the Data
tweetsDS<-read_csv("data/R-DAVIS-Setup_20180107.csv") %>% 
  rename(datetime=Timestamp, 
         content=`What you'd like to learn most from this course?`) %>% 
  select(datetime, content)

# format time
tweetsDS$datetime <- mdy_hms(tweetsDS$datetime)

# filter
tweetsDS <- tweetsDS %>% filter(year(datetime)>2017)

tweetsDS<-data.frame(tweetsDS)

## Calculate Corpus
tweetsDS.Corpus<-Corpus(VectorSource(tweetsDS$content))

##Data Cleaning and Wrangling
tweetsDS.Clean<-tm_map(tweetsDS.Corpus, PlainTextDocument
tweetsDS.Clean<-tm_map(tweetsDS.Corpus,tolower)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeNumbers)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeWords,stopwords("english"))
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removePunctuation)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stripWhitespace)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stemDocument)

wordcloud(words = tweetsDS.Clean, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
