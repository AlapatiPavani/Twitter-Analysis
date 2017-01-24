###############

setwd("~/Downloads")  # sets the path
tweets.df <- read.csv("demonetization-tweets.csv")  # reads the file into tweets.df
tweets.df = head(tweets.df, n =8000)

tweets.df[,2] = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df[,2])  # removing RT and @
tweets.df[,2]
tweets.df[,2] = gsub("^\n\\w+", "", tweets.df[,2])  # removing comma
tweets.df[,2] = gsub("[[:punct:]]", "", tweets.df[,2])  # removing punctuation
tweets.df[,2] = gsub("[[:digit:]]", "", tweets.df[,2])  # removing digits
tweets.df[,2] = gsub("http\\w+", "", tweets.df[,2])  # removing links
tweets.df[,2] = gsub("[ \t]{2,}", "", tweets.df[,2])  # removing tabs
tweets.df[,2]= gsub("^\\s+|\\s+$", "", tweets.df[,2])  # removing $
tweets.df[,2] = gsub("amp", "", tweets.df[,2])  # removing abbrevations
tweets.df[,2] = gsub("[\r\n]", "", tweets.df[,2])  # removing new line character


positive = readLines("positivewords.txt")  # takes all positive words
negative = readLines("negativewords.txt")  # takes all negative words
negative
positive
res = c()
#calculate the ranking for the tweet
for(text in tweets.df[,2]) {
  words = str_split(text,'\\s+')
  words = unlist(words)
  pos.matches = match(words, positive)
  neg.matches = match(words, negative)
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  score = sum(pos.matches) - sum(neg.matches)
  print(score)
  res = c(res, score)
}
res
tweets.df$scores = res  # creates a column in tweets.df
names(tweets.df)
write.csv(tweets.df, "demonetization-tweets.csv")  # writes to the csv file
boxplot(tweets.df$scores,col="sky blue",cex.lab=1.2, cex.axis=.8, col.lab="red",ylab="Scores",main="Analysis Of Tweets",col.main="purple")  # boxplot for scores

#######################

setwd("~/Downloads")  # setting the path
tweets.df <- read.csv("demonetization-tweets.csv")  # reading csv file into tweets.df
read.csv("demonetization-tweets.csv")
tweets.df = head(tweets.df, n =8000)
tweets.df

tweets.df[,2] = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df[,2])  # removing RT and @
tweets.df[,2]
tweets.df[,2] = gsub("^\n\\w+", "", tweets.df[,2])  # removing comma
tweets.df[,2] = gsub("[[:punct:]]", "", tweets.df[,2])  # removing punctuation
tweets.df[,2] = gsub("[[:digit:]]", "", tweets.df[,2])  # removing digits
tweets.df[,2] = gsub("http\\w+", "", tweets.df[,2])  # removing links
tweets.df[,2] = gsub("[ \t]{2,}", "", tweets.df[,2])  # removing tabs
tweets.df[,2]= gsub("^\\s+|\\s+$", "", tweets.df[,2])  # removing $
tweets.df[,2] = gsub("amp", "", tweets.df[,2])  # removing abbrevations
tweets.df[,2] = gsub("[\r\n]", "", tweets.df[,2])  # removing new line character

require(tm)  # adding tm package
require(wordcloud)  # adding wordcloud package
tweets = tweets.df[,2]  # taking tweets into tweets variable
tweets
str(tweets)  # gives the stucture of tweets
tweet_corpus = Corpus(VectorSource(tweets))  # it stores the data into documents
tweet_corpus
inspect(tweet_corpus)  # displays documents
tweet_clean = tm_map(tweet_corpus, removePunctuation)  # removes punctuation
tweet_clean = tm_map(tweet_clean, content_transformer(tolower))  # makes all letters to lowercase letters
tweet_clean = tm_map(tweet_clean, removeWords, stopwords("english"))  # removes the words like and, the
tweet_clean = tm_map(tweet_clean, removeNumbers)  # removes numbers
tweet_clean = tm_map(tweet_clean, stripWhitespace)  # removes white spaces
tweet_clean = tm_map(tweet_clean, removeWords, c("will"))  # removes will
wordcloud(tweet_clean,random.order = F,max.words = 100, min.freq = 100, scale = c(3,1), colors = rainbow(50), use.r.layout = TRUE, random.color = TRUE)  # generates wordcloud

######################

setwd("~/Downloads")#setting the path
tweets.df1 <- read.csv("demonetization-tweets.csv")#reading the file
tweets = head(tweets.df1, n = 5214)#considering the file tweets.df upto 5214
tweets#getting all the columns upto 5214
#sample(1:6, 10, replace = TRUE)
dt_time= tweets$created#considering created from tweets
dt_time#getting the column created
crdate = c()#creating a vector
#stores the time in crdate
for(i in dt_time) {#creating a for loop
  len = nchar(i) #to know the length of i
  date1 = substr(i,12, 13);
  print(date1)
  crdate = c(crdate,date1);
  #print(i)
}
crdate#time in this crdate
crdate = as.data.frame(table(crdate))#creating a table
crdate#time in table
library(ggplot2)#checks the library
qplot(data = crdate,crdate,Freq,geom="point",cex.lab=3,cex.axis=5, col = "red", main = "Mostly tweeted time",xlab = "Hours",ylab = "No.of.tweets",size=7)#plotting q plot
#ggplot(crdate, aes(x = crdate$crdate,y= crdate$Freq))
#barplot(as.matrix(crdate),xlab=crdate$crdate,main = "most tweets",ylab = "Freq")
#hist(data = crdate,crdate,Freq)

########################

tweets.df <- read.csv("demonetization-tweets.csv")#reading the file
setwd("~/Downloads")#setting the path
read.csv("demonetization-tweets.csv")#reading the file
tweets.df = head(tweets.df, n =8000)#considering the file tweets.df upto 8000
tweets = tweets.df$text#taking text from tweets.df

tweets = head(tweets, n = 8000)#considering tweets i,e text upto 8000
tweets.df#getting the file upto 8000
tweets#getting the text data
fav = tweets.df$favoriteCount#considering favourite count from file
time.creation = tweets.df$created#consider created from file
name = tweets.df$screenName#consider screen name from file
tweet = tweets.df$sno#consider sno from file
popular = tweets.df$retweetCount#consider retweet count from file
library(ggplot2)#checking the ggplot2
#boxplot(tweet, popular, col = "red")
hist(tweets.df$retweetCount,col="forestgreen",col.bars="darkblue", col.border="lightsteelblue4", col.bg="ivory",
     col.grid="darkgray", xlab="RetweetCount",density=25, angle=-45, cex.lab=1.2, cex.axis=.8, col.lab="sienna3",main="Histogram For Retweets", col.main="blue")#histogram for retweets

####################

setwd("~/Downloads")  # sets the path
tweets.df <- read.csv("demonetization-tweets.csv")  # reads the file into tweets.df
tweets.df = head(tweets.df, n =8000)
tweets = tweets.df$text  # takes all the tweets into tweets
source_tweet = tweets.df$statusSource  # stores the source used for twitter
source_tweet
i = 1
len = 3
src = c()
library(stringr)  # checks whether stringr library present or not
#stores the source data into src
for(s in source_tweet){
  source_tweet1 = word(s, -1);
  len = nchar(source_tweet1)-4;
  source_tweet1 = substr(source_tweet1,1,len)
  if(substr(source_tweet1,1,1) == 'r') {
    source_tweet1 = substr(source_tweet1, 16, len)
  }
  if(!(identical(source_tweet1,"Android")||identical(source_tweet1, "iPhone") || identical(source_tweet1, "Client") || identical(source_tweet1, "Facebook") || identical(source_tweet1, "(M5)") || identical(source_tweet1, "GrabInbox") || identical(source_tweet1, "IFTTT") || identical(source_tweet1, "iPad") || identical(source_tweet1, "Phone") || identical(source_tweet1, "TweetDeck") || identical(source_tweet1, "Windows"))){
    
    source_tweet1 = "Other"
    src = c(src, source_tweet1);
  }
  else{
    src = c(src,source_tweet1);
  }
}
src  # displays src
library(plyr)  # checks for plyr library
count = count(data.frame(src))  # counts the frequency
count  # displays the count
cols = c("skyblue", "violet", "green","yellow","pink","blue", "red", "grey","orange", "black", "purple", "brown")  # stores the colors into col
pie(count$freq, count$src, main = "Pie chart of sources used for TWITTER",col.main="purple" ,col = cols, cex = 0.03)  # pie chart for sources
legend("topright", c("Android","WebClient","Facebook","GrabInbox","IFTTT","ipad","iPhone","M5","Other","WindowsPhone","TweetDeck","Windows"), fill = cols)

#####################