library(rtweet)
library(httpuv)
library(tidyverse)
library(tm)
library(syuzhet)
library(RColorBrewer)
library(ggpubr)
library(SentimentAnalysis)
library(SnowballC)
library(textreg)
library(dplyr)
library(plotly)
library(httr)
library(jsonlite)
library(devtools)
library(academictwitteR)
library(geojsonR)
library(rtweet)
library(stringr)
library(lubridate)
library(tidyverse)
library(tseries)
library(quantmod)
library(timeSeries)
library(forecast)
library(xts)
library(caTools)
library(rpart)
library(caret)
library(ROCR)
library(randomForest)
library(e1071)

### Tweets from 2015 -2021
appleTwitterdata15 = read_csv("aapl.data_tweets_1516.csv", col_types = cols())
appleTwitterdata16 = read_csv("aapl.data_tweets_1617.csv", col_types = cols())
appleTwitterdata17 = read_csv("aapl.data_tweets_1718.csv", col_types = cols())
appleTwitterdata18 = read_csv("aapl.data_tweets_1819.csv", col_types = cols())
appleTwitterdata19 = read_csv("aapl.data_tweets_1920.csv", col_types = cols())
appleTwitterdata20 = read_csv("aapl.data_tweets_2021.csv", col_types = cols())

appleTwitterdata_combined = rbind(appleTwitterdata15, appleTwitterdata16, appleTwitterdata17,
                                  appleTwitterdata18, appleTwitterdata19, appleTwitterdata20)

googTwitterdata15 = read_csv("goog.data_tweets_1516.csv", col_types = cols())
googTwitterdata16 = read_csv("goog.data_tweets_1617.csv", col_types = cols())
googTwitterdata17 = read_csv("goog.data_tweets_1718.csv", col_types = cols())
googTwitterdata18 = read_csv("goog.data_tweets_1819.csv", col_types = cols())
googTwitterdata19 = read_csv("goog.data_tweets_1920.csv", col_types = cols())
googTwitterdata20 = read_csv("goog.data_tweets_2021.csv", col_types = cols())

googTwitterdata_combined = rbind(googTwitterdata15, googTwitterdata16, googTwitterdata17,
                                 googTwitterdata18, googTwitterdata19, googTwitterdata20)


msftTwitterdata15 = read_csv("msft.data_tweets_1516.csv", col_types = cols())
msftTwitterdata16 = read_csv("msft.data_tweets_1617.csv", col_types = cols())
msftTwitterdata17 = read_csv("msft.data_tweets_1718.csv", col_types = cols())
msftTwitterdata18 = read_csv("msft.data_tweets_1819.csv", col_types = cols())
msftTwitterdata19 = read_csv("msft.data_tweets_1920.csv", col_types = cols())
msftTwitterdata20 = read_csv("msft.data_tweets_2021.csv", col_types = cols())

msftTwitterdata_combined = rbind(msftTwitterdata15, msftTwitterdata16, msftTwitterdata17,
                                 msftTwitterdata18, msftTwitterdata19, msftTwitterdata20)

### clean the twitter data because it will have words which have no sentimental value
cleanData = function(x){
  text = VectorSource(x)
  corpusText = VCorpus(text)
  
  corpusText = tm_map(corpusText, content_transformer(tolower))
  corpusText = tm_map(corpusText, removePunctuation)
  corpusText = tm_map(corpusText, removeWords, stopwords("english"))
  corpusText <- tm_map(corpusText, removeNumbers)
  
  removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
  corpusText <- tm_map(corpusText, content_transformer(removeURL))
  
  removeCntrl <- function(x) gsub('[[:cntrl:]]', '', x)
  corpusText <- tm_map(corpusText, content_transformer(removeCntrl))
  
  
  corpusText = tm_map(corpusText, stripWhitespace)
  corpusText = tm_map(corpusText, stripWhitespace)
  return(corpusText)
}

#### change the date of the data  for easy summarization of data
correctDate = function(x){
  date = x
  date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
  date = as.Date(date)
  date = as.Date(date, format = "%m/%d/%y")
  return(date)
}

## organise data according to sentimental overal score using the syuzhet package
organiseData = function(x){
  organised.data = x %>%
    group_by(date) %>%
    summarise_at(vars(senti.over_scores), sum)
  return(organised.data)
}

cleanedappletext = convert.tm.to.character(cleanData(appleTwitterdata_combined$text))
cleanedgoogtext = convert.tm.to.character(cleanData(googTwitterdata_combined$text))
cleanedmsfttext = convert.tm.to.character(cleanData(msftTwitterdata_combined$text))

### obtain sentiment score with 8 categories of emotions for 2015-2021 data
apple.sent_score = get_nrc_sentiment(cleanedappletext)
goog.sent_score = get_nrc_sentiment(cleanedgoogtext)
msft.sent_score = get_nrc_sentiment(cleanedmsfttext)

### obtain overall sentiment score for each sentences for 2015-2021 data
appleTweet.sent_vals <- get_sentiment(cleanedappletext)
googTweet.sent_vals <- get_sentiment(cleanedgoogtext)
msftTweet.sent_vals <- get_sentiment(cleanedmsfttext)

### extract only the postive and nevative sentiments for 2015-2021 data
applneg_pos.sent = apple.sent_score[,c("negative", "positive")]
googneg_pos.sent = goog.sent_score[,c("negative", "positive")]
msftneg_pos.sent = msft.sent_score[,c("negative", "positive")]

### add dates to the sentiment scores
applneg_pos.revsent = cbind(date = correctDate(appleTwitterdata_combined$created_at), applneg_pos.sent)
googneg_pos.revsent = cbind(date = correctDate(googTwitterdata_combined$created_at), googneg_pos.sent)
msftneg_pos.revsent = cbind(date = correctDate(msftTwitterdata_combined$created_at), msftneg_pos.sent)

### obtain overall negative and positive sentiment score for each month
applrev_neg_pos.revsent = applneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%01/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)

googrev_neg_pos.revsent = googneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%01/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)

msftrev_neg_pos.revsent = msftneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%01/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)



tweetdataApple = tidyr::pivot_longer(applrev_neg_pos.revsent, cols=c('negative', 'positive'), names_to='sentiments', 
                                     values_to="values")

tweetdataGoog = tidyr::pivot_longer(googrev_neg_pos.revsent, cols=c('negative', 'positive'), names_to='sentiments', 
                                    values_to="values")

tweetdataMsft = tidyr::pivot_longer(msftrev_neg_pos.revsent, cols=c('negative', 'positive'), names_to='sentiments', 
                                    values_to="values")

### calculate bar plots for the negative and postive sentiments for each company for each month

draw_graphs = function(tweetdata, company){
  barplot=  ggplot(tweetdata, aes(x=date, y=values, fill=sentiments)) +
    ggtitle(paste0("Sentiment Scores for ", company, " from 2020-2021")) +
    scale_x_date(date_breaks = "months" , date_labels = "%Y (%b)") +
    xlab("Dates") + ylab("Sentiment Scores") +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_text(angle=45), plot.title = element_text(hjust = 0.5))
  return(barplot)
}

output.graph_aapl = draw_graphs(tweetdataApple, "Apple")
output.graph_goog = draw_graphs(tweetdataGoog, "Google")
output.graph_msft = draw_graphs(tweetdataMsft, "Microsoft")

output.graph_aapl
output.graph_goog
output.graph_msft

library(quantmod)

### get stock data using the yahoo finance API
apple.stockdata = getSymbols('AAPL', from = "2015-01-01", to = "2021-01-01", 
                             auto.assign = FALSE)

goog.stockdata = getSymbols('GOOG', from = "2015-01-01", to = "2021-01-01", 
                            auto.assign = FALSE)

msft.stockdata = getSymbols('MSFT', from = "2015-01-01", to = "2021-01-01", 
                            auto.assign = FALSE)

applStock_data = tibble(date = index(apple.stockdata), open = as.numeric(apple.stockdata$AAPL.Open),
                        close = as.numeric(apple.stockdata$AAPL.Close), volume = as.numeric(apple.stockdata$AAPL.Volume))

googStock_data = tibble(date = index(goog.stockdata), open = as.numeric(goog.stockdata$GOOG.Open),
                        close = as.numeric(goog.stockdata$GOOG.Close), volume = as.numeric(goog.stockdata$GOOG.Volume))

msftStock_data = tibble(date = index(msft.stockdata), open = as.numeric(msft.stockdata$MSFT.Open),
                        close = as.numeric(msft.stockdata$MSFT.Close), volume = as.numeric(msft.stockdata$MSFT.Volume))

applStock_data$date = as.Date(dmy(format(applStock_data$date, format="%d/%m/%Y")))
googStock_data$date = as.Date(dmy(format(googStock_data$date, format="%d/%m/%Y")))
msftStock_data$date = as.Date(dmy(format(msftStock_data$date, format="%d/%m/%Y")))

#### assign data to the twitter overall sentiment scores
appleTweet.over_score = tibble(date = correctDate(appleTwitterdata_combined$created_at), senti.over_scores = appleTweet.sent_vals)
googTweet.over_score = tibble(date = correctDate(googTwitterdata_combined$created_at), senti.over_scores = googTweet.sent_vals)
msftTweet.over_score = tibble(date = correctDate(msftTwitterdata_combined$created_at), senti.over_scores = msftTweet.sent_vals)

## organise the tweets according to each day
data.setsApple_over.score = organiseData(appleTweet.over_score)
data.setsGoog_over.score = organiseData(googTweet.over_score)
data.setsMsft_over.score = organiseData(msftTweet.over_score)

### calculate stock percentage change of closing price in 2015-2021
percentage_change <- function(x) {
  change <- (quantmod::Cl(x) - quantmod::Lag(quantmod::Cl(x)))/quantmod::Lag(quantmod::Cl(x)) * 100
  return(change)
}

applstocperc.change <- percentage_change(apple.stockdata)
googstocperc.change <- percentage_change(goog.stockdata)
msftstocperc.change <- percentage_change(msft.stockdata)

apple.stockdataChange = tibble(date = index(apple.stockdata), perc.change = as.numeric(applstocperc.change$AAPL.Close),
                               volume = apple.stockdata[,c("AAPL.Volume")], open = as.numeric(apple.stockdata$AAPL.Open),
                               close = as.numeric(apple.stockdata$AAPL.Close))
google.stockdataChange = tibble(date = index(goog.stockdata), perc.change = as.numeric(googstocperc.change$GOOG.Close),
                                volume = goog.stockdata[,c("GOOG.Volume")], open = as.numeric(goog.stockdata$GOOG.Open),
                                close = as.numeric(goog.stockdata$GOOG.Close))
msft.stockdataChange = tibble(date = index(msft.stockdata), perc.change = as.numeric(msftstocperc.change$MSFT.Close),
                              volume = msft.stockdata[,c("MSFT.Volume")], open = as.numeric(msft.stockdata$MSFT.Open),
                              close = as.numeric(msft.stockdata$MSFT.Close))

#### time series analysis

### percentqge change
apple.auto_arima =  auto.arima(apple.stockdataChange$perc.change, seasonal = F)
google.auto_arima =  auto.arima(google.stockdataChange$perc.change, seasonal = F)
msft.auto_arima =  auto.arima(google.stockdataChange$perc.change, seasonal = F)

### closing price
apple.auto_arima.cls =  auto.arima(apple.stockdataChange$close, seasonal = F)
google.auto_arima.cls =  auto.arima(google.stockdataChange$close, seasonal = F)
msft.auto_arima.cls =  auto.arima(google.stockdataChange$close, seasonal = F)

### forecast percentage change
term = 560
fcastapple = forecast(apple.auto_arima, h=term)
fcastgoog = forecast(google.auto_arima, h=term)
fcastmsft = forecast(msft.auto_arima, h=term)

### forecast closing price
fcastapple.cls = forecast(apple.auto_arima.cls, h=term)
fcastgoog.cls = forecast(google.auto_arima.cls, h=term)
fcastmsft.cls = forecast(msft.auto_arima.cls, h=term)

#### calculate negative and positive sentiment overal scores for each day
new.applrev_neg_pos.revsent = applneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%d/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)

new.googrev_neg_pos.revsent = googneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%d/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)

new.msftrev_neg_pos.revsent = msftneg_pos.revsent %>%
  group_by(date = as.Date(dmy(format(date, format="%d/%m/%Y")))) %>%
  summarise_at(vars(negative, positive), sum)

#### merge the tweet data overal scores and the stock data inorder to find correlation later
apple.sent_stock <- merge(apple.stockdataChange, data.setsApple_over.score, by='date')
goog.sent_stock <- merge(google.stockdataChange, data.setsGoog_over.score, by='date')
msft.sent_stock <- merge(msft.stockdataChange, data.setsMsft_over.score, by='date')

### remove the date column sand assign NA values to 0 so that it is easy to calculate the correlation matrix
newaapl.sent_stock = apple.sent_stock[,-1]
newaapl.sent_stock$perc.change[which(is.na(newaapl.sent_stock$perc.change))] = 0

newgoog.sent_stock = goog.sent_stock[,-1]
newgoog.sent_stock$perc.change[which(is.na(newgoog.sent_stock$perc.change))] = 0

newmsft.sent_stock = msft.sent_stock[,-1]
newmsft.sent_stock$perc.change[which(is.na(msft.sent_stock$perc.change))] = 0

###calculate the correlation matrix
cor(newaapl.sent_stock)
cor(newgoog.sent_stock)
cor(newmsft.sent_stock)

#### plot the graph of overall sentiment scores vs stock closing price
draw_graph = function(data, company){
  plot1 = ggplot(data,aes(x=date)) + ggtitle(paste0("Sentiment Scores for ", company, " from 2015-2021")) +
    geom_line(aes(y=senti.over_scores, group=1,colour=paste0("Sentiment Scores ", company))) + 
    scale_colour_manual(values=c("red")) + labs(y="Scores", x="Date", colour="Parameter") + 
    theme(legend.position=c(0.87,0.885))
  
  plot2 = ggplot(data,aes(x=date)) + ggtitle(paste0("Stocks Price Change for ", company, " from 2015-2021")) +
    geom_line(aes(y=close, group=1, colour=paste0("Stock Closing Price for ", company))) +
    scale_colour_manual(values=c("blue")) + labs(y="Price", x="Date", colour="Parameter") +
    theme(legend.position=c(0.87,0.885))
  
  combined_plots = ggarrange(plot1, plot2,
                             ncol = 2, nrow = 1)
  
  return(combined_plots)
}

draw_graph(apple.sent_stock, "Apple")
draw_graph(goog.sent_stock, "Google")
draw_graph(msft.sent_stock, "Microsoft")

### perform linear regression analysis
appleclosestocks.scores.lm <- lm(close~senti.over_scores, data=apple.sent_stock)
appleopenstocks.scores.lm <- lm(open~senti.over_scores, data=apple.sent_stock)

googclosestocks.scores.lm <- lm(close~senti.over_scores, data=goog.sent_stock)
googopenstocks.scores.lm <- lm(open~senti.over_scores, data=goog.sent_stock)

msftclosestocks.scores.lm <- lm(close~senti.over_scores, data=msft.sent_stock)
msftopenstocks.scores.lm <- lm(open~senti.over_scores, data=msft.sent_stock)

summary(appleclosestocks.scores.lm)
summary(appleopenstocks.scores.lm)

summary(googclosestocks.scores.lm)
summary(googopenstocks.scores.lm)

summary(msftclosestocks.scores.lm)
summary(msftopenstocks.scores.lm)

### clean data before machine learning

apple_tweet.scores = tibble(tweet.text = appleTwitterdata_combined$text, 
                            senti.over_scores = appleTweet.sent_vals)
goog_tweet.scores = tibble(tweet.text = googTwitterdata_combined$text, 
                           senti.over_scores = googTweet.sent_vals)
msft_tweet.scores = tibble(tweet.text = msftTwitterdata_combined$text, 
                           senti.over_scores = msftTweet.sent_vals)

apple_tweet.scores = apple_tweet.scores%>%
  mutate(sentiments = 
           case_when(
             senti.over_scores > 0  ~ "positive",
             senti.over_scores == 0  ~ "neutral",
             senti.over_scores < 0  ~ "negative",
           )
  )


goog_tweet.scores = goog_tweet.scores%>%
  mutate(sentiments = 
           case_when(
             senti.over_scores > 0  ~ "positive",
             senti.over_scores == 0  ~ "neutral",
             senti.over_scores < 0  ~ "negative",
           )
  )

msft_tweet.scores = msft_tweet.scores%>%
  mutate(sentiments = 
           case_when(
             senti.over_scores > 0  ~ "positive",
             senti.over_scores == 0  ~ "neutral",
             senti.over_scores < 0  ~ "negative",
           )
  )

#### perform Machine learning

set.seed(3)

sent.predict = function(data){
  id = sample(2, nrow(data), prob = c(0.7,0.3), replace = TRUE)
  data_train = data[id==1,]
  data_test = data[id==2,]
  
  data$sentiments = as.factor(data$sentiments)
  data_train$sentiments = as.factor(data_train$sentiments)
  data_test$sentiments = as.factor(data_test$sentiments)
  
  ### Random forest
  data_forest = randomForest(sentiments~tweet.text, data = data_train)
  pred_tweet.sent.forest = predict(data_forest, data_test, type= "class")
  conf.matrx.forest = confusionMatrix(table(pred_tweet.sent.forest, data_test$sentiments))
  
  ### Naive Bayes
  data_bayes = naiveBayes(sentiments~tweet.text, data = data_train)
  pred_tweet.sent.bayes = predict(data_bayes, data_test)
  conf.matrx.bayes = confusionMatrix(table(pred_tweet.sent.bayes, data_test$sentiments))
  
  ### support vector machine
  
  trcntrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
  
  svmlinear = train(sentiments~tweet, data = data_train, method = "svmLinear",
                    trControl = trcntrl,
                    preProcess = c("center", "scale"), tuneLength = 10
  )
  test_pred = predict(svmlinear, newdata = data_test)
  conf.matrx.svm = confusionMatrix(table(test_pred, data_test$sentiments))
  return(conf.matrx = list(conf.matrx.forest = conf.matrx.forest, conf.matrx.bayes = conf.matrx.bayes, conf.matrx.svm = conf.matrx.svm))
}

apple_tweet.scores%>%
  sent.predict()

goog_tweet.scores%>%
  sent.predict()

msft_tweet.scores%>%
  sent.predict()

