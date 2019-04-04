library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)


#define credentials to log in to Twitter API
consumerKey = "QqstUdQ2yLRUGboueH7wJIsv1"
consumerSecret = "ykRtbBKSarjpQYRBCUBUHaiuW4FnPz2iDLO2TkcEtO9mocYOTN"
accessToken = "240583200-1R98JE0NLVS1CzwngMk5zOywJzd4Ia2MMYX7jMF3"
accessSecret = "SdVrYx1Gdu0lNPBxAw6AkkQBiTcLUHPc6SrPNNPqttKGd"
options(httr_oauth_cache=TRUE)

setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

#Scraping replies to Creasy
tweetstostella <-
  searchTwitter("stellacreasy exclude:retweets", n = 3200)
tweetstostella_df <- twListToDF(tweetstostella)

tweet_words <- tweetstostella_df %>% select(id, text) %>% unnest_tokens(word,text)

#sorting and simple plot of tweets
word_count <- tweet_words %>% count(word, sort = T) 

wordstheme <- theme(plot.title = element_text(colour = "steelblue", size = 20, hjust = 0.5), 
                    axis.text.x = element_text(angle = 60, hjust = 1))


word_count %>% slice(1:20) %>%
  ggplot(aes(x = reorder(word, n, function(n) - n), y = n)) + 
  geom_bar(stat = "identity") + 
  wordstheme +
  xlab("words") + 
  ylab("word count") + 
  ggtitle("how many times do certain words appear?")


  
#adding stop words to plotting
my_stop_words <- stop_words %>% select(-lexicon) %>%
  bind_rows(data.frame(
    word = c(
      "https",
      "t.co",
      "stellacreasy",
      "a",
      "to",
      "theresa_may",
      "stella",
      "10downingstreet",
      "it's",
      "mp",
      "amp",
      "is",
      "it",
      "on",
      "that",
      "for",
      "be",
      "not",
      "it's",
      "jessphillips",
      "mrjamesbob",
      "bbcpolitics",
      "labour"
    )
  ))
#secondary plotting after stop words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% tally(sort = TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) - n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,
                                                           hjust = 1)) + xlab("")

#Sentiment analysis x 2 (nrc and bing)
bing_lex <- get_sentiments("nrc")

fn_sentiment <- tweet_words_interesting %>% left_join(bing_lex)

fn_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n =
                                                                                   n())
(get_sentiments("bing")) %>%
  count(sentiment) 

get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)

#next steps test 
 
ungroup() %>%
unnest_tokens(word, text)

bing_word_counts <- tweet_words_interesting %>%
  inner_join(get_sentiments("bing")) %>%
  count("word", sentiment, sort = TRUE) %>%
  ungroup()

nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiments =="negative")

filter(tweet_words_interesting) %>%
inner_join(nrc_negative)%>%
count(word, sort=TRUE)


sentiments %>% 
  count(word,sentiment) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word= reorder(word, n)) %>%
  sample_n(45) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()
