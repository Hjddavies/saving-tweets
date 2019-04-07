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
library(tidyverse)

consumerKey = "QqstUdQ2yLRUGboueH7wJIsv1"
consumerSecret = "ykRtbBKSarjpQYRBCUBUHaiuW4FnPz2iDLO2TkcEtO9mocYOTN"
accessToken = "240583200-1R98JE0NLVS1CzwngMk5zOywJzd4Ia2MMYX7jMF3"
accessSecret = "SdVrYx1Gdu0lNPBxAw6AkkQBiTcLUHPc6SrPNNPqttKGd"
options(httr_oauth_cache=TRUE)

#fix found on https://stackoverflow.com/questions/29634342/unable-to-use-the-setup-twitter-oauth-function-in-r-to-work-with-twitter
twitteR:::setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                              access_token = accessToken, access_secret = accessSecret)

#Scraping replies to Creasy
tweetstostella2 <- searchTwitter("@stellacreasy exclude:retweets", n=3200)
tweetstostella2_df <- tbl_df(map_df(tweetstostella2, as.data.frame))
write.csv(futureexwife_df, "tweetstostella2.csv")

tweetstostella2_df <- twListToDF(tweetstostella2)
tweet_words <- tweetstostella2_df %>% select(id, text) %>% unnest_tokens(word,text)


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
#(largely from http://utstat.toronto.edu/~nathan/teaching/sta4002/Class1/scrapingtwitterinR-NT.html)
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
      "labour",
      "grieve"
    )
  ))

#secondary plotting after stop words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% tally(sort = TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) - n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,
                                                                                                                             hjust = 1)) + xlab("")

#Sentiment analysis x 2 (nrc and bing)
(get_sentiments("bing")) %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment) 
#Result
#sentiment     n
#<chr>     <int>
#1 negative   4782
#2 positive   2006


get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)
#Result
#  sentiment     n
#<chr>     <int>
#1 negative   3324
#2 positive   2312

bing_negative <- get_sentiments("bing") %>%
  filter(sentiments == "negative")
filter(tweet_words_interesting) %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

bing_positive <- get_sentiments("bing") %>%
  filter(sentiments == "positive")
filter(tweet_words_interesting) %>%
  inner_join(bing_lex) %>%
  count(word, sort = TRUE)

bing_word_counts <- tweet_words_interesting %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


#plotting sentiments
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#searching gendered abuse in tweetstostella2.df manually finds little of interest:
#bitch appears twice, cunt no times, cow (as an insult) once. 
#But what of the abuse around Brexit that talks of traitors and liars?

traitor_words <- tweet_words_interesting %>% filter(str_detect(word, "^traito") | str_detect(word, "^treason"))

hypoocrite_words <- tweet_words_interesting %>% filter(str_detect(word, "^hypocr"))

deception_words <- tweet_words_interesting %>% filter(str_detect(word, "^decei"))

liar_words <- tweet_words_interesting %>% filter(str_detect(word, "^liar") | str_detect(word, "^lying"))

abuse_words <- bind_rows(betrayal_words, traitor_words, hypoocrite_words, deception_words, liar_words)

word_count_of_abuse <- data.frame(type_of_abuse = character(), count = integer())

word_count_of_abuse <-rbind(word_count_of_abuse, list(type_of_abuse = "betrayal words", count = nrow(betrayal_words)),stringsAsFactors=FALSE) 
word_count_of_abuse <-rbind(word_count_of_abuse, list(type_of_abuse = "traitor words", count = nrow(traitor_words)),stringsAsFactors=FALSE) 
word_count_of_abuse <-rbind(word_count_of_abuse, list(type_of_abuse = "hypocrite words", count = nrow(hypoocrite_words)),stringsAsFactors=FALSE) 
word_count_of_abuse <-rbind(word_count_of_abuse, list(type_of_abuse = "liar words ", count = nrow(liar_words)),stringsAsFactors=FALSE) 
word_count_of_abuse <-rbind(word_count_of_abuse, list(type_of_abuse = "deception words", count = nrow(deception_words)),stringsAsFactors=FALSE) 

wordstheme <- theme(plot.title = element_text(colour = "steelblue", size = 20, hjust = 0.5), 
                    axis.text.x = element_text(colour = "steelblue",angle = 60, hjust = 1),
                    axis.text.y = element_text(colour = "steelblue",angle = 0, hjust = 1),
                    axis.title.x = element_text(colour = "steelblue"),
                    axis.title.y = element_text(colour = "steelblue")
)

#We can of course do a simple plot of the abusive words used:
abuse_words %>% group_by(word) %>% tally(sort = TRUE) %>% slice(1:20) %>%
  ggplot(aes(x = reorder(word, n, function(n)
    - n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,
                                                                                   hjust = 1)) + xlab("")

#various other experiments with the binded rows give us various changes of colours, frames etc. (see Rplot5i, ii, iii, iv) then we do this:

#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#plus stackoverflow.com to remove legend
word_count_of_abuse %>%
  ggplot(aes(x = reorder(type_of_abuse, count, function(n) - n), y = count, fill = type_of_abuse)) + 
  geom_bar(stat="identity",color = "red") + 
  scale_fill_brewer() +
  wordstheme +
  xlab("Abuse") + 
  ylab("Word Count") + 
  ggtitle("Treasonous Tweets") +
  theme(legend.position = "none")



