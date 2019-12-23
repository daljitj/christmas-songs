library(tidyverse)
library(rvest)
library(stringr)

########
setwd("C:/Users/User/Documents/R/christmas songs")
getwd()

#Specifying the url for desired website to be scraped
url <- 'https://en.wikipedia.org/wiki/List_of_Christmas_hit_singles_in_the_United_Kingdom'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
songs_html <- html_nodes(webpage,xpath = '//*[@id="mw-content-text"]/div/table[2]')

#Converting the ranking data to text
songs_wiki <- html_table(songs_html) %>% as.data.frame(stringsasfactors=FALSE)

###################### USA ##################################
#Specifying the url for desired website to be scraped
url <- 'https://en.wikipedia.org/wiki/List_of_popular_Christmas_singles_in_the_United_States'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
USA_songs_html <- html_nodes(webpage,xpath = '//*[@id="mw-content-text"]/div/table[3]')

#Converting the ranking data to text
USA_songs_wiki <- html_table(USA_songs_html) %>% as.data.frame()

#################################################################

christmashits <- bind_rows(songs_wiki,USA_songs_wiki,.id="Country") %>% select(Artist, Title, Year, Country) %>%
                              mutate(Country = if_else(Country==1,"UK","USA"))


###################### UK ##########################

# count by artist and year
count(songs_wiki,Artist, sort=TRUE)

count(songs_wiki,Artist, sort=TRUE) %>%
  filter(n>1) %>%
  ggplot(aes(x=reorder(Artist,n), n)) +
  geom_col() +
  ylab("Number of hits") +
  xlab("Year") +
  labs(title="Number of UK Christmas hits by artist",subtitle = "Artists with more than 1 hit only") +
  coord_flip() +
  theme_classic()

## find cases of multiple songs made by same person
filter(songs_wiki,duplicated(songs_wiki$Artist) == TRUE ) %>% select(Artist, Title, Year) %>% arrange(Artist)

###################### USA ##########################
# count by artist and year
count(USA_songs_wiki,Artist, sort=TRUE)

count(USA_songs_wiki,Artist, sort=TRUE) %>%
  filter(n>1) %>%
  ggplot(aes(x=reorder(Artist,n), n)) +
  geom_col() +
  ylab("Number of hits") +
  xlab("Year") +
  labs(title="Number of USA Christmas hits by artist",subtitle = "Artists with more than 1 hit only") +
  coord_flip() +
  theme_classic()

## find cases of multiple songs made by same person
filter(USA_songs_wiki,duplicated(USA_songs_wiki$Artist) == TRUE ) %>% select(Artist, Title, Year) %>% arrange(Artist)

###################### Together ##########################
# count by artist and year
count(christmashits,Artist, sort=TRUE) ## 612 songs

count(christmashits,Artist, sort=TRUE) %>%
  filter(n>1) %>%
  ggplot(aes(x=reorder(Artist,n), n)) +
  geom_col() +
  ylab("Number of hits") +
  xlab("Year") +
  labs(title="Number of Christmas hits in the UK and USA by artist",subtitle = "Artists with more than 1 hit only") +
  coord_flip() +
  theme_classic()

## find cases of multiple songs made by same person
filter(christmashits,duplicated(christmashits$Artist) == TRUE ) %>% select(Artist, Title, Year) %>% arrange(Artist)


###################### UK ##########################
## plot releases by year
count(songs_wiki,Year,sort=TRUE)

count(songs_wiki,Year ) %>%
  ggplot(aes(Year, n))+
  geom_col() +
  ylab("Number of hit Christmas songs released") +
  xlab("Year") +
  labs(title="Release year of hit UK Christmas songs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic()

###################### USA ##########################
## plot releases by year
count(USA_songs_wiki,Year,sort=TRUE)

count(USA_songs_wiki,Year ) %>%
  ggplot(aes(Year, n))+
  geom_col() +
  ylab("Number of hit Christmas songs released") +
  xlab("Year") +
  labs(title="Release year of hit UK Christmas songs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic()

###################### Together ##########################
## plot releases by year
count(christmashits,Year,Country,sort=TRUE)

count(christmashits,Year, Country ) %>%
  ggplot(aes(Year, n,fill=Country))+
  geom_col() +
  ylab("Number of hit Christmas songs released") +
  xlab("Year") +
  labs(title="Release year of hit Christmas songs") +
  facet_wrap(~ Country) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic()

# Density plot
ggplot(data=christmashits, aes(x=Year, group=Country, fill=Country)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()

# Histogram
ggplot(data=christmashits, aes(x=Year, group=Country, fill=Country)) +
  geom_histogram( alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")


##################### Which songs are hits in the UK and USA

duplicates <- filter(christmashits,duplicated(christmashits$Title) == TRUE ) %>% select(Artist, Title, Year) %>% arrange(Artist)
count(duplicates,Artist, sort=TRUE)

notduplicated <- filter(christmashits,duplicated(christmashits$Title) == FALSE ) %>% select(Artist, Title, Year,Country) %>% arrange(Artist)
count(notduplicated,Artist, Country, sort=TRUE)


###########################################################
library(genius)
help(package = genius)

christmaslyrics <- christmashits %>% add_genius(Artist, Title, type = "lyrics")

write.csv(christmaslyrics,"christmaslyrics.csv")


filter(christmaslyrics, Country=="UK") %>% distinct(Title) # 95 UK songs
filter(christmaslyrics, Country=="USA") %>% distinct(Title) # 285 UK songs

count(christmaslyrics,Year, Country ) %>%
  ggplot(aes(Year, n,fill=Country))+
  geom_col() +
  ylab("Number of hit Christmas songs released") +
  xlab("Year") +
  labs(title="Release year of hit Christmas songs") +
  facet_wrap(~ Country) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic()

#### which songs have we lost

nolyrics <- filter(christmashits,!Title %in% christmaslyrics$Title)

count(nolyrics,Year, Country ) %>%
  ggplot(aes(Year, n,fill=Country))+
  geom_col() +
  ylab("Number of hit Christmas songs released") +
  xlab("Year") +
  labs(title="Release year of hit Christmas songs") +
  facet_wrap(~ Country) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme_classic()

count(nolyrics,Artist, sort=TRUE)   


####### songs with most words
unnest_tokens(christmaslyrics, word, lyric)  %>%
  group_by(Title) %>%
  mutate(numofwords = n())  %>% 
  ungroup() %>%  dplyr::distinct(Title, .keep_all=TRUE) %>%
  dplyr::top_n(20) %>%
  ggplot(aes(x=reorder(Title,numofwords), numofwords)) +
  geom_col() +
  ylab("Number of words") +
  xlab("") +
  labs(title="Number of words in UK and USA Christmas hit songs",subtitle = "Top 20 only") +
  coord_flip() +
  theme_classic()


####### songs with fewest words

unnest_tokens( word, lyric)  %>%
  group_by(Title) %>%
  mutate(numofwords = n())  %>% 
  ungroup() %>%  dplyr::distinct(Title, .keep_all=TRUE) %>%
  dplyr::top_n(-20) %>%
  ggplot(aes(x=reorder(Title,numofwords), numofwords)) +
  geom_col() +
  ylab("Number of words") +
  xlab("") +
  labs(title="UK and USA Christmas hit songs with the fewest number of words",subtitle = "Top 20 only") +
  coord_flip() +
  theme_classic()

################################################ UK vs USA average song length

unnest_tokens(christmaslyrics, word, lyric)  %>%
  group_by(Title) %>%
  mutate(numofwords = n())  %>% 
  ungroup() %>%  dplyr::distinct(Title, .keep_all=TRUE) %>%
  group_by(Country) %>%
  mutate(averagewords = mean(numofwords)) %>% 
  ggplot(aes(x=reorder(Country,averagewords), averagewords)) +
  geom_col() +
  ylab("Number of words") +
  xlab("") +
  labs(title="UK and USA Christmas hit songs with the fewest number of words",subtitle = "Top 20 only") +
  coord_flip() +
  theme_classic()


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

## boxplot
unnest_tokens(christmaslyrics, word, lyric)  %>%
  group_by(Title) %>%
  mutate(numofwords = n())  %>% 
  ungroup() %>%  dplyr::distinct(Title, .keep_all=TRUE) %>%
  ggplot( aes(x=Country, y=numofwords, fill=Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of words in Christmas hit songs") +
  xlab("")

## violin plot
unnest_tokens(christmaslyrics, word, lyric)  %>%
  group_by(Title) %>%
  mutate(numofwords = n())  %>% 
  ungroup() %>%  dplyr::distinct(Title, .keep_all=TRUE) %>%
ggplot( aes(x=Country, y=numofwords, fill=Country)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of words in Christmas hit songs") +
  xlab("")


#############

library(tidytext)
data("stop_words")


bagoflyrics <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(word,sort = TRUE)

### most common words

bagoflyrics %>%
  filter(n > 100) %>%
  top_n(25) %>%
  group_by(word) %>%
  ggplot(aes(x=fct_reorder(word,n), n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Number of words") +
  labs(title="Most common words found in UK and US Christmas hit songs",subtitle="Top 25 words only") +
  coord_flip() +
  theme_classic()

#### word cloud of words

library(wordcloud2)

wordcloud2(bagoflyrics, shape="star", size =1.5, color = "gold", backgroundColor="black")


#### tf-idf ###########

tfidf <- unnest_tokens(christmaslyrics, word, lyric) %>% 
            count(Country,word,sort = TRUE) %>%
            bind_tf_idf(word,Country,n) %>%
            arrange(desc(tf_idf))


tfidf %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Country) %>%
  top_n(25) %>% 
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = Country)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf",title = "TF-IDF of Christmas hit songs in the UK and USA",subtitle = "Top 25 words only") +
  facet_wrap(~Country, ncol = 2, scales = "free") +
  coord_flip()



################## sentiments

sentiments_afinn <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  inner_join(get_sentiments("afinn")) 
  
sentiments_bing <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  inner_join(get_sentiments("bing"))

sentiments_nrc  <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  inner_join(get_sentiments("nrc"))

### net positivity UK vs USA, USA below because of ICP and South Park
sentiments_afinn %>%  
  group_by(Title) %>%
  mutate(net_sentiment = sum(value)) %>%
  ungroup() %>%
  group_by(Country) %>% 
  mutate(netsent = mean(net_sentiment)) %>%
  ungroup() %>%
  distinct(netsent, .keep_all = TRUE) %>%
  ggplot(aes(x=reorder(Country,netsent),netsent)) +
  geom_col(fill="Gold", show.legend = FALSE)  +
  coord_flip() +
  ylab("Net positivity") +
  xlab("") +
  labs(title="Positivity among hit Christmas songs in the USA and UK") +
  theme_classic() 

## boxplot
sentiments_afinn %>%  
  group_by(Title) %>%
  mutate(net_sentiment = sum(value)) %>%
  ungroup() %>% dplyr::distinct(Title, .keep_all=TRUE) %>%
  ggplot( aes(x=Country, y=net_sentiment, fill=Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Net positivity in Christmas hit songs") +
  xlab("")+
  ylab("Net positivity")


## violin plot
sentiments_afinn %>%  
  group_by(Title) %>%
  mutate(net_sentiment = sum(value)) %>%
  ungroup() %>% dplyr::distinct(Title, .keep_all=TRUE) %>%
ggplot( aes(x=Country, y=net_sentiment, fill=Country)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Net positivity in Christmas hit songs") +
  xlab("") +
  ylab("Net positivity")


######## remove ICP and South Park

#sentiments_afinn %>%  
#  filter(!str_detect(Artist,"South Park")) %>%
#  filter(!str_detect(Artist,"Insane Clown")) %>%
#  group_by(Title) %>%
#  mutate(net_sentiment = sum(value)) %>%
#  ungroup() %>%
#  group_by(Country) %>% 
#  mutate(netsent = mean(net_sentiment)) %>%
#  ungroup() %>%
#  distinct(netsent, .keep_all = TRUE) %>%
#  ggplot(aes(x=reorder(Country,netsent),netsent)) +
#  geom_col(fill="Gold", show.legend = FALSE)  +
#  coord_flip() +
#  ylab("Net positivity") +
#  xlab("") +
#  labs(title="Positivity among hit Christmas songs in the USA and UK") +
#  theme_classic() 
#

### View sentiments 
#sentiments_afinn %>%  
#  group_by(Title) %>%
#  mutate(net_sentiment = sum(value)) %>%
#  ungroup() %>%
#  group_by(Country) %>% 
#  mutate(netsent = mean(net_sentiment)) %>%
#  ungroup() %>% distinct(Title, .keep_all = TRUE) %>% View()
#

## TOTAL most positive songs bing
sentiments_bing %>%
  count(Title,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  group_by(Title) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup() %>%
  top_n(15, (sentiment)) %>%
  ggplot(aes(x=reorder(Title,sentiment),sentiment)) +
  geom_col(fill="Gold", show.legend = FALSE)  +
  coord_flip() +
  ylab("Positivity") +
  labs(title="Most positive UK and US Christmas hit songs") +
  theme_classic() 

## TOTAL most positive songs afinn
sentiments_afinn %>%
  group_by(Title) %>%
  mutate(sum_sentiment = sum(value)) %>%
  ungroup() %>%
  distinct(Title, .keep_all = TRUE) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(Title,sum_sentiment),sum_sentiment)) +
  geom_col(fill="Gold", show.legend = FALSE)  +
  coord_flip() +
  ylab("Positivity") +
  labs(title="Top 25 most positive UK and US Christmas hit songs") +
  theme_classic() 

## TOTAL most negative songs bing
sentiments_bing %>%
  count(Title,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  group_by(Title) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup() %>%
  top_n(15, (-sentiment)) %>%
  ggplot(aes(x=reorder(Title,-sentiment),sentiment)) +
  geom_col(fill="Red", show.legend = FALSE)  +
  coord_flip() +
  ylab("Negativity") +
  labs(title="Most negative UK and US Christmas hit songs") +
  theme_classic() 

## TOTAL most negative songs afinn
sentiments_afinn %>%
  group_by(Title) %>%
  mutate(sum_sentiment = sum(value)) %>%
  ungroup() %>%
  distinct(Title, .keep_all = TRUE) %>%
  top_n(15, (-sum_sentiment)) %>%
  ggplot(aes(x=reorder(Title,-sum_sentiment),sum_sentiment)) +
  geom_col(fill="Red", show.legend = FALSE)  +
  coord_flip() +
  ylab("Negativity") +
  labs(title="Top 15 most negative UK and US Christmas hit songs") +
  theme_classic() 

########################### look at specific title  ##########################
filter(christmaslyrics,str_detect(Title, "Blue")) %>% View()
#############################################################################


## most positive songs bing
sentiments_bing %>%
  count(Title,sentiment, Country) %>%
  spread(sentiment,n,fill=0) %>%
  group_by(Title) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup() %>%
  group_by(Country) %>%
  top_n(15, (sentiment)) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(Title,sentiment),sentiment)) +
  geom_col(fill="Gold", show.legend = FALSE)  +
  coord_flip() +
  ylab("Positivity") +
  labs(title="Most positive UK and US Christmas hit songs") +
  theme_classic() +
  facet_wrap(~ Country,ncol=2,scales="free")

## most positive songs afinn
sentiments_afinn %>%
  group_by(Title) %>%
  mutate(sum_sentiment = sum(value)) %>%
  ungroup() %>%
  distinct(Title, .keep_all = TRUE) %>%
  group_by(Country) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(Title,sum_sentiment),sum_sentiment)) +
  geom_col(fill="Gold", show.legend = FALSE)  +
  coord_flip() +
  ylab("Positivity") +
  labs(title="Top 15 most positive UK and US Christmas hit songs") +
  theme_classic() +
  facet_wrap(~ Country,ncol=2,scales="free")




## most negative songs bing
sentiments_bing %>%
  count(Title,sentiment, Country) %>%
  spread(sentiment,n,fill=0) %>%
  group_by(Title) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup() %>%
  group_by(Country) %>%
  top_n(15, (-sentiment)) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(Title,-sentiment),sentiment)) +
  geom_col(fill="red", show.legend = FALSE)  +
  coord_flip() +
  ylab("Negativity") +
  labs(title="Most negative UK and US Christmas hit songs") +
  theme_classic() +
  facet_wrap(~ Country,ncol=2,scales="free")

## most negative songs afinn
sentiments_afinn %>%
  group_by(Title) %>%
  mutate(sum_sentiment = sum(value)) %>%
  ungroup() %>%
  distinct(Title, .keep_all = TRUE) %>%
  group_by(Country) %>%
  top_n(10, (-sum_sentiment)) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(Title,-sum_sentiment),sum_sentiment)) +
  geom_col(fill="Red", show.legend = FALSE)  +
  coord_flip() +
  ylab("Negativity") +
  labs(title="Top 10 most negative UK and US Christmas hit songs") +
  theme_classic() +
  facet_wrap(~ Country,ncol=2,scales="free")



### by emotion

sentiments_nrc %>%
  count(sentiment,sort=TRUE) %>%
  ungroup() %>%
  filter(!sentiment=="positive") %>%
  filter(!sentiment=="negative") %>%
  mutate(nrc = fct_reorder(sentiment,n)) %>%
  ggplot(aes(nrc,n)) +
  geom_col(show.legend=FALSE) +
  coord_flip()


##### country no difference
sentiments_nrc %>%
  count(sentiment,Country,sort=TRUE) %>%
  ungroup() %>%
  filter(!sentiment=="positive") %>%
  filter(!sentiment=="negative") %>%
  mutate(nrc = fct_reorder(sentiment,n)) %>%
  ggplot(aes(nrc,n,fill=Country)) +
  geom_col(show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~ Country,ncol=2,scales="free")



### emotion by word

sentiments_nrc %>%
count(word,sort=TRUE,sentiment) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment,scales="free_y",ncol = 5) +
  labs(y="Contribution to sentiment",
       x=NULL) +
  coord_flip()





### most common songs by emotion
sentiments_nrc %>%
  count(Title,sort=TRUE,sentiment) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  arrange(desc(n)) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(Title,n,sentiment),n,fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  scale_x_reordered() +
  facet_wrap(~sentiment,scales="free_y",ncol = 5) +
  labs(y="Contribution to sentiment",
       x=NULL) +
  coord_flip()



## boxplot
sentiments_nrc %>%
  group_by(Title) %>%
  mutate(net_sentiment = sum(value)) %>%
  ungroup() %>% dplyr::distinct(Title, .keep_all=TRUE) %>%
  ggplot( aes(x=Country, y=net_sentiment, fill=Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Net positivity in Christmas hit songs") +
  xlab("")+
  ylab("Net positivity")


###########################


################################ create dataframe for correlation analysis
frequency <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(word,Title,sort = TRUE) %>%
  group_by(Title) %>%
  mutate(proportion = n / sum(n)) %>% 
  ungroup() %>%
  select(-c(n))



#create dataframe for corr chart


word_cor <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(word,Title,sort = TRUE) %>%
    group_by(word) %>%
    filter(n() >= 5) %>%
    widyr::pairwise_cor(word, Title) 


library(igraph)
library(ggraph)
library(tidygraph)

word_cor %>% 
  filter(!is.na(correlation),
       correlation > 0.5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_classic()


## by songs
song_cor <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(word,Title,sort = TRUE) %>%
  group_by(Title) %>%
  filter(n() >= 10) %>%
  widyr::pairwise_cor(Title,word,n) 

song_cor %>% 
  filter(!is.na(correlation),
         correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_classic()


as_tbl_graph(song_cor,directed=FALSE) %>% activate(nodes) %>% 
  mutate(community = as.factor(group_infomap())) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(colour = community), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE)+
  theme_graph()

################################ create dataframe for correlation analysis
frequency1 <- unnest_tokens(christmaslyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(word,Country,sort = TRUE) %>% 
  group_by(Country) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Country, proportion) %>% 
  gather(Country, proportion, `UK`) %>%
  mutate(USA = if_else(is.na(USA)==TRUE,0,USA)) %>%
  mutate(proportion = if_else(is.na(proportion)==TRUE,0,proportion))



# Correlation of words between Boris and other leaders
#NAs not plotted?
ggplot(frequency1, aes(x = proportion, y = `USA`, color = abs(`USA` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75")  +
  theme(legend.position="none") +
  labs(y = "USA", x = "UK")


############## text generation #############
#devtools::install_github("abresler/markovifyR")
#system("pip install markovify")

library(markovifyR)


forgeneration <- christmaslyrics[!is.na(christmaslyrics$lyric), ] %>% select(lyric) %>% as.list() %>% unlist()
forgeneration <- as.character(forgeneration) %>% unlist()


markov_model <-
  generate_markovify_model(
    input_text = forgeneration,
    markov_state_size = 2L,
    max_overlap_total = 25,
    max_overlap_ratio = .85
  )

generatedlyrics <- markovify_text(
                    markov_model = markov_model,
                    maximum_sentence_length = NULL,
                    output_column_name = 'SKYNET Santa',
                    count = 25,
                    tries = 100,
                    only_distinct = TRUE,
                    return_message = TRUE)
                  
generate_start_words(markov_model = markov_model)


########### generate without south park ############


withoutsouthpark <- dplyr::filter(christmaslyrics,!Artist== "South Park")
withoutsouthpark <- dplyr::filter(withoutsouthpark,!Artist=="Stan Freberg")

forgeneration <- withoutsouthpark[!is.na(withoutsouthpark$lyric), ] %>% select(lyric)  
forgeneration <- as.character(forgeneration) %>% unlist()

markov_model <-
  generate_markovify_model(
    input_text = forgeneration,
    markov_state_size = 2L,
    max_overlap_total = 25,
    max_overlap_ratio = .85
  )

generatedlyricswithoutsouthpark <- markovify_text(
  markov_model = markov_model,
  maximum_sentence_length = NULL,
  output_column_name = 'SKYNET Santa',
  count = 50,
  tries = 100,
  only_distinct = TRUE,
  return_message = TRUE)

generate_start_words(markov_model = markov_model)



markovify_christmas <- function() {
  lines <- markovify_text(
    markov_model = markov_model,
    maximum_sentence_length = 10,
    output_column_name = 'song_line',
    count = 50,
    tries = 1000, 
    start_words = sample(generate_start_words(markov_model)$wordStart,length(generate_start_words(markov_model)$wordStart)),
    only_distinct = TRUE,
    return_message = FALSE) %>% 
    mutate(id = 1:n()) %>% 
    select(id, song_line) 
  
  
  #  print in a song-like format
  walk(1:16, function(.x) {
    cat(lines$song_line[.x], " \n")
    
    #  add a space every four lines
    if (.x %% 4 == 0) cat("\n") 
  })
}

markovify_christmas()


######## only bing crosby


bingcrosby <- dplyr::filter(christmaslyrics,Artist== "Bing Crosby")

forgeneration <- bingcrosby[!is.na(bingcrosby$lyric), ] %>% select(lyric)  %>% as.list() %>% unlist() %>% as.character()

markov_model <-
  generate_markovify_model(
    input_text = forgeneration,
    markov_state_size = 3L,
    max_overlap_total = 25,
    max_overlap_ratio = 0.7
  )


markovify_christmas()


