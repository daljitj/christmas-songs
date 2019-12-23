library(tidyverse)
library(rvest)
library(stringr)

########
setwd("C:/Users/User/Documents/R/christmas songs")
getwd()

#Specifying the url for desired website to be scraped
url <- 'https://www.liveabout.com/top-christmas-songs-3245323'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
songs_html <- html_nodes(webpage,'.mntl-sc-block-heading__link , #mntl-sc-block_2-0-42 , .mntl-sc-block-heading__text')

#Converting the ranking data to text
songs <- html_text(songs_html)
songs <- as_tibble(songs)

### remove Bieber duplicate
songs <- songs[-15,]


### split strings
songs_clean <- str_split(songs$value,":" )
songs_clean <-  matrix(unlist(songs_clean), ncol=2, byrow=TRUE) 
songs_clean1 <- str_split(songs_clean[,2],"\\(" )
songs_clean1 <-  matrix(unlist(songs_clean1), ncol=2, byrow=TRUE) 
artistyear <- tibble(artist = songs_clean1[,1],year=songs_clean1[,2])
artistyear <- mutate(artistyear, year = str_extract(year,"[0-9]{4}"))

## put final set together
christmas_songs <- tibble(artist=artistyear$artist, song = songs_clean[,1], year = artistyear$year)

# count by artist and year
count(christmas_songs,artist, sort=TRUE)

## find cases of multiple songs made by same person
filter(christmas_songs,duplicated(christmas_songs$artist) == TRUE )



## plot releases by year
count(christmas_songs,year ) %>%
  ggplot(aes(year, n))+
  geom_col() +
  ylab("Number of top Christmas songs") +
  xlab("Year") +
  labs(title="Release year of top Christmas songs") +
  theme_classic()

filter(christmas_songs,year == 2006)


### get lyrics from genius

library(genius)
help(package = genius)

lyrics <- add_genius(christmas_songs, artist, song, type = "lyrics")

write.csv(lyrics,"lyricsfromwebsite.csv")


count(lyrics,artist, sort=TRUE)


library(tidytext)
data("stop_words")


bagoflyrics1 <- unnest_tokens(lyrics, word, lyric) %>% 
  anti_join(stop_words) %>%
  count(song,word,sort = TRUE)



bagoflyrics1 %>%
  filter(n > 10) %>%
  group_by(word) %>%
  ggplot(aes(x=fct_reorder(word,n), n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Number of words") +
  coord_flip()



