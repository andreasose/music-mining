library(plyr)
library(ggplot2)
library(plotly)
library(lubridate)

#Make sure your csv file is in the working directory
music <- read.csv("andreas.csv", header = FALSE)


#By default, the data comes without column headers, so they are added manually:
colnames(music) <- c("artist", "album", "song", "date")

#the date colum is down to the second. We only need it by day, month and year:
music$date <- as.Date(music$date, format = "%d %B %Y")
music <- music[music$date >= "2010-06-01", ]


#first we look at top 12 played artists over time:
artist_freq <- count(music, "artist")
artist_freq <- artist_freq[order(-artist_freq$freq),]
top_12 <- artist_freq[1:12,]
top_12 <- as.character(top_12$artist)
top_12 <- music[ music$artist %in% top_12, ]
top_12$artist <- factor(top_12$artist)
#table(factor(top_12$artist))



#This operation takes a little time to process. 
artists <- ddply(top_12, c("date" ,"artist"), summarise, count= length(date))
artist_count <- tapply(artists$count, INDEX = artists$artist, sum)

p <- ggplot(data=artists, aes(x=date, y=count)) +
    geom_line() + geom_point() + 
    facet_wrap(~artist, nrow = 4)
    
ggplotly(p)

#let's focus on the year 2015 instead so we can see things a bit more clearly:
music_2015 <- music[music$date >= "2015-01-01" & music$date <= "2015-12-31", ]
artist_freq <- count(music_2015, "artist")
artist_freq <- artist_freq[order(-artist_freq$freq),]
top_12 <- artist_freq[1:12,]
top_12 <- as.character(top_12$artist)
top_12 <- music_2015[ music_2015$artist %in% top_12, ]
top_12$artist <- factor(top_12$artist)    
artists <- ddply(top_12, c("date" ,"artist"), summarise, count= length(date))
#artist_count <- tapply(artists$count, INDEX = artists$artist, sum)

p <- ggplot(data=artists, aes(x=date, y=count)) +
    geom_line() + geom_point() + 
    facet_wrap(~artist, nrow = 4)

ggplotly(p)

#what about most played songs of all time??
song_freq <- count(music, "song")
song_freq <- song_freq[order(-song_freq$freq),]
top_12 <- song_freq[1:12,]
top_12 <- as.character(top_12$song)
top_12 <- music[ music$song %in% top_12, ]
top_12$song <- factor(top_12$song)    
songs <- ddply(top_12, c("date" , "song"), summarise, count= length(date))


p <- ggplot(data=songs, aes(x=date, y=count)) +
    geom_line() + geom_point() + 
    facet_wrap(~song, nrow = 4) 
ggplotly(p)

#what about most played albums of 2015??
album_freq <- count(music_2015, "album")
#Consider removing all albums with no album label
album_freq <- album_freq[order(-album_freq$freq),]
top_12 <- album_freq[1:12,]
top_12 <- as.character(top_12$album)
top_12 <- music_2015[ music_2015$album %in% top_12, ]
top_12$album <- factor(top_12$album)    
albums <- ddply(top_12, c("date" ,"artist", "album"), summarise, count= length(date))

#This command fucks with R since it has to overlay so many plays
p <- ggplot(data=albums, aes(x=date, y=count, color = artist)) +
    geom_line() + geom_point() + 
    facet_wrap(~album, nrow = 4) 
ggplotly(p)

qplot(music_2015$date)



geom_line(music_2015$date)
p <- ggplot(data=music_2015, aes(x=date)) +
    geom_histogram() 
p




#Compare trends in volume by year, starting from 2011:
## this is all still experimental code
music_played <- music[music$date >= "2011-01-01" & music$date <= "2015-12-31", ]


music_played <- ddply(music_played, c("date"), summarise, count= length(date))



music_played$year <- as.factor(format(music_played$date, format = "%Y"))
music_played$month <- as.factor(format(music_played$date, format = "%m"))
music_played$day <- as.factor(format(music_played$date, format = "%d"))
music_played$daymonth <- as.factor(format(music_played$date, format = "%m%d"))


ggplot(data=music_played, aes(x=daymonth, y=count)) +
    geom_line(aes(group = year)) + facet_wrap(~year, nrow = 2)

ggplot(data=music_played, aes(x=daymonth, y=count)) +
    geom_line(aes(group = year)) + facet_wrap(~day, nrow = 2)

    facet_wrap(~artist, nrow = 4)