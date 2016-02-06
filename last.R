andreasose <- read.csv("andreas.csv", header = FALSE)



colnames(andreasose) <- c("artist", "album", "song", "date")

andreasose$date <- as.Date(andreasose$date, format = "%d %B %Y")


tre <- andreasose[andreasose$date >= "2010-06-01", ]


trend <- ddply(tre, c("artist"), summarise, count= length(date))
trend <- trend[!is.na(trend$count),]
trend <- trend[!is.na(trend$count),]
trendn <- trend[trend$count >= 500,]
trendn <- as.character(trendn$artist)
trend <- ddply(tre, c("date", "artist"), summarise, count= length(date))
trend <- trend[trend$artist %in% trendn, ]


trend$artist <- factor(trend$artist)







ggplot(data=trend, aes(x=date, y=count)) +
    geom_line(size = 1.5) + geom_point(size = 2.5) + 
    theme(axis.title= element_text(size=rel(3), face = "bold")) +
    theme(strip.text=element_text(size = rel(3), face = "bold")) +
    theme(axis.text = element_text(size = rel(2), face = "bold")) +
    facet_wrap(~artist, nrow = 6)

trend <- as.data.frame(trend)
trend$count <- as.integer(trend$count)
trend$date <- as.Date(trend$date)
tre <- trend

tre <- window(tre, start = as.Date("2011-02-02"), end = as.Date("2013-02-02"))

date <- seq( from = as.Date("2010-01-01"), to = as.Date("2016-02-02"), by = 1)

empty <- zoo(, date)
empty <- zoo(empty)
and <- merge(trend, empty, all=TRUE)


qplot(date, count, data = trend)
mega <-ddply(andreas, c("date","artist"), summarise, count= length(artist))
andreas <- subset(mega, mega$count >= 20)


rodSalg <- tapply(df1$Salg, INDEX = list(weeknd$Aar,df1$Produktgruppe), na.rm = TRUE)
qplot(date, song, data = weeknd)

s <- head(andreasose$date)
betterDates <- as.Date(s,  format = "%d %B %Y")
