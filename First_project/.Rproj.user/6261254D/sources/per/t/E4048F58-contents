#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)
setwd("flats.csv")
flats <- read.csv("flats.csv", stringsAsFactors=FALSE, encoding="UTF-8")
class(flats)
str(flats)
?read.csv

flats <- read.csv("flats.csv", stringsAsFactors = FALSE,encoding = "UTF-8", dec = ",")
str(flats)
dim(flats)
head(flats, 10)
head(flats, 3)
tail(flats, 5)

summary(flats)
glimpse(flats)
count(flats, Місто)

flats %>%
  count(Місто) %>%
  arrange(n)

flats %>%
  filter(Місто != "Києво-Святошинський") %>%
  filter(Кімнат == 3) %>%
  count(Місто) %>%
  arrange(desc(n))

flats %>%
  filter(Місто != "Києво-Святошинський")%>%
  filter(Кімнат == 2)%>%
  summarise(mean(Загальна_площа), sd(Загальна_площа))

flats %>%
  filter(Місто != "Києво-Святошинський")%>%
  filter(Кімнат == 1)%>%
  group_by(Місто)%>%
  summarise(mean=mean(Загальна_площа), sd=sd(Загальна_площа))

flats %>%
  filter(Місто == "Одеса")%>%
  filter(Кімнат == 3)%>%
  count(Місто)

flats %>%
  filter(Місто == "Львів")%>%
  filter(Кімнат == 1)%>%
  #count(Місто)%>%
  summarise(median=median(Загальна_площа), mean=mean(Загальна_площа))


ggplot(flats, aes(x=Кімнат)) +
  geom_bar(fill="lightblue",
           col="grey") +
  ylab('Кількість')

p <- ggplot(flats, aes(x=Загальна_площа)) +
  geom_bar(fill="blue",
           col="grey") +
  ylab("Кількість")
p

p2 <- ggplot(flats, aes(x=Загальна_площа)) +
  geom_histogram(breaks=seq(0, 250, by=25),
                 fill="blue",
                 col="grey") +
  ylab("Кількість")
p2

price_for_square <- ggplot(flats, aes(x=Загальна_площа, y=Ціна)) +
  geom_point(fill="blue")
price_for_square

price <- ggplot(flats, aes(x=Ціна)) +
  geom_histogram(breaks=seq(0,3000000, by=200000),
           fill="green", 
           col="grey")
price
?geom_histogram

price_of_square <- ggplot(flats, aes(x=Загальна_площа, y=Ціна)) +
  geom_point()
price_of_square

price_for_rooms <- ggplot(flats, aes(Кімнат, Ціна)) +
  geom_boxplot(aes(group=cut_width(Кімнат, 1))) +
  coord_flip()
price_for_rooms

flats_3_Odesa <- flats%>%
  filter(Місто == "Одеса")%>%
  filter(Кімнат == 3)%>%
  count(Місто)
flats_3_Odesa

median_square_lviv <- flats%>%
  filter(Місто == "Львів")%>%
  filter(Кімнат == 1)%>%
  summarise(median=median(Загальна_площа))
median_square_lviv
