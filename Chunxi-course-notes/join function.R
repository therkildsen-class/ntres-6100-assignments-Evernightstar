library(tidyverse)
install.packages("nycflights13")
library(nycflights13)


fship <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Fellowship_Of_The_Ring.csv")

ttow <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Two_Towers.csv")

rking <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Return_Of_The_King.csv")

lotr<-bind_rows(fship,ttow,rking)

fship_no_female<-fship |> 
  select(Male,Film,Race)

bind_rows(fship_no_female,ttow,rking)


#join functions # -----------------------------------------------------------------------

planes
planes |> 
  count(tailnum)

planes |> 
  count(year) |> 
  tail()

weather
View(weather)

weather |> 
  count(time_hour,origin) |> 
  filter(n>1)

planes |> 
  filter(is.na(tailnum)) |> 
  View()

flights2 <- flights |> 
  select(year:day, hour, origin, dest, tailnum, carrier)
View(flights2)

flights2 |> 
  left_join(airlines)

flights2 |> 
  left_join(weather)

flights2 |> 
  left_join(planes,join_by(tailnum),suffix = c("_flight","_planes"))

airports

flights2 |> 
  left_join(airports,join_by(origin==faa))

airports2<-airports |> 
  select(faa,name,lat,lon)
flights2 |> 
  left_join(airports2,join_by(origin==faa)) |> 
  left_join(airports2,join_by(dest==faa),suffix=c("_origin","_dest"))

