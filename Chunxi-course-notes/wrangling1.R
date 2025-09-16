library(tidyverse)
library(skimr)

# read in corona .csv
coronavirus <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv')

summary(coronavirus)
skim(coronavirus)
View(coronavirus)
head(coronavirus)
tail(coronavirus)

head(coronavirus$ases)

filter(coronavirus,cases>0)
filter(coronavirus,country=="US")
filter(coronavirus,country!="US")
filter(coronavirus,country=="US"|country=="Canada")
filter(coronavirus,country=="US" & type=="death")
filter(coronavirus,country=="US", type=="death")

filter(coronavirus, country %in% c("US", "Canada"))

#exersice 1
filter(coronavirus,country%in% c("Demark", "Italy","Spain"),type="death", data="2021-09-16")
