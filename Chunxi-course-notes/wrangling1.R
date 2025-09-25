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

#exercise 1
filter(coronavirus,country%in% c("Frence", "Italy","Spain"),type=="death", date=="2021-09-16")
View(count(coronavirus, country))

select(coronavirus,type, cases,date,uid)
select(coronavirus,-province)

#exercise 2

select(coronavirus,country, lat, long)
select(coronavirus,1:3)

select(coronavirus, data:cases)
select(coronavirus,contains("y"),everything())

#coronavirus_us<-filter(coronavirus,country=="US")
#coronavirus_us2<-select(coronavirus_us,-lat,-long,-province)

coronavirus |>
  filter(country=="US") |>
  select(-lat,-long,-province)

#exercise 3
coronavirus |> 
  filter(type=="death",country=="US"|country=="Canada"|country=="Mexico") |> 
  select(country,date,cases) |> 
  ggplot()+
  geom_line(mapping=aes(x=date, y=cases,colour = country))

coronavirus |> 
  count(country) |> 
  view() 


#vaccine data-------------------------------------------------------------------

vacc <- read_csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/main/csv/covid19_vaccine.csv")

View(vacc)
max(vacc$date)

vacc |> 
  filter(date == max(date)) |> 
  select(country_region, continent_name, people_at_least_one_dose, population) |> 
  mutate(vaxxrate = round(people_at_least_one_dose / population, 2))

vacc |> 
  filter(doses_admin>200000000) |> 
  select(country_region, continent_name, people_at_least_one_dose, doses_admin) |> 
  mutate(doses_perperson=doses_admin/people_at_least_one_dose) |> 
  filter(doses_perperson>3) |> 
  arrange(-doses_perperson)

vacc |> 
  filter(date == min(date)) |> 
  select(country_region,people_at_least_one_dose,population) |> 
  mutate(vaxxrate = round(people_at_least_one_dose / population, 2)) |> 
  filter(vaxxrate>0.9)
  arrange(-vaxxrate) |> 
  head(5)
  
  

  
#Summarize function----------------------------------------------------------------

coronavirus |>
    filter(type=="confirmed") |> 
    summarize(total=sum(cases)) |> 
    arrange(-total)

coronavirus |> 
  group_by(date,type) |> 
  summarize(total=sum(cases)) |> 
  filter(date=="2023-01-03")

coronavirus |> 
  filter(type=="death") |> 
  group_by(date) |> 
  summarize(total=sum(cases)) |> 
  arrange(-total)

gg_base<-coronavirus |> 
  group_by(date) |> 
  filter(type=="confirmed") |>
  summarize(total_case=sum(cases)) |> 
  ggplot(mapping=aes(x=date,y=total_case))

gg_base+geom_line()

gg_base+
  geom_point()

gg_base+
  geom_col(fill="blue")

gg_base+
  geom_area(fill="red")

gg_base+
  geom_line(
    color="purple",
    linetype="dashed"
  )

gg_base+
  geom_point(
    color="purple",
    shape=20,
    size=5,
    alpha=0.4
  )

gg_base+
  geom_point(
    mapping = aes(size=total_case,color=total_case),
    alpha=0.5
  )+
  theme_minimal()+
  theme(legend.background = element_rect(
    fill = "yellow",
    color="grey90",
    linewidth=0.7
  ))
#theme(legend.position = none)
  
  gg_base+
    geom_point(
      mapping = aes(size=total_case,color=total_case),
      alpha=0.5
    )+
    theme_minimal()+
    labs(
      x="date",
      y="Total confirmed cases",
      title = str_c("Daily counts of new coronavirus cases",max(coronavirus$date)),
      subtitle="Global Sums"
    )

top5<-coronavirus |> 
  group_by(country) |> 
  filter(type=="confirmed") |>
  summarize(total=sum(cases)) |> 
  arrange(-total) |> 
  head(5) |> 
  pull(country)

top5 |> 
  group_by(date,country) |> 
  filter(type=="confirmed") |>
  summarize(total=sum(cases)) |>
  ggplot()+
  geom_line(mapping=aes(x=date,y=total,color=country))

coronavirus |> 
  filter(type=="confirmed",country%in%top5,cases>=0) |>
  group_by(date,country) |> 
  summarize(total=sum(cases)) |>
  ggplot()+
  geom_line(mapping=aes(x=date,y=total,color=country))+
  facet_wrap(~country,ncol=1)

coronavirus |>
  filter(type == "confirmed") |>
  group_by(country) |>
  summarize(total = sum(cases)) |>
  arrange(-total) |>
  head(5) |>
  pull(country)
