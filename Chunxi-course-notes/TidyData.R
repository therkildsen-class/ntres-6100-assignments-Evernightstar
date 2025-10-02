library(tidyverse)

table1

table2

table4a
table4b

table1 |> ggplot(mapping=aes(x=year,y=cases))+
  geom_line()

table4a |> 
  pivot_longer(c('1999','2000'),names_to = "year",values_to = "cases")

table4b |> 
  pivot_longer(c('1999','2000'),names_to = "year",values_to = "poplation")

table4a_tidy<-table4a |> 
  pivot_longer(c('1999','2000'),names_to = "year",values_to = "cases")

table2 |> 
  pivot_wider(names_from = type,values_from = count)

table5<-table3 |> 
  separate(year,into=c("century","year"),sep=2)

table3 |> 
  separate(year,into=c("century","year"),sep = "/",convert = TRUE)

table5 |> 
  unit(fullyear,century,year,)





coronavirus <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv')

View(coronavirus)

coronavirus |> 
  filter(country=="US",cases>=0) |> 
  ggplot()+
  geom_line(aes(x=date,y=cases,color=type))

coron_wide<-coronavirus |> 
  pivot_wider(names_from=type,values_from=cases) |> 
  View()

coronavirus_ttd <- coronavirus |> 
  group_by(country, type) |>
  summarize(total_cases = sum(cases)) |>
  pivot_wider(names_from = type, values_from = total_cases)

# Now we can plot this easily
ggplot(coronavirus_ttd) +
  geom_label(mapping = aes(x = confirmed, y = death, label = country))
  