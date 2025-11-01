library(tidyverse)

parcels <- read.csv('D:/R program/ntres-6100-assignments-Evernightstar/data/Parcels_FLD.csv')

#tidy the data
parcels_clean <- parcels  |> 
  select(-SWRESNAME,-SWRESCODE,-SWRESUNITS,-SWAPTNAME,-SWAPTCODE,-SWAPTUNITS,-GISNOTES,
         -ZONE_SUBTY,-VEL_UNIT, -AR_REVERT,-AR_SUBTRV,-LOC,-FRONTAGE,-DEPTH,-MAIL_2ADDR,-MAIL_STATE) |> 
  filter(MAIL_CITY=='Ithaca') |> 
  mutate(
    sale_price_valid = ifelse(SALE_PRICE > 100, SALE_PRICE, NA)
  )
View(parcels_clean)
dim(parcels_clean)  

#data distribution
parcels_clean|> filter(SEWER=='Comm/public', WATER=='Comm/public') |>
  group_by(LOCALITY,Urb_Rural) |> 
  summarize(
    n=n(),.groups = 'drop'
  ) |>ggplot() +
  geom_col(aes(x = LOCALITY, y = n, fill=Urb_Rural))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# select parcels for mobile house 
parcels_house<-parcels_clean|> 
  filter(SEWER=='Comm/public', 
         WATER=='Comm/public',
         distance>2500,
         ACRES >= 0.25, ACRES <= 5 ) |> 
  arrange(ACRES, sale_price_valid)

parcels_house |> summarise(
  count = n(),
  avg_acres = mean(ACRES),
  median_price = median(sale_price_valid, na.rm = TRUE))
  