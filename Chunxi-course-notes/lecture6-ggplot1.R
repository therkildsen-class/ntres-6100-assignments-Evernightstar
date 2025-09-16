library(tidyverse)

mpg 

?mpg

?cars
cars

View(mpg)

head(cars,4)
tail(cars)

ggplot(data=mpg)+
    geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=class,y=drv))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class, size=cyl),shape=2)+
  geom_smooth(mapping = aes(x=displ,y=hwy))+
  facet_wrap(~ year,nrow=2)

?geom_point

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

#excersize 4
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
  facet_wrap(~ class)
