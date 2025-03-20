library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(checkthat)

air<-datasets::airquality

#dataset is air quality measurements from New York

air$Date<-as.Date(with(air,paste(Month,Day,sep="-")),"%m-%d")

air$doy <- yday(air$Date)

air <- air %>%
  select(Ozone,
         Solar.R,
         Wind,
         Temp,
         doy)


ggplot(air, aes(x=doy,y=Ozone
))+
  geom_point()+
  theme_classic()+
  geom_smooth(formula = y ~ x, method = "lm", color = "red", se = FALSE)

ggplot(air, aes(x=Solar.R,y=Ozone
))+
  geom_point()+
  theme_classic()+
  geom_smooth(formula = y ~ x, method = "lm", color = "red", se = FALSE)

ggplot(air, aes(x=Wind,y=Ozone
))+
  geom_point()+
  theme_classic()+
  geom_smooth(formula = y ~ x, method = "lm", color = "red", se = FALSE)

