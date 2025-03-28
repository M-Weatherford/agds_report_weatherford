library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(checkthat)
library(ggpmisc)

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
stats <- boxplot.stats(air$Ozone, coef = 1.5, do.conf = FALSE)

outliers <- stats$out

air$is_outlier <- colSums(sapply(air$Ozone, "%in%", x = outliers)) > 0

cols <- c("TRUE" = "red", "FALSE" = "blue")

ggplot(air, aes(x=doy,y=Ozone
))+
  geom_point(aes(color = is_outlier))+
  theme_classic()+
  scale_color_manual(values = cols)

ggplot(air, aes(x=Solar.R,y=Ozone
))+
  geom_point(aes(color = is_outlier))+
  theme_c