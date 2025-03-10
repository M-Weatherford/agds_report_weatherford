library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

soilcarbon<-read_csv("./Data/SoilCarbonData_CLEANED.csv")

soilcarbon$log_response <- log(soilcarbon$mean_increased_CO2/soilcarbon$mean_ambient_CO2)

soilcarbon <- soilcarbon %>%
  mutate(Phase = case_when(Time_years < 3 ~ "early",
                           Time_years >=3 & Time_years <=6 ~ "mid",
                           Time_years > 6 ~ "late"))

soilcarbonSummary <- soilcarbon %>%
  group_by(Phase) %>%
  summarise(mean_ambient_CO2=mean(mean_ambient_CO2),
            mean_elevated_CO2=mean(mean_increased_CO2),
            mean_log_response = mean(log_response))%>%
  arrange(mean_elevated_CO2)
  
