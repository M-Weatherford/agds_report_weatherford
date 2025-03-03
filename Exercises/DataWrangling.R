library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

starwars<-dplyr::starwars

##How many pale characters come from the planets Ryloth and Naboo?
nrow(dplyr::filter(starwars, skin_color=="pale", homeworld %in% c("Naboo","Ryloth")))

##Who is the oldest among the tallest thirty characters?

print(dplyr::slice_max(dplyr::slice_max(starwars, order_by=height, n=30), order_by=mass, n=1)$name)

##What is the name of the smallest character and their starship in “Return of the Jedi”

print(slice_min(dplyr::filter(unnest(starwars,films),films =="Return of the Jedi"),order_by = height,n=1)$name)
