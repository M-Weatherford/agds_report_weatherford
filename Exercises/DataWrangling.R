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


half_hourly_fluxes <- read_csv("Data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")

half_hourly_fluxes <- select(
  half_hourly_fluxes,
  starts_with("TIMESTAMP"),
  ends_with("_F"),
  GPP_NT_VUT_REF,
  NEE_VUT_REF_QC,
  starts_with("SWC_F_MDS_"),
  -contains("JSB"),
  NIGHT
)

#class(half_hourly_fluxes$TIMESTAMP_START[[1]])
#as.character(half_hourly_fluxes$TIMESTAMP_START[[1]])

#dates <- ymd_hm(half_hourly_fluxes$TIMESTAMP_START)
#dates[1]

#nextday <- dates + days(1)
#nextday[1]

#month(dates[1])

half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate( TIMESTAMP_START = ymd_hm(TIMESTAMP_START),TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  rename(ts_start = TIMESTAMP_START, ts_end = TIMESTAMP_END)

plot(
  half_hourly_fluxes[1:(2*24),]$ts_start,
  half_hourly_fluxes[1:(2*24),]$SW_IN_F,
  type = "l"
)

plot(
  half_hourly_fluxes[1:(365*2*24),]$ts_start,
  half_hourly_fluxes[1:(365*2*24),]$SW_IN_F,
  type = "l"
)

half_hourly_fluxes %>%
  mutate(year = year(ts_start),
         month = month(ts_start),
         doy = yday(ts_start)) %>%
  select(ts_start,ts_end,year,month,doy)

daily_fluxes <- half_hourly_fluxes %>%  
  mutate(date = as_date(ts_start)) %>%  # converts the ymd_hm-formatted date-time object to a date-only object (ymd)
  group_by(date) %>% 
  summarise(SW_IN_F = mean(SW_IN_F))

plot(daily_fluxes[1:365,]$date, daily_fluxes[1:365,]$SW_IN_F, type='l')

daily_fluxes <- half_hourly_fluxes %>%
  mutate(date = as_date(ts_start)) %>%
  group_by(date) %>%
  summarise(GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF, na.rm = TRUE),
            n_datapoints = n(), #Number of observations per day
            n_measured = sum(NEE_VUT_REF_QC == 0), #Number of actually measured data (NEE_VUT_REF_QC is a quality measurement)
            SW_IN_F = mean(SW_IN_F, na.rm = TRUE), 
            .groups = 'drop'   # to un-group the resulting data frame
            ) %>%
  mutate(f_measure = n_measured/ n_datapoints) #Calculates the proportion of actually measured datapoints
write_csv(daily_fluxes, file = "data/daily_fluxes.csv")


half_hourly_fluxes %>%
  mutate(date = as_date(ts_start)) %>%
  group_by(date) %>%
  nest()

half_hourly_fluxes |> 
  +     select(ts_start, starts_with("SWC_F_MDS_")) |> 
  +     head()

half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(across(where(is.numeric), ~na_if(.,-9999))) #replacing -9999 with NA which is what it represents

half_hourly_fluxes %>%
  select(ts_start, starts_with("SWC_F_MDS_")) %>%
  head()

visdat::vis_miss(
  half_hourly_fluxes |> slice(1:10000),
  cluster = FALSE, 
  warn_large_data = FALSE
)

half_hourly_fluxes |> 
  mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC %in% c(0,1), GPP_NT_VUT_REF, NA))

write_csv(half_hourly_fluxes, file = "data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")

vec_files <- list.files("./data", pattern = "_FLUXNET2015_FULLSET_DD_", full.names = TRUE)
print(vec_files)

list_df <- purrr::map(as.list(vec_files), ~read_csv(.)) #reading in all the csv files and combining them into one large list
names(list_df) <- vec_files  # this makes it a named list

# function definition
clean_data_dd <- function(df){
  
  df <- df |>
    
    # select only the variables we are interested in
    dplyr::select(
      TIMESTAMP,
      ends_with("_F"),
      GPP_NT_VUT_REF,
      NEE_VUT_REF_QC,
      starts_with("SWC_F_MDS_"),
      -contains("JSB")) |> 
    
    # convert to a nice date object
    dplyr::mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) |>
    
    # set all -9999 to NA
    dplyr::mutate(across(where(is.numeric), ~na_if(., -9999)))
  
  return(df)
}


list_df <- purrr::map(list_df, ~clean_data_dd(.))

daily_fluxes_allsites <- bind_rows(list_df, .id = "siteid")
daily_fluxes_allsites

# create a subset of the data
daily_fluxex_subset <- daily_fluxes_allsites |>
  slice(1:10000)

# visualize missing data
visdat::vis_miss(
  daily_fluxex_subset,
  cluster = FALSE,
  warn_large_data = FALSE
)

vec_sites <- str_sub(vec_files, start = 12, end = 17) #Extracting the site name from the siteid string
head(vec_sites)

daily_fluxes_allsites <- daily_fluxes_allsites %>%
  mutate(siteid = str_sub(siteid, start = 12, end = 17))

daily_fluxes_allsites

list_linmod <- purrr::map(list_df, ~lm(GPP_NT_VUT_REF ~ SW_IN_F, data = .)) #Fitting a linear regression across all data sets using the purrr map

list_linmod |>
  purrr::map(summary) |>  # apply the summary() function to each list element
  map_dbl("r.squared")    # extract R-squared from the list generated by summary()

tibble::tibble(
  siteid = vec_sites,
  linmod = list_linmod
)


tibble::tibble(
  siteid = vec_sites,
  data = list_df
)

daily_fluxes_allsites |>
  group_by(siteid) |>
  nest()

daily_fluxes_allsites |>
  group_by(siteid) |>
  nest() |>
  dplyr::mutate(linmod = purrr::map(data, ~lm(GPP_NT_VUT_REF ~ SW_IN_F, data = .)))

daily_fluxes_allsites_nested <- daily_fluxes_allsites |> #Combinging all the steps above
  group_by(siteid) |>
  nest() |>
  dplyr::mutate(linmod = purrr::map(data, ~lm(GPP_NT_VUT_REF ~ SW_IN_F, data = .))) |>
  dplyr::mutate(summ = purrr::map(linmod, ~summary(.))) |>
  dplyr::mutate(rsq = map_dbl(summ, "r.squared")) |>
  arrange(desc(rsq))  # to arrange output, with highest r-squared on top

daily_fluxes_allsites_nested

base::load("data/siteinfo_fluxnet2015.rda")  # loads siteinfo_fluxnet2015

daily_fluxes_allsites_nested_joined <- siteinfo_fluxnet2015 |>
  rename(siteid = sitename) |>
  right_join(
    select(daily_fluxes_allsites_nested, -linmod, -summ, -rsq),
    by = "siteid"
  )

daily_fluxes_allsites_joined <- siteinfo_fluxnet2015 |>
  rename(siteid = sitename) |>
  right_join(
    daily_fluxes_allsites,
    by = "siteid"
  )

print(paste("Flat and joined:", 
            format(object.size(daily_fluxes_allsites_joined),  
                   units = "auto", 
                   standard = "SI")))

print(paste("Nested and joined:", 
            format(object.size(daily_fluxes_allsites_nested_joined),  
                   units = "auto", 
                   standard = "SI")))

# save for later use
write_rds(
  daily_fluxes_allsites_nested_joined,
  file = "data/daily_fluxes_allsites_nested_joined.rds"
)
