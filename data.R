fish <- read_csv("fish.csv") %>%
  drop_na() %>%
  mutate(
    wbic = as.factor(wbic), # Convert characters to factors
    lake_type = as.factor(lake_type),
    clarity = as.factor(clarity),
    yoy_per_acre = yoy_catch/size_acre,
    yoy_per_km = yoy_per_meter*1000, # Convert m to km
    recruit_yoy = ifelse(yoy_per_km>6.2, 1, 0), # Success binary
    recruit_yoy = as.factor(recruit_yoy), # Make success a factor
    temp_survey_c = ((temp_survey-32)*(5/9)) # F to C
  ) %>% 
  filter(clarity!="Very Low", # Remove one "Very Low" clarity obs
         wbic!=2294900#, # Remove TURTLE-FLAMBEAU FL because it is
         # an outlier
  )

fish_sc <- fish %>%
  mutate(log_km_surv = log(meters_surv/1000),
         f_year = as.factor(year),
         s_year = (year-min(year)),
         log_size_acre = log(size_acre)) %>%
  mutate_at(.vars=c("temp_survey",
                    "gdd_wtr_5c"), .funs = scale)

ft <- fish %>%
  mutate(size_acre=log(size_acre),
         f_year = as.factor(year),
         s_year = (year-(min(year)))) %>%
  mutate_at(.vars=c("temp_survey",
                    "gdd_wtr_5c"), .funs = scale)

fish_nz <- fish %>%
  filter(yoy_catch!=0) %>%
  mutate(log_km_surv = log(meters_surv/1000),
         km_surv = meters_surv/1000,
         f_year = as.factor(year),
         s_year = year-(min(year)+1))