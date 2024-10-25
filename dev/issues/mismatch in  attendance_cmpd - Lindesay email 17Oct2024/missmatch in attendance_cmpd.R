# ------------------------- #
#         Preamble
# ------------------------- #

library(move2)
library(httr2)
library(purrr)
library(readr)
library(sf)
#library(ggplot2)
library(tmap)

# Helpers
source("tests/app-testing-helpers.r")

# Read (encrypted) input datasets for testing
dt <- read_rds("dev/issues/mismatch in  attendance_cmpd - Lindesay email 17Oct2024/combine_split_birds_for_kendall_and_wcs__Avian_Cluster_Detection__2024-09-26_10-59-50.rds")
  
  
# c("NCZ.20", "NCZ.61", "NCZ.78", "NCZ.75", "NCZ.108", "NCZ.366", "NCZ.401", "NCZ.425", "NCZ.478", "NCZ.500")

set_interactive_app_testing()




# ------------------------

test <- dt |> 
  mutate(
    date_local = date(with_tz(timestamp, first(local_tz))),
    # the hour element of local time
    hour_local = hour(with_tz(timestamp, first(local_tz))),
    # decimal hours of the local calender day
    dec_hrs_local = hour_local + 
      minute(with_tz(timestamp, first(local_tz)))/60 +
      second(with_tz(timestamp, first(local_tz)))/3600,
    .by = local_tz
  ) 





test <- dt %>% 
    mutate(
      hour_local = lubridate::hour(timestamp_local),
      date_local = lubridate::date(timestamp_local),
      #timediff_hrs = mt_time_lags(., units = "hours"),
      .by = local_tz
    )



test2 <- test |> 
  mutate(
    # the hour element of local time
    #hour_local = lubridate::hour(dttm_local),
    hour_local = if_else(nchar(dttm_local) > 10, as.integer(substr(dttm_local, 12, 13)), 0),
    min_local = if_else(nchar(dttm_local) > 10, as.integer(substr(dttm_local, 15, 16)), 0),
)









# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #


dt_case1 <- dt |> 
  filter(
    individual_local_identifier == "A151401",
    #yearmonthday %in% c("20150923", "20150924", "20150925", "20150926", "20150927", "20150928")
  ) #|> 
  #select(-c(
  #  import_marked_outlier, index, hour, min, secs, hourmin, sensor_type_id, activity_count,
  #  barometric_pressure, cpu_temperature, external_temperature, gps_satellite_count, 
  #  ground_speed, height_above_ellipsoid, location_error_numerical, tag_voltage, 
  #  underwater_time, visible, day, month, year, x, y
  #))

print(dt_case1, n = 100)

  
out_dt <- rFunction(
  data = dt_case1, 
  cluster_tbl_type = "whole-only"
)

out_dt




out_dt <- rFunction(
  data = dt, 
  cluster_tbl_type = "whole-only"
)



out_dt$attendance_cmpd != c(out_dt$attendance_SFeeding_cmpd + out_dt$attendance_SResting_cmpd + out_dt$attendance_SRoosting_cmpd)


out_dt$attendance_cmpd[49] - c(out_dt$attendance_SFeeding_cmpd + out_dt$attendance_SResting_cmpd + out_dt$attendance_SRoosting_cmpd)[49]


out_dt |> 
  mutate(
    
    #check_attnd = attendance_cmpd - c(attendance_SFeeding_cmpd + attendance_SResting_cmpd + attendance_SRoosting_cmpd)
  ) |> 
  pull(check_attnd)

