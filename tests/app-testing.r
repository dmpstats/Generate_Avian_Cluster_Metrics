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

# get App secret key for decrypting test dataset
app_key <- get_app_key()

# Read (encrypted) input datasets for testing
test_dt <- httr2::secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))


set_interactive_app_testing()


# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

#' ----------------------------------------------------
#' Comparison wit previous rFunction implementation
#'
#' NOTE: Newer implementations performs the cluster flattening on top of the
#' common metrics calcumations

### ---- Speed comparison
tictoc::tic()
out_dt_nam <- rFunction(
  data = test_dt$nam #|> slice(1:5000)
  )
tictoc::toc()


tictoc::tic()
out_dt_nam_old <- rFunction_old(
  data = test_dt$nam |> 
    rename(xy.clust = clust_id) |> 
    mt_set_time_column("timestamp_local") #|> slice(1:5000)
) 
tictoc::toc()


### ---- output comparison: events tables [aka track-level cluster metrics)
out_dt_nam_events <- out_dt_nam |> data.frame() |> units::drop_units()

out_dt_nam_old_events <- out_dt_nam_old |> 
  data.frame() |> 
  rename(
    first_dttm_local = firstdatetime,
    last_dttm_local = lastdatetime ,
    n_days_unique = days,
    n_days_span = totalduration,
    n_days_empty = daysempty,
    prop_days_empty = daysemptyprop,
    n_pts = Total,
    meanvisit_duration = meanvisit_time, 
    meanvisit_daytime_duration = meanvisit_time_day,
    mean_n_visits = meanvisits, 
    mean_n_daytime_visits = meanvisits_daytime,
    mean_night_dist = nightdist_med,
    night_prop_250m = near_night_prop,
    mean_arrival_dist = arrivaldists,
    clust_id = xy.clust,
    med_feed_hour_local = MedianHourFeed,
    med_daytime_hour_local = MedianHourDay
  )

event_cols <- intersect(names(out_dt_nam_events), names(out_dt_nam_old_events))
new = out_dt_nam_events |> select(all_of(event_cols)) 
old = out_dt_nam_old_events |> select(all_of(event_cols))

identical(new, old)


### ---- output comparison: track tables [aka whole cluster metrics)
out_dt_nam_trck <- mt_track_data(out_dt_nam) |> units::drop_units() |> data.frame()
out_dt_nam_old_trck <- mt_track_data(out_dt_nam_old) |> 
  rename(
    clust_id = xy.clust,
    member_tracks = birds,
    trks_mindist_m = mindist_m,
    trks_n_within_25km = within_25k,
    trks_n_within_50km = within_50k
  ) |> data.frame()

trck_cols <- intersect(names(out_dt_nam_trck), names(out_dt_nam_old_trck))
new_trks = out_dt_nam_trck |> select(all_of(trck_cols))
old_trks = out_dt_nam_old_trck |> select(all_of(trck_cols))

all.equal(new_trks, old_trks)



#' ----------------------------------------------------
#' Check output options

out_dt <- rFunction(data = test_dt$gaia, cluster_tbl_type = "whole-only")
out_dt

out_dt <- rFunction(data = test_dt$gaia, cluster_tbl_type = "track-and-whole")
out_dt


rFunction(data = test_dt$gaia)


#' ----------------------------------------------------
#' Different column IDs

out_dt <- rFunction(data = test_dt$wcs, cluster_id_col = "yearmonthday")
out_dt

out_dt <- rFunction(data = test_dt$wcs, behav_id_col = "RULE")
out_dt




#' ----------------------------------------------------
#' Check impact on metrics based on local timezones vs UTC timezones

out_dt <- rFunction(
  data = test_dt$ken_tnz
)

out_dt_utc <- rFunction(
  data = test_dt$ken_tnz |> mutate(timestamp_local = timestamp)
)

summary(out_dt$med_feed_hour_local - out_dt_utc$med_feed_hour_local)
summary(out_dt$med_daytime_hour_local - out_dt_utc$med_daytime_hour_local)
summary(out_dt$meanvisit_duration - out_dt_utc$meanvisit_duration )
summary(out_dt$meanvisit_daytime_duration - out_dt_utc$meanvisit_daytime_duration)
summary(out_dt$mean_night_dist - out_dt_utc$mean_night_dist)
summary(out_dt$n_pts_night - out_dt_utc$n_pts_night)
summary(out_dt$SFeeding - out_dt_utc$SFeeding)
summary(out_dt$SRoosting - out_dt_utc$SRoosting)
summary(out_dt$mean_n_daytime_visits - out_dt_utc$mean_n_daytime_visits)
summary(out_dt$mean_arrival_dist - out_dt_utc$mean_arrival_dist)



#' ----------------------------------------------------
#' Run for a test datasets from different studies

#' ---------------------
#' ---- GAIA

out_dt_gaia <- rFunction(data = test_dt$gaia)
out_dt_gaia
attributes(out_dt_gaia)

sense_check_map(clust_metrics = out_dt_gaia, loc_dt = test_dt$gaia)



#' ----------------------------
#' ---- WCS

out_dt_wcs <- rFunction(
  data = test_dt$wcs #|>  
    #filter(individual_name_deployment_id == "AW196499..deploy_id.2023814814.")
)

sense_check_map(clust_metrics = out_dt_wcs, loc_dt = test_dt$wcs)



#' ---------------------------------
#' ---- South Africa vfa
out_dt_sa_vfa <- rFunction(
  data = test_dt$sa_vfa
)

out_dt_sa_vfa

sense_check_map(out_dt_sa_vfa, test_dt$sa_vfa)


#' --------------------------------
#' --- Savahn 
out_dt_savahn <- rFunction(
  data = test_dt$savahn
)

out_dt_savahn


#' ---------------------------------
#' ---- Vultures Kendal Tanzania
out_dt_ken_tnz <- rFunction(
  data = test_dt$ken_tnz
)




# ---------------------------------------- #
# ----            SDK Testing           ----
# ---------------------------------------- #
run_sdk(data = test_dt$wcs)
read_rds("data/output/output.rds")


run_sdk(data = test_dt$sa_vfa, cluster_tbl_type = "whole-only")
read_rds("data/output/output.rds")


run_sdk(data = test_dt$sa_vfa, behav_col = "uydyd")

run_sdk(data = test_dt$savahn, behav_col = NULL)



# -------------------------------------------- #
# ----   Interactive calcGMedianSF testing  ----
# -------------------------------------------- #

mock_dt <- mt_sim_brownian_motion(tracks = 5, t = 1:50) |> 
  st_set_crs(4421) |> 
  mutate(clust_id = c("Z", sample(LETTERS[1:3], size = n()-1, replace = TRUE)))

mock_clustbl <- mock_dt |> 
  group_by(track, clust_id) |> 
  summarise(clust_points = st_combine(geometry))

st_geometry_type(mock_clustbl, by_geometry = FALSE)

bench::mark(check = FALSE,
  calcGMedianSF(mock_clustbl),
  calcGMedianSF_new(mock_clustbl)
)


