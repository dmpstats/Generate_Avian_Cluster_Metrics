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

out_dt_nam <- rFunction(data = test_dt$nam)

out_dt_nam
summary(out_dt_nam)

mt_track_data(out_dt_nam)
summary(mt_track_data(out_dt_nam) |> select(-member_tracks_ids ))



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
#' Run for test datasets from different studies

#' ---------------------
#' ---- GAIA

out_dt_gaia <- rFunction(data = test_dt$gaia)

out_dt_gaia
summary(out_dt_gaia)

mt_track_data(out_dt_gaia)
summary(mt_track_data(out_dt_gaia) |> select(-member_tracks_ids ))



out_dt_gaia |> 
  filter(is.na(mean_night_dist))

attributes(out_dt_gaia)
mt_is_time_ordered(out_dt_gaia)

sense_check_map(clust_metrics = out_dt_gaia, loc_dt = test_dt$gaia)



#' ----------------------------
#' ---- WCS

out_dt_wcs <- rFunction(
  data = test_dt$wcs # |> 
  #filter(individual_name_deployment_id == "AW196499..deploy_id.2023814814.")
)

out_dt_wcs
summary(out_dt_wcs)

mt_track_data(out_dt_wcs)
summary(mt_track_data(out_dt_wcs) |> select(-member_tracks_ids ))


sense_check_map(clust_metrics = out_dt_wcs, loc_dt = test_dt$wcs)




mt_track_data(out_dt_wcs) |> 
  print(n = 100)

out_dt_wcs |> 
  filter(is.na(mean_night_dist))



#' ---------------------------------
#' ---- South Africa vfa
out_dt_sa_vfa <- rFunction(data = test_dt$sa_vfa)

out_dt_sa_vfa
summary(out_dt_sa_vfa)

summary(mt_track_data(out_dt_sa_vfa) |> select(-member_tracks_ids ))



sense_check_map(out_dt_sa_vfa, test_dt$sa_vfa)




#' --------------------------------
#' --- Savahn 
out_dt_savahn <- rFunction(data = test_dt$savahn)

out_dt_savahn
summary(out_dt_savahn)


out_dt_sa_vfa |> 
  filter(is.na(mean_night_dist))


#' ---------------------------------
#' ---- Vultures Kendal Tanzania
out_dt_ken_tnz <- rFunction(
  data = test_dt$ken_tnz
)

out_dt_ken_tnz
summary(out_dt_ken_tnz)

summary(mt_track_data(out_dt_ken_tnz) |> select(-member_tracks_ids ))







out_dt_ken_tnz |> 
  filter(is.na(meanvisit_duration))


out_dt_ken_tnz |> 
  filter(is.na(mean_night_dist))



out_dt_ken_tnz |> 
  filter(clust_id == "TZN.351", individual_name_deployment_id == "ST1035A..deploy_id.3056462820.")

test_dt$ken_tnz |> 
  filter(clust_id == "TZN.351", individual_name_deployment_id == "ST1035A..deploy_id.3056462820.") |> 
  as_tibble() |> 
  print(n = 20)




out_dt_ken_tnz |> 
  filter(clust_id == "TZN.11", individual_name_deployment_id == "C181261..deploy_id.3118998707.")

test_dt$ken_tnz |> 
  filter(clust_id == "TZN.11", individual_name_deployment_id == "C181261..deploy_id.3118998707.") |> 
  as_tibble() |> 
  print(n = 20)





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







metrics_out <- readRDS("C:/Users/Bruno/Downloads/Workflow_Instance_001__Generate_Avian_Cluster_Metrics__2024-06-05_16-33-10.rds")

odd_clusts <- mt_track_data(metrics_out) |> filter(prop_days_inactive < 0) |> 
  pull(clust_id)

clust_det_output <- readRDS("C:/Users/Bruno/Downloads/Workflow_Instance_001__Avian_Cluster_Detection__2024-06-05_16-27-25.rds")

rFunction(clust_det_output |>  filter(clust_id %in% odd_clusts))

rFunction(clust_det_output)

