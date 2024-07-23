library(move2)
library(withr)
library(dplyr)
library(rlang)
library(httr2)
library(readr)
library(lubridate)
library(sf)
library(here)

if(rlang::is_interactive()){
  library(testthat)
  source("tests/app-testing-helpers.r")
  set_interactive_app_testing()
  app_key <- get_app_key()
}

# Load test data 
input3 <- read_rds(test_path("data/input3_move2loc_LatLon.rds"))
test_sets <- test_path("data/vult_unit_test_data.rds") |> 
  httr2::secret_read_rds(key = I(app_key)) 

# Test data pre-processing
test_sets <- map(test_sets, \(dt){
  dt <- dt %>%
    mutate(
      hour_local = lubridate::hour(timestamp_local),
      date_local = lubridate::date(timestamp_local),
      timediff_hrs = mt_time_lags(., units = "hours")
    )
})
  


# test_cluster_sets <- map(test_sets[1:2], \(dt, cluster_id_col = "clust_id"){
#   
#   browser()
#   
#   trk_id_col <- mt_track_id_column(dt)
#   
#   clust_tbl <- dt |>
#     dplyr::filter(!is.na(.data[[cluster_id_col]])) %>%
#     dplyr::group_by(.data[[cluster_id_col]], .data[[trk_id_col]]) |>
#     summarise(
#       first_dttm_local = min(timestamp_local),
#       last_dttm_local = max(timestamp_local),
#       .groups = "drop"
#     ) %>%
#     # Calculate track-level geometric medians in each cluster
#     dplyr::mutate(median_point = calcGMedianSF(.))
#   
#   wholeclusts <- clust_tbl |> 
#     summarise(
#     clust_track_meds = st_combine(median_point),
#     .by = all_of(cluster_id_col)
#   ) %>%
#     mutate(clust_centroid = calcGMedianSF(.)) |> 
#     st_set_geometry("clust_centroid") |> 
#     select(-clust_track_meds) |> 
#     as_tibble()
#   
#   clust_tbl <- clust_tbl %>% left_join(wholeclusts, by = cluster_id_col)
#   
# })




# Main rFunction -----------------------------------------

test_that("output is a valid move2 object", {
  actual <- rFunction(data = test_sets$wcs)
  # passes {move2} check
  expect_true(move2::mt_is_move2(actual))
  # check if 1st class is "move2"
  expect_true(class(actual)[1] == "move2")
})



test_that("input validation is doing it's job correctly", {

  # specified cluster ID column
  expect_error(
    rFunction(data = test_sets$nam, cluster_id_col = "NONEXISTENT"),
    "Specified column name `NONEXISTENT` must be present in the input data."
  )

  expect_error(
    rFunction(data = test_sets$nam, cluster_id_col = 3),
    "Parameter 'Cluster ID Column' \\(`cluster_id_col`\\) must be a string."
  )

  expect_error(
    rFunction(data = test_sets$nam, cluster_id_col = NULL),
    "Parameter 'Cluster ID Column' \\(`cluster_id_col`\\) must be a string, not `NULL`."
  )

  # behav_col
  expect_error(
    rFunction(data = test_sets$nam, behav_col = "NONEXISTENT"),
    "Specified column name `NONEXISTENT` must be present in the input data."
  )


  expect_error(
    rFunction(data = test_sets$wcs |> select(-behav), behav_col = "behav"),
    "Specified column name `behav` must be present in the input data"
  )

  expect_error(
    rFunction(data = test_sets$wcs, behav_col = "dist_m"),
    class = "invalid-behav-col-class"
  )


  # invalid specification of cluster table type in output
  expect_error(
    rFunction(data = test_sets$wcs, cluster_tbl_type = "WRONG_TYPE_SPEC"),
    "`cluster_tbl_type` must be one of \"track-and-whole\" or \"whole-only\", not \"WRONG_TYPE_SPEC\"."
  )

  # invalid specification of cluster table type in output
  expect_error(
    rFunction(data = test_sets$wcs, output_type = "WRONG_OUTPUT_TYPE"),
    "`output_type` must be one of \"cluster-based\" or \"merge-to-locs\", not \"WRONG_OUTPUT_TYPE\"."
  )
  
  # invalid specification for type of output
  expect_error(
    rFunction(data = test_sets$wcs, cluster_tbl_type = "WRONG_TYPE_SPEC"),
    "`cluster_tbl_type` must be one of \"track-and-whole\" or \"whole-only\", not \"WRONG_TYPE_SPEC\"."
  )
  

  # input data missing required timestamp_local
  expect_error(
    rFunction(data = test_sets$wcs |> select(-timestamp_local)),
    "Input data must contain column `timestamp_local`"
  )

  # input data missing required sun times when nightpoint is not available in input data
  expect_error(
    rFunction(data = test_sets$wcs |> select(-c(nightpoint, sunset_timestamp, sunrise_timestamp))),
    "Input data must contain columns `sunset_timestamp` and `sunrise_timestamp`."
  )

})



test_that("Option for type of ouputted cluster table works as expected", {

  # "whole-only"
  actual <- rFunction(data = test_sets$wcs |> slice(1:500), cluster_tbl_type = "whole-only")
  expect_equal(attributes(actual)$clust_dt_type, "whole-only")
  expect_equal(ncol(mt_track_data(actual)), 1)

  # "track-and-whole"
  actual <- rFunction(data = test_sets$wcs |> slice(1:500), cluster_tbl_type = "track-and-whole")
  expect_equal(attributes(actual)$clust_dt_type, "track-and-whole")
  expect_gt(ncol(mt_track_data(actual)), 1)

})



testthat::test_that("Option `output_type` is working as expected", {
  
  # if "merge-to-locs" selected, output and input should have the same number of rows 
  dt <- test_sets$wcs |> slice(1:300)
  actual <- rFunction(data = dt, output_type = "merge-to-locs")
  expect_equal(nrow(actual), nrow(dt))
  
  # if "merge-to-locs" selected, selection correctly stored in output
  expect_equal(attributes(actual)$clust_dt_type, "whole-binned-to-locs")
  
})



test_that("Optional specification of behavioural column works as expected", {

  behav_cols_event <- c("SFeeding_drtn", "SResting_drtn", "SRoosting_drtn")
  behav_cols_track <- c("SFeeding_drtn_cmpd", "SResting_drtn_cmpd", "SRoosting_drtn_cmpd")

  actual <- rFunction(data = test_sets$wcs |> slice(1:100), behav_col = "behav")
  expect_contains(colnames(actual), behav_cols_event)
  expect_contains(colnames(mt_track_data(actual)), behav_cols_track)
  expect_false("STravelling" %in% colnames(actual))

  actual <- rFunction(data = test_sets$wcs |> slice(1:100), behav_col = NULL)
  expect_false(any(behav_cols_event %in% colnames(actual)))
  expect_false(any(behav_cols_track %in% colnames(mt_track_data(actual))))

})




test_that("Expected main app outcome has not changed", {

  testthat::local_edition(3)

  # WCS
  expect_snapshot_value(
    rFunction(test_sets$wcs) |>
      as_tibble(),
    style = "json2"
  )

  # Namibia
  expect_snapshot_value(
    rFunction(test_sets$nam) |>
      as_tibble(),
    style = "json2"
  )

  # savanah
  expect_snapshot_value(
    rFunction(test_sets$savahn) |>
      as_tibble(),
    style = "json2"
  )

})

 
# Helper functions -----------------------------------------

test_that("Expected outcome of `attendanceTab_()` has not changed", {

  testthat::local_edition(3)

  # WCS
  expect_snapshot_value(
    attendanceTab_(
      dt = test_sets$wcs,
      clust_col = "clust_id",
      trck_col = mt_track_id_column(test_sets$wcs)) |>
      units::drop_units(),
    style = "json2"
  )

  # Kendall Tanzania
  expect_snapshot_value(
    attendanceTab_(
      dt = test_sets$ken_tnz,
      clust_col = "clust_id",
      trck_col = mt_track_id_column(test_sets$ken_tnz)),
    style = "json2"
  )

})
 
 
# Documentation   ---------------------------------------

test_that("output colnames match those in output documentation", {

  out <- rFunction(data = test_sets$nam |> slice(1:500))

  track_clust_tbl_names <- out |>
    as_tibble() |>
    dplyr::select(-c(clust_id, individual_name_deployment_id)) |>
    names()

  clust_tbl_names <- mt_track_data(out) |>
    select(-clust_id) |>
    names()

  track_clust_details <- read_rds(here("doc/track_clust_details.rds"))
  clust_details <- read_rds(here("doc/clust_details.rds"))

  # Track-per-cluster metrics
  track_clust_details_names <- names(track_clust_details) |>
    stringr::str_remove_all("`") |>
    stringr::str_remove("<behaviour-category>_drtn \\[e.g. ") |>
    stringr::str_remove("\\]") |>
    stringr::str_split(" and |, ") |>
    as_vector() |>
    # unpack acc colnames
    sapply(\(x){
      if(grepl("acc", x)){
        x <- sapply(c("x", "y", "z"), \(y) paste0(stringr::str_split(x, "<xyz>", simplify = TRUE), collapse = y))
      }
      return(x)
    },
    USE.NAMES = FALSE) |>
    unlist()

  expect_setequal(track_clust_tbl_names, track_clust_details_names)

  # whole-cluster metrics
  clust_details_names <- names(clust_details) |>
    stringr::str_remove_all("`") |>
    stringr::str_remove("<behaviour-category>_drtn_cmpd \\[e.g. ") |>
    stringr::str_remove("\\]") |>
    stringr::str_split(" and |, ") |>
    as_vector()

  expect_setequal(clust_tbl_names, clust_details_names)
})




# test_that("Expected outcome of `revisitTab_()` has not changed", {
#
#   testthat::local_edition(3)
#
#   # SOP Namibia
#   expect_snapshot_value(
#     revisitTab_(
#       trk_clust_dt = test_cluster_sets$nam,
#       dt = test_sets$nam,
#       clust_col = "clust_id",
#       trck_col = mt_track_id_column(test_sets$nam),
#       tm_col = "timestamp_local"),
#     style = "json2"
#   )
#
#   # Savanah
#   expect_snapshot_value(
#     revisitTab_(
#       trk_clust_dt = test_cluster_sets$savahn,
#       dt = test_sets$savahn,
#       clust_col = "clust_id",
#       trck_col = mt_track_id_column(test_sets$savahn),
#       tm_col = "timestamp_local"),
#     style = "json2"
#   )
# })






