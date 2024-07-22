#////////////////////////////////////////////////////////////////////////////////
## helper to get app key to then retrieve app secrets 
get_app_key <- function() {
  
  require(rlang)
  
  key <- Sys.getenv("MOVEAPPS_APP_KEY")
  if (identical(key, "")) {
    rlang::abort(message = c(
      "No App key found",
      "x" = "You won't be able to proceed with the App testing. Sorry!", 
      "i" = "You can request the App key from the App developers for testing purposes (bruno@dmpstats.co.uk)",
      "i" = "Set the provided App key via usethis::edit_r_environ() using enviroment variable named 'MOVEAPPS_APP_KEY'"
    ))
  }
  key
}



# /////////////////////////////////////////////////////////////////////////////
## helper to set interactive testing of main App RFunction (e.g. in testthat 
## interactive mode, or on any given script)
##
set_interactive_app_testing <- function(){
  
  require(here)
  
  source(here("RFunction.R"))
  source(here("src/common/logger.R"))
  source(here("src/io/app_files.R"))
  
  options(dplyr.width = Inf)
}



# /////////////////////////////////////////////////////////////////////////////
## helper to generate interactive map for output sense checking
## plots cluster polygons, track locations in clusters and all tracking data
##

require(MetBrewer)
require(units)
require(tmap)
require(dplyr)
require(units)
require(sf)

 
# icons <- tibble(
#   behav = c("SFeeding", "STravelling", "SRoosting", "SResting"),
#   icon = list(
#     leaflet::awesomeIcons(
#       icon = 'times',
#       iconColor = 'black',
#       library = 'fa',
#       markerColor = "red"
#     ),
#     leaflet::awesomeIcons(
#       icon = 'times',
#       iconColor = 'black',
#       library = 'fa',
#       markerColor = "red"
#     ),
#     leaflet::awesomeIcons(
#       icon = 'times',
#       iconColor = 'black',
#       library = 'fa',
#       markerColor = "red"),
#     leaflet::awesomeIcons(
#       icon = 'times',
#       iconColor = 'black',
#       library = 'fa',
#       markerColor = "red"
#     )
#   )
# )

sense_check_map <- function(clust_metrics, loc_dt){
  
  behav_pal <- MetBrewer::met.brewer("Austria", n = 4)
  track_id_pal <- MetBrewer::met.brewer("NewKingdom")
  
  trck_id_col <- mt_track_id_column(loc_dt)
  
  whole_clusters_dt <- clust_metrics |> 
    as_tibble() |> 
    # gather all cluster points
    summarise(
      cluster_points = st_combine(all_points),
      .by = clust_id
    ) |> 
    left_join(mt_track_data(clust_metrics), by = "clust_id")  |> 
    # generate cluster Polygon via `st_convex_hull` + application of 10m buffer 
    mutate(
      cluster_poly = st_convex_hull(cluster_points) |> 
        st_buffer(set_units(15, "m"))
    ) |> 
    select(
      clust_id, spawn_dttm_lcl, cease_dttm_lcl, centroid,  cluster_poly,  
      member_tracks_n, n_points:n_resting, avg_n_visits 
    ) |> 
    st_set_geometry("cluster_poly") |> 
    arrange(spawn_dttm_lcl) 
  
  
  track_level_clusters <- clust_metrics |> 
    filter(clust_id %in% whole_clusters_dt$clust_id) |> 
    as_tibble() |> 
    st_set_geometry("all_points")
  
  
  track_locs <- loc_dt |>
    as_tibble() |> 
    st_set_geometry("geometry") 
  
  track_path <- track_locs |> 
    summarise(tracks = st_combine(geometry), .by = all_of(trck_id_col)) |> 
    st_cast("LINESTRING")
  
  
  cluster_map <- whole_clusters_dt |> 
    tm_shape(name = "Clusters polygons") +
    tm_polygons(col = "white", alpha = 0.8) +
    tm_text(text = "spawn_dttm_lcl", just = "left", xmod = 0.005) +
    #tm_dots(col = "clust_id", size = 0.05, shape = 1, legend.show = FALSE) +
    
    tm_shape(track_level_clusters, name = "Track locations in clusters") +
    tm_dots(size = 0.15, col = trck_id_col, palette = track_id_pal) +
    
    tm_shape(track_locs, name = "Track Locations") +
    tm_dots(
      col = "behav", palette = behav_pal, size = 0.05, 
      popup.vars = c("behav", "RULE", "nightpoint", "clust_id", "kmph", "dist_m", 
                     "heading", "timestamp_local")
    ) +
    
    tm_shape(track_path) +
    tm_lines(col = trck_id_col, legend.col.show = FALSE, palette = track_id_pal)
  
  tmap_leaflet(cluster_map) |>  
     leaflet::addMeasure(primaryLengthUnit = "meters") 
  
}



 
## /////////////////////////////////////////////////////////////////////////////
# helper to run SDK testing with different settings
run_sdk <- function(data, 
                    cluster_id_col = "clust_id", 
                    behav_col = "behav",
                    output_type = c("cluster", "locs"),
                    cluster_tbl_type = c("track-and-whole")
){

  require(jsonlite)

  # get environmental variables specified in .env
  dotenv::load_dot_env(".env")
  app_config_file <- Sys.getenv("CONFIGURATION_FILE")
  source_file <- Sys.getenv("SOURCE_FILE")

  # store default app configuration
  dflt_app_config <- jsonlite::fromJSON(app_config_file)
  # get default input data
  dflt_dt <- readRDS(source_file)

  # set configuration to specified inputs
  new_app_config <- list(
    cluster_id_col = cluster_id_col,
    behav_col = behav_col,
    output_type = output_type,
    cluster_tbl_type = cluster_tbl_type
  )
  
  # overwrite config file with current inputs
  write(
    jsonlite::toJSON(new_app_config, pretty = TRUE, auto_unbox = TRUE, null = "null"),
    file = app_config_file
  )

  # overwrite app's source file with current input data
  saveRDS(data, source_file)

  # run SDK for the current settings
  try(source("sdk.R"))

  # reset to default config and data
  write(
    jsonlite::toJSON(dflt_app_config,  pretty = TRUE, auto_unbox = TRUE),
    file = app_config_file
  )
  saveRDS(dflt_dt, source_file)

  invisible()
}



