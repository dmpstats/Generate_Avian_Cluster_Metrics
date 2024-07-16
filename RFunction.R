library('move2')
library('lubridate')
library('dplyr')
library('tidyr')
library('Gmedian')
library('sf')
library('units')
library("cli")
library("purrr")
library("rlang")


# Wee helpers
`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)

rFunction = function(data, 
                     cluster_id_col = "clust_id", 
                     behav_col = "behav",
                     cluster_tbl_type = c("track-and-whole", "whole-only")) {
  

  #' -----------------------------------------------------------------
  ## 1. Input validation -----
  
  # `cluster_id_col`
  check_col_ids(
    id_col = cluster_id_col, 
    app_par_name = "Cluster ID Column", 
    dt_names = colnames(data),
    suggest_msg = paste0(
      "Use clustering Apps such as {.href [Avian Cluster Detection](https://www.moveapps.org/apps/browser/81f41b8f-0403-4e9f-bc48-5a064e1060a2)} ",
      "earlier in the workflow to generate the required column."),
    proceed_msg = paste0(
      "Cluster metrics will be calculated on location events grouped by column ",
      "`", cluster_id_col, "`"
    )
  )
  

  # `behav_col`
  if(not_null(behav_col)){
    
    check_col_ids(
      id_col = behav_col, 
      app_par_name = "Behaviour Category Column", 
      dt_names = colnames(data),
      suggest_msg = paste0(
        "Use behavioural classification Apps such as {.href [Behavioural Classification ",
        "for Vultures](https://www.moveapps.org/apps/browser/44bb2ffa-7d40-4fad-bff5-1269995ba1a2)} ",
        "earlier in the workflow to generate the required column."),
      proceed_msg = paste0(
        "Behaviour-related cluster metrics will be calculated based on column `",
        behav_col, "`"
      )
    )
    
    if(!is.character(data[[behav_col]])){
      cli::cli_abort("Column {.arg behav_col} must be of type {.cls character}", 
                     class = "invalid-behav-col-class")
    }
  }else{
    logger.info(paste0(
      "`behav_col` specified as `NULL`, therefore skipping derivation of ",
      "behavioural-based cluster metrics."))
  }
    
  
  #' Type of output ------
  cluster_tbl_type <- rlang::arg_match(cluster_tbl_type)
  
  
  #' Input data columns ----------
  
  # 'timestamp_local', i.e. dependency on 'Add local time' app
  if("timestamp_local" %!in% colnames(data)){
    logger.fatal("Input data must contain column 'timestamp_local'.")
    
    cli::cli_abort(c(
      "Input data must contain column {.code timestamp_local}.",
      "i" = paste0(
        "Deploy the App {.href [Add Local and Solar Time](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b)} ",
        "ealier in the Workflow to add local time to the input data."
      )
    ),
    class = "missing-input-col"
    )
  }
  


  if("nightpoint" %!in% names(data)){
    miss_suntime_cols <- setdiff(c("sunset_timestamp", "sunrise_timestamp"), colnames(data))
    
    if(length(miss_suntime_cols) > 0){
      logger.fatal(cli::cli_text("Input data must contain column{?s} {.code {miss_suntime_cols}}."))
      cli::cli_abort(c(
        "Input data must contain column{?s} {.code {miss_suntime_cols}}.",
        "i" = paste0(
          "Ensure the App {.href [Add Local and Solar Time](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b)} ",
          "is deployed earlier in the Workflow to make {qty(miss_suntime_cols)} {?this/these} column{?s} available."
        )
      ),
      class = "missing-input-col"
      )
    }
  }
  
  
  logger.info("Inputs checked - all good to proceed")
  
  logger.info(paste0(
    "Summary of data for cluster metrics calculation:\n",
    "         |- ", nrow(data), " location points\n",
    "         |- ", mt_n_tracks(data), " tracks\n",
    "         |- ", sum(!is.na(unique(data[[cluster_id_col]]))), " clusters"
  ))
  

  #' -------------------------------------------------------------
  ## 2. Pre-processing  ------
  
  # Get timestamp and track ID columns
  tm_id_col <- mt_time_column(data)
  trk_id_col <- mt_track_id_column(data)
  
  
  # force calculations to skip one-location clusters (not clusters by definition)
  one_pnt_clusters <- count(data, .data[[cluster_id_col]]) |>
    filter(n == 1) |>
    pull(.data[[cluster_id_col]])

  if(length(one_pnt_clusters) > 0){
    logger.warn(paste0(
      "Metrics will not be calculated for the following cluster(s), as they contain only one location-point each:\n",
      "        * ",
      paste0(one_pnt_clusters, collapse = "\n        * ")
    ))
    
    # assign NAs to cluster IDs of identified one-point clusters. 
    # This keeps the points for calculation purposes, but there will
    # be no metrics for one-point clusters
    data <- data |> 
      mutate("{cluster_id_col}" := ifelse(.data[[cluster_id_col]] %in% one_pnt_clusters, NA, .data[[cluster_id_col]]))
  }

  
  # ensuring data is ordered by time within track ID
  data <- data |> arrange(.data[[trk_id_col]], .data[[tm_id_col]])
  
  # Ensuring key variables are calculated
  data <- data %>%
    mutate(
      hour_local = lubridate::hour(timestamp_local),
      date_local = lubridate::date(timestamp_local),
      timediff_hrs = mt_time_lags(., units = "hours")
    )
  
  # if absent, generate nightpoint based solely on sunset and sunrise times
  if("nightpoint" %!in% names(data)){
    logger.warn(paste0(
      "`nighpoint` column is not contained in the input data. Deriving nightpoints ",
      "based on sunset and sunrise timestamps (no leeway)."
    ))
    
    data <- data |> 
      mutate(
        nightpoint = ifelse(
          between(lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # with_tz() ensures TZs consistency
          sunrise_timestamp, 
          sunset_timestamp),
        0, 1))
  }
  
  

  
  
  # get names behaviour categories that are present in clusters
  if(not_null(behav_col)){
    behav_ctgs <- data |> 
      filter(!is.na(.data[[cluster_id_col]])) |> 
      count(.data[[behav_col]]) |> 
      pull(.data[[behav_col]]) |> 
      as.character()  
    
    # extract name of feeding category
    feed_ctg <- grep("[f|F]eeding", behav_ctgs, value = TRUE)
  }else{
    feed_ctg <- NULL
    behav_ctgs <- NULL
  }
  

  #' -------------------------------------------------------------
  ## 3. Derive track-level summaries within clusters  -------
  
  logger.info("Generating track-level summaries within each cluster")
  
  ### 3.1 Basic summaries within each cluster ----
  track_cluster_tbl <- data |> 
    filter(!is.na(.data[[cluster_id_col]])) %>%
    group_by(.data[[cluster_id_col]], .data[[trk_id_col]]) |> 
    summarise(
      # combine cluster points into multipoint
      all_points = st_combine(geometry),
      # size-related
      n_pts = n(),
      n_pts_night = sum(nightpoint == 1),
      n_pts_day = sum(nightpoint == 0),
      # Time-related
      first_dttm = min(.data[[tm_id_col]]),
      last_dttm = max(.data[[tm_id_col]]),
      first_dttm_local = min(timestamp_local),
      last_dttm_local = max(timestamp_local),
      duration_hrs = as.numeric(last_dttm_local - first_dttm_local, units = "hours"),
      n_days_span = (ceiling_date(last_dttm_local, unit = "days", change_on_boundary = TRUE) - floor_date(first_dttm_local, unit = "days")) %>% as.integer(),
      n_days_unique = length(unique(date_local)),
      n_days_empty = as.numeric(n_days_span - n_days_unique),
      med_hour_local = median(hour_local, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate track-level geometric medians in each cluster
    mutate(median_point = calcGMedianSF(.), .after = all_points) 
  
  
  #' Add second geometry column for cluster-level centroids. These are
  #' per-cluster, not per-bird. This is required to calculate some of the
  #' metrics below.
  #'
  #' !IMPORTANT!: centroids computed as the geometric median of the track-level
  #' medians within each cluster
  wholeclusts <- track_cluster_tbl |> 
    summarise(
      clust_track_meds = st_combine(median_point),
      .by = all_of(cluster_id_col)
    ) %>%
    mutate(clust_centroid = calcGMedianSF(.)) |> 
    st_set_geometry("clust_centroid") |> 
    select(-clust_track_meds) |> 
    as_tibble()
  
  track_cluster_tbl <- track_cluster_tbl %>% left_join(wholeclusts, by = cluster_id_col)
  
  
  
  ### 3.2 Time spent at each behaviour category, per track within cluster -----
  if(not_null(behav_col)){
    
    logger.info("   [a] Deriving Time-at-Behaviour metrics")
    
    time_at_behav <- data |> 
      as_tibble() |> 
      filter(!is.na(.data[[cluster_id_col]])) |> 
      group_by(.data[[cluster_id_col]], .data[[trk_id_col]], behav) |> 
      summarise(time_spent = sum(timediff_hrs), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = behav, 
        values_from = time_spent, 
        names_glue = "{behav}_duration", 
        values_fill = units::set_units(0, "hr")
      )  
    
  }else{
    logger.info("   [a] Skipping Time-at-Behaviour calculations")
    time_at_behav <- NULL
  }
  
  
  ### 3.3 Sub-table with Accelerometer summaries, if ACC is available -----
  
  if ("var_acc_x" %in% colnames(data)) {
  
    logger.info("   [b] Accelerometer columns identified. Calculating ACC summaries")
    
    track_cluster_tbl_acc <- data |> 
      # drop geometry for efficiency, by avoiding default parsing to multipoints by `summarise.sf()`
      st_drop_geometry() |> 
      filter(!is.na(.data[[cluster_id_col]])) |> 
      group_by(.data[[cluster_id_col]], .data[[trk_id_col]]) |> 
      summarise(
        across(starts_with("var_acc_"), \(x) median(x, na.rm = TRUE), .names = "med_{.col}"),
        across(starts_with("var_acc_"), \(x) sd(x, na.rm = TRUE), .names = "sd_{.col}"),
        .groups = "drop"
      )
    
  } else {
    logger.info("   [b] No accelerometer data identified. Skipping ACC summaries")
    track_cluster_tbl_acc <- NULL
  }
  
  
  
  ### 3.4 Time-at-Carcass Calculations     -------------------------------------
  logger.info("   [c] Deriving Time-at-Carcass metrics")
  
  #' generate columns `meanvisit_duration` `meanvisit_daytime_duration`
  #' 
  carctime <- timeAtCarcTab_(
    dt = data, 
    clust_col = cluster_id_col, 
    trck_col = trk_id_col)
  
  
  ### 3.5 Revisitation Calculations  -------------------------------------------
  logger.info("   [d] Cyphering Revisitation metrics")
  
  #' generate columns  `mean_n_visits`
  #' 
  revisits <- revisitTab_(
    trk_clust_dt = track_cluster_tbl, 
    dt = data,
    clust_col = cluster_id_col, 
    trck_col = trk_id_col, 
    tm_col = "timestamp_local")
  
  
  ### 3.6 Night-Distance Calculations ------------------------------------------
  logger.info("   [e] Cooking Night-distance metrics")
  
  #' generate columns `mean_night_dist`; `night_prop_250m` and `night_prop_1km`
  #'
  nightdists <- nightTab_(
    dt = data, 
    trk_clust_dt = track_cluster_tbl,
    clust_col = cluster_id_col,
    trck_col = trk_id_col)
  
  
  ### 3.7. Arrival-Distance Calculations ---------------------------------------
  logger.info("   [f] Hammering Arrival-Distance metrics")
  
  #' generate column `mean_arrival_dist` 
  #' 
  arrivaldists <- arrivalTab_(
    dt = data, 
    trk_clust_dt = track_cluster_tbl,
    clust_col = cluster_id_col,
    trck_col = trk_id_col,
    tm_col = "timestamp_local")
  
  
  ### 3.8 Stack outputs into final clustertable ---------------------------------
  logger.info("   [g] Merging [a-f] metrics into primary track-level cluster table")
  
  if (not_null(time_at_behav)) {
    track_cluster_tbl <- track_cluster_tbl |>  
      left_join(time_at_behav, by = c(cluster_id_col, trk_id_col))
  }
  
  track_cluster_tbl <- track_cluster_tbl |> 
    left_join(carctime, by = c(cluster_id_col, trk_id_col)) |> 
    left_join(revisits, by = c(cluster_id_col, trk_id_col)) |> 
    left_join(nightdists, by = c(cluster_id_col, trk_id_col)) |> 
    left_join(arrivaldists, by = c(cluster_id_col, trk_id_col)) |> 
    st_set_geometry("median_point") #|> 
    #select(-clust_centroid)
  
  
  if (not_null(track_cluster_tbl_acc)) {
    track_cluster_tbl <- track_cluster_tbl |> 
      left_join(track_cluster_tbl_acc, by = c(cluster_id_col, trk_id_col))
  }
  

  
  
  #' --------------------------------------------------------------------------
  ## 4. Generate whole cluster attributes     ---------
  
  logger.info("Generating whole-cluster summaries")
  
  ### 4.1 Nearest-track Calculations --------------------------------------------
  logger.info("   [i] Summoning Nearest-Tracks metrics - this may run slowly!")
  
  nearbirds <- nearBirdsTab_(
    dt = data, 
    trk_clust_dt = track_cluster_tbl,
    clust_col = cluster_id_col,
    trck_col = trk_id_col,
    tm_col = "timestamp_local")
  
  
  ### 4.2 Aggregate over tracks ---------------------------------------

  logger.info("   [ii] Summarizing and aggregating over track-level cluster metrics")
  
  # basic whole-cluster metrics
  cluster_tbl <- data |>  
    filter(!is.na(.data[[cluster_id_col]])) %>%
    group_by(.data[[cluster_id_col]]) |> 
    summarise(
      # combine cluster points into multipoint
      clust_points = st_combine(geometry),
    
      # Time-related
      spawn_dttm = min(.data[[tm_id_col]]),
      cease_dttm = max(.data[[tm_id_col]]),
      spawn_dttm_local = min(timestamp_local),
      cease_dttm_local = max(timestamp_local),
      
      # track membership
      member_tracks_n = length(unique(.data[[trk_id_col]])),
      member_tracks_ids = list(unique(.data[[trk_id_col]])),
      
      # lifespan metrics
      duration_days = as.numeric(cease_dttm_local - spawn_dttm_local, units = "days"),
      span_days = (ceiling_date(cease_dttm_local, unit = "days", change_on_boundary = TRUE) - floor_date(spawn_dttm_local, unit = "days")) %>% as.integer(),
      n_days_active = length(unique(date_local)),
      n_days_inactive = span_days - n_days_active,
      
      n_pts = n(),
      
      # mean, median and sd of pairwise distance between points in cluster
      pairwise_dist_stats(geometry, name_prefix = "pnts_pairdist"),

      .groups = "drop"
    ) |>
    mutate(
      pnts_spread_area = st_convex_hull(clust_points) |>
        # add 50cm buffer to deal with linearly positioned points, to which
        # `st_convex_hull` doesn't produce a polygon
        st_buffer(units::set_units(0.5, "m")) |>
        st_area() |>
        units::set_units("m2")
    ) %>%
    st_set_geometry("clust_points") %>%
    # Calculate cluster centroids based on geometric medians
    mutate(centroid = calcGMedianSF(.), .after = cease_dttm_local) %>%
    st_set_geometry("centroid") %>%
    dplyr::select(-clust_points)
    
  
  
  # Attributes calculated from track-level cluster metrics 
  cluster_track_based <- track_cluster_tbl %>%
    group_by(.data[[cluster_id_col]]) %>%
    summarise(
      
      # total time spent on each behaviour
      if(not_null(behav_ctgs)) across(matches(behav_ctgs), ~sum(.x, na.rm = TRUE), .names = "cl_{.col}"),
      
      #avg_daytime_hour_local = mean(med_daytime_hour_local, na.rm = TRUE),
      avg_hour_local = mean(med_hour_local, na.rm = TRUE),

      avg_attendance = mean(mean_attendance, na.rm = TRUE),
      avg_attendance_daytime = mean(mean_attendance_daytime, na.rm = TRUE),
      avg_n_visits = mean(mean_n_visits, na.rm = TRUE),

      # Distance calculations
      avg_nightime_dist = mean(mean_night_dist, na.rm = TRUE),
      avg_nightime_prop_250m = mean(night_prop_250m, na.rm = TRUE),
      avg_nightime_prop_1km = mean(night_prop_1km, na.rm = TRUE),
      avg_arrival_dists = mean(mean_arrival_dist, na.rm = TRUE),
      
      # mean, sd and median of pairwise distances between track centroids in the
      # cluster
      pairwise_dist_stats(median_point, name_prefix = "track_cntrd_pairdist"),

      .groups = "drop"
    ) |> 
    st_drop_geometry()
  
  
  ### 4.3 Merge derived whole-clusters metrics  -----------------------------
  cluster_tbl <- cluster_tbl |>
    left_join(cluster_track_based, by = cluster_id_col) |> 
    left_join(nearbirds, by = cluster_id_col) |>
    #' drop move2 and sf classes for parsing as track data of the App's output, below
    as_tibble()


  ## 5. Finalise Outputs ---------------------------------------------------------

  # sort both tables by starting cluster time (ascending)
  cluster_tbl <- cluster_tbl |> arrange(spawn_dttm_local)
  track_cluster_tbl <- track_cluster_tbl |> arrange(.data[[cluster_id_col]], first_dttm_local)

  
  #' Output either a move2_loc with both track-per-cluster and whole-cluster metrics
  #' OR whole-cluster-only metrics. 
  #' The chosen type of outputted cluster table is stored as an attribute of the output
  if(cluster_tbl_type == "track-and-whole"){
    
    logger.info(paste0(
      "Option 'track-and-whole' selected, so output to contain both track-per-cluster ",
      "and whole-cluster metrics."
    ))
    
    #' Output is a move2_loc object holding track-per-cluster metrics as the event
    #' dataset and whole-clusters metrics as the track data. Track ID is the
    #' cluster ID and the time column is the first timepoint of each track at a
    #' given cluster
    cluster_data <- mt_as_move2(
      track_cluster_tbl |> select(-clust_centroid), 
      time_column = "first_dttm", 
      track_id_column = cluster_id_col
    ) |> 
      mt_set_track_data(cluster_tbl)  
    
    attr(cluster_data, "cluster_tbl_type") <- "track-and-whole"
    
  } else {
    
    logger.info("Option 'whole-only' selected, so output to contain only whole-cluster metrics")
    
    #' Output is a move2_loc object with whole-cluster metrics as the event
    #' table. The track table is a placeholder with no further data
    cluster_data <- mt_as_move2(
      cluster_tbl, 
      time_column = "spawn_dttm",
      track_id_column = cluster_id_col
    )
    
    attr(cluster_data, "cluster_tbl_type") <- "whole-only"
  }
  
  
  logger.info(paste0("Size of the generated cluster table: ", object.size(cluster_data) %>% format(units = "Mb")))
  
  logger.info("Right, that's the cluster metrics calculation done!")
  
  return(cluster_data)
  
}



# Helper Functions ====================================================================

check_col_ids <- function(id_col, app_par_name, dt_names, suggest_msg, proceed_msg){

  call_id_col <- caller_arg(id_col)
  
  if(is.null(id_col)){
    logger.fatal(paste0("`", caller_arg(id_col), "` is missing."))
    cli::cli_abort(c(
      "Parameter '{app_par_name}' ({.arg {call_id_col}}) must be a string, not `NULL`.",
      "i" = "Please provide a valid column name for this parameter."
    ),
    call = rlang::caller_env(),
    class = "invalid-id-col")
    
  } else if(!is.character(id_col) || length(id_col) > 1){
    logger.fatal(paste0("`", call_id_col, "` must be a string."))
    cli::cli_abort(
      "Parameter '{app_par_name}' ({.arg {call_id_col}}) must be a string.",
      call = rlang::caller_env(),
      class = "invalid-id-col"
    )
    
  } else if (id_col %!in% dt_names) {
    logger.fatal(paste0(
      "Input data does not have column '", id_col,"'. Please provide a ",
      "valid column name with cluster ID annotations."
    ))
    cli::cli_abort(c(
      "Specified column name {.arg {id_col}} must be present in the input data.",
      "!" = "Please provide a valid column name for parameter '{app_par_name}' ({.arg {call_id_col}}).",
      "i" = suggest_msg
    ),
    call = rlang::caller_env(),
    class = "invalid-id-col")
  }else{
    logger.info(proceed_msg)
  }
    
}


#' //////////////////////////////////////////////////////////////////////////////
#' Helper to calculate summary statistics for pairwise distances between
#' locations points. Summaries calculated on the lower triangle of the distance
#' matrix to avoid double accounting of paired distances and the inclusion of 0s
#' from same-point distances (i.e. the matrix diagonal)
pairwise_dist_stats <- function(geom_pts, name_prefix = "pnts_pairdist"){
  
  if(!all(st_is(geom_pts, "POINT"))){
    cli::cli_abort(
      "`geom_pts` must be an `sf` object of geometry type 'POINT'"
    )
  }
  # pairwise distance matrix
  dst_mat <- sf::st_distance(geom_pts)
  
  # lower triangle of distance matrix
  dst_lower_tri <- dst_mat[lower.tri(dst_mat)]
  
  # summary statistics
  tibble(
    "{name_prefix}_mean" := mean(dst_lower_tri),
    "{name_prefix}_med" := median(dst_lower_tri),
    # ifelse to handle SD single distances (due to only two points), returning
    # 0, instead of NA
    "{name_prefix}_sd" := ifelse(length(dst_lower_tri) == 1, 0, sd(dst_lower_tri))
  ) |> 
    mutate(across(everything(), ~set_units(.x, "m")))
}



#' #' //////////////////////////////////////////////////////////////////////////////
#' #' Helper to calculate time spent at each behaviour category, which is compatible with use
#' #' inside `summarise()`
#' timespent_by_behav <- function(behavs, lags, behav_ctgs){
#'   purrr::map(behav_ctgs, \(behav){
#'     tibble::tibble("{behav}_hrs" := sum(lags[behavs == behav], na.rm = TRUE))
#'   }) |> 
#'     purrr::list_cbind()
#' }



#' //////////////////////////////////////////////////////////////////////////////
# Function to calculate geometric medians:
calcGMedianSF <- function(data) {
  
  if (st_geometry_type(data[1,]) == "POINT") {
    
    med <- data %>% 
      st_coordinates()
    
    med <- Gmedian::Weiszfeld(st_coordinates(data))$median %>% as.data.frame() %>%
      rename(x = V1, y = V2) %>%
      st_as_sf(coords = c("x", "y"), crs = st_crs(data)) %>%
      st_geometry()
  }
  
  
  if (st_geometry_type(data[1,]) == "MULTIPOINT") {
    
    med <- data %>% 
      st_coordinates() %>%
      as.data.frame() %>%
      group_by(L1) %>%
      group_map( ~st_point(Gmedian::Weiszfeld(.)$median) ) |> 
      st_as_sfc(crs = st_crs(data))
    
    # med <- data %>% 
    #   st_coordinates() %>%
    #   as.data.frame() %>%
    #   group_by(L1) %>%
    #   group_map(
    #     ~ Gmedian::Weiszfeld(.)$median 
    #   ) %>%
    #   do.call(rbind, .) %>%
    #   as.data.frame() %>%
    #   st_as_sf(coords = colnames(.), crs = st_crs(data)) %>%
    #   st_geometry()
  }
  
  return(med)
  
}


#' //////////////////////////////////////////////////////////////////////////////
#' Function to calculate daily average attendance time by a track/animal in a given
#' cluster, which is assumed as a proxy of time at carcass
#' 
timeAtCarcTab_ <- function(dt, clust_col, trck_col) {
  
  if(!all(dt$nightpoint %in% c(0,1))){
    cli::cli_abort(
      "Column {.code nightpoint} in input data must only contain numeric values 0 and/or 1.", 
      call = NULL)
  } 
  
  carctime <- dt |> 
    # drop locally unnecessary geometry, for efficiency
    st_drop_geometry() |> 
    # flag last and first locations of each day.
    # NOTE 1: purposefully *not* grouping by track, so that last/first points of
    # the day are still tagged when the next/previous locations belong to a
    # different track, allowing the calculations below for `date_attn_lags` to
    # return values in those cases.
    # NOTE 2: want to do this before filtering out non-clustered locations, to
    # include all available data
    mutate(
      date_last_loc = date_local != lead(date_local),
      date_first_loc = date_local != lag(date_local)
    ) |> 
    filter(!is.na(.data[[clust_col]])) |> 
    group_by(.data[[clust_col]], .data[[trck_col]], date_local) |> 
    # compute attendance time lags, accounting for gaps between last/first
    # points and the midnight boundary
    mutate(
      date_attn_lags = case_when(
        date_last_loc ~ as.numeric(ceiling_date(timestamp_local, "day") - timestamp_local, "hours") |> set_units("h"),
        date_first_loc ~ timediff_hrs + as.numeric(timestamp_local - floor_date(timestamp_local, "day"), "hours") |> set_units("h"),
        .default = timediff_hrs
      )
    ) |>
    # track's full-day and daytime attendance to the cluster, per visited day 
    summarise(
      attendance = sum(date_attn_lags, na.rm = TRUE),
      attendance_daytime = sum(date_attn_lags[nightpoint == 0], na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    # track's mean daily and mean daily-daytime attendance time to the cluster
    summarise(
      mean_attendance = mean(attendance),
      mean_attendance_daytime = mean(attendance_daytime),
      .groups = "keep"
    ) 
  
  return(carctime)
}



#' //////////////////////////////////////////////////////////////////////////////
#' Helpers to calculate daily average number of visits by a track/animal in a 
#' given cluster

#' -------------------------------------------- 
#' Upper-level: run calculations iteratively for each track-in-cluster combination
#' --------------------------------------------
#' @param dt a `move2_loc` object
#' @param trk_clust_dt a data.frame with track-level cluster data
#' @param clust_col;trck_col;tm_col  a character string specifying the column
#'   name in `dt` providing, respectively, the cluster ID assigned to location
#'   points, the track ID of location points and the timestamp of location
#'   points
revisitTab_ <- function(trk_clust_dt, dt, clust_col, trck_col, tm_col) {
  
  # drop geometry, for efficiency
  dt <- dt |> 
    st_drop_geometry()
  
  #' compute avg daily visits for each track in a given cluster (i.e. by row of
  #' clustertable) - here performed using `pmap()` and low-level helper `revisit_calc_`
  trk_clust_dt |>
    st_drop_geometry() |> 
    mutate(
      mn_visits = purrr::pmap(
        .l = list(
          clust = .data[[clust_col]], trck = .data[[trck_col]], 
          start = first_dttm_local, end = last_dttm_local
        ),
        .f = \(clust, trck, start, end){
          revisit_calc_(
            clust = clust, trck = trck, start = start, end = end, 
            dt = dt, clust_col = clust_col, trck_col = trck_col, tm_col = tm_col
          )
        }, 
        .progress = TRUE
      )
    ) |> 
    select(all_of(c(clust_col, trck_col)), mn_visits) |> 
    tidyr::unnest(mn_visits)
}

#' -----------------------------------
#' Lower-level: computing avg nr. visits/day of a given track to a given cluster
#' -----------------------------------
revisit_calc_ <- function(clust, trck, start, end, dt, clust_col, trck_col, tm_col){
  
  # filter for current track within cluster's time span
  dt <- dt |> 
    filter(between(.data[[tm_col]], start, end), .data[[trck_col]] == trck)
  
  # check data is ordered
  if(is.unsorted(dt[[tm_col]])) stop("data must be ordered by time.")
  
  dt |> 
    # Identify visits of track to current cluster, and those occurring at daytime
    mutate(
      #' `if_else` offers better handling of NAs in clust ID col via `missing` arg
      #' 0s denote track locations events not grouped into current cluster.
      #' Given data is ordered by time, 0s indicate instances where track
      #' "left" the cluster
      incluster = dplyr::if_else(.data[[clust_col]] == clust, 1, 0, missing = 0)
    ) |>
    #' Nr. of visits and nr. of daytime visits per day of current track to current cluster
    group_by(date_local) %>%
    summarise(
      visits = sum(rle(incluster)$values),
      .groups = "drop" 
    ) %>%
    #' Daily averages
    summarise(
      mean_n_visits = mean(visits, na.rm = T),
      .groups = "drop" 
    )
}



#' //////////////////////////////////////////////////////////////////////////////
#' Function to bind the following columns:
#'  - `mean_night_dist`, average daily distance from night points to
#'  centroids of visited clusters
#'  
#'  - `night_prop_250m` and `night_prop_1km`, the per-day proportion of night 
#'  points within, respectively, 250m and 1km of a visited cluster
#'  
nightTab_ <- function(dt, trk_clust_dt, clust_col, trck_col) {
  
  if(st_crs(dt) != st_crs(trk_clust_dt)){
    cli::cli_abort("{.arg dt} and {.arg trk_clust_dt} must have the same CRS projection")
  }
  
  # convert to tibble for cleaner processing, as move2 functionality not required here
  dt <- as_tibble(dt)
  
  # Firstly, filter to all night locations
  nightpts <- dt %>% 
    dplyr::select(-all_of(clust_col)) %>%
    filter(nightpoint == 1) %>%
    mutate(nightime_ymd = case_when(
      # Fix night locations being 'split' by midnight:
      # If before midnight, associate it with that same date
      hour_local > 12 ~ date_local,
      # But if in the morning, associate it with the night before
      hour_local < 12 ~ date_local - days(1)
    ))
  
  # Generate second table:
  # clust-bird-date, one entry for each clust visited by a bird on each date
  clustdays <- dt %>%
    filter(!is.na(.data[[clust_col]])) %>%
    distinct(.data[[clust_col]], .data[[trck_col]], date_local) %>%
    #' merge cluster centroids
    left_join(
      distinct(trk_clust_dt, .data[[clust_col]], clust_centroid), 
      by = clust_col
    ) |> 
    distinct()
  
  
  # The following table contains all night locations 
  # and has matched them to a cluster visited by a bird on that same day.
  # Where more than 1 cluster is visited by a bird within a day, the
  # night location has been duplicated (once for each cluster) so that it can be
  # grouped more than once.
  night_table <- left_join(
    nightpts, 
    clustdays, 
    by = c(trck_col, "nightime_ymd" = "date_local"), relationship = "many-to-many") %>%
    filter(!is.na(.data[[clust_col]])) 
  
  #' distance between each night point and the centroid of a cluster visited in that day
  night_table <- night_table %>%
    mutate(night_dist = st_distance(geometry, clust_centroid, by_element = TRUE))
  
  # Testing a new variable: proportion of nearby night points 
  # This is the proportion of night points on the same day as this cluster 
  # within 250m and 1km
  nearnights <- night_table %>%
    group_by(.data[[clust_col]], .data[[trck_col]]) %>%
    summarise(
      night_prop_250m = sum(night_dist < units::set_units(250, "m")) / n(), 
      night_prop_1km = sum(night_dist < units::set_units(1000, "m")) / n(),
      .groups = "drop"
    )
  
  
  #' Calculate the per-day median distance of each track's night points to the
  #' centroid of a visited cluster
  nightdists_by_day <- night_table %>%
    group_by(.data[[clust_col]], .data[[trck_col]], nightime_ymd) %>%
    summarise(
      med_night_dist = median(night_dist, na.rm = TRUE), 
      .groups = "drop"
    )
  
  
  # Finally, take mean distance per track across days
  nightdists_track_clust <- nightdists_by_day %>%
    group_by(.data[[clust_col]], .data[[trck_col]]) %>%
    summarise(
      mean_night_dist = mean(med_night_dist, na.rm = TRUE), 
      .groups = "drop"
      ) %>%
    left_join(nearnights, by = c(clust_col, trck_col))
  
  
  return(nightdists_track_clust)
}




#' //////////////////////////////////////////////////////////////////////////////
#' Compute the average distance between tracks' night-points and centroids of
#' associated clusters on the date of arrival at the cluster (i.e. first overnight)
#' 
#' @param dt a `move2_loc` object
#' @param trk_clust_dt a data.frame with track-level cluster data
#' @param clust_col;trck_col;tm_col  a character string specifying the column
#'   name in `dt` providing, respectively, the cluster ID assigned to location
#'   points, the track ID of location points and the timestamp of location
#'   points
#' 
#' @return a data.frame containing the key column `mean_arrival_dist` for
#'   track-level cluster entries
#' 
arrivalTab_ <- function(dt, trk_clust_dt, clust_col, trck_col, tm_col) {
  
  if(st_crs(dt) != st_crs(trk_clust_dt)){
    cli::cli_abort("{.arg dt} and {.arg trk_clust_dt} must have the same CRS projection.")
  }
  
  # timezone required for accurate slicing of locations data below
  tmzn <- tz(dt[[tm_col]])
  
  # Compute date of arrival (i.e. day of first visit) of tracks to clusters
  clustarrivals <- dt %>%
    st_drop_geometry() %>%
    group_by(.data[[clust_col]], .data[[trck_col]]) %>%
    summarise(day_of_arrival = as_datetime(min(date_local), tz = tmzn), .groups = "drop") %>%
    # merge cluster centroids
    left_join(
      distinct(trk_clust_dt, .data[[clust_col]], clust_centroid),
      by = clust_col
    ) %>%
    distinct() |> 
    st_set_geometry("clust_centroid")
  
  
  # Core calculations
  outdat <- clustarrivals |> 
    mutate(
      #' subset track's night-time locations within +/- 12hrs from its arrival
      #' date to a given cluster
      #' 
      #' BC DOUBLE CHECKING QUESTION: this selects ALL (night-time) track
      #' points, irrespective of their cluster affiliation, within the stipulated
      #' period. Shouldn't it be only the points assigned to the cluster that we
      #' are interested in here?
      locs_slice = pmap(
        list(trck = .data[[trck_col]], arr_dt = day_of_arrival, tz = tmzn),
        .f = \(trck, arr_dt, tz, ann_dt = dt){
          
          #browser()
          
          ann_dt %>%
            filter(
              .data[[trck_col]] == trck,
              nightpoint == 1,
              between(
                .data[[tm_col]], 
                arr_dt - hours(12),
                arr_dt + hours(12)
                #' BC QUESTION: Not sure it should be the following....?
                #arr_dt,
                #arr_dt + hours(24) - seconds(1)
              )
            ) |> 
            # drop move2 attributes
            as_tibble() |> 
            select(event_id, geometry)
        }, 
        .progress = TRUE)
    ) |> 
    # bring locations slice to the forefront, akin to a merge
    tidyr::unnest(locs_slice) |> 
    # calculate distances between night-point locations and cluster centroids
    mutate(dist = st_distance(clust_centroid, geometry, by_element = TRUE)) |> 
    st_drop_geometry() |> 
    # get the average distance from cluster centroids
    group_by(.data[[clust_col]], .data[[trck_col]], day_of_arrival) |> 
    summarise(
      mean_arrival_dist = mean(dist, na.rm = TRUE), 
      .groups = "drop"
    ) |> 
    dplyr::select(-day_of_arrival)
  
  return(outdat)
}





#' //////////////////////////////////////////////////////////////////////////////#
#' Functions to calculate distance metrics between track location points and the
#' centroid of each cluster, for non-member tracks "active" from 14-days prior
#' to cluster spawning to the cluster's last timepoint.
#' 
#' -------------------------
#'  Upper-level function
#' -------------------------
nearBirdsTab_ <- function(dt, trk_clust_dt, clust_col, trck_col, tm_col) {
  
  if(st_crs(dt) != st_crs(trk_clust_dt)){
    cli::cli_abort("{.arg dt} and {.arg trk_clust_dt} must have the same CRS projection.")
  }
  
  # List all clusters
  topclusters <- trk_clust_dt %>% 
    ungroup() %>%
    st_set_geometry("clust_centroid") %>%
    group_by(.data[[clust_col]]) %>%
    summarise(
      #tracks = paste(unique(.data[[trck_col]]), collapse = ", "),
      tracks = list(unique(.data[[trck_col]])),
      spawn_tm_loc = min(first_dttm_local, na.rm = TRUE),
      end_tm_loc = max(last_dttm_local, na.rm = TRUE),
      n_tracks = length(unique(.data[[trck_col]])),
      .groups = "drop" 
    )
  
  #' store csr to use in iterative `pmap` below
  clust_crs <- st_crs(topclusters)
  
  #' For non-member tracks, relative to the centroid of a cluster, compute
  #' (i) the closest location point
  #'(ii) Number of tracks within 25km and 50km
  topclusters |> 
    mutate(
      nearpts_prox = purrr::pmap(
        .l = list(
          trks = tracks, 
          spawn_tm = spawn_tm_loc, 
          end_tm = end_tm_loc, 
          clst_ctrd = clust_centroid
        ),
        .f = \(trks, spawn_tm, end_tm, clst_ctrd, ...){
          distvals_(
            trks, spawn_tm, end_tm, clst_ctrd, clust_crs = clust_crs, 
            dt = dt, tm_col = tm_col, trck_col = trck_col)
        }, 
        .progress = TRUE
      )
    ) |> 
    unnest(nearpts_prox) |> 
    as_tibble() |> 
    dplyr::select(all_of(clust_col), trks_mindist_m, trks_n_within_25km, trks_n_within_50km)
}


#' -----------------------------
#' Lower-level function
#' -----------------------------
distvals_ <- function(trks, spawn_tm, end_tm, clst_ctrd, 
                      clust_crs, dt, tm_col, trck_col) {
  
  clst_ctrd <- st_sfc(clst_ctrd, crs = clust_crs)

  #' get all point locations spanning from 14 days before the cluster
  #' spawning to cluster end time, regardless of their cluster affiliation
  nearpoints <- dt |>
    filter(between(.data[[tm_col]], spawn_tm - days(14), end_tm))

  #' get track IDs of point locations occurring during the period
  #' of the cluster
  active_tracks <- nearpoints %>%
    filter(between(.data[[tm_col]], spawn_tm, end_tm)) %>%
    #.[[trck_col]] %>%
    pull(.data[[trck_col]]) |> 
    unique()
  
  # subset for active tracks that are not part of the cluster
  nearpoints_actv_out <- nearpoints |> 
    filter(
      .data[[trck_col]] %in% active_tracks,
      .data[[trck_col]] %!in% trks
    )
  
  #' For points of tracks that are not part of the cluster but occurred
  #' during the cluster period, calculate their distance to the cluster
  #' centroid
  nearpoints_dist <- nearpoints_actv_out %>%
    mutate(
      dist = st_distance(geometry, clst_ctrd) |> units::set_units("m")
      )
  
  #' For "active" tracks that not part of a given cluster, relative to
  #' the cluster's centroid, find out:
  #' (i) the the closest location point, across all non-member active tracks
  #'(ii) how many tracks are within 25km and 50km
  out <- nearpoints_dist |>
    st_drop_geometry() |>
    summarise(
      trks_mindist_m = ifelse(length(dist) == 0, NA, min(dist, na.rm = TRUE)),  # ifelse used to skip annoying warning on min(empty) == Inf when `nearpoints_dist` is empty :\
      #trks_mindist_m = ifelse(length(dist) == 0, NA, min(dist, na.rm = TRUE)) |> units::set_units("m"), # if one is too pesky with units...
      trks_n_within_25km = length(unique(.data[[trck_col]][dist < units::set_units(25, "km")])),
      trks_n_within_50km = length(unique(.data[[trck_col]][dist < units::set_units(50, "km")]))
    )
  
  return(out)
}


