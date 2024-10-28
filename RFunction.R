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
                     output_type = c("cluster-based", "merge-to-locs"),
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
  
  
  
  if(all(is.na(data[[cluster_id_col]]))){
    logger.fatal(cli::cli_text("Column {.code {cluster_id_col}} contains universally NAs"))
    cli::cli_abort(c(
      "Column {.code {cluster_id_col}} contains only NAs. Unable to proceed with cluster metrics calculations.",
      "i" = paste0(
        "Input data must include annotations for at least one cluster - Metrics cannot be derived for non-existent clusters!"
      )
    ),
    class = "no-clusters-in-input"
    )
  }
  
  
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
    
  
  #' output choices ---------------
  output_type <- rlang::arg_match(output_type)
  if(output_type == "cluster-based") cluster_tbl_type <- rlang::arg_match(cluster_tbl_type)
  
  
  #' Input data columns ----------
  
  # 'local_tz', i.e. dependency on 'Add local time' app
  if("local_tz" %!in% colnames(data)){
    logger.fatal("Input data must contain column 'local_tz'.")
    
    cli::cli_abort(c(
      "Input data must contain column {.code local_tz}.",
      "i" = paste0(
        "Deploy the App {.href [Add Local and Solar Time](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b)} ",
        "ealier in the Workflow to add local timezone to the input data."
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
  data$timediff_hrs <- mt_time_lags(data, units = "hours")
  
  data <- data |> 
    # the repetition of `with_tz()` in the next chunk looks horrible, but it's
    # necessary to guarantee derived local time elements respect the local TZ.
    mutate(
      date_local = date(with_tz(.data[[tm_id_col]], first(local_tz))),
      # the hour element of local time
      hour_local = hour(with_tz(.data[[tm_id_col]], first(local_tz))),
      # decimal time since local start of day (i.e midnight)
      dec_time_local = decimal_time(with_tz(.data[[tm_id_col]], first(local_tz))),
      .by = local_tz
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
          between(
            lubridate::with_tz(.data[[tm_id_col]], lubridate::tz(sunrise_timestamp)), # with_tz() ensures TZs consistency
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
    group_by(.data[[trk_id_col]], .data[[cluster_id_col]]) |> 
    summarise(
      # combine cluster points into multipoint
      all_points = st_combine(geometry),
      # size-related
      pts_n = n(),
      pts_night_n = sum(nightpoint == 1),
      pts_day_n = sum(nightpoint == 0),
      # Time-related
      first_dttm = with_tz(min(.data[[tm_id_col]]), "UTC"),
      last_dttm = with_tz(max(.data[[tm_id_col]]), "UTC"),
      # Time zone: ignore if cluster spans over multiple TZs (unlikely unless it sits right across a border)
      local_tz = first(local_tz),
      timespan = as.numeric(last_dttm - first_dttm, units = "hours") |> units::set_units("h"),
      timespan_ndays = length(seq(min(date_local), max(date_local), 1)),
      days_present_n = length(unique(date_local)),
      days_absent_n = as.numeric(timespan_ndays - days_present_n),
      hour_local_med = median(hour_local, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate track-level geometric medians in each cluster
    mutate(median_point = calcGMedianSF(.), .after = all_points) 
  
  
  # Add first and final local date-time, as a character string
  track_cluster_tbl <- track_cluster_tbl |> 
    mutate(
      first_dttm_local = as.character(with_tz(first_dttm, first(local_tz))),
      last_dttm_local = as.character(with_tz(last_dttm, first(local_tz))),
      .by = local_tz, 
      .after = last_dttm
    ) |> 
    select(-local_tz)
  
  
  
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
  

  ### 3.2 Sub-table with Accelerometer summaries, if ACC is available -----
  
  if ("var_acc_x" %in% colnames(data)) {
  
    logger.info("   [a] Accelerometer columns identified. Calculating ACC summaries")
    
    track_cluster_tbl_acc <- data |> 
      # drop geometry for efficiency, by avoiding default parsing to multipoints by `summarise.sf()`
      st_drop_geometry() |> 
      filter(!is.na(.data[[cluster_id_col]])) |> 
      group_by(.data[[trk_id_col]], .data[[cluster_id_col]]) |> 
      summarise(
        across(
          starts_with("var_acc_"), 
          list(med = ~ median(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
    
  } else {
    logger.info("   [a] No accelerometer data identified. Skipping ACC summaries")
    track_cluster_tbl_acc <- NULL
  }
  
  
  
  ### 3.3 Attendance Calculations     -------------------------------------
  logger.info("   [b] Deriving Attendance metrics")
  
  #' generate columns `attendance`, `attendance_dmean` & `attendance_daytime_dmean`
  #' 
  attendance_time <- attendanceTab_(
    dt = data, 
    clust_col = cluster_id_col, 
    trck_col = trk_id_col, 
    behav_col = behav_col
  )
  
  
  ### 3.4 Revisitation Calculations  -------------------------------------------
  logger.info("   [c] Cyphering Revisitation metrics")
  
  #' generate columns  `visits_day_mean` & `visit_drtn_mean`
  #' 
  revisits <- revisitTab_(
    trk_clust_dt = track_cluster_tbl, 
    dt = data,
    clust_col = cluster_id_col, 
    trck_col = trk_id_col, 
    tm_col = tm_id_col)
  
  
  ### 3.5 Night-Distance Calculations ------------------------------------------
  logger.info("   [d] Cooking Night-distance metrics")
  
  #' generate columns  `nightpts_dist_dmean`; `nightpts_250m_prop` & `nightpts_1km_prop`
  #'
  nightdists <- nightTab_(
    dt = data, 
    trk_clust_dt = track_cluster_tbl,
    clust_col = cluster_id_col,
    trck_col = trk_id_col)
  
  
  ### 3.6. Arrival-Distance Calculations ---------------------------------------
  logger.info("   [e] Hammering Arrival-Distance metrics")
  
  #' generate column `arrival_dist_mean` 
  #' 
  arrivaldists <- arrivalTab_(
    dt = data, 
    trk_clust_dt = track_cluster_tbl,
    clust_col = cluster_id_col,
    trck_col = trk_id_col,
    tm_col = tm_id_col)
  
  
  ### 3.7 Stack outputs into final clustertable ---------------------------------
  logger.info("   [f] Merging [a-e] metrics into primary track-level cluster table")
  
  track_cluster_tbl <- track_cluster_tbl |> 
    left_join(attendance_time, by = c(cluster_id_col, trk_id_col)) |> 
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
    tm_col = tm_id_col)
  
  
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
      spawn_dttm = lubridate::with_tz(min(.data[[tm_id_col]]), "UTC"),
      cease_dttm = lubridate::with_tz(max(.data[[tm_id_col]]), "UTC"),
      # Time zone: ignore if cluster spans over multiple TZs (unlikely unless it sits right across a border)
      local_tz = first(local_tz),
    
      # track membership
      members_n = length(unique(.data[[trk_id_col]])),
      members_ids = list(unique(.data[[trk_id_col]])),
      
      # lifespan metrics
      timespan = as.numeric(cease_dttm - spawn_dttm, units = "hours") |> units::set_units("h"),
      timespan_ndays = length(seq(min(date_local), max(date_local), 1)),
      days_active_n = length(unique(date_local)),
      days_inactive_n = timespan_ndays - days_active_n,
      
      pts_n = n(),
      
      # mean, median and sd of pairwise distance between points in cluster
      pairwise_dist_stats(geometry, name_prefix = "pts_pairdist"),

      .groups = "drop"
    ) |>
    mutate(
      pts_spread_area = st_convex_hull(clust_points) |>
        # add 50cm buffer to deal with linearly positioned points, to which
        # `st_convex_hull` doesn't produce a polygon
        st_buffer(units::set_units(0.5, "m")) |>
        st_area() |>
        units::set_units("m2")
    ) %>%
    st_set_geometry("clust_points") %>%
    # Calculate cluster centroids based on geometric medians
    mutate(centroid = calcGMedianSF(.), .after = cease_dttm) %>%
    st_set_geometry("centroid") %>%
    dplyr::select(-clust_points)
    
  
  # Add spawning and ceasing local date-time, as character string
  cluster_tbl <- cluster_tbl |> 
    mutate(
      spawn_dttm_local = as.character(with_tz(spawn_dttm, first(local_tz))),
      cease_dttm_local = as.character(with_tz(cease_dttm, first(local_tz))),
      .by = local_tz, 
      .after = cease_dttm
    ) |> 
    select(-local_tz)
  
  
  
  
  # Attributes calculated from track-level cluster metrics 
  cluster_track_based <- track_cluster_tbl %>%
    group_by(.data[[cluster_id_col]]) %>%
    summarise(
      
      #avg_daytime_hour_local = mean(med_daytime_hour_local, na.rm = TRUE),
      hour_local_avg = mean(hour_local_med, na.rm = TRUE),

      # attendance metrics
      attnd_cmpd = sum(attnd, na.rm = TRUE),
      attnd_davg = mean(attnd_dmean, na.rm = TRUE),
      attnd_daytime_davg = mean(attnd_daytime_dmean, na.rm = TRUE),
      # total time spent on each behaviour
      if(not_null(behav_ctgs)) across(matches(behav_ctgs), ~sum(.x, na.rm = TRUE), .names = "{.col}_cmpd"),
      
      
      # visits metrics
      visits_day_avg = mean(visits_day_mean, na.rm = TRUE),
      visit_drtn_avg = mean(visit_drtn_mean, na.rm = TRUE),

      # Distance calculations
      nightpts_dist_davg = mean(nightpts_dist_dmean, na.rm = TRUE),
      nightpts_250m_avgprop = mean(nightpts_250m_prop, na.rm = TRUE),
      nightpts_1km_avgprop = mean(nightpts_1km_prop, na.rm = TRUE),
      arrival_dist_avg = mean(arrival_dist_mean, na.rm = TRUE),
      
      # mean, sd and median of pairwise distances between track centroids in the
      # cluster
      pairwise_dist_stats(median_point, name_prefix = "members_centroid_pairdist"),

      .groups = "drop"
    ) |> 
    st_drop_geometry()
  
  
  ### 4.3 Merge derived whole-clusters metrics  -----------------------------
  cluster_tbl <- cluster_tbl |>
    left_join(cluster_track_based, by = cluster_id_col) |> 
    left_join(nearbirds, by = cluster_id_col) |>
    #' drop move2 and sf classes for parsing as track data of the App's output, below
    as_tibble()

  

  ## 5. Process result for Outputs ---------------------------------------------------------
  
  logger.info(paste0(
    "Processing results according to output options"
  ))
  
  
  # sort both tables by starting cluster time (ascending)
  cluster_tbl <- cluster_tbl |> arrange(spawn_dttm)
  track_cluster_tbl <- track_cluster_tbl |> arrange(.data[[cluster_id_col]], first_dttm)
  
  
  if(output_type == "cluster-based"){
    
    ### 5.1 Cluster tables --------------------------------------------
    logger.info(paste0(
      "   |- 'cluster' option selected for `output_type`, so producing ",
      "cluster-based outputs"
      ))
 
    #' Output either a move2_loc with both track-per-cluster and whole-cluster metrics
    #' OR whole-cluster-only metrics. 
    #' The chosen type of outputted cluster table is stored as an attribute of the output
    if(cluster_tbl_type == "track-and-whole"){
      
      logger.info(paste0(
        "   |-  'track-and-whole' option chosen for `cluster_tbl_type`, so ",
        "output to contain both track-per-cluster and whole-cluster metrics."
      ))
      
      #' Output is a move2_loc object holding track-per-cluster metrics as the event
      #' dataset and whole-clusters metrics as the track data. Track ID is the
      #' cluster ID and the time column is the first timepoint of each track at a
      #' given cluster
      output <- mt_as_move2(
        track_cluster_tbl |> select(-clust_centroid), 
        time_column = "first_dttm", 
        track_id_column = cluster_id_col
      ) |> 
        mt_set_track_data(cluster_tbl)  
      
      # store type of output, for reference in downstream apps
      attr(output, "clust_dt_type") <- "track-and-whole"
      
    } else {
      
      logger.info(paste0(
        "   |- 'whole-only' option selected for `cluster_tbl_type`, so output to ",
        "contain only whole-cluster metrics"
      ))
      
      #' Output is a move2_loc object with whole-cluster metrics as the event
      #' table. The track table is a placeholder with no further data
      output <- mt_as_move2(
        cluster_tbl, 
        time_column = "spawn_dttm",
        track_id_column = cluster_id_col
      )
      
      # store type of output, for reference in downstream apps
      attr(output, "clust_dt_type") <- "whole-only"
    }
    
  } else {
    
    ### 5.2 Merge metrics to track location points --------------------------------------------
    logger.info(paste0(
      "   |-  'merge-to-locs' option selected for `output_type`, therefore merging calculated ",
      "whole-cluster metrics to track locations data"
      ))

    
    # Merge metrics to track location points
    #
    # NOTE 1: only keeping location points annotated with cluster ID (i.e.
    # non-clustered points are dropped) and only keep strictly necessary columns
    # 
    # NOTE 2: rename metrics with "cl" prefix, for separation with columns describing
    # individual-level tracking data
    output <- data |> 
      # drop non-clustered location points
      filter(!is.na(.data[[cluster_id_col]])) |> 
      # strip down dataset to key columns
      select(any_of(c(trk_id_col, tm_id_col, "event_id", behav_col, cluster_id_col))) |> 
      # merge cluster metrics
      left_join( 
        cluster_tbl |> rename_with(~paste0("cl_", .x), .cols = !all_of(cluster_id_col)), 
        by = cluster_id_col
      ) 
    
    # Append track-level and whole level metrics table as attributes of move2
    # output object
    attr(output, "cluster_tbl") <- cluster_tbl
    attr(output, "track_cluster_tbl") <- track_cluster_tbl |> select(-clust_centroid)
    
    # store type of output, for reference in downstream apps
    attr(output, "clust_dt_type") <- "whole-binned-to-locs"
    attr(output, "cluster_id_col") <- cluster_id_col
    
  }
  
  
  logger.info(paste0("Size of the generated cluster table: ", object.size(output) %>% format(units = "Mb")))
  
  logger.info("Right, that's the cluster metrics calculation done!")
  
  return(output)
  
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
    # ifelse to handle SD on single distances (due to only two points), returning
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
attendanceTab_ <- function(dt, clust_col, trck_col, behav_col) {
  
  if(!all(dt$nightpoint %in% c(0,1))){
    cli::cli_abort(
      "Column {.code nightpoint} in input data must only contain numeric values 0 and/or 1.", 
      call = NULL)
  } 
  
  # -- Identify first and last locations of each day-date (local)
  # NOTE 1: purposefully *not* grouping by track, so that last/first points of
  # the day are still tagged when the next/previous locations belong to a
  # different track, allowing the calculations below for `date_attn_lags` to
  # return values in those cases.
  # NOTE 2: want to do this before filtering out non-clustered locations, to
  # include all available data
  
  dt <- dt |> 
    # drop locally unnecessary sf and move2 attributes, for efficiency and cleanness
    st_drop_geometry() |> 
    as_tibble() |> 
    mutate(
      date_last_loc = date_local != lead(date_local),
      date_first_loc = date_local != lag(date_local)
    )
  
  # -- Roosting Correction
  # Calculate correction factor to add to cluster attendance in cases where, for a
  # given clustered location point, the subsequent location is on the next day and
  # is not clustered. This correction strictly applies to locations classified as
  # Roosting, and expresses decimal hours between (local) midnight and sunrise,
  # i.e. it assumes the bird remained roosting in the same cluster till sunrise. It
  # uses the sunrise time of that last location as a reference, as the sunrise on
  # the subsequent (unclustered) location might be nonsensical
  if(not_null(behav_col) && any(grepl("[R|r]oost", dt[[behav_col]]))){
    dt <- dt |> 
      # local sunrise decimal time 
      # must be grouped by TZ label, so that time since local midnight calculation is correct
      mutate(
        sunrise_dec_local = decimal_time(with_tz(sunrise_timestamp, first(local_tz)), "h"),
        .by = local_tz
      ) |> 
      # correction factor (equals 0 if not conditions not met)
      mutate(
        attn_roost_correction = if_else(
          date_last_loc & 
            is.na(lead(.data[[clust_col]])) & 
            grepl("[R|r]oost", dt[[behav_col]]),
          true = sunrise_dec_local, 
          false = set_units(0, "h"))
      )
  } else{
    dt$attn_roost_correction <- set_units(0, "h") 
  }
  
  
  # -- Compute visited day-based attendance time lags. i.e accounting for gaps 
  # between last/first points and the midnight boundary, and non-visited days
  # are excluded from calculation
  dt <- dt |> 
    filter(!is.na(.data[[clust_col]])) |> 
    group_by(.data[[clust_col]], .data[[trck_col]], date_local) |> 
    mutate(
      date_attn_lags = case_when(
        date_last_loc ~ units::as_units(24, "h") - dec_time_local + attn_roost_correction,
        date_first_loc ~ timediff_hrs + dec_time_local,
        .default = timediff_hrs
      )
    )
  
  
  # -- Derive daily attendance summaries and total attendance
  attnd <- dt  |> 
    # track's full-day and daytime attendance to the cluster, per visited day 
    summarise(
      daily_attendance = sum(date_attn_lags, na.rm = TRUE),
      daily_attendance_daytime = sum(date_attn_lags[nightpoint == 0], na.rm = TRUE),
      .groups = "drop_last"
    )  |> 
    # track's mean daily and mean daytime-daily attendance to the cluster
    summarise(
      attnd = sum(daily_attendance),
      attnd_dmean = mean(daily_attendance),
      attnd_daytime_dmean = mean(daily_attendance_daytime),
      .groups = "keep"
    )
  
  
  # -- Total time spent at each behaviour while in the cluster
  if(not_null(behav_col)){
    
    logger.info("        |- Deriving time spent at each behaviour")
    
    attnd_at_behav <- dt |> 
      group_by(.data[[clust_col]], .data[[trck_col]], .data[[behav_col]]) |> 
      summarise(time_spent = sum(date_attn_lags, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = behav, 
        values_from = time_spent, 
        names_glue = "attnd_{behav}", 
        values_fill = units::set_units(0, "h")
      )  
    
    attnd <- left_join(attnd, attnd_at_behav, by = c(clust_col, trck_col))
    
  }
  
  attnd
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
    st_drop_geometry() |> 
    as_tibble()
  
  #' compute avg daily visits for each track in a given cluster (i.e. by row of
  #' clustertable) - here performed using `pmap()` and low-level helper `revisit_calc_`
  trk_clust_dt |>
    st_drop_geometry() |> 
    mutate(
      mn_visits = purrr::pmap(
        .l = list(
          clust = .data[[clust_col]], trck = .data[[trck_col]], 
          start = first_dttm, end = last_dttm
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



#' --------------------------------------------------------------------------------
#' Lower-level: computing avg nr. visits/day of a given track to a given cluster
#' --------------------------------------------------------------------------------
revisit_calc_ <- function(clust, trck, start, end, dt, clust_col, trck_col, tm_col){
  
  # filter for current track within cluster's time span
  dt <- dt |> 
    filter(between(.data[[tm_col]], start, end), .data[[trck_col]] == trck)
  
  # check data is ordered
  if(is.unsorted(dt[[tm_col]])) stop("data must be ordered by time.")
  
  # Identify visits of track to current cluster
  dt <- dt |> 
    mutate(
      #' `if_else` offers better handling of NAs in clust ID col via `missing` arg
      #' 0s denote track locations events not grouped into current cluster.
      #' Given data is ordered by time, 0s indicate instances where track
      #' "left" the cluster
      incluster = dplyr::if_else(.data[[clust_col]] == clust, 1, 0, missing = 0)
    ) 
  
  #' Nr. visits of current track per day of whole timespan of current cluster event
  visits_num <- dt |>
    group_by(date_local) %>%
    summarise(visits_day = sum(rle(incluster)$values), .groups = "drop" ) |> 
    # exclude absent days
    filter(visits_day > 0) |> 
    #' mean number of visits per day
    summarise(visits_day_mean = mean(visits_day, na.rm = TRUE))
  
  #' Duration of each visit and mean visit duration of current track to current cluster
  visits_duration <- dt |> 
    mutate(visit_id = consecutive_id(incluster)) |> 
    filter(incluster == 1) |> 
    group_by(visit_id) |> 
    # visits durations
    summarise(visit_drtn = sum(timediff_hrs, na.rm = TRUE)) |> 
    # mean visit duration
    summarise(visit_drtn_mean = mean(visit_drtn, na.rm = TRUE))
    
  # bind the two metrics
  bind_cols(visits_num, visits_duration)
  
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
  
  # Inputting missing night-points (only in the context of the current function)
  # This step intends to handle cases when, on a given date, there are no
  # night-points for the track forming/visiting the cluster, which would lead to
  # the cluster being dropped from calculations on that date and, if a
  # single-date cluster, on the whole output - producing NAs for these metrics
  # for the offended cluster. Here we try to minimize the issue by converting
  # the track's first day-point of the next day to a nightpoint. 
  #
  # BEWARE: the issue will persists (and NAs will occur) if there is no next day
  # data for the track
  dt <- dt |>
    group_by(.data[[trck_col]]) |>
    mutate(
      prev_row_prev_day = date_local - lag(date_local) == 1,
      nightpoint_prev = lag(nightpoint),
      nightpoint = if_else(prev_row_prev_day & nightpoint_prev == 0 & nightpoint == 0,  
                           1, 
                           nightpoint, 
                           missing = nightpoint)
    )
  
  # Filter to all night locations
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
  
  
  # Generate second table
  # cluster-bird-date, one entry for each cluster visited by a bird on each date
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
      nightpts_250m_prop = sum(night_dist < units::set_units(250, "m")) / n(), 
      nightpts_1km_prop = sum(night_dist < units::set_units(1000, "m")) / n(),
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
      nightpts_dist_dmean = mean(med_night_dist, na.rm = TRUE), 
      .groups = "drop"
      ) %>%
    left_join(nearnights, by = c(clust_col, trck_col))
  
  
  return(nightdists_track_clust)
}




#' //////////////////////////////////////////////////////////////////////////////
#' Compute the average distance between tracks' night-points and centroids of
#' associated clusters on the night before the date of arrival at the cluster.
#' In other words, the travelled distance to the cluster from the previous
#' overnight location on the first arrival
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
  
  # reference timezone required for accurate slicing of locations data below
  tmzn <- tz(dt[[tm_col]])
  
  # convert to tibble for cleaner processing, as move2 functionality not required here
  dt <- as_tibble(dt)
  
  # Inputting missing night-points (only in the context of the current function)
  # Handle cases when, for the date of first arrival of a given track to the
  # cluster, there are no night-points available for the period covering
  # midnight -/+ 12 hours of the arrival date. This leads to the cluster being dropped
  # from calculations below, producing NAs for these metrics for the offended
  # cluster. Here we try to minimize the issue by converting the track's last
  # day-point of the previous day to a nigh-point. 
  #
  # BEWARE: the issue will persists (and NAs will occur for the given track) if
  # there is no track data on the previous day after 12 o'clock
  dt <- dt |>
    group_by(.data[[trck_col]]) |>
    mutate(
      next_row_next_day =  lead(date_local) - date_local == 1,
      nightpoint_next = lag(nightpoint),
      nightpoint = if_else(
        next_row_next_day & nightpoint_next == 0 & nightpoint == 0,  
        1, 
        nightpoint, 
        missing = nightpoint
      )
    ) |> 
    ungroup()
  
  # Compute date of arrival (i.e. day of first visit) of tracks to clusters
  clustarrivals <- dt %>%
    st_drop_geometry() %>%
    group_by(.data[[clust_col]], .data[[trck_col]]) %>%
    summarise(arrival_date_local = min(date_local), local_tz = first(local_tz), .groups = "drop") |> 
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
      locs_slice = pmap(
        list(trck = .data[[trck_col]], arr_dt = arrival_date_local, tz = local_tz),
        .f = \(trck, arr_dt, ann_dt = dt, tz){
          #browser()
          ann_dt %>%
            filter(
              .data[[trck_col]] == trck,
              nightpoint == 1,
              between(
                .data[[tm_col]],
                # ensuring TZ accuracy and consistency
                with_tz(as_datetime(arr_dt, tz) - hours(12), tmzn),
                with_tz(as_datetime(arr_dt, tz) + hours(12), tmzn)
              )
            ) |>
            select(event_id, geometry)
        },
        .progress = TRUE)
    ) |> 
    # bring night-point locations slice to the forefront, akin to a merge
    tidyr::unnest(locs_slice) |>
    # calculate distances between night-point locations and cluster centroids
    mutate(dist = st_distance(clust_centroid, geometry, by_element = TRUE)) |>
    st_drop_geometry() |>
    # get the average distance from cluster centroids
    group_by(.data[[clust_col]], .data[[trck_col]], arrival_date_local) |>
    summarise(
      arrival_dist_mean = mean(dist, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(-arrival_date_local)
  
  
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
      spawn_tm = min(first_dttm, na.rm = TRUE),
      end_tm = max(last_dttm, na.rm = TRUE),
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
          spawn_tm = spawn_tm, 
          end_tm = end_tm, 
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
    dplyr::select(all_of(clust_col), nonmembers_dist_min, nonmembers_within_25km_n, nonmembers_within_50km_n)
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
  
  #' For "active" tracks that are not part of a given cluster, relative to
  #' the cluster's centroid, find out:
  #' (i) the the closest location point, across all non-member active tracks
  #'(ii) how many tracks are within 25km and 50km
  if(nrow(nearpoints_dist) > 0){
    out <- nearpoints_dist |>
      st_drop_geometry() |>
      summarise(
        nonmembers_dist_min = min(dist, na.rm = TRUE) |> units::set_units("m"),
        nonmembers_within_25km_n = length(unique(.data[[trck_col]][dist < units::set_units(25, "km")])),
        nonmembers_within_50km_n = length(unique(.data[[trck_col]][dist < units::set_units(50, "km")]))
      )
  }else{
    out <- dplyr::tibble(
      nonmembers_dist_min = units::as_units(NA, "m"), 
      nonmembers_within_25km_n = 0L, 
      nonmembers_within_50km_n = 0L
    )
  }
  

  
  
  return(out)
}



#' //////////////////////////////////////////////////////////////////////////////#
#' Derive decimal time since start of the day (i.e. since midnight) from a timestamp
#' 
decimal_time <- function(x, unit = "hr"){
  
  #browser()
  
  if(!is.POSIXct(x)) cli::cli_abort("{.arg x} must be a {.cls POSIXct} object")
  
  # compute decimal hours
  dec_hr <- hour(x) + minute(x)/60 + second(x)/3600
  units(dec_hr) <- "h"
  
  # convert to required units
  units::set_units(dec_hr, value = unit, mode = "standard")

}


