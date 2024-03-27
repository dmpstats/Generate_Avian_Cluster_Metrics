library('move2')
library('lubridate')


library('move2')
library('lubridate')
library('dplyr')
library('magrittr')
library('tidyr')
library('Gmedian')
library('sf')
library('stringr')
library('units')
library('pbapply')

# Shortened 'not in':
`%!in%` <- Negate(`%in%`)

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
      group_map(
        ~ Gmedian::Weiszfeld(.)$median 
      ) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      st_as_sf(coords = colnames(.), crs = st_crs(data)) %>%
      st_geometry()
  }
  
  return(med)
  
}


# Main Function: ------------------------


rFunction = function(data, clustercode = "") {
  
  # Input checks ----------------------------------------
  if ("xy.clust" %!in% colnames(data)) {
    logger.fatal("Input data does not have column 'xy.clust'. Please use 'Avian Cluster Detection' MoveApp prior to this in the workflow.")
  }
  
  #' ----------------------------------------------------
  # 8. Generate final clustertable ----------------------
  # This will contain output cluster information
  
  tablestarttime <- Sys.time()
  clustertable <- data %>%
    filter(!is.na(xy.clust)) %>%
    mutate(ID = mt_track_id(.), 
           timestamp = mt_time(.),
           timediff_hrs = mt_time_lags(.) %>% 
             units::set_units("minutes") %>% 
             units::drop_units()/60,) %>%
    #as.data.frame() %>%
    group_by(xy.clust, ID) %>%
    summarise(
      geometry = st_combine(geometry),
      all_cluster_points = geometry,
      
      # Time calculations:
      firstdatetime = min(timestamp),
      lastdatetime = max(timestamp),
      days = length(unique(lubridate::date(timestamp))),
      totalduration = (ceiling_date(lastdatetime, unit = "days") - floor_date(firstdatetime, unit = "days")) %>% as.integer(),
      daysempty = as.numeric(totalduration - days),
      daysemptyprop = daysempty / totalduration,
      no_nightpoints = all(nightpoint == 0),
      
      # Behavioural data:
      Total = n(),
      SFeeding = sum(behav == "SFeeding"),
      SRoosting = sum(behav == "SRoosting"),
      SResting = sum(behav == "SResting"),
      MedianHourFeed = median(hour[behav == "SFeeding"], na.rm = T),
      MedianHourDay = median(hour[hour > 5 & hour < 17], na.rm = T),
      
      # Accelerometer calculations
      # med_var_x = case_when(
      #   "var_acc_x" %in% colnames(data) ~ median(var_acc_x, na.rm = T),
      #   TRUE ~ NA
      # ),
      # med_var_y = case_when(
      #   "var_acc_y" %in% colnames(data) ~ median(var_acc_y, na.rm = T),
      #   TRUE ~ NA
      # ),
      # med_var_z = case_when(
      #   "var_acc_z" %in% colnames(data) ~ median(var_acc_z, na.rm = T),
      #   TRUE ~ NA
      # ),
      # sd_var_x = case_when(
      #   "var_acc_x" %in% colnames(data) ~ sd(var_acc_x, na.rm = T),
      #   TRUE ~ NA
      # ),
      # sd_var_y = case_when(
      #   "var_acc_y" %in% colnames(data) ~ sd(var_acc_y, na.rm = T),
      #   TRUE ~ NA
      # ),
      # sd_var_z = case_when(
      #   "var_acc_z" %in% colnames(data) ~ sd(var_acc_z, na.rm = T),
      #   TRUE ~ NA
      # )
      
      .groups = "keep"
    ) %>%
    st_as_sf(crs = st_crs(data))
  
  
  clustertable %<>% mt_as_move2(time_column = "firstdatetime", track_id_column = "xy.clust") # switch to move2 for ease
  
  # Generate distance data:
  logger.trace(paste0("Generating distance data for all clusters. This may run slowly"))
  
  
  ## CLUSTERTABLE DATA PREP ---------------------------------------------------
  
  
  # Reformat data for clustertable calculations
  
  # Update geometry column to be geometric medians 
  # These are per-bird, not per-cluster
  clustertable %<>% st_set_geometry(
    calcGMedianSF(.) %>%
      st_geometry()
  )
  
  # Add second geometry column for cluster-wide centroids
  # These are per-cluster, not per-bird
  wholeclusts <- clustertable %>%
    group_by(xy.clust) %>%
    summarise(
      geometry = st_combine(geometry),
      .groups = "keep"
    ) %>%
    st_set_geometry(
      calcGMedianSF(.) %>%
        st_geometry() 
    ) %>%
    rename(wholeclust_geometry = geometry) %>%
    as.data.frame()
  clustertable %<>% left_join(wholeclusts, by = "xy.clust")
  
  # Create matrix data for filtering speed
  mat.data <- data %>%
    mutate(ID = mt_track_id(.),
           timestamp = mt_time(.)) %>%
    as.data.frame() %>%
    dplyr::select(any_of(c("ID",
                           "geometry",
                           "timestamp",
                           "hour",
                           "dist_m",
                           "behav",
                           "sunrise_timestamp",
                           "sunset_timestamp",
                           "timediff_hrs",
                           "xy.clust",
                           "nightpoint"))) %>%
    st_as_sf(crs = st_crs(data))
  
  
  ## FURTHER CLUSTERTABLE ATTRIBUTES -------------------------------------------
  
  ### a. Calculate accelerometer data if ACC is available
  if ("var_acc_x" %in% colnames(data)) {
    
    logger.trace("   Accelerometer columns identified. Calculating ACC summaries")
    
    clustertable_acc <- data %>%
      filter(!is.na(xy.clust)) %>%
      mutate(ID = mt_track_id(.), 
             timestamp = mt_time(.),
             timediff_hrs = mt_time_lags(.) %>% 
               units::set_units("minutes") %>% 
               units::drop_units()/60,) %>%
      #as.data.frame() %>%
      group_by(xy.clust, ID) %>%
      summarise(
        # Accelerometer calculations
        med_var_x = median(var_acc_x, na.rm = T),
        med_var_y = median(var_acc_y, na.rm = T),
        med_var_z = median(var_acc_z, na.rm = T),
        
        sd_var_x = sd(var_acc_x, na.rm = T),
        sd_var_y = sd(var_acc_y, na.rm = T),
        sd_var_z = sd(var_acc_z, na.rm = T),
        
        .groups = "keep"
      )
  } else {
    logger.trace("   No accelerometer data identified. Skipping ACC summaries")
  }
  
  ### b. Time-at-Carcass Calculations -----------------------------------------
  logger.trace("   Generating [a. Time-at-Carcass data]")
  
  timeAtCarcTab <- function(clustdat) {
    
    carctime <- clustdat %>%
      filter(!is.na(xy.clust)) %>%
      as.data.frame() %>%
      group_by(xy.clust, ID, date(timestamp)) %>%
      # Remove all final points to prevent overnight locations messing things up:
      filter(row_number() != n()) %>%
      summarise(count = n(),
                time_spent = sum(timediff_hrs),
                time_spent_day = sum(timediff_hrs * !nightpoint),
                .groups = "keep"
      ) %>%
      
      group_by(xy.clust, ID) %>%
      summarise(
        meanvisit_time = mean(time_spent, na.rm = T),
        meanvisit_time_day = mean(time_spent_day, na.rm = T),
        .groups = "keep") 
    return(carctime)
  }
  carctime <- timeAtCarcTab(mat.data)
  
  
  ### c. Revisitation Calculations --------------------------------------------
  logger.trace("   Generating [b. Revisitation data]")
  
  
  revisitTab <- function(clustdat, clustertable) {
    
    revisit_calc <- function(row) {
      
      clust <- row$xy.clust
      bird <- row$ID
      firstdate <- as_datetime(row$firstdatetime) 
      lastdate <- as_datetime(row$lastdatetime)
      
      nearpoints <- clustdat %>% 
        filter(
          between(timestamp, firstdate, lastdate),
          ID == bird
        ) %>%
        mutate(incluster = ifelse(xy.clust == clust & !is.na(xy.clust), 1, 0),
               indaycluster = ifelse(incluster == 1 & nightpoint == 0, 1, 0)
        ) %>%
        group_by(date(timestamp)) %>%
        summarise(
          visits = sum(rle(incluster)$values),
          dayvisits = sum(rle(indaycluster)$values),
          dayvisits = pmin(dayvisits, visits), # fix case where dayvisits > visits
          .groups = "keep" 
        ) %>%
        ungroup() %>%
        summarise(
          meanvisits = mean(visits, na.rm = T),
          meandayvisits = mean(dayvisits, na.rm = T),
          .groups = "keep" 
        )
      
      return(c(
        nearpoints$meanvisits,
        nearpoints$meandayvisits
      ))
    }
    
    revdat <- pbapply(clustertable, 1, revisit_calc) %>% 
      t() %>%
      as.data.frame() %>%
      rename(meanvisits = V1, meanvisits_daytime = V2)
    outclusts <- cbind(clustertable, revdat) %>%
      as.data.frame() %>%
      dplyr::select(c("xy.clust", "ID", "meanvisits", "meanvisits_daytime"))
    return(outclusts)
  }
  
  revisits <- revisitTab(mat.data, clustertable) 
  
  
  
  ### d. Night-Distance Calculations ------------------------------------------
  
  
  
  logger.trace("   Generating [c. Night-distance data]")
  
  nightTab <- function(clustdat, clustertable) {
    
    # ALTERNATIVE METHOD TEST
    # Firstly, filter to all night locations
    nightpts <- clustdat %>% 
      dplyr::select(-xy.clust) %>%
      as.data.frame() %>%
      filter(nightpoint == 1) %>%
      mutate(date = case_when(
        # Fix night locations being 'split' by midnight:
        # If before midnight, associate it with that same date
        hour(timestamp) > 12 ~ date(timestamp),
        # But if in the morning, associate it with the night before
        hour(timestamp) < 12 ~ date(timestamp) - days(1)
      ))
    
    # Generate second table:
    # clust-bird-date, one entry for each clust visited by a bird on each date
    clustdays <- clustdat %>%
      as.data.frame() %>%
      filter(!is.na(xy.clust)) %>%
      group_by(xy.clust, ID, date(timestamp)) %>%
      summarise() %>%
      rename(date = `date(timestamp)`) %>%
      
      # Bind clust centroid data:
      left_join(
        clustertable %>%
          as.data.frame() %>%
          dplyr::select(c("xy.clust", "wholeclust_geometry")) %>%
          .[!duplicated(.),] %>%
          st_as_sf(crs = st_crs(data)),
        by = "xy.clust", relationship = "many-to-many")  %>%
      .[!duplicated(.),] 
    
    # The following table contains all night locations 
    # and has matched them to a cluster visited by a bird on that same day.
    # Where more than 1 cluster is visited by a bird within a day, the
    # night location has been duplicated (once for each cluster) so that it can be grouped more than once.
    night_table <- left_join(nightpts, clustdays, by = c("ID", "date"), relationship = "many-to-many") %>% 
      filter(!is.na(xy.clust)) 
    
    # Now we introduce a distance column:
    dists <- pbapply::pbmapply(st_distance, night_table$geometry, night_table$wholeclust_geometry)
    nightdists <- cbind(night_table, dists)
    
    # Testing a new variable: proportion of nearby night points 
    # This is the proportion of night points on the same day as this cluster 
    # within 250m
    nearnights <- nightdists %>%
      group_by(xy.clust, ID) %>%
      summarise(near_night_prop = sum(dists < 250) / n())
    
    
    # Group by clust-ID-date and summarise, taking median first
    nightdists_by_day <- nightdists %>%
      group_by(ID, date, xy.clust) %>%
      summarise(nightdist = median(dists, na.rm = T), .groups = "keep")
    
    # Finally, take mean per bird across several days
    nightdists_bird_clust <- nightdists_by_day %>%
      group_by(xy.clust, ID) %>%
      summarise(nightdist_med = mean(nightdist, na.rm = T), .groups = "keep") %>%
      left_join(nearnights, by = c("xy.clust", "ID"))
    
    return(nightdists_bird_clust)
    
  }
  
  nightdists <- nightTab(mat.data, clustertable)
  
  
  
  ### e. Arrival-Distance Calculations ---------------------------------------
  logger.trace("   Generating [d. Arrival-Distance data]")
  
  arrivalTab <- function(clustdat, clustertable) {
    
    clustarrivals <- clustdat %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      group_by(xy.clust, ID) %>%
      summarise(day_of_arrival = min(date(timestamp)), .groups = "keep") %>%
      rename(birdID = ID) %>%
      left_join(
        clustertable %>%
          as.data.frame() %>%
          dplyr::select(c("xy.clust", "wholeclust_geometry")) %>%
          .[!duplicated(.),] %>%
          st_as_sf(crs = st_crs(data)),
        by = "xy.clust", relationship = "many-to-many") %>%
      .[!duplicated(.),] %>%
      st_set_geometry(.$wholeclust_geometry) %>%
      dplyr::select(-wholeclust_geometry)
    
    genClustDists <- function(row) {
      
      clust <- row$xy.clust
      bird <- row$birdID
      arrivaldate <- row$day_of_arrival %>% as_date()
      clustgeometry <- row$wholeclust_geometry %>%
        st_sfc(crs = st_crs(clustdat))
      
      arrivaldist <- clustdat %>%
        filter(between(
          timestamp,
          arrivaldate - hours(12), 
          arrivaldate + hours(12)
        ),
        nightpoint == 1,
        ID == bird) %>%
        mutate(
          dist = st_distance(geometry, clustgeometry)
        ) %>%
        .$dist %>%
        mean(na.rm = T)
      
      return(arrivaldist)
    }
    arrivaldat <- pbapply::pbapply(clustarrivals, 1, genClustDists)
    
    outdat <- clustarrivals %>%
      ungroup() %>%
      mutate(arrivaldists = arrivaldat) %>%
      st_drop_geometry() %>%
      rename(ID = birdID) %>%
      dplyr::select(-c("day_of_arrival"))
    
    return(outdat)
  }
  arrivaldists <- arrivalTab(mat.data, clustertable)
  
  
  ### f. Nearest-Tag Calculations --------------------------------------------
  logger.trace("   Generating [d. Nearest-Tag data]")
  
  nearBirdsTab <- function(clustdat, clustertable) {
    
    # List all clusters
    topclusters <- clustertable %>% 
      ungroup() %>%
      st_set_geometry(.$wholeclust_geometry) %>%
      group_by(xy.clust) %>%
      summarise(
        birds = paste(unique(ID), collapse = ", "),
        firstdatetime = min(firstdatetime, na.rm = T),
        lastdatetime = max(lastdatetime, na.rm = T),
        nbirds = length(unique(ID)),
        .groups = "keep" 
      )
    
    distvals <- function(row) {
      
      clustID <- row$xy.clust
      firstdatetime <- row$firstdatetime %>% as_datetime()
      lastdatetime <- row$lastdatetime %>% as_datetime()
      clustgeometry <- row$geometry %>%
        st_sfc(crs = st_crs(clustdat))
      
      nearpoints <- clustdat %>%
        filter(between(timestamp, firstdatetime - days(14), lastdatetime)) 
      
      activetags <- nearpoints %>%
        filter(between(timestamp, firstdatetime, lastdatetime)) %>%
        .$ID %>%
        unique()
      
      nearpoints2 <- nearpoints %>% 
        mutate(
          dist = st_distance(geometry, clustgeometry)
        ) %>% 
        filter(ID %in% activetags,
               ID %!in% row$birds
        )
      
      mindist_m <- min(nearpoints2$dist, na.rm = T) %>%
        units::set_units("metres") %>%
        units::drop_units()
      within_25k <- length(unique(nearpoints2$ID[nearpoints2$dist < units::set_units(25, "kilometres")]))
      within_50k <- length(unique(nearpoints2$ID[nearpoints2$dist < units::set_units(50, "kilometres")]))
      
      return(c(mindist_m, within_25k, within_50k))
    }
    
    birddat <- pbapply::pbapply(topclusters, 1, distvals) %>%
      t() %>%
      as.data.frame() %>%
      rename(
        mindist_m = V1,
        within_25k = V2, 
        within_50k = V3
      )
    
    topclusters_final <- cbind(topclusters, birddat) %>%
      as.data.frame() %>%
      dplyr::select(c("xy.clust", "birds", "mindist_m", "within_25k", "within_50k"))
    return(topclusters_final)
  }
  nearbirds <- nearBirdsTab(mat.data, clustertable)
  
  ### g. Stack outputs into final clustertable ---------------------------------
  logger.trace("   Merging [a-f] into primary clustertable")
  
  clustertable %<>%
    left_join(carctime, by = c("xy.clust", "ID")) %>%
    left_join(revisits, by = c("xy.clust", "ID")) %>%
    left_join(nightdists, by = c("xy.clust", "ID")) %>%
    left_join(arrivaldists, by = c("xy.clust", "ID")) %>%
    left_join(nearbirds, by = "xy.clust") %>%
    mt_as_move2(time_column = "firstdatetime", track_id_column = "xy.clust")
  
  if ("var_acc_x" %in% colnames(data)) {
    clustertable %<>% 
      left_join(st_drop_geometry(clustertable_acc) %>% 
                  dplyr::select(c("xy.clust", "med_var_x", "med_var_y", "med_var_z", "sd_var_x", "sd_var_y", "sd_var_z")), 
                by = c("xy.clust", "ID"))
  }
  
  
  ## 9. Finalise Outputs ---------------------------------------------------------
  
  # Finally, remove 1-location clusters from tagdata and clustertable
  rem <- clustertable$xy.clust[clustertable$Total == 1]
  data %<>% mutate(
    xy.clust = case_when(
      xy.clust %in% rem ~ NA,
      TRUE ~ xy.clust
    )
  ) %>%
    dplyr::select(-c("ID", "X", "Y"))
  clustertable %<>% filter(xy.clust %!in% rem)
  
  
  # Looping complete - log time and release outputs
  #tableendtime <- Sys.time()
  #logger.trace(paste0("Clustertable generation completed. Time taken: ", 
  #                    difftime(tableendtime, tablestarttime, units = "mins"), " mins."))
  
  clustertable %<>% as.data.frame() %>% st_as_sf() # temporarily convert to DF to add clustercodes (Move object creates errors)
  logger.trace(paste0("Clustertable is size ", object.size(clustertable) %>% format(units = "Mb")))
  
  # Fix to remove overwritten clusters from clustertable:
  logger.trace(paste0("Removing clusters that have since been overwritten in the tagdata: ", 
                      toString(
                        clustertable$xy.clust[which(clustertable$xy.clust %!in% data$xy.clust)]
                      )))
  
  # Add clustercode:
  clustertable %<>% mutate(xy.clust = ifelse(!is.na(xy.clust), paste0(clustercode, xy.clust), NA)) %>%
    mt_as_move2(time_column = "firstdatetime", track_id_column = "xy.clust")  %>%
    mt_as_track_attribute(c("birds", "mindist_m", "within_25k", "within_50k")) %>%
    st_set_geometry(.$wholeclust_geometry) %>% # change geometry to be whole-cluster centroid (the same location will be shared by several rows)
    dplyr::select(-"wholeclust_geometry")
  data %<>% mutate(xy.clust = ifelse(!is.na(xy.clust), paste0(clustercode, xy.clust), NA))
  
  
  # Add clustertable as an attribute of the main dataset
  attr(data, "cluster_tbl") <- clustertable
  
  # Release outputs
  saveRDS(clustertable, file = appArtifactPath("clustertable.rds")) 
  return(data)
  
  
}
