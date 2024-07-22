

# Generate Avian Cluster Metrics

MoveApps

Github repository:
https://github.com/dmpstats/Generate_Avian_Cluster_Metrics

## Description

Generates cluster metrics from cluster-annotated locations data obtained
from an upstream clustering App. Returns cluster properties summarized
at track-per-cluster and/or whole-cluster levels.

## Documentation

This App calculates a range of metrics for clusters of animal locations
grouped based on spatio-temporal proximity. It is designed to be used
with tracking data that has been processed through a spatial clustering
App, requiring track locations to be labelled with cluster IDs.

Outputted metrics describe various cluster attributes including cluster
timespan, daily and compounded attendance levels, summaries of
behavioural activities within the cluster (if input data includes
annotations of animal behaviour), frequency and duration of cluster
visits, and information on first arrivals and night-time travelled
distances to the cluster. Calculations are performed at both
track-per-cluster and whole-cluster levels. A detailed description of
the cluster metrics generated at each aggregation level is provided
below in Section [Cluster Metrics](#cluster-metrics).

In practice, cluster attributes generated by this App can play a pivotal
role on inferring points of interest for ground patrolling purposes and
guiding response actions in animal protection efforts.

### MoveApps Worflow Dependencies

This App relies on the following prerequisites:

- Ensure the prior deployment of the App [Add Local and Solar
  Time](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b)
  ([GitHub](https://github.com/movestore/Convert-Times)) in the
  workflow, selecting the binding of local, sunset and sunrise times.

- The input data must contain a column providing cluster annotations for
  location points. Use Apps like [Avian Cluster
  Detection](https://www.moveapps.org/apps/browser/81f41b8f-0403-4e9f-bc48-5a064e1060a2)
  ([GitHub](https://github.com/dmpstats/Avian_Cluster_Detection))
  earlier in the workflow to detect and identify spatial clusters from
  tracking data\].

- Optionally, the App can produce behavioural-related cluster metrics if
  the input data includes the required information. Use Apps such as
  [Behavioural Classification for
  Vultures](https://www.moveapps.org/apps/browser/44bb2ffa-7d40-4fad-bff5-1269995ba1a2)
  ([GitHub](https://github.com/dmpstats/Behavioural_Classification_for_Vultures))
  earlier in the workflow to derive and bind behavioural categories to
  the input data.

### Input data

A `move2::move2_loc` object.

### Output data

A `move2::move2_loc` object. The format of the output dataset can be
either cluster-based tables or retain the same structure as the input
data (i.e. by track location event). See he [Settings](#settings)
section below for further details.

### Artefacts

None

### Settings

**Cluster ID Column** (`cluster_id_col`): character string, the name of
the column in the input data indicating the cluster IDs of location
points. Default: `"clust_id"`.

**Behaviour Category Column** (`behav_col`): character string, the name
of the column in the input data indicating the behavioural categories of
location points. Setting this to `NULL` will skip the derivation of
behavioural-related cluster metrics. Default: `"behav"`.

**Output Type** (`output_type`): Choose the format for outputting
derived cluster metrics. Two options available:

1.  As a compact cluster-based dataset (`"cluster"`, default) - see
    setting **Cluster-based Output Content** for additional options.
2.  Merge whole-cluster metrics to the track-location attributes
    provided by the input dataset (`"locs"`). Note: this option must be
    selected if using the ‘EarthRanger Integration’ App as a downstream
    App.

**Cluster-based Output Content** (`cluster_tbl_type`): If **Output
Type** is set to ‘Cluster-based’, choose the content of the output
dataset:

1.  Metrics at both track-per-cluster and whole-cluster levels
    (`"track-and-whole"`, default)
2.  Solely whole-cluster level metrics (`"whole-only"`)

### Most common errors

The app will halt processing an throw an error under the following
conditions:

- Failing to deploy the App ‘Add Local and Solar Time’ earlier in the
  workflow, as several of the calculated metrics rely on the presence of
  additional time-related columns in the input data.

- Specifying a **Cluster ID Column** or a **Behaviour Category Column**
  that is not included in the input data.

### Null or error handling

- **Behaviour Category Column**: if no column name is given (`NULL`),
  the app will not attempt to produce behavioural-based cluster metrics.

### Cluster Metrics

This section provides a detailed description of the cluster metrics
generated in the App [Generate Avian Cluster
Metrics](https://github.com/dmpstats/Generate_Avian_Cluster_Metrics),
including how each attribute is calculated.

#### Track-per-Cluster Level

The attributes listed below denote column names of a dataset where each
row represents a subset of track location points spatially grouped into
a cluster event. Track ID is extracted using the function
`move2::mt_track_id_column()`, while cluster ID is specified by the App
parameter `clust_id_col`. This dataset constitutes the events table of
the `move2_loc` output object when the App settings `output_type` is set
to `"cluster"` and `cluster_tbl_type` is defined to `"track-and-whole"`.

| Attribute Name                                                                          | Description                                                                                                                                                                                                                             | Calculation                                                                                                                                                                                                                                                                              |
|:----------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `all_points`                                                                            | A `MULTIPOINT` geometry feature providing track location points in the cluster                                                                                                                                                          | Spatial combination of track-level `POINT` geometries per cluster                                                                                                                                                                                                                        |
| `median_point`                                                                          | A `POINT` geometry locating the spatial median of track location points in the cluster                                                                                                                                                  | Geometric median obtained via the Weiszfeld algorithm ([`Gmedian::Weiszfeld`](https://rdrr.io/cran/Gmedian/man/Gmedian.html))                                                                                                                                                            |
| `pts_n`                                                                                 | The number of track location points in the cluster                                                                                                                                                                                      | Count number of track points in cluster                                                                                                                                                                                                                                                  |
| `pts_night_n` and `pts_day_n`                                                           | The number of night-time and day-time track location points in the cluster                                                                                                                                                              | Count track points in cluster grouped by column `nightpoint`[^1]                                                                                                                                                                                                                         |
| `first_dttm` and `first_dttm_local`                                                     | Timestamp of the first track point in the cluster, in UTC and local timezone, respectively                                                                                                                                              | Find the track’s earliest recorded locations in the cluster                                                                                                                                                                                                                              |
| `last_dttm` and `last_dttm_local`                                                       | Timestamp of the last track point in the cluster, in UTC and local timezone, respectively                                                                                                                                               | Find the track’s latest recorded locations in the cluster                                                                                                                                                                                                                                |
| `timespan`                                                                              | Total duration, in hours, over which the track visits the cluster, including periods where the track is absent from the cluster.                                                                                                        | The time difference between `first_dttm_local` and `last_dttm_local`                                                                                                                                                                                                                     |
| `timespan_ndays`                                                                        | Total number of integer days over which the track visits the cluster, including lags during which the track is absent from the cluster                                                                                                  | The integer difference between the date components of `first_dttm_local` and `last_dttm_local`                                                                                                                                                                                           |
| `days_present_n`                                                                        | The cumulative number of days on which the track was present in the cluster                                                                                                                                                             | The length of unique dates on which the track was assigned to the cluster                                                                                                                                                                                                                |
| `days_absent_n`                                                                         | The cumulative number of days where the track was absent from the cluster, having later revisited                                                                                                                                       | The difference between `timespan_ndays` and `days_present_n`                                                                                                                                                                                                                             |
| `hour_local_med`                                                                        | The median local hour of track locations in the cluster                                                                                                                                                                                 | Filter the local hour component of all track locations in cluster and calculate the median                                                                                                                                                                                               |
| `<behaviour-category>_drtn` \[e.g. `SFeeding_drtn`, `SResting_drtn`, `SRoosting_drtn`\] | The time spent by the track in each behavioural category while visiting the cluster (unit: hours)                                                                                                                                       | For each attributed behavioural class, sum over time lags between track points annotated with the cluster ID                                                                                                                                                                             |
| `attendance`                                                                            | The track’s total time spent at the cluster (units: hours)                                                                                                                                                                              | Sum over time lags between consecutive track location points within the cluster                                                                                                                                                                                                          |
| `attendance_dmean`                                                                      | The track’s daily mean attendance time at the cluster (unit: hours)                                                                                                                                                                     | Compute the time spent by the track in the cluster per day (excluding absent days), and compute the mean across visited days.                                                                                                                                                            |
| `attendance_daytime_dmean`                                                              | The track’s daily mean attendance time at the cluster during daytime, i.e. the period between sunset and sunrise (unit: hours)                                                                                                          | Identical to `mean_attendance`, but including only location events where `nightpoint == 0`                                                                                                                                                                                               |
| `visits_day_mean`                                                                       | The track’s mean number of unique visits per day to the cluster                                                                                                                                                                         | Run length calculations are applied to the track location points to quantify the number of cluster visits per day (excluding absent days), which are then averaged across all visited days.                                                                                              |
| `visit_drtn_mean`                                                                       | The mean duration of the track’s visits to the cluster (units: hours)                                                                                                                                                                   | Track revisits to the cluster are identified via run lengths, from which visit durations are calculated and the mean across visits is taken.                                                                                                                                             |
| `nightpts_dist_dmean`                                                                   | The track’s daily mean distance between night-time location points and the cluster’s centroid (unit: meters)                                                                                                                            | For each date the track visits the cluster, calculate the median distance between all night-time location points occurred on that date (regardless of cluster affiliation status) and the cluster centroid. Median distances are then averaged over all days to produce this metric[^2]. |
| `nightpts_250m_prop` and `nightpts_1km_prop`                                            | Mean proportion of all night-time track location points within, respectively, 250m and 1km from the cluster centroid, on a daily basis                                                                                                  | Using the same distances calculated for `nightpts_dist_dmean`, get the proportions of night points located within the considered proximity distances per day, before taking the mean across all visited days                                                                             |
| `arrival_dist_mean`                                                                     | The mean distance between track’s night-points and the centroid of the cluster on the date of arrival (i.e. first visit; unit: meters)                                                                                                  | Filter night-time location points spanning the date of the first visit to the cluster, and compute the mean distance to the cluster’s centroid[^3].                                                                                                                                      |
| `var_acc_<xyz>_med` and `var_acc_<xyz>_sd`                                              | Median and standard deviation, respectively, of Accelerometer variance across track location points within the cluster, for each active accelerometer axis (`x`, `y` and/or `z`). Only calculated when accelerometer data is available. | Median and SD of ACC variances on track points annotated with each cluster ID                                                                                                                                                                                                            |

#### Whole-Cluster Level

The following attributes are column names of a dataset where each row
represents the properties of a cluster event, which can comprise one or
multiple tracks at any given time during the cluster’s existence.
Cluster IDs are provided in the input data column specified by App
parameter `clust_id_col`.

The formatting of whole-cluster metrics in the App’s output is defined
by settings `output_type` and `cluster_tbl_type`:

- if `output_type` is set to `"cluster"`, whole-cluster metrics are
  provided as a dedicated table, with its placement in the `move2_loc`
  output object determined by the `cluster_tbl_type` setting:

  - if `"track-and-whole"` (default), metrics are provided as the track
    table
  - if `cluster_tbl_type = "whole-only"`, metrics are outputted as the
    events table

- if `output_type` is set to `"locs"`, then whole-metrics are included
  alongside track location attributes, with repeated values for location
  points associated with a common cluster ID.

> [!IMPORTANT]
>
> When `output_type` is set to `"locs"`, a prefix “`cl_`” is added to
> the names of all cluster-level properties to distinguish them from
> observation-specific attributes.

| Attribute Name                                                                                              | Description                                                                                                                                                                       | Calculation                                                                                                                                                                      |
|:------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `spawn_dttm` and `spawn_dttm_local`                                                                         | The ‘spawning’ date-time of the cluster event, in UTC and local timezone, respectively                                                                                            | The earliest timestamp of all point locations associated with the cluster.                                                                                                       |
| `cease_dttm` and `cease_dttm_local`                                                                         | The ‘ending’ date-time of the cluster event, in UTC and local timezone, respectively                                                                                              | The latest timestamp of all point locations associated with the cluster.                                                                                                         |
| `centroid`                                                                                                  | A `POINT` geometry providing the coordinates of the spatial median of the cluster                                                                                                 | Apply the Weiszfeld algorithm ([`Gmedian::Weiszfeld`](https://rdrr.io/cran/Gmedian/man/Gmedian.html)) to all location points associated with the cluster                         |
| `members_n`                                                                                                 | Number of tracks containing location points affiliated with the cluster event at any point of its lifespan                                                                        | Length of unique track IDs in the cluster                                                                                                                                        |
| `members_ids`                                                                                               | List of track IDs associated with the cluster event during its lifespan                                                                                                           |                                                                                                                                                                                  |
| `timespan`                                                                                                  | Total duration of the cluster event (unit: decimal days)                                                                                                                          | The difference between `cease_dttm_lcl` and `spawn_dttm_local`                                                                                                                   |
| `timespan_ndays`                                                                                            | Total number of integer days covered by the cluster event, including days without visiting tracks                                                                                 | The integer difference between the date components of `cease_dttm_lcl` and `spawn_dttm_local`                                                                                    |
| `days_active_n`                                                                                             | Number of days the cluster was active, i.e. visited by at least one track during the day                                                                                          | Length of unique dates of point locations affiliated to the cluster event                                                                                                        |
| `days_inactive_n`                                                                                           | Number of days the cluster was inactive, i.e. no track visited during the day                                                                                                     | The difference between `timespan_ndays` and `days_active_n`                                                                                                                      |
| `pts_n`                                                                                                     | The total number of location points comprised by the cluster                                                                                                                      | Count number of points in cluster across associated tracks                                                                                                                       |
| `pts_pairdist_mean`, `pts_pairdist_med` and `pts_pairdist_sd`                                               | Respectively, the mean, median and standard deviation of pairwise distances between location points comprised by the cluster (unit: meters)                                       | Summary statistics applied to all points within the cluster                                                                                                                      |
| `pts_spread_area`                                                                                           | The spread area of location points comprised by the cluster (units: km^2)                                                                                                         | The area of the polygon created from the convex hull of points in the cluster (calculated via [`sf::st_convex_hull`](https://r-spatial.github.io/sf/reference/geos_unary.html)). |
| `<behaviour-category>_drtn_cmpd` \[e.g. `SFeeding_drtn_cmpd`, `SResting_drtn_cmpd`, `SRoosting_drtn_cmpd`\] | The compounded time spent by visiting tracks in each each behavioural category during the cluster event                                                                           | The sum `<behaviour-category>_drtn` (e.g. `SFeeding_drtn`) across member tracks                                                                                                  |
| `hour_local_avg`                                                                                            | The average local hour of track visits throughout the cluster event                                                                                                               | The mean of `hour_local_med` across affiliated tracks                                                                                                                            |
| `attendance_cmpd`                                                                                           | The compounded time spent by member tracks in the cluster (units: hours)                                                                                                          | Sum of `attendance_aggr` across member tracks                                                                                                                                    |
| `attendance_davg`                                                                                           | The daily average attendance time by member tracks at the cluster (unit: hours)                                                                                                   | The average of `mean_attendance` across affiliated tracks                                                                                                                        |
| `attendance_daytime_davg`                                                                                   | The daily average attendance time spent by member tracks in the cluster during the daytime period (unit: hours)                                                                   | The average of `mean_attendance_daytime` across member tracks                                                                                                                    |
| `visits_day_avg`                                                                                            | The average number of unique visits per day by member tracks                                                                                                                      | The average `visits_day_mean` across member tracks                                                                                                                               |
| `visit_drtn_avg`                                                                                            | The average visit duration of member tracks to the cluster (units: hours)                                                                                                         | The average `visit_drtn_mean` across member tracks                                                                                                                               |
| `nightpts_dist_davg`                                                                                        | The daily average distance between member tracks’ night-point locations and the cluster centroid (unit: meters)                                                                   | The average `nightpts_dist_dmean` across member tracks                                                                                                                           |
| `nightpts_250m_avgprop` and `nightpts_1km_avgprop`                                                          | Average proportion of member track’s night-point locations within, respectively, 250m and 1km from the cluster centroid, on a daily basis                                         | The averages of `nightpts_250m_prop` and `nightpts_1km_prop` across member tracks                                                                                                |
| `arrival_dist_avg`                                                                                          | The average distance between member tracks’ night-points and the cluster centroid on the date of arrival (i.e. first visit; unit: meters)                                         | The average of `arrival_dist_mean` across affiliated tracks                                                                                                                      |
| `members_centroid_pairdist_mean`, `members_centroid_pairdist_med` and `members_centroid_pairdist_sd`        | Respectively, the mean, median and standard deviation of pairwise distances between track-level centroids of location points in the cluster (unit: meters)                        | Summary statistics applied to `median_point`s within the cluster                                                                                                                 |
| `nonmembers_dist_min`                                                                                       | The minimum distance between a **non-member** location point and the cluster centroid, during the 2 weeks leading up to and throughout the cluster event (unit: meters)           | Find the closest distance from non-member location points to the cluster centroid occurring between `spawn_dttm_local - lubridate::days(14)` and `cease_dttm_lcl`                |
| `nonmembers_within_25km_n` and `nonmembers_within_50km_n`                                                   | Number of non-member tracks with location points within, respectively, 25km and 50km from the cluster centroid, during the 2 weeks leading up to and throughout the cluster event | Count the non-member track IDs that have any locations within 25km and 50km of the cluster centroid between 14 days before `spawn_dttm_local` and `cease_dttm_lcl`               |

[^1]: If the `nightpoint` column is not present in the input data, it
    will derived internally based on `sunrise_timestamp` and
    `sunset_timestamp` columns. These columns are prerequisites that
    must be added using the ‘Add Local and Solar Time’ App.

[^2]: In the absence of night-point locations for a given date for the
    track forming/visiting the cluster, take the track’s first point of
    the next day to calculate the intended night-time distance. Beware:
    NAs can still occur when there is no next day data available for the
    track.

[^3]: In the absence of night-point locations on the date of arrival of
    the track forming/visiting the cluster, take the track’s last point
    of the previous day to calculate the intended arrival distance.
    Beware: NAs can still occur when there is no track data on the
    previous day.
