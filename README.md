

# Generate Avian Cluster Metrics

MoveApps

Github repository:
https://github.com/dmpstats/Generate_Avian_Cluster_Metrics

## Description

Generates cluster metrics from cluster-annotated locations data obtained
from an upstream clustering App. Returned cluster properties are
provided on the basis of summaries performed at track-per-cluster and/or
whole-cluster level.

## Documentation

This App is tasked with calculating cluster properties

At the practical level (real world relevance), generated cluster
attributes are pivotal for inferring points of interest for ground
patrolling purposes.

A description of the cluster attributes generated at each aggregation
level is provided below in Section [Cluster
Metrics](#list-of-cluster-metrics).

### MoveApps Worflow Dependencies

This App relies on the following prerequisites:

- Ensure the prior deployment of the App [Add Local and Solar
  Time](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b)
  ([GitHub](https://github.com/movestore/Convert-Times)) in the
  workflow.

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

A `move2::move2_loc` object.

### Artefacts

None

### Settings

**Cluster ID Column** (`cluster_id_col`): character string, the name of
the column in the input data indicating the cluster IDs of location
points. Default: `clust_id`.

**Behaviour Category Column** (`behav_col`): character string, the name
of the column in the input data indicating the behavioural categories of
location points. Setting this to `NULL` will skip the derivation of
behavioural-related cluster metrics. Default: `behav`.

**Cluster Metrics Aggregation Level** (`cluster_tbl_type`): choose
whether to generate metrics at both the track-per-cluster level and
whole-cluster levels (`"track-and-whole"`) or solely at the
whole-cluster level (`"whole-only"`). Default: `"track-and-whole"`.

### Most common errors

*Please describe shortly what most common errors of the App can be, how
they occur and best ways of solving them.*

### Null or error handling

*Please indicate for each setting as well as the input data which
behaviour the App is supposed to show in case of errors or NULL
values/input. Please also add notes of possible errors that can happen
if settings/parameters are improperly set and any other important
information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given,
the input data set is returned with a warning. If no radius is given
(NULL), but a duration is defined then a default radius of 1000m = 1km
is set.

<!-- generating data on the cluster's properties (hours spent, number of revisits, number of animals, and so on) -->
<!-- At the end of the process, a cluster-table (detailing various cluster attributes) is also generated and released as a *move2* object. -->

### Cluster Metrics

This section provides a full description of the cluster metrics
generated in the App [Generate Avian Cluster
Metrics](https://github.com/dmpstats/Generate_Avian_Cluster_Metrics),
including how each attribute is calculated and their significance.

#### Track-per-Cluster Level

The attributes listed below denote column names of a dataset where each
row represents a subset of track location points spatially grouped into
a cluster event. Track ID is extracted using the function
`move2::mt_track_id_column()`, while cluster ID is specified by the App
parameter `clust_id_col`. This dataset constitutes the events table of
the `move2_loc` output object when the App setting `cluster_tbl_type` is
defined to `"track-and-whole"`.

| Attribute Name                                                    | Description                                                                                                                                                                                                                                                                            | Calculation                                                                                                                                                                                                                                                   |
|:------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `all_points`                                                      | A `MULTIPOINT` geometry feature providing track location points in the cluster                                                                                                                                                                                                         | Spatial combination of track-level `POINT` geometries per cluster                                                                                                                                                                                             |
| `median_point`                                                    | A `POINT` geometry locating the spatial median of track location points in the cluster                                                                                                                                                                                                 | Geometric median obtained via the Weiszfeld algorithm ([`Gmedian::Weiszfeld`](https://rdrr.io/cran/Gmedian/man/Gmedian.html))                                                                                                                                 |
| `n_pts`                                                           | The number of track location points in the cluster                                                                                                                                                                                                                                     | Count number of track points in cluster                                                                                                                                                                                                                       |
| `n_pts_night` and `n_pts_day`                                     | The number of night-time and day-time track location points in the cluster                                                                                                                                                                                                             | Count track points in cluster grouped by column `nightpoint`                                                                                                                                                                                                  |
| `first_dttm_local` and `last_dttm_local`                          | The timestamps (local timezone) of the, respectively, first and final track points in the cluster                                                                                                                                                                                      | Find the track’s earliest and latest recorded locations in the cluster                                                                                                                                                                                        |
| `duration_hrs`                                                    | Total duration, in hours, over which the track visits the cluster, including periods where the track is outside the cluster.                                                                                                                                                           | The time difference between `first_dttm_local` and `last_dttm_local`                                                                                                                                                                                          |
| `n_days_span`                                                     | Total number of integer days over which the track visits the cluster, including lags during which the track is absent from the cluster                                                                                                                                                 | The integer difference between the date components of `first_dttm_local` and `last_dttm_local`                                                                                                                                                                |
| `n_days_unique`                                                   | The cumulative number of days on which the track was present in the cluster                                                                                                                                                                                                            | The length of unique dates on which the track was assigned to the cluster                                                                                                                                                                                     |
| `n_days_empty`                                                    | Number of days where the track was absent from the cluster, having later revisited                                                                                                                                                                                                     | The difference between `n_days_span` and `n_days_unique`                                                                                                                                                                                                      |
| `prop_days_empty`                                                 | The proportion of days where the track was absent from the cluster, having later revisited                                                                                                                                                                                             | The ratio between `n_days_empty` and `n_days_span`                                                                                                                                                                                                            |
| `<behaviour-category>` (e.g. `SFeeding`, `SResting`, `SRoosting`) | The total number of track location points assigned to each behavioural category within the cluster                                                                                                                                                                                     | Tally of track points annotated with the cluster ID AND each attributed behavioural class                                                                                                                                                                     |
| `med_feed_hour_local`                                             | The median local hour associated with ‘feeding’ behaviour across track location points within the cluster. This will not be generated if parameter `behav_col` is set to `NULL` or if a ‘feeding’ category cannot be found within the specified column through string pattern matching | Extract the local hour component of all track locations annotated with ‘feeding’ behaviour in the cluster, and calculate the median                                                                                                                           |
| `med_daytime_hour_local`                                          | The median local day-time hour of track locations in the cluster                                                                                                                                                                                                                       | Filter the local hour component of all track locations in cluster where column `nightpoint`[^1] equals `0`, and calculate the median                                                                                                                          |
| `meanvisit_duration`                                              | The track’s daily mean visit time to the cluster (unit: hours)                                                                                                                                                                                                                         | Calculate the time spent by the track in the cluster per day (excluding absent days), and compute the mean across visited days                                                                                                                                |
| `meanvisit_daytime_duration`                                      | The track’s daily mean visit time to the cluster during the daytime period (unit: hours)                                                                                                                                                                                               | Identical to `meanvisit_duration`, but including only location events where `nightpoint == 0`                                                                                                                                                                 |
| `mean_n_visits`                                                   | The track’s mean number of unique visits to the cluster per day                                                                                                                                                                                                                        | Run length calculations are applied to the track location points to quantify the number of cluster visits per day (excluding absent days), which are then averaged across all visited days.                                                                   |
| `mean_n_daytime_visits`                                           | The track’s mean number of unique visits to the cluster during the daytime period per day                                                                                                                                                                                              | Identical to `mean_n_visits`, but including only location events where `nightpoint == 0`                                                                                                                                                                      |
| `mean_night_dist`                                                 | The track’s daily mean (median) distance between all night-time location points and the cluster’s centroid (unit: meters)                                                                                                                                                              | For each day the track visits the cluster, calculate the median distance between all night-time location points (regardless of cluster affiliation status) and the cluster centroid. Median distances are then averaged over all days to produce this metric. |
| `night_prop_250m` and `night_prop_1km`                            | Mean proportion of all night-time track location points within, respectively, 250m and 1km from the cluster centroid per day                                                                                                                                                           | Using the same distances calculated for `mean_night_dist`, get the proportions of night points located within the considered proximity distances per day, before taking the mean across all visited days                                                      |
| `mean_arrival_dist`                                               | The mean distance between track’s night-points and the centroid of the cluster on the date of arrival (i.e. first visit)                                                                                                                                                               | Filter night-time location points spanning the date of the first visit to the cluster, and compute the mean distance to the cluster’s centroid                                                                                                                |

#### Whole-Cluster Level

The following attributes are column names of a dataset where each row
represents the properties of a cluster event, which can comprise one or
multiple tracks at any given time during the cluster’s existence.
Cluster IDs are provided in the input data column specified by App
parameter `clust_id_col`. The placement of this dataset in the output
`move2_loc` object is determined by the App setting `cluster_tbl_type`:

- if `"track-and-whole"` (default), it is provided as the track table
- if `cluster_tbl_type = "whole-only"`, is is outputted as the events
  table

| Attribute Name                                                         | Description | Calculation |
|:-----------------------------------------------------------------------|:------------|:------------|
| `spawn_dttm_lcl` and `spawn_dttm_lcl`                                  |             |             |
| `centroid`                                                             |             |             |
| `member_tracks_n`                                                      |             |             |
| `member_tracks_ids`                                                    |             |             |
| `n_days_active`                                                        |             |             |
| `n_days_inactive`                                                      |             |             |
| `span_days`                                                            |             |             |
| `prop_days_inactive`                                                   |             |             |
| `avg_med_feed_hour_local` and `avg_med_daytime_hour_local`             |             |             |
| `n_points`                                                             |             |             |
| `n_<behaviour-category> [e.g.`n_SFeeding`,`n_SResting`,`n_SRoosting`]` |             |             |
| `avg_visit_duration` and `avg_daytime_visit_duration`                  |             |             |
| `avg_n_visits` and `avg_n_daytime_visits`                              |             |             |
| `avg_nightime_dist`                                                    |             |             |
| `avg_nightime_prop_250m` and `avg_nightime_prop_1km`                   |             |             |
| `avg_arrival_dists`                                                    |             |             |
| `trks_mindist_m`                                                       |             |             |
| `trks_n_within_25km` and `trks_n_within_50km`                          |             |             |

[^1]: If the `nightpoint` column is not present in the input data, it
    will derived internally based on `sunrise_timestamp` and
    `sunset_timestamp` columns. These columns are prerequisites that
    must be added using the ‘Add Local and Solar Time’ App.
