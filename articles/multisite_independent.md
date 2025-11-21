# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

After following the instructions to reproduce the analysis, you will
also need to change the `output_function` column to tell the
`pes_output` function which check you executed. Reference the table
below for the labels that are associated with each check:

| Check Type                                     | output_function |
|:-----------------------------------------------|:----------------|
| Multi Site, Exploratory, Cross-Sectional       | pes_ms_exp_cs   |
| Multi Site, Exploratory, Longitudinal          | pes_ms_exp_la   |
| Multi Site, Anomaly Detection, Cross-Sectional | pes_ms_anom_cs  |
| Multi Site, Anomaly Detection, Longitudinal    | pes_ms_anom_la  |

## Multi-Site Exploratory Analysis

The process for the exploratory analysis is the same for both the
cross-sectional and longitudinal configurations.

First, execute either of the **Single Site, Exploratory** analyses,
configured appropriately for your study, against each data source.

``` r
library(patienteventsequencing)

my_table <- pes_process(cohort = my_cohort,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        time = T / F,
                        ...)
```

Then, combine these results into a single table with the different sites
delineated in the `site` column.

``` r
my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = '{see table above}')
```

## Multi-Site Anomaly Detection Analysis

For anomaly detection analysis, start by executing the same steps as the
exploratory analysis. Then, you will execute the relevant anomaly
detection algorithm against the resulting table. See below for the
different processes for cross-sectional and longitudinal analysis.

### Cross-Sectional

For a cross-sectional analysis, after some initial post-processing, the
`compute_dist_anomalies` and `detect_outliers` functions, both available
through the `squba.gen` package, should be executed against your
results. Copy the code below, inputting the table you generated.

The `p_value` can be selected by the user.

``` r
# First, uncount the table to simulate patient level counts and apply some post-processing
expanded_table <- my_table %>%
  uncount(pt_ct)

pts_without_events <- my_table %>%
  select(site, user_cutoff, total_pts, 
         event_a_name, event_b_name, pts_without_both) %>%
  pivot_longer(cols = 'pts_without_both',
               names_to = 'threshold_cutoff',
               values_to = 'n_pts_thrs') %>%
  mutate(prop_pts_thrs = round(n_pts_thrs / total_pts, 3)) %>% distinct()

date_threshold_cutoffs <- expanded_table %>%
  mutate(user_thrs = ifelse(abs(num_days) <= user_cutoff, 1, 0),
         thirty_thrs = ifelse(abs(num_days) <= 30, 1, 0),
         sixty_thrs = ifelse(abs(num_days) <= 60, 1, 0),
         ninety_thrs = ifelse(abs(num_days) <= 90, 1, 0),
         year_thrs = ifelse(abs(num_days) <= 365, 1, 0)) %>%
  pivot_longer(cols = c('user_thrs', 'thirty_thrs', 'sixty_thrs',
                        'ninety_thrs', 'year_thrs')) %>%
  group_by(site, user_cutoff, total_pts, name, event_a_name, event_b_name) %>%
  summarise(n_pts_thrs = sum(value, na.rm = TRUE)) %>%
  mutate(prop_pts_thrs = round(n_pts_thrs / total_pts, 3)) %>%
  rename('threshold_cutoff' = name) %>%
  union(pts_without_events)

# Then execute the compute_dist_anomalies function
df_start <- compute_dist_anomalies(df_tbl = date_threshold_cutoffs,
                                   grp_vars = c('threshold_cutoff'),
                                   var_col = 'prop_pts_thrs',
                                   denom_cols = c('threshold_cutoff', 
                                                  'total_pts'))

# Finally, use that output as input for the detect_outliers function
df_final <- detect_outliers(df_tbl = df_start,
                            tail_input = 'both',
                            p_input = p_value,
                            column_analysis = 'prop_pts_thrs',
                            column_variable = 'threshold_cutoff') %>%
  dplyr::mutate(output_function = '{see table above}')
```

### Longitudinal

For a longitudinal analysis, after some initial post-processing, the
`ms_anom_euclidean` function, available through the `squba.gen` package,
should be executed against your results. Copy the code below, inputting
the data you generated.

``` r
# First, uncount the table to simulate patient-level output and apply some post-processing
expanded_table <- my_table %>%
  uncount(pt_ct)

date_threshold_cutoffs <- expand_cts %>%
  mutate(user_thrs = ifelse(abs(num_days) <= user_cutoff, 1, 0)) %>%
  pivot_longer(cols = c('user_thrs')) %>%
  group_by(site, user_cutoff, total_pts, name, time_start, time_increment,
           event_a_name, event_b_name) %>%
  summarise(n_pts_thrs = sum(value, na.rm = TRUE)) %>%
  mutate(prop_pts_thrs = round(n_pts_thrs / total_pts, 3)) %>%
  rename('threshold_cutoff' = name) %>% ungroup()

# Then, pass this table into the Euclidean distance function
df <- ms_anom_euclidean(fot_input_tbl = date_threshold_cutoffs,
                        var_col = 'prop_pts_thrs',
                        grp_vars = c('site', 'threshold_cutoff', 'user_cutoff',
                                     'event_a_name', 'event_b_name')) %>%
  dplyr::mutate(output_function = '{see table above}')
```
