# Patient Event Sequencing – PCORnet

Patient Event Sequencing – PCORnet

## Usage

``` r
pes_process_pcornet(
  cohort,
  user_cutoff = 30,
  n_event_a = 1,
  n_event_b = 1,
  pes_event_file = patienteventsequencing::pes_event_file,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  age_groups = NULL,
  patient_level_tbl = FALSE,
  p_value = 0.9,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  cohort for SQUBA testing; required fields:

  - `site`

  - `patid`

  - `start_date`

  - `end_date`

- user_cutoff:

  user selected number of days between events to be used as a threshold
  cutoff for analyses

- n_event_a:

  the number of times event A should occur before establishing the index
  date; defaults to 1

- n_event_b:

  the number of times event B should occur before establishing the
  occurrence date; defaults to 1

- pes_event_file:

  CSV file with definitions of each of the events

- multi_or_single_site:

  direction to determine what kind of check to run string that is either
  `multi` or `single`

- anomaly_or_exploratory:

  direction to determine what kind of check to run; a string that is
  either `anomaly` or `exploratory`

- age_groups:

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and include it as the
  `age_groups` function parameter:

  - `min_age`: the minimum age for the group (i.e. 10)

  - `max_age`: the maximum age for the group (i.e. 20)

  - `group`: a string label for the group (i.e. 10-20, Young Adult,
    etc.)

  If you would *not* like to stratify by age group, leave the argument
  as NULL

- patient_level_tbl:

  logical to define whether an intermediate table with patient level
  output should be returned

- p_value:

  the p value to be used as a threshold in the multi-site anomaly
  detection analysis

- time:

  logical to determine whether to output the check across time

- time_span:

  when `time = TRUE`, a vector of two dates for the observation period
  of the study

- time_period:

  when time = TRUE, this argument defines the distance between dates
  within the specified time period. defaults to `year`, but other time
  periods such as `month` or `week` are also acceptable

## Value

dataframe with the number of days between events A and B as an integer,
and the number of patients who had the events occur that far apart;

        over time analyses will return the same output, grouped by each time
        period in the time span provided
