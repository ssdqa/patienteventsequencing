# Patient Event Sequencing

This is a plausibility module that will evaluate the sequence of two
clinical events to establish whether they occur in the expected order.
Users can define the clinical events in the `pes_event_file` input, an
example of which can be seen using `patienteventsequencing::`. This
function is compatible with both the OMOP and PCORnet CDMs based on the
user's selection.

## Usage

``` r
pes_process(
  cohort,
  user_cutoff = 30,
  n_event_a = 1,
  n_event_b = 1,
  pes_event_file = patienteventsequencing::pes_event_file,
  omop_or_pcornet,
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

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- user_cutoff:

  *integer* \|\| defaults to `30`

  An integer representing a custom number of days between events to be
  used as a threshold cutoff for analyses

- n_event_a:

  *integer* \|\| defaults to `1`

  An integer representing the number of times event A should occur
  before establishing the index date

- n_event_b:

  *integer* \|\| defaults to `1`

  An integer representing the number of times event B should occur
  before establishing the occurrence date

- pes_event_file:

  *tabular input* \|\| **required**

  A table with the definitions of each of the events. This table should
  contain two rows, one for each event, with the following columns:

  - `event` \| *character* \| a string, either A or B, representing the
    event type. A will be treated as the event that is expected to occur
    first in a sequence

  - `event_label` \| *character* \| a descriptive label for the event

  - `domain_tbl` \| *character* \| the name of the CDM table where the
    event is defined

  - `concept_field` \| *character* \| the string name of the field in
    the domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  - `codeset_name` \| *character* \| the name of the codeset that
    defines the event of interest

  - `filter_logic` \| *character* \| logic to be applied to the
    domain_tbl in order to achieve the definition of interest; should be
    written as if you were applying it in a dplyr::filter command in R

  To see an example of this input, see
  [`?patienteventsequencing::pes_event_file`](https://ssdqa.github.io/patienteventsequencing/reference/pes_event_file.md)

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`pes_process_omop()`](https://ssdqa.github.io/patienteventsequencing/reference/pes_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`pes_process_pcornet()`](https://ssdqa.github.io/patienteventsequencing/reference/pes_process_pcornet.md)
    function against a PCORnet CDM instance

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- patient_level_tbl:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether an additional table with patient level
  results should be output.

  If `TRUE`, the output of this function will be a list containing both
  the summary and patient level output. Otherwise, this function will
  just output the summary dataframe

- p_value:

  *numeric* \|\| defaults to `0.9`

  The p value to be used as a threshold in the Multi-Site, Anomaly
  Detection, Cross-Sectional analysis

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

## Value

This function will return a dataframe summarizing the distribution of
the days between each event. For a more detailed description of output
specific to each check type, see the PEDSpace metadata repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'patienteventsequencing'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'pes_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA
#> To see environment settings, run `get_argos_default()`

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Build function input table
pes_events <- tidyr::tibble(event = c('a', 'b'),
                            event_label = c('hypertension', 'inpatient/ED visit'),
                            domain_tbl = c('condition_occurrence', 'visit_occurrence'),
                            concept_field = c('condition_concept_id', 'visit_concept_id'),
                            date_field = c('condition_start_date', 'visit_start_date'),
                            vocabulary_field = c(NA, NA),
                            codeset_name = c('dx_hypertension', 'visit_edip'),
                            filter_logic = c(NA, NA))

#' Execute `pes_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
pes_process_example <- pes_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   user_cutoff = 10000,
                                   n_event_a = 1,
                                   n_event_b = 2,
                                   pes_event_file = pes_events)
#> Rows: 77 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (5): module, check, Always Required, Required for Check, Optional
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Joining with `by = join_by(person_id, start_date, end_date, site, site_summ, fu_diff, fu)`
#> Joining with `by = join_by(person_id)`
#>  -> copy_to
#> 2025-11-21 18:54:20.840348
#> Data: self$read_codeset(name, col_types = col_types, full_path = full_path)
#> Table name: dx_hypertension (temp: TRUE)
#> Data elements: concept_id,concept_code,concept_name,vocabulary_id,pcornet_vocabulary_id,cc_decimal_removal
#> Rows: 553
#> 2025-11-21 18:54:20.868224 ==> 0.02787566 secs
#> Joining with `by = join_by(person_id)`
#>  -> copy_to
#> 2025-11-21 18:54:21.126877
#> Data: self$read_codeset(name, col_types = col_types, full_path = full_path)
#> Table name: visit_edip (temp: TRUE)
#> Data elements: concept_id,concept_name
#> Rows: 3
#> 2025-11-21 18:54:21.152107 ==> 0.02522993 secs
#> Joining with `by = join_by(person_id, site_summ)`
#> Joining with `by = join_by(site_summ)`
#> Joining with `by = join_by(site_summ, total_pts)`
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `pes_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output                               │
#> │                                                               │
#> │ See ?pes_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

pes_process_example
#> # A tibble: 3 × 9
#>   site     num_days user_cutoff event_a_name event_b_name       pt_ct total_pts
#>   <chr>       <dbl>       <dbl> <chr>        <chr>              <int>     <int>
#> 1 combined     5628       10000 hypertension inpatient/ED visit     1        12
#> 2 combined     7637       10000 hypertension inpatient/ED visit     1        12
#> 3 combined    10800       10000 hypertension inpatient/ED visit     1        12
#> # ℹ 2 more variables: pts_without_both <int>, output_function <chr>

#' Execute `pes_output` function
pes_output_example <- pes_output(process_output = pes_process_example)

pes_output_example[[1]]
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

pes_output_example[[2]]


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(pes_output_example[[2]])

{"x":{"data":[{"orientation":"v","width":0.89999999999999991,"base":0,"x":[1],"y":[0],"text":"lb: 30 days<br />prop_pts_thrs: 0.000<br />lb: 30 days","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,77,111,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"30 days","legendgroup":"30 days","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000013,"base":0,"x":[2],"y":[0],"text":"lb: 60 days<br />prop_pts_thrs: 0.000<br />lb: 60 days","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(235,155,9,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"60 days","legendgroup":"60 days","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[3],"y":[0],"text":"lb: 90 days<br />prop_pts_thrs: 0.000<br />lb: 90 days","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(93,114,152,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"90 days","legendgroup":"90 days","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[4],"y":[0],"text":"lb: 1 year<br />prop_pts_thrs: 0.000<br />lb: 1 year","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(85,31,63,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"1 year","legendgroup":"1 year","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[5],"y":[0.16700000000000001],"text":"lb: User Threshold <br />10000 days<br />prop_pts_thrs: 0.167<br />lb: User Threshold <br />10000 days","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(189,119,122,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"User Threshold <br />10000 days","legendgroup":"User Threshold <br />10000 days","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":40.840182648401829,"r":7.3059360730593621,"b":37.260273972602747,"l":48.949771689497723},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Proportion of Patients where Events <br />Occur within Threshold Window","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,5.5999999999999996],"tickmode":"array","ticktext":["30 days","60 days","90 days","1 year","User Threshold <br />10000 days"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["30 days","60 days","90 days","1 year","User Threshold <br />10000 days"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Threshold","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.0083500000000000015,0.17535000000000001],"tickmode":"array","ticktext":["0.00","0.05","0.10","0.15"],"tickvals":[0,0.05000000000000001,0.10000000000000002,0.15000000000000002],"categoryorder":"array","categoryarray":["0.00","0.05","0.10","0.15"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Proportion Patients","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"19935184008f":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"19935184008f","visdat":{"19935184008f":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
