
#' Patient Event Sequencing
#'
#' @param cohort cohort for SSDQA testing; required fields:
#' - `site`
#' - `person_id`
#' - `start_date`
#' - `end_date`
#' @param user_cutoff user selected number of days between events to be used
#'                    as a threshold cutoff for analyses
#' @param n_event_a the number of times event A should occur before establishing
#'                  the index date; defaults to 1
#' @param n_event_b the number of times event B should occur before establishing
#'                  the occurrence date; defaults to 1
#' @param pes_event_file CSV file with definitions of each of the events
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [pes_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [pes_process_pcornet()] function against a PCORnet CDM instance
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory direction to determine what kind of check to run; a string
#'                               that is either `anomaly` or `exploratory`
#' @param age_groups If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age`: the minimum age for the group (i.e. 10)
#' - `max_age`: the maximum age for the group (i.e. 20)
#' - `group`: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param patient_level_tbl logical to define whether an intermediate table with
#'                         patient level output should be returned
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param time logical to determine whether to output the check across time
#' @param time_span when `time = TRUE`, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return dataframe with the number of days between events A and B as an integer,
#'         and the number of patients who had the events occur that far apart;
#'
#'         over time analyses will return the same output, grouped by each time
#'         period in the time span provided
#'
#' @example inst/example-pes_process_output.R
#'
#' @export
#'
pes_process<- function(cohort,
                       user_cutoff = 30,
                       n_event_a = 1,
                       n_event_b = 1,
                       pes_event_file = patienteventsequencing::pes_event_file,
                       omop_or_pcornet = 'omop',
                       multi_or_single_site = 'single',
                       anomaly_or_exploratory='exploratory',
                       age_groups = NULL,
                       patient_level_tbl = FALSE,
                       p_value = 0.9,
                       time = FALSE,
                       time_span = c('2012-01-01', '2020-01-01'),
                       time_period = 'year'){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue'),
                            inform = list(color = 'green')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'pes',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    pes_rslt <- pes_process_omop(cohort = cohort,
                                 user_cutoff = user_cutoff,
                                 n_event_a = n_event_a,
                                 n_event_b = n_event_b,
                                 pes_event_file = pes_event_file,
                                 multi_or_single_site = multi_or_single_site,
                                 anomaly_or_exploratory=anomaly_or_exploratory,
                                 age_groups = age_groups,
                                 patient_level_tbl = patient_level_tbl,
                                 p_value = p_value,
                                 time = time,
                                 time_span = time_span,
                                 time_period = time_period)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    pes_rslt <- pes_process_pcornet(cohort = cohort,
                                    user_cutoff = user_cutoff,
                                    n_event_a = n_event_a,
                                    n_event_b = n_event_b,
                                    pes_event_file = pes_event_file,
                                    multi_or_single_site = multi_or_single_site,
                                    anomaly_or_exploratory=anomaly_or_exploratory,
                                    age_groups = age_groups,
                                    patient_level_tbl = patient_level_tbl,
                                    p_value = p_value,
                                    time = time,
                                    time_span = time_span,
                                    time_period = time_period)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  cli::cli_inform(paste0(col_green('Based on your chosen parameters, we recommend using the following
                       output function in pes_output: '), col_blue(style_bold(output_type,'.'))))

  return(pes_rslt)
}
