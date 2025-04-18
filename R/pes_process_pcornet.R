
#' Patient Event Sequencing -- PCORnet
#'
#' @param cohort cohort for SQUBA testing; required fields:
#' - `site`
#' - `patid`
#' - `start_date`
#' - `end_date`
#' @param user_cutoff user selected number of days between events to be used
#'                    as a threshold cutoff for analyses
#' @param n_event_a the number of times event A should occur before establishing
#'                  the index date; defaults to 1
#' @param n_event_b the number of times event B should occur before establishing
#'                  the occurrence date; defaults to 1
#' @param pes_event_file CSV file with definitions of each of the events
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
#'
pes_process_pcornet <- function(cohort,
                                user_cutoff = 30,
                                n_event_a = 1,
                                n_event_b = 1,
                                pes_event_file = patienteventsequencing::pes_event_file,
                                multi_or_single_site = 'single',
                                anomaly_or_exploratory='exploratory',
                                age_groups = NULL,
                                patient_level_tbl = FALSE,
                                p_value = 0.9,
                                time = FALSE,
                                time_span = c('2012-01-01', '2020-01-01'),
                                time_period = 'year'){

  ## parameter summary output
  # output_type <- suppressWarnings(param_summ(check_string = 'pes',
  #                                            as.list(environment())))


  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  # Set up grouped list

  #grouped_list <- grouped_list %>% append('domain')

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}

  site_output <- list()
  ptlv_output <- list()

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups,
                                codeset = NULL, omop_or_pcornet = 'pcornet') %>%
    group_by(!!! syms(grouped_list))

  for(k in 1:length(site_list_adj)) {

    site_list_thisrnd <- site_list_adj[[k]]

    # filters by site
    cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

    pes_tbl_site <- compute_event_sequence_pcnt(cohort = cohort_site,
                                                grouped_list = grouped_list,
                                                site_col = site_col,
                                                user_cutoff = user_cutoff,
                                                n_event_a = n_event_a,
                                                n_event_b = n_event_b,
                                                time = time,
                                                time_period = time_period,
                                                time_span = time_span,
                                                patient_level_tbl = patient_level_tbl,
                                                event_csv = pes_event_file)

    site_output[[k]] <- pes_tbl_site[[1]]
    ptlv_output[[k]] <- pes_tbl_site[[2]]

  }


  pes_tbl_reduce <- purrr::reduce(.x = site_output,
                                  .f = dplyr::union) %>%
    replace_site_col()

  if(!time){

    if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){

      expand_cts <- pes_tbl_reduce %>%
        uncount(pt_ct)

      without_both_thrs <- pes_tbl_reduce %>%
        select(site, user_cutoff, total_pts, event_a_name, event_b_name, pts_without_both) %>%
        pivot_longer(cols = 'pts_without_both',
                     names_to = 'threshold_cutoff',
                     values_to = 'n_pts_thrs') %>%
        mutate(prop_pts_thrs = round(n_pts_thrs / total_pts, 3)) %>% distinct()

      thrs_cutoffs <- expand_cts %>%
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
        union(without_both_thrs)

      pes_tbl_int <- compute_dist_anomalies(df_tbl = thrs_cutoffs,
                                            grp_vars = c('threshold_cutoff'),
                                            var_col = 'prop_pts_thrs',
                                            denom_cols = c('threshold_cutoff', 'total_pts'))

      pes_tbl_final <- detect_outliers(df_tbl = pes_tbl_int,
                                       tail_input = 'both',
                                       p_input = p_value,
                                       column_analysis = 'prop_pts_thrs',
                                       column_variable = 'threshold_cutoff')
    }else{pes_tbl_final <- pes_tbl_reduce}

  }else{

    if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){

      expand_cts <- pes_tbl_reduce %>%
        uncount(pt_ct)

      thrs_cutoffs <- expand_cts %>%
        mutate(user_thrs = ifelse(abs(num_days) <= user_cutoff, 1, 0)) %>%
        pivot_longer(cols = c('user_thrs')) %>%
        group_by(site, user_cutoff, total_pts, name, time_start, time_increment,
                 event_a_name, event_b_name) %>%
        summarise(n_pts_thrs = sum(value, na.rm = TRUE)) %>%
        mutate(prop_pts_thrs = round(n_pts_thrs / total_pts, 3)) %>%
        rename('threshold_cutoff' = name) %>% ungroup()

      pes_tbl_final <- ms_anom_euclidean(fot_input_tbl = thrs_cutoffs,
                                         var_col = 'prop_pts_thrs',
                                         grp_vars = c('site', 'threshold_cutoff', 'user_cutoff',
                                                      'event_a_name', 'event_b_name'))

    }else{pes_tbl_final <- pes_tbl_reduce}

  }

  if(patient_level_tbl){
    ptlv_reduce <- purrr::reduce(.x = ptlv_output,
                                 .f = dplyr::union)

    output <- list('pes_summary_results' = pes_tbl_final %>% replace_site_col(),
                   'pes_patient_level_results' = ptlv_reduce %>% replace_site_col())

  }else{output <- pes_tbl_final %>% replace_site_col()}

  # cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
  #                      output function in pes_output: ', output_type, '.')))

  return(output)
}

