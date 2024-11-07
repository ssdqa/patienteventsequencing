
#' Compute sequence of 2 events -- PCORnet
#'
#' @param cohort a table with the cohort of interest; should minimally include:
#' - `site`
#' - `patid`
#' - `start_date`
#' - `end_date`
#' @param grouped_list list of columns in the data that should be used to group
#'                     the results
#' @param site_col the name of the column with site name(s)
#' @param user_cutoff a user specified threshold against which to measure the
#'                    number of days between the two events
#' @param time a logical to indicate whether this analysis is being conducted
#'             over time or not
#' @param n_event_a the number of times Event A should occur before establishing
#'                  the index date
#' @param n_event_b the number of times Event B should occur before esrablishing
#'                  the occurrence date
#' @param patient_level_tbl a logical to indicate whether an intermediate table with
#'                         patient level results should be output. if TRUE, a dataframe
#'                         with one row per patient will be returned in addition
#'                         to aggregate output
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format.
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param event_csv a csv file with the definitions for each event with the columns
#' - `event` - A or B
#' - `event_label` - a descriptive label for the event
#' - `domain_tbl` - the default CDM table from which data is retrieved
#' - `concept_field` - the field in the table where the codes of interest are stored
#' - `date_field` - the date field to be used to establish the index & occurrence dates
#' - `vocabulary_field` - (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
#' - `codeset_name` - the name of the codeset in the specs directory to define the variable of interest
#' - `filter_logic` - a string indicating any filter logic that should be applied to establish the event
#' ex: an Hba1c > 6.5
#'
#' @return an aggregated dataframe outlining the number of patients that had the two events
#'         occur in X number of days
#'
#'         if patient_level_tbl = TRUE, a patient level dataframe is also returned
#'
compute_event_sequence_pcnt <- function(cohort,
                                        grouped_list,
                                        site_col,
                                        user_cutoff = 30,
                                        n_event_a = 1,
                                        n_event_b = 1,
                                        patient_level_tbl = FALSE,
                                        time = FALSE,
                                        time_period = 'year',
                                        time_span = c('2011-01-01', '2023-12-31'),
                                        event_csv = patienteventsequencing::pes_event_file){

  ## Pull event information
  event_list <- split(event_csv, seq(nrow(event_csv)))

  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}

  ## Get event data
  event_rslt <- list()

  for(i in 1:length(event_list)){

    if(toupper(event_list[[i]]$event) == 'A'){
      event_ct <- n_event_a}else{event_ct <- n_event_b}

    event_domain <- cdm_tbl(event_list[[i]]$domain_tbl) %>%
      inner_join(cohort) %>%
      filter(!!sym(event_list[[i]]$date_field) >= start_date,
             !!sym(event_list[[i]]$date_field) <= end_date) %>%
      group_by(patid, !!!syms(grouped_list))

    join_cols <- set_names('concept_code', event_list[[i]]$concept_field)

    if(!is.na(event_list[[i]]$vocabulary_field)){
      join_cols2 <- set_names('vocabulary_id', event_list[[i]]$vocabulary_field)
      join_cols <- join_cols %>% append(join_cols2)
    }

    if(is.na(event_list[[i]]$filter_logic)){
      event_index <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        window_order(!!sym(event_list[[i]]$date_field)) %>%
        mutate(event_n = row_number()) %>%
        filter(event_n == event_ct) %>%
        mutate(event_date = !!sym(event_list[[i]]$date_field)) %>%
        distinct(patid, !!!syms(grouped_list), event_date) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }else{
      event_index <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]]$filter_logic)) %>%
        window_order(!!sym(event_list[[i]]$date_field)) %>%
        mutate(event_n = row_number()) %>%
        filter(event_n == event_ct) %>%
        mutate(event_date = !!sym(event_list[[i]]$date_field)) %>%
        distinct(patid, !!!syms(grouped_list), event_date) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }

    event_rslt[[i]] <- event_index

  }

  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)

  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'patid']

  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_index_date = event_date,
           event_a_name = event_name) %>% select(-event_type)

  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_occurrence_date = event_date,
           event_b_name = event_name) %>% select(-event_type)

  event_ptlv <- eventa %>% left_join(eventb) %>%
    mutate(num_days = sql(calc_days_between_dates(date_col_1 = 'event_a_index_date',
                                                  date_col_2 = 'event_b_occurrence_date'))) %>%
    mutate(user_cutoff = user_cutoff) %>% ungroup() %>% fill(event_b_name, .direction = 'updown')

  ## OPTIONAL: output patient level results
  if(patient_level_tbl){pt_lv_rslt <- event_ptlv}else{pt_lv_rslt <- NULL}

  if(time){

    t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
    t2 <- ceiling_date(t1, time_period) - 1

    time_df <- tibble('time_start' = t1,
                      'time_end' = t2)

    cat_df <- event_ptlv %>% select(!!!syms(new_grp), user_cutoff, event_a_name, event_b_name) %>%
      distinct() %>% cross_join(time_df) %>% select(-time_end)

    event_agg <- event_ptlv %>%
      cross_join(time_df) %>%
      filter(event_a_index_date <= time_end,
             event_a_index_date >= time_start) %>%
      group_by(!!!syms(new_grp), num_days, user_cutoff, time_start,
               event_a_name, event_b_name) %>%
      summarise(pt_ct = n()) %>%
      group_by(!!!syms(new_grp), user_cutoff, time_start) %>%
      mutate(total_pts = sum(pt_ct)) %>%
      ungroup() %>%
      filter(!is.na(!!sym(site_col))) %>%
      full_join(cat_df) %>%
      mutate(user_cutoff = user_cutoff,
             total_pts = ifelse(is.na(total_pts), 0, total_pts),
             pt_ct = ifelse(is.na(pt_ct), 0, pt_ct),
             time_increment = time_period)


  }else{

    ## Total cohort denom
    total_pts <- cohort %>%
      group_by(!!!syms(new_grp)) %>%
      summarise(total_pts = n()) %>% collect()

    ## Aggregate patient level output
    event_agg <- event_ptlv %>%
      group_by(!!!syms(new_grp), num_days, user_cutoff,
               event_a_name, event_b_name) %>%
      summarise(pt_ct = n()) %>%
      ungroup() %>%
      left_join(total_pts)
    # mutate(total_pts = total_pts)

    ## Both events not present
    event_miss <- event_agg %>%
      filter(!is.na(num_days)) %>%
      group_by(!!!syms(new_grp), total_pts) %>%
      summarise(with_both = sum(pt_ct)) %>%
      mutate(pts_without_both = total_pts - with_both) %>% select(-with_both)

    event_agg <- event_agg %>%
      filter(!is.na(num_days)) %>%
      left_join(event_miss)
  }

  op_list <- list(event_agg,
                  pt_lv_rslt)

  return(op_list)

}
