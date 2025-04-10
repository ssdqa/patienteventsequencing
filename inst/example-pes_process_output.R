
#' Source setup file
source(system.file('setup.R', package = 'patienteventsequencing'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'pes_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'patienteventsequencing'),
                      cdm_schema = NA)

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
                                   user_cutoff = 15,
                                   n_event_a = 1,
                                   n_event_b = 2,
                                   pes_event_file = pes_events)

pes_process_example

#' Execute `pes_output` function
pes_output_example <- pes_output(process_output = pes_process_example,
                                 output_function = 'pes_ss_exp_cs')

pes_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(pes_output_example)
