
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(pes_output(process_output = tbl_test,
                          output_function = 'pes_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'),
                            num_days = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
                            user_cutoff = c(25, 25, 25, 25, 25, 25, 25, 25, 25),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(4, 2, 7, 3, 100, 5, 2, 8, 6),
                            total_pts = c(250, 250, 250, 250, 250, 250, 250, 250, 250),
                            pts_without_both = c(20, 20, 20, 20, 20, 20, 20, 20, 20))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ss_exp_nt'))

})


test_that('multi site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b'),
                            num_days = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
                            user_cutoff = c(25, 25, 25, 25, 25, 25, 25, 25, 25),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(4, 2, 7, 3, 100, 5, 2, 8, 6),
                            total_pts = c(250, 250, 250, 250, 250, 250, 250, 250, 250),
                            pts_without_both = c(20, 20, 20, 20, 20, 20, 20, 20, 20))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ms_exp_nt'))

})


test_that('multi site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'threshold_cutoff' = c('ninety_thresh', 'ninety_thresh', 'ninety_thresh'),
                            'total_pts' = c(100, 200, 300),
                            'user_cutoff' = c(20, 20, 20),
                            'event_a_name' = c('t1', 't1', 't1'),
                            'event_b_name' = c('t2', 't2', 't2'),
                            'n_pts_thrs' = c(30, 20, 15),
                            'prop_pts_thrs' = c(0.1, 0.3, 0.4),
                            'mean_val' = c(0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3),
                            'analysis_eligible' = c('yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier'))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ms_anom_nt'))

  expect_no_error(pes_output(process_output = tbl_test %>% dplyr::mutate(anomaly_yn = 'no outlier in group'),
                             output_function = 'pes_ms_anom_nt'))

})


test_that('single site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a', 'a', 'a'),
                            num_days = c(100, 200, 150, 40, -10,
                                         -50, -60, 120, 15, 0),
                            user_cutoff = c(1,1,1,1,1,1,1,1,1,1),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ss_exp_at'))

})

test_that('multi site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'b', 'b', 'b', 'b', 'b'),
                            num_days = c(100, 200, 150, 40, -10,
                                         -50, -60, 120, 15, 0),
                            user_cutoff = c(1,1,1,1,1,1,1,1,1,1),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ms_exp_at'))

})


test_that('single site, anomaly detection, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a', 'a', 'a'),
                            num_days = c(100, 200, 150, 40, -10,
                                         -50, -60, 120, 15, 0),
                            user_cutoff = c(1,1,1,1,1,1,1,1,1,1),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ss_anom_at'))

})


test_that('multi site, anomaly detection, across time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c'),
                            'threshold_cutoff' = c('user_thresh', 'user_thresh', 'user_thresh',
                                                   'user_thresh', 'user_thresh', 'user_thresh',
                                                   'user_thresh', 'user_thresh', 'user_thresh'),
                            'time_start' = c('2018-01-01', '2018-01-01', '2019-01-01',
                                             '2019-01-01', '2020-01-01', '2020-01-01',
                                             '2021-01-01', '2021-01-01', '2022-01-01'),
                            'total_pts' = c(100, 200, 300, 100, 200, 300, 100, 200, 300),
                            'user_cutoff' = c(20, 20, 20, 20, 20, 20, 20, 20, 20),
                            'event_a_name' = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            'event_b_name' = c('t2', 't2', 't2', 't2', 't2', 't2', 't2', 't2', 't2'),
                            'n_pts_thrs' = c(30, 20, 15, 30, 20, 15, 30, 20, 15),
                            'prop_pts_thrs' = c(0.1, 0.3, 0.4, 0.1, 0.3, 0.4, 0.1, 0.3, 0.4),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9,0.9,0.9,0.9))

  expect_no_error(pes_output(process_output = tbl_test,
                             output_function = 'pes_ms_anom_at'))

})
