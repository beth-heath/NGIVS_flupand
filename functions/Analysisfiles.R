#### Analysis files
month_of_interest <- 4
restricting_topandemictime <- function(epid_dt, infs_dt, month_of_interest){
  pandemic_results <- epid_dt %>% filter(is.na(original_date))
  pandemic_results <- pandemic_results %>% mutate(epidemic_start_date = as.Date(as.numeric(epid_start_date)),
                                                  restriction_period = epidemic_start_date %m+% months(month_of_interest) )
  restricing_infs_code <- infs_dt %>%
    inner_join(pandemic_results, by = "simulation_index") %>%
    filter(time < restriction_period)
  
  
  totals_of_infs <- restricing_infs_code %>%
    group_by(simulation_index, vacc_type) %>%
    summarise(tot_sum = sum(tot, na.rm = TRUE), .groups = 'drop')
  
  outputs <- totals_of_infs %>%
    group_by(vacc_type) %>%
    summarise(listing = list(tot_sum), .groups = 'drop')
  
  t_test_results <- map(outputs$listing, ~ t.test(.x))
  
  # Extract the mean and confidence interval (CI) for each t-test result
  t_test_mean <- map_dbl(t_test_results, ~ .x$estimate)
  t_test_CI <- map(t_test_results, ~ .x$conf.int)
  
  t_test_output<- matrix(c(unique(outputs$vacc_type), t_test_mean, t_test_CI), ncol=3)
  colnames(t_test_output) <-c('vacc_type', 'mean', 'CI')
  
  return(t_test_output)
  
}


### undertsnading infections at time of pandemic


infects_at_pan <- function(epid_dt, infs_dt, month_of_interest){
  pandemic_results <- epid_dt %>% filter(is.na(original_date))
  pandemic_results <- pandemic_results %>% mutate(epidemic_start_date = as.Date(as.numeric(epid_start_date)),
                                                  restriction_period = epidemic_start_date %m+% months(0) )
  restricing_infs_code <- infs_dt %>%
    inner_join(pandemic_results, by = "simulation_index") %>%
    filter(time < last_monday(restriction_period))
  
  last_entries <- restricing_infs_code %>%
    group_by(simulation_index, vacc_type) %>%
    slice_tail(n = 1)
  
  summary_stats <- last_entries %>%
    group_by(simulation_index, vacc_type) %>%
    summarise(
      tot,
      .groups = "drop"
    )
  return(summary_stats)
  
}


#### testing this time to restrict 

test_func <- function(epid_dt, infs_dt, month_of_interest){
  pandemic_results <- epid_dt %>% filter(is.na(original_date))
  pandemic_results <- pandemic_results %>% mutate(epidemic_start_date = as.Date(as.numeric(epid_start_date)),
                                                  restriction_period = epidemic_start_date %m+% months(month_of_interest) )
  restricing_infs_code <- infs_dt %>%
    inner_join(pandemic_results, by = "simulation_index") %>%
    filter(time < restriction_period)
  
  
  totals_of_infs <- restricing_infs_code %>%
    group_by(simulation_index, vacc_type) %>%
    summarise(tot_sum = sum(tot, na.rm = TRUE), .groups = 'drop')
  
  return(totals_of_infs)
  
}

test4<- test_func(epid_dt, infs_dt,4)














