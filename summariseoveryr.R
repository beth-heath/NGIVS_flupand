#Creating a file to summarise over year
infs_dt2 <- infs_dt
infs_dt2$year <-lubridate::year(infs_dt2$time)
inf_summary <- infs_dt2 %>%
  group_by(year, simulation_index) %>%
  summarise(tot_sum = sum(tot,na.rm=TRUE), .groups = 'drop')

#### Cutting off measurements at the date of the pandemic

#1:simulations
for (sim in 1:5){
  pandemic_results <- epid_dt[is.na(original_date) == TRUE]
  pandemic_for_sim <- as.Date(as.numeric(pandemic_results[pandemic_results$simulation_index == sim,]$epid_start_date))
  restriction_period <- AddMonths(pandemic_for_sim, 6)
  restricting_infs_dt_code <- infs_dt[simulation_index == sim & pandemic_for_sim < time & time < restriction_period]
  #summarising the total for each vaccine type
  test_ing <- restricting_infs_dt_code %>%
    group_by(simulation_index, vacc_type) %>%
    summarise(tot_sum = sum(tot,na.rm=TRUE), .groups = 'drop')
  
}

## Creating function

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

#restricting_topandemictime(epid_dt, infs_dt, 6)
test1<- restricting_topandemictime(epid_dt, infs_dt, 1)
test2<- restricting_topandemictime(epid_dt, infs_dt, 6)

test3 <- test1 %>%
  group_by(simulation_index, vacc_type) %>%
  summarise(tot_sum = sum(tot, na.rm = TRUE), .groups = 'drop')

test4 <- test2 %>%
  group_by(simulation_index, vacc_type) %>%
  summarise(tot_sum = sum(tot, na.rm = TRUE), .groups = 'drop')

t_test_CI <- list()
t_test_mean <- list()
for (i in 1:length(test1$vacc_type)){
  t_test_mean[i] <- t.test(test1$listing[[i]])[5]
  t_test_CI[i] <- t.test(test1$listing[[i]])[4]
}


t_test_results <- map(outputs$outputs, ~ t.test(.x))

# Extract the mean and confidence interval (CI) for each t-test result
t_test_mean <- map_dbl(t_test_results, ~ .x$estimate)
t_test_CI <- map(t_test_results, ~ .x$conf.int)




outputs <- test_ing %>%
  group_by(vacc_type) %>%
  summarise(outputs = list(tot_sum), .groups = 'drop')

#### testing


outputs <- mutate(mean_result= t.test(outputs))

outputs$outputs[[1]]




