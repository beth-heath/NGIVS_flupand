
#comporartor = 0 means no vaccination
producing_data_for_barchart <- function(overall_file, mechanism_of_interest, comparator){
  
  restricting_mechanism <- overall_file[age_testing_strategy == 4|age_testing_strategy == 5, ]
  
  
  restricting_mechanism <- restricting_mechanism[restricting_mechanism$mechanism == mechanism_of_interest, ]
  #restricting_mechanism <- restricting_mechanism[restricting_mechanism$age_testing_strategy == age_test, ]
  
  if (comparator =='0'){
    restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == '0'|restricting_mechanism$vacc_type == 'C', ]
  } else{
    restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == 'A.2'|restricting_mechanism$vacc_type == 'C', ]
  }
  
  restricting_mechanism <- restricting_mechanism %>% mutate(infections_difference = total_infections.x - total_infections.y,
                                  hospital_difference = hospitalisations.x - hospitalisations.y,
                                  deaths_difference = total_deaths.x - total_deaths.y,
                                  DALY_difference = total_DALYS.x -  total_DALYS.y,
                                  cost_difference = total_epi_costs.x - total_epi_costs.y
                                  )
  
  restriction_set <- restricting_mechanism %>% select(simulation_index, vacc_type, ITZ, country, pandemic, time_of_pandemic, infections_difference, hospital_difference, deaths_difference, DALY_difference, cost_difference, age_testing_strategy)
  
  restriction_set <- dcast(restriction_set, simulation_index+ ITZ + country  + pandemic + time_of_pandemic + age_testing_strategy ~ vacc_type , value.var = c("infections_difference", "hospital_difference" , "deaths_difference", "DALY_difference", "cost_difference"))
  
  if (comparator =='0'){
    restriction_set <- restriction_set %>% mutate(infections_difference = infections_difference_0 - infections_difference_C,
                                                  hospital_difference = hospital_difference_0 - hospital_difference_C,
                                                  deaths_difference = deaths_difference_0 - deaths_difference_C,
                                                  DALY_difference = DALY_difference_0 - DALY_difference_C,
                                                  cost_difference =  cost_difference_0 -  cost_difference_C
                                                  )
  }else{
    restriction_set <- restriction_set %>% mutate(infections_difference = infections_difference_A.2 - infections_difference_C,
                                                  hospital_difference = hospital_difference_A.2 - hospital_difference_C,
                                                  deaths_difference = deaths_difference_A.2 - deaths_difference_C,
                                                  DALY_difference = DALY_difference_A.2 - DALY_difference_C,
                                                  cost_difference =  cost_difference_A.2 -  cost_difference_C
    )
  }
  
  restriction_set  <- restriction_set%>% select(simulation_index,  ITZ, country,  pandemic,age_testing_strategy,   time_of_pandemic, infections_difference, hospital_difference, deaths_difference, DALY_difference, cost_difference)
  
  restriction_set <-  restriction_set %>%
    group_by(simulation_index,  ITZ, pandemic,  time_of_pandemic, age_testing_strategy) %>%
    summarise(infections_difference=sum(infections_difference),
              hospital_difference = sum(hospital_difference),
              deaths_difference = sum(deaths_difference),
              DALY_difference = sum(DALY_difference),
              cost_difference = sum(cost_difference),
              .groups='drop')
  
  restriction_set_recoded <- restriction_set %>%
    pivot_longer(
      cols = ends_with("_difference"),
      names_to = "Outcome",
      values_to = "Value"
    ) %>%
    mutate(
      Outcome = recode(Outcome,
                       infections_difference = "Infections",
                       hospital_difference  = "Hospitalisations",
                       deaths_difference = "Deaths",
                       DALY_difference = "DALYs",
                       cost_difference = "Cost",
                       dose_cost_difference = 'Dose cost'),
      ITZ = recode(ITZ,
                   '1' = 'Africa',
                   '2'='Asia-Europe',
                   '3' = 'Eastern and Southern Asia',
                   '4' = 'Europe',
                   '5' = 'Northern America',
                   '6'='Oceania-Melanesia-Polynesia',
                   '7'='Southern America'),
      
      age_testing_strategy = recode(age_testing_strategy,
                                    '1' = '0-4',
                                    '2'='0-10',
                                    '3'='0-17',
                                    '4'='65+',
                                    '5'='0-17, 65+'),
      age_testing_strategy = factor(age_testing_strategy,
                                    levels = c("0-4", "0-10", "0-17", "65+", "0-17, 65+"))
    )
      
  
  restriction_set_recoded <- restriction_set_recoded %>%
    group_by(ITZ, pandemic,age_testing_strategy,  Outcome) %>%
    summarise(
      mean_value = mean(Value, na.rm = TRUE),
      sd_value = sd(Value, na.rm = TRUE),
      n = n(),
      se_value = sd_value / sqrt(n),
      .groups = "drop"
    )
  
  restriction_set_recoded <- restriction_set_recoded %>%
    mutate(
      lower_ci = mean_value - 1.96 * se_value,
      upper_ci = mean_value + 1.96 * se_value
    )
  
  return(restriction_set_recoded)
  
}

####### Trying to work out the line graoh


producing_data_for_barchart <- function(overall_file, mechanism_of_interest, testing_strat, age_test){
  
  restricting_mechanism <- overall_file[overall_file$mechanism == mechanism_of_interest, ]
  restricting_mechanism <- restricting_mechanism[restricting_mechanism$age_testing_strategy == age_test, ]
  restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == 'C', ]
  
  restricting_mechanism <- restricting_mechanism %>% select(simulation_index,  ITZ, country,  pandemic,age_testing_strategy,   time_of_pandemic, cost_by_dose.x)
  
  restricting_mechanism <-  restricting_mechanism %>%
    group_by(ITZ, pandemic,  time_of_pandemic) %>%
    summarise(threshold_price=sum(cost_by_dose.x),
              .groups='drop')
  
  restricting_mechanism <- restricting_mechanism %>%
    mutate(
      ITZ = factor(ITZ),
      pandemic = factor(pandemic)
    )
  
  
  return(restricting_mechanism )
  
}

test3 <-producing_data_for_barchart(overall_file, 'sterilising', '0', 5) 

ggplot(test3, aes(x = time_of_pandemic, y = threshold_price, color = ITZ, group = ITZ)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ pandemic, nrow = 1) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(
    breaks = seq(min(test3$time_of_pandemic), max(test3$time_of_pandemic), by = 1)
  ) +
  labs(
    x = "Year of Pandemic",
    y = "Threshold Price",
    color = "ITZ Zone",
    title = "Threshold Price Over Time by ITZ and Pandemic Type"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






test1 <- producing_data_for_barchart(overall_file, 'sterilising', '0')

ggplot(test1, aes(x = ITZ, y = mean_value / 1e6)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6), width = 0.2) +
  facet_grid(rows = vars(Outcome), cols = vars(age_testing_strategy), 
             labeller = label_wrap_gen(width = 10)) +
  facet_grid(rows = vars(Outcome), cols = vars(pandemic, age_testing_strategy), scales = "free_y") +
  labs(
    x = "ITZ",
    y = "Difference between seasonal and pandemic (millions)",
    title = "Comparison of Outcomes by ITZ and Pandemic Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )








