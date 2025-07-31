#aesthics file

source(here::here('setup','aesthetics.R'))


#loading in WHO data file for WHO region
WHO_region_file <- wtp_thresh <- data.table(read_csv(here::here('data/WHO_regions.csv'), show_col_type=F))


#comporartor = 0 means no vaccination
producing_data_for_barchart <- function(overall_file, mechanism_of_interest, comparator){
  
  restricting_mechanism <- overall_file[age_testing_strategy == 2|age_testing_strategy == 4, ]
  
  
  if (comparator =='0'){
    zero_test_compare <- restricting_mechanism[restricting_mechanism$vacc_type == '0', ]
    restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == 'C' & restricting_mechanism$mechanism == mechanism_of_interest, ]
    restricting_mechanism <- rbind(restricting_mechanism, zero_test_compare)
    
  } else{
    restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == 'A.2'|restricting_mechanism$vacc_type == 'C', ]
    restricting_mechanism <- restricting_mechanism[restricting_mechanism$mechanism == mechanism_of_interest, ]
  }
  
  
  
  restricting_mechanism <- restricting_mechanism %>% mutate(infections_difference = total_infections.x - total_infections.y,
                                  hospital_difference = hospitalisations.x - hospitalisations.y,
                                  deaths_difference = total_deaths.x - total_deaths.y,
                                  DALY_difference = total_DALYS.x -  total_DALYS.y,
                                  cost_difference = total_epi_costs.x - total_epi_costs.y
                                  )
  
  restriction_set <- restricting_mechanism %>% select(simulation_index, vacc_type, WHO_region, country, pandemic, time_of_pandemic, infections_difference, hospital_difference, deaths_difference, DALY_difference, cost_difference, age_testing_strategy)
  
  restriction_set <- dcast(restriction_set, simulation_index+ WHO_region + country  + pandemic + time_of_pandemic + age_testing_strategy ~ vacc_type , value.var = c("infections_difference", "hospital_difference" , "deaths_difference", "DALY_difference", "cost_difference"))
  
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
  
  restriction_set  <- restriction_set%>% select(simulation_index,  WHO_region, country,  pandemic,age_testing_strategy,   time_of_pandemic, infections_difference, hospital_difference, deaths_difference, DALY_difference, cost_difference)
  
  restriction_set <-  restriction_set %>%
    group_by(simulation_index, WHO_region, pandemic, age_testing_strategy, time_of_pandemic) %>%
    summarise(
      infections_difference = sum(infections_difference),
      hospital_difference = sum(hospital_difference),
      deaths_difference = sum(deaths_difference),
      DALY_difference = sum(DALY_difference),
      cost_difference = sum(cost_difference),
      .groups = "drop"
    )
  
  return(restriction_set)
  
}



producing_data_for_barchart_overall <- function(combined_set){
  
  
  restriction_set_recoded <- combined_set %>%
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
      WHO_region = recode(WHO_region,
                   'SEAR' = 'South-East Asia Region',
                   'WPR'='Western Pacific Region',
                   'AMR' = 'Region of the Americas',
                   'EMR' = 'Eastern Mediterranean Region',
                   'EUR' = 'European Region',
                   'AFR'='African Region'),
      
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
    group_by(WHO_region, pandemic,age_testing_strategy,  Outcome) %>%
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


producing_data_for_linechart <- function(overall_file, mechanism_of_interest, age_test){
  
  restricting_mechanism <- overall_file[overall_file$mechanism == mechanism_of_interest, ]
  restricting_mechanism <- restricting_mechanism[restricting_mechanism$age_testing_strategy == age_test, ]
  restricting_mechanism <- restricting_mechanism[restricting_mechanism$vacc_type == 'C', ]
  comparitive <- overall_file[overall_file$mechanism == 'sterilising' & overall_file$vacc_type == '0' & overall_file$age_testing_strategy == age_test, ]
  restricting_mechanism <- rbind(restricting_mechanism, comparitive)
  restricting_mechanism <- restricting_mechanism %>% select(simulation_index,  vacc_type, WHO_region, country,  pandemic,age_testing_strategy,   time_of_pandemic, combined_epi.x, vaccs.x)
  
  restricting_mechanism <-  restricting_mechanism %>%
    group_by(WHO_region, pandemic,  time_of_pandemic, vacc_type) %>%
    summarise(total_epi_costs=sum(combined_epi.x),
              vaccs = sum(vaccs.x),
              .groups='drop')
  
  restricting_mechanism <- restricting_mechanism %>%
    mutate(
      WHO_region = factor(WHO_region),
      pandemic = factor(pandemic)
    )
  
  restricting_mechanism <- restricting_mechanism %>%  group_by(WHO_region, pandemic,  time_of_pandemic, vacc_type) %>%
    summarise(total_epi_costs=sum(total_epi_costs),
              vaccs = sum(vaccs),
              .groups='drop')
  
  restricting_mechanism <- as.data.table(restricting_mechanism)
  restricting_mechanism <- dcast(restricting_mechanism,  WHO_region   + pandemic + time_of_pandemic  ~ vacc_type , value.var = c("total_epi_costs", "vaccs" ))
  
  return(restricting_mechanism )
  
}




###### Overall run ######



starting <- 0
begin <- 0
ITZ_zone_done <- c(1, 2,3,4, 5,6,7)

for (i in 1:length(ITZ_zone_done)) {
  ITZregion <- ITZ_zone_done[i]
  c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
              "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
              "Southern America")[ITZregion]
  
  country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
  country_codes_not_considered <-c('GUF', 'HKG', 'MAC', 'NCL', 'PRI', 'PSE', 'TWN', 'XKX')
  country_codes <- country_codes[!country_codes %in% country_codes_not_considered]
  for (country_of_interest in country_codes){
    overall_file <- read_parquet(file.path('Run_script', 'Overall', paste0('Overallfile', country_of_interest, '.parquet')))
    region_of_interest<- WHO_region_file[country_code == country_of_interest, ]$WHOREGION
    overall_file$WHO_region <- rep(region_of_interest, nrow(overall_file))
    
    print(country_of_interest)
                                 
                                 if (starting ==0){
                                   starting <- 1
                                   bar_chart_sterilising_0 <- producing_data_for_barchart(overall_file, 'sterilising', '0')
                                   bar_chart_sterilising_current <- producing_data_for_barchart(overall_file, 'sterilising', 'A.2')
                                   bar_chart_disease_mod_0 <- producing_data_for_barchart(overall_file, 'disease mod', '0')
                                   bar_chart_disease_mod_current <- producing_data_for_barchart(overall_file, 'disease mod', 'A.2')
                                   bar_chart_infectious_period_0 <- producing_data_for_barchart(overall_file, 'infection period', '0')
                                   bar_chart_infectious_period_current <- producing_data_for_barchart(overall_file, 'infection period', 'A.2')
                                   line_chart_sterilising <- producing_data_for_linechart(overall_file, 'sterilising', 5)
                                   line_chart_disease_mod <- producing_data_for_linechart(overall_file, 'disease mod', 5)
                                   line_chart_infectious_period <- producing_data_for_linechart(overall_file, 'infection period', 5)
                                 }else{
                                   bar_chart_sterilising_0 <- rbind(bar_chart_sterilising_0, producing_data_for_barchart(overall_file, 'sterilising', '0'))
                          
                                   bar_chart_sterilising_0 <- bar_chart_sterilising_0 %>%  group_by(simulation_index,  WHO_region, pandemic,   age_testing_strategy, time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   bar_chart_sterilising_current <- rbind(bar_chart_sterilising_current,  producing_data_for_barchart(overall_file, 'sterilising', 'A.2'))
                                   
                                   bar_chart_sterilising_current <- bar_chart_sterilising_current %>%  group_by(simulation_index,  WHO_region, pandemic,  age_testing_strategy,  time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   bar_chart_disease_mod_0 <- rbind(bar_chart_disease_mod_0, producing_data_for_barchart(overall_file, 'disease mod', '0'))
                                   bar_chart_disease_mod_0 <- bar_chart_disease_mod_0 %>%  group_by(simulation_index,  WHO_region, pandemic,  age_testing_strategy,  time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   bar_chart_disease_mod_current <- rbind(bar_chart_disease_mod_current,  producing_data_for_barchart(overall_file, 'disease mod', 'A.2')) 
                                   bar_chart_disease_mod_current <- bar_chart_disease_mod_current %>%  group_by(simulation_index,  WHO_region, pandemic,  age_testing_strategy,  time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   bar_chart_infectious_period_0 <- rbind(bar_chart_infectious_period_0, producing_data_for_barchart(overall_file, 'infection period', '0')) 
                                   bar_chart_infectious_period_0 <- bar_chart_infectious_period_0 %>%   group_by(simulation_index,  WHO_region, pandemic, age_testing_strategy,  time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   bar_chart_infectious_period_current <- rbind(bar_chart_infectious_period_current,  producing_data_for_barchart(overall_file, 'infection period', 'A.2'))
                                   bar_chart_infectious_period_current <- bar_chart_infectious_period_current %>%   group_by(simulation_index,  WHO_region, pandemic,  age_testing_strategy,  time_of_pandemic) %>%
                                     summarise(infections_difference=sum(infections_difference),
                                               hospital_difference = sum(hospital_difference),
                                               deaths_difference = sum(deaths_difference),
                                               DALY_difference = sum(DALY_difference),
                                               cost_difference = sum(cost_difference),
                                               .groups='drop')
                                   
                                   
                                   line_chart_sterilising <- rbind(line_chart_sterilising, producing_data_for_linechart(overall_file, 'sterilising', 5) ) 
                                     line_chart_sterilising <- line_chart_sterilising %>%  group_by(WHO_region, pandemic,  time_of_pandemic) %>%
                                     summarise(total_epi_costs_0=sum(total_epi_costs_0),
                                               total_epi_costs_C=sum(total_epi_costs_C),
                                               vaccs_0 = sum(vaccs_0),
                                               vaccs_C = sum(vaccs_C),
                                               .groups='drop')
                                   
                                   line_chart_disease_mod <- line_chart_disease_mod %>% rbind(line_chart_disease_mod, producing_data_for_linechart(overall_file, 'disease mod', 5) ) 
                                   line_chart_disease_mod <- line_chart_disease_mod %>%  group_by(WHO_region, pandemic,  time_of_pandemic) %>%
                                     summarise(total_epi_costs_0=sum(total_epi_costs_0),
                                               total_epi_costs_C=sum(total_epi_costs_C),
                                               vaccs_0 = sum(vaccs_0),
                                               vaccs_C = sum(vaccs_C),
                                               .groups='drop')
                                   line_chart_infectious_period <- rbind(line_chart_infectious_period, producing_data_for_linechart(overall_file, 'infection period', 5) ) 
                                   line_chart_infectious_period <- line_chart_infectious_period %>%   group_by(WHO_region, pandemic,  time_of_pandemic) %>%
                                     summarise(total_epi_costs_0=sum(total_epi_costs_0),
                                               total_epi_costs_C=sum(total_epi_costs_C),
                                               vaccs_0 = sum(vaccs_0),
                                               vaccs_C = sum(vaccs_C),
                                               .groups='drop')
                                   
                                 }
                                 
                                 
                                 if (ITZregion == ITZ_zone_done[length(ITZ_zone_done)] & country_of_interest == country_codes[length(country_codes)]){
                                   
                                   line_chart_sterilising <- line_chart_sterilising %>% mutate(threshold_price = (total_epi_costs_0-total_epi_costs_C)/vaccs_C)
                                   line_chart_disease_mod <- line_chart_disease_mod %>% mutate(threshold_price = (total_epi_costs_0-total_epi_costs_C)/vaccs_C)
                                   line_chart_infectious_period <- line_chart_infectious_period %>% mutate(threshold_price = (total_epi_costs_0-total_epi_costs_C)/vaccs_C)
                                   
                                   bc_ster_0 <- producing_data_for_barchart_overall(bar_chart_sterilising_0)
                                   bc_ster_cu <- producing_data_for_barchart_overall(bar_chart_sterilising_current)
                                   bc_dm_0 <- producing_data_for_barchart_overall(bar_chart_disease_mod_0)
                                   bc_dm_cu <- producing_data_for_barchart_overall(bar_chart_disease_mod_current)
                                   bc_ip_0  <- producing_data_for_barchart_overall(bar_chart_infectious_period_0)
                                   bc_ip_cu <- producing_data_for_barchart_overall(bar_chart_infectious_period_current)
                                  
                                   
                                   ###### sterilising  no ######
                                   
                                   
                                   bc_ster_0$pandemic <- as.factor(bc_ster_0$pandemic)
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   bc_ster_0$WHO_region_recoded <- recode(
                                     bc_ster_0$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   
                                   bc_ster_0$pandemic = recode(bc_ster_0$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_ster_0$Outcome <- factor(bc_ster_0$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                          
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_ster_0 <- ggplot(bc_ster_0, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) + 
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_ster_0_prop <- bc_ster_0
                                   WHO_region_scalars = data.frame(
                                     WHO_region = c('African Region', 'Eastern Mediterranean Region', 'European Region',
                                                    'Region of the Americas', 'South-East Asia Region', 'Western Pacific Region'),
                                     scalar = c(1400000000,700000000, 930000000, 1100000000,2000000000, 2100000000  )
                                   ) 
                                   
                                   bc_ster_0_prop <- bc_ster_0_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_ster_0_prop <- bc_ster_0_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_ster_0_prop <- ggplot(bc_ster_0_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region",
                                       title = "Comparison of Outcomes by WHO region and Pandemic Type\n Universal NGIVs vs No Vaccination (Sterilising)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   ###### sterilising  current ######
                                   
                                   bc_ster_cu$pandemic <- as.factor(bc_ster_cu$pandemic)
                                   
                                   bc_ster_cu$pandemic = recode(bc_ster_cu$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_ster_cu$Outcome <- factor(bc_ster_cu$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                                   bc_ster_cu$WHO_region_recoded <- recode(
                                     bc_ster_cu$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_ster_cu <- ggplot(bc_ster_cu, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_ster_cu_prop <- bc_ster_cu
                                   
                                   
                                   bc_ster_cu_prop <- bc_ster_cu_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_ster_cu_prop <- bc_ster_cu_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_ster_cu_prop <- ggplot(bc_ster_cu_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region",
                                       title = "Comparison of Outcomes by WHO region and Pandemic Type\n Universal NGIVs vs Current Vaccination (Sterilising)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   ###### disease-modifying  no ######
                                    
                                   bc_dm_0$pandemic <- as.factor(bc_dm_0$pandemic)
                                   
                                   bc_dm_0$pandemic = recode(bc_dm_0$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_dm_0$Outcome <- factor(bc_dm_0$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                                   bc_dm_0$WHO_region_recoded <- recode(
                                     bc_dm_0$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_dm_0 <- ggplot(bc_dm_0, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_dm_0_prop <- bc_dm_0
                                   
                                   
                                   bc_dm_0_prop <- bc_dm_0_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_dm_0_prop <- bc_dm_0_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_dm_0_prop <- ggplot(bc_dm_0_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   ###### dm  current ######
                                   
                                   bc_dm_cu$pandemic <- as.factor(bc_dm_cu$pandemic)
                                   
                                   bc_dm_cu$pandemic = recode(bc_dm_cu$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_dm_cu$Outcome <- factor(bc_dm_cu$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                                   bc_dm_cu$WHO_region_recoded <- recode(
                                     bc_dm_cu$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_dm_cu <- ggplot(bc_dm_cu, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_dm_cu_prop <- bc_dm_cu
                                   
                                   bc_dm_cu_prop <- bc_dm_cu_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_dm_cu_prop <- bc_dm_cu_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_dm_cu_prop <- ggplot(bc_dm_cu_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   ###### infectios period modifying  no ######
                                   
                                   bc_ip_0$pandemic <- as.factor(bc_ip_0$pandemic)
                                   
                                   bc_ip_0$pandemic = recode(bc_ip_0$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_ip_0$Outcome <- factor(bc_ip_0$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                                   bc_ip_0$WHO_region_recoded <- recode(
                                     bc_ip_0$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_ip_0 <- ggplot(bc_ip_0, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_ip_0_prop <- bc_ip_0
                                  
                                   
                                   bc_ip_0_prop <- bc_ip_0_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_ip_0_prop <- bc_ip_0_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_ip_0_prop <- ggplot(bc_ip_0_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   ###### infectios period modifying  current ######
                                   
                                   bc_ip_cu$pandemic <- as.factor(bc_ip_cu$pandemic)
                                   
                                   bc_ip_cu$pandemic = recode(bc_ip_cu$pandemic,
                                                               '1918' = 'Scenario One',
                                                               '1957'='Scenario Two',
                                                               '2009' = 'Scenario Three')
                                   
                                   bc_ip_cu$Outcome <- factor(bc_ip_cu$Outcome, levels = rev(c("Cost", "DALYs","Deaths",   "Hospitalisations",  "Infections")))
                                   
                                   bc_ip_cu$WHO_region_recoded <- recode(
                                     bc_ip_cu$WHO_region,
                                     "Region of the Americas" = "Region of \n the Americas",
                                     "Western Pacific Region" = "Western\nPacific\nRegion",
                                     "South-East Asia Region" = "South-East\nAsia Region",
                                     "Eastern Mediterranean Region" = "Eastern\nMediterranean\nRegion",
                                     "European Region" = "European\nRegion",
                                     "African Region" = "African\nRegion"
                                   )
                                   
                                   outcome_labels <- c(
                                     "Infections" = "Infections",
                                     "Hospitalisations" = "Hospitalisations",
                                     "Deaths" = "Deaths",
                                     "DALYs" = "DALYs",
                                     "Cost" = "Cost"
                                   )
                                   
                                   strategy_labels <- c(
                                     "0-10" = "Ages 0–10 Vaccinated",
                                     "65+" = "Ages 65+ Vaccinated"
                                   )
                                   
                                   # Now plot
                                   plot_ip_cu <- ggplot(bc_ip_cu, aes(x = WHO_region_recoded, y = mean_value / 1e6, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci / 1e6, ymax = upper_ci / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic (millions)"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                   
                                   bc_ip_cu_prop <- bc_ip_cu
                                   
                                   
                                   bc_ip_cu_prop <- bc_ip_cu_prop %>%
                                     left_join(WHO_region_scalars, by = "WHO_region")
                                   
                                   bc_ip_cu_prop <- bc_ip_cu_prop %>%
                                     mutate(
                                       mean_value_adj = mean_value / scalar,
                                       lower_ci_adj = lower_ci / scalar,
                                       upper_ci_adj = upper_ci / scalar
                                     )
                                   
                                   
                                   plot_ip_cu_prop <- ggplot(bc_ip_cu_prop, aes(x = WHO_region_recoded, y = mean_value_adj, fill = pandemic)) +
                                     geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                                     geom_errorbar(
                                       aes(ymin = lower_ci_adj / 1e6, ymax = upper_ci_adj / 1e6),
                                       position = position_dodge(width = 0.9), width = 0.2
                                     ) +
                                     scale_fill_manual(
                                       values = c(
                                         "Scenario One" = "#FFC1C2",   # pink
                                         "Scenario Two" = "#C1C3FF",   # purple/blue
                                         "Scenario Three" = "grey"       # grey
                                       ),
                                       name = "Pandemic"
                                     ) +
                                     scale_y_continuous(labels = scales::label_comma())+
                                     facet_grid(Outcome ~ age_testing_strategy, scales = "free_y", labeller = labeller(
                                       Outcome = outcome_labels,
                                       age_testing_strategy = strategy_labels
                                     )
                                     ) +
                                     labs(
                                       x = "WHO region",
                                       y = "Difference between seasonal and pandemic standardising for population of region"
                                     ) +
                                     theme_bw(base_size = 14) +  # Makes everything bigger
                                     theme(
                                       axis.text.x = element_text(angle = 45, hjust = 1),
                                       strip.text = element_text(size = 9),             # Bigger facet labels
                                       legend.position = "top",                           # Move legend to top
                                       legend.title = element_text(size = 12),
                                       legend.text = element_text(size = 10),
                                       panel.spacing = unit(0.8, "lines"),                # More space between facets
                                       plot.title = element_text(hjust = 0.5, size = 16)  # Centered and larger title
                                     )
                                  
                                   
                                   line_chart_sterilising$pandemic = recode(line_chart_sterilising$pandemic,
                                                              '1918' = 'Scenario One',
                                                              '1957'='Scenario Two',
                                                              '2009' = 'Scenario Three')
                                   
                                   
                                   line_chart_sterilising$WHO_region <- factor(
                                     line_chart_sterilising$WHO_region,
                                     levels = c('SEAR', 'WPR', 'AMR', 'EMR', 	
                                                'EUR', 'AFR' ),
                                     labels = c("South-East Asia Region", 'Western Pacific Region',
                                                'Region of the Americas', 'Eastern Mediterranean Region',
                                                'European Region', 'African Region'
                                     )
                                   )
                                   
                                   line_graph_ster <- ggplot(line_chart_sterilising, aes(x = time_of_pandemic, y = threshold_price, color = WHO_region, group = WHO_region)) +
                                     geom_line(size = 1) +
                                     geom_point(size = 2) +
                                     facet_wrap(~ pandemic, nrow = 1) +
                                     scale_color_manual(values = WHO_colors_2) +
                                     scale_x_continuous(
                                       breaks = seq(min(line_chart_sterilising$time_of_pandemic), max(line_chart_sterilising$time_of_pandemic), by = 1)
                                     ) +
                                     labs(
                                       x = "Year of Pandemic",
                                       y = "Threshold Price",
                                       color = "WHO Region",
                                       title = "Threshold Price Over Time by region and Pandemic Type (Sterilising)"
                                     ) +
                                     theme_bw() +
                                     theme(
                                       strip.text = element_text(size = 12),
                                       axis.text.x = element_text(angle = 45, hjust = 1)
                                     )
                                   
                                   line_chart_disease_mod$pandemic = recode(line_chart_disease_mod$pandemic,
                                                                            '1918' = 'Scenario One',
                                                                            '1957'='Scenario Two',
                                                                            '2009' = 'Scenario Three')
                                   
                                   line_chart_disease_mod$WHO_region <- factor(
                                     line_chart_disease_mod$WHO_region,
                                     levels = c('SEAR', 'WPR', 'AMR', 'EMR', 	
                                                'EUR', 'AFR' ),
                                     labels = c("South-East Asia Region", 'Western Pacific Region',
                                                'Region of the Americas', 'Eastern Mediterranean Region',
                                                'European Region', 'African Region'
                                     )
                                   )
                                   
                                   
                                   
                                   line_graph_dm <- ggplot(line_chart_disease_mod, aes(x = time_of_pandemic, y = threshold_price, color = WHO_region, group = WHO_region)) +
                                     geom_line(size = 1) +
                                     geom_point(size = 2) +
                                     facet_wrap(~ pandemic, nrow = 1) +
                                     scale_color_manual(values = WHO_colors_2) +
                                     scale_x_continuous(
                                       breaks = seq(min(line_chart_disease_mod$time_of_pandemic), max(line_chart_disease_mod$time_of_pandemic), by = 1)
                                     ) +
                                     labs(
                                       x = "Year of Pandemic",
                                       y = "Threshold Price",
                                       color = "WHO Region",
                                       title = "Threshold Price Over Time by region and Pandemic Type (Disease modifying)"
                                     ) +
                                     theme_bw() +
                                     theme(
                                       strip.text = element_text(size = 12),
                                       axis.text.x = element_text(angle = 45, hjust = 1)
                                     )
                                   
                                   line_chart_infectious_period$pandemic = recode(line_chart_infectious_period$pandemic,
                                                                            '1918' = 'Scenario One',
                                                                            '1957'='Scenario Two',
                                                                            '2009' = 'Scenario Three')
                                   
                                   line_chart_infectious_period$WHO_region <- factor(
                                     line_chart_infectious_period$WHO_region,
                                     levels = c('SEAR', 'WPR', 'AMR', 'EMR', 	
                                                'EUR', 'AFR' ),
                                     labels = c("South-East Asia Region", 'Western Pacific Region',
                                                'Region of the Americas', 'Eastern Mediterranean Region',
                                                'European Region', 'African Region'
                                                )
                                   )
                                   
                                   line_graph_ip <- ggplot(line_chart_infectious_period, aes(x = time_of_pandemic, y = threshold_price, color = WHO_region, group = WHO_region)) +
                                     geom_line(size = 1) +
                                     geom_point(size = 2) +
                                     facet_wrap(~ pandemic, nrow = 1) +
                                     scale_color_manual(values = WHO_colors_2) +
                                     scale_x_continuous(
                                       breaks = seq(min(line_chart_infectious_period$time_of_pandemic), max(line_chart_infectious_period$time_of_pandemic), by = 1)
                                     ) +
                                     labs(
                                       x = "Year of Pandemic",
                                       y = "Threshold Price",
                                       color = "WHO region",
                                       title = "Threshold Price Over Time by region and Pandemic Type (Infectious Period)"
                                     ) +
                                     theme_bw() +
                                     theme(
                                       strip.text = element_text(size = 12),
                                       axis.text.x = element_text(angle = 45, hjust = 1)
                                     )
                                   
                                 }
                                 
                                 
  }
  
  
}





