
set.seed(123)

simulations <-100
start_year_of_analysis <- 2025
years_of_analysis <- 30

national_ifrs <- data.table(read_csv('data/econ/national_ifrs.csv',
                                     show_col_types=F))

pandemic_ifrs <- data.table(read_csv('data/econ/pandemic_scns_ifr.csv',
                                     show_col_types=F))
pandemic_ifrs$age_grp <- as.character(pandemic_ifrs$age_grp)

disease_scenarios <- c('1918', '1957', '2009')[3]
if (disease_scenarios == '1918'){
  susceptibility_range <- c(0.80, 0.9)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.9)
  r0 <- NA
} else if (disease_scenarios == '1957'){
  susceptibility_range <- c(0.60, 0.8)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.7,0.9)
  r0 <- NA
} else if (disease_scenarios == '2009'){
  susceptibility_range <- c(0.50, 0.7)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.95)
  r0 <- NA
}

vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[1]

if (vaccine_strategy_pandemics == 'sterilising'){
  vacc_type_list_pand <- vacc_type_list_sterilising
} else if (vaccine_strategy_pandemics == 'disease mod'){
  vacc_type_list_pand <- vacc_type_list_dis_mod
} else if (vaccine_strategy_pandemics == 'infection period'){
  vacc_type_list_pand <- vacc_type_list_reduced_infec
}

seasonal_flu_included <- c('TRUE', 'FALSE')[1]

death_list_0_2009 <- list()
death_list_A1_2009 <- list()
death_list_B1_2009 <- list()
death_list_B2_2009 <- list()
death_list_C_2009 <- list()

for (pand_time in 0:28){
  
  if (seasonal_flu_included == 'TRUE'){
    epidemic_data <- converting_epidemic_code(itz_input,years_of_analysis,simulations, ageing_date)
    
    epid_dt <<- epidemic_data
  } else{
    titles <- c('simulation_index', 'susceptibility', 'transmissibility', 'r0_to_scale', 
                'match', 'start_date_late','original_date', 'ageing_year_start',
                'epid_start_date', 'initial_infected', 'period_start_date', 'end_date', 'susceptibility_for_kids')
    epid_dt <<- data.frame(matrix(nrow=0, ncol=length(titles)))
    colnames(epid_dt) <- titles
  }
  epid_dt <- Pandemic_addition_function(epid_dt, simulations, pand_time, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                        start_year_of_analysis, years_of_analysis)
  
  infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
  all_epid <- list(infs_rds_list[[1]][[1]], infs_rds_list[[2]][[1]], infs_rds_list[[3]][[1]], infs_rds_list[[4]][[1]], infs_rds_list[[5]][[1]], infs_rds_list[[6]][[1]])
  pandemic_only <- list(infs_rds_list[[1]][[2]], infs_rds_list[[2]][[2]], infs_rds_list[[3]][[2]], infs_rds_list[[4]][[2]], infs_rds_list[[5]][[2]], infs_rds_list[[6]][[1]])
  
  all_epid_dt <- rbindlist(all_epid)
  pandemic_dt <- rbindlist(pandemic_only)
  seasonal_dt <- all_epid_dt
  seasonal_dt[, 2:16] <- seasonal_dt[,2:16]- pandemic_dt[,2:16]
  
  pandemic_dt <- restrict_pandemictime(epid_dt, pandemic_dt, 6)
  seasonal_dt <- restrict_pandemictime(epid_dt, seasonal_dt, 6)
  
  summary_statistic <- summary_overall_deaths(seasonal_dt, pandemic_dt, 'GBR', disease_scenarios , national_ifrs, pandemic_ifrs)
  print(pand_time)
  death_list_0_2009[[paste('pan_year_', pand_time)]] <- summary_statistic[vacc_type=='0']$total_deaths
  death_list_A1_2009[[paste('pan_year_', pand_time)]] <- summary_statistic[vacc_type=='A.1']$total_deaths
  death_list_B1_2009[[paste('pan_year_', pand_time)]] <- summary_statistic[vacc_type=='B.1']$total_deaths
  death_list_B2_2009[[paste('pan_year_', pand_time)]] <- summary_statistic[vacc_type=='B.2']$total_deaths
  death_list_C_2009[[paste('pan_year_', pand_time)]] <- summary_statistic[vacc_type=='C']$total_deaths
  
}

#1918

death_diff_1918 <- list()
death_diff_1918$pan_year_0 <- death_list_0_1918$`pan_year_ 0` - death_list_C_1918$`pan_year_ 0`
death_diff_1918$pan_year_1 <- death_list_0_1918$`pan_year_ 1` - death_list_C_1918$`pan_year_ 1`
death_diff_1918$pan_year_2 <- death_list_0_1918$`pan_year_ 2` - death_list_C_1918$`pan_year_ 2`
death_diff_1918$pan_year_3 <- death_list_0_1918$`pan_year_ 3` - death_list_C_1918$`pan_year_ 3`
death_diff_1918$pan_year_4 <- death_list_0_1918$`pan_year_ 4` - death_list_C_1918$`pan_year_ 4`
death_diff_1918$pan_year_5 <- death_list_0_1918$`pan_year_ 5` - death_list_C_1918$`pan_year_ 5`
death_diff_1918$pan_year_6 <- death_list_0_1918$`pan_year_ 6` - death_list_C_1918$`pan_year_ 6`
death_diff_1918$pan_year_7 <- death_list_0_1918$`pan_year_ 7` - death_list_C_1918$`pan_year_ 7`
death_diff_1918$pan_year_8 <- death_list_0_1918$`pan_year_ 8` - death_list_C_1918$`pan_year_ 8`
death_diff_1918$pan_year_9 <- death_list_0_1918$`pan_year_ 9` - death_list_C_1918$`pan_year_ 9`
death_diff_1918$pan_year_10 <- death_list_0_1918$`pan_year_ 10` - death_list_C_1918$`pan_year_ 10`
death_diff_1918$pan_year_11 <- death_list_0_1918$`pan_year_ 11` - death_list_C_1918$`pan_year_ 11`
death_diff_1918$pan_year_12 <- death_list_0_1918$`pan_year_ 12` - death_list_C_1918$`pan_year_ 12`
death_diff_1918$pan_year_13 <- death_list_0_1918$`pan_year_ 13` - death_list_C_1918$`pan_year_ 13`
death_diff_1918$pan_year_14 <- death_list_0_1918$`pan_year_ 14` - death_list_C_1918$`pan_year_ 14`
death_diff_1918$pan_year_15 <- death_list_0_1918$`pan_year_ 15` - death_list_C_1918$`pan_year_ 15`
death_diff_1918$pan_year_16 <- death_list_0_1918$`pan_year_ 16` - death_list_C_1918$`pan_year_ 16`
death_diff_1918$pan_year_17 <- death_list_0_1918$`pan_year_ 17` - death_list_C_1918$`pan_year_ 17`
death_diff_1918$pan_year_18 <- death_list_0_1918$`pan_year_ 18` - death_list_C_1918$`pan_year_ 18`
death_diff_1918$pan_year_19 <- death_list_0_1918$`pan_year_ 19` - death_list_C_1918$`pan_year_ 19`
death_diff_1918$pan_year_20 <- death_list_0_1918$`pan_year_ 20` - death_list_C_1918$`pan_year_ 20`
death_diff_1918$pan_year_21 <- death_list_0_1918$`pan_year_ 21` - death_list_C_1918$`pan_year_ 21`
death_diff_1918$pan_year_22 <- death_list_0_1918$`pan_year_ 22` - death_list_C_1918$`pan_year_ 22`
death_diff_1918$pan_year_23 <- death_list_0_1918$`pan_year_ 23` - death_list_C_1918$`pan_year_ 23`
death_diff_1918$pan_year_24 <- death_list_0_1918$`pan_year_ 24` - death_list_C_1918$`pan_year_ 24`
death_diff_1918$pan_year_25 <- death_list_0_1918$`pan_year_ 25` - death_list_C_1918$`pan_year_ 25`
death_diff_1918$pan_year_26 <- death_list_0_1918$`pan_year_ 26` - death_list_C_1918$`pan_year_ 26`
death_diff_1918$pan_year_27 <- death_list_0_1918$`pan_year_ 27` - death_list_C_1918$`pan_year_ 27`
death_diff_1918$pan_year_28 <- death_list_0_1918$`pan_year_ 28` - death_list_C_1918$`pan_year_ 28`

deaths_difference_1918 <- matrix(unlist(death_diff_1918), nrow = 100)

#1957

death_diff_1957 <- list()
death_diff_1957$pan_year_0 <- death_list_0_1957$`pan_year_ 0` - death_list_C_1957$`pan_year_ 0`
death_diff_1957$pan_year_1 <- death_list_0_1957$`pan_year_ 1` - death_list_C_1957$`pan_year_ 1`
death_diff_1957$pan_year_2 <- death_list_0_1957$`pan_year_ 2` - death_list_C_1957$`pan_year_ 2`
death_diff_1957$pan_year_3 <- death_list_0_1957$`pan_year_ 3` - death_list_C_1957$`pan_year_ 3`
death_diff_1957$pan_year_4 <- death_list_0_1957$`pan_year_ 4` - death_list_C_1957$`pan_year_ 4`
death_diff_1957$pan_year_5 <- death_list_0_1957$`pan_year_ 5` - death_list_C_1957$`pan_year_ 5`
death_diff_1957$pan_year_6 <- death_list_0_1957$`pan_year_ 6` - death_list_C_1957$`pan_year_ 6`
death_diff_1957$pan_year_7 <- death_list_0_1957$`pan_year_ 7` - death_list_C_1957$`pan_year_ 7`
death_diff_1957$pan_year_8 <- death_list_0_1957$`pan_year_ 8` - death_list_C_1957$`pan_year_ 8`
death_diff_1957$pan_year_9 <- death_list_0_1957$`pan_year_ 9` - death_list_C_1957$`pan_year_ 9`
death_diff_1957$pan_year_10 <- death_list_0_1957$`pan_year_ 10` - death_list_C_1957$`pan_year_ 10`
death_diff_1957$pan_year_11 <- death_list_0_1957$`pan_year_ 11` - death_list_C_1957$`pan_year_ 11`
death_diff_1957$pan_year_12 <- death_list_0_1957$`pan_year_ 12` - death_list_C_1957$`pan_year_ 12`
death_diff_1957$pan_year_13 <- death_list_0_1957$`pan_year_ 13` - death_list_C_1957$`pan_year_ 13`
death_diff_1957$pan_year_14 <- death_list_0_1957$`pan_year_ 14` - death_list_C_1957$`pan_year_ 14`
death_diff_1957$pan_year_15 <- death_list_0_1957$`pan_year_ 15` - death_list_C_1957$`pan_year_ 15`
death_diff_1957$pan_year_16 <- death_list_0_1957$`pan_year_ 16` - death_list_C_1957$`pan_year_ 16`
death_diff_1957$pan_year_17 <- death_list_0_1957$`pan_year_ 17` - death_list_C_1957$`pan_year_ 17`
death_diff_1957$pan_year_18 <- death_list_0_1957$`pan_year_ 18` - death_list_C_1957$`pan_year_ 18`
death_diff_1957$pan_year_19 <- death_list_0_1957$`pan_year_ 19` - death_list_C_1957$`pan_year_ 19`
death_diff_1957$pan_year_20 <- death_list_0_1957$`pan_year_ 20` - death_list_C_1957$`pan_year_ 20`
death_diff_1957$pan_year_21 <- death_list_0_1957$`pan_year_ 21` - death_list_C_1957$`pan_year_ 21`
death_diff_1957$pan_year_22 <- death_list_0_1957$`pan_year_ 22` - death_list_C_1957$`pan_year_ 22`
death_diff_1957$pan_year_23 <- death_list_0_1957$`pan_year_ 23` - death_list_C_1957$`pan_year_ 23`
death_diff_1957$pan_year_24 <- death_list_0_1957$`pan_year_ 24` - death_list_C_1957$`pan_year_ 24`
death_diff_1957$pan_year_25 <- death_list_0_1957$`pan_year_ 25` - death_list_C_1957$`pan_year_ 25`
death_diff_1957$pan_year_26 <- death_list_0_1957$`pan_year_ 26` - death_list_C_1957$`pan_year_ 26`
death_diff_1957$pan_year_27 <- death_list_0_1957$`pan_year_ 27` - death_list_C_1957$`pan_year_ 27`
death_diff_1957$pan_year_28 <- death_list_0_1957$`pan_year_ 28` - death_list_C_1957$`pan_year_ 28`

deaths_difference_1957 <- matrix(unlist(death_diff_1957), nrow = 100)

#1957

death_diff_2009 <- list()
death_diff_2009$pan_year_0 <- death_list_0_2009$`pan_year_ 0` - death_list_C_2009$`pan_year_ 0`
death_diff_2009$pan_year_1 <- death_list_0_2009$`pan_year_ 1` - death_list_C_2009$`pan_year_ 1`
death_diff_2009$pan_year_2 <- death_list_0_2009$`pan_year_ 2` - death_list_C_2009$`pan_year_ 2`
death_diff_2009$pan_year_3 <- death_list_0_2009$`pan_year_ 3` - death_list_C_2009$`pan_year_ 3`
death_diff_2009$pan_year_4 <- death_list_0_2009$`pan_year_ 4` - death_list_C_2009$`pan_year_ 4`
death_diff_2009$pan_year_5 <- death_list_0_2009$`pan_year_ 5` - death_list_C_2009$`pan_year_ 5`
death_diff_2009$pan_year_6 <- death_list_0_2009$`pan_year_ 6` - death_list_C_2009$`pan_year_ 6`
death_diff_2009$pan_year_7 <- death_list_0_2009$`pan_year_ 7` - death_list_C_2009$`pan_year_ 7`
death_diff_2009$pan_year_8 <- death_list_0_2009$`pan_year_ 8` - death_list_C_2009$`pan_year_ 8`
death_diff_2009$pan_year_9 <- death_list_0_2009$`pan_year_ 9` - death_list_C_2009$`pan_year_ 9`
death_diff_2009$pan_year_10 <- death_list_0_2009$`pan_year_ 10` - death_list_C_2009$`pan_year_ 10`
death_diff_2009$pan_year_11 <- death_list_0_2009$`pan_year_ 11` - death_list_C_2009$`pan_year_ 11`
death_diff_2009$pan_year_12 <- death_list_0_2009$`pan_year_ 12` - death_list_C_2009$`pan_year_ 12`
death_diff_2009$pan_year_13 <- death_list_0_2009$`pan_year_ 13` - death_list_C_2009$`pan_year_ 13`
death_diff_2009$pan_year_14 <- death_list_0_2009$`pan_year_ 14` - death_list_C_2009$`pan_year_ 14`
death_diff_2009$pan_year_15 <- death_list_0_2009$`pan_year_ 15` - death_list_C_2009$`pan_year_ 15`
death_diff_2009$pan_year_16 <- death_list_0_2009$`pan_year_ 16` - death_list_C_2009$`pan_year_ 16`
death_diff_2009$pan_year_17 <- death_list_0_2009$`pan_year_ 17` - death_list_C_2009$`pan_year_ 17`
death_diff_2009$pan_year_18 <- death_list_0_2009$`pan_year_ 18` - death_list_C_2009$`pan_year_ 18`
death_diff_2009$pan_year_19 <- death_list_0_2009$`pan_year_ 19` - death_list_C_2009$`pan_year_ 19`
death_diff_2009$pan_year_20 <- death_list_0_2009$`pan_year_ 20` - death_list_C_2009$`pan_year_ 20`
death_diff_2009$pan_year_21 <- death_list_0_2009$`pan_year_ 21` - death_list_C_2009$`pan_year_ 21`
death_diff_2009$pan_year_22 <- death_list_0_2009$`pan_year_ 22` - death_list_C_2009$`pan_year_ 22`
death_diff_2009$pan_year_23 <- death_list_0_2009$`pan_year_ 23` - death_list_C_2009$`pan_year_ 23`
death_diff_2009$pan_year_24 <- death_list_0_2009$`pan_year_ 24` - death_list_C_2009$`pan_year_ 24`
death_diff_2009$pan_year_25 <- death_list_0_2009$`pan_year_ 25` - death_list_C_2009$`pan_year_ 25`
death_diff_2009$pan_year_26 <- death_list_0_2009$`pan_year_ 26` - death_list_C_2009$`pan_year_ 26`
death_diff_2009$pan_year_27 <- death_list_0_2009$`pan_year_ 27` - death_list_C_2009$`pan_year_ 27`
death_diff_2009$pan_year_28 <- death_list_0_2009$`pan_year_ 28` - death_list_C_2009$`pan_year_ 28`


#creating datatable of the resutls from the model


deaths_difference_2009 <- as.data.frame(death_diff_2009)
deaths_difference_1957 <- as.data.frame(death_diff_1957)
deaths_difference_1918 <- as.data.frame(death_diff_1918)

#getting the quantiles of deaths
quants <- c(0.25, 0.5, 0.75)
death_quantiles_2009 <- sapply(deaths_difference_2009, quantile, probs=quants)
death_quantiles_1957 <- sapply(deaths_difference_1957, quantile, probs=quants)
death_quantiles_1918 <- sapply(deaths_difference_1918, quantile, probs=quants)

death_mean_2009 <- sapply(deaths_difference_2009, mean)
death_mean_1957 <- sapply(deaths_difference_1957, mean)
death_mean_1918 <- sapply(deaths_difference_1918, mean)

death_quantiles_2009 <- as.data.frame(rbind(death_quantiles_2009, death_mean_2009))
death_quantiles_1957 <- as.data.frame(rbind(death_quantiles_1957, death_mean_1957))
death_quantiles_1918 <- as.data.frame(rbind(death_quantiles_1918, death_mean_1918))


Statistic = c("q25", "q50", "q75", "Mean")
death_quantiles_1957 <- cbind(Statistic, death_quantiles_1957)
death_quantiles_2009 <- cbind(Statistic, death_quantiles_2009)
death_quantiles_1918 <- cbind(Statistic, death_quantiles_1918)


combined_1957 <- death_quantiles_1957 %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)

combined_2009 <- death_quantiles_2009 %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)
combined_1918 <- death_quantiles_1918 %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)


combined_1957$Year <- as.numeric(gsub("[^0-9]", "", combined_1957$Year))
combined_1957 %>% mutate(Year = as.factor(Year))

combined_1918$Year <- as.numeric(gsub("[^0-9]", "", combined_1918$Year))
combined_1918 %>% mutate(Year = as.factor(Year))

combined_2009$Year <- as.numeric(gsub("[^0-9]", "", combined_2009$Year))
combined_2009 %>% mutate(Year = as.factor(Year))

plot1 <- ggplot(combined_1957, aes(x = Year, y = Mean/1e5)) +
  geom_bar(stat = "identity", fill = '#C1C3FF') +
  geom_errorbar(aes(ymin = q25/1e5, ymax = q75/1e5), width = 0.2, color = "black") +
  labs(title = "Total Deaths Averted by Pandemic Year for NGIVs vs seasonal flu vaccines (1957)", y = "Deaths Averted (100,000)", x = "Year of Pandemic") +
  ylim(0, 5.5)+
  theme_minimal()

plot2 <- ggplot(combined_1918, aes(x = Year, y = Mean/1e5)) +
  geom_bar(stat = "identity", fill = '#FFC1C2') +
  geom_errorbar(aes(ymin = q25/1e5, ymax = q75/1e5), width = 0.2, color = "black") +
  labs(title = "Total Deaths Averted by Pandemic Year for NGIVs vs seasonal flu vaccines (1918)", y = "Deaths Averted (100,000)", x = "Year of Pandemic") +
  ylim(0, 5.5)+
  theme_minimal()

plot3<- ggplot(combined_2009, aes(x = Year, y = Mean/1e5)) +
  geom_bar(stat = "identity", fill = 'grey') +
  geom_errorbar(aes(ymin = q25/1e5, ymax = q75/1e5), width = 0.2, color = "black") +
  labs(title = "Total Deaths Averted by Pandemic Year for NGIVs vs seasonal flu vaccines (2009)", y = "Deaths Averted (100,000)", x = "Year of Pandemic") +
  ylim(0, 5.5)+
  theme_minimal()

plot2/plot1/plot3


