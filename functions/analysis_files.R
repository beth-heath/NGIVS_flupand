
continent <- 6

c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[continent]
country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 

years_of_interest <- c(1918, 1957, 2009)

hospital_ratio <- 10 

age_testing_strategy <- 5

years_list <- c()
country_list <- c()
pandemic_list <- c()
diff_seasonal <- c()
diff_none <- c()

for (years in 1:28){
  for (country in country_codes){
    for (pandemic in years_of_interest){
      
      print(years)
      
      pandemic_zone_6 <- pandemic_datasets_6(country, age_testing_strategy, pandemic, years)
      epidemic_zone_6 <- epidemic_datasets_combine_6(country, age_testing_strategy, pand_dt[pand_dt$year_pandemic == years, ])
      
      pandemic_analyse <- total_DALYS_for_pandemic_updated(pandemic_zone_6, symp_samples, dcr_infr, pandemic, hospital_ratio, 
                                                           global_ihrs, outpatient_ratios, country ,
                                                           DALY_weight_samples, pandemic_ifrs, yll_df, national_ifrs)
      
      epidemic_analyse <- total_DALYS_for_seasonal(epidemic_zone_6, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,
                                                   country,
                                        national_ifrs, yll_df)
      combined_analyse <- rbind(pandemic_analyse, epidemic_analyse)
      
      combined_test_DALY <- combined_analyse %>% select(age_grp, simulation_index, vacc_type, year, total_DALYS)
  
      
      
      combined_test_DALY_1 <- combined_test_DALY %>%
        group_by(simulation_index, vacc_type) %>%
        summarise(total_DALYS = sum(total_DALYS),
                  .groups='drop')
      
      seasonal_flu <- combined_test_DALY_1[combined_test_DALY_1$vacc_type == "A.2", ]$total_DALYS
      next_gen_flu <- combined_test_DALY_1[combined_test_DALY_1$vacc_type == "C", ]$total_DALYS
      no_vaccination <- combined_test_DALY_1[combined_test_DALY_1$vacc_type == "0", ]$total_DALYS
      
      diff_seasonal_and_next <- seasonal_flu -next_gen_flu
      diff_none_and_next <- no_vaccination -next_gen_flu
      
      years_list <- c(years_list, years)
      country_list <- c(country_list, country)
      pandemic_list <- c(pandemic_list, pandemic)
      diff_seasonal  <- c(diff_seasonal , mean(diff_seasonal_and_next))
      diff_none <- c(diff_none, mean(diff_none_and_next))
      
    }
    
  }
}

combined_data <- data.frame(years_list, country_list, pandemic_list, diff_seasonal, diff_none)

combined_data_1918 <- combined_data[combined_data$pandemic_list==1918,]
reduced_example <- combined_data_1918  %>% select(years_list, country_list, diff_seasonal)

reduced_example_avg <- reduced_example %>%
  group_by(years_list) %>%
  summarise(avg_diff = mean(diff_seasonal), .groups = "drop")


ggplot() +
  geom_point(data = reduced_example, aes(x = years_list, y = diff_seasonal), color = "red", size = 2) +  # fixed color
  geom_line(data = reduced_example_avg, aes(x = years_list, y = avg_diff), color = "red", size = 1.2) +
  labs(title = "DALYs reduced for next-gen flu vaccines versus seasonal flu (1918)",
       x = "Year of Pandemic", y = "DALYs reduced") +
  theme_minimal()


combined_data_1957 <- combined_data[combined_data$pandemic_list==1957,]
reduced_example <- combined_data_1957  %>% select(years_list, country_list, diff_seasonal)

reduced_example_avg <- reduced_example %>%
  group_by(years_list) %>%
  summarise(avg_diff = mean(diff_seasonal), .groups = "drop")


ggplot() +
  geom_point(data = reduced_example, aes(x = years_list, y = diff_seasonal), color = "red", size = 2) +  # fixed color
  geom_line(data = reduced_example_avg, aes(x = years_list, y = avg_diff), color = "red", size = 1.2) +
  labs(title = "DALYs reduced for next-gen flu vaccines versus seasonal flu (1957)",
       x = "Year of Pandemic", y = "DALYs reduced") +
  theme_minimal()


combined_data_2009 <- combined_data[combined_data$pandemic_list==2009,]
reduced_example <- combined_data_2009  %>% select(years_list, country_list, diff_seasonal)

reduced_example_avg <- reduced_example %>%
  group_by(years_list) %>%
  summarise(avg_diff = mean(diff_seasonal), .groups = "drop")


ggplot() +
  geom_point(data = reduced_example, aes(x = years_list, y = diff_seasonal), color = "red", size = 2) +  # fixed color
  geom_line(data = reduced_example_avg, aes(x = years_list, y = avg_diff), color = "red", size = 1.2) +
  labs(title = "DALYs reduced for next-gen flu vaccines versus seasonal flu (2009)",
       x = "Year of Pandemic", y = "DALYs reduced") +
  theme_minimal()




