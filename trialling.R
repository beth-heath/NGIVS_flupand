
### Creating diagram for comparitive analysis

creating_data_for_bar_chart <- function(ITZregion, country_of_interest, age_testing_strategy, year_of_interest , years, pand_dt,
                              symp_samples,dcr_infr, global_ihrs,
                              national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                              cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                              cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                              doses_info, wastage, dose_price, case_proportion){
  
  creating_dataset <- Pandemic_impact(ITZregion, country_of_interest, age_testing_strategy, year_of_interest , years, pand_dt,
                                      symp_samples,dcr_infr, global_ihrs,
                                      national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                      cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                      cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                      doses_info, wastage, dose_price, case_proportion)
 
  creating_dataset <- creating_dataset %>% mutate(infection_difference = total_infections.x - total_infections.y,
                              hospitilisations_difference = hospitalisations.x - hospitalisations.y,
                              deaths_difference = total_deaths.x - total_deaths.y,
                              DALY_difference = total_DALYS.x - total_DALYS.y,
                              cost_difference = total_cost.x - total_cost.y,
                              dose_cost_difference = cost_by_dose.x - cost_by_dose.y)
  
  creating_dataset <- creating_dataset %>%
    group_by(vacc_type) %>%
    summarise(
      infection_difference = mean(infection_difference), 
      hospitilisations_difference = mean(hospitilisations_difference),
      deaths_difference = mean(deaths_difference),
      DALY_difference = mean(DALY_difference),
      cost_difference =mean(cost_difference),
      dose_cost_difference = mean(dose_cost_difference),
      .groups = 'drop'
    )
  
  
  creating_dataset$ITZ <- ITZregion
  creating_dataset$country <- country_of_interest
  creating_dataset$age_testing_strategy <- age_testing_strategy
  creating_dataset$pandemic <- year_of_interest
  creating_dataset$time_of_pandemic <- years
  
  return(creating_dataset)
  
}



ITZregion <- 7
year_of_interest <- 1918
case_proportion <- 0.65
c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[ITZregion]
country_list <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
pand_dt <- pandemic_combined

test <- 1

country_of_interest <- country_list [1]

  for (age_testing_strategy in 1:5){
    for (years in 1:28){
      outputting <- creating_data_for_bar_chart(ITZregion, country_of_interest, age_testing_strategy, year_of_interest , years, pand_dt,
                                  symp_samples,dcr_infr, global_ihrs,
                                  national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                  cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                  cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                  doses_info, wastage, dose_price, case_proportion)
      if (test == 1){
        output <- outputting
        test <- 2
      } else{
        output <- rbind(output, outputting)
      }
      
       
    }
  }

save(output, file=paste0('File of', c_name, '.RData'))



trial_output <- output %>%
  group_by(ITZ, vacc_type, country, age_testing_strategy, pandemic ) %>%
  summarise(
    infection_difference = mean(infection_difference), 
    hospitilisations_difference = mean(hospitilisations_difference),
    deaths_difference = mean(deaths_difference),
    DALY_difference = mean(DALY_difference),
    cost_difference =mean(cost_difference),
    dose_cost_difference = mean(dose_cost_difference),
    .groups = 'drop'
  )



df_long <- trial_output %>%
  pivot_longer(
    cols = ends_with("_difference"),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  mutate(
    Outcome = recode(Outcome,
                     infection_difference = "Infections",
                     hospitilisations_difference = "Hospitalisations",
                     deaths_difference = "Deaths",
                     DALY_difference = "DALYs",
                     cost_difference = "Cost",
                     dose_cost_difference = 'Dose cost'),
    vacc_type = recode(vacc_type,
                       '0'='No vaccination',
                       'A.1' = 'Improved (minimal)',
                       'A.2'='Current',
                       'B.1'='Improved (efficacy)',
                       'B.2'= 'Improved (breadth)',
                       'C'='Universal'
                       ),
    
    VaccineType = as.factor(vacc_type),
    ITZ = recode(ITZ,
                 '1' = 'ITZ1',
                 '2'='ITZ2',
                 '3' = 'ITZ3',
                 '4' = 'ITZ4',
                 '5' = 'ITZ5',
                 '6'='ITZ6',
                 '7'='ITZ7')
  )

df_long <- df_long %>% mutate(age_testing_strategy = recode(age_testing_strategy,
                                                         '1' = '0-4',
                                                         '2'='0-10',
                                                         '3'='0-17',
                                                         '4'='65+',
                                                         '5'='0-17, 65+'
                                                            
                                                            ),
                              age_testing_strategy = factor(age_testing_strategy, levels = c("0-4", "0-10", "0-17", "65+", "0-17, 65+"))
                              )



ggplot(df_long, aes(x = ITZ, y = Value / 1e6, fill = VaccineType)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(rows = vars(Outcome), cols = vars(age_testing_strategy), scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "ITZ",
    y = "Difference between seasonal and pandemic (millions)",
    fill = "Vaccine Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )








Pandemic_impact(ITZregion, country_of_interest, age_testing_strategy, year_of_interest , years, pand_dt,
                symp_samples,dcr_infr, global_ihrs,
                national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                doses_info, wastage, dose_price, case_proportion)

trial1<- Pandemic_impact(4, 'GBR', 3, 1918, 2, epid_dt, symp_samples, dcr_infr, global_ihrs,
                national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                doses_info, wastage, dose_price, 0.8)

trial2 <- trial1 %>% mutate(infection_difference = total_infections.x - total_infections.y,
                            hospitilisations_difference = hospitalisations.x - hospitalisations.y,
                            deaths_difference = total_deaths.x - total_deaths.y,
                            DALY_difference = total_DALYS.x - total_DALYS.y,
                            cost_difference = total_cost.x - total_cost.y,
                            dose_cost_difference = cost_by_dose.x - cost_by_dose.y)


average_dataset <- trial2 %>%
  group_by(vacc_type) %>%
  summarise(
    infection_difference = mean(infection_difference), 
    hospitilisations_difference = mean(hospitilisations_difference),
    deaths_difference = mean(deaths_difference),
    DALY_difference = mean(DALY_difference),
    cost_difference =mean(cost_difference),
    dose_cost_difference = mean(dose_cost_difference),
    .groups = 'drop'
  )




