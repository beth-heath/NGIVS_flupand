
##### Pandemic datasets #######

pandemic_datasets <- function(ITZ, country, age_groups, year_of_interest, pandemic_year, coverage_scenario){
  
  condensed_c_name <- c("Africa", "Asia-Europe", "EasternandSouthernAsia",
              "Europe", "NorthernAmerica", "Oceania-Melanesia-Polynesia",
              "SouthernAmerica")[ITZ]
  
  if (year_of_interest == 1918){
    year_of_interest = 1
  } else if (year_of_interest == 1957){
    year_of_interest = 2
  } else if (year_of_interest == 2009){
    year_of_interest = 3
  }
  
  if (pandemic_year <= 5){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_1.parquet')), compression = "zstd")
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    ##selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_1.parquet')), compression = "zstd")
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    ##selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_2.parquet')), compression = "zstd")
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year < 12 & pandemic_year >6){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_2.parquet')), compression = "zstd")
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_2.parquet')), compression = "zstd")
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_3.parquet')), compression = "zstd")
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if  (pandemic_year <17 & 12 <pandemic_year){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_3.parquet')), compression = "zstd")
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_3.parquet')), compression = "zstd")
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_4.parquet')), compression = "zstd")
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_4.parquet')), compression = "zstd")
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_4.parquet')), compression = "zstd")
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_5.parquet')), compression = "zstd")
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <-read_parquet(here::here('Run_script',paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(condensed_c_name, country,age_groups,'pansn', year_of_interest,'_5.parquet')), compression = "zstd")
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
  restricted_dataset <- restricted_dataset %>% mutate(year = year(time))
  
  summary_dataset <- restricted_dataset %>%
    group_by(year, vacc_type, simulation_index, mechanism) %>%
    summarise(I1 = sum(I1),
              I2 = sum(I2),
              I3=sum(I3),
              I4 = sum(I4),
              IU1 = sum(IU1),
              IU2 = sum(IU2),
              IU3 = sum(IU3),
              IU4 = sum(IU4),
              IV1 = sum(IV1),
              IV2 = sum(IV2),
              IV3 = sum(IV3),
              IV4 = sum(IV4),
              IVR1= sum(IVR1),
              IVR2 = sum(IVR2),
              IVR3 = sum(IVR3),
              IVR4 = sum(IVR4),
              tot=sum(tot),
              .groups='drop')
  
  return(summary_dataset)
  
}

#### Epidemic dataset ###


epidemic_datasets_combine <- function(ITZ, country,age_groups, pand_dt, seasonal_only, coverage_scenario){
  
  c_name <- c("Africa", "Asia-Europe", "EasternandSouthernAsia",
              "Europe", "NorthernAmerica", "Oceania-Melanesia-Polynesia",
              "SouthernAmerica")[ITZ]
  
  
  final_dataset1 <- read_parquet(here::here('Run_script', paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(c_name, country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt, seasonal_only)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Run_script', paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(c_name, country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt, seasonal_only)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Run_script', paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(c_name, country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt, seasonal_only)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Run_script', paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(c_name, country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt, seasonal_only)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Run_script', paste0('ITZzone', ITZ,'_', coverage_scenario), paste0(c_name, country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt, seasonal_only)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}


#### Reducing dataset #####
reducing_dataset <- function(final_dataset, pand_dt, seasonal_only){
  #function to reduce down the times and overview by year
  
  pandemic_timings <- pand_dt$epid_start_date
  
  final_dataset$pandemic_date <- as.Date(sapply(final_dataset$simulation_index, function(i)  pandemic_timings[[i]]))
  
  final_dataset <- final_dataset %>% mutate(date_late = pandemic_date %m+% months(6),
                                            date_early = pandemic_date %m-% months(2))
  
  restricted_dataset <- final_dataset[final_dataset$time < final_dataset$date_late,]
  if (seasonal_only == FALSE){
    restricted_dataset <- restricted_dataset[restricted_dataset$time_epidemic < restricted_dataset$date_early, ] 
  }
  
  restricted_dataset <- restricted_dataset %>% mutate(year = year(time))
  
  summary_dataset <- restricted_dataset %>%
    group_by(year, vacc_type, simulation_index) %>%
    summarise(I1 = sum(I1),
              I2 = sum(I2),
              I3=sum(I3),
              I4 = sum(I4),
              IU1 = sum(IU1),
              IU2 = sum(IU2),
              IU3 = sum(IU3),
              IU4 = sum(IU4),
              IV1 = sum(IV1),
              IV2 = sum(IV2),
              IV3 = sum(IV3),
              IV4 = sum(IV4),
              IVR1= sum(IVR1),
              IVR2 = sum(IVR2),
              IVR3 = sum(IVR3),
              IVR4 = sum(IVR4),
              tot=sum(tot),
              .groups='drop')
  
  return(summary_dataset)
}

### Running overall analysis

Analysis_file <- function(situation){
  age_testing_strategy <- situation %% 5 + 1
  interest_yr <- situation %% 3 + 1
  year_of_interest <- c(1918, 1957, 2009)[interest_yr]
  
  testing_output <- Pandemic_impact(ITZregion, country_of_interest, age_testing_strategy, year_of_interest, years, pand_dt,
                                    symp_samples, global_ihrs,
                                    national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                    cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                    cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                    doses_info, wastage, dose_price, case_proportion, LMICS_country, LMIC_boost, coverage_scenario)
  
  
  
  testing_output$ITZ <- ITZregion
  testing_output$country <- country_of_interest
  testing_output$age_testing_strategy <- age_testing_strategy
  testing_output$pandemic <- year_of_interest
  testing_output$time_of_pandemic <- years
  
  
  testing_output <- as.data.table(testing_output)

  
  return(testing_output)
  
}




