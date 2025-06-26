
combined_datasets <- function(continent, country, age_groups, year_of_interest, pandemic_year, pand_dt ){
  
  if (continent == 1){
    pandemic_data <- pandemic_datasets_1(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_1(country,age_groups, pandemic_selected)
  } else if (continent == 2){
    pandemic_data <- pandemic_datasets_2(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_2(country,age_groups, pandemic_selected)
  }else if (continent == 3){
    pandemic_data <- pandemic_datasets_3(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_3(country,age_groups, pandemic_selected)
  }else if (continent == 4){
    pandemic_data <- pandemic_datasets_4(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_4(country,age_groups, pandemic_selected)
  }else if (continent == 5){
    pandemic_data <- pandemic_datasets_5(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_5(country,age_groups, pandemic_selected)
  }else if (continent == 6){
    pandemic_data <- pandemic_datasets_6(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_6(country,age_groups, pandemic_selected)
  }else if (continent == 7){
    pandemic_data <- pandemic_datasets_7(country, age_groups, year_of_interest, pandemic_year)
    pandemic_selected <- pand_dt[pand_dt$year_pandemic == pandemic_year, ]
    epidemic_data <- epidemic_datasets_combine_7(country,age_groups, pandemic_selected)
  }
  
  
  combined_datasets <- rbind(pandemic_data, epidemic_data)
  return(combined_datasets)
  
}



#### Africa datasets #####

pandemic_datasets_1 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    ##selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    ##selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year < 12 & pandemic_year >6){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if  (pandemic_year <17 & 12 <pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_1 <- function(country,age_groups, pand_dt){

  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()

  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}

#### Asia-Europe datasets ####

pandemic_datasets_2 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_2 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone2', paste0('Asia-Europe', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}


#### EasternandSouthernAsia datasets ####

pandemic_datasets_3 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_3 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone3', paste0('EasternandSouthernAsia', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}

#### Europe datasets ####


pandemic_datasets_4 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_4 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone4', paste0('Europe', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}

#### NorthernAmerica datasets ####

pandemic_datasets_5 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_5 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone5', paste0('NorthernAmerica', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}


#### Oceania-Melanesia-Polynesia datasets ####
pandemic_datasets_6 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_6 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone6', paste0('Oceania-Melanesia-Polynesia', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}

#### SouthernAmerica datasets ####

pandemic_datasets_7 <- function(country, age_groups, year_of_interest, pandemic_year){
  
  if (pandemic_year <= 5){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_1', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  } else if (pandemic_year == 6) {
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_1', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    selected_year <- rbind(select_year1, select_year2)
  } else if (pandemic_year<12 & 6<pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
  } else if (pandemic_year == 12){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_2', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if ( pandemic_year <17 & 12 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 17){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_3', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (pandemic_year <23 & 17 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }else if (pandemic_year == 23){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_4', '.parquet')))
    select_year1 <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100]
    #selected_year1$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_5', '.parquet')))
    select_year2 <- pandemic_dataset[pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year2$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
    selected_year <- rbind(select_year1, select_year2)
  }else if (23 < pandemic_year){
    pandemic_dataset <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,year_of_interest,'_5', '.parquet')))
    selected_year <- pandemic_dataset[pandemic_dataset$simulation_index > (pandemic_year-1)*100 & pandemic_dataset$simulation_index < (100*pandemic_year)+1]
    #selected_year$simulation_index <- seq(1,100,1)
    rm(pandemic_dataset)
    gc()
  }
  
  selected_year <- selected_year %>% mutate(date_late = time_epidemic %m+% months(6) )
  
  restricted_dataset <- selected_year[selected_year$time < selected_year$date_late,]
  
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


epidemic_datasets_combine_7 <- function(country,age_groups, pand_dt){
  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,'Epidemic_','1', '.parquet')))
  reduced_1 <- reducing_dataset(final_dataset1, pand_dt)
  rm(final_dataset1)
  gc()
  final_dataset2 <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,'Epidemic_','2', '.parquet')))
  reduced_2 <- reducing_dataset(final_dataset2, pand_dt)
  rm(final_dataset2)
  gc()
  final_dataset3 <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,'Epidemic_','3', '.parquet')))
  reduced_3 <- reducing_dataset(final_dataset3, pand_dt)
  rm(final_dataset3)
  gc()
  
  final_dataset4 <- read_parquet(here::here('Reduced_run', 'ITZzone7', paste0('SouthernAmerica', country, age_groups,'Epidemic_','4', '.parquet')))
  reduced_4 <- reducing_dataset(final_dataset4, pand_dt)
  rm(final_dataset4)
  gc()
  
  final_dataset5 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','5', '.parquet')))
  reduced_5 <- reducing_dataset(final_dataset5, pand_dt)
  rm(final_dataset5)
  gc()
  
  combined_overview <- rbind(reduced_1, reduced_2, reduced_3, reduced_4, reduced_5)
}

#### Reducing dataset #####
reducing_dataset <- function(final_dataset, pand_dt){
  #function to reduce down the times and overview by year
  
  pandemic_timings <- pand_dt$epid_start_date
  
  final_dataset$pandemic_date <- as.Date(sapply(final_dataset$simulation_index, function(i)  pandemic_timings[[i]]))
  
  final_dataset <- final_dataset %>% mutate(date_late = pandemic_date %m+% months(6),
                                            date_early = pandemic_date %m-% months(2))
  
  restricted_dataset <- final_dataset[final_dataset$time < final_dataset$date_late,]
  restricted_dataset <- restricted_dataset[restricted_dataset$time_epidemic < restricted_dataset$date_early, ]
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
























#epidemic data
analysing_data_set <- function(country, age_groups, pand_dt){
  
  test1 <- pand_dt[1:100,]$epid_start_date

  
  final_dataset1 <- read_parquet(here::here('Reduced_run', 'ITZzone1', paste0('Africa', country, age_groups,'Epidemic_','1', '.parquet')))
  
  final_dataset1$sim_value <- sapply(df$simulation_index, function(i) my_list[[i]])
  
  restricted_dataset1 <- final_dataset1[final_dataset1$time < pandemic_date %m+% months(6)]
  restricted_dataset1 <- restricted_dataset1[restricted_dataset1$time_epidemic < pandemic_date %m-% months(2)]
  
  
  
}






reconstructing_dataset <- function(final_dataset1,final_dataset2, final_dataset3, pandemic_date ){

  restricted_dataset1 <- final_dataset1[final_dataset1$time < pandemic_date %m+% months(6)]
  restricted_dataset1 <- restricted_dataset1[restricted_dataset1$time_epidemic < pandemic_date %m-% months(2)]
  
  restricted_dataset2 <- final_dataset2[final_dataset2$time < pandemic_date %m+% months(6)]
  restricted_dataset2 <- restricted_dataset1[restricted_dataset2$time_epidemic < pandemic_date %m-% months(2)]
  
  restricted_dataset3 <- final_dataset3[final_dataset3$time < pandemic_date %m+% months(6)]
  restricted_dataset3 <- restricted_dataset1[restricted_dataset3$time_epidemic < pandemic_date %m-% months(2)]
  
  summary_dataset1 <- restricted_dataset1 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  summary_dataset2 <- restricted_dataset2 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  summary_dataset3 <- restricted_dataset3 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  combined_dataset <- rbind(summary_dataset1, summary_dataset2, summary_dataset3)
  
  combined_dataset <- combined_dataset %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  
  return(combined_dataset)
  
  
  
}












