set.seed(123)

#### load relevant packages ####
library(here)
source(here::here('setup','packages.R'))

#### colour schemes etc. ####
# same as https://github.com/lucy-gf/flu_model_LG
source(here::here('setup','aesthetics.R'))

################################################
############## set key parameters ##############
################################################

model_age_groups <- c(0,5,18,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

### adding here the randomness of the dates

start_year_of_analysis <- 2025
years_of_analysis <- 30

simulations <-100

ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

################################################
################################################
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))

#loading in the pandemic addition function sets 
source(here::here('functions/creating_pandemic_data.R'))



vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?

cov_val <- 0.5

# define age groups targeted, e.g. here <10yos and 65+yos
#cov_ages <- c(0:10, 65:101)

# what % coverage in each model age group?
vacc_type_list <- vacc_type_list_sterilising

vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[3]

if (vaccine_strategy_pandemics == 'sterilising'){
  vacc_type_list_pand <- vacc_type_list_sterilising
} else if (vaccine_strategy_pandemics == 'disease mod'){
  vacc_type_list_pand <- vacc_type_list_dis_mod
} else if (vaccine_strategy_pandemics == 'infection period'){
  vacc_type_list_pand <- vacc_type_list_reduced_infec
}

source(here::here('functions/fluparallelalteredITZ.R'))




continent <- 5

c_name <- c("Africa", "Asia-Europe", "EasternandSouthern Asia",
            "Europe", "NorthernAmerica", "Oceania-Melanesia-Polynesia",
            "SouthernAmerica")[continent]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[continent]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[continent]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 

ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])

load("1918_combined_set.Rdata")
pand_dt <- pandemic_combined


for (age_groups in 1:5){
  for (countries in country_codes){
    
    if (age_groups == 1){
      cov_ages <- c(0:4)
      cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    } else if (age_groups ==2){
      cov_ages <- c(0:10)
      cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    } else if (age_groups ==3){
      cov_ages <- c(0:17)
      cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    } else if (age_groups ==4){
      cov_ages <- c(65:101)
      cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    } else if (age_groups ==5){
      cov_ages <- c(0:17, 65:101)
      cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    } 
    
    iso3c_input <- countries
    print(countries)
    
    simulation_nos_input <- 1:560
    epid_dt <- pand_dt %>% subset(simulation_index <561)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    
    overall_dt1 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    
    rm(infs_rds_list)
    gc()
    
    #saveRDS(overall_dt1, file = here::here('outputs(0-33)',paste0(c_name, 'Epidemic_overall',countries,age_groups,'.rds')))
    write_parquet(overall_dt1, sink = here::here('Reduced_run','ITZzone6', paste0('Oceania-Melanesia-Polynesia',countries,age_groups,'1918_1.parquet')), compression = "zstd")
    rm(overall_dt1)
    gc()
    
    #overall_file2 <- list(infs_rds_list[[1]][[4]], infs_rds_list[[2]][[4]], infs_rds_list[[3]][[4]], infs_rds_list[[4]][[4]], infs_rds_list[[5]][[4]], infs_rds_list[[6]][[4]])
    #overall_file3 <- list(infs_rds_list[[1]][[5]], infs_rds_list[[2]][[5]], infs_rds_list[[3]][[5]], infs_rds_list[[4]][[5]], infs_rds_list[[5]][[5]], infs_rds_list[[6]][[5]])
    
    simulation_nos_input <- 561:1120
    epid_dt <- pand_dt %>% subset(simulation_index <1121 & simulation_index >560)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt2 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    
    rm(infs_rds_list)
    gc()
    
    write_parquet(overall_dt2, sink = here::here('Reduced_run','ITZzone5', paste0('Northern America',countries,age_groups,'1918_2.parquet')), compression = "zstd")
    rm(overall_dt2)
    gc()
    
    simulation_nos_input <- 1121:1680
    epid_dt <- pand_dt %>% subset(simulation_index <1681 & simulation_index >1120)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    
    #run each of these save the summary file then remove with rm
    overall_dt3 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    
    rm(infs_rds_list)
    gc()
    
    write_parquet(overall_dt3, sink = here::here('Reduced_run','ITZzone5', paste0('Northern America',countries,age_groups,'1918_3.parquet')), compression = "zstd")
    rm(overall_dt3)
    gc()
    
    simulation_nos_input <- 1681:2240
    epid_dt <- pand_dt %>% subset(simulation_index <2241 & simulation_index >1680)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    
    #run each of these save the summary file then remove with rm
    overall_dt4 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    
    rm(infs_rds_list)
    gc()
    
    
    write_parquet(overall_dt4, sink = here::here('Reduced_run','ITZzone5', paste0('Northern America',countries,age_groups,'1918_4.parquet')), compression = "zstd")
    
    rm(overall_dt4)
    gc()
    
    simulation_nos_input <- 2241:2800
    epid_dt <- pand_dt %>% subset(simulation_index >2240)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    
    #run each of these save the summary file then remove with rm
    overall_dt5 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    
    rm(infs_rds_list)
    gc()
    
    
    write_parquet(overall_dt5, sink = here::here('Reduced_run','ITZzone5', paste0('Northern America',countries,age_groups,'1918_5.parquet')), compression = "zstd")
    
    rm(overall_dt5)
    gc()
    
    
  }
}




