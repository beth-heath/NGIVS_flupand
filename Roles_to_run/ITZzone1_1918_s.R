### File for ITZzone1 (Africa) for a 1918-like pandemic with sterilising mechanism of action####

# setting seed 
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
# these will be kept constant throughout the simulations

model_age_groups <- c(0,5,18,65) #where the age cutoffs are
start_year_of_analysis <- 2025 #the age the analysis starts
years_of_analysis <- 30 #studying for 30 years in keeping in Goodfellow et al paper
simulations <-100 #the number of simulations
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

################################################
######### loading in functions needed ##########
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))

#loading in the pandemic addition function sets 
source(here::here('functions/creating_pandemic_data.R'))


################################################
########### parameters that are set ############
################################################

vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?
cov_val <- 0.5 #what % coverage in each model age group?

vacc_type_list <- vacc_type_list_sterilising #setting the epidemic model used to have a sterilising mechanism of action

#setting the vaccine strategy used in this work
vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[1]

if (vaccine_strategy_pandemics == 'sterilising'){
  vacc_type_list_pand <- vacc_type_list_sterilising
} else if (vaccine_strategy_pandemics == 'disease mod'){
  vacc_type_list_pand <- vacc_type_list_dis_mod
} else if (vaccine_strategy_pandemics == 'infection period'){
  vacc_type_list_pand <- vacc_type_list_reduced_infec
}

### Setting the ITZ used ###

continent <- 1

c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[continent]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[continent]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[continent]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])

### Selecting the pandemic used ###
#note we must select both for the pandemic and for the hemisphere it is occuring in
load("1918_combined_set_NH.Rdata")

### Setting up the simulation for all different age-groups tested and for all countries in the ITZ

for (age_groups in 1:5){
  for (countries in country_codes){
    #which age-testing policy selected determines which individuals are eligible for vaccination
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
    
    # De to size of the files these have been split into five different sets to run so that they can run on HPC memory
    
    #running for the first fifth of simulations
    simulation_nos_input <- 1:560
    # selecting the pandemic data for these
    epid_dt <- pandemic_combined %>% subset(simulation_index <561)
    #running the code for these
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list),mc.preschedule = FALSE)
    #reducing down into an arrow table
    overall_dt1 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    #removing the infs_rds_list file to save space and clearing garbage can
    rm(infs_rds_list)
    gc()
    #writing the arrow table with further compression to reduce the size of the files
    write_parquet(overall_dt1, sink = here::here('Reduced_run','ITZzone1', paste0('Africa',countries,age_groups,'1918_1.parquet')), compression = "zstd")
    #remove all files from this segment to save space once again clearing the garbage can
    rm(overall_dt1)
    gc()
    
    #running for the second fifth of simulations
    simulation_nos_input <- 561:1120
    epid_dt <- pandemic_combined %>% subset(simulation_index <1121 & simulation_index >560)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt2 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt2, sink = here::here('Reduced_run','ITZzone1', paste0('Africa',countries,age_groups,'1918_2.parquet')), compression = "zstd")
    rm(overall_dt2)
    gc()
    
    #running for the third fifth of simulations
    simulation_nos_input <- 1121:1680
    epid_dt <- pandemic_combined %>% subset(simulation_index <1681 & simulation_index >1120)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt3 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt3, sink = here::here('Reduced_run','ITZzone1', paste0('Africa',countries,age_groups,'1918_3.parquet')), compression = "zstd")
    rm(overall_dt3)
    gc()
   
    #running for the fourth fifth of simulations
    simulation_nos_input <- 1681:2240
    epid_dt <- pandemic_combined %>% subset(simulation_index <2241 & simulation_index >1680)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt4 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt4, sink = here::here('Reduced_run','ITZzone1', paste0('Africa',countries,age_groups,'1918_4.parquet')), compression = "zstd")
    rm(overall_dt4)
    gc()
    
    #running for the final fifth of simulations
    simulation_nos_input <- 2241:2800
    epid_dt <- pandemic_combined %>% subset(simulation_index >2240)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt5 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt5, sink = here::here('Reduced_run','ITZzone1', paste0('Africa',countries,age_groups,'1918_5.parquet')), compression = "zstd")
    rm(overall_dt5)
    gc()
    
  }
}




