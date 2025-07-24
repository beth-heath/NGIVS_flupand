### File for ITZzone3 (EasternandSouthernAsia) for seasonal epidemics ####

# setting seed 
set.seed(123)

#### load relevant packages ####
#library(here)
source(here::here('setup','packages.R'))

#### colour schemes etc. ####
#source(here::here('setup','aesthetics.R'))

################################################
############## set key parameters ##############
################################################

# these will be kept constant throughout the simulations

model_age_groups <- c(0,5,18,65) #where the age cutoffs are
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99)) #names of the age-groups
start_year_of_analysis <- 2025 #the age the analysis starts
years_of_analysis <- 30 #studying for 30 years in keeping in Goodfellow et al paper
simulations <-100 #the number of simulations
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program


################################################
################################################
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))



################################################
########### parameters that are set ############
################################################

vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?
cov_val <- 0.7 #what % coverage in each model age group?

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

continent <- 3

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
    
    # Due to size of the files these have been split into five different sets to run so that they can run on HPC memory
    
    #selecting the first set of simulations
    simulation_nos_input <- 1:20
    #loading and converting the epidemic file
    epid_dt <- converting_epidemic_code(itz_input,years_of_analysis,1:20, ageing_date)
    #running the code for these simulations
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    
    #reducing down into an arrow table
    overall_dt1 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    #removing the infs_rds_list file to save space and clearing garbage can
    rm(infs_rds_list)
    gc()
    #writing the arrow table with further compression to reduce the size of the files
    write_parquet(overall_dt1, sink = here::here('Run_script','ITZzone3(70)', paste0('EasternandSouthernAsia',countries,age_groups,'Epidemic_1.parquet')), compression = "zstd")
    rm(overall_dt1)
    gc()
    
    
    
    #selecting the second set of simulations
    simulation_nos_input <- 21:40
    epid_dt <- converting_epidemic_code(itz_input,years_of_analysis,21:40, ageing_date)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    overall_dt2 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt2, sink = here::here('Run_script','ITZzone3(70)', paste0('EasternandSouthernAsia',countries,age_groups,'Epidemic_2.parquet')), compression = "zstd")
    rm(overall_dt2)
    gc()
    
    #selecting the third set of simulations
    simulation_nos_input <- 41:60
    epid_dt <- converting_epidemic_code(itz_input,years_of_analysis,41:60, ageing_date)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    overall_dt3 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt3, sink = here::here('Run_script','ITZzone3(70)', paste0('EasternandSouthernAsia',countries,age_groups,'Epidemic_3.parquet')), compression = "zstd")
    rm(overall_dt3)
    gc()
    
    #selecting the fourth set of simulations
    simulation_nos_input <- 61:80
    epid_dt <- converting_epidemic_code(itz_input,years_of_analysis,61:80, ageing_date)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    overall_dt4 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt4, sink = here::here('Run_script','ITZzone3(70)', paste0('EasternandSouthernAsia',countries,age_groups,'Epidemic_4.parquet')), compression = "zstd")
    rm(overall_dt4)
    gc()
    
    #selecting the final set of simulations
    simulation_nos_input <- 81:100
    epid_dt <- converting_epidemic_code(itz_input,years_of_analysis,81:100, ageing_date)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    overall_dt5 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt5, sink = here::here('Run_script','ITZzone3(70)', paste0('EasternandSouthernAsia',countries,age_groups,'Epidemic_5.parquet')), compression = "zstd")
    rm(overall_dt5)
    gc()
    
  }
}
