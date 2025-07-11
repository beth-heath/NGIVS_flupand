### File for pandemics running ### 

# setting seed 
set.seed(123)

#### load relevant packages ####
library(here)
source(here::here('setup','packages.R'))

#### colour schemes etc. ####
source(here::here('setup','aesthetics.R'))

#### Reading in the parameters ####

args <- commandArgs(trailingOnly = TRUE)
continent <- as.numeric(args[1])
pandemic_scenario <- as.numeric(args[2])

print(continent)

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
######### loading in functions needed ##########
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))

#loading in the pandemic addition function sets 
#source(here::here('functions/creating_pandemic_data.R'))


################################################
########### parameters that are set ############
################################################

vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?
cov_val <- 0.5 #what % coverage in each model age group?

vacc_type_list <- vacc_type_list_sterilising #setting the epidemic model used to have a sterilising mechanism of action



### Setting the ITZ used ###

c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[continent]
condensed_c_name <- c("Africa", "Asia-Europe", "EasternandSouthernAsia",
                      "Europe", "NorthernAmerica", "Oceania-Melanesia-Polynesia",
                      "SouthernAmerica")[continent]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[continent]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[continent]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])

### Selecting the pandemic used ###
#note we must select both for the pandemic and for the hemisphere it is occurring in


if (pandemic_scenario == 1 & hemisphere_input == 'NH'){
  load("1918_combined_set_NH.Rdata")
} else if (pandemic_scenario == 1 & hemisphere_input == 'SH'){
  load("1918_combined_set_SH.Rdata")
}else if (pandemic_scenario == 2 & hemisphere_input == 'NH'){
  load("1957_combined_set_NH.Rdata")
} else if (pandemic_scenario == 2 & hemisphere_input == 'SH'){
  load("1957_combined_set_SH.Rdata")
}else if (pandemic_scenario == 3 & hemisphere_input == 'NH'){
  load("2009_combined_set_NH.Rdata")
}else if (pandemic_scenario == 3 & hemisphere_input == 'SH'){
  load("2009_combined_set_SH.Rdata")
}



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
    
    #running for the first fifth of simulations
    simulation_nos_input <- 1:560
    # selecting the pandemic data for these
    epid_dt <- pandemic_combined %>% subset(simulation_index <561)
    #running the code for these
    infs_rds_list <- mclapply(1:16, flu_parallel_ITZ, mc.cores=16,mc.preschedule = FALSE)
    #reducing down into an arrow table
    overall_dt1 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    #removing the infs_rds_list file to save space and clearing garbage can

    
    rm(infs_rds_list)
    gc()
    #writing the arrow table with further compression to reduce the size of the files
    write_parquet(overall_dt1, sink = here::here('Run_script',paste0('ITZzone', continent), paste0(condensed_c_name, countries,age_groups,'pansn', pandemic_scenario,'_1.parquet')), compression = "zstd")
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
    write_parquet(overall_dt2, sink = here::here('Run_script',paste0('ITZzone', continent), paste0(condensed_c_name, countries,age_groups,'pansn', pandemic_scenario,'_2.parquet')), compression = "zstd")
    rm(overall_dt2)
    gc()
    
    #running for the third fifth of simulations
    simulation_nos_input <- 1121:1680
    epid_dt <- pandemic_combined %>% subset(simulation_index <1681 & simulation_index >1120)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt3 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt3, sink = here::here('Run_script',paste0('ITZzone', continent), paste0(condensed_c_name, countries,age_groups,'pansn', pandemic_scenario,'_3.parquet')), compression = "zstd")
    rm(overall_dt3)
    gc()
    
    #running for the fourth fifth of simulations
    simulation_nos_input <- 1681:2240
    epid_dt <- pandemic_combined %>% subset(simulation_index <2241 & simulation_index >1680)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt4 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt4, sink = here::here('Run_script',paste0('ITZzone', continent), paste0(condensed_c_name, countries,age_groups,'pansn', pandemic_scenario,'_4.parquet')), compression = "zstd")
    rm(overall_dt4)
    gc()
    
    #running for the final fifth of simulations
    simulation_nos_input <- 2241:2800
    epid_dt <- pandemic_combined %>% subset(simulation_index >2240)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_dt5 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt5, sink = here::here('Run_script',paste0('ITZzone', continent), paste0(condensed_c_name, countries,age_groups,'pansn', pandemic_scenario,'_5.parquet')), compression = "zstd")
    rm(overall_dt5)
    gc()
  }
}

