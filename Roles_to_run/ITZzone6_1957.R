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

vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[1]

if (vaccine_strategy_pandemics == 'sterilising'){
  vacc_type_list_pand <- vacc_type_list_sterilising
} else if (vaccine_strategy_pandemics == 'disease mod'){
  vacc_type_list_pand <- vacc_type_list_dis_mod
} else if (vaccine_strategy_pandemics == 'infection period'){
  vacc_type_list_pand <- vacc_type_list_reduced_infec
}

source(here::here('functions/fluparallelalteredITZ.R'))




continent <- 6

c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[continent]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[continent]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[continent]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 

ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])

load("1957_combined_set.Rdata")
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
    
    simulation_nos_input <- 1:990
    epid_dt <- pand_dt %>% subset(simulation_index <991)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    
    
    overall_file1 <- list(infs_rds_list[[1]][[3]], infs_rds_list[[2]][[3]], infs_rds_list[[3]][[3]], infs_rds_list[[4]][[3]], infs_rds_list[[5]][[3]], infs_rds_list[[6]][[3]])
    overall_dt1 <- rbindlist(overall_file1)
    overall_dt1 <- arrow_table(overall_dt1)
    
    
    #saveRDS(overall_dt1, file = here::here('outputs(0-33)',paste0(c_name, 'Epidemic_overall',countries,age_groups,'.rds')))
    saveRDS(overall_dt1, file = here::here(paste0('Africa1',countries,age_groups,'.rds')))
    rm(overall_file1 )
    rm(overall_dt1)
    
    #overall_file2 <- list(infs_rds_list[[1]][[4]], infs_rds_list[[2]][[4]], infs_rds_list[[3]][[4]], infs_rds_list[[4]][[4]], infs_rds_list[[5]][[4]], infs_rds_list[[6]][[4]])
    #overall_file3 <- list(infs_rds_list[[1]][[5]], infs_rds_list[[2]][[5]], infs_rds_list[[3]][[5]], infs_rds_list[[4]][[5]], infs_rds_list[[5]][[5]], infs_rds_list[[6]][[5]])
    
    simulation_nos_input <- 991:1980
    epid_dt <- pand_dt %>% subset(simulation_index <1981 & simulation_index >990)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_file2 <- list(infs_rds_list[[1]][[3]], infs_rds_list[[2]][[3]], infs_rds_list[[3]][[3]], infs_rds_list[[4]][[3]], infs_rds_list[[5]][[3]], infs_rds_list[[6]][[3]])
    overall_dt2 <- rbindlist(overall_file2)
    overall_dt2 <- arrow_table(overall_dt2)
    saveRDS(overall_dt2, file = here::here(paste0('Africa2',countries,age_groups,'.rds')))
    rm(overall_file2 )
    rm(overall_dt2)
    
    simulation_nos_input <- 1981:2800
    epid_dt <- pand_dt %>% subset(simulation_index >1980)
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    overall_file3 <- list(infs_rds_list[[1]][[3]], infs_rds_list[[2]][[3]], infs_rds_list[[3]][[3]], infs_rds_list[[4]][[3]], infs_rds_list[[5]][[3]], infs_rds_list[[6]][[3]])
    #run each of these save the summary file then remove with rm
    overall_dt3 <- rbindlist(overall_file3)
    overall_dt3 <- arrow_table(overall_dt3)
    saveRDS(overall_dt3, file = here::here(paste0('Africa3',countries,age_groups,'.rds')))
    rm(overall_file3 )
    rm(overall_dt3)
    
  }
}




