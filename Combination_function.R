
##### Creating function to run the code more easily #### 

#### Initial set-up #####

# setting seed for it all
set.seed(123)

#loading in packages
library(here)
source(here::here('setup','packages.R'))

#loading in colour schemes

# same as https://github.com/lucy-gf/flu_model_LG
source(here::here('setup','aesthetics.R'))


#### Setting parameter that will be unchanged between model runs ####

#adding in age-groups

model_age_groups <- c(0,5,18,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

#adding in the start dates of analysis and years of analysis 

start_year_of_analysis <- 2025
years_of_analysis <- 30

#number of simulations should be consistent

simulations <-100

#key dates for the model

ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

# setting vaccine to coverage
vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?

#### define coverage if using ####
if(vaccine_variable == 'coverage'){
  
  # define percentage coverage intended
  cov_val <- 0.5
  
  # define age groups targeted, e.g. here <10yos and 65+yos
  cov_ages <- c(0:10, 65:101)
  
  # what % coverage in each model age group?
  cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
  
}else{
  
  #### read in test doses ####
  doses <- data.table(read_csv(here::here('data','test_doses.csv'), show_col_types=F))
  
}

##### Loading in functions ####

# flu function
source(here::here('functions/fluparallelalteredITZ.R'))

#load in analysis files

#source(here::here('functions/Analysisfiles.R'))


#### Creating options to select for run ####

#whether to summarise by week or year

year_summary <- c('TRUE', 'FALSE')[2]

#whether the simulation has seasonal flu and the pandemic
seasonal_flu_included <- c('TRUE', 'FALSE')[1]
pandemic_flu_included <- c('TRUE', 'FALSE')[1]



#switch on whether to run for just GBR or all countries

example_country <- c('TRUE', 'FALSE')[1]

##### Creating function to run it all ####

#switches that can be incorporated to run over

disease_scenarios <- c('1918', '1957', '2009')[1]
vaccine_strategy_seasonal <- c('sterilising', 'disease mod', 'infection period')[1]
vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[1]


overall_run_function <- function(example_country, c_number, pandemic_year_chosen, disease_scenarios, vaccine_strategy_seasonal, vaccine_strategy_pandemics){
  if (disease_scenarios == '1918'){
    susceptibility_range <- c(0.80, 0.9)
    trans_range <- c(0.07249, 0.09834)
    sus_boost_for_children <- c(0.8,0.9)
    r0 <- NA
  } else if (disease_scenarios == '1957'){
    susceptibility_range <- c(0.50, 0.7)
    trans_range <- c(0.07249, 0.09834)
    sus_boost_for_children <- c(0.6,0.8)
    r0 <- NA
  } else if (disease_scenarios == '2009'){
    susceptibility_range <- c(0.50, 0.7)
    trans_range <- c(0.07249, 0.09834)
    sus_boost_for_children <- c(0.8,0.95)
    r0 <- NA
  }
  
  if (vaccine_strategy_seasonal == 'sterilising'){
    vacc_type_list <- vacc_type_list_sterilising
  } else if (vaccine_strategy_seasonal == 'disease mod'){
    vacc_type_list <- vacc_type_list_dis_mod
  } else if (vaccine_strategy_seasonal == 'infection period'){
    vacc_type_list <- vacc_type_list_reduced_infec
  }
  
  if (vaccine_strategy_pandemics == 'sterilising'){
    vacc_type_list_pand <- vacc_type_list_sterilising
  } else if (vaccine_strategy_pandemics == 'disease mod'){
    vacc_type_list_pand <- vacc_type_list_dis_mod
  } else if (vaccine_strategy_pandemics == 'infection period'){
    vacc_type_list_pand <- vacc_type_list_reduced_infec
  }
  
  
  if (example_country == TRUE){
    c_number <- 4
    c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
                "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
                "Southern America")[c_number]
    itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[c_number]
    hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[c_number]
    ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
    
    
    ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
    ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
    ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
    vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])
    
    if (seasonal_flu_included == 'TRUE'){
      epidemic_data <- converting_epidemic_code(itz_input,years_of_analysis,simulations, ageing_date)
      
      epid_dt <- epidemic_data
    
    } else{
      titles <- c('simulation_index', 'susceptibility', 'transmissibility', 'r0_to_scale', 
                  'match', 'start_date_late','original_date', 'ageing_year_start',
                  'epid_start_date', 'initial_infected', 'period_start_date', 'end_date', 'susceptibility_for_kids')
      epid_dt <- data.frame(matrix(nrow=0, ncol=length(titles)))
      colnames(epid_dt) <- titles
    }
    
    epid_dt <<- Pandemic_addition_function(epid_dt, simulations, pandemic_year_chosen, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                          start_year_of_analysis, years_of_analysis)

    if (pandemic_flu_included == 'FALSE'){
      epid_dt<- epid_dt %>% drop_na(original_date)
    }
    
    iso3c_input <- 'GBR'
    #infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=1)
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    infs_dt <- rbindlist(infs_rds_list)
    infs_dt$tot <- rowSums(infs_dt[,2:5])
    if (year_summary == 'TRUE'){
      infs_dt$year <-lubridate::year(infs_dt$time)
      inf_summary <- infs_dt %>%
        group_by(year, simulation_index, vacc_type) %>%
        summarise(tot_sum = sum(tot,na.rm=TRUE), .groups = 'drop')
    }
    return(infs_dt)
    
  } else{
    c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
                "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
                "Southern America")[c_number]
    itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[c_number]
    hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[c_number]
    ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
    country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
    
    
    ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
    ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
    ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
    vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])
    
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
    epid_dt <- Pandemic_addition_function(epid_dt, simulations, pandemic_year_chosen, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                          start_year_of_analysis, years_of_analysis)
    if (pandemic_flu_included == 'FALSE'){
      epid_dt<- epid_dt %>% drop_na(original_date)
    }
  }
  
  for (countries in 1:length(country_codes) ){
    iso3c_input <<- country_codes[countries]
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    results_list <- paste0('Results for', iso3c_input)
    assign(results_list, infs_rds_list)
    ### save outputs ####
    infs_dt <- rbindlist(infs_rds_list)
    saveRDS(infs_dt, file = here::here('outputs','data','epi', paste0(iso3c_input, 'results','.rds')) )
  }
  
  
}


overall_run_function(example_country, 4, 15, disease_scenarios, vaccine_strategy_seasonal, vaccine_strategy_pandemics)



test1 <- overall_run_function(example_country, 4, 15, disease_scenarios, vaccine_strategy_seasonal, vaccine_strategy_pandemics)


















