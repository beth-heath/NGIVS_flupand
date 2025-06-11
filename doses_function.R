#### Reading in parameters ####

pandemic <- pandemic_example[pandemic_example$simulation_index==1,]
vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?
cov_val <- 0.5
key_dates <- c('01-04', '01-10')
country_codes <- country_itzs_names$codes
### Adding in flu dose calculator ####

flu_dose_calculator <- function(
    country,
    ageing,
    ageing_date,
    pandemic,
    vaccine_type,
    vaccine_variable,
    vacc_type_list,
    model_age_groups,
    cov_vec
){
  dates_many_flu <- seq.Date(last_monday(pandemic$period_start_date), 
                             last_monday(pandemic$end_date ), 
                             by=7)
  
  vacc_name <- names(vacc_type_list)[vaccine_type]
  
  vaccine_used_vec <- if(vaccine_variable == 'doses'){
    # if using doses, NGIVs are often introduced years after the start of the epidemic period
    doses[vacc_scenario == vacc_name & model_age_group==1]$vacc_used
  }else{
    # if using coverage, assuming NGIVs available each year (adjust here if not!)
    rep(vacc_name, year(pandemic$end_date) - year(pandemic$period_start_date))
  }
  
  doses <- fcn_annual_doses(
    country,
    ageing,
    ageing_date,
    dates_in = dates_many_flu,
    demographic_start_year = year(min(pandemic$period_start_date)),
    vaccine_used = vaccine_used_vec,
    vaccine_var = vaccine_variable,
    doses_dt = if(vaccine_variable == 'doses'){doses}else{NULL},
    vacc_cov_vec = if(vaccine_variable == 'coverage'){cov_vec}else{NULL},
    init_vaccinated = c(0,0,0,0),
    model_age_groups = model_age_groups
  )
  
  return(doses)
  
}

### flu doses parallal code ##### 

flu_doses_parallal_1 <- function(scn){
  if (scn == 1){
    
    vaccine_type <- 1
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==2) { 
    
    vaccine_type <- 1
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==3) { 
    
    vaccine_type <- 1
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==4) { 
    
    vaccine_type <- 1
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==5) { 
    
    vaccine_type <- 1
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn == 6){
    
    vaccine_type <- 2
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==7) { 
    
    vaccine_type <- 2
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==8) { 
    
    vaccine_type <- 2
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==9) { 
    
    vaccine_type <- 2
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==10) { 
    
    vaccine_type <- 2
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } 
}




flu_doses_parallal_2 <- function(scn){
  if (scn == 1){
    
    vaccine_type <- 3
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==2) { 
    
    vaccine_type <- 3
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==3) { 
    
    vaccine_type <- 3
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==4) { 
    
    vaccine_type <- 3
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==5) { 
    
    vaccine_type <- 3
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn == 6){
    
    vaccine_type <- 4
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==7) { 
    
    vaccine_type <- 4
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==8) { 
    
    vaccine_type <- 4
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==9) { 
    
    vaccine_type <- 4
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==10) { 
    
    vaccine_type <- 4
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } 
}

flu_doses_parallal_3 <- function(scn){
  if (scn == 1){
    
    vaccine_type <- 5
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==2) { 
    
    vaccine_type <- 5
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==3) { 
    
    vaccine_type <- 5
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==4) { 
    
    vaccine_type <- 5
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==5) { 
    
    vaccine_type <- 5
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn == 6){
    
    vaccine_type <- 6
    cov_ages <- c(0:17, 65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17 & 65-101', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==7) { 
    
    vaccine_type <- 6
    cov_ages <- c(0:4)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-4', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==8) { 
    
    vaccine_type <- 6
    cov_ages <- c(0:10)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-10', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==9) { 
    
    vaccine_type <- 6
    cov_ages <- c(0:17)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('0-17', nrow(doses_calculated))
    doses_calculated
  } else if (scn ==10) { 
    
    vaccine_type <- 6
    cov_ages <- c(65:101)
    cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
    doses_calculated <- flu_dose_calculator(country,
                                            ageing,
                                            ageing_date,
                                            pandemic,
                                            vaccine_type,
                                            vaccine_variable,
                                            vacc_type_list,
                                            model_age_groups,
                                            cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } 
}

doses_table <- c() 

for (country_no in 1:length(country_codes)){
  
  country = country_codes[country_no]
  
  continent_interest <- country_itzs_names[country_no]$cluster_name
  
  ageing_date =  ifelse(continent_interest=="Oceania-Melanesia-Polynesia"|continent_interest=="Southern America", key_dates[2], key_dates[1])
  vacc_calendar_start <- ifelse(continent_interest=="Oceania-Melanesia-Polynesia"|continent_interest=="Southern America", key_dates[1], key_dates[2])
  
  #first_doses <- mclapply(1:10, flu_doses_parallal_1, mc.cores=1)
  
  first_doses <- mclapply(1:10, flu_doses_parallal_1, mc.cores=10)
  second_doses <- mclapply(1:10, flu_doses_parallal_2, mc.cores=10)
  third_doses <- mclapply(1:10, flu_doses_parallal_3, mc.cores=10)
  
  combined_doses <- rbind(rbindlist(first_doses), rbindlist(second_doses), rbindlist(third_doses))
  
  if (is.null(doses_table)){
    doses_table <- combined_doses
  } else{
    doses_table <- rbind(doses_table, combined_doses)
  }
  
  print(country_no)
  
  
}


saveRDS(doses_table, file = here::here(paste0('dose_table.rds')))

doses_table<- readRDS('dose_table.rds')

#### adding in code to analyse the files


function_testing <- function(dose_table, epid_time, year_of_interest){
  epid_time <- epid_time %m+% months(6+12*year_of_interest)
  restricting_to_time <- dose_table[week < epid_time]
  restricting_to_time <- restricting_to_time %>% mutate(year = year(week))
  output_v <- restricting_to_time %>% select(year, vaccs, vacc_type, vacc_population) %>% 
    group_by(year, vacc_type, vacc_population) %>% summarise(vaccs = max(vaccs))
  
  return(output_v)
}



reworked_doses <- purrr::map_dfr(country_codes, function(country_code) {
  param_grid_country <- expand.grid(
    simulation_index = 1:100,
    year_pandemic = 1:30,
    stringsAsFactors = FALSE
  ) %>%
    mutate(country = country_code)
  
  dose_table_sub <- doses_table %>% filter(country == country_code)
  print(country_code)
  
  # Add epidemic time for each simulation index
  param_grid_country <- param_grid_country %>%
    mutate(
      #adding in simulation index into the map index
      epid_time = map(simulation_index, ~ {
        pandemic_example %>%
          #filtering for the simulation index put in
          filter(simulation_index == .x) %>%
          #taking the epid_start_date from function
          pull(epid_start_date)
      })
    )
  
  # Apply function_testing to the fed in parameters
  param_grid_country %>%
    mutate(
      doses_tab = pmap(list(simulation_index, epid_time, year_pandemic), function(i, ep, yp) {
        function_testing(dose_table_sub, ep, yp)
      })
    ) %>%
    select(country, simulation_index, year_pandemic, doses_tab) %>%
    unnest(doses_tab)
})

saveRDS(reworked_doses, file = here::here(paste0('dose_table_reworked.rds')))


### repeating by country ####

for (country_code in country_codes){
  param_grid_country <- expand.grid(
    simulation_index = 1:100,
    year_pandemic = 1:30,
    stringsAsFactors = FALSE
  ) %>%
    mutate(country = country_code)
  
  dose_table_sub <- doses_table %>% filter(country == country_code)
  print(country_code)
  
  # Add epidemic time for each simulation index
  param_grid_country <- param_grid_country %>%
    mutate(
      #adding in simulation index into the map index
      epid_time = map(simulation_index, ~ {
        pandemic_example %>%
          #filtering for the simulation index put in
          filter(simulation_index == .x) %>%
          #taking the epid_start_date from function
          pull(epid_start_date)
      })
    )
  
  # Apply function_testing to the fed in parameters
  doses_analysed <- param_grid_country %>%
    mutate(
      doses_tab = pmap(list(simulation_index, epid_time, year_pandemic), function(i, ep, yp) {
        function_testing(dose_table_sub, ep, yp)
      })
    ) %>%
    select(country, simulation_index, year_pandemic, doses_tab) %>%
    unnest(doses_tab)
  
  saveRDS(doses_analysed, file = here::here(paste0('Rearranged_dose_for',country,'.rds')))
  
}




