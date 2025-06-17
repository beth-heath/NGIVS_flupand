#### RUN THE FLU MODEL ####

# key outputs: vaccine_programs, vacc_type_list
source(here::here('functions','vacc_types.R'))
# will calculate weekly age- and vaccine-specific population, also loads transmission model
source(here::here('functions','demography.R'))
# runs the flu model
source(here::here('functions','flu_sim.R'))

#function to reduce down the size of the files

reduce_function <- function(dataset){
  dataset$tot <- rowSums(dataset[,2:5])
  dataset <- dataset[dataset$tot != 0, ]
  return(dataset)
  
}



#Functions to translate Lucy's previous files

Hemisphere_matching <- function(epi_initial_ds, hemisphere){
  if(hemisphere == 'NH'){
    vec <- epi_initial_ds$N_A_match
    vec[which(epi_initial_ds$strain=='INF_B')] <- epi_initial_ds$N_B_match[which(epi_initial_ds$strain=='INF_B')]
  } else{
    vec <- epi_initial_ds$S_A_match
    vec[which(epi_initial_ds$strain=='INF_B')] <- epi_initial_ds$S_B_match[which(epi_initial_ds$strain=='INF_B')]
  }
  return(vec)
}

converting_epidemic_code <- function(itz_input,years_of_analysis,simulation_set, ageing_date){
  sampled_epids <- data.table(read_csv(here::here('data','epi','sampled_epids',paste0('sampled_epidemics_30_100_',itz_input,'_wr0.csv')), show_col_types=F))
  epids <- sampled_epids[simulation_cal_year <= years_of_analysis & simulation_index %in% simulation_set]
  Converting_epidemics_dataset <- epids %>% select(simulation_index, sus, trans, contains('match'), strain, day, month, year, simulation_cal_year,
                                                   pushback, init_ageing_date, init_nye, r0)
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% rename(susceptibility=sus, transmissibility=trans, r0_to_scale=r0)
  Converting_epidemics_dataset$match <- Hemisphere_matching(Converting_epidemics_dataset, 'NH')
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% select(!c(strain,contains('_match')))
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(start_date_late = as.Date(paste0(as.numeric(day), '-', as.numeric(month), '-', 
                                                                                                           (start_year_of_analysis + simulation_cal_year - 1)), '%d-%m-%Y'),
                                                                          original_date = as.Date(paste0(as.numeric(day), '-', month, '-', year), '%d-%m-%Y') ) 
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(ageing_year_start = case_when(month(start_date_late) < ageing_month ~ start_year_of_analysis + simulation_cal_year - 2,
                                                                                                        (month(start_date_late) = ageing_month) & (day(start_date_late) < ageing_day) ~ start_year_of_analysis + simulation_cal_year - 2,
                                                                                                        T ~ start_year_of_analysis + simulation_cal_year - 1))
  
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(epid_start_date = case_when(!is.na(pushback) ~ start_date_late - pushback,
                                                                                                      is.na(pushback) ~ case_when(is.na(init_ageing_date) ~ as.Date(paste0('01-01-', start_year_of_analysis), format = '%d-%m-%Y'),
                                                                                                                                  !is.na(init_ageing_date) ~ as.Date(paste0(ageing_date,  '-', ageing_year_start), format = '%d-%m-%Y'))))
  
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(initial_infected = case_when(!is.na(pushback) ~ 10,
                                                                                                       is.na(pushback) ~ case_when(is.na(init_ageing_date) ~ init_nye,
                                                                                                                                   !is.na(init_ageing_date) ~ init_ageing_date)),
  )
  
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(epid_start_date = last_monday(epid_start_date))
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% select(!c(pushback,init_ageing_date,init_nye,day,month,year,simulation_cal_year))
  Converting_epidemics_dataset <- Converting_epidemics_dataset %>% mutate(period_start_date = as.Date(paste0('01-01-',start_year_of_analysis),format='%d-%m-%Y'), end_date = as.Date(paste0('01-01-',start_year_of_analysis + years_of_analysis),format='%d-%m-%Y'))
  
  return(Converting_epidemics_dataset)
}







#### FUNCTION TO RUN ####
## only input is vaccine type, to parallelise over vt ##
flu_parallel_ITZ <- function(vaccine_type){
  
  set.seed(123)
  
  # total_start_time <- Sys.time()
  
  dates_many_flu <- seq.Date(last_monday(min(epid_dt$period_start_date)), 
                             last_monday(max(epid_dt$end_date)), 
                             by=7)
  
  vacc_name <- names(vacc_type_list)[vaccine_type]
  
  vaccine_used_vec <- if(vaccine_variable == 'doses'){
    # if using doses, NGIVs are often introduced years after the start of the epidemic period
    doses[vacc_scenario == vacc_name & model_age_group==1]$vacc_used
  }else{
    # if using coverage, assuming NGIVs available each year (adjust here if not!)
    rep(vacc_name, (year(epid_dt$end_date[1]) - year(epid_dt$period_start_date[1])))
  }
  
  ## vaccination and ageing
  demography_dt <- fcn_weekly_demog(
    country = iso3c_input,
    ageing,
    ageing_date,
    dates_in = dates_many_flu,
    demographic_start_year = start_year_of_analysis,
    vaccine_used = vaccine_used_vec,
    vaccine_var = vaccine_variable,
    doses_dt = if(vaccine_variable == 'doses'){doses}else{NULL},
    vacc_cov_vec = if(vaccine_variable == 'coverage'){cov_vec}else{NULL},
    init_vaccinated = c(0,0,0,0),
    model_age_groups
  )
  
  # demography_dt %>% mutate(prop = value/total_as) %>% filter(V==T) %>% ggplot() + geom_line(aes(week, prop, col=age_grp))
  if(min(demography_dt$value) < 0){ # quick fix if any vaccination issues (there shouldn't be)
    print(paste0('Negative values in demography_dt, iso3c = ', iso3c_input,', vaccine type = ', vaccine_type))
  }
  
  mf_output <- data.table()
  pan_output <- data.table()
  combined_output <- data.table()
  combined_output2 <- data.table()
  combined_output3 <- data.table()
  
  
  # loop over 1:100 simulations
  #for(sim_index in unique(epid_dt$simulation_index)){
  for(sim_index in unique(simulation_nos_input )){
    start_time <- Sys.time()
    
    # run flu simulations
    mf_output_si <- many_flu(country = iso3c_input,
                             ageing, 
                             ageing_date,
                             epid_inputs = epid_dt[epid_dt$simulation_index == sim_index, ],  
                             vaccine_used = vaccine_used_vec,
                             vaccine_var = vaccine_variable,
                             doses_dt = if(vaccine_variable == 'doses'){doses}else{NULL},
                             vacc_cov_vec = if(vaccine_variable == 'coverage'){cov_vec}else{NULL},
                             model_age_groups,
                             demography_dt
    )
    
    pandemic_only <- mf_output_si[[2]]
    combined_trial <- mf_output_si[[3]]
    

    
    
    
    pandemic_only <- pandemic_only[year(time) >= start_year_of_analysis] # in case epidemic started pre-2025 
    pandemic_only[, vacc_type := names(vacc_type_list)[vaccine_type]] # add vaccine name
    pandemic_only[, simulation_index := sim_index] # add simulation number
    
    
    # printing if there is an NA error (shouldn't happen)
    if(is.na(sum(rowSums(pandemic_only %>% select(starts_with('I')))))){
      print(paste0('vt = ', vaccine_type, ', sim_index = ', sim_index, ' - is.na'))
    }
  
    
    combined_trial <- combined_trial[year(time) >= start_year_of_analysis] # in case epidemic started pre-2025 
    combined_trial[, vacc_type := names(vacc_type_list)[vaccine_type]] # add vaccine name
    combined_trial[, simulation_index := sim_index] # add simulation number
    
    
    # printing if there is an NA error (shouldn't happen)
    if(is.na(sum(rowSums(combined_trial %>% select(starts_with('I')))))){
      print(paste0('vt = ', vaccine_type, ', sim_index = ', sim_index, ' - is.na'))
    }
    
    
    mf_output_si <- mf_output_si[[1]]
    
    
    mf_output_si <- mf_output_si[year(time) >= start_year_of_analysis] # in case epidemic started pre-2025 
    mf_output_si[, vacc_type := names(vacc_type_list)[vaccine_type]] # add vaccine name
    mf_output_si[, simulation_index := sim_index] # add simulation number
    

    # printing if there is an NA error (shouldn't happen)
    if(is.na(sum(rowSums(mf_output_si %>% select(starts_with('I')))))){
      print(paste0('vt = ', vaccine_type, ', sim_index = ', sim_index, ' - is.na'))
    }
    
    # merge output 
    if(nrow(mf_output)==0){
      mf_output <- mf_output_si
    }else{
      mf_output <- rbind(mf_output, mf_output_si)
    }
    
    if(nrow(pan_output)==0){
      pan_output <- pandemic_only
    }else{
      pan_output <- rbind(pan_output, pandemic_only)
    }
    
    #if(nrow(combined_output)==0 & sim_index <= 33){
    #  combined_output <- combined_trial
    #}else if (sim_index <= 33 & nrow(combined_output)>0 ){
    #  combined_output <- rbind(combined_output, combined_trial)
    #} else if (nrow(combined_output2)==0 & sim_index > 33 & sim_index <= 66){
    #  combined_output2 <- combined_trial
    #} else if (sim_index > 33 & sim_index <= 66 & nrow(combined_output)>0){
    #  combined_output2 <- rbind(combined_output2, combined_trial)
    #}else if (nrow(combined_output2)==0 & sim_index > 66){
    #  combined_output3 <- combined_trial
    #} else if (sim_index > 66  & nrow(combined_output)>0){
    #  combined_output3 <- rbind(combined_output3, combined_trial)
    #}
    
    if(nrow(combined_output)==0 ){
      combined_output <- combined_trial
    }else if (nrow(combined_output)>0 ){
      combined_output <- rbind(combined_output, combined_trial)
    }
    
    
    # if(!file.exists(here::here('output','data','epi',paste0(itz_input,'_text')))){
    #   dir.create(file.path(here::here('output','data','epi',paste0(itz_input,'_text'))))
    # }
    
    # print txt file to keep track of simulations
    # if(sim_index == 1 | sim_index %% 10 == 0){
    #   writeLines(paste0(iso3c_input, ', simulation ', sim_index, ', time taken = ', round(Sys.time() - start_time,2),
    #                     ', number of epids = ', nrow(epid_dt[simulation_index==sim_index]),
    #                     ', total time on country = ', round(Sys.time() - total_start_time,2)),
    #              paste0('output/data/epi/',paste0(itz_input,'_text'),'/',paste0(vaccine_type, '_text.txt')))  
    # }
    
  }
  
  #trial <- list(mf_output, pan_output, combined_output, combined_output2, combined_output3)
  
  #trial <- list(mf_output, pan_output, combined_output)
  
  trial <- combined_output
  
  return(trial)
  
  #mf_output
}







