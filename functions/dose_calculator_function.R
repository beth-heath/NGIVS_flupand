#cannot calculate for less than one year 
#probably best to run this code for all simulations, for 6 months and combine together

flu_dose_calculator <- function(
    country,
    ageing,
    ageing_date,
    pandemic,
    vaccine_type,
    vaccine_variable,
    vacc_type_list,
    model_age_groups,
    month_of_interest,
    cov_vec
){
  dates_many_flu <- seq.Date(last_monday(pandemic$period_start_date), 
                             last_monday(pandemic$epid_start_date %m+% months(month_of_interest) ), 
                             by=7)
  
  vacc_name <- names(vacc_type_list)[vaccine_type]
  
  vaccine_used_vec <- if(vaccine_variable == 'doses'){
    # if using doses, NGIVs are often introduced years after the start of the epidemic period
    doses[vacc_scenario == vacc_name & model_age_group==1]$vacc_used
  }else{
    # if using coverage, assuming NGIVs available each year (adjust here if not!)
    rep(vacc_name, year(pandemic$epid_start_date %m+% months(month_of_interest)) - year(pandemic$period_start_date))
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

test1 <- flu_dose_calculator(country ='GBR',
                    ageing=T,
                    ageing_date='01-04',
                    pandemic = pandemic_example[pandemic_example$simulation_index==1,],
                    vaccine_type =6,
                    vaccine_variable,
                    vacc_type_list,
                    model_age_groups,
                    month_of_interest=12, cov_vec)


test2 <- flu_dose_calculator(country ='GBR',
                             ageing=T,
                             ageing_date='01-04',
                             pandemic = pandemic_example[pandemic_example$simulation_index==1,],
                             vaccine_type =1,
                             vaccine_variable,
                             vacc_type_list,
                             model_age_groups,
                             month_of_interest=48, cov_vec)




c_number <- 4
c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[c_number]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[c_number]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[c_number]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
simulations <-100
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

vaccine_variable <- c('doses','coverage')[2] 

cov_val <- 0.5

# define age groups targeted, e.g. here <10yos and 65+yos
cov_ages <- c(0:10, 65:101)

# what % coverage in each model age group?
cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)

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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                                            month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                                            month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
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
                        month_of_interest,
                        cov_vec)
    
    vacc_name <- names(vacc_type_list)[vaccine_type]
    doses_calculated$vacc_type <- rep(vacc_name, nrow(doses_calculated))
    doses_calculated$vacc_population <- rep('65-101', nrow(doses_calculated))
    doses_calculated
  } 
}






mclapply(1:10, flu_doses_parallal_1, mc.cores=1)
mclapply(1:10, flu_doses_parallal_2, mc.cores=10)
mclapply(1:10, flu_doses_parallal_3, mc.cores=10)


test1 <- mclapply(1:10, flu_doses_parallal_1, mc.cores=10)
test2 <- mclapply(1:10, flu_doses_parallal_2, mc.cores=10)
test3 <- mclapply(1:10, flu_doses_parallal_3, mc.cores=10)


test4 <- rbind(rbindlist(test1), rbindlist(test2), rbindlist(test3))

country_codes <- country_itzs_names$codes

doses_table <- c()

key_dates <- c('01-04', '01-10')

#for (country_no in 1:length(country_codes)){
#  for (simulation_number in 1:100){
#    for (year_choice in 1:30){

      
#    country = country_codes[country_no]
    
#    continent_interest <- country_itzs_names[country_no]$cluster_name
    
#    ageing_date =  ifelse(continent_interest=="Oceania-Melanesia-Polynesia"|continent_interest=="Southern America", key_dates[2], key_dates[1])
#    pandemic = pandemic_example[pandemic_example$simulation_index==simulation_number,]
#    month_of_interest = 12*year_choice
    
#    first_doses <- mclapply(1:10, flu_doses_parallal_1, mc.cores=10)
#    second_doses <- mclapply(1:10, flu_doses_parallal_2, mc.cores=10)
#    third_doses <- mclapply(1:10, flu_doses_parallal_3, mc.cores=10)
    
#    combined_doses <- rbind(rbindlist(first_doses), rbindlist(second_doses), rbindlist(third_doses))
    
#    combined_doses$simulation_number <- rep(simulation_number, nrow(combined_doses))
#    combined_doses$year_pandemic <- rep(year_choice, nrow(combined_doses))
    
#    if (is.null(doses_table)){
#      doses_table <- combined_doses
#    } else{
#      doses_table <- rbind(doses_table, combined_doses)
#    }
    
#    print(country_no)
    
#    }
#  }
#}

doses_table <- c()

key_dates <- c('01-04', '01-10')


for (country_no in 1:length(country_codes)){

      country = country_codes[country_no]
      
      continent_interest <- country_itzs_names[country_no]$cluster_name
      
      ageing_date =  ifelse(continent_interest=="Oceania-Melanesia-Polynesia"|continent_interest=="Southern America", key_dates[2], key_dates[1])
      pandemic = pandemic_example[pandemic_example$simulation_index==simulation_number,]
      month_of_interest = 12*30 + 6
      
      
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




pandemic = pandemic_example[pandemic_example$simulation_index==1,]

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

doses_table <- c()

key_dates <- c('01-04', '01-10')


for (country_no in 1:length(country_codes)){
  
  country = country_codes[country_no]
  
  continent_interest <- country_itzs_names[country_no]$cluster_name
  
  ageing_date =  ifelse(continent_interest %in% c("Oceania-Melanesia-Polynesia", "Southern America"), key_dates[2], key_dates[1])
  vacc_calendar_start <- ifelse(continent_interest %in% c("Oceania-Melanesia-Polynesia", "Southern America"), key_dates[1], key_dates[2])
  first_doses <- mclapply(1:10, flu_doses_parallal_1, mc.cores=1)
  
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








