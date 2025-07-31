set.seed(123)
library("lubridate")


#creating the list of dates to feed into the model
creating_dates <- rdate(100,
                        min = paste0(2026, "-01-01"),
                        max = paste0(2026, "-12-31")
)


##### Having a pandemic addition function to add in the pandemic to the data ######

Pandemic_addition_function <- function(simulations, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                        length_analysis, ageing_date, creating_dates){
  #simutlion index is the number of simulations
  simulation_index<- seq(1,simulations, 1)
  #adding in susceptibility between certain bounds
  susceptibility <- runif(simulations, susceptibility_range[1], susceptibility_range[2])
  #adding in the susceptibility for children
  susceptibility_for_kids <- runif(simulations, sus_boost_for_children[1], sus_boost_for_children[2])
  #adding in transmissibility 
  transmissibility <- runif(simulations, trans_range[1], trans_range[2])
  #adding in r0_to_scale_match - this will be set to NA in our case
  r0_to_scale <- rep(r0, simulations)
  # does not match
  match <- rep(FALSE, simulations)
  #adding in the date for the simulation
  start_date_late <- creating_dates[1:simulations]
  
  #setting the original date to NA as does not exist for the pandemic and make the pandemic distinct 
  original_date <- rep(NA, simulations)
  
  # Working out the ageing date and which year it is effectively in
  ageing_year_start <- ifelse(
    as.Date(paste0(2026, '-', ageing_date), format = "%Y-%d-%m") > start_date_late,
    2025,
    2026
  )
  
  #select start_date_late to be the same as the epidemic_start_date 
  epid_start_date <- start_date_late
  #As we have no delay - we have the number of initial infections to be 10 to match the other epidemics.
  initial_infected <- rep(10, simulations)
  #Adding in the set start date 
  period_start_date <- rep(as.Date(paste0('01-01-2025'),format='%d-%m-%Y'), simulations)
  #adding in the same end date for each of them
  end_date <- rep(as.Date(paste0('01-01-',start_year_of_analysis + length_analysis),format='%d-%m-%Y'), simulations)
  #combining together the dataset
  combined_table <- data.frame(simulation_index, susceptibility, susceptibility_for_kids, transmissibility, r0_to_scale, 
                               match, start_date_late,original_date, ageing_year_start,
                               epid_start_date, initial_infected, period_start_date, end_date)
  #setting it up to distinguish from the others 
  combined_table$year_pandemic <- rep(1, nrow(combined_table))
  combined_table$true_sim_no <- combined_table$simulation_index
  
  pandemic_combined <- combined_table
  
  #adding in the other times that the pandemic could be
  for (pand_time in 1:27){
    addition_version <- combined_table
    #adding 12 months for each year later that the analysis is
    addition_version$epid_start_date <- addition_version$epid_start_date  %m+% months(12*pand_time)
    addition_version$start_date_late <- addition_version$start_date_late  %m+% months(12*pand_time)
    addition_version$true_sim_no <- addition_version$simulation_index
    addition_version$simulation_index <- addition_version$simulation_index + 100*(pand_time)
    addition_version$ageing_year_start <- addition_version$ageing_year_start + pand_time
    
    addition_version$year_pandemic <- rep(pand_time, nrow(addition_version))
    
    pandemic_combined <- rbind(pandemic_combined, addition_version)
    
  }
  
  
  return(pandemic_combined)
}


pandemic_example <- Pandemic_addition_function( 100, c(0.80, 0.95), c(0.06772161, 0.08324118), c(0.8,0.95), NA,
                                                     30, '01-04', creating_dates)
save(pandemic_example, file='1918_combined_set_NH.Rdata')

pandemic_example<- Pandemic_addition_function(100, c(0.60, 0.8), c(0.06772161, 0.08324118), c(0.7,0.9), NA,
                                                     30,'01-04', creating_dates)
save(pandemic_example, file='1957_combined_set_NH.Rdata')

pandemic_example<- Pandemic_addition_function(100, c(0.30, 0.5), c(0.06772161, 0.08324118), c(0.8,0.95), NA,
                                                   30, '01-04', creating_dates)
save(pandemic_example, file='2009_combined_set_NH.Rdata')

pandemic_example <- Pandemic_addition_function(100, c(0.80, 0.95), c(0.06772161, 0.08324118), c(0.8,0.95), NA,
                                                30, '01-10', creating_dates)
save(pandemic_example, file='1918_combined_set_SH.Rdata')

pandemic_example<- Pandemic_addition_function(100, c(0.60, 0.8), c(0.06772161, 0.08324118), c(0.7,0.9), NA,
                                              30, '01-10', creating_dates)
save(pandemic_example, file='1957_combined_set_SH.Rdata')

pandemic_example<- Pandemic_addition_function(100, c(0.30, 0.5), c(0.06772161, 0.08324118), c(0.8,0.95), NA,
                                             30, '01-10', creating_dates)
save(pandemic_example, file='2009_combined_set_NH.Rdata')


