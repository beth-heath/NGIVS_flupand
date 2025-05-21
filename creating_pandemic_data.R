set.seed(123)
library("lubridate")

titles <- c('simulation_index', 'susceptibility', 'transmissibility', 'r0_to_scale', 
            'match', 'start_date_late','original_date', 'ageing_year_start',
            'epid_start_date', 'initial_infected', 'period_start_date', 'end_date', 'susceptibility_for_kids')
pan_dt <<- data.frame(matrix(nrow=0, ncol=length(titles)))
colnames(pan_dt) <- titles

disease_scenarios <- c('1918', '1957', '2009')[1]
if (disease_scenarios == '1918'){
  susceptibility_range <- c(0.80, 0.9)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.9)
  r0 <- NA
} else if (disease_scenarios == '1957'){
  susceptibility_range <- c(0.60, 0.8)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.7,0.9)
  r0 <- NA
} else if (disease_scenarios == '2009'){
  susceptibility_range <- c(0.50, 0.7)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.95)
  r0 <- NA
}


Pandemic_addition_function <- function(original_epi_data, simulations, pandemic_year_chosen, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                       start_year_of_analysis, length_analysis){
  simulation_index<- seq(1,simulations, 1)
  #adding in random parts of susceptibility between certain bounds
  susceptibility <- runif(simulations, susceptibility_range[1], susceptibility_range[2])
  #adding in transmissibility 
  transmissibility <- runif(simulations, trans_range[1], trans_range[2])
  #adding in r0_to_scale_match - do not think will have these
  r0_to_scale <- rep(r0, simulations)
  
  #not having matching
  match <- rep(FALSE, simulations)
  
  #start_date_late
  start_date_late <- rdate(simulations,
                           min = paste0(2025+pandemic_year_chosen, "-01-01"),
                           max = paste0(2025+pandemic_year_chosen, "-12-31")
  )
  
  #original date
  original_date <- rep(NA, simulations)
  
  #ageing year start
  ageing_year_start <- rep(2025+pandemic_year_chosen, simulations)
  
  #epidemic_start_date - in version put in the same as the start_date_late
  epid_start_date <- start_date_late
  
  
  
  #initial infected -  as can choose can choose 10
  initial_infected <- rep(10, simulations)
  #copying the other dates for the start date and the end date
  #period_start_date <- as.Date(paste0('01-01-',start_year_of_analysis),format='%d-%m-%Y')
  #end_date <- as.Date(paste0('01-01-',start_year_of_analysis + pandemic_year_chosen),format='%d-%m-%Y')
  
  #trialling
  period_start_date_1 <- as.Date(paste0('01-01-',start_year_of_analysis),format='%d-%m-%Y')
  end_date_1 <- as.Date(paste0('01-01-',start_year_of_analysis + length_analysis),format='%d-%m-%Y')
  period_start_date <- rep(period_start_date_1, simulations)
  end_date <- rep(end_date_1[1], simulations)
  
  #prev
  #period_start_date <- rep(original_epi_data$period_start_date[1], simulations)
  #end_date <- rep(original_epi_data$end_date[1], simulations)
  
  #adding in the sus boost for children, which will come from input data
  susceptibility_for_kids <- runif(simulations, sus_boost_for_children[1], sus_boost_for_children[2])
  
  combined_table <- data.frame(simulation_index, susceptibility, transmissibility, r0_to_scale, 
                               match, start_date_late,original_date, ageing_year_start,
                               epid_start_date, initial_infected, period_start_date, end_date, susceptibility_for_kids)
  
  original_epi_data$susceptibility_for_kids <- original_epi_data$susceptibility
  
  # Count how many times each simulation appears
  
  if (nrow(original_epi_data)>0){
    simulation_counts <- base::table(original_epi_data$simulation_index)
    
    # Repeat each end_date value according to the count
    #originally had the end count at the time of the pandemic however want to change this to make sure not too many cases when turned off
    #therefore instead consider that have to be three weeks before the release of the pathogen 
    #can add this to description and discuss later
    #original_epi_data$cut_off_time <- rep(epid_start_date, times = simulation_counts)
    
    #calculating new cut off date - have it at a month - 30 days
    epid_start_wdelay <- as.Date(as.numeric(epid_start_date)-60)
    
    
    original_epi_data$cut_off_time <- rep(epid_start_wdelay, times = simulation_counts)
    #comparing with the start date late
    reduced_timing <- original_epi_data[which(original_epi_data$epid_start_date < original_epi_data$cut_off_time ),]
    
    reduced_timing <- reduced_timing[,1:13]
    original_epi_data <- rbind(reduced_timing, combined_table)
  } else {
    reduced_timing <- original_epi_data
    original_epi_data <- rbind(reduced_timing, combined_table)
  }
  return(original_epi_data)
}

pandemic_example <- Pandemic_addition_function(pan_dt, 100, 0, c(0.80, 0.9), c(0.07249, 0.09834), c(0.7,0.9), NA,
                                                    2025, 30)

pandemic_example<- Pandemic_addition_function(pan_dt, 100, 0, c(0.60, 0.8), c(0.07249, 0.09834), c(0.8,0.9), NA,
                                                    2025, 30)

pandemic_example<- Pandemic_addition_function(pan_dt, 100, 0, c(0.50, 0.7), c(0.07249, 0.09834), c(0.8,0.95), NA,
                                                   2025, 30)
save(pandemic_example, file='1918_pandemic_samples.Rdata')
save(pandemic_example, file='1957_pandemic_samples.Rdata')
save(pandemic_example, file='2009_pandemic_samples.Rdata')

## To do - have a function that:
# - col combine the pandemic file to the epid dt one
# adds the time that is studied to the start_date late, ageing_start_year, epid_start_year
# gets rid of the epidemic that occur less than two months or after the pandemic
#Once function works:
#alter the GBR code to work with these new changes to have a set file for the pandemic
#remember to link to this in the loaded files.

selecting_pandemic_parameters <- function(epid_dt, year_pandemic, pandemic_scns){
  
  #opening the correct pandemic year of samples
  file_name <- paste0(pandemic_scns, "_pandemic_samples.Rdata")
  if (file.exists(file_name)){
    load(file_name)
    pandemic_chosen <- pandemic_example
  } else {
    stop("Pandemic year chosen does not exist")
  }
  
  #alter the time that is studied to add the year of pandemic to the different parameters
  pandemic_chosen$start_date_late <- pandemic_chosen$start_date_late %m+% months(year_pandemic*12)
  
  
  pandemic_chosen$ageing_year_start <- pandemic_chosen$ageing_year_start + year_pandemic
  pandemic_chosen$epid_start_date <- pandemic_chosen$epid_start_date %m+% months(year_pandemic*12)
  
  if (length(epid_dt)==1){
    combining_pand_seas <- pandemic_chosen
  } else{
    epid_dt$susceptibility_for_kids <- epid_dt$susceptibility
    simulation_counts <- base::table(epid_dt$simulation_index)
    epid_start_wdelay <- as.Date(as.numeric(pandemic_chosen$epid_start_date)-60)
    epid_dt$cut_off_time <- rep(epid_start_wdelay, times = simulation_counts)
    reduced_timing <- epid_dt[which(epid_dt$epid_start_date < epid_dt$cut_off_time ),]
    reduced_timing <- reduced_timing[,1:13]
    combining_pand_seas <- rbind(reduced_timing, pandemic_chosen)
    
  }
  
  return(combining_pand_seas)
  
}



test1 <- selecting_pandemic_parameters(epid_dt, 2, '1918')

test2 <- test1[[1]]
test3 <- test1[[2]]

