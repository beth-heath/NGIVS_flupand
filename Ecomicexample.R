#### Economic Analysis #####

#### Reading in variables that will be used ####

if (vaccine_strategy_pandemics == 'disease-mod'){
  dcr_infr <- 0.7
}else{
  dcr_infr <- 1
}

#### Editing data to get into the correct form #####

#let us start with just reading in the example-produced datasets

all_epid_dt <- rbindlist(all_epid)
pandemic_dt <- rbindlist(pandemic_only)
seasonal_dt <- all_epid_dt
seasonal_dt[, 2:16] <- seasonal_dt[,2:16]- pandemic_dt[,2:16]

#four_months<- restricting_topandemictime(epid_dt, infs_dt, 4)
#six_months<- restricting_topandemictime(epid_dt, infs_dt, 6)
#twelve_months<- restricting_topandemictime(epid_dt, infs_dt, 12)

### function to limit down the time that we study

restrict_pandemictime <- function(epid_dt, infs_dt, month_of_interest){
  pandemic_results <- epid_dt %>% filter(is.na(original_date))
  pandemic_results <- pandemic_results %>% mutate(restriction_period = epid_start_date %m+% months(month_of_interest) )
  restricing_infs_code <- infs_dt %>%
    inner_join(pandemic_results, by = "simulation_index") %>%
    filter(time < restriction_period)
  return(restricing_infs_code)
}

pandemic_dt <- restrict_pandemictime(epid_dt, pandemic_dt, 6)
seasonal_dt <- restrict_pandemictime(epid_dt, seasonal_dt, 6)

#### function to transform the data from how it was inputted from the previous model ####

long_form_data <- function(dataset){
  
  dataset_edit <- dataset[, lapply(.SD,sum),
                                  by=c('simulation_index','vacc_type'), .SDcols = 2:17]
  
  
  long_dt <- melt(dataset_edit, 
                  id.vars = c('simulation_index','vacc_type'),
                  measure.vars = c("IU1", "IU2", "IU3", "IU4", "IV1", "IV2", "IV3", "IV4", "IVR1", "IVR2", "IVR3", "IVR4"),
                  variable.name = "group", value.name = "value")
  
  long_dt$infection_group <- as.character(gsub("[0-9]", "", long_dt$group))
  long_dt$age_grp <- gsub("[A-Za-z]", "", long_dt$group)
  long_dt<- long_dt[,-'group']
  
  wide_dt <- dcast(long_dt, simulation_index + vacc_type+age_grp ~ infection_group, value.var = "value", fun.aggregate = sum)
  wide_dt <- wide_dt %>% mutate(infection_nonvac = IU +IV, infection_vac = IVR)
  
  return(wide_dt)
  
}


#### IFRS #####
#load in the dataset
national_ifrs <- data.table(read_csv('data/econ/national_ifrs.csv',
                                     show_col_types=F))

pandemic_ifrs <- data.table(read_csv('data/econ/pandemic_scns_ifr.csv',
                                     show_col_types=F))
pandemic_ifrs$age_grp <- as.character(pandemic_ifrs$age_grp)


calculating_deaths_for_seasonal <- function(dataset, country_of_interest, national_ifrs){
  #finding the ifr for country of interest
  ifrs_of_interest <- national_ifrs[country_code==country_of_interest]
  ifrs_of_interest$age_grp <- as.character(ifrs_of_interest$age_grp)
  
  #translating the data to longform
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .(simulation_index, age_grp, ifr)], by = c("simulation_index", "age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(deaths = infection_nonvac*ifr )
  
  return(merged_dataset)
}

calculating_deaths_for_pandemic <- function(dataset, year_of_interest, pandemic_ifrs){
  
  ifrs_of_interest <- pandemic_ifrs[pandemic_scns==year_of_interest]
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .( age_grp, ifr)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(deaths = infection_nonvac*ifr )
  
  return(merged_dataset)
}


## creating an overall summary of deaths

summary_overall_deaths <- function(epidemic_data, pandemic_data, country_of_interest, year_of_interest,
                                   national_ifrs, pandemic_ifrs){
  
  seasonal_data <- calculating_deaths_for_seasonal(epidemic_data, country_of_interest, national_ifrs)
  pandemic_data <- calculating_deaths_for_pandemic(pandemic_data, year_of_interest, pandemic_ifrs)
  combined_data <- merge(seasonal_data, pandemic_data, by=c("age_grp", "vacc_type", 'simulation_index') )
  combined_data <- combined_data %>% mutate(combined_deaths = deaths.x + deaths.y)
  summarising_over_age <- combined_data[, .(total_deaths = sum(combined_deaths)), by = .(vacc_type, simulation_index)]
  return(summarising_over_age)
}

##### IHRS ####

# loading in the datasets

global_ihrs <- data.table(read_csv('data/econ/global_ihrs.csv',
                                     show_col_types=F))


