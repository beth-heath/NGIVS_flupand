#### Economic Analysis #####

#### Reading in variables that will be used ####

if (vaccine_strategy_pandemics == 'disease-mod'){
  dcr_infr <- 0.7
}else{
  dcr_infr <- 1
}

#adding in WTP choice

WTP_choice <- c('lancet','gdp')[1]
WTP_GDP_ratio <- 1

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
  
  dataset <- dataset %>% mutate(year = year(time))
  
  dataset_edit <- dataset[, lapply(.SD,sum),
                                  by=c('simulation_index','vacc_type', 'year'), .SDcols = 2:17]
  
  
  long_dt <- melt(dataset_edit, 
                  id.vars = c('simulation_index','vacc_type', 'year'),
                  measure.vars = c("IU1", "IU2", "IU3", "IU4", "IV1", "IV2", "IV3", "IV4", "IVR1", "IVR2", "IVR3", "IVR4"),
                  variable.name = "group", value.name = "value")
  
  long_dt$infection_group <- as.character(gsub("[0-9]", "", long_dt$group))
  long_dt$age_grp <- gsub("[A-Za-z]", "", long_dt$group)
  long_dt<- long_dt[,-'group']
  
  wide_dt <- dcast(long_dt, simulation_index + vacc_type+age_grp + year ~ infection_group, value.var = "value", fun.aggregate = sum)
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


calculating_deaths_for_pandemic <- function(dataset, year_of_interest, pandemic_ifrs, dcr_infr){
  
  ifrs_of_interest <- pandemic_ifrs[pandemic_scns==year_of_interest]
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .( age_grp, ifr)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(deaths = infection_nonvac*ifr + dcr_infr*infection_vac*ifr )
  
  return(merged_dataset)
}


## creating an overall summary of deaths

summary_overall_deaths <- function(epidemic_data, pandemic_data, country_of_interest, year_of_interest,
                                   national_ifrs, pandemic_ifrs){
  
  seasonal_data <- calculating_deaths_for_seasonal(epidemic_data, country_of_interest, national_ifrs)
  pandemic_data <- calculating_deaths_for_pandemic(pandemic_data, year_of_interest, pandemic_ifrs)
  combined_data <- merge(seasonal_data, pandemic_data, by=c("age_grp", "vacc_type", 'simulation_index', 'year') )
  combined_data <- combined_data %>% mutate(combined_deaths = deaths.x + deaths.y)
  summarising_over_age <- combined_data[, .(total_deaths = sum(combined_deaths)), by = .(vacc_type, simulation_index)]
  return(summarising_over_age)
}

##### IHRS ####

# loading in the datasets

global_ihrs <- data.table(read_csv('data/econ/global_ihrs.csv',
                                     show_col_types=F))

outpatient_ratios <- data.table(read_csv(here::here('data','econ','outpatient_ratios.csv'),
                                         show_col_types=F))

#seasonal influenza function

calculating_hosps_for_seasonal <- function(dataset, dcr_infr, global_ihrs, outpatient_ratios,country_of_interest){
  
  edited_dataset <- long_form_data(dataset)
  
  global_ihrs$age_grp <- as.character(global_ihrs$age_grp)
  
  merged_dataset <- merge(edited_dataset, global_ihrs[, .( age_grp, ihr, simulation_index)], by = c("age_grp", "simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(hospitalisations = infection_nonvac*ihr*dcr_infr)
  
  #creating the number of outpatients from the number of hospitlisations
  
  outpatients_of_interest <- outpatient_ratios[country_code==country_of_interest]
  
  merged_dataset <- merge(merged_dataset, outpatients_of_interest [, .( simulation_index, ratio)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(outpatients = hospitalisations*ratio)
  
  return(merged_dataset)
}


#pandemic information

calculating_hosps_for_pandemic <- function(dataset, year_of_interest, 
                                           pandemic_ifrs, dcr_infr, hosp_ratio, outpatient_ratios,country_of_interest){
  
  #creating the number of hospitilisations 
  ifrs_of_interest <- pandemic_ifrs[pandemic_scns==year_of_interest]
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .( age_grp, ifr)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(hospitalisations = infection_nonvac*ifr*hosp_ratio + dcr_infr*infection_vac*ifr*hosp_ratio)
  
  #creating the number of outpatients from the number of hospitlisations
  
  outpatients_of_interest <- outpatient_ratios[country_code==country_of_interest]
  
  merged_dataset <- merge(merged_dataset, outpatients_of_interest [, .( simulation_index, ratio)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(outpatients = hospitalisations*ratio)
  
  return(merged_dataset)
}

### Symptomatic and Fever ####

symp_samples <- data.table(read_csv('data/econ/symp_samples.csv',
                                    show_col_types=F))

calculating_symptomatics <- function(dataset, symp_samples,dcr_infr){
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, symp_samples [, .( simulation_index, symp_prob, fever_prob)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(symptomatics = symp_prob*infection_nonvac + dcr_infr*symp_prob*infection_vac,
                                              fevers=fever_prob*infection_nonvac + dcr_infr*fever_prob*infection_vac,
                                              non_fevers = symptomatics - fevers) 
  return(merged_dataset)
  
}

#### YLLs #### 

yll_df <- data.table(read_csv('data/econ/yll_df.csv',
                                    show_col_types=F))

calculating_ylls_for_seasonal <- function(dataset, country_of_interest, national_ifrs,
                                            yll_df){
  #finding the ifr for country of interest
  
  merged_dataset <- calculating_deaths_for_seasonal(dataset, country_of_interest, national_ifrs)
  
  yll_of_interest <- yll_df[iso3c==country_of_interest]
  yll_of_interest$age_grp <- as.character(yll_of_interest$age_grp)
  
  merged_dataset <- merge(merged_dataset , yll_of_interest[, .(age_grp, yll)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(YLLs = yll*deaths )
  
  return(merged_dataset)
}

#### will need to add a different ylls for 1918 if chose to consider these seperately 

calculating_ylls_for_pandemics <- function(dataset, country_of_interest, year_of_interest, pandemic_ifrs, dcr_infr,
                                          yll_df){
  #finding the ifr for country of interest
  
  merged_dataset <- calculating_deaths_for_pandemic(dataset, year_of_interest, pandemic_ifrs, dcr_infr)
  
  yll_of_interest <- yll_df[iso3c==country_of_interest]
  yll_of_interest$age_grp <- as.character(yll_of_interest$age_grp)
  
  merged_dataset <- merge(merged_dataset , yll_of_interest[, .(age_grp, yll)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(YLLs = yll*deaths )
  
  return(merged_dataset)
}


### YLDs ####

DALY_weight_samples <- data.table(read_csv('data/econ/DALY_weight_samples.csv',
                              show_col_types=F))

flu_duration <- 4/365

calculating_ylds_for_seasonal <- function(dataset, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,country_of_interest){
  
  symptomatic_numbers <- calculating_symptomatics(dataset, symp_samples,dcr_infr)
  
  
  hospital_numbers <- calculating_hosps_for_seasonal(dataset, dcr_infr, global_ihrs, outpatient_ratios,country_of_interest)
  
  merged_dataset <- merge(symptomatic_numbers, hospital_numbers[, .(age_grp, simulation_index, vacc_type ,year, hospitalisations, outpatients)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)
  
  merged_dataset <- merge(merged_dataset, DALY_weight_samples[, .(simulation_index, non_fever_DALY,fever_DALY, hosp_DALY)], by = c("simulation_index"), all.x = TRUE)
 
  merged_dataset <- merged_dataset %>% mutate(non_fever_DALYs= flu_duration*non_fever_DALY*non_fevers,
                                              fever_DALYs := flu_duration*fever_DALY*fevers,
                                              hosp_DALYs := flu_duration*hosp_DALY*hospitalisations)
  
  return(merged_dataset)
  
}




calculating_ylds_for_pandemic <- function(dataset, symp_samples,dcr_infr, year_of_interest,hosp_ratio, 
                                          global_ihrs, outpatient_ratios,country_of_interest,
                                          DALY_weight_samples){
  
  symptomatic_numbers <- calculating_symptomatics(dataset, symp_samples,dcr_infr)
  

  hospital_numbers <- calculating_hosps_for_pandemic(dataset, year_of_interest, 
                                                     pandemic_ifrs, dcr_infr, hosp_ratio, outpatient_ratios,country_of_interest)
  
  merged_dataset <- merge(symptomatic_numbers, hospital_numbers[, .(age_grp, simulation_index, vacc_type ,hospitalisations, outpatients, year)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)
  
  merged_dataset <- merge(merged_dataset, DALY_weight_samples[, .(simulation_index, non_fever_DALY,fever_DALY, hosp_DALY)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(non_fever_DALYs= flu_duration*non_fever_DALY*non_fevers,
                                              fever_DALYs := flu_duration*fever_DALY*fevers,
                                              hosp_DALYs := flu_duration*hosp_DALY*hospitalisations)
  
  return(merged_dataset)
  
}

### calculating total DALYs

total_DALYS_for_seasonal <- function(dataset, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,country_of_interest,
                                     national_ifrs, yll_df){
  
  ylds<- calculating_ylds_for_seasonal(dataset, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,country_of_interest)
  
  ylls<- calculating_ylls_for_seasonal(dataset, country_of_interest, national_ifrs, yll_df)
  
  merged_dataset <- merge(ylds, ylls[, .(age_grp, simulation_index, vacc_type ,YLLs, year)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(total_DALYS = non_fever_DALYs + fever_DALYs + hosp_DALYs + YLLs)
  
  return(merged_dataset)
  
}





total_DALYS_for_pandemic <- function(dataset, symp_samples,dcr_infr, year_of_interest,hosp_ratio, 
                                     global_ihrs, outpatient_ratios,country_of_interest,
                                     DALY_weight_samples, pandemic_ifrs, yll_df){
  ylds<- calculating_ylds_for_pandemic(dataset, symp_samples,dcr_infr, year_of_interest,hosp_ratio, 
                                       global_ihrs, outpatient_ratios,country_of_interest,
                                       DALY_weight_samples)
  
  ylls<- calculating_ylls_for_pandemics(dataset, country_of_interest, year_of_interest, pandemic_ifrs, dcr_infr,
                                        yll_df)
  
  
  merged_dataset <- merge(ylds, ylls[, .(age_grp, simulation_index, vacc_type ,YLLs, year)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)

  
  merged_dataset <- merged_dataset %>% mutate(total_DALYS = non_fever_DALYs + fever_DALYs + hosp_DALYs + YLLs)
  return(merged_dataset)
  
}

#### Healthcare costs #####
## loads in the pred_costs file in
load(here::here('data','econ','predicted_costs'))
 
## getting the data in the correct form

#rearranges the table
cost_predic_c <- dcast(pred_costs[,c('iso3c','outcome','study_pop',
                                     'simulation_index','gdpcap','sample_cost')], iso3c + study_pop + gdpcap + simulation_index ~ outcome, value.var = 'sample_cost')

#
cost_predic_c[study_pop == 'adults', age_grp := 3]
cost_predic_c[study_pop == 'children', age_grp := 1]
cost_predic_c[study_pop == 'elderly', age_grp := 4]
#duplicating out the rows of adults and having one with an age-group of 2
cost_predic_c <- rbind(cost_predic_c, cost_predic_c[study_pop == 'adults',][,age_grp := 2])
cost_predic_c[, study_pop := NULL]
setnames(cost_predic_c, 'hospital', 'hosp_cost')
setnames(cost_predic_c, 'outpatient', 'outp_cost')

healthcost_analysis <- function(DALY_file, cost_predic_c, country_of_interest){
  cost_of_interest <- cost_predic_c[iso3c == country_of_interest]
  cost_of_interest$age_grp <- as.character(cost_of_interest$age_grp)
  merged_dataset <- merge(DALY_file, cost_of_interest[, .(age_grp, simulation_index, hosp_cost ,outp_cost)], by = c("age_grp", "simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(total_hosp_cost = hosp_cost*hospitalisations,
                                              total_outp_cost = outp_cost * outpatients
                                              )
  return(merged_dataset)
  
}


##### Dose costs ####

##### WTP threshold #####

wtp_thresh <- data.table(read_csv(here::here('data/econ/WTP_thresholds.csv'), show_col_type=F))

adding_in_WTP <- function(DALY_file, WTP_choice, wtp_thresh, country_of_interest, WTP_GDP_ratio){
  if(WTP_choice == 'lancet'){
    wtp_of_interest <- wtp_thresh[iso3c==country_of_interest]$cet
  }
  if(WTP_choice == 'gdp'){
    wtp_of_interest <- WTP_GDP_ratio*wtp_thresh[iso3c==country_of_interest]$gdpcap
  }
  
  DALY_file <- DALY_file %>% mutate(cost_of_DALYs = wtp_of_interest*total_DALYS)
  return(DALY_file)
}


##### Discounting #####

adding_in_discounting <- function(final_analysis_file, discount_rate){
  
}

trial1<- total_DALYS_for_seasonal(seasonal_dt, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,'GBR',
                         national_ifrs, yll_df)

healthcost_analysis(trial1, cost_predic_c, 'GBR')

adding_in_WTP(trial1, WTP_choice, wtp_thresh, 'GBR', WTP_GDP_ratio)


