#### Economic Analysis #####

#### Reading in parameter values that will be used ####


wastage <- 0.1
WTP_choice <- c('lancet','gdp')[1]
WTP_GDP_ratio <- 1


cost_discount_rate_val <- c(0.03, 0.03)[1 + discount_SA]
DALY_discount_rate_val <- c(0.03, 0)[1 + discount_SA]


#### Editing data to get into the correct form ####
#### function to transform the data from how it was inputted from the previous model ####

long_form_data <- function(dataset){
  
  dataset <- setDT(dataset)
  
  dataset_edit <- dataset[, lapply(.SD,sum),
                          by=c('simulation_index','vacc_type', 'year'), .SDcols = 4:20]
  
  dataset_edit$simulation_index <- (dataset_edit$simulation_index +99) %% 100 +1
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

long_form_data_pandemic <- function(dataset){
  
  dataset <- setDT(dataset)
  
  dataset_edit <- dataset[, lapply(.SD, sum),
                          by = c('simulation_index', 'vacc_type', 'year', 'mechanism'),
                          .SDcols = names(dataset)[sapply(dataset, is.numeric)]
  ]
  
  dataset_edit$simulation_index <- (dataset_edit$simulation_index +99) %% 100 +1
  long_dt <- melt(dataset_edit, 
                  id.vars = c('simulation_index','vacc_type', 'year', 'mechanism'),
                  measure.vars = c("IU1", "IU2", "IU3", "IU4", "IV1", "IV2", "IV3", "IV4", "IVR1", "IVR2", "IVR3", "IVR4"),
                  variable.name = "group", value.name = "value")
  
  
  
  long_dt$infection_group <- as.character(gsub("[0-9]", "", long_dt$group))
  long_dt$age_grp <- gsub("[A-Za-z]", "", long_dt$group)
  long_dt<- long_dt[,-'group']
  
  wide_dt <- dcast(long_dt, simulation_index + vacc_type+age_grp + year+mechanism  ~ infection_group, value.var = "value", fun.aggregate = sum)
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

country_specs <- data.table(read.xlsx(here::here('data','econ','country_specs.xlsx')))
LMICS_country <- country_specs[country_specs$income_g %in% c('LIC', 'LMIC'), ]$iso3c
LMICS_country <- c(LMICS_country, 'PSE')

calculating_deaths_for_pandemic <- function(dataset, year_of_interest, pandemic_ifrs, case_proportion, LMICS_country, LMIC_boost, country_of_interest){
  
  ifrs_of_interest <- pandemic_ifrs[pandemic_scns==year_of_interest]
  
  edited_dataset <- long_form_data_pandemic(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .( age_grp, ifr)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% 
    mutate(dcr_infr = case_when(
      mechanism == 'sterilising' ~ 1,
      TRUE ~ 0.7
    ))
  
  boost <- if (country_of_interest %in% LMICS_country) LMIC_boost else 1
  
  merged_dataset <- merged_dataset %>% mutate(deaths = infection_nonvac*ifr*case_proportion*boost + dcr_infr*infection_vac*ifr*case_proportion*boost )
  
  return(merged_dataset)
}





##### IHRS ####

# loading in the datasets

global_ihrs <- data.table(read_csv('data/econ/global_ihrs.csv',
                                     show_col_types=F))

outpatient_ratios <- data.table(read_csv(here::here('data','econ','outpatient_ratios.csv'),
                                         show_col_types=F))

#seasonal influenza function

calculating_hosps_for_seasonal <- function(dataset, global_ihrs, outpatient_ratios,country_of_interest){
  
  edited_dataset <- long_form_data(dataset)
  
  global_ihrs$age_grp <- as.character(global_ihrs$age_grp)
  
  merged_dataset <- merge(edited_dataset, global_ihrs[, .( age_grp, ihr, simulation_index)], by = c("age_grp", "simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(hospitalisations = infection_nonvac*ihr)
  
  #creating the number of outpatients from the number of hospitlisations
  
  outpatients_of_interest <- outpatient_ratios[country_code==country_of_interest]
  
  merged_dataset <- merge(merged_dataset, outpatients_of_interest [, .( simulation_index, ratio)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(outpatients = hospitalisations*ratio)
  
  return(merged_dataset)
}


#pandemic information

calculating_hosps_for_pandemic <- function(dataset, year_of_interest, 
                                           pandemic_ifrs, hosp_ratio, outpatient_ratios,country_of_interest,
                                           case_proportion){
  
  #creating the number of hospitilisations 
  ifrs_of_interest <- pandemic_ifrs[pandemic_scns==year_of_interest]
  
  edited_dataset <- long_form_data_pandemic(dataset)
  
  merged_dataset <- merge(edited_dataset, ifrs_of_interest[, .( age_grp, ifr)], by = c("age_grp"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% 
    mutate(dcr_infr = case_when(
      mechanism == 'sterilising' ~ 1,
      TRUE ~ 0.7
    ))
  
  merged_dataset <- merged_dataset %>% mutate(hospitalisations = infection_nonvac*ifr*hosp_ratio*case_proportion + dcr_infr*infection_vac*ifr*hosp_ratio*case_proportion)
  
  #creating the number of outpatients from the number of hospitlisations
  
  outpatients_of_interest <- outpatient_ratios[country_code==country_of_interest]
  
  merged_dataset <- merge(merged_dataset, outpatients_of_interest [, .( simulation_index, ratio)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(outpatients = hospitalisations*ratio)
  
  return(merged_dataset)
}



### Symptomatic and Fever ####

symp_samples <- data.table(read_csv('data/econ/symp_samples.csv',
                                    show_col_types=F))

calculating_symptomatics <- function(dataset, symp_samples){
  
  edited_dataset <- long_form_data(dataset)
  
  merged_dataset <- merge(edited_dataset, symp_samples [, .( simulation_index, symp_prob, fever_prob)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(symptomatics = symp_prob*infection_nonvac,
                                              fevers=fever_prob*infection_nonvac,
                                              non_fevers = symptomatics - fevers) 
  return(merged_dataset)
  
}

calculating_symptomatics_pandemics <- function(dataset, symp_samples){
  
  edited_dataset <- long_form_data_pandemic(dataset)
  
  merged_dataset <- merge(edited_dataset, symp_samples [, .( simulation_index, symp_prob, fever_prob)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% 
    mutate(dcr_infr = case_when(
      mechanism == 'sterilising' ~ 1,
      TRUE ~ 0.7
    ))
  
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

calculating_ylls_for_pandemics <- function(dataset, country_of_interest, year_of_interest, pandemic_ifrs,
                                          yll_df, case_proportion, LMICS_country, LMIC_boost){
  #finding the ifr for country of interest
  
  merged_dataset <- calculating_deaths_for_pandemic(dataset, year_of_interest, pandemic_ifrs, case_proportion, LMICS_country, LMIC_boost, country_of_interest)
  
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

calculating_ylds_for_seasonal <- function(dataset, symp_samples, global_ihrs, outpatient_ratios,country_of_interest, DALY_weight_samples){
  
  symptomatic_numbers <- calculating_symptomatics(dataset, symp_samples)
  
  
  hospital_numbers <- calculating_hosps_for_seasonal(dataset, global_ihrs, outpatient_ratios,country_of_interest)
  
  merged_dataset <- merge(symptomatic_numbers, hospital_numbers[, .(age_grp, simulation_index, vacc_type ,year, hospitalisations, outpatients)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)
  
  merged_dataset <- merge(merged_dataset, DALY_weight_samples[, .(simulation_index, non_fever_DALY,fever_DALY, hosp_DALY)], by = c("simulation_index"), all.x = TRUE)
 
  merged_dataset <- merged_dataset %>% mutate(non_fever_DALYs= flu_duration*non_fever_DALY*non_fevers,
                                              fever_DALYs := flu_duration*fever_DALY*fevers,
                                              hosp_DALYs := flu_duration*hosp_DALY*hospitalisations)
  
  return(merged_dataset)
  
}




calculating_ylds_for_pandemic <- function(dataset, symp_samples, year_of_interest,hosp_ratio, 
                                          global_ihrs, outpatient_ratios,country_of_interest,
                                          DALY_weight_samples, case_proportion){
  
  symptomatic_numbers <- calculating_symptomatics_pandemics(dataset, symp_samples)
  

  hospital_numbers <- calculating_hosps_for_pandemic(dataset, year_of_interest, 
                                                     pandemic_ifrs, hosp_ratio, outpatient_ratios,country_of_interest, case_proportion)
  
  merged_dataset <- merge(symptomatic_numbers, hospital_numbers[, .(age_grp, simulation_index, vacc_type,mechanism, hospitalisations, outpatients, year)], by = c("age_grp", "simulation_index", "vacc_type", 'year', 'mechanism'), all.x = TRUE)
  
  merged_dataset <- merge(merged_dataset, DALY_weight_samples[, .(simulation_index, non_fever_DALY,fever_DALY, hosp_DALY)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% 
    mutate(flu_duration = case_when(
      mechanism == 'infection period' ~ 2/365,
      TRUE ~ 4/365
    ))
  
  
  merged_dataset <- merged_dataset %>% mutate(non_fever_DALYs= flu_duration*non_fever_DALY*non_fevers,
                                              fever_DALYs := flu_duration*fever_DALY*fevers,
                                              hosp_DALYs := flu_duration*hosp_DALY*hospitalisations)
  
  return(merged_dataset)
  
}



### calculating total DALYs

total_DALYS_for_seasonal <- function(dataset, symp_samples, global_ihrs, outpatient_ratios,
                                     country_of_interest,
                                     national_ifrs, yll_df, DALY_weight_samples){
  
  ylds<- calculating_ylds_for_seasonal(dataset, symp_samples, global_ihrs, outpatient_ratios,country_of_interest, DALY_weight_samples)
  
  ylls<- calculating_ylls_for_seasonal(dataset, country_of_interest, national_ifrs, yll_df)
  
  merged_dataset <- merge(ylds, ylls[, .(age_grp, simulation_index, vacc_type ,YLLs, year, deaths)], by = c("age_grp", "simulation_index", "vacc_type", 'year'), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(total_DALYS = non_fever_DALYs + fever_DALYs + hosp_DALYs + YLLs)
  
  return(merged_dataset)
  
}





total_DALYS_for_pandemic <- function(dataset, symp_samples, year_of_interest,hosp_ratio, 
                                     global_ihrs, outpatient_ratios,country_of_interest,
                                     DALY_weight_samples, pandemic_ifrs, yll_df, case_proportion,
                                     LMICS_country, LMIC_boost){
  ylds<- calculating_ylds_for_pandemic(dataset, symp_samples, year_of_interest,hosp_ratio, 
                                       global_ihrs, outpatient_ratios,country_of_interest,
                                       DALY_weight_samples, case_proportion)
  
  ylls<- calculating_ylls_for_pandemics(dataset, country_of_interest, year_of_interest, pandemic_ifrs, 
                                        yll_df, case_proportion, LMICS_country, LMIC_boost)
  
  
  merged_dataset <- merge(ylds, ylls[, .(age_grp, simulation_index, vacc_type ,YLLs, year, deaths, mechanism)], by = c("age_grp", "simulation_index", "vacc_type", 'year', 'mechanism'), all.x = TRUE)

  
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

# Doses code where for each simulation it works out the doses by restricting to the time studied from what is coming in
#calculate the doses then do sums for them
#I think I should make a seperate dose calculator and add that lists each pandemic number and then consequently the number of doses used in each year for each pandemic scenario.
#then load this into the dataset


#loading in the doses datasets


costs <- data.table(read_csv(here::here('data','econ','prices.csv'), show_col_types=F))
country_specs <- data.table(read.xlsx(here::here('data','econ','country_specs.xlsx')))
delivery_cost_samples <- data.table(read_csv(here::here('data','econ','delivery_cost_samples.csv'), show_col_types=F))


#setting up the doses code

doses_calculator <- function(country_specs, delivery_cost_samples, 
                             country_of_interest, wastage, start_year_of_analysis,
                             cost_discount_rate_val, age_policy, pandemic_year, costs){
  doses_info <-  read_rds(here::here('data', 'Rearranged_doses', paste0('Rearranged_dose_for', country_of_interest, '.rds')))
  doses_info <- subset(doses_info, year_pandemic == pandemic_year)
  
  if (age_policy ==1){
    doses_info <- subset(doses_info, vacc_population == '0-4')
  } else if (age_policy == 2){
    doses_info <- subset(doses_info, vacc_population == "0-10")
  } else if (age_policy == 3){
    doses_info <- subset(doses_info, vacc_population == "0-17")
  } else if (age_policy == 4){
    doses_info <- subset(doses_info, vacc_population == "65-101")
  } else if (age_policy == 5){
    doses_info <- subset(doses_info, vacc_population == "0-17 & 65-101")
  }
  
  country_specs <- country_specs[country_specs$iso3c==country_of_interest, ]
  
  country_specs[, country_type := case_when(
    country_of_interest == 'USA' ~'USA',
    # adding in these so the code runs
    income_g == 'HIC' & !iso3c=='USA' ~ 'hics',
    income_g == 'UMIC' ~ 'umics',
    income_g %in% c('LMIC','LIC') & procure_mech == 'UNICEF' ~ 'lmic_un_proc',
    income_g %in% c('LMIC','LIC') & procure_mech == 'Self-procuring' ~ 'lmic_self_proc',
  )]
  
  if (country_of_interest %in% c('GUF', 'HKG', 'MAC', 'NCL','TWN')){
    selecting_pricing <- costs[costs$country_type=='hics',  ]
  }else if (country_of_interest %in% c('PRI')){
    selecting_pricing <- costs[costs$country_type=='USA',  ] 
  } else if (country_of_interest %in% c('PSE')){
    selecting_pricing <- costs[costs$country_type=='lmic_un_proc',  ]  
  } else if (country_of_interest %in% c('XKX')){
    selecting_pricing <- costs[costs$country_type=='umics',  ]  
    }
  else{
    selecting_pricing <- costs[costs$country_type==country_specs$country_type,  ]
  }
  
  
  selecting_pricing <- selecting_pricing %>% select(vacc_type, midpoint)
  
  doses <- doses_info %>% mutate(doses_wastage = ceiling(vaccs*(1+wastage)))
  
  doses <- merge(doses, selecting_pricing, by = c('vacc_type'))
  
  
  doses <- doses %>% mutate(dose_cost = doses_wastage*midpoint)
  
  delivery_cost <- delivery_cost_samples[iso3c %in% country_of_interest]
  
  merged_dataset <- merge(doses, delivery_cost[, .(simulation_index, delivery_cost)], by = c("simulation_index"), all.x = TRUE)
  
  merged_dataset <- merged_dataset %>% mutate(total_delivery_cost = doses_wastage*delivery_cost,
                                              total_cost = total_delivery_cost + dose_cost,
                                              discount_year = year - start_year_of_analysis,
                                              discount_rate = (1 + cost_discount_rate_val)^(-discount_year),
                                              discounted_doses_cost = total_cost*discount_rate)
  
  
  return(merged_dataset)
  
}


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

adding_in_discounting <- function(final_analysis_file, start_year_of_analysis, cost_discount_rate_val, DALY_discount_rate_val){
  final_analysis_file <- final_analysis_file %>% mutate(discount_year = year -start_year_of_analysis,
                                                        cost_discount_rate = (1 + cost_discount_rate_val)^(-discount_year),
                                                        DALY_discount_rate = (1 + DALY_discount_rate_val)^(-discount_year),
                                                        discounted_epi_costs = (total_hosp_cost + total_outp_cost)*cost_discount_rate,
                                                        discounted_DALYs_cost = cost_of_DALYs*DALY_discount_rate
                                                        )
  return(final_analysis_file)
  
}


### Overall analysis

Overall_economic_analysis <- function(seasonal_data_set, pandemic_data_set, symp_samples, global_ihrs,
                                      country_of_interest,
                                      national_ifrs, yll_df, year_of_interest, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                      cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                      cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                      doses_info, wastage, dose_price, case_proportion, age_policy, pandemic_year,
                                      LMICS_country, LMIC_boost){
  
  #calculating the DALYS for seasonal and pandemic
  
  seasonal_DALYs <- total_DALYS_for_seasonal(seasonal_data_set, symp_samples, global_ihrs, outpatient_ratios,
                                             country_of_interest,
                                             national_ifrs, yll_df, DALY_weight_samples)
  
  pandemic_DALYs <- total_DALYS_for_pandemic(pandemic_data_set, symp_samples, year_of_interest,hosp_ratio, 
                                             global_ihrs, outpatient_ratios,country_of_interest,
                                             DALY_weight_samples, pandemic_ifrs, yll_df, case_proportion, LMICS_country, LMIC_boost)
  
  pandemic_DALYs <- subset(pandemic_DALYs, select = c('simulation_index', 'vacc_type', 'year', 'age_grp', 'hospitalisations', 'outpatients', 'total_DALYS', 'infection_nonvac', 'infection_vac', 'deaths', 'mechanism'))
  
  seasonal_DALYs <- subset(seasonal_DALYs, select = c('simulation_index', 'vacc_type', 'year', 'age_grp', 'hospitalisations', 'outpatients', 'total_DALYS', 'infection_nonvac', 'infection_vac', 'deaths'))
  seasonal_DALYs1 <- seasonal_DALYs
  seasonal_DALYs1$mechanism <- 'sterilising'
  seasonal_DALYs2 <- seasonal_DALYs
  seasonal_DALYs2$mechanism <- 'disease mod'
  seasonal_DALYs3 <- seasonal_DALYs
  seasonal_DALYs3$mechanism <- 'infection period'
  seasonal_DALYs <- rbind(seasonal_DALYs1, seasonal_DALYs2, seasonal_DALYs3)
  
  merged_dataset <- rbind(seasonal_DALYs, pandemic_DALYs)
  
  merged_dataset <- merged_dataset %>%
    group_by(simulation_index, vacc_type, year, age_grp, mechanism) %>%
    summarise(
      total_infections = sum(infection_nonvac) + sum(infection_vac), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(deaths),
      .groups = 'drop'
    )
  
  healthcareananalysis <- healthcost_analysis(merged_dataset, cost_predic_c, country_of_interest)
  
  combined_analysis <- adding_in_WTP(healthcareananalysis, WTP_choice, wtp_thresh, country_of_interest, WTP_GDP_ratio)
  
  combined_analysis <- adding_in_discounting(combined_analysis, 2025, cost_discount_rate_val, DALY_discount_rate_val)
  
  combined_analysis <- combined_analysis %>%
    group_by(simulation_index, vacc_type, year, mechanism) %>%
    summarise(
      total_infections = sum(total_infections), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(total_deaths),
      total_epi_costs=sum(discounted_epi_costs),
      total_DALY_costs=sum(discounted_DALYs_cost),
      .groups = 'drop'
    )
  
  doses_calculated <- doses_calculator(country_specs, delivery_cost_samples, 
                                       country_of_interest, wastage, 2025,
                                       cost_discount_rate_val, age_policy, pandemic_year, costs)
  
  
  
  merged_dataset <- merge(combined_analysis, doses_calculated, by=c('simulation_index', 'vacc_type', 'year'))
  
  
  merged_dataset <- merged_dataset %>% mutate(combined_epi = total_epi_costs +  total_DALY_costs,
                                              total_cost = total_epi_costs +  total_DALY_costs + discounted_doses_cost)
  
  merged_dataset <- merged_dataset %>%
    group_by(simulation_index, vacc_type, mechanism) %>%
    summarise(
      total_infections = sum(total_infections), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(total_deaths),
      total_epi_costs=sum(total_epi_costs),
      total_DALY_costs=sum(total_DALY_costs),
      combined_epi=sum(combined_epi),
      total_cost=sum(total_cost),
      vaccs=sum(vaccs),
      .groups = 'drop'
    )
  
  merged_dataset <- merged_dataset %>%
    # Create a temporary dataset of just the comparator values
    left_join(
      merged_dataset %>%
        filter(vacc_type == 0 & mechanism == 'sterilising') %>%
        select(simulation_index, comparator_value = combined_epi),
      by = "simulation_index"
    )
  
  
  #for the whole period dividing the other costs by the number of doses
  
  merged_dataset <- merged_dataset %>% mutate(cost_by_dose = (comparator_value- combined_epi)/vaccs)
  
  
  
  return(merged_dataset)
  
}

Overall_economic_analysis_seasonal <-  function(seasonal_data_set, symp_samples, global_ihrs,
                                                country_of_interest,
                                                national_ifrs, yll_df, year_of_interest, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                                cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                                cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                                wastage, dose_price, age_policy, pandemic_year, costs){
  
  #calculating the DALYS for seasonal and pandemic
  
  seasonal_DALYs <- total_DALYS_for_seasonal(seasonal_data_set, symp_samples, global_ihrs, outpatient_ratios,
                                             country_of_interest,
                                             national_ifrs, yll_df, DALY_weight_samples)
  
  seasonal_DALYs <- seasonal_DALYs %>%
    group_by(simulation_index, vacc_type, year, age_grp) %>%
    summarise(
      total_infections = sum(infection_nonvac) + sum(infection_vac), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(deaths),
      .groups = 'drop'
    )
  
  healthcareananalysis <- healthcost_analysis(seasonal_DALYs, cost_predic_c, country_of_interest)
  
  combined_analysis <- adding_in_WTP(healthcareananalysis, WTP_choice, wtp_thresh, country_of_interest, WTP_GDP_ratio)
  
  combined_analysis <- adding_in_discounting(combined_analysis, 2025, cost_discount_rate_val, DALY_discount_rate_val)
  
  doses_calculated <- doses_calculator(country_specs, delivery_cost_samples, 
                                       country_of_interest, wastage, 2025,
                                       cost_discount_rate_val, age_policy, pandemic_year, costs)
  
  combined_analysis <- combined_analysis %>%
    group_by(simulation_index, vacc_type, year) %>%
    summarise(
      total_infections = sum(total_infections), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(total_deaths),
      total_epi_costs=sum(discounted_epi_costs),
      total_DALY_costs=sum(discounted_DALYs_cost),
      .groups = 'drop'
    )
  
  
  merged_dataset <- merge(combined_analysis, doses_calculated, by=c('simulation_index', 'vacc_type', 'year'))
  
  merged_dataset <- merged_dataset %>% mutate(combined_epi = total_epi_costs +  total_DALY_costs,
                                              total_cost = total_epi_costs +  total_DALY_costs + discounted_doses_cost)
  
  #summarising over age group
  merged_dataset <- merged_dataset %>%
    group_by(simulation_index, vacc_type) %>%
    summarise(
      total_infections = sum(total_infections), 
      hospitalisations = sum(hospitalisations),
      outpatients = sum(outpatients),
      total_DALYS = sum(total_DALYS),
      total_deaths = sum(total_deaths),
      total_epi_costs=sum(total_epi_costs),
      total_DALY_costs=sum(total_DALY_costs),
      combined_epi=sum(combined_epi),
      total_cost=sum(total_cost),
      vaccs=sum(vaccs),
      .groups = 'drop'
    )
  
  
  merged_dataset <- merged_dataset %>%
    # Create a temporary dataset of just the comparator values
    left_join(
      merged_dataset %>%
        filter(vacc_type == 0) %>%
        select(simulation_index, comparator_value = combined_epi),
      by = "simulation_index"
    )
  
  
  #for the whole period dividing the other costs by the number of doses
  
  merged_dataset <- merged_dataset %>% mutate(cost_by_dose = ( comparator_value- combined_epi)/vaccs)
  
  
  
  return(merged_dataset)
  
}

Pandemic_impact<- function(ITZregion, country_of_interest, age_testing_strategy, year_of_interest, years, pand_dt,
         symp_samples, global_ihrs,
         national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
         cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
         cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
         doses_info, wastage, dose_price, case_proportion, LMICS_country, LMIC_boost){
  
  pandemic_dataset <- pandemic_datasets(ITZregion, country_of_interest, age_testing_strategy, year_of_interest, years)
  seasonal_dataset_pan <- epidemic_datasets_combine(ITZregion, country_of_interest, age_testing_strategy, pand_dt[pand_dt$year_pandemic == years, ], FALSE)
  seasonal_dataset_only <- epidemic_datasets_combine(ITZregion, country_of_interest, age_testing_strategy, pand_dt[pand_dt$year_pandemic == years, ], TRUE)
  
  pandemic_plus <- Overall_economic_analysis(seasonal_dataset_pan, pandemic_dataset, symp_samples, global_ihrs,
                                             country_of_interest,
                                             national_ifrs, yll_df, year_of_interest, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                             cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                             cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                             doses_info, wastage, dose_price, case_proportion, age_testing_strategy,years, LMICS_country, LMIC_boost)
  
  seasonal_only <- Overall_economic_analysis_seasonal(seasonal_dataset_only, symp_samples, global_ihrs,
                                                      country_of_interest,
                                                      national_ifrs, yll_df, year_of_interest, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                                      cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                                      cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                                      wastage, dose_price, age_testing_strategy, years, costs)
  
  seasonal_only1 <- seasonal_only
  seasonal_only1$mechanism <- 'sterilising'
  seasonal_only2 <- seasonal_only
  seasonal_only2$mechanism <- 'disease mod'
  seasonal_only3 <- seasonal_only
  seasonal_only3$mechanism <- 'infection period'
  seasonal_only <- rbind(seasonal_only1, seasonal_only2, seasonal_only3)
  
  merged_dataset <- merge(pandemic_plus, seasonal_only, by=c('simulation_index', 'vacc_type', 'mechanism'))
  merged_dataset <- as.data.table(merged_dataset)
  
  merged_dataset <- merged_dataset[!(mechanism %in% c("disease mod", "infection period") & vacc_type == '0')]
  
  return(merged_dataset)
  
}


