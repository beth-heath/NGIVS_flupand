#### File to run the code for ITZ region  

#loading in region
args <- commandArgs(trailingOnly = TRUE)
ITZregion <- as.numeric(args[1])
LMIC_boost <- as.numeric(args[2])
DALY_discount <- as.numeric(args[3])
Coverage_chosen <- as.numeric(args[4])


LMIC_boost <- c(1,3)[LMIC_boost]
discount_SA <- c(0,1)[DALY_discount]

##### loading in these considered parameters ##### 
case_proportion <- 0.669
hosp_ratio <- 4


#overall parameters
model_age_groups <- c(0,5,18,65) #where the age cutoffs are
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99)) #names of the age-groups
start_year_of_analysis <- 2025 #the age the analysis starts
years_of_analysis <- 30 #studying for 30 years in keeping in Goodfellow et al paper
simulations <-100 #the number of simulations
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

#loading in files
source(here::here('setup','packages.R'))
source(here::here('functions','Ecomicexample.R'))
source(here::here('functions','Analysisfile.R'))
source(here::here('functions/fluparallelalteredITZ.R'))


#loading in example of the pandemic data to extract when the pandemics occur 
load("1918_combined_set_NH.Rdata")
pand_dt <- pandemic_example


c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[ITZregion]

country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 



for (country in country_codes){
  country_of_interest <- country
  print(country_of_interest)
  
  for (years in 1:28){
    years <- years 
    infs_rds_list <- mclapply(1:15, Analysis_file, mc.cores=15)
    print(years)
    if (years ==1 ){
      overall_file <- rbindlist(infs_rds_list)
    } else {
      overall_file <- rbind(overall_file, rbindlist(infs_rds_list))
    }
    rm(infs_rds_list )
    
    if (years ==28){
      overall_file <- arrow_table(overall_file)
      write_parquet(overall_file, sink = here::here('Run_script',paste0('Overall_', coverage_scenario), paste0('Overallfile',country_of_interest,'LMICS',LMIC_boost,'discounting', DALY_discount, '.parquet')), compression = "zstd")
      rm(overall_file)
      }
    
  }
}




