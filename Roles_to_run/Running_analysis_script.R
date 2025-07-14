#### File to run the code for ITZ region 7 

#loading in files
source(here::here('setup','packages.R'))
source(here::here('functions','Economiceexample.R'))
source(here::here('functions','Analysisfile.R'))

#loading in parameter sets
ITZregion <- 7
load("1918_combined_set_NH.Rdata")
pand_dt <- pandemic_combined
case_proportion <- 0.669
hosp_ratio <- 5

c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[ITZregion]

country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 

for (country in country_codes){
  country_of_interest <- country
  print(country_of_interest)
  for (years in 1:28){
    
    infs_rds_list <- mclapply(1:15, flu_parallel_ITZ_epi, mc.cores=length(vacc_type_list))
    overall_dt2 <- rbindlist(infs_rds_list) %>% reduce_function() %>% arrow_table()
    rm(infs_rds_list)
    gc()
    write_parquet(overall_dt2, sink = here::here('Run_script','ITZzone7', paste0('SouthernAmerica',countries,age_groups,'Epidemic_2.parquet')), compression = "zstd")
    rm(overall_dt2)
    gc()
    
    
  }
  
  
}





country_of_interest <- 'SLV'
age_testing_strategy <- 5
year_of_interest <- 1918
years <- 3


pandemic_combined

trial1<- Pandemic_impact(ITZregion, country_of_interest, age_testing_strategy, year_of_interest, years, pand_dt,
                 symp_samples, global_ihrs,
                 national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                 cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                 cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                 doses_info, wastage, dose_price, case_proportion)

trial2 <- creating_data_for_bar_chart(ITZregion, country_of_interest, age_testing_strategy, year_of_interest , years, pand_dt,
                                       symp_samples, global_ihrs,
                                       national_ifrs, yll_df, hosp_ratio, outpatient_ratios, DALY_weight_samples, pandemic_ifrs,
                                       cost_predic_c,  WTP_choice, wtp_thresh, WTP_GDP_ratio,
                                       cost_discount_rate_val, DALY_discount_rate_val, country_specs, delivery_cost_samples,
                                       doses_info, wastage, dose_price, case_proportion)





