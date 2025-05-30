
set.seed(123)

simulations <-100
start_year_of_analysis <- 2025
years_of_analysis <- 30


simulations <-100

source(here::here('functions/fluparallelalteredITZ.R'))

#loading in the pandemic addition function sets 
source(here::here('functions/creating_pandemic_data.R'))

for (scns in 1:3){
  for (pand_time in 0:28){
  
  
  disease_scenarios <- c('1918', '1957', '2009')[scns]
  
  epidemic_data <- converting_epidemic_code(itz_input,years_of_analysis,simulations, ageing_date)
  epid_dt <<- selecting_pandemic_parameters(epidemic_data, pand_time, disease_scenarios, simulations)
  
  infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
  all_epid <- list(infs_rds_list[[1]][[1]], infs_rds_list[[2]][[1]], infs_rds_list[[3]][[1]], infs_rds_list[[4]][[1]], infs_rds_list[[5]][[1]], infs_rds_list[[6]][[1]])
  pandemic_only <- list(infs_rds_list[[1]][[2]], infs_rds_list[[2]][[2]], infs_rds_list[[3]][[2]], infs_rds_list[[4]][[2]], infs_rds_list[[5]][[2]], infs_rds_list[[6]][[2]])
  
  all_epid_dt <- rbindlist(all_epid)
  pandemic_dt <- rbindlist(pandemic_only)
  seasonal_dt <- all_epid_dt
  seasonal_dt[, 2:16] <- seasonal_dt[,2:16]- pandemic_dt[,2:16]
  
  pandemic_dt <- restrict_pandemictime(epid_dt, pandemic_dt, 6)
  seasonal_dt <- restrict_pandemictime(epid_dt, seasonal_dt, 6)
  
  print(pand_time)
  saveRDS(pandemic_dt, file = here::here(paste0('Pand_GBR',disease_scenarios,pand_time,'.rds')))
  saveRDS(seasonal_dt, file = here::here(paste0('Seasonal_GBR',disease_scenarios,pand_time,'.rds')))
  }
  
}


