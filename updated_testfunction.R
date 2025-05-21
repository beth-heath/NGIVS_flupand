#### TEST SCRIPT ####
setwd('/Users/lshbh6/Documents/GitHub/next_gen_flu-coverage copy')
#### load relevant packages ####
library(here)
source(here::here('setup','packages.R'))

#### colour schemes etc. ####
# same as https://github.com/lucy-gf/flu_model_LG
source(here::here('setup','aesthetics.R'))

################################################
############## set key parameters ##############
################################################

model_age_groups <- c(0,5,18,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

### adding here the randomness of the dates

start_year_of_analysis <- 2025
years_of_analysis <- 30



simulations <-100
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

################################################
################################################
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))

#### read in test epidemics ####

c_number <- 2
c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[c_number]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[c_number]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[c_number]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])

country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 


#### choose vaccine variable ####
vaccine_variable <- c('doses','coverage')[2] # using MMGH doses or % coverage?

#### define coverage if using ####
if(vaccine_variable == 'coverage'){
  
  # define percentage coverage intended
  cov_val <- 0.5
  
  # define age groups targeted, e.g. here <10yos and 65+yos
  cov_ages <- c(0:10, 65:101)
  
  # what % coverage in each model age group?
  cov_vec <- coverage_vector(cov_ages, cov_val, model_age_groups)
  
}else{
  
  #### read in test doses ####
  doses <- data.table(read_csv(here::here('data','test_doses.csv'), show_col_types=F))
  
}

ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))
vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])

#adding in extra to epidemics
seasonal_flu_included <- c('TRUE', 'FALSE')[2]

if (seasonal_flu_included == 'TRUE'){
  epidemic_data <- converting_epidemic_code(itz_input,years_of_analysis,simulations, ageing_date)
  
  epid_dt <<- epidemic_data
} else{
  titles <- c('simulation_index', 'susceptibility', 'transmissibility', 'r0_to_scale', 
              'match', 'start_date_late','original_date', 'ageing_year_start',
              'epid_start_date', 'initial_infected', 'period_start_date', 'end_date', 'susceptibility_for_kids')
  epid_dt <<- data.frame(matrix(nrow=0, ncol=length(titles)))
  colnames(epid_dt) <- titles
}


#setting for the standard pandemics that there is no increase in sus boost for children


pandemic_year_chosen <- 12
#disease scenarios possible 
disease_scenarios <- c('1918', '1957', '2009')[1]
if (disease_scenarios == '1918'){
  susceptibility_range <- c(0.80, 0.9)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.9)
  r0 <- 1.8
} else if (disease_scenarios == '1957'){
  susceptibility_range <- c(0.50, 0.7)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.6,0.8)
  r0 <- 1.7
} else if (disease_scenarios == '2009'){
  susceptibility_range <- c(0.50, 0.7)
  trans_range <- c(0.07249, 0.09834)
  sus_boost_for_children <- c(0.8,0.95)
  r0 <- 1.4
}

epid_dt <- Pandemic_addition_function(epid_dt, simulations, pandemic_year_chosen, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                      start_year_of_analysis)

vaccine_strategy <- c('sterilising', 'disease mod', 'infection period')[3]

if (vaccine_strategy == 'sterilising'){
  vacc_type_list <- vacc_type_list_sterilising
} else if (vaccine_strategy == 'disease mod'){
  vacc_type_list <- vacc_type_list_dis_mod
} else if (vaccine_strategy == 'infection period'){
  vacc_type_list <- vacc_type_list_reduced_infec
}




iso3c_input <- 'AFG'
mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=1)






for (countries in 1:length(country_codes) ){
  iso3c_input <<- country_codes[countries]
  print(iso3c_input)
  infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
  results_list <- paste0('Results for', iso3c_input)
  assign(results_list, infs_rds_list)
  ### save outputs ####
  infs_dt <- rbindlist(infs_rds_list)
  saveRDS(infs_dt, file = here::here('outputs','data','epi', paste0(iso3c_input, 'results','.rds')) )
}






#infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel, mc.cores=length(vacc_type_list))
#infs_dt <- rbindlist(infs_rds_list)

#### SAVE OUTPUTS ####

saveRDS(infs_dt, file = here::here('outputs','data','epi',paste0('vacc_',itz_input,'.rds')))
# infs_dt <- readRDS(here::here('outputs','data','epi','vacc_GBR.rds'))

#### PLOT OUTPUTS ####


infs_dt$tot <- rowSums(infs_dt[,2:5])
infs_out <- infs_dt[,c('vacc_type','tot','simulation_index')][, lapply(.SD, cumsum), by=c('vacc_type','simulation_index')]
infs_out$time <- infs_dt$time

infs_out %>% 
  group_by(time,vacc_type) %>% 
  summarise(med = median(tot),
            eti95L = quantile(tot, 0.025),
            eti95U = quantile(tot, 0.975)) %>% 
  ggplot() +
  geom_ribbon(aes(time, ymin=eti95L/1e6, ymax=eti95U/1e6,
                  fill = vacc_type), alpha=0.4) +
  geom_line(aes(time, med/1e6, col=vacc_type),lwd=0.8) +
  theme_bw() + scale_color_manual(values = vtn_colors) +
  scale_fill_manual(values = vtn_colors) +
  ylab('Infections (millions)') +
  facet_wrap(vacc_type~.) + theme(legend.position = 'none')

infs_out %>% 
  group_by(time,vacc_type) %>% 
  summarise(med = median(tot),
            eti95L = quantile(tot, 0.025),
            eti95U = quantile(tot, 0.975)) %>% 
  ggplot() +
  geom_ribbon(aes(time, ymin=eti95L/1e6, ymax=eti95U/1e6,
                  fill = vacc_type), alpha=0.4) +
  geom_line(aes(time, med/1e6, col=vacc_type),lwd=0.8) +
  theme_bw() + scale_color_manual(values = vtn_colors) +
  scale_fill_manual(values = vtn_colors) +
  ylab('Infections (millions)') + theme(legend.position = 'none')


