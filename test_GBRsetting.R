#### TEST SCRIPT ####
#setting seed
set.seed(123)

setwd('/Users/lshbh6/Documents/GitHub/Pandemicscurrent')
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



simulations <-5

ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program

################################################
################################################
################################################

#### load flu functions ####
source(here::here('functions/fluparallelalteredITZ.R'))

#loading in the pandemic addition function sets 
source(here::here('functions/creating_pandemic_data.R'))


#load in the analysis files



#### read in test epidemics ####

c_number <- 4
c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
            "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
            "Southern America")[c_number]
itz_input <- c('GHA','TUR','CHN','GBR','CAN','AUS','ARG')[c_number]
hemisphere_input <- c('NH','NH','NH','NH','NH','SH','SH')[c_number]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])

country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 

# adding in the option to summarise over year 

year_summary <- c('TRUE', 'FALSE')[2]





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


seasonal_flu_included <- c('TRUE', 'FALSE')[1]
pandemic_flu_included <- c('TRUE', 'FALSE')[2]

pandemic_year_chosen <- 5

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

if (seasonal_flu_included == 'TRUE'){
  epidemic_data <- converting_epidemic_code(itz_input,years_of_analysis,simulations, ageing_date)
  epid_dt <<- selecting_pandemic_parameters(epidemic_data, pandemic_year_chosen, disease_scenarios)
} else{
  epid_dt <<- selecting_pandemic_parameters(NA, pandemic_year_chosen, disease_scenarios)
}


if (pandemic_flu_included == 'FALSE'){
  epid_dt<- epid_dt %>% drop_na(original_date)
}






vaccine_strategy_seasonal <- c('sterilising', 'disease mod', 'infection period')[1]

if (vaccine_strategy_seasonal == 'sterilising'){
  vacc_type_list <- vacc_type_list_sterilising
} else if (vaccine_strategy_seasonal == 'disease mod'){
  vacc_type_list <- vacc_type_list_dis_mod
} else if (vaccine_strategy_seasonal == 'infection period'){
  vacc_type_list <- vacc_type_list_reduced_infec
}

#think it makes most sense to add into function and it becomes part of the strategy at the date of pandemics inclusion
#

vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[1]

if (vaccine_strategy_pandemics == 'sterilising'){
  vacc_type_list_pand <- vacc_type_list_sterilising
} else if (vaccine_strategy_pandemics == 'disease mod'){
  vacc_type_list_pand <- vacc_type_list_dis_mod
} else if (vaccine_strategy_pandemics == 'infection period'){
  vacc_type_list_pand <- vacc_type_list_reduced_infec
}

iso3c_input <- 'GBR'
#trialling code
#infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=1)

infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
all_epid <- list(infs_rds_list[[1]][[1]], infs_rds_list[[2]][[1]], infs_rds_list[[3]][[1]], infs_rds_list[[4]][[1]], infs_rds_list[[5]][[1]])
pandemic_only <- list(infs_rds_list[[1]][[2]], infs_rds_list[[2]][[2]], infs_rds_list[[3]][[2]], infs_rds_list[[4]][[2]], infs_rds_list[[5]][[2]])

infs_dt <- rbindlist(all_epid)
infs_dt$tot <- rowSums(infs_dt[,2:5])

pand_dt <- rbindlist(pandemic_only)
pand_dt$tot <- rowSums(pand_dt[,2:5]) 


if (year_summary == 'TRUE'){
  infs_dt$year <-lubridate::year(infs_dt$time)
  inf_summary <- infs_dt %>%
    group_by(year, simulation_index, vacc_type) %>%
    summarise(tot_sum = sum(tot,na.rm=TRUE), .groups = 'drop')
}


#infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel, mc.cores=length(vacc_type_list))
#infs_dt <- rbindlist(infs_rds_list)

#### Analysis ####
#month_of_interest <- 6
#restricting_topandemictime(epid_dt, infs_dt, month_of_interest)
#four_months<- restricting_topandemictime(epid_dt, infs_dt, 4)
#six_months<- restricting_topandemictime(epid_dt, infs_dt, 6)
#twelve_months<- restricting_topandemictime(epid_dt, infs_dt, 12)


#### SAVE OUTPUTS ####

#saveRDS(infs_dt, file = here::here('outputs','data','GBR_tests',paste0('GBR',disease_scenarios,vaccine_strategy,'.rds')))
# infs_dt <- readRDS(here::here('outputs','data','epi','vacc_GBR.rds'))

#### PLOT OUTPUTS ####


infs_dt$tot <- rowSums(infs_dt[,2:5])

#looking at cumulative
#infs_out <- infs_dt[,c('vacc_type','tot','simulation_index')][, lapply(.SD, cumsum), by=c('vacc_type','simulation_index')]

#looking at just total
infs_out <- infs_dt[,c('vacc_type','tot','simulation_index')]

infs_out$time <- infs_dt$time

#restricting down to only time of year
infs_restrict<- infs_out %>% filter(time > as.Date(paste0('01-01-',start_year_of_analysis+pandemic_year_chosen),format='%d-%m-%Y'))
infs_restrict<- infs_restrict %>% filter(time < as.Date(paste0('01-01-',start_year_of_analysis+pandemic_year_chosen+1),format='%d-%m-%Y'))

#infs_restrict<- infs_out %>% filter(time < as.Date(paste0('01-01-',start_year_of_analysis+pandemic_year_chosen+2),format='%d-%m-%Y'))

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



infs_restrict %>% 
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

infs_restrict %>% 
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

