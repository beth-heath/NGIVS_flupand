## plots - lucy - final

source(here::here('setup','aesthetics.R'))
suppressMessages(source(here::here('setup','packages.R')))

options(arrow.unsafe_metadata = TRUE)
options(scipen = 9999)

## NGIV IN ACTION
ngiv <- 'B.2'

merged_SA_data <- data.frame()
test_dt_all <- data.frame()

## SET SENS. ANALYSIS VALUES
for(SA in 1:7){

cov <- c(50, 20, 70, 50, 50, 50, 50)[SA]
lmic_num <- c(1, 1, 1, 3, 1, 1, 1)[SA]
discount_num <- c(1, 1, 1, 1, 2, 1, 1)[SA]

read <- rep(T, 7)[SA]

SA_FOLDER <- paste0('cov_', cov, '_lmic_', lmic_num, '_discount_', discount_num)
SA_FILEPATH <- file.path('Graphs_included', SA_FOLDER)
if(!file.exists(SA_FILEPATH)){dir.create(SA_FILEPATH)}

## PANDEMIC MECHANISM
mech <- c(rep('sterilising', 5),'disease mod','infection period')[SA]
MECH_FILEPATH <- file.path(SA_FILEPATH, mech)
if(!file.exists(MECH_FILEPATH)){dir.create(MECH_FILEPATH)}

cat('\nSensitivity analysis: ', sep = '')
if(cov == 50 & lmic_num == 1 & discount_num == 1 & mech == 'sterilising'){
  cat('Base\n')
}else{
  if(lmic_num == 1 & discount_num == 1 & mech == 'sterilising'){
    cat(cov, '% coverage\n', sep = '')
  }else{
    if(cov == 50 & lmic_num == 3 & discount_num == 1){
      cat('increased LMIC CFR\n')
    }else{
      if(cov == 50 & lmic_num == 1 & discount_num == 2){
        cat('no DALY discount\n')
      }else{
        if(cov == 50 & mech != 'sterilising'){
          cat('vaccine mechanism: ', mech, sep = '')
        }else{
          stop('ERROR')
        }
      }
    }
  }
}

#loading in WHO data file for WHO region
WHO_region_file <- wtp_thresh <- data.table(read_csv(here::here('data/WHO_regions.csv'), show_col_type=F))
country_itzs_names <- data.table(read_csv(here::here('data','country_itzs_names.csv'), show_col_types=F))

###### MAKING PLOT DATA ######

if(!read){
  
  outputs <- data.table()
  
  for (i in 1:7){
    
    c_name <- c("Africa", "Asia-Europe", "Eastern and Southern Asia",
                "Europe", "Northern America", "Oceania-Melanesia-Polynesia",
                "Southern America")[i]
    
    country_codes <- unique(country_itzs_names[which(country_itzs_names$cluster_name == c_name), ]$codes) 
    country_codes_not_considered <-c('GUF', 'HKG', 'MAC', 'NCL', 'PRI', 'PSE', 'TWN', 'XKX')
    country_codes <- country_codes[!country_codes %in% country_codes_not_considered]
    
    for (country_of_interest in country_codes){
      overall_file <- suppressWarnings(read_parquet(here::here('Run_script', paste0('Overall_',cov), 
                                                               paste0('Overallfile', country_of_interest, 'LMICS',lmic_num,'discounting',discount_num,'.parquet'))))
      
      # overall_file %>% filter(vacc_type != '0') %>% group_by(mechanism, pandemic) %>% summarise(sum(total_infections.x), sum(total_infections.y)) # check what does what
      
      region_of_interest <- WHO_region_file[country_code == country_of_interest, ]$WHOREGION
      overall_file$WHO_region <- rep(region_of_interest, nrow(overall_file))
      
      overall_file <- overall_file %>% 
        filter(vacc_type %in% c('0',ngiv), 
               age_testing_strategy %in% c(2, 4))
      
      if(nrow(outputs) == 0){
        outputs <- overall_file
      }else{
        outputs <- rbind(outputs,
                         overall_file)
      }
      
      cat(country_of_interest, ', ', sep = '')
      
    }  
    
  }
  
  outputs <- outputs %>% 
    arrange(WHO_region, country, simulation_index, mechanism, vacc_type)
  
  outputs <- data.table(outputs)
  write_rds(outputs, here::here('Run_script',paste0('Overall_', cov), paste0('outputs_',SA_FOLDER,'.rds')))
  
}else{
  
  outputs <- readRDS(here::here('Run_script',paste0('Overall_', cov), paste0('outputs_',SA_FOLDER,'.rds')))
  
}

# .x suffix -> pandemics, .y suffix -> seasonal influenza

colnames(outputs) <- gsub('.x','_pand', gsub('.y','_epid',colnames(outputs), fixed = T), fixed = T)

#### HEALTH PLOT - FIGURE 1 ####

by_vec <- c('simulation_index','vacc_type','age_testing_strategy','WHO_region','pandemic','time_of_pandemic')
by_vec_no_vacc <- by_vec[!grepl('vacc_type', by_vec)]
by_vec_no_vacc_no_sim <- by_vec_no_vacc[!grepl('simulation_index', by_vec_no_vacc)]
by_vec_no_vacc_no_year <- by_vec_no_vacc[!grepl('time_of_pandemic', by_vec_no_vacc)]
output_vec <- c('total_infections_pand','hospitalisations_pand','total_deaths_pand','total_cost_pand','vaccs_pand')
output_vec <- c(output_vec, gsub('_pand','_epid', output_vec))
both_vec <- c(by_vec, output_vec)
all_vec <- c(both_vec, 'mechanism')
by_vec_mech <- c(by_vec, 'mechanism')

outputs_agg <- outputs[, ..all_vec]
outputs_agg <- outputs_agg[, lapply(.SD, sum), by = by_vec_mech]

outputs_base <- outputs_agg[mechanism == 'sterilising' & vacc_type == '0']
outputs_ngiv <- outputs_agg[mechanism == mech & vacc_type == ngiv]

health_dat_annual <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
  select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
  group_by(!!!syms(by_vec_no_vacc_no_year)) %>% 
  mutate(that_year_pand_base_i = total_infections_pand_base - lag(total_infections_epid_base, default = 0), # minus seasonal epidemics *up to the year before*
         that_year_pand_ngiv_i = total_infections_pand_ngiv - lag(total_infections_epid_ngiv, default = 0),
         that_year_pand_base_h = hospitalisations_pand_base - lag(hospitalisations_epid_base, default = 0),
         that_year_pand_ngiv_h = hospitalisations_pand_ngiv - lag(hospitalisations_epid_ngiv, default = 0),
         that_year_pand_base_d = total_deaths_pand_base - lag(total_deaths_epid_base, default = 0),
         that_year_pand_ngiv_d = total_deaths_pand_ngiv - lag(total_deaths_epid_ngiv, default = 0)) 

health_dat <- health_dat_annual %>% 
  group_by(!!!syms(by_vec_no_vacc)) %>% 
  summarise(annual_infs_av_epid = (total_infections_epid_base - total_infections_epid_ngiv)/time_of_pandemic,
            annual_hosps_av_epid = (hospitalisations_epid_base - hospitalisations_epid_ngiv)/time_of_pandemic,
            annual_deaths_av_epid = (total_deaths_epid_base - total_deaths_epid_ngiv)/time_of_pandemic,
            infs_av_pand = (that_year_pand_base_i - that_year_pand_ngiv_i),
            hosps_av_pand = (that_year_pand_base_h - that_year_pand_ngiv_h),
            deaths_av_pand = (that_year_pand_base_d - that_year_pand_ngiv_d))

health_dat_annual %>% 
  select(!!!syms(by_vec_no_vacc), contains('that_year')) %>% ungroup() %>% 
  select(!simulation_index) %>%
  group_by(age_testing_strategy, WHO_region, pandemic, time_of_pandemic) %>%
  summarise(that_year_pand_base_i = mean(that_year_pand_base_i),
            that_year_pand_ngiv_i = mean(that_year_pand_ngiv_i)) %>% 
  ggplot() + 
  geom_line(aes(x = time_of_pandemic, y = (that_year_pand_base_i - that_year_pand_ngiv_i)/that_year_pand_base_i, 
                col = as.factor(pandemic), 
                group = interaction(pandemic))) +
  facet_grid(age_testing_strategy ~ WHO_region, scales = 'free') + 
  ylim(c(0,NA)) + 
  theme_bw()

## check:
health_dat_annual %>%
  group_by(simulation_index, age_testing_strategy, pandemic, time_of_pandemic) %>% 
  summarise(that_year_pand_base_i = sum(that_year_pand_base_i),
            that_year_pand_ngiv_i = sum(that_year_pand_ngiv_i)) %>% 
  ggplot() + 
  geom_point(aes(time_of_pandemic, that_year_pand_base_i/8e9), col = 'red', alpha = 0.5) + 
  geom_point(aes(time_of_pandemic, that_year_pand_ngiv_i/8e9), col = 'blue', alpha = 0.5) + 
  facet_grid(age_testing_strategy ~ pandemic, scales = 'free') + ylim(c(0,NA))

health_dat %>%
  group_by(simulation_index, age_testing_strategy, pandemic, time_of_pandemic) %>% 
  summarise(infs_av_pand = sum(infs_av_pand),
            annual_infs_av_epid = sum(annual_infs_av_epid)) %>% 
  ggplot() + 
  geom_point(aes(time_of_pandemic, infs_av_pand/8e9), col = 'red') + 
  geom_point(aes(time_of_pandemic, annual_infs_av_epid/8e9), col = 'blue') + 
  facet_grid(age_testing_strategy ~ pandemic, scales = 'free') # + ylim(c(0,NA))

health_dat %>%
  group_by(simulation_index, age_testing_strategy, pandemic, time_of_pandemic) %>% 
  summarise(hosps_av_pand = sum(hosps_av_pand),
            annual_hosps_av_epid = sum(annual_hosps_av_epid)) %>% 
  ggplot() + 
  geom_point(aes(time_of_pandemic, hosps_av_pand/8e9), col = 'red') + 
  geom_point(aes(time_of_pandemic, annual_hosps_av_epid/8e9), col = 'blue') + 
  facet_grid(age_testing_strategy ~ pandemic, scales = 'free') #+ ylim(c(0,NA))

health_cols_1 <- by_vec_no_vacc[!grepl('time_of', by_vec_no_vacc)]

health_dat <- health_dat %>% 
  group_by(!!!syms(health_cols_1)) %>% 
  summarise(annual_infs_av_epid = mean(annual_infs_av_epid)/1e6,
            annual_hosps_av_epid = mean(annual_hosps_av_epid)/1000,
            annual_deaths_av_epid = mean(annual_deaths_av_epid)/1000,
            infs_av_pand = mean(infs_av_pand)/1e6,
            hosps_av_pand = mean(hosps_av_pand)/1000,
            deaths_av_pand = mean(deaths_av_pand)/1000)

# save global health outcomes averted
global_health_dat <- data.table(health_dat)
global_health_dat[, WHO_region := NULL]
global_health_dat <- global_health_dat[, lapply(.SD, sum), 
                                            by=c('simulation_index','age_testing_strategy','pandemic')]
global_health_dat_meas <- dt_to_meas(global_health_dat, cols = c('age_testing_strategy','pandemic'), usingMean = T)
write_csv(global_health_dat_meas,
          here::here(MECH_FILEPATH,'global_health_averted.csv'))

health_cols_2 <- health_cols_1[!grepl('simulation', health_cols_1)]

health_dat_meas <- dt_to_meas(health_dat, cols = health_cols_2, usingMean = T)

health_dat_meas_l <- health_dat_meas %>% 
  pivot_longer(!c(health_cols_2, 'measure')) %>% 
  mutate(outbreak = case_when(grepl('epid', name) ~ 'epid',
                              grepl('pand', name) ~ 'pand'),
         outcome = gsub('annual_|addn_|_av_|epid|pand', '', name)) %>%
  select(!name) 

for(who_reg in unique(health_dat_meas_l$WHO_region)){
  
  health_dat_meas_l <- health_dat_meas_l %>% 
    mutate(WHO_region = case_when(WHO_region==who_reg ~ who_region_labs[who_reg],
                                  T ~ WHO_region))
  
}

health_dat_meas_l <- health_dat_meas_l %>% 
  mutate(region_pand = paste0(WHO_region, '_', pandemic))

supp.labs.age_vacc <- paste0('Vaccinating: ', supp.labs.age)
names(supp.labs.age_vacc) <- names(supp.labs.age)

health_dat_meas_l$outcome <- factor(health_dat_meas_l$outcome, 
                                 levels = unique(health_dat_meas_l$outcome))
health_dat_meas_l$WHO_region <- factor(health_dat_meas_l$WHO_region, 
                                    levels = who_region_labs)

health_dat_meas_l_w_epid <- health_dat_meas_l %>% 
  filter(outbreak == 'pand') %>% 
  select(!c(outbreak, region_pand)) %>% 
  pivot_wider(names_from = pandemic, values_from = value) %>% 
  left_join(
  health_dat_meas_l %>% 
    filter(outbreak == 'epid', pandemic == '1918') %>% 
    rename(epid = value) %>% 
    select(age_testing_strategy, WHO_region, measure, outcome, epid),
  by = c('age_testing_strategy','WHO_region','measure','outcome')) %>% 
  pivot_longer(!c(age_testing_strategy, WHO_region, measure, outcome)) %>% 
  mutate(region_pand = paste0(WHO_region, '_', name)) %>% 
  arrange(region_pand) %>%
  mutate(region_pand_num = rep(1:24, each = 18))

pandemic_colors_w_epid <- c(pandemic_colors, 'orange')
names(pandemic_colors_w_epid)[4] <- 'epid'

who_region_labs_alphabetical <- who_region_labs[c(1,5,4,2,3,6)]
                            
ggplot() +
  geom_bar(data = health_dat_meas_l_w_epid %>% filter(measure == 'mean'),
           aes(x = region_pand_num, y = value, fill = name),
           stat = 'identity', position = 'dodge', col = 1, width=0.7) +
  geom_errorbar(data = health_dat_meas_l_w_epid %>% filter(measure!='mean') %>% 
                                    pivot_wider(names_from = measure, values_from = value),
                  aes(x = region_pand_num, ymin = eti95L, ymax = eti95U),
                width = 0.2) +
  geom_vline(data = health_dat_meas_l_w_epid %>% 
               filter(name == 'epid', WHO_region != unique(health_dat_meas_l_w_epid$WHO_region)[6]), 
             aes(xintercept = region_pand_num + 0.5), 
             lty = 2, alpha = 0.5) + 
  facet_grid(outcome ~ age_testing_strategy, scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age_vacc,
                                 outcome = outcomes_labs)) +
  scale_fill_manual(values = pandemic_colors_w_epid, labels = c(paste0('Pandemic Scenario ', 1:3), 'Seasonal influenza')) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.5, 24.5), breaks = 1:24,
                     labels = c(rbind(rep('', 6), rep('', 6), unname(who_region_labs_alphabetical), rep('', 6)))
                     ) +
  theme(text = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  labs(fill = '', x = '', y = 'Average annual outcomes averted')

ggsave(here::here(MECH_FILEPATH,'Fig_1.png'),
       width = 14, height = 14)

## save data

save_averted <- health_dat_meas_l_w_epid %>% 
  pivot_wider(names_from = measure) %>% 
  mutate(WHO_region = gsub('\n',' ', WHO_region),
         age_testing_strategy = case_when(
           age_testing_strategy == 2 ~ '0-10',
           age_testing_strategy == 4 ~ '65+'
         )) %>% 
  mutate(averted = paste0(round(mean), ' (', round(eti95L), ' - ', round(eti95U), ')')) %>% 
  select(!c(mean, eti95L, eti95U)) %>% 
  pivot_wider(names_from = outcome, values_from = averted)

write_csv(save_averted, here::here(MECH_FILEPATH,'averted.csv'))

# per dose provided  

doses_dt <- outputs_ngiv %>% 
  select(simulation_index, age_testing_strategy, WHO_region, pandemic, time_of_pandemic, vaccs_pand, vaccs_epid) %>% 
  group_by(age_testing_strategy, WHO_region, pandemic, time_of_pandemic) %>% 
  summarise(vaccs_pand = mean(vaccs_pand),
            vaccs_epid = mean(vaccs_epid)) %>% 
  group_by(age_testing_strategy, WHO_region, pandemic) %>% 
  summarise(vaccs_pand = mean(vaccs_pand/time_of_pandemic),
            vaccs_epid = mean(vaccs_epid/time_of_pandemic)) %>% 
  pivot_longer(c(vaccs_pand, vaccs_epid)) %>% ungroup() %>%  
  mutate(name = case_when(name == 'vaccs_pand' ~ paste0(pandemic),
                          name == 'vaccs_epid' ~ 'epid')) %>% 
  select(!pandemic) %>% unique() %>% rename(doses = value)

for(who_reg in unique(doses_dt$WHO_region)){
  
  doses_dt <- doses_dt %>% 
    mutate(WHO_region = case_when(WHO_region==who_reg ~ who_region_labs[who_reg],
                                  T ~ WHO_region))
  
}

health_dat_doses <- 
  health_dat_meas_l_w_epid %>% 
  left_join(doses_dt,
            by = c('age_testing_strategy', 'WHO_region', 'name')) %>% 
  mutate(value = case_when(outcome == 'infs' ~ value*1e6,
                           T ~ value*1e3))

outcomes_labs_pc <- c('Infections','Deaths','Hospitalisations')
names(outcomes_labs_pc) <- c('infs','deaths','hosps')

ggplot() +
  geom_bar(data = health_dat_doses %>% filter(measure == 'mean'),
           aes(x = region_pand_num, y = value/doses, fill = name),
           stat = 'identity', position = 'dodge', col = 1, width=0.7) +
  geom_errorbar(data = health_dat_doses %>% filter(measure!='mean') %>% 
                  pivot_wider(names_from = measure, values_from = value),
                aes(x = region_pand_num, ymin = eti95L/doses, ymax = eti95U/doses),
                width = 0.2) +
  geom_vline(data = health_dat_doses %>% 
               filter(name == 'epid', WHO_region != unique(health_dat_doses$WHO_region)[6]), 
             aes(xintercept = region_pand_num + 0.5), 
             lty = 2, alpha = 0.5) + 
  facet_grid(outcome ~ age_testing_strategy, scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age_vacc,
                                 outcome = outcomes_labs_pc)) +
  scale_fill_manual(values = pandemic_colors_w_epid, labels = c(paste0('Pandemic Scenario ', 1:3), 'Seasonal influenza')) + 
  theme_bw() +
  scale_x_continuous(limits = c(0.5, 24.5), breaks = 1:24, 
                     labels = c(rbind(rep('', 6), rep('', 6), unname(who_region_labs_alphabetical), rep('', 6)))
                     ) +
  theme(text = element_text(size = 14),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  labs(fill = '', x = '', y = 'Average annual outcomes averted per dose')

ggsave(here::here(MECH_FILEPATH,'Fig_1_by_dose.png'),
       width = 14, height = 14)

# save data

write_health_dat <- health_dat_meas_l_w_epid %>% 
  mutate(age_target = case_when(age_testing_strategy == 2 ~ '0-10',
                                          age_testing_strategy == 4 ~ '65+'),
         WHO_region = gsub('\n',' ', WHO_region),
         outcome = case_when(outcome == 'infs' ~ 'Infections (millions)',
                             outcome == 'hosps' ~ 'Hospitalisations (thousands)',
                             outcome == 'deaths' ~ 'Deaths (thousands)'),
         scenario = case_when(name == '1918' ~ 'Pandemic Scenario 1',
                              name == '1957' ~ 'Pandemic Scenario 2',
                              name == '2009' ~ 'Pandemic Scenario 3',
                              name == 'epid' ~ 'Seasonal influenza')) %>% 
  select(WHO_region, age_target, scenario, outcome, measure, value) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(averted = paste0(signif(mean, 4), ' (', signif(eti95L, 4), ' - ', signif(eti95U, 4), ')')) %>% 
  select(!c(mean, eti95L, eti95U)) %>% 
  pivot_wider(names_from = scenario, values_from = averted)

write_csv(write_health_dat, here::here(MECH_FILEPATH, 'health_data.csv'))

#### National threshold cost with/without pandemics ####

by_vec <- c('simulation_index','country','vacc_type','age_testing_strategy','WHO_region','pandemic','time_of_pandemic')
by_vec_no_vacc <- by_vec[!grepl('vacc_type', by_vec)]
by_vec_no_vacc_no_sim <- by_vec_no_vacc[!grepl('simulation_index', by_vec_no_vacc)]
output_vec <- c('total_cost_pand','total_cost_epid','vaccs_pand', 'vaccs_epid')
both_vec <- c(by_vec, output_vec)
all_vec <- c(both_vec, 'mechanism')
by_vec_mech <- c(by_vec, 'mechanism')

outputs_agg <- outputs[, ..all_vec]
outputs_agg <- outputs_agg[, lapply(.SD, sum), by = by_vec_mech]

outputs_base <- outputs_agg[mechanism == 'sterilising' & vacc_type == '0']
outputs_ngiv <- outputs_agg[mechanism == mech & vacc_type == ngiv]

# computing additional economic value per dose
comp_out <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
  select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
  mutate(threshold_cost_pand = (total_cost_pand_base - total_cost_pand_ngiv)/vaccs_pand_ngiv,
         threshold_cost_epid = (total_cost_epid_base - total_cost_epid_ngiv)/vaccs_pand_ngiv) %>% 
  select(!!!syms(by_vec_no_vacc), starts_with('threshold_')) %>% 
  mutate(diff_in_threshold_cost = threshold_cost_pand - threshold_cost_epid,
         year_of_pandemic = 2025 + time_of_pandemic + 1)

## add population sizes
{
pop_proj_WPP_data <- read_csv("data/pop_proj_WPP_data.csv", show_col_types = F)
population_data <- pop_proj_WPP_data %>% 
  filter(Year == 2025) %>% select(!c(Year, Type)) %>% 
  rename(country = name) %>% 
  pivot_longer(!country) %>% 
  mutate(value = 1000*value) %>% 
  group_by(country) %>% 
  summarise(population = sum(value))

library(countrycode)
population_data$country <- suppressWarnings(countrycode(
  population_data$country,
  origin = 'country.name',
  destination = 'iso3c'
))
population_data <- population_data %>% 
  mutate(country = case_when(is.na(country) ~ 'XKX',
                             T ~ country))
}

# add regional pop-weighted threshold_cost_diff
comp_out_pop <- comp_out %>% 
  left_join(population_data, by = 'country') %>% 
  group_by(simulation_index, WHO_region, age_testing_strategy, pandemic, time_of_pandemic) %>% 
  mutate(regional_mean_diff = weighted.mean(x = diff_in_threshold_cost,
                                          w = population),
         regional_mean_tc_pand = weighted.mean(x = threshold_cost_pand,
                                          w = population),
         regional_mean_tc_epid = weighted.mean(x = threshold_cost_epid,
                                               w = population)) %>% 
  select(!population) %>% 
  mutate(pandemic = case_when(pandemic == '1918' ~ 'Pandemic Scenario 1',
                              pandemic == '1957' ~ 'Pandemic Scenario 2',
                              pandemic == '2009' ~ 'Pandemic Scenario 3'),
         age_testing_strategy = case_when(age_testing_strategy == '2' ~ '0-10',
                                          age_testing_strategy == '4' ~ '65+'),
         facet_pandemic = paste0(pandemic, ',\n', age_testing_strategy))

comp_out_pop_meds <- comp_out_pop %>% 
  group_by(country, WHO_region, facet_pandemic, year_of_pandemic) %>% 
  summarise(threshold_cost_pand = mean(threshold_cost_pand),
            threshold_cost_epid = mean(threshold_cost_epid),
            diff_in_threshold_cost = mean(diff_in_threshold_cost),
            regional_mean_diff = mean(regional_mean_diff),
            regional_mean_tc_pand = mean(regional_mean_tc_pand),
            regional_mean_tc_epid = mean(regional_mean_tc_epid))

#### FIGURE 2 ####

comp_out_pop_meds %>% 
  filter(year_of_pandemic >= 2028) %>%
  mutate(diff_in_threshold_cost = case_when(diff_in_threshold_cost < 1 ~ 1, T ~ diff_in_threshold_cost),
         regional_mean_diff = case_when(regional_mean_diff < 1 ~ 1,T ~ regional_mean_diff)) %>%
  ggplot() +
  geom_line(aes(x = year_of_pandemic, y = diff_in_threshold_cost, col = WHO_region, group = country),
            lwd = 0.6, alpha = 0.2, lty = 1) +
  geom_line(aes(x = year_of_pandemic, y = regional_mean_diff, col = WHO_region, group = WHO_region), lwd = 1.5, alpha = 1) +
  facet_grid(facet_pandemic ~ WHO_region,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  theme_bw() +
  scale_y_log10(breaks = c(1,3,10,30,100,300,1000,3000,10000,30000,100000),
                labels = c('<$1',paste0('$',c(3,10,30,100,300,1000,3000,10000,30000,100000)))) +
  theme(text = element_text(size = 14),
        strip.text = element_text(size = 13),
        legend.position = 'none',
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  # ggtitle('National and regional mean additional economic benefit per dose, by year of pandemic occurrence') + 
  labs(x = 'Year of pandemic occurrence', 
       y = 'Difference in additional economic benefit per dose when a pandemic occurs, $2022', 
       col = 'WHO region', fill = 'WHO region')

ggsave(here::here(MECH_FILEPATH,'Fig_2.png'),
       width = 15, height = 14)

#### FIGURE 2b ####

comp_out_pop_meds %>% 
  filter(year_of_pandemic >= 2028) %>%
  mutate(threshold_cost_pand = case_when(threshold_cost_pand < 1 ~ 1, T ~ threshold_cost_pand),
         regional_mean_tc_pand = case_when(regional_mean_tc_pand < 1 ~ 1,T ~ regional_mean_tc_pand)) %>%
  ggplot() +
  geom_line(aes(x = year_of_pandemic, y = threshold_cost_pand, col = WHO_region, group = country),
            lwd = 0.6, alpha = 0.2, lty = 1) +
  geom_line(aes(x = year_of_pandemic, y = regional_mean_tc_pand, col = WHO_region, group = WHO_region), lwd = 1.5, alpha = 1) +
  facet_grid(facet_pandemic ~ WHO_region,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  theme_bw() +
  scale_y_log10(breaks = c(1,3,10,30,100,300,1000,3000,10000,30000,100000),
                labels = c('<$1',paste0('$',c(3,10,30,100,300,1000,3000,10000,30000,100000)))) +
  theme(text = element_text(size = 14),
        strip.text = element_text(size = 13),
        legend.position = 'none',
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  # ggtitle('National and regional mean additional economic benefit per dose, by year of pandemic occurrence') + 
  labs(x = 'Year of pandemic occurrence', 
       y = 'Economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region')

ggsave(here::here(MECH_FILEPATH,'Fig_2_b.png'),
       width = 15, height = 14)

## saving threshold costs
tc_comp_out <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
  select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
  mutate(threshold_cost_pand = (total_cost_pand_base - total_cost_pand_ngiv)/vaccs_pand_ngiv,
         threshold_cost_epid = (total_cost_epid_base - total_cost_epid_ngiv)/vaccs_pand_ngiv) 

## save test outputs
test_dt <- tc_comp_out %>% filter(simulation_index == 1, country %in% c('GBR','AGO'),
                                  pandemic == 1918, age_testing_strategy == 2) %>% 
  mutate(tc_diff = threshold_cost_pand - threshold_cost_epid)

#### MERGE WITH OTHER SENS ANALYSIS DATA ####

years_vec <- c(2030,2040,2050)  

mech_name <- if(mech=='sterilising'){'sterilising'}else{
  if(mech=='infection period'){'Infectious period-modifying'}else{
    if(mech=='disease mod'){'Disease-modifying'}
  }
}

sens_a_name <- if(cov == 50 & lmic_num == 1 & discount_num == 1 & mech == 'sterilising'){'50% coverage'}else{
  if(lmic_num == 1 & discount_num == 1 & mech == 'sterilising'){paste0(cov, '% coverage')}else{
    if(cov == 50 & lmic_num == 3 & discount_num == 1){paste0('Increased LMIC CFR')}else{
      if(cov == 50 & lmic_num == 1 & discount_num == 2){paste0('DALY 0% discount rate')}else{
        if(cov == 50 & mech != 'sterilising'){paste0(mech_name, ' vaccine mechanism')}}}}}

merged_SA_data <- rbind(
  merged_SA_data, 
  comp_out_pop_meds %>% filter(year_of_pandemic %in% years_vec) %>% 
    mutate(sens_a = sens_a_name)
)

test_dt_all <- rbind(test_dt_all, test_dt %>% mutate(sens_a = sens_a_name))

}

write_rds(merged_SA_data, here::here(paste0('merged_SA_data.rds')))
write_csv(test_dt_all, here::here(paste0('gbr_out.csv')))

#### FIGURE 3 ####

for(year_index in 1:length(years_vec)){

merged_SA_data_plot <- merged_SA_data %>% 
  filter(year_of_pandemic == years_vec[year_index]) %>% 
  mutate(pandemic_scenario = gsub("^(.*?),.*", "\\1", facet_pandemic),
         age_grp = gsub('.*,\\s*', '', facet_pandemic),
         ANALYSIS = paste0(age_grp, ', ', sens_a)) %>% 
  mutate(diff_in_threshold_cost = case_when(diff_in_threshold_cost < 1 ~ 1, T ~ diff_in_threshold_cost),
         regional_mean_diff = case_when(regional_mean_diff < 1 ~ 1,T ~ regional_mean_diff),
         threshold_cost_epid = case_when(threshold_cost_epid < 1 ~ 1,T ~ threshold_cost_epid),
         threshold_cost_pand = case_when(threshold_cost_pand < 1 ~ 1,T ~ threshold_cost_pand),
         regional_mean_tc_pand = case_when(regional_mean_tc_pand < 1 ~ 1,T ~ regional_mean_tc_pand),
         regional_mean_tc_epid = case_when(regional_mean_tc_epid < 1 ~ 1,T ~ regional_mean_tc_epid),)

SA_cols <- c('#54278f', '#756bb1', '#bcbddc',
             '#a63603', '#e6550d', '#fdbe85')

merged_SA_data_plot %>% 
  filter(grepl('cov', ANALYSIS)) %>%
  ggplot() +
  geom_violin(aes(x = diff_in_threshold_cost, y = ANALYSIS, fill = ANALYSIS, col = ANALYSIS), alpha = 0.6) +
  geom_point(aes(x = regional_mean_diff, y = ANALYSIS, col = ANALYSIS), size = 3) +
  facet_grid(WHO_region ~ pandemic_scenario,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  theme_bw() +
  scale_x_log10(breaks = c(1,3,10,30,100,300,1000,3000,10000,30000,100000),
                labels = c('<$1',paste0('$',c(3,10,30,100,300,1000,3000,10000,30000,100000)))) +
  scale_color_manual(values = SA_cols, guide = guide_legend(reverse = T)) + 
  scale_fill_manual(values = SA_cols, guide = guide_legend(reverse = T)) + 
  theme(text = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  # ggtitle(paste0('National and regional median additional economic benefit per dose, when a pandemic occurs in ', years_vec[year_index])) + 
  labs(x = 'Additional economic benefit per dose, $2022', y = '', col = '', fill = '')

ggsave(here::here('Graphs_included',paste0('Fig_3_', years_vec[year_index], '.png')),
       width = 20, height = 15)

merged_SA_data_plot %>% 
  filter(grepl('cov', ANALYSIS)) %>%
  ggplot() +
  geom_violin(aes(x = threshold_cost_pand, y = ANALYSIS, fill = ANALYSIS, col = ANALYSIS), alpha = 0.6) +
  geom_point(aes(x = regional_mean_tc_pand, y = ANALYSIS, col = ANALYSIS), size = 3) +
  facet_grid(WHO_region ~ pandemic_scenario,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  theme_bw() +
  scale_x_log10(breaks = c(1,3,10,30,100,300,1000,3000,10000,30000,100000),
                labels = c('<$1',paste0('$',c(3,10,30,100,300,1000,3000,10000,30000,100000)))) +
  scale_color_manual(values = SA_cols, guide = guide_legend(reverse = T)) + 
  scale_fill_manual(values = SA_cols, guide = guide_legend(reverse = T)) + 
  theme(text = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 16),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  # ggtitle(paste0('National and regional median additional economic benefit per dose, when a pandemic occurs in ', years_vec[year_index])) + 
  labs(x = 'Economic benefit per dose, $2022', y = '', col = '', fill = '')

ggsave(here::here('Graphs_included',paste0('Fig_3_pand_threshold_cost_', years_vec[year_index], '.png')),
       width = 20, height = 10)

SA_cols <- c('#005a32','#006837', '#31a354', '#addd8e', '#d9f0a3',
             '#034e7b','#045a8d', '#2b8cbe', '#a6bddb', '#d0d1e6')

supp_plot_dat <- merged_SA_data_plot %>% 
  filter(! grepl('20',ANALYSIS), ! grepl('70', ANALYSIS)) %>%
  mutate(ANALYSIS = gsub('50% coverage', 'Base', ANALYSIS, fixed = T)) %>% 
  arrange(ANALYSIS) %>% 
  group_by(WHO_region, ANALYSIS, pandemic_scenario) %>% 
  mutate(indices = 1:n())

levs <- unique(supp_plot_dat$ANALYSIS)
supp_plot_dat$ANALYSIS <- factor(supp_plot_dat$ANALYSIS,
                                 levels = rev(levs))

supp_plot_dat %>% 
  ggplot() +
  geom_violin(aes(x = diff_in_threshold_cost, y = ANALYSIS, fill = ANALYSIS, col = ANALYSIS), alpha = 0.6) +
  geom_point(data = supp_plot_dat %>% filter(indices == 1),
             aes(x = regional_mean_diff, y = ANALYSIS, col = ANALYSIS), size = 3) +
  geom_point(data = supp_plot_dat %>% filter(indices == 1),
             aes(x = regional_mean_diff, y = ANALYSIS), size = 3, alpha = 0.2) +
  facet_grid(WHO_region ~ pandemic_scenario,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  theme_bw() +
  scale_x_log10(breaks = c(1,3,10,30,100,300,1000,3000,10000,30000,100000),
                labels = c('<$1',paste0('$',c(3,10,30,100,300,1000,3000,10000,30000,100000)))) +
  scale_color_manual(values = SA_cols, guide = guide_legend(reverse = T)) + 
  scale_fill_manual(values = SA_cols, guide = guide_legend(reverse = T)) +
  theme(text = element_text(size = 13),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  # ggtitle(paste0('National and regional median additional economic benefit per dose, when a pandemic occurs in ', years_vec[year_index])) + 
  labs(x = 'Additional economic benefit per dose, $2022', y = '', col = '', fill = '')

ggsave(here::here('Graphs_included',paste0('Fig_3_', years_vec[year_index],'_SUPP.png')),
       width = 19, height = 14)

}


## testing various coverage assumptions

test_dt_all %>% 
  filter(grepl('cov', sens_a)) %>% 
  ggplot() +
  geom_line(aes(x = time_of_pandemic, y = total_cost_pand_ngiv, col = sens_a)) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_pand_base, col = sens_a), lty=2) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_epid_ngiv, col = sens_a), lty=3) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_epid_base, col = sens_a), lty=4) +
  facet_grid(country ~ ., scales = 'free') + 
  theme_bw() + ylim(c(0,NA))

test_dt_all %>% 
  filter(grepl('cov', sens_a)) %>% 
  ggplot() +
  geom_line(aes(x = time_of_pandemic, y = total_cost_pand_ngiv/vaccs_epid_base, col = sens_a)) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_pand_base/vaccs_epid_base, col = sens_a), lty=2) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_epid_ngiv/vaccs_epid_base, col = sens_a), lty=3) +
  geom_line(aes(x = time_of_pandemic, y = total_cost_epid_base/vaccs_epid_base, col = sens_a), lty=4) +
  facet_grid(country ~ ., scales = 'free') + 
  theme_bw() + ylim(c(0,NA))

test_dt_all %>% 
  filter(grepl('cov', sens_a)) %>% 
  ggplot() +
  geom_line(aes(x = time_of_pandemic, y = threshold_cost_pand, col = sens_a)) +
  geom_line(aes(x = time_of_pandemic, y = threshold_cost_epid, col = sens_a), lty=2) +
  geom_line(aes(x = time_of_pandemic, y = threshold_cost_pand - threshold_cost_epid, col = sens_a), lty=3) + 
  facet_grid(country ~ ., scales = 'free') + 
  theme_bw() + ylim(c(0,NA))


