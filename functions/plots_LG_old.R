## plots - lucy

source(here::here('setup','aesthetics.R'))
suppressMessages(source(here::here('setup','packages.R')))

options(arrow.unsafe_metadata = TRUE)
options(scipen = 9999)

## NGIV IN ACTION
ngiv <- 'B.2'

## SET SENS. ANALYSIS VALUES
for(SA in 1:5){

cov <- c(50, 20, 70, 50, 50)[SA]
lmic_num <- c(1, 1, 1, 3, 1)[SA]
discount_num <- c(1, 1, 1, 1, 2)[SA]

read <- c(T,T,T,T,T)[SA]

SA_FOLDER <- paste0('cov_', cov, '_lmic_', lmic_num, '_discount_', discount_num)
SA_FILEPATH <- file.path('Graphs_included', SA_FOLDER)
if(!file.exists(SA_FILEPATH)){dir.create(SA_FILEPATH)}

cat('\nSensitivity analysis: ', sep = '')
if(cov == 50 & lmic_num == 1 & discount_num == 1){
  cat('Base\n')
}else{
  if(lmic_num == 1 & discount_num == 1){
    cat(cov, '% coverage\n', sep = '')
  }else{
    if(cov == 50 & lmic_num == 3 & discount_num == 1){
      cat('increased LMIC CFR\n')
    }else{
      if(cov == 50 & lmic_num == 1 & discount_num == 2){
        cat('no DALY discount\n')
      }else{
        stop('ERROR')
      }
    }
  }
}

## PANDEMIC MECHANISM
mech <- c('sterilising','disease mod','infection period')[1]
MECH_FILEPATH <- file.path(SA_FILEPATH, mech)
if(!file.exists(MECH_FILEPATH)){dir.create(MECH_FILEPATH)}

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

#### MAKING PLOTS ####

# .x suffix -> pandemics, .y suffix -> seasonal influenza

#### HEALTH PLOTS ####

# TODO FINALISE THIS... NEEDS MUCH IMPROVING

by_vec <- c('simulation_index','vacc_type','age_testing_strategy','WHO_region','pandemic','time_of_pandemic')
by_vec_no_vacc <- by_vec[!grepl('vacc_type', by_vec)]
by_vec_no_vacc_no_sim <- by_vec_no_vacc[!grepl('simulation_index', by_vec_no_vacc)]
output_vec <- c('total_infections.x','hospitalisations.x','total_deaths.x','total_cost.x','vaccs.x')
output_vec <- c(output_vec, gsub('.x','.y', output_vec))
both_vec <- c(by_vec, output_vec)
all_vec <- c(both_vec, 'mechanism')
by_vec_mech <- c(by_vec, 'mechanism')

outputs_agg <- outputs[, ..all_vec]
outputs_agg <- outputs_agg[, lapply(.SD, sum), by = by_vec_mech]

colnames(outputs_agg) <- c(by_vec_mech, gsub('.x','_pand', gsub('.y','_epid',output_vec)))

outputs_base <- outputs_agg[mechanism == 'sterilising' & vacc_type == '0']
outputs_ngiv <- outputs_agg[mechanism == mech & vacc_type == ngiv]

health_dat <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
  select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
  group_by(!!!syms(by_vec_no_vacc)) %>% 
  summarise(annual_infs_av_epid = (total_infections_epid_base - total_infections_epid_ngiv)/time_of_pandemic,
            annual_hosps_av_epid = (hospitalisations_epid_base - hospitalisations_epid_ngiv)/time_of_pandemic,
            annual_deaths_av_epid = (total_deaths_epid_base - total_deaths_epid_ngiv)/time_of_pandemic,
            addn_infs_av_pand = (total_infections_pand_base - total_infections_epid_base) - 
              (total_infections_pand_ngiv - total_infections_epid_ngiv),
            addn_hosps_av_pand = (hospitalisations_pand_base - hospitalisations_epid_base) - 
              (hospitalisations_pand_ngiv - hospitalisations_epid_ngiv),
            addn_deaths_av_pand = (total_deaths_pand_base - total_deaths_epid_base) - 
              (total_deaths_pand_ngiv - total_deaths_epid_ngiv))

health_cols_1 <- by_vec_no_vacc[!grepl('time_of', by_vec_no_vacc)]

health_dat <- health_dat %>% 
  group_by(!!!syms(health_cols_1)) %>% 
  summarise(annual_infs_av_epid = mean(annual_infs_av_epid)/1e6,
            annual_hosps_av_epid = mean(annual_hosps_av_epid)/1000,
            annual_deaths_av_epid = mean(annual_deaths_av_epid)/1000,
            addn_infs_av_pand = mean(addn_infs_av_pand)/1e6,
            addn_hosps_av_pand = mean(addn_hosps_av_pand)/1000,
            addn_deaths_av_pand = mean(addn_deaths_av_pand)/1000)

health_cols_2 <- health_cols_1[!grepl('simulation', health_cols_1)]

health_dat_meas <- dt_to_meas(health_dat, cols = health_cols_2, usingMean = T)

health_dat_meas_l <- health_dat_meas %>% 
  pivot_longer(!c(health_cols, 'measure')) %>% 
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

ggplot() +
  geom_bar(data = health_dat_meas_l %>% filter(measure=='mean'),
             aes(x = region_pand, y = value, fill = as.factor(pandemic), 
                 alpha = outbreak),
                 stat = 'identity', position = 'stack', col = 1) +
  geom_errorbar(data = health_dat_meas_l %>% filter(measure!='mean') %>% 
                  pivot_wider(names_from = outbreak, values_from = value) %>% 
                  group_by(age_testing_strategy, region_pand, pandemic, outcome) %>% 
                  mutate(epid = epid + pand) %>% 
                  pivot_longer(cols = c(epid, pand)) %>% 
                  rename(outbreak = name) %>% 
                  pivot_wider(names_from = measure, values_from = value),
                aes(x = region_pand, ymin = eti95L, ymax = eti95U, alpha = outbreak),
                width = 0.4, position = position_dodge(width = 0.2)) +
  facet_grid(outcome ~ age_testing_strategy, scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age_vacc,
                                 outcome = outcomes_labs)) +
  # scale_linetype_manual(values = c(2,1), labels = c('Seasonal epidemic','Pandemic')) + 
  scale_alpha_manual(values = c(0.5,1), labels = c('Seasonal epidemic','Pandemic')) + 
  scale_fill_manual(values = pandemic_colors, labels = paste0('Pandemic Scenario ', 1:3)) + 
  theme_bw() +
  scale_x_discrete(labels = rbind(rep('', 6), unname(who_region_labs), rep('', 6))) + 
  theme(text = element_text(size = 14),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  labs(fill = 'Pandemic', linetype = 'Outbreak', alpha = 'Outbreak', x = 'WHO Region', y = 'Average annual outcomes averted')

ggsave(here::here(MECH_FILEPATH,'outcomes_averted.png'),
       width = 18, height = 10)


#### ECONOMIC PLOTS ####

# by_vec <- c('simulation_index','country','vacc_type','age_testing_strategy','WHO_region','pandemic','time_of_pandemic')
# by_vec_no_vacc <- by_vec[!grepl('vacc_type', by_vec)]
# by_vec_no_vacc_no_sim <- by_vec_no_vacc[!grepl('simulation_index', by_vec_no_vacc)]
# output_vec <- c('total_infections.x','hospitalisations.x','total_deaths.x','total_cost.x','vaccs.x')
# both_vec <- c(by_vec, output_vec)
# all_vec <- c(both_vec, 'mechanism')
# by_vec_mech <- c(by_vec, 'mechanism')
# 
# outputs_agg <- outputs[, ..all_vec]
# outputs_agg <- outputs_agg[, lapply(.SD, sum), by = by_vec_mech]
# 
# colnames(outputs_agg) <- c(by_vec_mech, gsub('[.x]','',output_vec))
#          
# outputs_base <- outputs_agg[mechanism == 'sterilising' & vacc_type == '0']
# outputs_ngiv <- outputs_agg[mechanism == mech & vacc_type == ngiv]
# 
# comp_out <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
#   select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
#   mutate(infs_av = total_infections_base - total_infections_ngiv,
#          hosps_av = hospitalisations_base - hospitalisations_ngiv,
#          deaths_av = total_deaths_base - total_deaths_ngiv,
#          threshold_cost = (total_cost_base - total_cost_ngiv)/vaccs_ngiv)
# 
# comp_out_meas <- dt_to_meas(comp_out, by_vec_no_vacc_no_sim, usingMean = F)
# comp_out_meas_w <- comp_out_meas %>% 
#   select(!!!syms(by_vec_no_vacc_no_sim), threshold_cost, infs_av, hosps_av, deaths_av, measure) %>% 
#   pivot_wider(names_from = measure, values_from = c(threshold_cost, infs_av, hosps_av, deaths_av))


## comparison of threshold cost with/without pandemics ##

by_vec <- c('simulation_index','country','vacc_type','age_testing_strategy','WHO_region','pandemic','time_of_pandemic')
by_vec_no_vacc <- by_vec[!grepl('vacc_type', by_vec)]
by_vec_no_vacc_no_sim <- by_vec_no_vacc[!grepl('simulation_index', by_vec_no_vacc)]
output_vec <- c('total_infections.x','hospitalisations.x','total_deaths.x',
                'total_infections.y','hospitalisations.y','total_deaths.y',
                'total_cost.x','total_cost.y','vaccs.x')
both_vec <- c(by_vec, output_vec)
all_vec <- c(both_vec, 'mechanism')
by_vec_mech <- c(by_vec, 'mechanism')

outputs_agg <- outputs[, ..all_vec]
outputs_agg <- outputs_agg[, lapply(.SD, sum), by = by_vec_mech]

colnames(outputs_agg) <- c(by_vec_mech, gsub('x','_pand',gsub('y','_epid',gsub('[.]','',output_vec))))

outputs_base <- outputs_agg[mechanism == 'sterilising' & vacc_type == '0']
outputs_ngiv <- outputs_agg[mechanism == mech & vacc_type == ngiv]

comp_out <- outputs_ngiv %>% left_join(outputs_base, by = by_vec_no_vacc, suffix = c('_ngiv','_base')) %>% 
  select(!c(vacc_type_ngiv, vacc_type_base, mechanism_ngiv, mechanism_base)) %>% 
  mutate(threshold_cost_pand = (total_cost_pand_base - total_cost_pand_ngiv)/vaccs_pand_ngiv,
         threshold_cost_epid = (total_cost_epid_base - total_cost_epid_ngiv)/vaccs_pand_ngiv)

comp_out_meas <- dt_to_meas(comp_out, by_vec_no_vacc_no_sim, usingMean = F)
comp_out_meas_w <- comp_out_meas %>% 
  select(!!!syms(by_vec_no_vacc_no_sim), threshold_cost_pand, threshold_cost_epid, measure) %>% 
  pivot_wider(names_from = measure, values_from = c(threshold_cost_pand, threshold_cost_epid))

years_vec <- c(2030,2040,2050)  # c(2030,2035,2040,2045,2050)
years_vec_labels <- c()
for(i in years_vec){ years_vec_labels <- c(years_vec_labels, i, '') }

## print test:
cat('TEST: GBR ', unname(unlist(comp_out_meas_w %>% 
                      filter(country == 'GBR', time_of_pandemic == 10, age_testing_strategy == 2, pandemic == 1957) %>% 
                      select(threshold_cost_pand_median))), sep = '')

comp_out_meas_w %>% 
  mutate(year_of_pandemic = 2025 + time_of_pandemic + 1,
         WHO_year = paste0(WHO_region,'_',year_of_pandemic)) %>% 
  filter(year_of_pandemic %in% years_vec) %>% 
  select(!c(time_of_pandemic, year_of_pandemic)) %>% 
  pivot_longer(!c(WHO_region, country, WHO_year, age_testing_strategy, pandemic)) %>% 
  mutate(is_pandemic = grepl('pand',name), name = gsub('pand_','',gsub('epid_','',name))) %>% 
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(WHO_year = case_when(is_pandemic ~ paste0(WHO_year,'_1'),
                              T ~ WHO_year)) %>% 
  ggplot() +
  geom_violin(aes(x = WHO_year, y = threshold_cost_median, fill = WHO_region, alpha = is_pandemic)) + 
  # geom_point(aes(x = WHO_year, y = threshold_cost_median, color = WHO_region)) + 
  facet_grid(pandemic ~ age_testing_strategy,
             scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  # scale_color_manual(values = WHO_colors, labels = who_region_labs) +
  scale_alpha_manual(values = c(0.4, 1), guide="none") + 
  scale_fill_manual(values = WHO_colors, labels = who_region_labs) +
  scale_x_discrete(labels = rep(years_vec_labels, 6)) + 
  theme_bw() +
  scale_y_log10(breaks = c(0.1,1,3,10,30,100,300,1000,3000,10000,30000), limits = c(NA, 30000)) +
  theme(text = element_text(size = 14),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1.3, hjust=0),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  labs(x = '', y = 'Economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region')

y_label <- function(x){
  out <- rep(0, length(x))
  for(i in 1:length(x)){
    if(x[i] >= 1){out[i] <- as.character(round(x[i]))}
    if(x[i] == 0.1){out[i] <- '0.10'} 
    if(x[i] == 0.01){out[i] <- '0.01'}
  }
  
  out
}

lower_cap <- 0.1
years_vec <- c(2030,2040,2050)  # c(2030,2035,2040,2045,2050)
years_vec_labels <- c()
for(i in years_vec){ years_vec_labels <- c(years_vec_labels, i, '') }

min_max_data <- comp_out_meas_w %>% 
  mutate(year_of_pandemic = 2025 + time_of_pandemic + 1,
         WHO_year = paste0(WHO_region,'_',year_of_pandemic)) %>% 
  filter(year_of_pandemic %in% years_vec) %>% 
  group_by(WHO_region, WHO_year, age_testing_strategy, pandemic) %>% 
  summarise(pand_med_med = median(threshold_cost_pand_median),
            pand_med_min = max(c(lower_cap, min(threshold_cost_pand_median))),
            pand_med_max = max(threshold_cost_pand_median),
            epid_med_med = median(threshold_cost_epid_median),
            epid_med_min = max(c(lower_cap,min(threshold_cost_epid_median))),
            epid_med_max = max(threshold_cost_epid_median)) %>% 
  pivot_longer(!c(WHO_region, WHO_year, age_testing_strategy, pandemic)) %>% 
  mutate(is_pandemic = grepl('pand',name), name = gsub('pand_','',gsub('epid_','',name))) %>% 
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(WHO_year = case_when(is_pandemic ~ paste0(WHO_year,'_1'),
                              T ~ WHO_year))

violin_dat <- comp_out_meas_w %>% 
  mutate(year_of_pandemic = 2025 + time_of_pandemic + 1,
         WHO_year = paste0(WHO_region,'_',year_of_pandemic)) %>% 
  filter(year_of_pandemic %in% years_vec) %>% 
  select(WHO_region, WHO_year, age_testing_strategy, pandemic, country, 
         threshold_cost_pand_median, threshold_cost_epid_median) %>% 
  group_by(WHO_region, WHO_year, age_testing_strategy, pandemic) %>% 
  mutate(pand_med_med = median(threshold_cost_pand_median),
            pand_med_min = max(c(lower_cap, min(threshold_cost_pand_median))),
            pand_med_max = max(threshold_cost_pand_median),
            epid_med_med = median(threshold_cost_epid_median),
            epid_med_min = max(c(lower_cap,min(threshold_cost_epid_median))),
            epid_med_max = max(threshold_cost_epid_median)) %>% 
  pivot_longer(!c(WHO_region, WHO_year, age_testing_strategy, pandemic, country)) %>% 
  mutate(is_pandemic = grepl('pand',name), name = gsub('pand_','',gsub('epid_','',name))) %>% 
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(WHO_year = case_when(is_pandemic ~ paste0(WHO_year,'_1'),
                              T ~ WHO_year))

min_max_data %>% 
  ggplot() +
  geom_errorbar(aes(x = WHO_year, ymin = med_min, ymax = med_max, col = WHO_region, lty = is_pandemic), width = 0) + 
  geom_errorbar(data = min_max_data %>% filter(med_min == lower_cap),
                aes(x = WHO_year, ymin = med_max, ymax = med_max, col = WHO_region, lty = is_pandemic)) + 
  geom_errorbar(data = min_max_data %>% filter(med_min > lower_cap),
                aes(x = WHO_year, ymin = med_min, ymax = med_max, col = WHO_region, lty = is_pandemic)) + 
  geom_point(aes(x = WHO_year, y = med_med, col = WHO_region, shape = is_pandemic), size = 2) + 
  geom_point(data = min_max_data %>% filter(med_min == lower_cap),
             aes(x = WHO_year, y = med_min, col = WHO_region), 
             shape = "\u25BC", size = 2) + 
  geom_segment(data = min_max_data %>%
                 mutate(WHO_year = gsub('_1','',WHO_year)) %>% 
                 select(!c(med_min,med_max)) %>% 
                 pivot_wider(names_from = is_pandemic, values_from = med_med),
              aes(x = WHO_year, xend = paste0(WHO_year, '_1'),
                   y = `FALSE`, yend = `TRUE`, col = WHO_region), lty = 2) + 
  facet_grid(pandemic ~ age_testing_strategy,
             scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  scale_shape_manual(values = c(1,16), labels = c('Only seasonal influenza','And pandemic')) + 
  scale_linetype_manual(values = c(1,1), labels = c('Only seasonal influenza','And pandemic')) + 
  # scale_alpha_manual(values = c(0.5,1), labels = c('Only seasonal influenza','And pandemic')) + 
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  # scale_fill_manual(values = WHO_colors, labels = who_region_labs) +
  scale_x_discrete(labels = rep(years_vec_labels, 6)) + 
  theme_bw() +
  scale_y_log10(breaks = c(10^((-1):5)),
                labels = y_label(c(10^((-1):5))),
                limits = c(NA, 100000)) +
  theme(text = element_text(size = 14),
        axis.ticks.x=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1.3, hjust=0),
        # strip.text.x = element_text(size = 0),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  ggtitle('Ranges of national threshold prices, by year of pandemic occurrence and WHO region') + 
  labs(x = 'Year of pandemic occurrence', y = 'Economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region', alpha = '', shape = '', lty = '')

ggsave(here::here(MECH_FILEPATH,'thresh_price_diff_years.png'),
       width = 6*length(years_vec), height = 10)

violin_dat_less_than <- violin_dat %>% filter(threshold_cost_median <= 0.1)

violin_dat %>% 
  filter(threshold_cost_median > 0.1) %>% 
  ggplot() +
  geom_violin(aes(x = WHO_year, y = threshold_cost_median, fill = WHO_region, alpha = is_pandemic)) + 
  geom_point(aes(x = WHO_year, y = med_med, shape = is_pandemic), size = 2, col = 'black') + 
  geom_point(data = violin_dat_less_than,
             aes(x = WHO_year, y = 0.1, col = WHO_region), 
             size = 2, shape = "\u25BC") +
  geom_segment(data = violin_dat_less_than,
               aes(x = WHO_year, xend = WHO_year, y = 0.1, yend = 0.2, col = WHO_region)) +
  facet_grid(pandemic ~ age_testing_strategy,
             scales = 'free',
             labeller = labeller(age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  scale_shape_manual(values = c(1,16), labels = c('Only seasonal influenza','And pandemic')) + 
  # scale_linetype_manual(values = c(1,1), labels = c('Only seasonal influenza','And pandemic')) + 
  scale_alpha_manual(values = c(0.5,0.9), labels = c('Only seasonal influenza','And pandemic')) +
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  scale_fill_manual(values = WHO_colors, labels = who_region_labs) +
  scale_x_discrete(labels = rep(years_vec_labels, 6)) + 
  theme_bw() +
  scale_y_log10(breaks = c(10^((-1):5)),
                labels = y_label(c(10^((-1):5))),
                limits = c(NA, 100000)) +
  theme(text = element_text(size = 14),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1.3, hjust=0),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
  labs(x = 'Year of pandemic occurrence', y = 'Economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region', alpha = '', shape = '', lty = '',
       title = 'Ranges of national threshold prices, by year of pandemic occurrence and WHO region',
       subtitle = 'Arrows show if the range includes values less than $0.10')

ggsave(here::here(MECH_FILEPATH,'thresh_price_diff_years_violin.png'),
       width = 6*length(years_vec), height = 10)

## DIFFERENCE IN THRESHOLD PRICES

diff_dat <- comp_out %>% 
  mutate(diff_in_threshold_cost = threshold_cost_pand - threshold_cost_epid)

## add population sizes
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

diff_dat_pop <- diff_dat %>% 
  left_join(population_data, by = 'country') %>% 
  group_by(WHO_region, age_testing_strategy, pandemic, time_of_pandemic) %>% 
  mutate(regional_mean_tc = weighted.mean(x = diff_in_threshold_cost,
                                          w = population))

diff_dat_pop_meas <- dt_to_meas(diff_dat_pop, cols = c('WHO_region', 'country', 'age_testing_strategy', 'pandemic', 'time_of_pandemic'), usingMean = T)

## TODO What is going on in Europe?

thresh_plot <- diff_dat_pop_meas %>% 
  mutate(year_of_pandemic = 2027 + time_of_pandemic,
         pandemic = case_when(pandemic == '1918' ~ 'Pandemic Scenario 1',
                              pandemic == '1957' ~ 'Pandemic Scenario 2',
                              pandemic == '2009' ~ 'Pandemic Scenario 3'),
         age_testing_strategy = case_when(age_testing_strategy == '2' ~ '0-10',
                                          age_testing_strategy == '4' ~ '65+'),
         facet_pandemic = paste0(pandemic, ', ', age_testing_strategy)) %>% 
  filter(year_of_pandemic > 2028,
         diff_in_threshold_cost >= 0.01,
         measure == 'mean') %>% 
  ggplot() +
  # geom_jitter(aes(x = year_of_pandemic, y = diff_in_threshold_cost, col = WHO_region, group = country), 
  #            alpha = 0.2, shape = 4) +
  geom_line(aes(x = year_of_pandemic, y = diff_in_threshold_cost, col = WHO_region, group = country),
            lwd = 0.6, alpha = 0.2, lty = 1) +
  geom_line(aes(x = year_of_pandemic, y = regional_mean_tc, col = WHO_region, group = WHO_region), lwd = 1.5, alpha = 1) +
  facet_grid(facet_pandemic ~ WHO_region,
             scales = 'free',
             labeller = labeller(WHO_region = who_region_labs,
                                 age_testing_strategy = supp.labs.age,
                                 pandemic = supp.labs.pand)) +
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  theme_bw() +
  scale_y_log10(breaks = c(0.1,1,3,10,30,100,300,1000,3000,10000,30000,100000)) +
  theme(text = element_text(size = 14),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ggtitle('National and regional additional economic benefit, by year of pandemic occurrence') + 
  labs(x = 'Year of pandemic', y = 'Additional economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region'); thresh_plot

ggsave(here::here(MECH_FILEPATH,'annual_increased_benefit.png'),
       width = 20, height = 10)

  
diff_dat_meas <-  dt_to_meas(diff_dat, by_vec_no_vacc_no_sim, usingMean = F) 
 
trend_dat <- diff_dat_meas %>% 
  select(!!!syms(by_vec_no_vacc_no_sim), diff_in_threshold_cost, measure) %>% 
  pivot_wider(names_from = measure, values_from = diff_in_threshold_cost) %>% 
  mutate(year_of_pandemic = 2025 + time_of_pandemic + 1,
         WHO_year = paste0(WHO_region,'_',year_of_pandemic)) %>% 
  select(WHO_region, WHO_year, year_of_pandemic, age_testing_strategy, pandemic, country, median) %>% 
  mutate(pandemic = case_when(pandemic == '1918' ~ 'Pandemic Scenario 1',
                              pandemic == '1957' ~ 'Pandemic Scenario 2',
                              pandemic == '2009' ~ 'Pandemic Scenario 3'),
         age_testing_strategy = case_when(age_testing_strategy == '2' ~ '0-10',
                                          age_testing_strategy == '4' ~ '65+')) %>% 
  mutate(facet_pandemic = paste0(pandemic, ', ', age_testing_strategy))

library(mgcv)

fitted_trend_dat <- data.frame()

for(f_p in unique(trend_dat$facet_pandemic)){
  for(who_reg in unique(trend_dat$WHO_region)){
    
    dat1 <- trend_dat %>% 
      filter(facet_pandemic == f_p, WHO_region == who_reg)
    
    fitted <- gam(median ~ s(year_of_pandemic, k = 3), data = dat1)
    fitted_ci <- data.frame(predict(fitted, data.frame(year_of_pandemic = sort(unique(dat1$year_of_pandemic))),
                                    se.fit = TRUE))
    
    join_df <- data.frame(
      facet_pandemic = f_p,
      WHO_region = who_reg,
      year_of_pandemic = sort(unique(dat1$year_of_pandemic)),
      fit = fitted_ci$fit,
      fit_lower = fitted_ci$fit - 1.96 * fitted_ci$se.fit,
      fit_upper = fitted_ci$fit + 1.96 * fitted_ci$se.fit
    )
    
    fitted_trend_dat <- rbind(fitted_trend_dat, join_df)
    
  }
}

  
trend_dat %>% 
  left_join(fitted_trend_dat, 
            by = c('facet_pandemic', 'WHO_region', 'year_of_pandemic')) %>% 
  ggplot() + 
  geom_jitter(aes(x = year_of_pandemic, y = median, col = WHO_region),
              alpha = 0.3, shape = 4) +
  geom_ribbon(aes(x = year_of_pandemic, ymin = fit_lower, ymax = fit_upper, 
                  fill = WHO_region), alpha = 0.5) + 
  geom_line(aes(x = year_of_pandemic, y = fit, col = WHO_region), lwd = 0.8) + 
  scale_fill_manual(values = WHO_colors, labels = who_region_labs) + 
  scale_color_manual(values = WHO_colors, labels = who_region_labs) + 
  facet_wrap(facet_pandemic ~ .,
             nrow = 3,
             scales = 'free') +
  theme_bw() +
  labs(x = 'Year of pandemic occurrence', y = 'Increased economic benefit per dose, $2022', 
       col = 'WHO region', fill = 'WHO region', alpha = '', shape = '', lty = '',
       title = 'Increased economic benefit per dose, with pandemic compared to without, $2022',
       subtitle = 'Fitted trend, with 95% confidence interval') +
  theme(text = element_text(size = 14),
        # axis.ticks.x=element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = 1.3, hjust=0),
        strip.text = element_text(size = 13),
        strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) 

ggsave(here::here(MECH_FILEPATH,'thresh_price_diff_years_trend.png'),
       width = 15, height = 15)


## adding GDP per capita ($2022)

WTP_thresholds <- data.table(read_csv("data/econ/WTP_thresholds.csv", show_col_types = F)) %>% 
  rename(country = iso3c)

heat_dat <- trend_dat %>% 
  left_join(WTP_thresholds %>% select(country, gdpcap), by = 'country')

min_fill <- min(heat_dat$median)
max_fill <- max(heat_dat$median)

plot_tile <- function(X){
  
  heat_dat_who <- heat_dat %>% 
    filter(WHO_region == X) %>% 
    arrange(gdpcap)
  
  country_order <- unique(heat_dat_who$country)
  
  heat_dat_who$country <- factor(heat_dat_who$country,
                                 levels = country_order)
  
  plot_out <- heat_dat_who %>% 
    ggplot() + 
    geom_tile(aes(x = year_of_pandemic, y = country, fill = median)) + 
    theme_bw() + 
    facet_grid(WHO_region ~ facet_pandemic) + 
    scale_fill_viridis(trans = 'pseudo_log',
                       limits = c(min_fill, max_fill),
                       breaks = c(-1000,-100,-10,0,10,100,1000,10000,100000)) + 
    # scale_x_continuous(limits = c(min(heat_dat_who$year_of_pandemic),max(heat_dat_who$year_of_pandemic))) +
    theme(text = element_text(size = 14),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 13),
          strip.background = element_rect(color=NA, fill=NA, linewidth=0.5)) +
    labs(x = '', y = '', 
         fill = 'Economic benefit\nper dose', alpha = '', shape = '', lty = ''); plot_out
  
  if(X != top_plot_who){
    plot_out <- plot_out +
      theme(strip.text.x = element_blank())
  }
  
  if(X != bottom_plot_who){
    plot_out <- plot_out +
      theme(axis.text.x = element_blank())
  }
  
  if(X == bottom_plot_who){
    plot_out <- plot_out +
      labs(x = 'Year of pandemic occurrence')
  }
  
  plot_out
  
}

top_plot_who <- unique(heat_dat$WHO_region)[1]
bottom_plot_who <- unique(heat_dat$WHO_region)[6]

plots_tile <- map(
  .x = unique(heat_dat$WHO_region),
  .f = plot_tile
)

p <- patchwork::wrap_plots(plots_tile,
                      nrow = 6,
                      guides = 'collect')

p 

ggsave(here::here(MECH_FILEPATH,'thresh_price_diff_years_heatmap.png'),
       width = 18, height = 12)




}
