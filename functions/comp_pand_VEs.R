## COMPARING HEALTH OUTCOMES AVERTED WHEN CHANGING PANDEMIC VEs ##

base_path <- file.path('Graphs_included', 'A.2', 'cov_50_lmic_1_discount_1', 'sterilising', 'global_health_averted.csv')
SA_75_path <- gsub('discount_1', 'discount_1_PANDVE_75', base_path)
SA_100_path <- gsub('discount_1', 'discount_1_PANDVE_100', base_path)

paths <- c(base_path, SA_75_path, SA_100_path)
analyses <- c('pandemic_base', 'pandemic_75_VE', 'pandemic_100_VE')

table <- data.table()

for(index in 1:3){
  
  path <- paths[index]
  
  table <- rbind(table, 
                 read_csv(path, show_col_types = F) %>% mutate(SA = analyses[index]))
  
}

table_edited <- table %>% 
  pivot_longer(!c(age_testing_strategy, pandemic, measure, SA)) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(neat = paste0(signif(mean, 2), ' (95% CI: ', signif(eti95L, 2), ', ', signif(eti95U, 2), ')')) %>% 
  select(!c(mean, eti95L, eti95U)) %>% 
  mutate(outcome = case_when(
    grepl('infs', name) ~ 'Infections averted (1000s)',
    grepl('hosps', name) ~ 'Hospitalisations averted (1000s)',
    grepl('deaths', name) ~ 'Deaths averted (1000s)'
  ), flu = case_when(
    grepl('epid', name) ~ 'seasonal',
    T ~ SA
  )) %>% select(!c(name, SA)) %>% unique() %>% 
  rename(age_targeting = age_testing_strategy) %>% 
  mutate(age_targeting = case_when(age_targeting == 2 ~ '0-10',
                                   age_targeting == 4 ~ '65+'),
         pandemic = case_when(
           pandemic == 1918 ~ 'Pandemic Scenario 1',
           pandemic == 1957 ~ 'Pandemic Scenario 2',
           pandemic == 2009 ~ 'Pandemic Scenario 3'
         )) %>% 
  pivot_wider(names_from = flu, values_from = neat) %>% 
  arrange(desc(outcome), pandemic)

write_csv(table_edited, file.path('Graphs_included', 'A.2', 'pand_VE_outcomes_comparison.csv'))

table %>% 
  pivot_longer(!c(age_testing_strategy, pandemic, measure, SA)) %>% 
  filter(measure == 'mean') %>% select(! measure) %>% 
  mutate(outcome = case_when(
    grepl('infs', name) ~ 'Infections averted (1000s)',
    grepl('hosps', name) ~ 'Hospitalisations averted (1000s)',
    grepl('deaths', name) ~ 'Deaths averted (1000s)'
  ), flu = case_when(
    grepl('epid', name) ~ 'seasonal',
    T ~ SA
  )) %>% select(!c(name, SA)) %>% unique() %>% 
  rename(age_targeting = age_testing_strategy) %>% 
  mutate(age_targeting = case_when(age_targeting == 2 ~ '0-10',
                                   age_targeting == 4 ~ '65+'),
         pandemic = case_when(
           pandemic == 1918 ~ 'Pandemic Scenario 1',
           pandemic == 1957 ~ 'Pandemic Scenario 2',
           pandemic == 2009 ~ 'Pandemic Scenario 3'
         )) %>% 
  ggplot() +
    geom_bar(aes(x = pandemic, y = value, fill = flu),
             position = 'dodge', stat = 'identity') + 
    facet_wrap(outcome ~ age_targeting, scales = 'free', nrow = 3) +
    theme_bw()












