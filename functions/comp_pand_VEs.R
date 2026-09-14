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
    grepl('infs', name) ~ 'infections',
    grepl('hosps', name) ~ 'hospitalisations',
    grepl('deaths', name) ~ 'deaths'
  ), flu = case_when(
    grepl('epid', name) ~ 'seasonal',
    T ~ SA
  )) %>% select(!c(name, SA)) %>% unique() %>% 
  pivot_wider(names_from = flu, values_from = neat) %>% 
  arrange(desc(outcome), pandemic)

write_csv(table_edited, file.path('Graphs_included', 'A.2', 'pand_VE_outcomes_comparison.csv'))








