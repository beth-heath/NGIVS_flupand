set.seed(123)

for (scenario in 1:3){
  for (vaccine in 1:3){
    
    disease_scenarios <- c('1918', '1957', '2009')[scenario]
    if (disease_scenarios == '1918'){
      susceptibility_range <- c(0.80, 0.9)
      trans_range <- c(0.07249, 0.09834)
      sus_boost_for_children <- c(0.8,0.9)
      r0 <- NA
    } else if (disease_scenarios == '1957'){
      susceptibility_range <- c(0.50, 0.7)
      trans_range <- c(0.07249, 0.09834)
      sus_boost_for_children <- c(0.6,0.8)
      r0 <- NA
    } else if (disease_scenarios == '2009'){
      susceptibility_range <- c(0.50, 0.7)
      trans_range <- c(0.07249, 0.09834)
      sus_boost_for_children <- c(0.8,0.95)
      r0 <- NA
    }
    
    vaccine_strategy_pandemics <- c('sterilising', 'disease mod', 'infection period')[vaccine]
    
    if (vaccine_strategy_pandemics == 'sterilising'){
      vacc_type_list_pand <- vacc_type_list_sterilising
    } else if (vaccine_strategy_pandemics == 'disease mod'){
      vacc_type_list_pand <- vacc_type_list_dis_mod
    } else if (vaccine_strategy_pandemics == 'infection period'){
      vacc_type_list_pand <- vacc_type_list_reduced_infec
    }
    
    #adding in the pandemic chosen 
    
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
    epid_dt <- Pandemic_addition_function(epid_dt, simulations, pandemic_year_chosen, susceptibility_range, trans_range, sus_boost_for_children, r0,
                                          start_year_of_analysis, years_of_analysis)
    
    
    #running the epidemic
    
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel_ITZ, mc.cores=length(vacc_type_list))
    infs_dt <- rbindlist(infs_rds_list)
    infs_dt$tot <- rowSums(infs_dt[,2:5])
  
    round_output <- test_func(epid_dt, infs_dt,4)
    round_output$mechanisms <- vaccine_strategy_pandemics
    round_output$pandemic <- disease_scenarios

    if (scenario ==1 & vaccine==1){
      results <- round_output
    } else{
      results <- rbind(results, round_output)
    }
    
  }
}


  
ggplot(results, aes(x = vacc_type, y = (tot_sum/1e6), fill = vacc_type)) +
geom_boxplot() +
facet_grid(mechanisms ~ pandemic) +
theme_bw() + 
scale_color_manual(values = vtn_colors) +  
scale_fill_manual(values = vtn_colors) +  
ylab('Infections (millions)') +
xlab('Vaccine type') +
theme(legend.position = 'none')


