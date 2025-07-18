
#### Initial read stage ####

#start looking at the pandemic dataset


test1 <- pandemic_dataset %>%
  group_by(vacc_type,  simulation_index, mechanism) %>%
  summarise(tot=sum(tot),
            .groups='drop')

# look at the seasonal dataset with pandemics

test2 <- seasonal_dataset_pan %>%
  group_by(vacc_type,  simulation_index, time_epidemic) %>%
  summarise(tot=sum(tot),
            .groups='drop')

# look at the seasonal dataset without pandemics

test3 <- seasonal_dataset_only %>%
  group_by(vacc_type,  simulation_index, time_epidemic) %>%
  summarise(tot=sum(tot),
            .groups='drop')

##### Overall economic analysis ######

test4 <- pandemic_plus %>%
  group_by(vacc_type,  simulation_index, mechanism) %>%
  summarise(total_infections=sum(total_infections),
            .groups='drop')

test5 <- seasonal_only %>%
  group_by(vacc_type,  simulation_index) %>%
  summarise(total_infections=sum(total_infections),
            .groups='drop')

#Issue is croppping up in the overall economic analysis therefore need to 
#look more in detail at these

#check the DALY calculations

test6 <- pandemic_DALYs %>%
  group_by(vacc_type,  simulation_index, mechanism) %>%
  summarise(infection_nonvac=sum(infection_nonvac),
            .groups='drop')

#out of set by a factor of 1 - check the long form data

test7 <- long_form_data_pandemic(pandemic_dataset)

test8 <- test7 %>%
  group_by(vacc_type,  simulation_index, mechanism) %>%
  summarise(infection_nonvac=sum(infection_nonvac),
            .groups='drop')

### long factor is the same 



