options(scipen = 999)
##### 15 years ######


#### Australia convergence tests #####


AUS_dataset_1 <- combined_datasets(6, 'AUS', 1, 1957, 15, pand_dt) 


AUS_dataset_2 <- combined_datasets(6, 'AUS', 1, 1918, 15, pand_dt) 


AUS_dataset_3 <- combined_datasets(6, 'AUS', 1, 2009, 15, pand_dt) 

#restricting down to no testing

AUS_dataset_1 <- AUS_dataset_1[AUS_dataset_1$vacc_type == '0',]
AUS_dataset_2 <- AUS_dataset_2[AUS_dataset_2$vacc_type == '0',]
AUS_dataset_3 <- AUS_dataset_3[AUS_dataset_3$vacc_type == '0',]


AUS_dataset_1$simulation_index <- AUS_dataset_1$simulation_index %% 100 +1
AUS_dataset_2$simulation_index <- AUS_dataset_2$simulation_index %% 100 + 1
AUS_dataset_3$simulation_index <- AUS_dataset_3$simulation_index %% 100 + 1


AUS_dataset_1 <- AUS_dataset_1 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

AUS_dataset_2 <- AUS_dataset_2 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

AUS_dataset_3 <- AUS_dataset_3 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')



#### GBR convergence tests #####

GBR_dataset_1 <- combined_datasets(4, 'GBR', 1, 1957, 15, pand_dt) 


GBR_dataset_2 <- combined_datasets(4, 'GBR', 1, 1918, 15, pand_dt) 


GBR_dataset_3 <- combined_datasets(4, 'GBR', 1, 2009, 15, pand_dt) 

#restricting down to no testing

GBR_dataset_1 <- GBR_dataset_1[GBR_dataset_1$vacc_type == '0',]
GBR_dataset_2 <- GBR_dataset_2[GBR_dataset_2$vacc_type == '0',]
GBR_dataset_3 <- GBR_dataset_3[GBR_dataset_3$vacc_type == '0',]


GBR_dataset_1$simulation_index <- GBR_dataset_1$simulation_index %% 100 + 1
GBR_dataset_2$simulation_index <- GBR_dataset_2$simulation_index %% 100 + 1
GBR_dataset_3$simulation_index <- GBR_dataset_3$simulation_index %% 100 + 1


GBR_dataset_1 <- GBR_dataset_1 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

GBR_dataset_2 <- GBR_dataset_2 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

GBR_dataset_3 <- GBR_dataset_3 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')


#### CHN tests

CHN_dataset_1 <- combined_datasets(3, 'CHN', 1, 1957, 15, pand_dt) 


CHN_dataset_2 <- combined_datasets(3, 'CHN', 1, 1918, 15, pand_dt) 


CHN_dataset_3 <- combined_datasets(3, 'CHN', 1, 2009, 15, pand_dt) 

#restricting down to no testing

CHN_dataset_1 <- CHN_dataset_1[CHN_dataset_1$vacc_type == '0',]
CHN_dataset_2 <- CHN_dataset_2[CHN_dataset_2$vacc_type == '0',]
CHN_dataset_3 <- CHN_dataset_3[CHN_dataset_3$vacc_type == '0',]


CHN_dataset_1$simulation_index <- CHN_dataset_1$simulation_index %% 100 + 1
CHN_dataset_2$simulation_index <- CHN_dataset_2$simulation_index %% 100 + 1
CHN_dataset_3$simulation_index <- CHN_dataset_3$simulation_index %% 100 + 1


CHN_dataset_1 <- CHN_dataset_1 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

CHN_dataset_2 <- CHN_dataset_2 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

CHN_dataset_3 <- CHN_dataset_3 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')


####### 5 years #####


#### Australia convergence tests #####


AUS_dataset_1_5 <- combined_datasets(6, 'AUS', 1, 1957, 5, pand_dt) 


AUS_dataset_2_5 <- combined_datasets(6, 'AUS', 1, 1918, 5, pand_dt) 


AUS_dataset_3_5 <- combined_datasets(6, 'AUS', 1, 2009, 5, pand_dt) 

#restricting down to no testing

AUS_dataset_1_5 <- AUS_dataset_1_5[AUS_dataset_1_5$vacc_type == '0',]
AUS_dataset_2_5 <- AUS_dataset_2_5[AUS_dataset_2_5$vacc_type == '0',]
AUS_dataset_3_5 <- AUS_dataset_3_5[AUS_dataset_3_5$vacc_type == '0',]


AUS_dataset_1_5$simulation_index <- AUS_dataset_1_5$simulation_index %% 100 + 1
AUS_dataset_2_5$simulation_index <- AUS_dataset_2_5$simulation_index %% 100 + 1
AUS_dataset_3_5$simulation_index <- AUS_dataset_3_5$simulation_index %% 100 + 1


AUS_dataset_1_5 <- AUS_dataset_1_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

AUS_dataset_2_5 <- AUS_dataset_2_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

AUS_dataset_3_5 <- AUS_dataset_3_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')



#### GBR convergence tests #####

GBR_dataset_1_5 <- combined_datasets(4, 'GBR', 1, 1957, 5, pand_dt) 


GBR_dataset_2_5 <- combined_datasets(4, 'GBR', 1, 1918, 5, pand_dt) 


GBR_dataset_3_5 <- combined_datasets(4, 'GBR', 1, 2009, 5, pand_dt) 

#restricting down to no testing

GBR_dataset_1_5 <- GBR_dataset_1_5[GBR_dataset_1_5$vacc_type == '0',]
GBR_dataset_2_5 <- GBR_dataset_2_5[GBR_dataset_2_5$vacc_type == '0',]
GBR_dataset_3_5 <- GBR_dataset_3_5[GBR_dataset_3_5$vacc_type == '0',]


GBR_dataset_1_5$simulation_index <- GBR_dataset_1_5$simulation_index %% 100 + 1 
GBR_dataset_2_5$simulation_index <- GBR_dataset_2_5$simulation_index %% 100 + 1
GBR_dataset_3_5$simulation_index <- GBR_dataset_3_5$simulation_index %% 100 + 1


GBR_dataset_1_5 <- GBR_dataset_1_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

GBR_dataset_2_5 <- GBR_dataset_2_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

GBR_dataset_3_5 <- GBR_dataset_3_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')


#### CHN tests

CHN_dataset_1_5 <- combined_datasets(3, 'CHN', 1, 1957, 5, pand_dt) 


CHN_dataset_2_5 <- combined_datasets(3, 'CHN', 1, 1918, 5, pand_dt) 


CHN_dataset_3_5 <- combined_datasets(3, 'CHN', 1, 2009, 5, pand_dt) 

#restricting down to no testing

CHN_dataset_1_5 <- CHN_dataset_1_5[CHN_dataset_1_5$vacc_type == '0',]
CHN_dataset_2_5 <- CHN_dataset_2_5[CHN_dataset_2_5$vacc_type == '0',]
CHN_dataset_3_5 <- CHN_dataset_3_5[CHN_dataset_3_5$vacc_type == '0',]


CHN_dataset_1_5$simulation_index <- CHN_dataset_1_5$simulation_index %% 100 + 1
CHN_dataset_2_5$simulation_index <- CHN_dataset_2_5$simulation_index %% 100 + 1
CHN_dataset_3_5$simulation_index <- CHN_dataset_3_5$simulation_index %% 100 + 1


CHN_dataset_1_5 <- CHN_dataset_1_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

CHN_dataset_2_5 <- CHN_dataset_2_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')

CHN_dataset_3_5 <- CHN_dataset_3_5 %>%
  group_by(simulation_index) %>%
  summarise(I1 = sum(I1),
            I2 = sum(I2),
            I3=sum(I3),
            I4 = sum(I4),
            IU1 = sum(IU1),
            IU2 = sum(IU2),
            IU3 = sum(IU3),
            IU4 = sum(IU4),
            IV1 = sum(IV1),
            IV2 = sum(IV2),
            IV3 = sum(IV3),
            IV4 = sum(IV4),
            IVR1= sum(IVR1),
            IVR2 = sum(IVR2),
            IVR3 = sum(IVR3),
            IVR4 = sum(IVR4),
            tot=sum(tot),
            .groups='drop')


#### 15 year totals ####
extracting_total_AUS_dataset_1 <- AUS_dataset_1$tot
extracting_total_AUS_dataset_2 <- AUS_dataset_2$tot
extracting_total_AUS_dataset_3 <- AUS_dataset_3$tot
extracting_total_GBR_dataset_1 <- GBR_dataset_1$tot
extracting_total_GBR_dataset_2 <- GBR_dataset_2$tot
extracting_total_GBR_dataset_3 <- GBR_dataset_3$tot
extracting_total_CHN_dataset_1 <- CHN_dataset_1$tot
extracting_total_CHN_dataset_2 <- CHN_dataset_2$tot
extracting_total_CHN_dataset_3 <- CHN_dataset_3$tot

## creating the dataframe


year_15_database <- data.frame(
  simulation_run = rep(1:100,9),
  totals = c(AUS_dataset_1$tot/15, AUS_dataset_2$tot/15, AUS_dataset_3$tot/15,
              GBR_dataset_1$tot/15, GBR_dataset_2$tot/15, GBR_dataset_3$tot/15,
              CHN_dataset_1$tot/15, CHN_dataset_2$tot/15, CHN_dataset_3$tot/15),
  country = c(rep('AUS',300), rep('GBR', 300), rep('CHN',300)),
  year = c(rep('1957', 100), rep('1919', 100), rep('2009', 100),
           rep('1957', 100), rep('1919', 100), rep('2009', 100),
           rep('1957', 100), rep('1919', 100), rep('2009', 100))
)

#calculating runnning mean
year_15_database <- year_15_database %>%
  group_by(country, year) %>%
  arrange(simulation_run) %>%
  mutate(running_mean = cummean(totals)) %>%
  ungroup()

ggplot(year_15_database, aes(x = simulation_run)) +
  geom_point(aes(y = totals/1e6), color = "red", alpha = 0.6) +             # Points
  geom_line(aes(y = running_mean/1e6), color = "black", size = 1.2) +     # Running mean line
  facet_wrap(~country+year, scales = "free_y") +                           # One grid per group
  labs(
    title = "Convergence analysis for pandemic at year 15",
    x = "Simulation Index",
    y = "Mean annual infections (millions)"
  ) +
  theme_minimal()


#### 5 year totals ####
extracting_total_AUS_dataset_1_5 <- AUS_dataset_1_5$tot
extracting_total_AUS_dataset_2_5 <- AUS_dataset_2_5$tot
extracting_total_AUS_dataset_3_5 <- AUS_dataset_3_5$tot
extracting_total_GBR_dataset_1_5 <- GBR_dataset_1_5$tot
extracting_total_GBR_dataset_2_5 <- GBR_dataset_2_5$tot
extracting_total_GBR_dataset_3_5 <- GBR_dataset_3_5$tot
extracting_total_CHN_dataset_1_5 <- CHN_dataset_1_5$tot
extracting_total_CHN_dataset_2_5 <- CHN_dataset_2_5$tot
extracting_total_CHN_dataset_3_5 <- CHN_dataset_3_5$tot

## creating the dataframe


year_5_database <- data.frame(
  simulation_run = rep(1:100,9),
  totals = c(AUS_dataset_1_5$tot/5, AUS_dataset_2_5$tot/5, AUS_dataset_3_5$tot/5,
             GBR_dataset_1_5$tot/5, GBR_dataset_2_5$tot/5, GBR_dataset_3_5$tot/5,
             CHN_dataset_1_5$tot/5, CHN_dataset_2_5$tot/5, CHN_dataset_3_5$tot/5),
  country = c(rep('AUS',300), rep('GBR', 300), rep('CHN',300)),
  year = c(rep('1957', 100), rep('1919', 100), rep('2009', 100),
           rep('1957', 100), rep('1919', 100), rep('2009', 100),
           rep('1957', 100), rep('1919', 100), rep('2009', 100))
)

#calculating runnning mean
year_5_database <- year_5_database %>%
  group_by(country, year) %>%
  arrange(simulation_run) %>%
  mutate(running_mean = cummean(totals)) %>%
  ungroup()

ggplot(year_5_database, aes(x = simulation_run)) +
  geom_point(aes(y = totals/1e6), color = "red", alpha = 0.6) +             # Points
  geom_line(aes(y = running_mean/1e6), color = "black", size = 1.2) +     # Running mean line
  facet_wrap(~country+year, scales = "free_y") +                           # One grid per group
  labs(
    title = "Convergence analysis for pandemic at year 5",
    x = "Simulation Index",
    y = "Mean annual infections (millions)"
  ) +
  theme_minimal()

