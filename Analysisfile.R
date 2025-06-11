

test_only <- list(infs_rds_list[[1]][[3]], infs_rds_list[[2]][[3]], infs_rds_list[[3]][[3]], infs_rds_list[[4]][[3]], infs_rds_list[[5]][[3]], infs_rds_list[[6]][[3]])
test5 <- rbindlist(test_only )




reconstructing_dataset <- function(final_dataset1,final_dataset2, final_dataset3, pandemic_date ){

  restricted_dataset1 <- final_dataset1[final_dataset1$time < pandemic_date %m+% months(6)]
  restricted_dataset1 <- restricted_dataset1[restricted_dataset1$time_epidemic < pandemic_date %m-% months(2)]
  
  restricted_dataset2 <- final_dataset2[final_dataset2$time < pandemic_date %m+% months(6)]
  restricted_dataset2 <- restricted_dataset1[restricted_dataset2$time_epidemic < pandemic_date %m-% months(2)]
  
  restricted_dataset3 <- final_dataset3[final_dataset3$time < pandemic_date %m+% months(6)]
  restricted_dataset3 <- restricted_dataset1[restricted_dataset3$time_epidemic < pandemic_date %m-% months(2)]
  
  summary_dataset1 <- restricted_dataset1 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  summary_dataset2 <- restricted_dataset2 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  summary_dataset3 <- restricted_dataset3 %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  combined_dataset <- rbind(summary_dataset1, summary_dataset2, summary_dataset3)
  
  combined_dataset <- combined_dataset %>%
    group(time, vacc_type, simulation_index) %>%
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
              .groups='drop')
  
  
  return(combined_dataset)
  
  
  
}

##### TESTER

pandemic_date <- as.Date('2027-05-06')
test1 <- readRDS(here::here('outputs','data','Africa(0-33)BDI1.rds'))


restricted_dataset1 <- test1[test1$time < pandemic_date %m+% months(6)]
rm(test1)
restricted_dataset1 <- restricted_dataset1[restricted_dataset1$time_epidemic < pandemic_date %m-% months(2)]

summary_dataset1 <- restricted_dataset1 %>%
  group_by(time, vacc_type, simulation_index) %>%
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
            .groups='drop')

test2 <- readRDS(here::here('outputs','data','Africa(34-66)BDI1.rds'))


restricted_dataset2 <- test2[test2$time < pandemic_date %m+% months(6)]
rm(test2)
restricted_dataset2 <- restricted_dataset2[restricted_dataset2$time_epidemic < pandemic_date %m-% months(2)]

summary_dataset2 <- restricted_dataset2 %>%
  group_by(time, vacc_type, simulation_index) %>%
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
            .groups='drop')

test3 <- readRDS(here::here('outputs','data','Africa(67-100)BDI1.rds'))

restricted_dataset3 <- test3[test3$time < pandemic_date %m+% months(6)]
rm(test3)
restricted_dataset3 <- restricted_dataset3[restricted_dataset3$time_epidemic < pandemic_date %m-% months(2)]

summary_dataset3 <- restricted_dataset3 %>%
  group_by(time, vacc_type, simulation_index) %>%
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
            .groups='drop')

combined_dataset <- rbind(summary_dataset1, summary_dataset2, summary_dataset3)






