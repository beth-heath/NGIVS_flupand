#### 1918 pandemic ####

#summary functions
# Define the range of years
years <- 0:27

# Loop through each year and process
Pand_GBR_1918_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR1918%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_pandemic(
    data, symp_samples, dcr_infr, '1918', 3, 
    global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
  )
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Pand_GBR_1918_list) <- paste0("Year_", years, "1918")

#### 1957  

Pand_GBR_1957_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR1957%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_pandemic(
    data, symp_samples, dcr_infr, '1957', 3, 
    global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
  )
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Pand_GBR_1957_list) <- paste0("Year_", years, "1957")

#### 2009 pandemic

Pand_GBR_2009_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR2009%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_pandemic(
    data, symp_samples, dcr_infr, '2009', 3, 
    global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
  )
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Pand_GBR_2009_list) <- paste0("Year_", years, "2009")


Seasonal_GBR_1918_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Seasonal_GBR1918%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_seasonal(data, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,'GBR',
                                              national_ifrs, yll_df)
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Seasonal_GBR_1918_list) <- paste0("Year_", years, "1918")


Seasonal_GBR_1957_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Seasonal_GBR1957%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_seasonal(data, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,'GBR',
                                              national_ifrs, yll_df)
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Seasonal_GBR_1957_list) <- paste0("Year_", years, "1957")

Seasonal_GBR_2009_list <- lapply(years, function(year) {
  # Construct the file path
  file_path <- sprintf('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Seasonal_GBR2009%d.rds', year)
  
  # Read the RDS file
  data <- readRDS(file_path)
  
  # Add DALYs
  data_with_DALYs <- total_DALYS_for_seasonal(data, symp_samples,dcr_infr, global_ihrs, outpatient_ratios,'GBR',
                                              national_ifrs, yll_df)
  
  # Summarise total_DALYs by simulation_index and vacc_type
  summarised <- data_with_DALYs[, .(
    total_DALYS = sum(total_DALYS)
  ), by = .(simulation_index, vacc_type)]
  
  # Split total_DALYs by vacc_type
  split(summarised$total_DALYS, summarised$vacc_type)
})

# Optionally: name the list by year
names(Seasonal_GBR_2009_list) <- paste0("Year_", years, "2009")


#### 1918 difference calculations ####

Difference_1918_total <- list()
Difference_1918_pandemic <- list()

Difference_1918_total$pan_year_0 <- Seasonal_GBR_1918_list$Year_01918$`0` + Pand_GBR_1918_list$Year_01918$`0` - (Seasonal_GBR_1918_list$Year_01918$C + Pand_GBR_1918_list$Year_01918$C)
Difference_1918_total$pan_year_1 <- Seasonal_GBR_1918_list$Year_11918$`0` + Pand_GBR_1918_list$Year_11918$`0` - (Seasonal_GBR_1918_list$Year_11918$C + Pand_GBR_1918_list$Year_11918$C)
Difference_1918_total$pan_year_2 <- Seasonal_GBR_1918_list$Year_21918$`0` + Pand_GBR_1918_list$Year_21918$`0` - (Seasonal_GBR_1918_list$Year_21918$C + Pand_GBR_1918_list$Year_21918$C)
Difference_1918_total$pan_year_3 <- Seasonal_GBR_1918_list$Year_31918$`0` + Pand_GBR_1918_list$Year_31918$`0` - (Seasonal_GBR_1918_list$Year_31918$C + Pand_GBR_1918_list$Year_31918$C)
Difference_1918_total$pan_year_4 <- Seasonal_GBR_1918_list$Year_41918$`0` + Pand_GBR_1918_list$Year_41918$`0` - (Seasonal_GBR_1918_list$Year_41918$C + Pand_GBR_1918_list$Year_41918$C)
Difference_1918_total$pan_year_5 <- Seasonal_GBR_1918_list$Year_51918$`0` + Pand_GBR_1918_list$Year_51918$`0` - (Seasonal_GBR_1918_list$Year_51918$C + Pand_GBR_1918_list$Year_51918$C)
Difference_1918_total$pan_year_6 <- Seasonal_GBR_1918_list$Year_61918$`0` + Pand_GBR_1918_list$Year_61918$`0` - (Seasonal_GBR_1918_list$Year_61918$C + Pand_GBR_1918_list$Year_61918$C)
Difference_1918_total$pan_year_7 <- Seasonal_GBR_1918_list$Year_71918$`0` + Pand_GBR_1918_list$Year_71918$`0` - (Seasonal_GBR_1918_list$Year_71918$C + Pand_GBR_1918_list$Year_71918$C)
Difference_1918_total$pan_year_8 <- Seasonal_GBR_1918_list$Year_81918$`0` + Pand_GBR_1918_list$Year_81918$`0` - (Seasonal_GBR_1918_list$Year_81918$C + Pand_GBR_1918_list$Year_81918$C)
Difference_1918_total$pan_year_9 <- Seasonal_GBR_1918_list$Year_91918$`0` + Pand_GBR_1918_list$Year_91918$`0` - (Seasonal_GBR_1918_list$Year_91918$C + Pand_GBR_1918_list$Year_91918$C)
Difference_1918_total$pan_year_10 <- Seasonal_GBR_1918_list$Year_101918$`0` + Pand_GBR_1918_list$Year_101918$`0` - (Seasonal_GBR_1918_list$Year_101918$C + Pand_GBR_1918_list$Year_101918$C)
Difference_1918_total$pan_year_11 <- Seasonal_GBR_1918_list$Year_111918$`0` + Pand_GBR_1918_list$Year_111918$`0` - (Seasonal_GBR_1918_list$Year_111918$C + Pand_GBR_1918_list$Year_111918$C)
Difference_1918_total$pan_year_12 <- Seasonal_GBR_1918_list$Year_121918$`0` + Pand_GBR_1918_list$Year_121918$`0` - (Seasonal_GBR_1918_list$Year_121918$C + Pand_GBR_1918_list$Year_121918$C)
Difference_1918_total$pan_year_13 <- Seasonal_GBR_1918_list$Year_131918$`0` + Pand_GBR_1918_list$Year_131918$`0` - (Seasonal_GBR_1918_list$Year_131918$C + Pand_GBR_1918_list$Year_131918$C)
Difference_1918_total$pan_year_14 <- Seasonal_GBR_1918_list$Year_141918$`0` + Pand_GBR_1918_list$Year_141918$`0` - (Seasonal_GBR_1918_list$Year_141918$C + Pand_GBR_1918_list$Year_141918$C)
Difference_1918_total$pan_year_15 <- Seasonal_GBR_1918_list$Year_151918$`0` + Pand_GBR_1918_list$Year_151918$`0` - (Seasonal_GBR_1918_list$Year_151918$C + Pand_GBR_1918_list$Year_151918$C)
Difference_1918_total$pan_year_16 <- Seasonal_GBR_1918_list$Year_161918$`0` + Pand_GBR_1918_list$Year_161918$`0` - (Seasonal_GBR_1918_list$Year_161918$C + Pand_GBR_1918_list$Year_161918$C)
Difference_1918_total$pan_year_17 <- Seasonal_GBR_1918_list$Year_171918$`0` + Pand_GBR_1918_list$Year_171918$`0` - (Seasonal_GBR_1918_list$Year_171918$C + Pand_GBR_1918_list$Year_171918$C)
Difference_1918_total$pan_year_18 <- Seasonal_GBR_1918_list$Year_181918$`0` + Pand_GBR_1918_list$Year_181918$`0` - (Seasonal_GBR_1918_list$Year_181918$C + Pand_GBR_1918_list$Year_181918$C)
Difference_1918_total$pan_year_19 <- Seasonal_GBR_1918_list$Year_191918$`0` + Pand_GBR_1918_list$Year_191918$`0` - (Seasonal_GBR_1918_list$Year_191918$C + Pand_GBR_1918_list$Year_191918$C)
Difference_1918_total$pan_year_20 <- Seasonal_GBR_1918_list$Year_201918$`0` + Pand_GBR_1918_list$Year_201918$`0` - (Seasonal_GBR_1918_list$Year_201918$C + Pand_GBR_1918_list$Year_201918$C)
Difference_1918_total$pan_year_21 <- Seasonal_GBR_1918_list$Year_211918$`0` + Pand_GBR_1918_list$Year_211918$`0` - (Seasonal_GBR_1918_list$Year_211918$C + Pand_GBR_1918_list$Year_211918$C)
Difference_1918_total$pan_year_22 <- Seasonal_GBR_1918_list$Year_221918$`0` + Pand_GBR_1918_list$Year_221918$`0` - (Seasonal_GBR_1918_list$Year_221918$C + Pand_GBR_1918_list$Year_221918$C)
Difference_1918_total$pan_year_23 <- Seasonal_GBR_1918_list$Year_231918$`0` + Pand_GBR_1918_list$Year_231918$`0` - (Seasonal_GBR_1918_list$Year_231918$C + Pand_GBR_1918_list$Year_231918$C)
Difference_1918_total$pan_year_24 <- Seasonal_GBR_1918_list$Year_241918$`0` + Pand_GBR_1918_list$Year_241918$`0` - (Seasonal_GBR_1918_list$Year_241918$C + Pand_GBR_1918_list$Year_241918$C)
Difference_1918_total$pan_year_25 <- Seasonal_GBR_1918_list$Year_251918$`0` + Pand_GBR_1918_list$Year_251918$`0` - (Seasonal_GBR_1918_list$Year_251918$C + Pand_GBR_1918_list$Year_251918$C)
Difference_1918_total$pan_year_26 <- Seasonal_GBR_1918_list$Year_261918$`0` + Pand_GBR_1918_list$Year_261918$`0` - (Seasonal_GBR_1918_list$Year_261918$C + Pand_GBR_1918_list$Year_261918$C)
Difference_1918_total$pan_year_27 <- Seasonal_GBR_1918_list$Year_271918$`0` + Pand_GBR_1918_list$Year_271918$`0` - (Seasonal_GBR_1918_list$Year_271918$C + Pand_GBR_1918_list$Year_271918$C)

Difference_1918_pandemic$pan_year_0 <-Pand_GBR_1918_list$Year_01918$`0` - ( Pand_GBR_1918_list$Year_01918$C)
Difference_1918_pandemic$pan_year_1 <-  Pand_GBR_1918_list$Year_11918$`0` - ( Pand_GBR_1918_list$Year_11918$C)
Difference_1918_pandemic$pan_year_2 <-  Pand_GBR_1918_list$Year_21918$`0` - (Pand_GBR_1918_list$Year_21918$C)
Difference_1918_pandemic$pan_year_3 <-  Pand_GBR_1918_list$Year_31918$`0` - ( Pand_GBR_1918_list$Year_31918$C)
Difference_1918_pandemic$pan_year_4 <-  Pand_GBR_1918_list$Year_41918$`0` - ( Pand_GBR_1918_list$Year_41918$C)
Difference_1918_pandemic$pan_year_5 <-  Pand_GBR_1918_list$Year_51918$`0` - ( Pand_GBR_1918_list$Year_51918$C)
Difference_1918_pandemic$pan_year_6 <-  Pand_GBR_1918_list$Year_61918$`0` - ( Pand_GBR_1918_list$Year_61918$C)
Difference_1918_pandemic$pan_year_7 <-  Pand_GBR_1918_list$Year_71918$`0` - ( Pand_GBR_1918_list$Year_71918$C)
Difference_1918_pandemic$pan_year_8 <-  Pand_GBR_1918_list$Year_81918$`0` - ( Pand_GBR_1918_list$Year_81918$C)
Difference_1918_pandemic$pan_year_9 <-  Pand_GBR_1918_list$Year_91918$`0` - (Pand_GBR_1918_list$Year_91918$C)
Difference_1918_pandemic$pan_year_10 <-  Pand_GBR_1918_list$Year_101918$`0` - ( Pand_GBR_1918_list$Year_101918$C)
Difference_1918_pandemic$pan_year_11 <-  Pand_GBR_1918_list$Year_111918$`0` - ( Pand_GBR_1918_list$Year_111918$C)
Difference_1918_pandemic$pan_year_12 <-  Pand_GBR_1918_list$Year_121918$`0` - ( Pand_GBR_1918_list$Year_121918$C)
Difference_1918_pandemic$pan_year_13 <-  Pand_GBR_1918_list$Year_131918$`0` - ( Pand_GBR_1918_list$Year_131918$C)
Difference_1918_pandemic$pan_year_14 <-  Pand_GBR_1918_list$Year_141918$`0` - ( Pand_GBR_1918_list$Year_141918$C)
Difference_1918_pandemic$pan_year_15 <-  Pand_GBR_1918_list$Year_151918$`0` - ( Pand_GBR_1918_list$Year_151918$C)
Difference_1918_pandemic$pan_year_16 <-  Pand_GBR_1918_list$Year_161918$`0` - ( Pand_GBR_1918_list$Year_161918$C)
Difference_1918_pandemic$pan_year_17 <-  Pand_GBR_1918_list$Year_171918$`0` - ( Pand_GBR_1918_list$Year_171918$C)
Difference_1918_pandemic$pan_year_18 <-  Pand_GBR_1918_list$Year_181918$`0` - ( Pand_GBR_1918_list$Year_181918$C)
Difference_1918_pandemic$pan_year_19 <-  Pand_GBR_1918_list$Year_191918$`0` - ( Pand_GBR_1918_list$Year_191918$C)
Difference_1918_pandemic$pan_year_20 <-  Pand_GBR_1918_list$Year_201918$`0` - ( Pand_GBR_1918_list$Year_201918$C)
Difference_1918_pandemic$pan_year_21 <-  Pand_GBR_1918_list$Year_211918$`0` - ( Pand_GBR_1918_list$Year_211918$C)
Difference_1918_pandemic$pan_year_22 <-  Pand_GBR_1918_list$Year_221918$`0` - ( Pand_GBR_1918_list$Year_221918$C)
Difference_1918_pandemic$pan_year_23 <-  Pand_GBR_1918_list$Year_231918$`0` - ( Pand_GBR_1918_list$Year_231918$C)
Difference_1918_pandemic$pan_year_24 <-  Pand_GBR_1918_list$Year_241918$`0` - ( Pand_GBR_1918_list$Year_241918$C)
Difference_1918_pandemic$pan_year_25 <-  Pand_GBR_1918_list$Year_251918$`0` - (Pand_GBR_1918_list$Year_251918$C)
Difference_1918_pandemic$pan_year_26 <-  Pand_GBR_1918_list$Year_261918$`0` - ( Pand_GBR_1918_list$Year_261918$C)
Difference_1918_pandemic$pan_year_27 <-  Pand_GBR_1918_list$Year_271918$`0` - ( Pand_GBR_1918_list$Year_271918$C)

#### 1957 difference calculations ####

Difference_1957_total <- list()
Difference_1957_pandemic <- list()

Difference_1957_total$pan_year_0 <- Seasonal_GBR_1957_list$Year_01957$`0` + Pand_GBR_1957_list$Year_01957$`0` - (Seasonal_GBR_1957_list$Year_01957$C + Pand_GBR_1957_list$Year_01957$C)
Difference_1957_total$pan_year_1 <- Seasonal_GBR_1957_list$Year_11957$`0` + Pand_GBR_1957_list$Year_11957$`0` - (Seasonal_GBR_1957_list$Year_11957$C + Pand_GBR_1957_list$Year_11957$C)
Difference_1957_total$pan_year_2 <- Seasonal_GBR_1957_list$Year_21957$`0` + Pand_GBR_1957_list$Year_21957$`0` - (Seasonal_GBR_1957_list$Year_21957$C + Pand_GBR_1957_list$Year_21957$C)
Difference_1957_total$pan_year_3 <- Seasonal_GBR_1957_list$Year_31957$`0` + Pand_GBR_1957_list$Year_31957$`0` - (Seasonal_GBR_1957_list$Year_31957$C + Pand_GBR_1957_list$Year_31957$C)
Difference_1957_total$pan_year_4 <- Seasonal_GBR_1957_list$Year_41957$`0` + Pand_GBR_1957_list$Year_41957$`0` - (Seasonal_GBR_1957_list$Year_41957$C + Pand_GBR_1957_list$Year_41957$C)
Difference_1957_total$pan_year_5 <- Seasonal_GBR_1957_list$Year_51957$`0` + Pand_GBR_1957_list$Year_51957$`0` - (Seasonal_GBR_1957_list$Year_51957$C + Pand_GBR_1957_list$Year_51957$C)
Difference_1957_total$pan_year_6 <- Seasonal_GBR_1957_list$Year_61957$`0` + Pand_GBR_1957_list$Year_61957$`0` - (Seasonal_GBR_1957_list$Year_61957$C + Pand_GBR_1957_list$Year_61957$C)
Difference_1957_total$pan_year_7 <- Seasonal_GBR_1957_list$Year_71957$`0` + Pand_GBR_1957_list$Year_71957$`0` - (Seasonal_GBR_1957_list$Year_71957$C + Pand_GBR_1957_list$Year_71957$C)
Difference_1957_total$pan_year_8 <- Seasonal_GBR_1957_list$Year_81957$`0` + Pand_GBR_1957_list$Year_81957$`0` - (Seasonal_GBR_1957_list$Year_81957$C + Pand_GBR_1957_list$Year_81957$C)
Difference_1957_total$pan_year_9 <- Seasonal_GBR_1957_list$Year_91957$`0` + Pand_GBR_1957_list$Year_91957$`0` - (Seasonal_GBR_1957_list$Year_91957$C + Pand_GBR_1957_list$Year_91957$C)
Difference_1957_total$pan_year_10 <- Seasonal_GBR_1957_list$Year_101957$`0` + Pand_GBR_1957_list$Year_101957$`0` - (Seasonal_GBR_1957_list$Year_101957$C + Pand_GBR_1957_list$Year_101957$C)
Difference_1957_total$pan_year_11 <- Seasonal_GBR_1957_list$Year_111957$`0` + Pand_GBR_1957_list$Year_111957$`0` - (Seasonal_GBR_1957_list$Year_111957$C + Pand_GBR_1957_list$Year_111957$C)
Difference_1957_total$pan_year_12 <- Seasonal_GBR_1957_list$Year_121957$`0` + Pand_GBR_1957_list$Year_121957$`0` - (Seasonal_GBR_1957_list$Year_121957$C + Pand_GBR_1957_list$Year_121957$C)
Difference_1957_total$pan_year_13 <- Seasonal_GBR_1957_list$Year_131957$`0` + Pand_GBR_1957_list$Year_131957$`0` - (Seasonal_GBR_1957_list$Year_131957$C + Pand_GBR_1957_list$Year_131957$C)
Difference_1957_total$pan_year_14 <- Seasonal_GBR_1957_list$Year_141957$`0` + Pand_GBR_1957_list$Year_141957$`0` - (Seasonal_GBR_1957_list$Year_141957$C + Pand_GBR_1957_list$Year_141957$C)
Difference_1957_total$pan_year_15 <- Seasonal_GBR_1957_list$Year_151957$`0` + Pand_GBR_1957_list$Year_151957$`0` - (Seasonal_GBR_1957_list$Year_151957$C + Pand_GBR_1957_list$Year_151957$C)
Difference_1957_total$pan_year_16 <- Seasonal_GBR_1957_list$Year_161957$`0` + Pand_GBR_1957_list$Year_161957$`0` - (Seasonal_GBR_1957_list$Year_161957$C + Pand_GBR_1957_list$Year_161957$C)
Difference_1957_total$pan_year_17 <- Seasonal_GBR_1957_list$Year_171957$`0` + Pand_GBR_1957_list$Year_171957$`0` - (Seasonal_GBR_1957_list$Year_171957$C + Pand_GBR_1957_list$Year_171957$C)
Difference_1957_total$pan_year_18 <- Seasonal_GBR_1957_list$Year_181957$`0` + Pand_GBR_1957_list$Year_181957$`0` - (Seasonal_GBR_1957_list$Year_181957$C + Pand_GBR_1957_list$Year_181957$C)
Difference_1957_total$pan_year_19 <- Seasonal_GBR_1957_list$Year_191957$`0` + Pand_GBR_1957_list$Year_191957$`0` - (Seasonal_GBR_1957_list$Year_191957$C + Pand_GBR_1957_list$Year_191957$C)
Difference_1957_total$pan_year_20 <- Seasonal_GBR_1957_list$Year_201957$`0` + Pand_GBR_1957_list$Year_201957$`0` - (Seasonal_GBR_1957_list$Year_201957$C + Pand_GBR_1957_list$Year_201957$C)
Difference_1957_total$pan_year_21 <- Seasonal_GBR_1957_list$Year_211957$`0` + Pand_GBR_1957_list$Year_211957$`0` - (Seasonal_GBR_1957_list$Year_211957$C + Pand_GBR_1957_list$Year_211957$C)
Difference_1957_total$pan_year_22 <- Seasonal_GBR_1957_list$Year_221957$`0` + Pand_GBR_1957_list$Year_221957$`0` - (Seasonal_GBR_1957_list$Year_221957$C + Pand_GBR_1957_list$Year_221957$C)
Difference_1957_total$pan_year_23 <- Seasonal_GBR_1957_list$Year_231957$`0` + Pand_GBR_1957_list$Year_231957$`0` - (Seasonal_GBR_1957_list$Year_231957$C + Pand_GBR_1957_list$Year_231957$C)
Difference_1957_total$pan_year_24 <- Seasonal_GBR_1957_list$Year_241957$`0` + Pand_GBR_1957_list$Year_241957$`0` - (Seasonal_GBR_1957_list$Year_241957$C + Pand_GBR_1957_list$Year_241957$C)
Difference_1957_total$pan_year_25 <- Seasonal_GBR_1957_list$Year_251957$`0` + Pand_GBR_1957_list$Year_251957$`0` - (Seasonal_GBR_1957_list$Year_251957$C + Pand_GBR_1957_list$Year_251957$C)
Difference_1957_total$pan_year_26 <- Seasonal_GBR_1957_list$Year_261957$`0` + Pand_GBR_1957_list$Year_261957$`0` - (Seasonal_GBR_1957_list$Year_261957$C + Pand_GBR_1957_list$Year_261957$C)
Difference_1957_total$pan_year_27 <- Seasonal_GBR_1957_list$Year_271957$`0` + Pand_GBR_1957_list$Year_271957$`0` - (Seasonal_GBR_1957_list$Year_271957$C + Pand_GBR_1957_list$Year_271957$C)

Difference_1957_pandemic$pan_year_0 <-Pand_GBR_1957_list$Year_01957$`0` - ( Pand_GBR_1957_list$Year_01957$C)
Difference_1957_pandemic$pan_year_1 <-  Pand_GBR_1957_list$Year_11957$`0` - ( Pand_GBR_1957_list$Year_11957$C)
Difference_1957_pandemic$pan_year_2 <-  Pand_GBR_1957_list$Year_21957$`0` - (Pand_GBR_1957_list$Year_21957$C)
Difference_1957_pandemic$pan_year_3 <-  Pand_GBR_1957_list$Year_31957$`0` - ( Pand_GBR_1957_list$Year_31957$C)
Difference_1957_pandemic$pan_year_4 <-  Pand_GBR_1957_list$Year_41957$`0` - ( Pand_GBR_1957_list$Year_41957$C)
Difference_1957_pandemic$pan_year_5 <-  Pand_GBR_1957_list$Year_51957$`0` - ( Pand_GBR_1957_list$Year_51957$C)
Difference_1957_pandemic$pan_year_6 <-  Pand_GBR_1957_list$Year_61957$`0` - ( Pand_GBR_1957_list$Year_61957$C)
Difference_1957_pandemic$pan_year_7 <-  Pand_GBR_1957_list$Year_71957$`0` - ( Pand_GBR_1957_list$Year_71957$C)
Difference_1957_pandemic$pan_year_8 <-  Pand_GBR_1957_list$Year_81957$`0` - ( Pand_GBR_1957_list$Year_81957$C)
Difference_1957_pandemic$pan_year_9 <-  Pand_GBR_1957_list$Year_91957$`0` - (Pand_GBR_1957_list$Year_91957$C)
Difference_1957_pandemic$pan_year_10 <-  Pand_GBR_1957_list$Year_101957$`0` - ( Pand_GBR_1957_list$Year_101957$C)
Difference_1957_pandemic$pan_year_11 <-  Pand_GBR_1957_list$Year_111957$`0` - ( Pand_GBR_1957_list$Year_111957$C)
Difference_1957_pandemic$pan_year_12 <-  Pand_GBR_1957_list$Year_121957$`0` - ( Pand_GBR_1957_list$Year_121957$C)
Difference_1957_pandemic$pan_year_13 <-  Pand_GBR_1957_list$Year_131957$`0` - ( Pand_GBR_1957_list$Year_131957$C)
Difference_1957_pandemic$pan_year_14 <-  Pand_GBR_1957_list$Year_141957$`0` - ( Pand_GBR_1957_list$Year_141957$C)
Difference_1957_pandemic$pan_year_15 <-  Pand_GBR_1957_list$Year_151957$`0` - ( Pand_GBR_1957_list$Year_151957$C)
Difference_1957_pandemic$pan_year_16 <-  Pand_GBR_1957_list$Year_161957$`0` - ( Pand_GBR_1957_list$Year_161957$C)
Difference_1957_pandemic$pan_year_17 <-  Pand_GBR_1957_list$Year_171957$`0` - ( Pand_GBR_1957_list$Year_171957$C)
Difference_1957_pandemic$pan_year_18 <-  Pand_GBR_1957_list$Year_181957$`0` - ( Pand_GBR_1957_list$Year_181957$C)
Difference_1957_pandemic$pan_year_19 <-  Pand_GBR_1957_list$Year_191957$`0` - ( Pand_GBR_1957_list$Year_191957$C)
Difference_1957_pandemic$pan_year_20 <-  Pand_GBR_1957_list$Year_201957$`0` - ( Pand_GBR_1957_list$Year_201957$C)
Difference_1957_pandemic$pan_year_21 <-  Pand_GBR_1957_list$Year_211957$`0` - ( Pand_GBR_1957_list$Year_211957$C)
Difference_1957_pandemic$pan_year_22 <-  Pand_GBR_1957_list$Year_221957$`0` - ( Pand_GBR_1957_list$Year_221957$C)
Difference_1957_pandemic$pan_year_23 <-  Pand_GBR_1957_list$Year_231957$`0` - ( Pand_GBR_1957_list$Year_231957$C)
Difference_1957_pandemic$pan_year_24 <-  Pand_GBR_1957_list$Year_241957$`0` - ( Pand_GBR_1957_list$Year_241957$C)
Difference_1957_pandemic$pan_year_25 <-  Pand_GBR_1957_list$Year_251957$`0` - (Pand_GBR_1957_list$Year_251957$C)
Difference_1957_pandemic$pan_year_26 <-  Pand_GBR_1957_list$Year_261957$`0` - ( Pand_GBR_1957_list$Year_261957$C)
Difference_1957_pandemic$pan_year_27 <-  Pand_GBR_1957_list$Year_271957$`0` - ( Pand_GBR_1957_list$Year_271957$C)

#### 2009 difference calculations ####

Difference_2009_total <- list()
Difference_2009_pandemic <- list()

Difference_2009_total$pan_year_0 <- Seasonal_GBR_2009_list$Year_02009$`0` + Pand_GBR_2009_list$Year_02009$`0` - (Seasonal_GBR_2009_list$Year_02009$C + Pand_GBR_2009_list$Year_02009$C)
Difference_2009_total$pan_year_1 <- Seasonal_GBR_2009_list$Year_12009$`0` + Pand_GBR_2009_list$Year_12009$`0` - (Seasonal_GBR_2009_list$Year_12009$C + Pand_GBR_2009_list$Year_12009$C)
Difference_2009_total$pan_year_2 <- Seasonal_GBR_2009_list$Year_22009$`0` + Pand_GBR_2009_list$Year_22009$`0` - (Seasonal_GBR_2009_list$Year_22009$C + Pand_GBR_2009_list$Year_22009$C)
Difference_2009_total$pan_year_3 <- Seasonal_GBR_2009_list$Year_32009$`0` + Pand_GBR_2009_list$Year_32009$`0` - (Seasonal_GBR_2009_list$Year_32009$C + Pand_GBR_2009_list$Year_32009$C)
Difference_2009_total$pan_year_4 <- Seasonal_GBR_2009_list$Year_42009$`0` + Pand_GBR_2009_list$Year_42009$`0` - (Seasonal_GBR_2009_list$Year_42009$C + Pand_GBR_2009_list$Year_42009$C)
Difference_2009_total$pan_year_5 <- Seasonal_GBR_2009_list$Year_52009$`0` + Pand_GBR_2009_list$Year_52009$`0` - (Seasonal_GBR_2009_list$Year_52009$C + Pand_GBR_2009_list$Year_52009$C)
Difference_2009_total$pan_year_6 <- Seasonal_GBR_2009_list$Year_62009$`0` + Pand_GBR_2009_list$Year_62009$`0` - (Seasonal_GBR_2009_list$Year_62009$C + Pand_GBR_2009_list$Year_62009$C)
Difference_2009_total$pan_year_7 <- Seasonal_GBR_2009_list$Year_72009$`0` + Pand_GBR_2009_list$Year_72009$`0` - (Seasonal_GBR_2009_list$Year_72009$C + Pand_GBR_2009_list$Year_72009$C)
Difference_2009_total$pan_year_8 <- Seasonal_GBR_2009_list$Year_82009$`0` + Pand_GBR_2009_list$Year_82009$`0` - (Seasonal_GBR_2009_list$Year_82009$C + Pand_GBR_2009_list$Year_82009$C)
Difference_2009_total$pan_year_9 <- Seasonal_GBR_2009_list$Year_92009$`0` + Pand_GBR_2009_list$Year_92009$`0` - (Seasonal_GBR_2009_list$Year_92009$C + Pand_GBR_2009_list$Year_92009$C)
Difference_2009_total$pan_year_10 <- Seasonal_GBR_2009_list$Year_102009$`0` + Pand_GBR_2009_list$Year_102009$`0` - (Seasonal_GBR_2009_list$Year_102009$C + Pand_GBR_2009_list$Year_102009$C)
Difference_2009_total$pan_year_11 <- Seasonal_GBR_2009_list$Year_112009$`0` + Pand_GBR_2009_list$Year_112009$`0` - (Seasonal_GBR_2009_list$Year_112009$C + Pand_GBR_2009_list$Year_112009$C)
Difference_2009_total$pan_year_12 <- Seasonal_GBR_2009_list$Year_122009$`0` + Pand_GBR_2009_list$Year_122009$`0` - (Seasonal_GBR_2009_list$Year_122009$C + Pand_GBR_2009_list$Year_122009$C)
Difference_2009_total$pan_year_13 <- Seasonal_GBR_2009_list$Year_132009$`0` + Pand_GBR_2009_list$Year_132009$`0` - (Seasonal_GBR_2009_list$Year_132009$C + Pand_GBR_2009_list$Year_132009$C)
Difference_2009_total$pan_year_14 <- Seasonal_GBR_2009_list$Year_142009$`0` + Pand_GBR_2009_list$Year_142009$`0` - (Seasonal_GBR_2009_list$Year_142009$C + Pand_GBR_2009_list$Year_142009$C)
Difference_2009_total$pan_year_15 <- Seasonal_GBR_2009_list$Year_152009$`0` + Pand_GBR_2009_list$Year_152009$`0` - (Seasonal_GBR_2009_list$Year_152009$C + Pand_GBR_2009_list$Year_152009$C)
Difference_2009_total$pan_year_16 <- Seasonal_GBR_2009_list$Year_162009$`0` + Pand_GBR_2009_list$Year_162009$`0` - (Seasonal_GBR_2009_list$Year_162009$C + Pand_GBR_2009_list$Year_162009$C)
Difference_2009_total$pan_year_17 <- Seasonal_GBR_2009_list$Year_172009$`0` + Pand_GBR_2009_list$Year_172009$`0` - (Seasonal_GBR_2009_list$Year_172009$C + Pand_GBR_2009_list$Year_172009$C)
Difference_2009_total$pan_year_18 <- Seasonal_GBR_2009_list$Year_182009$`0` + Pand_GBR_2009_list$Year_182009$`0` - (Seasonal_GBR_2009_list$Year_182009$C + Pand_GBR_2009_list$Year_182009$C)
Difference_2009_total$pan_year_19 <- Seasonal_GBR_2009_list$Year_192009$`0` + Pand_GBR_2009_list$Year_192009$`0` - (Seasonal_GBR_2009_list$Year_192009$C + Pand_GBR_2009_list$Year_192009$C)
Difference_2009_total$pan_year_20 <- Seasonal_GBR_2009_list$Year_202009$`0` + Pand_GBR_2009_list$Year_202009$`0` - (Seasonal_GBR_2009_list$Year_202009$C + Pand_GBR_2009_list$Year_202009$C)
Difference_2009_total$pan_year_21 <- Seasonal_GBR_2009_list$Year_212009$`0` + Pand_GBR_2009_list$Year_212009$`0` - (Seasonal_GBR_2009_list$Year_212009$C + Pand_GBR_2009_list$Year_212009$C)
Difference_2009_total$pan_year_22 <- Seasonal_GBR_2009_list$Year_222009$`0` + Pand_GBR_2009_list$Year_222009$`0` - (Seasonal_GBR_2009_list$Year_222009$C + Pand_GBR_2009_list$Year_222009$C)
Difference_2009_total$pan_year_23 <- Seasonal_GBR_2009_list$Year_232009$`0` + Pand_GBR_2009_list$Year_232009$`0` - (Seasonal_GBR_2009_list$Year_232009$C + Pand_GBR_2009_list$Year_232009$C)
Difference_2009_total$pan_year_24 <- Seasonal_GBR_2009_list$Year_242009$`0` + Pand_GBR_2009_list$Year_242009$`0` - (Seasonal_GBR_2009_list$Year_242009$C + Pand_GBR_2009_list$Year_242009$C)
Difference_2009_total$pan_year_25 <- Seasonal_GBR_2009_list$Year_252009$`0` + Pand_GBR_2009_list$Year_252009$`0` - (Seasonal_GBR_2009_list$Year_252009$C + Pand_GBR_2009_list$Year_252009$C)
Difference_2009_total$pan_year_26 <- Seasonal_GBR_2009_list$Year_262009$`0` + Pand_GBR_2009_list$Year_262009$`0` - (Seasonal_GBR_2009_list$Year_262009$C + Pand_GBR_2009_list$Year_262009$C)
Difference_2009_total$pan_year_27 <- Seasonal_GBR_2009_list$Year_272009$`0` + Pand_GBR_2009_list$Year_272009$`0` - (Seasonal_GBR_2009_list$Year_272009$C + Pand_GBR_2009_list$Year_272009$C)

Difference_2009_pandemic$pan_year_0 <-Pand_GBR_2009_list$Year_02009$`0` - ( Pand_GBR_2009_list$Year_02009$C)
Difference_2009_pandemic$pan_year_1 <-  Pand_GBR_2009_list$Year_12009$`0` - ( Pand_GBR_2009_list$Year_12009$C)
Difference_2009_pandemic$pan_year_2 <-  Pand_GBR_2009_list$Year_22009$`0` - (Pand_GBR_2009_list$Year_22009$C)
Difference_2009_pandemic$pan_year_3 <-  Pand_GBR_2009_list$Year_32009$`0` - ( Pand_GBR_2009_list$Year_32009$C)
Difference_2009_pandemic$pan_year_4 <-  Pand_GBR_2009_list$Year_42009$`0` - ( Pand_GBR_2009_list$Year_42009$C)
Difference_2009_pandemic$pan_year_5 <-  Pand_GBR_2009_list$Year_52009$`0` - ( Pand_GBR_2009_list$Year_52009$C)
Difference_2009_pandemic$pan_year_6 <-  Pand_GBR_2009_list$Year_62009$`0` - ( Pand_GBR_2009_list$Year_62009$C)
Difference_2009_pandemic$pan_year_7 <-  Pand_GBR_2009_list$Year_72009$`0` - ( Pand_GBR_2009_list$Year_72009$C)
Difference_2009_pandemic$pan_year_8 <-  Pand_GBR_2009_list$Year_82009$`0` - ( Pand_GBR_2009_list$Year_82009$C)
Difference_2009_pandemic$pan_year_9 <-  Pand_GBR_2009_list$Year_92009$`0` - (Pand_GBR_2009_list$Year_92009$C)
Difference_2009_pandemic$pan_year_10 <-  Pand_GBR_2009_list$Year_102009$`0` - ( Pand_GBR_2009_list$Year_102009$C)
Difference_2009_pandemic$pan_year_11 <-  Pand_GBR_2009_list$Year_112009$`0` - ( Pand_GBR_2009_list$Year_112009$C)
Difference_2009_pandemic$pan_year_12 <-  Pand_GBR_2009_list$Year_122009$`0` - ( Pand_GBR_2009_list$Year_122009$C)
Difference_2009_pandemic$pan_year_13 <-  Pand_GBR_2009_list$Year_132009$`0` - ( Pand_GBR_2009_list$Year_132009$C)
Difference_2009_pandemic$pan_year_14 <-  Pand_GBR_2009_list$Year_142009$`0` - ( Pand_GBR_2009_list$Year_142009$C)
Difference_2009_pandemic$pan_year_15 <-  Pand_GBR_2009_list$Year_152009$`0` - ( Pand_GBR_2009_list$Year_152009$C)
Difference_2009_pandemic$pan_year_16 <-  Pand_GBR_2009_list$Year_162009$`0` - ( Pand_GBR_2009_list$Year_162009$C)
Difference_2009_pandemic$pan_year_17 <-  Pand_GBR_2009_list$Year_172009$`0` - ( Pand_GBR_2009_list$Year_172009$C)
Difference_2009_pandemic$pan_year_18 <-  Pand_GBR_2009_list$Year_182009$`0` - ( Pand_GBR_2009_list$Year_182009$C)
Difference_2009_pandemic$pan_year_19 <-  Pand_GBR_2009_list$Year_192009$`0` - ( Pand_GBR_2009_list$Year_192009$C)
Difference_2009_pandemic$pan_year_20 <-  Pand_GBR_2009_list$Year_202009$`0` - ( Pand_GBR_2009_list$Year_202009$C)
Difference_2009_pandemic$pan_year_21 <-  Pand_GBR_2009_list$Year_212009$`0` - ( Pand_GBR_2009_list$Year_212009$C)
Difference_2009_pandemic$pan_year_22 <-  Pand_GBR_2009_list$Year_222009$`0` - ( Pand_GBR_2009_list$Year_222009$C)
Difference_2009_pandemic$pan_year_23 <-  Pand_GBR_2009_list$Year_232009$`0` - ( Pand_GBR_2009_list$Year_232009$C)
Difference_2009_pandemic$pan_year_24 <-  Pand_GBR_2009_list$Year_242009$`0` - ( Pand_GBR_2009_list$Year_242009$C)
Difference_2009_pandemic$pan_year_25 <-  Pand_GBR_2009_list$Year_252009$`0` - (Pand_GBR_2009_list$Year_252009$C)
Difference_2009_pandemic$pan_year_26 <-  Pand_GBR_2009_list$Year_262009$`0` - ( Pand_GBR_2009_list$Year_262009$C)
Difference_2009_pandemic$pan_year_27 <-  Pand_GBR_2009_list$Year_272009$`0` - ( Pand_GBR_2009_list$Year_272009$C)

DALY_difference_2009_total <- as.data.frame(Difference_2009_total)
DALY_difference_1957_total <- as.data.frame(Difference_1957_total)
DALY_difference_1918_total <- as.data.frame(Difference_1918_total)
DALY_difference_2009_pand <- as.data.frame(Difference_2009_pandemic)
DALY_difference_1957_pand <- as.data.frame(Difference_1957_pandemic)
DALY_difference_1918_pand <- as.data.frame(Difference_1918_pandemic)


### getting quantiles ###

quants <- c(0.25, 0.5, 0.75)

#quantiles
DALY_quantiles_2009 <- sapply(DALY_difference_2009_total, quantile, probs=quants)
DALY_quantiles_1957 <- sapply(DALY_difference_1957_total, quantile, probs=quants)
DALY_quantiles_1918 <- sapply(DALY_difference_1918_total, quantile, probs=quants)
#mean of total
DALY_mean_2009 <- sapply(DALY_difference_2009_total, mean)
DALY_mean_1957 <- sapply(DALY_difference_1957_total, mean)
DALY_mean_1918 <- sapply(DALY_difference_1918_total, mean)
#mean of pandemic
DALY_mean_2009_pand <- sapply(DALY_difference_2009_pand , mean)
DALY_mean_1957_pand <- sapply(DALY_difference_1957_pand, mean)
DALY_mean_1918_pand <- sapply(DALY_difference_1918_pand, mean)

DALY_quantiles_2009 <- as.data.frame(rbind(DALY_quantiles_2009, DALY_mean_2009, DALY_mean_2009_pand))
DALY_quantiles_1957 <- as.data.frame(rbind(DALY_quantiles_1957, DALY_mean_1957, DALY_mean_1957_pand))
DALY_quantiles_1918 <- as.data.frame(rbind(DALY_quantiles_1918, DALY_mean_1918, DALY_mean_1918_pand))

Statistic = c("q25", "q50", "q75", "Mean Total", "Mean Pandemic")
DALY_quantiles_2009 <- cbind(Statistic, DALY_quantiles_2009)
DALY_quantiles_1957 <- cbind(Statistic, DALY_quantiles_1957)
DALY_quantiles_1918 <- cbind(Statistic, DALY_quantiles_1918)


combined_1957 <- DALY_quantiles_1957  %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)

combined_2009 <- DALY_quantiles_2009 %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)
combined_1918 <- DALY_quantiles_1918 %>% pivot_longer(-Statistic,names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)


combined_1957$Year <- as.numeric(gsub("[^0-9]", "", combined_1957$Year))
combined_1957 %>% mutate(Year = as.factor(Year))
combined_1957<- combined_1957 %>% mutate(diff =`Mean Total`- `Mean Pandemic`)


combined_1918$Year <- as.numeric(gsub("[^0-9]", "", combined_1918$Year))
combined_1918 %>% mutate(Year = as.factor(Year))
combined_1918<- combined_1918 %>% mutate(diff =`Mean Total`- `Mean Pandemic`)

combined_2009$Year <- as.numeric(gsub("[^0-9]", "", combined_2009$Year))
combined_2009 %>% mutate(Year = as.factor(Year))
combined_2009<- combined_2009 %>% mutate(diff =`Mean Total`- `Mean Pandemic`)


combined_2009 <- pivot_longer(combined_2009,
                        cols = c(`Mean Pandemic`, diff),
                        names_to = "type",
                        values_to = "total")

combined_1957 <- pivot_longer(combined_1957,
                              cols = c(`Mean Pandemic`, diff),
                              names_to = "type",
                              values_to = "total")
combined_1918 <- pivot_longer(combined_1918,
                              cols = c(`Mean Pandemic`, diff),
                              names_to = "type",
                              values_to = "total")



plot1 <- ggplot(combined_2009, aes(x = Year, y = total/1e6, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total DALYs averted from NGIVs vs no vaccination (2009)",
       x = "Year of Pandemic", y = "DALYs averted (1,000,000)") +
  geom_errorbar(aes(ymin = q25/1e6, ymax = q75/1e6), width = 0.2, color = "black") +
  scale_fill_manual(
    values = c("Mean Pandemic" = "gray40", "diff" = "gray80"),
    labels = c("Mean Pandemic" = "Pandemic", "diff" = "Seasonal")
  )+
  labs(fill = "Cause of DALYs") +
  theme_minimal()

plot2 <- ggplot(combined_1957, aes(x = Year, y = total/1e6, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total DALYs averted from NGIVs vs no vaccination (1957)",
       x = "Year of Pandemic", y = "DALYs averted (1,000,000)") +
  geom_errorbar(aes(ymin = q25/1e6, ymax = q75/1e6), width = 0.2, color = "black") +
  scale_fill_manual(
    values = c("Mean Pandemic" = "blue", "diff" = '#C1C3FF'),
    labels = c("Mean Pandemic" = "Pandemic", "diff" = "Seasonal")
  )+
  labs(fill = "Cause of DALYs") +
  theme_minimal()

plot3 <- ggplot(combined_1918, aes(x = Year, y = total/1e6, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total DALYs averted from NGIVs vs no vaccination (1918)",
       x = "Year of Pandemic", y = "DALYs averted (1,000,000)") +
  geom_errorbar(aes(ymin = q25/1e6, ymax = q75/1e6), width = 0.2, color = "black") +
  scale_fill_manual(
    values = c("Mean Pandemic" = "red", "diff" = '#FFC1C2'),
    labels = c("Mean Pandemic" = "Pandemic", "diff" = "Seasonal")
  )+
  labs(fill = "Cause of DALYs") +
  theme_minimal()

plot1/plot2/plot3


#### 1918: Pandemic Scenarios #####

# 0 years
Pand_GBR_1918_0 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19180.rds')
Pand_GBR_1918_0_adding_DALYS <- total_DALYS_for_pandemic(
  Pand_GBR_1918_0, symp_samples, dcr_infr, '1918', 3, 
  global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
)

# Summarise total_DALYS by simulation_index and vacc_type
Pand_GBR_1918_0_summarising <- Pand_GBR_1918_0_adding_DALYS[, .(
  total_DALYS = sum(total_DALYS)
), by = .(simulation_index, vacc_type)]

# Create a named list with vacc_type as names and total_DALYS vectors as values
Pand_GBR_1918_0 <- split(Pand_GBR_1918_0_summarising$total_DALYS, Pand_GBR_1918_0_summarising$vacc_type)

#1 year

Pand_GBR_1918_1 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19181.rds')
Pand_GBR_1918_1_adding_DALYS <- total_DALYS_for_pandemic(
  Pand_GBR_1918_1, symp_samples, dcr_infr, '1918', 3, 
  global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
)

# Summarise total_DALYS by simulation_index and vacc_type
Pand_GBR_1918_1_summarising <- Pand_GBR_1918_1_adding_DALYS[, .(
  total_DALYS = sum(total_DALYS)
), by = .(simulation_index, vacc_type)]

# Create a named list with vacc_type as names and total_DALYS vectors as values
Pand_GBR_1918_1 <- split(Pand_GBR_1918_1_summarising$total_DALYS, Pand_GBR_1918_1_summarising$vacc_type)

#2 years

Pand_GBR_1918_2 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19182.rds')
Pand_GBR_1918_2_adding_DALYS <- total_DALYS_for_pandemic(
  Pand_GBR_1918_2, symp_samples, dcr_infr, '1918', 3, 
  global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
)

Pand_GBR_1918_2_summarising <- Pand_GBR_1918_2_adding_DALYS[, .(
  total_DALYS = sum(total_DALYS)
), by = .(simulation_index, vacc_type)]

Pand_GBR_1918_2 <- split(Pand_GBR_1918_2_summarising$total_DALYS, Pand_GBR_1918_2_summarising$vacc_type)

#3 years

Pand_GBR_1918_3 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19183.rds')
Pand_GBR_1918_3_adding_DALYS <- total_DALYS_for_pandemic(
  Pand_GBR_1918_3, symp_samples, dcr_infr, '1918', 3, 
  global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
)

Pand_GBR_1918_3_summarising <- Pand_GBR_1918_3_adding_DALYS[, .(
  total_DALYS = sum(total_DALYS)
), by = .(simulation_index, vacc_type)]

Pand_GBR_1918_3 <- split(Pand_GBR_1918_3_summarising$total_DALYS, Pand_GBR_1918_3_summarising$vacc_type)

#4 years

Pand_GBR_1918_4 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19184.rds')
Pand_GBR_1918_4_adding_DALYS <- total_DALYS_for_pandemic(
  Pand_GBR_1918_4, symp_samples, dcr_infr, '1918', 3, 
  global_ihrs, outpatient_ratios, 'GBR', DALY_weight_samples, pandemic_ifrs, yll_df
)

Pand_GBR_1918_4_summarising <- Pand_GBR_1918_4_adding_DALYS[, .(
  total_DALYS = sum(total_DALYS)
), by = .(simulation_index, vacc_type)]

Pand_GBR_1918_4 <- split(Pand_GBR_1918_4_summarising$total_DALYS, Pand_GBR_1918_4_summarising$vacc_type)





#seasonal 1918

Seasonal_GBR_1918_0 <- readRDS('/Users/lshbh6/Documents/GitHub/Pandemicscurrent/GBR_runs/Pand_GBR19180.rds')
Seasonal_GBR_1918_0_adding_DALYS <- total_DALYS_for_seasonal(Seasonal_GBR_1918_0, symp_samples, dcr_infr, global_ihrs, outpatient_ratios,'GBR', national_ifrs, yll_df)
Seasonal_GBR_1918_0_summarising <- Seasonal_GBR_1918_0_adding_DALYS[, .(total_DALYS = sum(total_DALYS)), by = .(simulation_index, vacc_type)]
Seasonal_GBR_1918_0_summary_0 <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == '0')
Seasonal_GBR_1918_0_summary_0 <- Seasonal_GBR_1918_0_summary_0$total_DALYS
Seasonal_GBR_1918_0_summary_A1 <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == 'A.1')
Seasonal_GBR_1918_0_summary_A1 <- Seasonal_GBR_1918_0_summary_A1$total_DALYS
Seasonal_GBR_1918_0_summary_A2 <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == 'A.2')
Seasonal_GBR_1918_0_summary_A2 <- Seasonal_GBR_1918_0_summary_A2$total_DALYS
Seasonal_GBR_1918_0_summary_B1 <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == 'B.1')
Seasonal_GBR_1918_0_summary_B1 <- Seasonal_GBR_1918_0_summary_B1$total_DALYS
Seasonal_GBR_1918_0_summary_B2 <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == 'B.2')
Seasonal_GBR_1918_0_summary_B2 <- Seasonal_GBR_1918_0_summary_B2$total_DALYS
Seasonal_GBR_1918_0_summary_C <- Seasonal_GBR_1918_0_summarising %>% subset(vacc_type == 'C')
Seasonal_GBR_1918_0_summary_C <- Seasonal_GBR_1918_0_summary_C$total_DALYS

