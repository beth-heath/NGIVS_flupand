# Pandemics_project

Project that runs seasonal epidemics based on code from Goodfellow et al., and includes pandemics based on the 1919, 1957 and 2009 pandemics.

Built from work from <https://github.com/lucy-gf/flu_model_LG>

The project consists of five folders: data to include the data to run the code, functions including the functions contained in the code, setup which includes the files to download packages, Roles_to_run which included the scripts to run to produce the outputs for the analysis and Run_scripts which is the output from Run scripts.

### `Roles_to_run/Overall_script_epidemics.R`

This code will run the epidemics portion of the model. 
It requires the contintent (ITZ) and the coverage to be included to run the code (20, 50, 70). 
This code will save the corresponding ITZ and coverage levels in the Run_script folder. 
This only contains data from weeks where the number of infections is non-zero. 

### `Roles_to_run/Overall_script_pandemics.R`

This code will run the pandemic portion of the model. 
It requires the continent (ITZ), pandemic scenario (either 1918, 1957 or 2009) and the coverage to be included to run the code (20, 50, 70).
This code will save the corresponding ITZ and coverage levels in the Run_script folder. 
This only contains data from weeks where the number of infections is non-zero. 

### `functions/Running_analysis_script.R`

This code runs the analysis of the epidemics and pandemics code. 
It requires the input of four parameters: ITZ region, LMIC_boost (1 for no booking and 2 for three times multipler in LMICs), DALY_discount (0 for 3% discount on DALYs and 1 for 0% discount on DALYs) and then coverage (20, 50, 70).
The code will save code to the corresponding Overall files by coverage. 

### `functions/vacc_types.R`

Contains functions to define vaccine programs for no vaccinations and 5 NGIVs (can be edited).

-   Inputs: coverage level, targeted ages
-   Outputs: VE, mean immunity length, coverage across model age groups

### `functions/demography.R`

Calculates weekly age- and vaccination-status specific population over the relevant time period (can remove ageing if needed), for a given country and vaccine program, and contains the function to calculate contact matrices from a given country and age-specific population.

### `functions/transmission_model.R`

Contains the ODE model builder, epidemic simulation function, a function to calculate vaccination status-specific demography.
This has been updated to add extra compartments to allow for the infection progression for the correctly vaccinated individuals. 

### `functions/flu_sim.R`

Contains: 

- `one_flu()`, which runs an epidemic or pandemic, 
- `many_flu()`, which takes a data-table of epidemic data and combines many epidemics or combines different pandemics, 
- `dfn_vaccine_calendar()`, which converts the vaccine program and epidemic dates into a vaccine calendar 
- `flu_doses()`, which calculates how many vaccines were given (before wastage) in the same epidemics as `many_flu()`

### `functions/fluparallelalteredITZ.R`

This files contains the function to convert the previous Goodfellow epidemics into a form that the model can use. It also contains the reduction code that is used to reduce down the results to only those that are non-zero.

Sets vaccine programs, then runs `many_flu()` for some epidemic data, parallelised across each vaccine type.
Note: if adjusting the number of strategies or disease mechanisms then these numbers cannot have a prime factor greater than 1.

### `functions/Analysisfile.R`

This contains the files to read in the data and analysise the files that feeds into the Pandemic_impact function in Economice example that is used in the Running_analysis_script.R

### `functions/contact_matr_fcns.R`

Taken from Goodfellow et al. Produces the contact matrices.

### `functions/Convergence_tests.R`

Runs the convergence tests.

### `functions/creating_pandemic_data.R`

This function should be run before the running of other code and does not require a HPC.
This sets the pandemic runs for the model. Although, the values of these pandemics can be changed, this runs without need for input from the user.

### `functions/doses_function.R.R`

Function to get the doses for each country, year of pandemic and simulation index. It requires input from the command line for the vaccine coverage - 1 for 20%, 2 for 50% and 3 for 70%. 
This saves both the overall doses dose_table_coverage and the country dose by country. This dose by country is the one used in later code. 

### `functions/Economiceexample.R`

Contains the economic analysis used in the paper.

### `functions/Pandemic_Scenarios_diagram.R`

Code to create the figures contained in the paper. 

### `functions/plots_LG.R`

Produces figures shown in the manuscript.
