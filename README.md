# Pandemics_project

Project that runs seasonal epidemics based on code from Goodfellow and includes pandemics based on the 1919, 1957 and 2009 pandemics.


Built from work from <https://github.com/lucy-gf/flu_model_LG>

The project consists of five folders: data to include the data to run the code, functions including the functions contained in the code, setup which includes the files to download
packages, Roles_to_run which included the scripts to run to produce the outputs for the analysis and Run_scripts which is the output from Run scripts.

### `Roles_to_run\ITZzone*.R`

The folder contains 7 files where each continent has a separate set of files for one of the epidemic.

### `Roles_to_run\Overall_script_pandemics.R`

[TBD]

### `Running_analysis_script.R`

[TBD]

### `vacc_types.R`

Contains functions to define vaccine programs for no vaccinations and 5 NGIVs (can be edited).

-   Inputs: coverage level, targeted ages
-   Outputs: VE, mean immunity length, coverage across model age groups

### `functions/demography.R`

Calculates weekly age- and vaccination-status specific population over the relevant time period (can remove ageing if needed), for a given country and vaccine program, and contains the function to calculate contact matrices from a given country and age-specific population.

### `functions/transmission_model.R`

[TBD]

Contains the ODE model builder, epidemic simulation function, a function to calculate vaccination status-specific demography.

### `functions/flu_sim.R`

[TBD]

Contains: 

- `one_flu()`, which runs an epidemic, 
- `many_flu()`, which takes a data-table of epidemic data and combines many epidemics, 
- `dfn_vaccine_calendar()`, which converts the vaccine program and epidemic dates into a vaccine calendar 
- `flu_doses()`, which calculates how many vaccines were given (before wastage) in the same epidemics as `many_flu()`

### `fluparallelalteredITZ.R`

[TBD]

Sets vaccine programs, then runs `many_flu()` for some epidemic data, parallelised across each vaccine type.

### `analysis_files.R` & `Analysisfiles.R`

[TBD]

### `functions/contact_matr_fcns.R`

Taken from Goodfellow et al. Produces the contact matricwes.

### `functions/Convergence_tests.R`

[TBD]

### `functions/creating_pandemic_data.R`

[TBD]

### `functions/dose_calculator_function.R.R` & `functions/doses_function.R.R`

[TBD]

### `functions/Economiceexample.R`

[TBD]




