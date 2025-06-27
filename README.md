# Pandemics_project

Project that runs seasonal epidemics based on code from Goodfellow and includes pandemics based on the 1919, 1957 and 2009 pandemics.


Built from work from <https://github.com/lucy-gf/flu_model_LG>

The project consists of five folders: data to include the data to run the code, functions including the functions contained in the code, setup which includes the files to download
packages, Roles_to_run which included the scripts to run to produce the outputs for the analysis and Run_scripts which is the output from Run scripts.

### `Roles_to_run\ITZzone*.R`

This folder contains 70 files where each continent has a separate set of files with one that runs such for seasonal epidemics and then nine for the pandemics for the three pandemics and the three mechainsms of action of the pandemic vaccine.
These files require no input from the user and need only run. 

### `vacc_types.R`

Contains functions to define vaccine programs for no vaccinations and 5 NGIVs (can be edited).

-   Inputs: coverage level, targeted ages
-   Outputs: VE, mean immunity length, coverage across model age groups

### `functions/demography.R`

Calculates weekly age- and vaccination-status specific population over the relevant time period (can remove ageing if needed), for a given country and vaccine program, and contains the function to calculate contact matrices from a given country and age-specific population.

### `functions/transmission_model.R`

Contains the ODE model builder, epidemic simulation function, a function to calculate vaccination status-specific demography.

### `functions/flu_sim.R`

Contains: 

- `one_flu()`, which runs an epidemic, 
- `many_flu()`, which takes a data-table of epidemic data and combines many epidemics, 
- `dfn_vaccine_calendar()`, which converts the vaccine program and epidemic dates into a vaccine calendar 
- `flu_doses()`, which calculates how many vaccines were given (before wastage) in the same epidemics as `many_flu()`

### `flu_parallel.R`

Sets vaccine programs, then runs `many_flu()` for some epidemic data, parallelised across each vaccine type.

### `flu_plots.R`

Plots influenza incidence and cumulative infections.
