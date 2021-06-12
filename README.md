# Modelling the impact of preventive measures on the transmission of SARS-CoV-2 in secondary schools in the Netherlands
## Description
This repository contains the data, code and figures for the manuscript "Modelling the impact of preventive measures on the transmission of SARS-CoV-2 in secondary schools in the Netherlands" by Thi Mui Pham et al

## Manuscript
The manuscript and supplementary material of the study can be found on [here](https://github.com/tm-pham/covid19_school_transmission/blob/master/manuscript/COVID_19_School_transmission_in_NL.pdf). 

## Model code
The simulation code was built by Thi Mui Pham using R (version 4.0.1). 

The code is divided into several parts. Please find a short overview below. A more detailed description can be found in the supplementary material. 
### Running the code
[run_simulations_in_parallel.sh](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/run_simulations_in_parallel.sh): Bash script to run the simulation code for several scenarios in parallel. You can run the script by executing
```
./run_simulations_in_parallel.sh
```
Please check the file and folder paths in the bash file. 
### Simulation
[covid19school_simulation.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_simulation.R): Run the simulation (epidemic) for several iterations and several scenarios
### Epidemic
[covid19school_epidemic.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_epidemic.R): Function for simulating the epidemic in the school
### Variables
[covid19school_vars.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_vars.R): Setting variables

[covid19school_init_vars_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_init_vars_function.R): Initializing the data frames for students and teachers and contact networks of students. 
### Main functions
[covid19school_transmission_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_transmission_function.R): Function for transmission events. 

[covid19school_quarantine_isolation_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_quarantine_isolation_function.R): Function for isolation of symptomatic individuals and quarantine of their close contacts.

[covid19school_external_foi_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_external_foi_function.R): Function for infection of individuals from community.

[covid19school_screening_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_screening_function.R): Function for regular screening

[covid19school_risk_testing_function.R](https://github.com/tm-pham/covid19_school_transmission/blob/master/model_code/covid19school_risk_testing_function.R): Function for risk-based testing

## Correspondence
Corresponding authors of this work: Thi Mui Pham (t.m.pham-2@umcutrecht.nl)

Last update: 7th June 2021
