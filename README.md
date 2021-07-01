# Modelling the impact of preventive measures on the transmission of SARS-CoV-2 in secondary schools in the Netherlands

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5057515.svg)](https://doi.org/10.5281/zenodo.5057515)

## Description
This repository contains the data, code and figures for the manuscript "Modelling the impact of preventive measures on the transmission of SARS-CoV-2 in secondary schools in the Netherlands" by Thi Mui Pham, Ganna Rozhnova, Ilse Westerhof, Marc Bonten, Martin Bootsma, Mirjam Kretzschmar, Patricia Bruijning-Verhagen. This report  serves  as  the  basis  for  the  [presentation](https://assets-eu-01.kc-usercontent.com/546dd520-97db-01b7-154d-79bb6d950a2d/1bf8fdb4-940e-4ea2-b29d-213cdcfa5eb2/20210528_presentatie_Onderzoek\%20openen\%20middelbare\%20scholen.pdf) delivered  by  the  authors  on  20  May  2021  for  the Ministry of OCW (Ministry of Education, Culture and Science) and the OMT (Outbreak Management Team). 

Please note that we are still in the progress of refining the model and new functions will be added in the future in this repository. 

## Correspondence
Corresponding authors of this work: Thi Mui Pham (t.m.pham-2@umcutrecht.nl)

## Manuscript
The manuscript and supplementary material of the study can be found on [here](https://github.com/tm-pham/covid19_school_transmission/blob/master/manuscript/COVID_19_School_transmission_in_NL.pdf). 

## Summary
We developed an agent-based model for SARS-CoV-2 transmission in a secondary school parameterized using observational data from a pilot study including contact patterns and test results collected in February-April 2021 in NL. 

This work was done in a short amount of time and thus, we made some simplifying assumptions but we performed sensitivity analyses to check whether our overall results were robust to these assumptions. 

Our base case scenario represents the situation in NL before full reopening: half occupancy w baseline measures (isolation of symptomatic cases&quarantine of close contacts) and risk-based testing of classmates&other contacts of symptomatic cases (50% adherence to testing). 

We estimated the effect of reopening sec. schools to full occupancy with baseline measures (as implemented in NL from 7th June 2021) on the no. of school-related infections in students&teachers within the 1st month. 

We compared it with the base case scenario, with twice-weekly screening (antigen self-testing, different levels of adherences) and with vaccination of teachers prior to reopening on the no. of school-related infections in students&teachers within 1st month. 

We accounted for introductions from the community and calibrated the probability to become infected from school-unrelated contacts in the community to data from the pilot study in March 2021. 

Our results suggest that screening twice weekly at 50% adherence can significantly reduce infections in both students and teachers. Advancing vaccination of teachers before reopening could further reduce infections and is especially effective when screening adherence is low. 

The main implications of this work are that safe reopening is possible IF students and teachers perform self-testing twice weekly at high adherences, especially since infection risk has declined since March. 

We didn't explicitly model aerosol transmission in rooms, physical distancing nor mask-wearing. We assumed an R-value of 1.05 in all scenarios based on the estimate in March in NL. It reflects the reduced transmissibility due to physical distancing and mitigation measures. 

This is work-in-progress and we will continue to refine the model. Updated results will be uploaded in this GitHub repository and at some point also to a preprint server. 

## Model code
The simulation code was built by Thi Mui Pham using R (version 4.0.1). 

The code is divided into several parts. Please find a short overview below.
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

