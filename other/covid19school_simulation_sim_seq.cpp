rm(list=ls())

source("covid19school_packages.R")  # Installs and loads all necessary pacakges
source("covid19school_functions.R") # Loads all relevant functions
source("covid19school_plotting_functions.R") # Loads all functions for plotting
source("covid19school_vars.R")      # Load all relevant variables
source("covid19school_epidemic.R")  # Load main function for simulation

### Variables
iter=1

resultsPath="../results/"
folder='sim'
if(!file.exists(paste0(resultsPath,folder))) dir.create(file.path(paste0(resultsPath,folder)))
print(paste0("File path = ", resultsPath,folder))

### Scenarios
suffix <- c("", "", "_vaccination","_risk_based_testing","_risk_based_testing","_screening_twice_weekly","_screening_weekly")
scenarios <- c("Full occup", 
               "Half occup", 
               "Vaccination (full)", 
               "Risk based testing (full)", 
               "Risk based testing (half)",
               "Screening twice weekly (full)",
               "Screening weekly (full)")
length(suffix)==length(scenarios)

###
tol = 1e-5

### Proportion of effective contacts of teachers
# 0.5 for half occupancy, 0.3 for full occupancy
sample_prop = c(0.3, 0.5, 0.3, 0.3, 0.5, 0.3, 0.3)

### Scaling factor for transmission probability from teachers
cont_susc_scale = c(0.25, 0.5)

### Scaling factor for transmission probability from teachers
prop_vaccinated = c(0, 0.85)

### Time
unit=24
steps <- 8/24; time_period <- 4*7; time_steps <- seq(1, time_period, by = steps)
group_per_step <- rep(c(rep(1,(1/steps)*(24/unit)),rep(2,(1/steps)*(24/unit))), time_period/2)[1:length(time_steps)] # Assign groups to each time point, assuming 2 groups

### Types of individuals
types <- c("S", "PS", "IS", "IA", "R", "IH", "Q")

### External incidence rate for importations
external_prob = 2*c(0.0007965315, 0.0007965315)

### Screening days
testing_mat <- matrix(c(rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        c(1,0,1,0,0,0,0),
                        c(1,0,0,0,0,0,0)), ncol=7, byrow=T)
colnames(testing_mat) <- day_names
nrow(testing_mat)==length(scenarios)

### Adherence to screening
screening_adherence <- 0.5
risk_testing_adherence <- 0.5

### Occupancy
occup_suffix <- c("full_occup","half_occup","full_occup",
                  "full_occup","half_occup",
                  "full_occup","full_occup")
occup <- c(1, 0.5, 1, 
           1, 0.5, 
           1, 1) # Occupancy level, options: 0.5, 1

### Flags for screening, risk-based testing and vaccination
scr_flags <- c(F,F,F,F,F,T,T); risk_flags <- c(F,F,F,T,T,F,F)
vacc_flag <- c(F,F,T,F,F,F,F)

## ============================================================================ #
# Absent days and absent individuals
abs_days_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
abs_days_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Peak number of infected individuals
peak_students <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))
peak_teachers <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))

# Outbreak size
outbreak_data_students <- outbreak_data_teachers <- vector(mode="list", length=length(scenarios))
outbreak_size_students <- outbreak_size_teachers <- rep(list(rep(0,iter)), length(scenarios))

# Outbreak size per "location"
outbreak_per_loc_st <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
outbreak_per_loc_teach <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Secondary cases
sec_cases_students <- sec_cases_teachers <- vector(mode="list", length=length(scenarios))

# Number over time
ns_list <- vector(mode="list", length=length(scenarios))
nt_list <- vector(mode="list", length=length(scenarios))
epidemic_list <- vector(mode="list", length=length(scenarios))

# ============================================================================ #
# Starting the simulations
# ============================================================================ #
total_start_time <- Sys.time()
for(scenario in seq){
  # Matrix output template
  mat <- matrix(0, nrow=length(time_steps), ncol=iter)
  ns <- nt <- rep(list(mat),7)
  names(ns) <- names(nt) <- types
  # Start simulation 
  start_time <- Sys.time()
  for(it in 1:iter){
    epidemic <- school.epidemic(seed=it, 
                                df_history, df_agent, df_teach_hist, df_teacher,
                                occup = occup[scenario],
                                steps = steps,
                                time_period = time_period, 
                                test_days = testing_mat[scenario,], 
                                screening_adherence = screening_adherence,
                                risk_testing_adherence = risk_testing_adherence,
                                day_names = day_names,
                                spec=spec, 
                                screening_flag = scr_flags[scenario], 
                                risk_based_flag = risk_flags[scenario],
                                vaccination_flag = vacc_flag[scenario],
                                isolation_flag = T, 
                                external_prob = external_prob, 
                                cont_close = cont_close, 
                                cont_class = cont_class, 
                                cont_grade = cont_grade, 
                                cont_teacher = cont_teacher, 
                                cont_tt = cont_tt,
                                susc_close = susc_close, 
                                susc_class = susc_class, 
                                susc_grade = susc_grade, 
                                susc_out_school = susc_out_school, 
                                susc_teacher = susc_teacher, 
                                susc_tt = susc_tt, 
                                sample_prop=sample_prop[scenario], 
                                prop_vaccinated = prop_vaccinated,
                                cont_susc_scale = cont_susc_scale, 
                                tol=tol)
    df_history <- epidemic$df_history
    df_agent <- epidemic$df_agent
    df_teach_hist <- epidemic$df_teach_hist
    df_teacher <- epidemic$df_teacher
    df_screening <- epidemic$df_screening
    screening_list <- epidemic$screening_list
    df_risk_testing <- epidemic$df_risk_testing
    infecteds <- epidemic$infecteds
    susc_close <- epidemic$susc_close
    susc_class <- epidemic$susc_class
    susc_grade <- epidemic$susc_grade
    susc_out_school <- epidemic$susc_out_school
    # For plotting
    for(j in 1:length(ns)){
      # Students
      ns[[j]][,it] <- sapply(unique(df_history$time), function(x) nrow(df_history %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
      # Teachers
      nt[[j]][,it] <- sapply(unique(df_teach_hist$time), function(x) nrow(df_teach_hist %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
    }
    
    # Outbreak size students
    outbreak_size_students[[scenario]][it] <- sum(!is.na(df_agent$infector))
    outbreak_size_teachers[[scenario]][it] <- sum(!is.na(df_teacher$infector))
    
    # Outbreak size per introduction
    outbreak_data_students[[scenario]][[it]] <- table(df_agent$source)
    outbreak_data_teachers[[scenario]][[it]] <- table(df_teacher$source)
    
    # Outbreak size per "location"
    colnames(outbreak_per_loc_st[[scenario]]) <- c("Outside school-related", "Within school", "External")
    outbreak_per_loc_st[[scenario]][it, ] <- table(factor(df_agent$location, levels=0:2))
    colnames(outbreak_per_loc_teach[[scenario]]) <- c("Outside school-related", "Within school", "External")
    outbreak_per_loc_teach[[scenario]][it, ] <- table(factor(df_teacher$location, levels=0:2))
    
    # Peak number 
    colnames(peak_students[[scenario]]) <- c("Asymptomatic", "Pre-symptomatic", "Symptomatic", "Infected")
    peak_students[[scenario]][it, "Asymptomatic"] <- max(ns[["IA"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Pre-symptomatic"] <- max(ns[["PS"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Symptomatic"] <- max(ns[["IS"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Infected"] <- sum(peak_students[[scenario]][it, -4])
    
    colnames(peak_teachers[[scenario]]) <- c("Asymptomatic", "Pre-symptomatic", "Symptomatic", "Infected")
    peak_teachers[[scenario]][it, "Asymptomatic"] <- max(nt[["IA"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Pre-symptomatic"] <- max(nt[["PS"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Symptomatic"] <- max(nt[["IS"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Infected"] <- sum(peak_teachers[[scenario]][it, -4])
    
    # Number of absences
    colnames(abs_days_students[[scenario]]) <- c("Isolated", "Quarantined", "Total absent")
    abs_days_students[[scenario]][it, "Isolated"] <- nrow(unique(df_history[df_history$iso_state=="IH" & df_history$pres==1,]))*steps
    abs_days_students[[scenario]][it, "Quarantined"] <- nrow(unique(df_history[df_history$iso_state=="Q" & df_history$pres==1,]))*steps
    abs_days_students[[scenario]][it, "Total absent"] <- abs_days_students[[scenario]][it, 1] + abs_days_students[[scenario]][it, 2]
    
    # Number of absences
    colnames(absences_students[[scenario]]) <- c("Isolated", "Quarantined", "Total absent")
    absences_students[[scenario]][it, "Isolated"] <- length(unique(df_history[df_history$iso_state=="IH","id"]))
    absences_students[[scenario]][it, "Quarantined"] <- length(unique(df_history[df_history$iso_state=="Q","id"]))
    absences_students[[scenario]][it, "Total absent"] <- absences_students[[scenario]][it, 1] + absences_students[[scenario]][it, 2]
    
    # Number of absences
    colnames(abs_days_teachers[[scenario]]) <- c("Isolated", "Quarantined", "Total absent")
    abs_days_teachers[[scenario]][it, "Isolated"] <- nrow(unique(df_teach_hist[df_teach_hist$iso_state=="IH" & df_teach_hist$pres==1,]))*steps
    abs_days_teachers[[scenario]][it, "Quarantined"] <- nrow(unique(df_teach_hist[df_teach_hist$iso_state=="Q" & df_teach_hist$pres==1,]))*steps
    abs_days_teachers[[scenario]][it, "Total absent"] <- abs_days_teachers[[scenario]][it, 1] + abs_days_teachers[[scenario]][it, 2]
    
    # Number of absences
    colnames(absences_teachers[[scenario]]) <- c("Isolated", "Quarantined", "Total absent")
    absences_teachers[[scenario]][it, "Isolated"] <- length(unique(df_teach_hist[df_teach_hist$iso_state=="IH","id"]))
    absences_teachers[[scenario]][it, "Quarantined"] <- length(unique(df_teach_hist[df_teach_hist$iso_state=="Q","id"]))
    absences_teachers[[scenario]][it, "Total absent"] <- absences_teachers[[scenario]][it, 1] + absences_teachers[[scenario]][it, 2]
    
    sec_cases_students[[scenario]][[it]] <- as.numeric(table(df_agent$infector))
    sec_cases_teachers[[scenario]][[it]] <- as.numeric(table(df_teacher$infector))
  }
  end_time <- Sys.time()
  run_time <- end_time-start_time
  print(paste0("Runtime = ", run_time))
  
  ns_list[[scenario]] <- ns
  nt_list[[scenario]] <- nt
}
total_end_time <- Sys.time()
total_run_time <- total_end_time-total_start_time
print(paste0("Total runtime = ", total_run_time))

# Saving output
save.image(file=paste0(resultsPath,folder,'/sim.RData'))

