# ============================================================================ #
# Parallelized version
# ============================================================================ #
rm(list=ls())
source("covid19school_packages.R")
source("covid19school_plot_template.R")
source("covid19school_plotting_functions.R")
source("covid19school_functions.R")
source("covid19school_vars.R")
source("covid19school_epidemic.R")

### Variables
iter <- 10

### Figures path
figuresPath <- "/figures/parallel/"

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

###
sample_prop=0.5

### Time
unit=24
steps <- 8/24; time_period <- 4*7; time_steps <- seq(1, time_period, by = steps)
group_per_step <- rep(c(rep(1,(1/steps)*(24/unit)),rep(2,(1/steps)*(24/unit))), time_period/2)[1:length(time_steps)] # Assign groups to each time point, assuming 2 groups

### Types of individuals
types <- c("S", "PS", "IS", "IA", "R", "IH", "Q")

### External incidence rate for importations
external_prob = 2*c(0.0007965315, 0.0007965315)

### Testing 
testing_mat <- matrix(c(rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        c(1,0,1,0,0,0,0),
                        c(1,0,0,0,0,0,0)), ncol=7, byrow=T)
colnames(testing_mat) <- day_names
nrow(testing_mat)==length(scenarios)

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
for(scenario in 1){
  # Matrix output template
  mat <- matrix(0, nrow=length(time_steps), ncol=iter)
  ns <- nt <- rep(list(mat),7)
  names(ns) <- names(nt) <- types
  # Start simulation 
  start_time <- Sys.time()
  epidemic_list <- foreach(
    it=1:iter) %dopar% {
      school.epidemic(seed=it, 
                      df_history, df_agent, df_teach_hist, df_teacher,
                      occup = occup[scenario],
                      steps = steps,
                      time_period = time_period, 
                      test_days = testing_mat[scenario,], 
                      day_names = day_names,
                      test_sens = test_sens, spec=spec, 
                      screening_flag = scr_flags[scenario], 
                      risk_based_flag = risk_flags[scenario],
                      vaccination_flag = vacc_flag[scenario],
                      external_prob = external_prob, 
                      cont_close = cont_close, cont_class = cont_class, cont_grade = cont_grade, 
                      cont_teacher = cont_teacher, cont_tt = cont_tt,
                      susc_close = susc_close, susc_class = susc_class, susc_grade = susc_grade, 
                      susc_out_school = susc_out_school, 
                      susc_teacher = susc_teacher, susc_tt = susc_tt, 
                      sample_prop=sample_prop)
    }
} 
total_end_time <- Sys.time()
total_run_time <- total_end_time-total_start_time
print(paste0("Total runtime = ", total_run_time))
