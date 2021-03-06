rm(list=ls())
setwd("/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/model_code/")
source("covid19school_packages.R")  # Installs and loads all necessary packages
options(warn=0)                     # Turn warning messages on again
source("covid19school_functions.R") # Loads all relevant functions
source("covid19school_vars.R")      # Load all relevant variables
source("covid19school_init_vars_function.R") 
source("covid19school_epidemic.R")  # Load main function for simulation

# ============================================================================ #
# Starting the simulations
# ============================================================================ #
total_start_time <- Sys.time()
print(paste("Starting the simulations with", iter, "iterations."))
for(scenario in 1){
  # Matrix output template
  mat <- matrix(0, nrow=length(time_steps), ncol=iter)
  ns <- nt <- rep(list(mat),7)
  names(ns) <- names(nt) <- types
  # Start simulation 
  start_time <- Sys.time()
  for(it in 1:iter){
    epidemic <- school.epidemic(seed=it*10, 
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
                                external_prob = external_prob, 
                                cont_close = cont_close, 
                                cont_class = cont_class, 
                                cont_grade = cont_grade, 
                                cont_out_school = cont_out_school,
                                cont_teacher = cont_teacher,
                                cont_tt = cont_tt,
                                cont_ts = cont_ts,
                                susc_close = susc_close, 
                                susc_class = susc_class, 
                                susc_grade = susc_grade, 
                                susc_out_school = susc_out_school, 
                                susc_teacher = susc_teacher, 
                                susc_tt = susc_tt, 
                                susc_ts = susc_ts,
                                sample_prop=sample_prop[scenario], 
                                prop_vaccinated = prop_vaccinated,
                                cont_susc_scale = cont_susc_scale,
                                no_intervention = no_int_vec[scenario],
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
    susc_teacher <- epidemic$susc_teacher
    susc_tt <- epidemic$susc_tt
    susc_ts <- epidemic$susc_ts
    cont_close <- epidemic$cont_close
    cont_class <- epidemic$cont_class
    cont_grade <- epidemic$cont_grade
    cont_out_school <- epidemic$cont_out_school
    cont_teacher <- epidemic$cont_teacher
    cont_tt <- epidemic$cont_tt
    cont_ts <- epidemic$cont_ts
    # For plotting
    for(j in 1:length(ns)){
      # Students
      ns[[j]][,it] <- sapply(unique(df_history$time), function(x) nrow(df_history %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
      # Teachers
      nt[[j]][,it] <- sapply(unique(df_teach_hist$time), function(x) nrow(df_teach_hist %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
    }
    # Incidence of symptomatic individuals
    new_symp_t <- sort(df_agent$iso_time[which(df_agent$iso_time>=0)])
    new_symp[[scenario]][it,1] <- length(new_symp_t[new_symp_t<=7])
    new_symp[[scenario]][it,2] <- length(new_symp_t[new_symp_t>7 & new_symp_t<=14])
    new_symp[[scenario]][it,3] <- length(new_symp_t[new_symp_t>14 & new_symp_t<=21])
    new_symp[[scenario]][it,4] <- length(new_symp_t[new_symp_t>21 & new_symp_t<=28])
    
    
    # Contacts between teachers and students
    contact_student_teachers[[scenario]][[it]] <- unlist(lapply(cont_ts, function(x) length(x)))
    
    # Outbreak size students
    outbreak_size_students[[scenario]][it] <- sum(!is.na(df_agent$infector))
    outbreak_size_teachers[[scenario]][it] <- sum(!is.na(df_teacher$infector))
    
    # Outbreak size per introduction
    outbreak_data_students[[scenario]][[it]] <- table(df_agent$source)
    outbreak_data_teachers[[scenario]][[it]] <- table(df_teacher$source)
    
    # Infector 
    infector_students[[scenario]][[it]] <- table(df_agent$infector)
    infector_teachers[[scenario]][[it]] <- table(df_teacher$infector)
    
    # Symptomatic cases
    symptomatic_students[[scenario]][[it]]<- length(which(df_agent$state=="IS"))
    symptomatic_teachers[[scenario]][[it]]<- length(which(df_teacher$state=="IS"))
    
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
    
    infected_students <- unique(df_agent[!is.na(df_agent$location),"id"])
    infected_teachers <- unique(df_teacher[!is.na(df_teacher$location),"id"])
    sec_cases_students[[scenario]][[it]] <- sapply(infected_students, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    sec_cases_teachers[[scenario]][[it]] <- sapply(infected_teachers, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    
    infected_symp_students <- unique(df_agent[!is.na(df_agent$location) & df_agent$state=="IS","id"])
    sec_symp_students[[scenario]][[it]] <- sapply(infected_symp_students, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    infected_symp_teachers <- unique(df_teacher[!is.na(df_teacher$location) & df_teacher$state=="IS","id"])
    sec_symp_teachers[[scenario]][[it]] <- sapply(infected_symp_teachers, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    
    # Detected students and teachers by risk-based testing
    det_risk_students[[scenario]][[it]] <- sum(df_risk_testing$pos_students)
    det_risk_teachers[[scenario]][[it]] <- sum(df_risk_testing$pos_teachers)
    
    # Infected students detected in quarantine (symptomatic)
    # Only student go into quarantine
    IS_quaran_students[[scenario]][[it]] <- length(df_agent[df_agent$quaran==1 & df_agent$state=="IS", "id"])
    IA_quaran_students[[scenario]][[it]] <- length(df_agent[df_agent$quaran==1 & df_agent$state=="IA", "id"])
    
    # Asymptomatically infected students and teachers in isolation
    IA_iso_students[[scenario]][[it]] <- length(df_agent[df_agent$iso==1 & df_agent$state=="IA", "id"])
    IA_iso_teachers[[scenario]][[it]] <- length(df_teacher[df_teacher$iso==1 & df_teacher$state=="IA", "id"])
  }
  end_time <- Sys.time()
  run_time <- end_time-start_time
  print(paste0("Runtime for scenario ", scenario, ": ", run_time))
  
  ns_list[[scenario]] <- ns
  nt_list[[scenario]] <- nt
}
total_end_time <- Sys.time()
total_run_time <- total_end_time-total_start_time
print(paste0("Total runtime = ", total_run_time))

# Saving output
save.image(file=paste0(resultsPath,folder,'/sim_1.RData'))

