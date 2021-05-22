source("covid19school_functions_diseaseChar.R")
source("covid19school_risk_testing_function.R")
source("covid19school_screening_function.R")
source("covid19school_quarantine_isolation_function.R")
source("covid19school_transmission_function.R")
source("covid19school_external_foi_function.R")
source("covid19school_plotting_functions.R")

# ============================================================================ #
# Packages necessary
# Automatically check whether packages are installed and load them
# ============================================================================ #
check.and.install.pkgs <- function(pkgs, lib_path=NULL){
  new.packages <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, lib=lib_path)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
# ============================================================================ #
# Transform for plotting
# ---------------------------------------------------------------------------- #
# Input:
# time_steps = Number of time steps in one simulation
# iter =  Number of iterations/simulations
# nn = List with number of S, IA, PS, IS, R, IH, Q 
# Output: 
# Data frame with mean, 95% quantile 
transform.df <- function(time_steps, 
                         iter,       
                         nn, 
                         tol=1e-5){  
  names(nn) <- c("Susceptible", "Presymptomatic", "Symptomatic", "Asymptomatic", 
                 "Recovered", "Isolated", "Quarantined")
  df_mean <- df_cl <- df_cu <- as.data.frame(matrix(time_steps, ncol=1))
  colnames(df_mean) <- colnames(df_cl) <- colnames(df_cu) <- "time"
  for(i in 1:length(nn)){
    df <- as.data.frame(cbind(time_steps,nn[[i]]))
    colnames(df) <- c("time",1:iter)
    df_mean <- cbind(df_mean, sapply(time_steps, function(x) mean(unlist(df[abs(df$time-x)<=tol,-1]))))
    df_cl <- cbind(df_cl, sapply(time_steps, function(x) quantile(unlist(df[abs(df$time-x)<=tol,-1]), probs=0.025)))
    df_cu <- cbind(df_cu, sapply(time_steps, function(x) quantile(unlist(df[abs(df$time-x)<=tol,-1]), probs=0.975)))
  }
  colnames(df_mean) <- colnames(df_cl) <- colnames(df_cu) <- c("time", names(nn))
  
  data_mean <- reshape::melt(df_mean, id="time")
  colnames(data_mean)[3] <- "mean"
  data_cl <- reshape::melt(df_cl, id="time")
  colnames(data_cl)[3] <- "ci_lower"
  data_cu <- reshape::melt(df_cu, id="time")
  colnames(data_cu)[3] <- "ci_upper"
  
  data_ci <- merge(data_cl, data_cu)
  data <- merge(data_mean, data_ci)
  
  return(data)
}

# ============================================================================ #
# Generate day of the week 
# ---------------------------------------------------------------------------- #
# Input:
# time_steps = Numeric vector with time steps of study period wrt 1 day
# day_names = String vector with names of days to be used
# ---------------------------------------------------------------------------- #
# Output: 
# time_names = String vector with name
# ============================================================================ #
day.of.the.week <- function(time_steps, day_names=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")){
  time_names <- NULL
  fl_time_steps <- floor(time_steps)
  unique_fl_time <- unique(fl_time_steps)
  for(t in unique_fl_time){
    n_rep <- sum(fl_time_steps==t)
    time_names <- c(time_names, rep(day_names[ifelse(t%%7>0, t%%7, 7)], times=n_rep))
  }
  return(time_names)
}

# ============================================================================ #
# Functions to create when contacts will take place
contacts.in.school <- function(time_steps, time_names, t_school=8/24){
  cont_in_school <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,2)<round(t_school,1) & !time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  return(cont_in_school)
}
contacts.leisure <- function(time_steps, time_names, t_school=8/24, t_out_school=16/24){
  cont_1 <- sapply(1:length(time_steps), function(i) ifelse(time_steps[i]%%1>round(t_school,1) & round(time_steps[i],1)%%1<round(t_out_school,2) & !time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  cont_2 <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,1)<=round(t_out_school,2) & time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  cont_out_school <- as.numeric(sapply(1:length(cont_1), function(x) cont_1[x]| cont_2[x]))
  return(cont_out_school)
}
no.contacts <- function(time_steps, t_out_school=16/24){
  no_contacts <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,1)>t_out_school,1,0))
  return(no_contacts)
}


# ============================================================================ #
# Assign teachers to subjects
assign.teachers <- function(df_teacher, grades, subjects, initial=F){
  if(initial) id <- 1
  ind <- which(df_teacher$grade%in%grades & df_teacher$subject%in%subjects)
  remaining <- 1:nrow(df_teacher[ind,])
  while(length(remaining)>0){
    temp <- sample(remaining, size = length(subjects))
    remaining <- setdiff(remaining, temp)
    df_teacher[ind[temp], "id"] <- id
    id <- id + 1
  }
  return(list(df_teacher=df_teacher, 
              id=id))
}

# ============================================================================ #
# Update susceptible contacts
# ---------------------------------------------------------------------------- #
# Input: 
# t_i = current time
# df_history = dataframe with information per day
# susc_list = list of susceptible contacts per individual
# Output:
# Updated susc_list
# ============================================================================ #
update.susc.contacts <- function(t_i, df_history, susc_list, tol=1e-5){
  susc_ids <- df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
  return(lapply(susc_list, function(x) intersect(unlist(x), susc_ids)))
}

# ============================================================================ #
# Function to sample contacts of students per day
# NOT USED AT THE MOMENT
# ============================================================================ #
sample.contacts <- function(t_i, df_history, tol=1e-5){
  cont_close <- cont_class <- cont_grade <- cont_other_grades <- cont_out_school <- list()
  susc_close <- susc_class <- susc_grade <- susc_other_grades <- susc_out_school <- list()
  student_ids <- unique(df_agent$id)
  remove_ids <- unlist(df_history %>% filter(abs(time-t_i)<=tol & state!="S") %>% summarize(id))
  for(i in 1:length(student_ids)){
    id <- student_ids[i]
    row <- which(df_agent$id==id)
    class <- df_agent[row, "class"]
    grade <- df_agent[row, "grade"]
    students_class <- df_agent[df_agent$class==class & df_agent$grade==grade, "id"] # Id of students that are in the same class
    students_grade <- setdiff(df_agent[df_agent$grade==grade, "id"], students_class) # Id of students that are in the same grade but not in the same class
    students_class <- setdiff(students_class, id) # Remove the current student from students_class
    
    cont_close[[i]] <- sample(students_class, n_cont_close)
    susc_close[[i]] <- setdiff(cont_close[[i]], remove_ids)
    cont_class[[i]] <- sample(setdiff(students_class, cont_close[[i]]), n_cont_class[grade])
    susc_class[[i]] <- setdiff(cont_class[[i]], remove_ids)
    cont_grade[[i]] <- sample(students_grade, n_cont_grade)
    susc_grade[[i]] <- setdiff(cont_grade[[i]], remove_ids)
    cont_out_school[[i]] <- sample(c(students_class, students_grade), n_cont_out_school)
    susc_out_school[[i]] <- setdiff(cont_out_school[[i]], remove_ids)
  }
  names(cont_close) <- names(cont_class) <- names(cont_grade) <- names(cont_out_school) <- student_ids
  names(susc_close) <- names(susc_class) <- names(susc_grade) <- names(susc_out_school) <- student_ids
  
  return(list(cont_close=cont_close, 
              cont_class=cont_class, 
              cont_grade=cont_grade, 
              cont_out_school=cont_out_school,
              susc_close=susc_close, 
              susc_class=susc_class, 
              susc_grade=susc_grade, 
              susc_out_school=susc_out_school))
}

# ============================================================================ #
# SYMPTOM ONSET
# ---------------------------------------------------------------------------- #
# Input:
# df_history = dataframe with information per day
# df_agent = dataframe with information per individual
# new_inf_ids = ids of newly infected individuals
# prop_symp = Proportion of symptomatically infected individuals
# Output:
# Updated df_history and df_agent
# ============================================================================ #
symptom.onset <- function(df_history, df_agent, new_inf_ids, teacher=F, vaccination_flag=F, tol=1e-5){
  # Sample from newly infected individuals who will be symptomatically infected 
  prop_symp_vec <- prop.symp(n=length(new_inf_ids), teacher=teacher, vaccination_flag=vaccination_flag)
  symp_inf <- sapply(prop_symp_vec, function(x) sample(c(0,1), size=1, prob=c(1-x, x)))
  symp_ids <- new_inf_ids[symp_inf==1]
  nonsymp_ids <- new_inf_ids[symp_inf==0]
  # Update data frames for symptomatically infected 
  if(length(symp_ids)>0){
    df_agent[df_agent$id%in%symp_ids, "state"] <- "IS"
    df_agent[df_agent$id%in%symp_ids, "inc"] <- inc.period(length(symp_ids))
    t_inf <- df_agent[df_agent$id%in%symp_ids, "inf"]
    inc_days <- df_agent[df_agent$id%in%symp_ids, "inf"] + df_agent[df_agent$id%in%symp_ids, "inc"]
    for(x in 1:length(symp_ids)){ # Set incubation time
      df_history[df_history$id==symp_ids[x] & df_history$time+tol>=t_inf[x], "state"] <- "PS"
      ind_temp <- which(df_history$id==symp_ids[x] & df_history$time+tol>=inc_days[x])
      df_history[ind_temp, "state"] <- rep("IS", length(ind_temp))
    }
  }
  # Update data frames for asymptomatically infected 
  if(length(nonsymp_ids)>0){ 
    t_inf <- df_agent[df_agent$id%in%nonsymp_ids, "inf"]
    df_agent[df_agent$id%in%nonsymp_ids, "state"] <- "IA"
    for(x in 1:length(nonsymp_ids)){
      ind_temp <- which(df_history$id==nonsymp_ids[x] & df_history$time+tol>=t_inf[x])
      df_history[ind_temp, "state"] <- rep("IA", length(ind_temp))
    }
  }
  return(list(df_history = df_history,
              df_agent = df_agent))
}

# ============================================================================ #
# Recovery
# ---------------------------------------------------------------------------- #
# Input:
# t_i = current time
# df_history = dataframe with information per day
# df_agent = dataframe with information per individual
# rec_time = recovery time
# Output:
# Updated df_history and df_agent  
# ============================================================================ #
recovery <- function(t_i, df_history, df_agent, rec_time=7, tol=1e-5){
  # curr_infected can be IS, IA
  curr_inf_ids <- unique(df_history[df_history$state%in%c("IA","IS") & abs(df_history$time-t_i)<=tol,"id"])
  curr_inf_ids <- intersect(df_agent[df_agent$recovered==0, "id"], curr_inf_ids)
  if(length(curr_inf_ids)>0){
    inc_time <- sapply(df_agent[df_agent$id%in%curr_inf_ids, "inc"], function(x) ifelse(x>=0, x, 0))
    inf_days <- df_agent[df_agent$id%in%curr_inf_ids, "inf"] + inc_time
    # Recovery
    recovered_ids <- curr_inf_ids[sapply(inf_days, function(x) t_i+tol>=x+rec_time)]
    df_history[df_history$id%in%recovered_ids & df_history$time+tol>=t_i,"state"] <- "R"
    df_agent[df_agent$id%in%recovered_ids, "recovered"] <- rep(1, length(recovered_ids))
  }
  return(list(df_history = df_history, 
              df_agent = df_agent))
}

# Testing function
testing <- function(t_i, df_history, df_agent, curr_noninf, curr_inf, tol=1e-5){
  FP_ind <- pos_ind <- pos_tested <- pos_tested_ids <- FP_ids <- probs <- NULL
  # False positives among non-infected individuals
  # Accounting for imperfect specificity
  if(length(curr_noninf$id)>0){
    FP <- sample(c(0,1), size=length(curr_noninf$id), prob=c(spec,1-spec), replace=T)
    FP_ids <- curr_noninf$id[FP==1]
    FP_ind <- unlist(lapply(FP_ids, function(x) which(df_history$id==x & df_history$time+tol>=t_i & df_history$time<=t_i+fp_iso_time+tol)))
  }
  # Testing of infected individuals
  # Accounting for imperfect sensitivity
  if(length(curr_inf$id)>0){
    t_since_inf <- t_i-df_agent[df_agent$id%in%curr_inf$id, "inf"]
    probs <- predict(test_sens_fun, ifelse(t_since_inf<0, 0, t_since_inf))$y
    probs <- ifelse(probs>1, 1, probs)
    probs <- ifelse(probs<0, 0, probs)
    pos_tested <- sapply(probs, function(x) sample(c(0,1), size=1, prob = c(1-x, x)))
    pos_tested_ids <- curr_inf$id[pos_tested==1]
    pos_ind <- unlist(lapply(pos_tested_ids, function(x) which(df_history$id==x & df_history$time+tol>=t_i & df_history$time<=t_i+iso_time+tol)))
  }
  return(list(pos_ind=pos_ind, 
              FP_ind=FP_ind, 
              pos_tested=pos_tested, 
              pos_tested_ids=pos_tested_ids, 
              FP_ids = FP_ids,
              probs=probs))
}

