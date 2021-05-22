# ============================================================================ #
# Isolation
# ---------------------------------------------------------------------------- #
# Input:
# t_i = current time
# df_history = dataframe with information per day
# df_agent = dataframe with information per individual
# df_inf_sens = dataframe with sensitivity of diagnostic tests
# Output:
# Updated df_history and df_agent
# curr_symp_ids = ids of currently symptomatic individuals
# pos_tested_ids = ids of positive tested individuals
# ============================================================================ #
isolation <- function(t_i, df_history, df_agent, tol = 1e-5){
  # First determine the individuals that developed symptoms
  # Then test them with an antigen test
  # Then put them in isolation if antigen test is positive
  # Only symptom onset
  curr_symp_ids <- unlist(df_history %>% filter(state=="IS", abs(time-t_i)<=tol, iso_state=="P") %>% summarize(id))
  # curr_symp_ids <- unique(df_agent[df_agent$state=="IS" & df_agent$inf+df_agent$inc<=t_i, "id"])
  if(length(curr_symp_ids)>0){
    t_inc <- df_agent[df_agent$id%in%curr_symp_ids, "inf"] + df_agent[df_agent$id%in%curr_symp_ids, "inc"]
    t_since_inf <- t_i-df_agent[df_agent$id%in%curr_symp_ids, "inf"]
    probs <- predict(pcr_test_sens_fun, ifelse(t_since_inf>0, 1, t_since_inf))$y
    probs <- ifelse(probs>1, 1, probs)
    probs <- ifelse(probs<0, 0, probs)
    # probs <- df_inf_sens[df_inf_sens$time%in%t_since_inf, "pcr_sens"] # Prob that symptomatic individual was tested positive      
    pos_tested <- sapply(probs, function(x) sample(c(0,1), size=1, prob = c(1-x, x))) 
    pos_tested_ids <- curr_symp_ids[pos_tested==1]
    t_inc_pos <- t_inc[pos_tested==1]
    if(length(pos_tested_ids)>0){
      # Isolate positive tested individuals
      for(x in 1:length(pos_tested_ids)){
        t_inc_pos <- df_agent[df_agent$id==pos_tested_ids[x], "inf"] + df_agent[df_agent$id==pos_tested_ids[x], "inc"]
        ind <- which(df_history$state=="IS" & df_history$id==pos_tested_ids[x] & df_history$time+tol>=t_inc_pos & df_history$time<=t_inc_pos+iso_time+tol)
        df_history[ind,"iso_state"] <- "IH"
        # df_history[ind,"pres"] <- 0
      }      
      # ind <- unlist(lapply(1:length(pos_tested_ids), function(x) which(df_history$state=="IS" & df_history$id==pos_tested_ids[x] & df_history$time>=t_inc_pos[x] & df_history$time<=t_inc_pos[x]+iso_time)))
      # df_history[ind,"state"] <- "IH"
      df_agent[df_agent$id%in%pos_tested_ids, "iso_time"] <- rep(t_i, length(pos_tested_ids))
      df_agent[df_agent$id%in%pos_tested_ids, "iso"] <- rep(1, length(pos_tested_ids))  
    }else pos_tested_ids <- NULL
  }else pos_tested_ids <- NULL
  
  return(list(df_history = df_history, 
              df_agent = df_agent,
              curr_symp_ids = curr_symp_ids,
              pos_tested_ids = pos_tested_ids))
}

# ============================================================================ #
# Quarantine close contacts and n_quaran other contacts in other grades
# ---------------------------------------------------------------------------- #
# All close contacts of index case have to quarantine without a test upon 
# symptom onset of index case
# ============================================================================ #
quarantine.close.contacts <- function(t_i, 
                                      df_history, df_agent, 
                                      df_teach_hist, df_teacher, 
                                      pos_tested_ids, 
                                      cont_close, cont_class, cont_grade, cont_teacher,
                                      susc_close, susc_class, susc_grade, susc_out_school, 
                                      susc_teacher, susc_tt, susc_ts,
                                      tol=1e-5){
  for(i in pos_tested_ids){
    ind <- which(as.numeric(names(cont_close))==i)
    close_cont_ids <- unlist(cont_close[[ind]])
    # Randomly choose n_quaran contacts from contact list (excluding classmates)
    # that have to immediately quarantine as well
    # Note: independent of infection status
    contacts <- unlist(cont_grade[[ind]])
    len_cont_grade <- length(contacts)
    if(len_cont_grade>0){
      sample_size <- min(len_cont_grade, n_quaran)
      quaran_ids <- c(contacts[sample(sample_size)], close_cont_ids)
    }else{
      quaran_ids <- close_cont_ids
    }

    quaran_ids <- quaran_ids[order(match(quaran_ids, unique(df_agent$id)))]
    # Check whether those students were already quarantined or isolated? Not implemented yet
    if(length(quaran_ids)>0){
      # df_teach_hist <- df_teach_hist %>% mutate(state=replace(state, id%in%teacher_id & time>=t_i & time<=t_i+iso_time, "Q"))
      # df_teacher[df_teacher$id%in%teacher_id, "quaran"] <- rep(1, length(teacher_id))
      ind_time <- intersect(which(df_history$time+tol>=t_i), which(df_history$time<=t_i+quaran_time+tol))
      ind_Q <- intersect(ind_time, which(df_history$id%in%quaran_ids))
      df_history[ind_Q, "iso_state"] <- rep("Q", length(ind_Q))
      # df_history[ind_Q, "pres"] <- rep(0, length(ind_Q))
      df_agent[df_agent$id%in%quaran_ids, "quaran"] <- rep(1, length(which(df_agent$id%in%quaran_ids)))
      df_agent[df_agent$id%in%quaran_ids, "iso_time"] <- rep(t_i, length(which(df_agent$id%in%quaran_ids)))
      # Remove quarantined individuals from eligible susceptible contacts
      susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), quaran_ids))
      susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), quaran_ids))
      susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), quaran_ids))
      susc_out_school <- lapply(susc_out_school, function(x) setdiff(unlist(x), quaran_ids))
      susc_ts <- lapply(susc_ts, function(x) setdiff(unlist(x), quaran_ids))
    }
  }
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade,
              susc_out_school = susc_out_school,
              susc_teacher = susc_teacher,
              susc_tt = susc_tt,
              susc_ts = susc_ts))
}


test.after.quarantine <- function(t_i, 
                                  df_history, df_agent, 
                                  df_teach_hist, df_teacher, 
                                  cont_close, cont_class, cont_grade, cont_teacher,
                                  susc_close, susc_class, susc_grade, susc_out_school, 
                                  susc_teacher, susc_tt, susc_ts,
                                  tol=1e-5){
  # Identify student ids that are eligible for being released
  to_be_released <- df_agent[df_agent$quaran==1 & abs(df_agent$iso_time+5-t_i)<=tol,]
  curr_noninf <- to_be_released[to_be_released$state%in%c("S","R"),]
  curr_inf <- to_be_released[to_be_released$state%in%c("PS","IS","IA"),]
  # Test those studemts
  tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
  released_ids <- setdiff(to_be_released$id, c(tested$FP_ids, tested$pos_tested_ids))
  # Release students that were tested negative from quarantine
  ind_released <- unlist(lapply(released_ids, function(x) which(df_history$id==x & df_history$time>=t_i)))
  df_history[ind_released, "iso_state"] <- "P"
  # Update susceptible contacts (released students may have contacts again)
  susc_close <- update.susc.contacts(t_i, df_history, susc_close)
  susc_class <- update.susc.contacts(t_i, df_history, susc_class)
  susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
  susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
  susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
  
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade,
              susc_out_school = susc_out_school, 
              susc_ts = susc_ts))
}



