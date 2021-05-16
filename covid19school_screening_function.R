# ============================================================================ # 
# Pre-emptive screening
# ---------------------------------------------------------------------------- #
# Input:
# test_days
# Output:
screening <- function(t_i, 
                      df_history, df_agent, 
                      susc_close=NULL, susc_class=NULL, susc_grade=NULL,
                      susc_out_school=NULL, 
                      susc_teacher=NULL, susc_tt=NULL, susc_ts=NULL, 
                      teacher=T,
                      tol=1e-5){
  curr_noninf <- curr_inf <- pos_tested <- NULL
  # Regular pre-emptive testing
  # Determine ids that are present in school
  curr_df_history <- df_history %>% filter(abs(time-t_i)<=tol)
  curr_ids <- unique(unlist(curr_df_history$id))
  # Determine ids that are eligible for testing
  # Exclude those that were isolated in the past and were therefore infected
  ids_to_be_tested <- df_agent[df_agent$id%in%curr_ids & df_agent$iso==0, "id"]
  curr_to_be_tested <- curr_df_history[curr_df_history$id%in%ids_to_be_tested, ]

  curr_noninf <- curr_to_be_tested %>% filter(state%in%c("S","R"))
  curr_inf <- curr_to_be_tested %>% filter(state%in%c("PS","IA"))
  # Account for adherence
  curr_noninf <- df_agent[df_agent$id%in%curr_noninf$id & df_agent$adherence==1, ]
  curr_inf <- df_agent[df_agent$id%in%curr_inf$id & df_agent$adherence==1, ]
  
  # Perform testing
  tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
  # Change status for positive tested (both false- and true positives)
  ind <- c(tested$FP_ind, tested$pos_ind)
  pos_tested <- c(pos_tested, tested$pos_ind)
  if(length(ind)>0){
    df_history[ind,"iso_state"] <- "IH"
    ids <- unique(df_history[ind,"id"])
    df_agent[df_agent$id%in%ids, "iso"] <- rep(1, length(ids))
    # Remove isolated individuals from eligible susceptible contacts
    if(!teacher){ # Screening of students
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
    }else{ # Screening of teachers
      susc_teacher <- update.susc.contacts(t_i, df_history, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_history, susc_tt)
    }
    # susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), ids))
    # susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), ids))
    # susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), ids))
  }
  return(list(df_history = df_history, 
              df_agent = df_agent, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade, 
              susc_out_school = susc_out_school, 
              susc_teacher = susc_teacher,
              susc_tt = susc_tt, 
              susc_ts = susc_ts,
              curr_noninf = curr_noninf, 
              curr_inf = curr_inf, 
              pos_tested = pos_tested))
}

