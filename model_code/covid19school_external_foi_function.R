# ============================================================================ #
# Introductions from community
# ============================================================================ #
external.introductions <- function(t_i, 
                                   df_history, 
                                   df_agent, 
                                   external_prob=0.0006341289, 
                                   teacher=F,
                                   tol=1e-5){
  external_inf_ids <- NULL
  # Determine individuals that are currently susceptible
  curr_susc_ids <- unique(df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$iso_state=="P", "id"])
  if(length(curr_susc_ids)>0){
    susc_scale_vec <- susc.scale(length(curr_susc_ids))
    susc_scale_vec <- sapply(1:length(curr_susc_ids), function(x) max(as.numeric(!teacher)*susc_scale_vec[x], as.numeric(teacher)*1))
    external_inf <- sapply(1:length(curr_susc_ids), function(x) sample(c(0,1), size=1, prob=c(1-susc_scale_vec[x]*external_prob, susc_scale_vec[x]*external_prob)))
    external_inf_ids <- curr_susc_ids[external_inf==1]
    if(length(external_inf_ids)>0){
      ind <- which(df_agent$id%in%external_inf_ids)
      df_agent[ind, "location"] <- 2
      df_agent[ind, "inf"] <- t_i
      symptom_onset <- symptom.onset(df_history, df_agent, external_inf_ids, teacher=teacher)
      df_history <- symptom_onset$df_history
      df_agent <- symptom_onset$df_agent

    }
  }
  return(list(df_history = df_history,
              df_agent = df_agent, 
              external_inf_ids = external_inf_ids))
}
