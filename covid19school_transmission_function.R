# ============================================================================ #
# Transmission events
# ---------------------------------------------------------------------------- #
# 
# ============================================================================ #
transmission.events <- function(t_i, 
                                time_names,
                                df_history, 
                                df_agent, 
                                df_teach_hist, 
                                df_teacher, 
                                df_teacher_gcs,
                                susc_close, 
                                susc_class, 
                                susc_grade, 
                                susc_out_school, 
                                susc_teacher, 
                                susc_tt, 
                                susc_ts,
                                infecteds=NA, 
                                contacts_in_school, 
                                contacts_out_school,
                                vaccination_flag=F, 
                                sample_prop=1, 
                                cont_susc_scale=c(0.5,0.85),
                                tol=1e-5){
  new_inf <- new_inf_id <- NULL
  # -------------------------------------------------------------------------- #
  # Transmission from students
  # -------------------------------------------------------------------------- #
  # Determine which students are currently infected and present in school
  # df_infected contains information on day, grade, class, student
  df_infected <- df_history %>% filter(state%in%c("PS","IA"), 
                                       !iso_state%in%c("Q","IH"), 
                                       abs(time-t_i)<=tol, 
                                       pres==1)   
  inf_id <- unique(df_infected$id) # Ids of currently infected individuals
  # For each infected individual, perform transmission events to their susceptible contacts
  for(i in inf_id){
    student_infections <- infect.susc(i, t_i, time_names, 
                                      df_history, df_agent, 
                                      df_teach_hist, df_teacher, df_teacher_gcs,
                                      susc_close, susc_class, susc_grade, 
                                      susc_out_school, susc_teacher, 
                                      susc_tt, susc_ts,
                                      infecteds, teacher=F, 
                                      contacts_in_school, contacts_out_school,
                                      vaccination_flag=vaccination_flag,
                                      sample_prop=sample_prop,
                                      cont_susc_scale = cont_susc_scale)
    df_history <- student_infections$df_history
    df_agent <- student_infections$df_agent
    df_teach_hist <- student_infections$df_teach_hist
    df_teacher <- student_infections$df_teacher
    susc_close <- student_infections$susc_close
    susc_class <- student_infections$susc_class
    susc_grade <- student_infections$susc_grade
    susc_out_school <- student_infections$susc_out_school
    susc_teacher <- student_infections$susc_teacher
    susc_tt <- student_infections$susc_tt
    susc_ts <- student_infections$susc_ts
    infecteds <- student_infections$infecteds
  } 
  # -------------------------------------------------------------------------- #
  # Transmissions from teachers
  # -------------------------------------------------------------------------- #
  # Determine which teachers are currently infected and neither isolated nor 
  # quarantined
  df_infected <- df_teach_hist %>% filter(state%in%c("PS","IA"), !iso_state%in%c("Q","IH"), abs(time-t_i)<=tol)   
  inf_id <- unique(df_infected$id) # Ids of currently infected individuals
  for(i in inf_id){
    teacher_infections <- infect.susc(i, t_i, time_names, 
                                      df_history, df_agent, 
                                      df_teach_hist, df_teacher, df_teacher_gcs, 
                                      susc_close, susc_class, susc_grade, 
                                      susc_out_school, susc_teacher, 
                                      susc_tt, susc_ts,
                                      infecteds, 
                                      teacher=T, 
                                      contacts_in_school, contacts_out_school,
                                      vaccination_flag=vaccination_flag, 
                                      sample_prop=sample_prop,
                                      cont_susc_scale = cont_susc_scale)
    df_history <- teacher_infections$df_history
    df_agent <- teacher_infections$df_agent
    df_teach_hist <- teacher_infections$df_teach_hist
    df_teacher <- teacher_infections$df_teacher
    susc_close <- teacher_infections$susc_close
    susc_class <- teacher_infections$susc_class
    susc_grade <- teacher_infections$susc_grade
    susc_teacher <- teacher_infections$susc_teacher
    susc_tt <- teacher_infections$susc_tt
    susc_ts <- teacher_infections$susc_ts
    infecteds <- teacher_infections$infecteds
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
              susc_ts = susc_ts,
              infecteds=infecteds))
}

# ============================================================================ #
# Infection of susceptible contacts of index case
# ---------------------------------------------------------------------------- #
infect.susc <- function(i, t_i, time_names,
                        df_history, 
                        df_agent, 
                        df_teach_hist, 
                        df_teacher, 
                        df_teacher_gcs=NULL, 
                        susc_close=NULL, 
                        susc_class=NULL, 
                        susc_grade=NULL, 
                        susc_out_school=NULL, 
                        susc_teacher=NULL, 
                        susc_tt=NULL, 
                        susc_ts=NULL,
                        infecteds=NA, 
                        teacher=F, 
                        contacts_in_school, 
                        contacts_out_school, 
                        vaccination_flag=F, 
                        sample_prop=1, 
                        cont_susc_scale=c(0.5,0.85),
                        tol=1e-5){
  
  # -------------------------------------------------------------------------- #
  # Compute transmission probability and determine susceptible contacts
  # -------------------------------------------------------------------------- #
  if(teacher){ ### Teacher is index case
    if(contacts_in_school[abs(time_steps-t_i)<=tol]==1){
      source <- df_teacher[df_teacher$id==i,"source"] # Source of infector i
      # Identify day since infection of that specific individual at time t_i
      t_since_inf <- t_i - df_teacher[df_teacher$id==i, "inf"]
      # Flag whether infected student is asymptomatically infected
      asymp <- ifelse(df_teacher[df_teacher$id==i, "state"]=="IA", 1, 0)
      # ---------------------------------------------------------------------- # 
      # Translate into infectiousness on day d and transmission prob per contact
      transmprob <- prob.trans(time = ifelse(t_since_inf<0, 0, t_since_inf), 
                               infec = F, susc = F, asymp = as.logical(asymp), 
                               vacc = F)
      # Susceptible teachers in contact with infected teacher
      susc_student_ids <- susc_ts[[i]]      
      # Susceptible teachers in contact with infected teacher
      ind <- which(names(susc_tt)==i)
      susc_teacher_ids <- unlist(susc_tt[[ind]])
    }else susc_student_ids <- susc_teacher_ids <- NULL
  }else{ ### Student is index case
    source <- df_agent[df_agent$id==i,"source"] # Source of infector i
    # Identify day since infection of that specific individual at time t_i
    t_since_inf <- t_i - df_agent[df_agent$id==i, "inf"]
    # Flag whether infected student is asymptomatically infected
    asymp <- ifelse(df_agent[df_agent$id==i, "state"]=="IA", 1, 0)
    # ------------------------------------------------------------------------ # 
    # Translate into infectiousness on day d and transmission prob per contact
    transmprob <- prob.trans(time = ifelse(t_since_inf<0, 1, t_since_inf), 
                             infec = T, susc = F, asymp = as.logical(asymp), 
                             vacc = F)
    # Determine susceptible contacts of infected student
    # Get index of infected id in contact lists
    ind <- which(as.numeric(names(cont_close))==i)
    # Susceptible contacts are dependent on whether student is currently
    # in school or outside school
    if(contacts_out_school[abs(time_steps-t_i)<=tol]==1){
      susc_student_ids <- intersect(unlist(susc_out_school[[ind]]), unique(df_history[df_history$state=="S" & abs(df_history$time-t_i)<=tol,"id"]))
      susc_teacher_ids <- NULL  
    }   
    if(contacts_in_school[abs(time_steps-t_i)<=tol]==1){
      susc_student_ids <- c(unlist(susc_close[[ind]]), unlist(susc_class[[ind]]), unlist(susc_grade[[ind]]))
      susc_student_ids <- intersect(susc_student_ids, unique(df_history[df_history$pres==1 & abs(df_history$time-t_i)<=tol,"id"]))
      susc_teacher_ids <- unlist(susc_teacher[[ind]])
    }
  }
  # -------------------------------------------------------------------------- # 
  # Infect susceptible contacts
  # Sort first
  susc_teacher_ids <- susc_teacher_ids[order(match(susc_teacher_ids, unique(df_teacher$id)))]
  susc_student_ids <- susc_student_ids[order(match(susc_student_ids, unique(df_agent$id)))]
  susc_cont_ids <- c(susc_student_ids, susc_teacher_ids)
  ### Infect susceptible students
  if(length(susc_student_ids)>0){ 
    susc_vec <- susc.scale(length(susc_student_ids))
    # If teacher was index case, then reduce infectiousness
    if(teacher) transmprob <- contact.infec.scale(min=cont_susc_scale[1], max=cont_susc_scale[2])*transmprob
    new_inf <- sapply(susc_vec, function(x) sample(c(0,1), size=1, prob=c(1-x*transmprob, x*transmprob)))
    inf_student_ids <- susc_student_ids[which(new_inf==1)]
    infecteds <- c(infecteds, inf_student_ids)
    if(length(inf_student_ids)>0){ # Students
      # Check whether individuals to be infected are currently susceptible
      if(!all(inf_student_ids%in%unique(df_history[df_history$state=="S" & abs(df_history$time-t_i)<=tol & df_history$iso_state=="P", "id"]))){
        print(paste0("Students to be infected are not susceptible."))
        print(inf_student_ids[which(!inf_student_ids%in%unique(df_history[df_history$state=="S" & abs(df_history$time-t_i)<=tol & df_history$iso_state=="P", "id"]))])
      }
      df_agent[df_agent$id%in%inf_student_ids, "infector"] <- rep(i, length(inf_student_ids)) # Set infector
      df_agent[df_agent$id%in%inf_student_ids, "source"] <- rep(ifelse(is.na(source), i, source), length(inf_student_ids)) # Set source of outbreak
      df_agent[df_agent$id%in%inf_student_ids, "inf"] <- rep(t_i, length(inf_student_ids)) # Set infection time
      df_agent[df_agent$id%in%inf_student_ids, "location"] <- contacts_in_school[abs(time_steps-t_i)<=tol] # 1=in school, 0=outside school
      df_agent[df_agent$id%in%inf_student_ids, "week_day"] <- time_names[abs(time_steps-t_i)<=tol]
      # Symptom onset of newly infected students
      student_symp_onset <- symptom.onset(df_history, df_agent, inf_student_ids, teacher=F)
      df_history <- student_symp_onset$df_history
      df_agent <- student_symp_onset$df_agent
      # Remove newly infected from susceptible contact list
      susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), inf_student_ids))
      susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), inf_student_ids))
      susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), inf_student_ids))
      susc_out_school <- lapply(susc_out_school, function(x) setdiff(unlist(x), inf_student_ids))
      susc_ts <- lapply(susc_ts, function(x) setdiff(unlist(x), inf_student_ids))
      # susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      # susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      # susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      # susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      # susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
    }
  }
  ### Infect susceptible teachers
  if(length(susc_teacher_ids)>0){
    # If teachers are vaccinated (vaccination_flag=1), then reduce susceptibility
    # of teachers to be infected
    if(vaccination_flag){
      temp <- df_teacher[df_teacher$id%in%susc_teacher_ids, "vaccinated"]
      ind_no_vacc <- which(temp==0)
      vacc_vec <- susc.vacc.scale(length(susc_teacher_ids)) # Vector with reduced susceptibility for each susceptible teacher
      vacc_vec[ind_no_vacc] <- rep(1, length(ind_no_vacc))  # Susceptibility not reduced for teachers who are not vaccinated
    }else vacc_vec <- rep(1, length(susc_teacher_ids))
    # Reduce infectiousness of infected individual
    # Depends on whether student-teacher or teacher-teacher contact
    if(!teacher) transmprob <- contact.infec.scale(min=cont_susc_scale[1], max=cont_susc_scale[2])*transmprob
    else transmprob <- cont_susc_scale[2]*transmprob
    new_inf <- sapply(1:length(susc_teacher_ids), function(i) sample(c(0,1), size=1, prob=c(1-vacc_vec[i]*transmprob, vacc_vec[i]*transmprob)))
    inf_teacher_ids <- susc_teacher_ids[which(new_inf==1)]
    infecteds <- c(infecteds, inf_teacher_ids)
    if(length(inf_teacher_ids)>0){ # Teachers
      # Check whether individuals to be infected are currently susceptible
      if(!all(inf_teacher_ids%in%unique(df_teach_hist[df_teach_hist$state=="S" & abs(df_teach_hist$time-t_i)<=tol & df_teach_hist$iso_state=="P", "id"]))){
        print(paste0("Teachers to be infected are not susceptible."))
      }
      df_teacher[df_teacher$id%in%inf_teacher_ids, "infector"] <- rep(i, length(inf_teacher_ids)) # Set infector
      df_teacher[df_teacher$id%in%inf_teacher_ids, "source"] <- rep(ifelse(is.na(source), i, source), length(inf_teacher_ids)) # Set source of outbreak
      df_teacher[df_teacher$id%in%inf_teacher_ids, "inf"] <- rep(t_i, length(inf_teacher_ids)) # Set infection time
      df_teacher[df_teacher$id%in%inf_teacher_ids, "location"] <- 1 # 1=in school, 0=outside school
      df_teacher[df_teacher$id%in%inf_teacher_ids, "week_day"] <- time_names[abs(time_steps-t_i)<=tol]
      # Remove newly infected from susceptible contact list
      susc_teacher <- lapply(susc_teacher, function(x) setdiff(unlist(x), inf_teacher_ids))
      susc_tt <- lapply(susc_tt, function(x) setdiff(unlist(x), inf_teacher_ids))
      # Symptom onset of newly infected teachers
      teacher_symp_onset <- symptom.onset(df_teach_hist, df_teacher, inf_teacher_ids, teacher=T)
      df_teach_hist <- teacher_symp_onset$df_history
      df_teacher <- teacher_symp_onset$df_agent
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
              susc_ts = susc_ts,
              infecteds = infecteds))
} 