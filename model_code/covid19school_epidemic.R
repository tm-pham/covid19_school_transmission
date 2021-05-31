# ============================================================================ #
# Main function 
# ============================================================================ #
school.epidemic <- function(seed = 12345,
                            occup = 0.5,   # Occupation, value = 0.5 or 1
                            steps = 8/24,  # Time steps
                            time_period = 4*7, # Duration of study period 
                            test_days = c(1,0,1,0,0,0,0), # Test days, corresponds to Monday, Tuesday, etc.
                            screening_adherence = 0.5,    # Adherence to screening/testing
                            risk_testing_adherence = 0.5,
                            day_names = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                            spec = 1,      # Specificity of the test
                            screening_flag = F,   # Flag whether screening takes place
                            risk_based_flag = F,  # Flag whether risk-based testing takes place
                            vaccination_flag = F, # Flag whether there is vaccination
                            external_prob = c(0.0006341289, 0.0006341289), # External prob. of infection from community for sutdents and teachers
                            cont_close, cont_class, cont_grade, cont_out_school, cont_teacher, cont_tt, cont_ts, # List with contacts for each student and teacher
                            susc_close, susc_class, susc_grade, susc_out_school, susc_teacher, susc_tt, susc_ts,# List with susceptible contacts for each student and teacher
                            sample_prop=1,  # Proportion of effective contacts (for teachers), samples a subset of students that are educated by teachers
                            prop_vaccinated = c(0, 0.85),
                            cont_susc_scale = c(0.5, 0.85),
                            no_intervention=F,
                            tol=1e-5
                            ){
  set.seed(seed)
  # -------------------------------------------------------------------------- #
  # Time
  # -------------------------------------------------------------------------- #
  t_0 <- 1 # Start time
  time_steps = seq(t_0, time_period, by = steps)
  time_names = day.of.the.week(time_steps, day_names) # Assign day of the week to time_steps
  # For each time step:
  contacts_in_school = contacts.in.school(time_steps, time_names) # Are contacts in school taking place?
  contacts_out_school = contacts.leisure(time_steps, time_names)  # Are contacts outside school taking place?
  no_contacts = no.contacts(time_steps) # Are no contacts taking place? 

  # Initialize data frames for students and teachers and contact network
  # source("covid19school_init_vars.R")
  init <- init.vars(sero_prev_students = 0.25, 
                    sero_prev_teacher = 0.25,
                    occup = occup, 
                    group_per_step = group_per_step, 
                    class_size = class_size, 
                    n_grades = n_grades, 
                    n_cont_same_class = n_cont_same_class, 
                    n_cont_vec = n_cont_vec, 
                    n_cont_close = n_cont_close, 
                    n_cont_out_school - n_cont_out_school, 
                    n_cont_other = n_cont_other, 
                    n_subjects = n_subjects, 
                    n_quaran = n_quaran, 
                    n_cont_teachers = n_cont_teachers)
  df_history <- init$df_history 
  df_agent <- init$df_agent
  df_teach_hist <- init$df_teach_hist 
  df_teacher <- init$df_teacher 
  df_teacher_gcs <- init$df_teacher_gcs
  df_screening <- init$df_screening 
  screening_list <- init$screening_list 
  df_risk_testing <- init$df_risk_testing 
  risk_testing_list <- init$risk_testing_list 
  cont_close <- init$cont_close 
  cont_class <- init$cont_class 
  cont_grade <- init$cont_grade 
  cont_out_school <- init$cont_out_school
  cont_teacher <- init$cont_teacher 
  cont_tt <- init$cont_tt 
  susc_close <- init$susc_close 
  susc_class <- init$susc_class 
  susc_grade <- init$susc_grade 
  susc_out_school <- init$susc_out_school
  susc_teacher <- init$susc_teacher 
  susc_tt <- init$susc_tt
  
    # -------------------------------------------------------------------------- #
  # Vaccinated teachers
  # -------------------------------------------------------------------------- #
  vacc_teachers <- sample(c(0,1), size=nrow(df_teacher), prob=c(1-prop_vaccinated[2], prop_vaccinated[2]), replace=T)
  df_teacher[, "vaccinated"] <- as.numeric(vaccination_flag)*vacc_teachers
  
  # -------------------------------------------------------------------------- #
  # Adherence to screening and risk-based testing
  # -------------------------------------------------------------------------- #
  if(screening_flag){
    df_agent$adherence <- sample(c(0,1), size=length(df_agent$id), prob=c(1-screening_adherence, screening_adherence), replace=T)
    df_teacher$adherence <- sample(c(0,1), size=length(df_teacher$id), prob=c(1-screening_adherence, screening_adherence), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_agent$iso_compliance[df_agent$adherence==0] <- sample(c(0,1), size=length(which(df_agent$adherence==0)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    df_agent$iso_compliance[df_agent$adherence==1] <- sample(c(0,1), size=length(which(df_agent$adherence==1)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_teacher$iso_compliance[df_teacher$adherence==0] <- sample(c(0,1), size=length(which(df_teacher$adherence==0)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    df_teacher$iso_compliance[df_teacher$adherence==1] <- sample(c(0,1), size=length(which(df_teacher$adherence==1)), prob=c(1-compliance_iso, compliance_iso), replace=T)
  } 
  if(risk_based_flag){
    df_agent$adherence <- sample(c(0,1), size=length(df_agent$id), prob=c(1-risk_testing_adherence, risk_testing_adherence), replace=T)
    df_teacher$adherence <- sample(c(0,1), size=length(df_teacher$id), prob=c(1-risk_testing_adherence, risk_testing_adherence), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_agent$quaran_compliance[df_agent$adherence==0] <- sample(c(0,1), size=length(which(df_agent$adherence==0)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    df_agent$quaran_compliance[df_agent$adherence==1] <- sample(c(0,1), size=length(which(df_agent$adherence==1)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_teacher$quaran_compliance[df_teacher$adherence==0] <- sample(c(0,1), size=length(which(df_teacher$adherence==0)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    df_teacher$quaran_compliance[df_teacher$adherence==1] <- sample(c(0,1), size=length(which(df_teacher$adherence==1)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
  }

  inf_count <- 0
  infecteds <- NULL
  pos_tested_ids <- pos_tested_teacher_ids <- NULL
  # -------------------------------------------------------------------------- #
  # SIMULATION
  # -------------------------------------------------------------------------- #
  for(t_i in time_steps){
    if(t_i%%1==0){
      # ---------------------------------------------------------------------- #
      # Sample contacts for teachers and students
      # Note: Less readable but faster than "for loop"
      # ---------------------------------------------------------------------- #
      # Determine classes that teacher educates
      df_gc_list <- lapply(df_teacher$id, function(id_t) df_teacher_gcs[df_teacher_gcs$id==id_t, c("grade","class")])
      # Students that are present in class
      class_student_ids_list <- lapply(df_gc_list, function(df_grade_class) unique(unlist(apply(df_grade_class, 1, function(x) df_history %>% filter(grade==x["grade"],class==x["class"], iso_state=="P", abs(time-t_i)<=tol, pres==1) %>% summarize(id)))))
      # Sample effective contacts
      cont_ts <- lapply(class_student_ids_list, function(class_student_ids) sample(class_student_ids, size=ceiling(sample_prop*length(class_student_ids))))
      names(cont_ts) <- unique(df_teacher$id)
      # Set the same contacts for students (to ensure symmetric contact matrix)
      cont_teacher <- lapply(1:length(cont_teacher), function(x) sapply(1:length(cont_ts), function(y) ifelse(names(cont_teacher)[[x]]%in%cont_ts[[y]], names(cont_ts)[[y]], NA)))
      cont_teacher <- lapply(cont_teacher, function(x) x[!is.na(x)])
      names(cont_teacher) <- unique(df_agent$id)
      susc_teacher <- lapply(cont_teacher, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
      names(susc_teacher) <- unique(df_agent$id)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_ts <- update.susc.contacts(t_i, df_history, cont_ts)  
      
      # ------------------------------------------------------------------------ #
      # Introductions from community
      # ------------------------------------------------------------------------ #
      # Students
      external_inf_students <- external.introductions(t_i, df_history, df_agent, external_prob[1], teacher=F)
      df_history <- external_inf_students$df_history
      df_agent <- external_inf_students$df_agent
      external_inf_student_ids <- external_inf_students$external_inf_ids
      # Teachers
      external_inf_teachers <- external.introductions(t_i, df_teach_hist, df_teacher, external_prob[2], teacher=T)
      df_teach_hist <- external_inf_teachers$df_history
      df_teacher <- external_inf_teachers$df_agent
      external_inf_teacher_ids <- external_inf_teachers$external_inf_ids
      
      # ------------------------------------------------------------------------ #
      # Update the list of susceptible contacts
      # ------------------------------------------------------------------------ #
      # Add everyone who is in "S" to the susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      
      # ------------------------------------------------------------------------ #
      # Isolation of symptomatic individuals
      # ------------------------------------------------------------------------ #
      # Students
      if(!no_intervention){
        isolated_students <- isolation(t_i, df_history, df_agent)
        df_history <- isolated_students$df_history
        df_agent <- isolated_students$df_agent
        pos_tested_ids <- isolated_students$pos_tested_ids
        # Teachers
        isolated_teachers <- isolation(t_i, df_teach_hist, df_teacher)
        df_teach_hist <- isolated_teachers$df_history
        df_teacher <- isolated_teachers$df_agent
        pos_tested_teacher_ids <- isolated_teachers$pos_tested_ids
        # ------------------------------------------------------------------------ #
        # Update the list of susceptible contacts
        # ------------------------------------------------------------------------ #
        # Add everyone who is in "S" to the susceptible contacts
        susc_close <- update.susc.contacts(t_i, df_history, susc_close)
        susc_class <- update.susc.contacts(t_i, df_history, susc_class)
        susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
        susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
        susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
        susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
        susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      }

      # Release students from quarantine after day 5 if antigen test is negative
      if(risk_based_flag){
        released_students <- test.after.quarantine(t_i, 
                                                   df_history, df_agent, 
                                                   df_teach_hist, df_teacher, 
                                                   cont_close, cont_class, cont_grade, cont_teacher,
                                                   susc_close, susc_class, susc_grade, susc_out_school, 
                                                   susc_teacher, susc_tt, susc_ts,
                                                   tol=tol)
        df_history <- released_students$df_history
        df_agent <- released_students$df_agent
        susc_close <- released_students$susc_close
        susc_class <- released_students$susc_class
        susc_grade <- released_students$susc_grade
        susc_out_school <- released_students$susc_out_school
        susc_ts <- released_students$susc_ts
      }
    }
    
    # ------------------------------------------------------------------------ #
    # Recovery
    # ------------------------------------------------------------------------ #
    # Students
    rec_students <- recovery(t_i, df_history, df_agent, rec_time)
    df_history <- rec_students$df_history
    df_agent <- rec_students$df_agent
    # Teachers
    rec_teachers <- recovery(t_i, df_teach_hist, df_teacher, rec_time)
    df_teach_hist <- rec_teachers$df_history
    df_teacher <- rec_teachers$df_agent
    
    # ------------------------------------------------------------------------ #
    # Quarantine of close contacts
    # ------------------------------------------------------------------------ #
    if(!no_intervention){
      # Students
      quarantined_students <- quarantine.close.contacts(t_i, 
                                                        df_history, 
                                                        df_agent, 
                                                        df_teach_hist, 
                                                        df_teacher, 
                                                        pos_tested_ids, 
                                                        cont_close, cont_class, cont_grade, cont_teacher,
                                                        susc_close, susc_class, susc_grade, susc_out_school, 
                                                        susc_teacher, susc_tt, susc_ts,
                                                        tol=tol)
      df_history <- quarantined_students$df_history
      df_agent <- quarantined_students$df_agent
      susc_close <- quarantined_students$susc_close
      susc_class <- quarantined_students$susc_class
      susc_grade <- quarantined_students$susc_grade
      susc_out_school <- quarantined_students$susc_out_school
      susc_ts <- quarantined_students$susc_ts
    }
    
    # ----------------------------------------------------------------------- #
    # Pre-emptive testing
    # ----------------------------------------------------------------------- #
    if(screening_flag){
      if(t_i%%1==0 & time_names[abs(time_steps-t_i)<=tol]%in%names(test_days[test_days==1])){
        ### Screening of students
        screened_students <- screening(t_i, 
                                       df_history, 
                                       df_agent, 
                                       susc_close, susc_class, susc_grade, 
                                       susc_out_school, susc_teacher, 
                                       susc_tt, susc_ts,
                                       teacher=F, tol=tol)
        df_history <- screened_students$df_history
        df_agent <- screened_students$df_agent
        susc_close <- screened_students$susc_close
        susc_class <- screened_students$susc_class
        susc_grade <- screened_students$susc_grade
        susc_out_school <- screened_students$susc_out_school
        susc_ts <- screened_students$susc_ts
        pos_students <- screened_students$pos_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_students)
        ### Screening of teachers
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_students"] <- length(pos_students)
        screened_teachers <- screening(t_i, 
                                       df_teach_hist, 
                                       df_teacher, 
                                       susc_close, susc_class, susc_grade, 
                                       susc_out_school, susc_teacher, 
                                       susc_tt, susc_ts,
                                       teacher=T, tol=tol)
        df_teach_hist <- screened_teachers$df_history
        df_teacher <- screened_teachers$df_agent
        susc_teacher <- screened_teachers$susc_teacher
        susc_tt <- screened_teachers$susc_tt
        pos_teachers <- screened_teachers$pos_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_teachers"] <- length(pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_tested"] <- length(pos_students) + length(pos_teachers)
        if(is.null(df_teach_hist)) print("screening")
      } # Bracket for screening_flag
    }
    
    # Events that take place only on school week days
    if(!time_names[abs(time_steps-t_i)<=tol]%in%c("Saturday", "Sunday")){
      # If there were symptomatic individuals with positive test: risk-based testing
      if(length(pos_tested_ids)>0){
        # -------------------------------------------------------------------- #
        # Risk-based testing
        # -------------------------------------------------------------------- #
        if(risk_based_flag & contacts_in_school[abs(time_steps-t_i)<=tol]==1){
          risk_tested <- risk_based_testing(t_i, 
                                            df_history, 
                                            df_agent, 
                                            df_teach_hist, 
                                            df_teacher, 
                                            df_teacher_gcs,
                                            pos_tested_ids, 
                                            pos_tested_teacher_ids,
                                            cont_close, cont_class, cont_grade, cont_teacher, cont_tt, cont_ts,
                                            susc_close, susc_class, susc_grade, susc_out_school, 
                                            susc_teacher, susc_tt, susc_ts)
          df_history <- risk_tested$df_history
          df_agent <- risk_tested$df_agent
          df_teach_hist <- risk_tested$df_teach_hist
          df_teacher <- risk_tested$df_teacher
          susc_close <- risk_tested$susc_close
          susc_class <- risk_tested$susc_class
          susc_grade <- risk_tested$susc_grade
          susc_out_school <- risk_tested$susc_out_school
          susc_teacher <- risk_tested$susc_teacher
          susc_tt <- risk_tested$susc_tt
          susc_ts <- risk_tested$susc_ts
          ind_time <- abs(df_risk_testing$time-t_i)<=tol
          df_risk_testing[ind_time, "pos_students"] <- length(risk_tested$det_students)
          df_risk_testing[ind_time, "pos_teachers"] <- length(risk_tested$det_teacher)
          df_risk_testing[ind_time, "pos_tested"] <- df_risk_testing[t_i, "pos_students"] + df_risk_testing[t_i, "pos_teachers"] 
          if(is.null(df_teach_hist)) print("risk")
        } # Bracket for Risk-based testing
      }
    } # Bracket for Events only during week days
    # -------------------------------------------------------------------- #
    # Transmission events
    # -------------------------------------------------------------------- #
    if(no_contacts[abs(time_steps-t_i)<=tol]==0){
      # ------------------------------------------------------------------------ #
      # Update the list of susceptible contacts
      # ------------------------------------------------------------------------ #
      # Add everyone who is in "S" to the susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      
      transm <- transmission.events(t_i, time_names,
                                    df_history, df_agent,
                                    df_teach_hist, df_teacher, df_teacher_gcs,
                                    susc_close, susc_class, susc_grade, 
                                    susc_out_school,
                                    susc_teacher, susc_tt, susc_ts,
                                    infecteds,
                                    contacts_in_school, contacts_out_school,
                                    vaccination_flag=vaccination_flag, 
                                    sample_prop=sample_prop,
                                    cont_susc_scale = cont_susc_scale)
      df_history <- transm$df_history
      df_agent <- transm$df_agent
      df_teach_hist <- transm$df_teach_hist
      df_teacher <- transm$df_teacher
      susc_close <- transm$susc_close
      susc_class <- transm$susc_class
      susc_grade <- transm$susc_grade
      susc_out_school <- transm$susc_out_school
      susc_teacher <- transm$susc_teacher
      susc_tt <- transm$susc_tt
      susc_ts <- transm$susc_ts
      infecteds <- transm$infecteds
    } # Transmission events
  }
  return(list(df_history=df_history, 
              df_agent=df_agent,
              df_teach_hist=df_teach_hist,
              df_teacher=df_teacher,
              cont_close=cont_close, 
              cont_class=cont_class, 
              cont_grade=cont_grade, 
              cont_out_school=cont_out_school,
              cont_teacher=cont_teacher,
              cont_tt=cont_tt, 
              cont_ts=cont_ts,
              susc_close=susc_close, 
              susc_class=susc_class, 
              susc_grade=susc_grade, 
              susc_out_school=susc_out_school,
              susc_teacher=susc_teacher,
              susc_tt=susc_tt, 
              susc_ts=susc_ts,
              df_screening=df_screening, 
              screening_list=screening_list, 
              df_risk_testing=df_risk_testing,
              infecteds=infecteds))
}
