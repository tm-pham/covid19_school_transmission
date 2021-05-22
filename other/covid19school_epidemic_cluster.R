# ============================================================================ #
# Main function 
# ---------------------------------------------------------------------------- #
# Input:
# ============================================================================ #
school.epidemic.cluster <- function(seed = 12345, 
                            df_history, df_agent, df_teach_hist, df_teacher,
                            occup = 0.5,
                            steps = 8/24,
                            time_period = 4*7, 
                            test_days = c(1,0,1,0,0,0,0), 
                            screening_adherence = 0.8, 
                            day_names = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                            test_sens, 
                            spec = 1, 
                            screening_flag = F, 
                            risk_based_flag = F,
                            vaccination_flag = F, 
                            external_prob = c(0.0006341289, 0.0006341289),
                            cont_close, cont_class, cont_grade, cont_out_school, cont_teacher, cont_tt,
                            susc_close, susc_class, susc_grade, susc_out_school, susc_teacher, susc_tt, 
                            sample_prop=1, 
                            tol=1e-5, 
                            print_flag=F){
  set.seed(seed)
  # -------------------------------------------------------------------------- #
  # Time
  # -------------------------------------------------------------------------- #
  # if(print_flag) print("Start school.epidemic function.")
  t_0 <- 1 # Start time
  time_steps = seq(1, time_period, by = steps)
  time_names = day.of.the.week(time_steps, day_names) # Assign day of the week to time_steps
  contacts_in_school = contacts.in.school(time_steps, time_names)
  contacts_out_school = contacts.leisure(time_steps, time_names)
  no_contacts = no.contacts(time_steps)
  
  if(print_flag) print("Initialized variables.")
  {
    # ============================================================================ #
    # Variables that need to be initialized 
    # ============================================================================ #
    # ============================================================================ #
    # DATA FRAMES
    # ---------------------------------------------------------------------------- #
    # STUDENTS
    # ---------------------------------------------------------------------------- #
    df_agent <- data.frame(grade=numeric(), class=numeric(), student=numeric(),
                           group=numeric(), inf=numeric(), inc=numeric(), 
                           state=numeric(), iso=numeric(), iso_time=numeric(), 
                           recovered = numeric(), quaran=numeric(), 
                           source=numeric(), infector=numeric(), 
                           location=numeric(), # 1 = in school, 0 = outside school, school related, 2 = outside school
                           week_day=character(), 
                           id=numeric())
    for(g in 1:n_grades){
      for(c in 1:n_class[g]){
        for(s in 1:class_size[g]){
          temp <- cbind(grade=g, class=c, student=s, group=1,
                        inf=-1, inc=-1, state="S",
                        iso=0, iso_time=-1, 
                        recovered=0, quaran=0, 
                        source=NA, infector=NA, 
                        location=NA, week_day=NA,
                        id=paste0(g, c, s))
          df_agent <- rbind(df_agent, temp)
        }
      }
    }
    df_agent[,-which(names(df_agent)%in%c("state","week_day"))] <- apply(df_agent[,-which(names(df_agent)%in%c("state","week_day"))] , 2, as.numeric)
    df_agent$week_day <- factor(df_agent$week_day, levels=day_names)
    # Assign students to group if occupancy at school is not full 
    df_agent$group <- rep(c(1,1/occup[scenario]), nrow(df_agent)/2)
    
    # History of disease states of students
    df_history <- as.data.frame(merge(cbind(time=time_steps, pres=1, iso_state="P"), 
                                      df_agent[,c("grade","class","group","student","state","id")]))
    df_history[,-which(names(df_history)%in%c("iso_state","state"))] <- apply(df_history[,-which(names(df_history)%in%c("iso_state","state"))] , 2, as.numeric)
    # Assign presence flag according to group
    if(occup[scenario]!=1){
      for(i in df_agent$id){
        df_history[df_history$id==i & df_history$group==1,"pres"] <- group_per_step%%2
        df_history[df_history$id==i & df_history$group==2,"pres"] <- (group_per_step-1)%%2
      }
    }
    
    # ---------------------------------------------------------------------------- #
    # Initially infected students
    # ---------------------------------------------------------------------------- #
    student_ids <- unique(df_agent$id)
    
    # ---------------------------------------------------------------------------- #
    # Initially recovered students
    # ---------------------------------------------------------------------------- #
    sero_prev_students <- 0.25
    init_rec_students <- sample(df_agent[df_agent$state=="S", "id"], floor(sero_prev_students*nrow(df_agent)))
    df_agent[df_agent$id%in%init_rec_students, "location"] <- NA
    df_agent[df_agent$id%in%init_rec_students, "recovered"] <- 1
    df_history[df_history$id%in%init_rec_students,"state"] <- "R"
    
    # ============================================================================ #
    # DATA FRAMES
    # ---------------------------------------------------------------------------- #
    # Teachers
    # ---------------------------------------------------------------------------- #
    prob_quaran <- 0 # Probability that a teacher needs to quarantine at symptom onset of an index case 
    df_teacher <- data.frame(grade=numeric(), class=numeric(), subject=factor(),
                             group=numeric(), inf=numeric(), inc=numeric(), 
                             state=numeric(), iso=numeric(), iso_time=numeric(), 
                             recovered = numeric(), vaccinated = numeric(), quaran=numeric(), 
                             source=numeric(), infector=numeric(),
                             location=numeric(), week_day=character(),
                             id=numeric())
    
    occupancy <- 1
    i <- 0
    gr_1 <- 1:floor(n_grades/2)
    gr_2 <- (floor(n_grades/2)+1):n_grades
    s_1 <- 1:floor(n_subjects/2)
    s_2 <- (floor(n_subjects/2)+1):n_subjects
    
    for(s in 1:n_subjects){
      for(g in 1:n_grades){
        for(c in 1:n_class[g]){
          temp <- cbind(grade=g, class=c, subject=s, group=1,
                        inf=-1, inc=-1, state="S", 
                        iso=-1, iso_time=-1, 
                        recovered=0, vaccinated=0, quaran=-1, 
                        source=NA, infector=NA, 
                        location=NA, week_day=NA, 
                        id=NA)
          df_teacher <- rbind(df_teacher, temp)
        }
      }
    }
    df_teacher[,-which(names(df_teacher)%in%c("state","week_day","id"))] <- apply(df_teacher[,-which(names(df_teacher)%in%c("state","week_day","id"))], 2, as.numeric)
    
    # Teachers teaching subject 1,2 to grades 1,2,3
    assigned_teachers <- assign.teachers(df_teacher, gr_1, s_1, initial=T)
    df_teacher <- assigned_teachers$df_teacher
    id <- assigned_teachers$id
    
    # Teachers teaching subjects 1,2 to grades 4,5,6
    assigned_teachers <- assign.teachers(df_teacher, gr_2, s_1)
    df_teacher <- assigned_teachers$df_teacher
    id <- assigned_teachers$id
    
    # Teachers teaching subjects 3,4,5 to grades 1,2,3
    assigned_teachers <- assign.teachers(df_teacher, gr_1, s_2)
    df_teacher <- assigned_teachers$df_teacher
    id <- assigned_teachers$id
    
    # Teachers teaching subjects 3,4,5 to grades 4,5,6
    assigned_teachers <- assign.teachers(df_teacher, gr_2, s_2)
    df_teacher <- assigned_teachers$df_teacher
    id <- assigned_teachers$id
    
    df_teacher$id <- as.numeric(df_teacher$id)
    df_teacher <- df_teacher[order(df_teacher$id),]
    df_teacher$id <- paste0("T",df_teacher$id)
    head(df_teacher)
    
    # History of disease states for teachers
    df_teach_hist <- as.data.frame(unique(merge(cbind(time=time_steps, pres=1, iso_state="P"), df_teacher[,c("state","id")])))
    df_teach_hist[,-which(names(df_teach_hist)%in%c("iso_state","state","id"))] <- apply(df_teach_hist[,-which(names(df_teach_hist)%in%c("iso_state","state","id"))] , 2, as.numeric)
    
    
    # Split into two
    df_teacher_gcs <-  unique(df_teacher[,c("grade","class","subject","group","id")])
    df_teacher <- unique(df_teacher[,c("id","source","infector","inf","inc","state","iso","iso_time","recovered","vaccinated","quaran","location","week_day")])
    
    # ---------------------------------------------------------------------------- #
    # Initially infected teachers
    # ---------------------------------------------------------------------------- #
    teacher_ids <- unique(df_teacher$id)
    
    # ---------------------------------------------------------------------------- #
    # Initially recovered teachers
    # ---------------------------------------------------------------------------- #
    sero_prev_teacher <- 0.2
    init_rec_teachers <- sample(df_teacher[df_teacher$state=="S", "id"], floor(sero_prev_teacher*nrow(df_teacher)))
    df_teacher[df_teacher$id%in%init_rec_teachers, "location"] <- NA
    df_teacher[df_teacher$id%in%init_rec_teachers, "recovered"] <- 1
    df_teach_hist[df_teach_hist$id%in%init_rec_teachers,"state"] <- "R"
    
    # ============================================================================ #
    # Construct contact network
    # ---------------------------------------------------------------------------- #
    # For each student: choose the contacts
    cont_close <- cont_class <- cont_grade <- cont_out_school <- cont_teacher <- list()
    susc_close <- susc_class <- susc_grade <- susc_other_grades <- susc_out_school <- susc_teacher <- list()
    infected_ids <- df_agent[df_agent$state=="IS" | df_agent$state=="IA", "id"]
    infected_teacher_ids <- df_teacher[df_teacher$state%in%c("IS","IA"), "id"]
    sec_cases <- list()
    for(i in 1:length(student_ids)){
      # sec_cases[[i]] <- NULL
      id <- student_ids[i]
      row <- which(df_agent$id==id)
      class <- df_agent[row, "class"]
      grade <- df_agent[row, "grade"]
      group <- df_agent[row, "group"]
      cont_teacher[[i]] <- unique(df_teacher_gcs[df_teacher_gcs$grade==grade&df_teacher_gcs$class==class,"id"])
      students_class <- df_agent[df_agent$class==class & df_agent$grade==grade & df_agent$group==group, "id"]
      students_grade <- setdiff(df_agent[df_agent$grade==grade & df_agent$group==group, "id"],students_class)
      students_class <- setdiff(students_class, id)
      
      remaining_close_cont <- setdiff(students_class, unlist(cont_close))
      if(length(remaining_close_cont)>0){
        cont_close[[i]] <- sample(remaining_close_cont, n_cont_close)
      }else cont_close[[i]] <- numeric(0)
      
      susc_close[[i]] <- setdiff(cont_close[[i]], infected_ids)
      cont_class[[i]] <- sample(setdiff(students_class, cont_close[[i]]), n_cont_class)
      susc_class[[i]] <- setdiff(cont_class[[i]], infected_ids)
      cont_grade[[i]] <- sample(students_grade, n_cont_grade)
      susc_grade[[i]] <- setdiff(cont_grade[[i]], infected_ids)
      cont_out_school[[i]] <- sample(c(students_class, students_grade), n_cont_out_school)
      susc_out_school[[i]] <- setdiff(cont_out_school[[i]], infected_ids)
    }
    susc_teacher <- lapply(cont_teacher, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
    names(cont_close) <- names(cont_class) <- names(cont_grade) <- names(cont_out_school) <- names(cont_teacher) <- student_ids
    names(susc_close) <- names(susc_class) <- names(susc_grade) <- names(susc_out_school) <- names(susc_teacher) <- student_ids
    
    # Contacts between teachers
    cont_tt <- lapply(df_teacher$id, function(x) sample(df_teacher$id[!df_teacher$id==x], size=n_cont_teachers))
    names(cont_tt) <- unique(df_teacher$id)
    susc_tt <- lapply(cont_tt, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
    
    # ============================================================================ #
    # Initialize screening matrix
    # ============================================================================ #
    df_screening <- as.data.frame(cbind(time=time_steps, 
                                        pos_tested=rep(0, length(time_steps)), 
                                        pos_students=rep(0,length(time_steps)), 
                                        pos_teachers=rep(0, length(time_steps))))
    screening_list <- vector(mode = "list", length = length(time_steps))
    
    # ============================================================================ #
    # Initialize screening matrix
    # ============================================================================ #
    df_risk_testing <- as.data.frame(cbind(time=time_steps, 
                                           pos_tested=rep(0, length(time_steps)), 
                                           pos_students=rep(0,length(time_steps)), 
                                           pos_teachers=rep(0, length(time_steps))))
    risk_testing_list <- vector(mode = "list", length = length(time_steps))
  }
  # ---------------------------------------------------------------------------- #
  # Vaccinated teachers
  # ---------------------------------------------------------------------------- #
  df_teacher[, "vaccinated"] <- as.numeric(vaccination_flag)
  
  
  inf_count <- 0
  infecteds <- NULL
  # -------------------------------------------------------------------------- #
  # SIMULATION
  # -------------------------------------------------------------------------- #
  for(t_i in time_steps){
    # ------------------------------------------------------------------------ #
    # Introductions from community
    # ------------------------------------------------------------------------ #
    t_i
    if(t_i%%1==0){
      if(print_flag) print("Introductions from community.")
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
      # Isolation of symptomatic individuals
      # ------------------------------------------------------------------------ #
      if(print_flag) print("Isolation of symptomatic individuals.")
      # Students
      isolated_students <- isolation(t_i, df_history, df_agent)
      df_history <- isolated_students$df_history
      df_agent <- isolated_students$df_agent
      pos_tested_ids <- isolated_students$pos_tested_ids
      # Teachers
      isolated_teachers <- isolation(t_i, df_teach_hist, df_teacher)
      df_teach_hist <- isolated_teachers$df_history
      df_teacher <- isolated_teachers$df_agent
      pos_tested_teacher_ids <- isolated_teachers$pos_tested_ids
    }
    
    # ------------------------------------------------------------------------ #
    # Recovery
    # ------------------------------------------------------------------------ #
    if(print_flag) print("Recovery.")
    # Students
    rec_students <- recovery(t_i, df_history, df_agent, rec_time)
    df_history <- rec_students$df_history
    df_agent <- rec_students$df_agent
    # Teachers
    rec_teachers <- recovery(t_i, df_teach_hist, df_teacher, rec_time)
    df_teach_hist <- rec_teachers$df_history
    df_teacher <- rec_teachers$df_agent
    
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
    
    # ------------------------------------------------------------------------ #
    # Quarantine of close contacts
    # ------------------------------------------------------------------------ #
    # if(print_flag) print("Quarantine.")
    # Students
    quarantined_students <- quarantine.close.contacts(t_i, 
                                                      df_history, df_agent, 
                                                      df_teach_hist, df_teacher, 
                                                      pos_tested_ids, 
                                                      cont_close, cont_class, cont_grade, cont_teacher,
                                                      susc_close, susc_class, susc_grade, susc_out_school, 
                                                      susc_teacher, susc_tt)
    df_history <- quarantined_students$df_history
    df_agent <- quarantined_students$df_agent
    susc_close <- quarantined_students$susc_close
    susc_class <- quarantined_students$susc_class
    susc_grade <- quarantined_students$susc_grade
    susc_out_school <- quarantined_students$susc_out_school
    # Teachers
    quarantined_teachers <- quarantine.close.contacts(t_i, 
                                                      df_history, df_agent, 
                                                      df_teach_hist, df_teacher, 
                                                      pos_tested_ids, 
                                                      cont_close, cont_class, cont_grade, cont_teacher,
                                                      susc_close, susc_class, susc_grade, susc_out_school, 
                                                      susc_teacher, susc_tt)
    df_teach_hist <- quarantined_teachers$df_teach_hist
    df_teacher <- quarantined_teachers$df_teacher
    susc_teacher <- quarantined_teachers$susc_teacher
    susc_tt <- quarantined_teachers$susc_tt
    
    # if(print_flag) print("Screening.")
    # ----------------------------------------------------------------------- #
    # Pre-emptive testing
    # ----------------------------------------------------------------------- #
    if(screening_flag){
      if(t_i%%1==0 & time_names[abs(time_steps-t_i)<=tol]%in%names(test_days[test_days==1])){
        # print(paste0("t_i=", t_i))
        # print(paste0("day=", time_names[abs(time_steps-t_i)<=tol]))
        # print(paste0("Screening t_i=", t_i))
        screened_students <- screening(t_i, 
                                       df_history, df_agent, 
                                       df_inf_sens, 
                                       susc_close, susc_class, susc_grade, susc_out_school,
                                       susc_teacher, susc_tt,
                                       teacher=F, tol=tol, 
                                       screening_adherence = min(1, screening_adherence))
        df_history <- screened_students$df_history
        df_agent <- screened_students$df_agent
        susc_close <- screened_students$susc_close
        susc_class <- screened_students$susc_class
        susc_grade <- screened_students$susc_grade
        susc_out_school <- screened_students$susc_out_school
        pos_students <- screened_students$pos_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_students)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_students"] <- length(pos_students)
        screened_teachers <- screening(t_i, 
                                       df_teach_hist, df_teacher, 
                                       df_inf_sens, 
                                       susc_close, susc_class, susc_grade, 
                                       susc_out_school, 
                                       susc_teacher, susc_tt,
                                       teacher=T, tol=tol, 
                                       screening_adherence = min(0.9, 1.25*screening_adherence))
        df_teach_hist <- screened_teachers$df_history
        df_teacher <- screened_teachers$df_agent
        susc_teacher <- screened_teachers$susc_teacher
        susc_tt <- screened_teachers$susc_tt
        pos_teachers <- screened_teachers$pos_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_teachers"] <- length(pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_tested"] <- length(pos_students) + length(pos_teachers)
        if(is.null(df_teach_hist)) print("screening")
      } # screening_flag
    }
    
    # Events that take place only on school week days
    if(!time_names[abs(time_steps-t_i)<=tol]%in%c("Saturday", "Sunday")){
      if(length(pos_tested_ids)>0){
        # -------------------------------------------------------------------- #
        # Risk-based testing
        # -------------------------------------------------------------------- #
        if(risk_based_flag & contacts_in_school[abs(time_steps-t_i)<=tol]==1){
          risk_tested <- risk_based_testing(t_i, 
                                            df_history, df_agent, 
                                            df_teach_hist, df_teacher, 
                                            pos_tested_ids, 
                                            cont_close, cont_class, cont_grade, cont_teacher,
                                            susc_close, susc_class, susc_grade, susc_out_school, 
                                            susc_teacher, susc_tt)
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
          df_risk_testing[abs(df_risk_testing$time-t_i)<=tol, "pos_students"] <- length(risk_tested$ind_students)
          df_risk_testing[abs(df_risk_testing$time-t_i)<=tol, "pos_teachers"] <- length(risk_tested$ind_teacher)
          df_risk_testing[abs(df_risk_testing$time-t_i)<=tol, "pos_tested"] <- df_risk_testing[t_i, "pos_students"] + df_risk_testing[t_i, "pos_teachers"] 
          if(is.null(df_teach_hist)) print("risk")
        } # Risk-based testing
      }
    } # Events only during week days
    # -------------------------------------------------------------------- #
    # Transmission events
    # -------------------------------------------------------------------- #
    if(no_contacts[abs(time_steps-t_i)<=tol]==0){
      transm <- transmission.events(t_i, time_names,
                                    df_history, df_agent,
                                    df_teach_hist, df_teacher, df_teacher_gcs,
                                    susc_close, susc_class, susc_grade, 
                                    susc_out_school,
                                    susc_teacher, susc_tt,
                                    infecteds,
                                    contacts_in_school, contacts_out_school,
                                    vaccination_flag=vaccination_flag, 
                                    sample_prop=sample_prop)
      df_history <- transm$df_history
      df_agent <- transm$df_agent
      df_teach_hist <- transm$df_teach_hist
      df_teacher <- transm$df_teacher
      df_teacher_gcs <- transm$df_teacher_gcs
      susc_close <- transm$susc_close
      susc_class <- transm$susc_class
      susc_grade <- transm$susc_grade
      susc_out_school <- transm$susc_out_school
      susc_teacher <- transm$susc_teacher
      susc_tt <- transm$susc_tt
      infecteds <- transm$infecteds
    } # Transmission events
  }
  return(list(df_history=df_history, 
              df_agent=df_agent,
              df_teach_hist=df_teach_hist,
              df_teacher=df_teacher,
              susc_close=susc_close, 
              susc_class=susc_class, 
              susc_grade=susc_grade, 
              susc_out_school=susc_out_school,
              susc_teacher=susc_teacher,
              susc_tt=susc_tt, 
              df_screening=df_screening, 
              screening_list=screening_list, 
              df_risk_testing=df_risk_testing,
              infecteds=infecteds))
}
