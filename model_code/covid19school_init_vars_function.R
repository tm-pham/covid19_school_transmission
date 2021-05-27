# ============================================================================ #
# Variables that need to be initialized 
# ============================================================================ #
init.vars <- function(sero_prev_students=0.25, 
                      sero_prev_teacher=0.25, 
                      occup=c(1,0.5,0.5,1,1,1), 
                      group_per_step, 
                      class_size=c(23, 29, 23, 29, 30, 24), 
                      n_grades=6, 
                      n_cont_same_class=c(6,5,6,8,8,11), 
                      n_cont_vec=c(3, 0, 0, 0, 0, 0,
                                   2, 5, 0, 0, 0, 0,
                                   1, 1, 5, 0, 0, 0,
                                   1, 1, 2, 7, 1, 1,
                                   0, 0, 0, 1, 7, 1,
                                   0, 0, 0, 1, 2, 7), 
                      n_cont_close=1,
                      n_cont_out_school=2,
                      n_cont_other=3,
                      n_subjects=5,
                      n_quaran=2,
                      n_cont_teachers=6
                      ){
  
  n_cont <- matrix(n_cont_vec, nrow=6, byrow = T)
  n_cont_grade <- diag(n_cont)
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
                         adherence=numeric(), 
                         iso_compliance=numeric(), quaran_compliance=numeric(),
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
                      adherence=1, 
                      iso_compliance=1, quaran_compliance=1,
                      id=paste0(g, c, s))
        df_agent <- rbind(df_agent, temp)
      }
    }
  }
  df_agent[,-which(names(df_agent)%in%c("state","week_day"))] <- apply(df_agent[,-which(names(df_agent)%in%c("state","week_day"))] , 2, as.numeric)
  df_agent$week_day <- factor(df_agent$week_day, levels=day_names)
  # Assign students to group if occupancy at school is not full 
  df_agent$group <- rep(c(1,1/occup), nrow(df_agent)/2)
  # Sample individuals who will be compliant to isolation and quarantine
  df_agent$iso_compliance <- sample(c(0,1), size=nrow(df_agent), prob=c(1-compliance_iso, compliance_iso), replace=T)
  df_agent$quaran_compliance <- sample(c(0,1), size=nrow(df_agent), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
  
  # History of disease states of students
  df_history <- as.data.frame(merge(cbind(time=time_steps, pres=1, iso_state="P"), 
                                    df_agent[,c("grade","class","group","student","state","id")]))
  df_history[,-which(names(df_history)%in%c("iso_state","state"))] <- apply(df_history[,-which(names(df_history)%in%c("iso_state","state"))] , 2, as.numeric)
  # Assign presence flag according to group
  if(occup!=1){
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
  init_rec_students <- sample(df_agent[df_agent$state=="S", "id"], floor(sero_prev_students*nrow(df_agent)))
  df_agent[df_agent$id%in%init_rec_students, "location"] <- NA
  df_agent[df_agent$id%in%init_rec_students, "recovered"] <- 1
  df_history[df_history$id%in%init_rec_students,"state"] <- "R"
  df_agent[df_agent$id%in%init_rec_students,"state"] <- "R"
  
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
                           adherence=numeric(), 
                           iso_compliance=numeric(), quaran_compliance=numeric(),
                           id=numeric())
  
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
                      iso=0, iso_time=-1, 
                      recovered=0, vaccinated=0, quaran=0, 
                      source=NA, infector=NA, 
                      location=NA, week_day=NA, 
                      adherence=1, 
                      iso_compliance=1, quaran_compliance=1,
                      id=-1)
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
  assigned_teachers <- assign.teachers(df_teacher, gr_2, s_1, id=id)
  df_teacher <- assigned_teachers$df_teacher
  id <- assigned_teachers$id
  
  # Teachers teaching subjects 3,4,5 to grades 1,2,3
  assigned_teachers <- assign.teachers(df_teacher, gr_1, s_2, id=id)
  df_teacher <- assigned_teachers$df_teacher
  id <- assigned_teachers$id
  
  # Teachers teaching subjects 3,4,5 to grades 4,5,6
  assigned_teachers <- assign.teachers(df_teacher, gr_2, s_2, id=id)
  df_teacher <- assigned_teachers$df_teacher
  id <- assigned_teachers$id
  
  df_teacher$id <- as.numeric(df_teacher$id)
  df_teacher <- df_teacher[order(df_teacher$id),]
  df_teacher$id <- paste0("T",df_teacher$id)
  
  # History of disease states for teachers
  df_teach_hist <- as.data.frame(unique(merge(cbind(time=time_steps, pres=1, iso_state="P"), df_teacher[,c("state","id")])))
  df_teach_hist[,-which(names(df_teach_hist)%in%c("iso_state","state","id"))] <- apply(df_teach_hist[,-which(names(df_teach_hist)%in%c("iso_state","state","id"))] , 2, as.numeric)
  
  
  # Split into two
  df_teacher_gcs <-  unique(df_teacher[,c("grade","class","subject","group","id")])
  df_teacher <- unique(df_teacher[,!colnames(df_teacher)%in%c("grade","class","subject","group")])
  
  # Sample individuals who will be compliant to isolation and quarantine
  df_teacher$iso_compliance <- sample(c(0,1), size=nrow(df_teacher), prob=c(1-compliance_iso, compliance_iso), replace=T)
  # Note that at the moment teachers don't need to quarantine
  df_teacher$quaran_compliance <- sample(c(0,1), size=nrow(df_teacher), prob=c(prob_quaran*(1-compliance_quaran), 1-prob_quaran*(1-compliance_quaran)), replace=T)
  
  # ---------------------------------------------------------------------------- #
  # Initially infected teachers
  # ---------------------------------------------------------------------------- #
  teacher_ids <- unique(df_teacher$id)
  
  # ---------------------------------------------------------------------------- #
  # Initially recovered teachers
  # ---------------------------------------------------------------------------- #
  init_rec_teachers <- sample(df_teacher[df_teacher$state=="S", "id"], floor(sero_prev_teacher*nrow(df_teacher)))
  df_teacher[df_teacher$id%in%init_rec_teachers, "location"] <- NA
  df_teacher[df_teacher$id%in%init_rec_teachers, "recovered"] <- 1
  df_teach_hist[df_teach_hist$id%in%init_rec_teachers,"state"] <- "R"
  df_teacher[df_teacher$id%in%init_rec_teachers,"state"] <- "R"
  
  # ============================================================================ #
  # Construct contact network
  # ---------------------------------------------------------------------------- #
  # For each student: choose the contacts
  cont_close <- cont_class <- cont_grade <- cont_out_school <- cont_teacher <- vector(mode="list", length=length(student_ids))
  susc_close <- susc_class <- susc_grade <- susc_other_grades <- susc_out_school <- susc_teacher <- vector(mode="list", length=length(student_ids))
  names(cont_close) <- names(cont_class) <- names(cont_grade) <- names(cont_out_school) <- names(cont_teacher) <- student_ids
  names(susc_close) <- names(susc_class) <- names(susc_grade) <- names(susc_out_school) <- names(susc_teacher) <- student_ids
  infected_ids <- df_agent[df_agent$state=="IS" | df_agent$state=="IA", "id"]
  infected_teacher_ids <- df_teacher[df_teacher$state%in%c("IS","IA"), "id"]
  sec_cases <- list()
  cont_grade <- lapply(cont_grade, function(x) vector(mode="list", length=ncol(n_cont)))
  for(i in 1:length(student_ids)){
    id <- student_ids[i]
    row <- which(df_agent$id==id)
    class <- df_agent[row, "class"]
    grade <- df_agent[row, "grade"]
    group <- df_agent[row, "group"]
    # Students within the same class and same group (important for half occupancy)
    students_class <- df_agent[df_agent$class==class & df_agent$grade==grade & df_agent$group==group, "id"]
    # Students in school in the same group (important for half occupancy)
    students_school <- lapply(1:n_grades, function(x) setdiff(df_agent[df_agent$group==group& df_agent$grade==x,"id"], students_class))
    # Students within the same grade
    students_grade <- students_school[[grade]]
    # Exclude current student from that list
    students_class <- setdiff(students_class, id)
    ### Close contacts in class (usually sitting next to each other)
    # Ensure no overlapping close contacts
    remaining_close_cont <- setdiff(students_class, unlist(cont_close))
    if(length(remaining_close_cont)>0 && length(cont_close[[i]])<n_cont_close){
      sampled_close_cont <- remaining_close_cont[sample.int(length(remaining_close_cont), max(0, n_cont_close-length(cont_close[[i]])))]
      cont_close[[i]] <- unique(c(cont_close[[i]], sampled_close_cont))
      # The next two lines is to ensure symmetric contact network
      ind_students <- as.character(sampled_close_cont)
      cont_close[ind_students] <- lapply(cont_close[ind_students], function(x) unique(c(x,id)))
      susc_close[[i]] <- setdiff(cont_close[[i]], infected_ids)
    }
    ### Contacts students have within the same class
    remaining_ids <- setdiff(students_class, cont_class[[i]])
    remaining_ids <- remaining_ids[which(unlist(lapply(cont_class[as.character(remaining_ids)], length))<n_cont_same_class[grade])]
    if(length(remaining_ids)>0 && length(cont_class[[i]])<n_cont_same_class[grade]){
      cont_class[[i]] <- unique(c(cont_class[[i]], remaining_ids[sample.int(length(remaining_ids), max(0, min(length(remaining_ids), n_cont_same_class[grade]-length(cont_class[[i]]))))]))
      # The next two lines is to ensure symmetric contact network
      ind_students <- as.character(cont_class[[i]])
      cont_class[ind_students] <- lapply(cont_class[ind_students], function(x) unique(c(x,id)))
      susc_class[[i]] <- setdiff(cont_class[[i]], infected_ids)
    }
    
    ### Contacts students have outside the class (might be in different grades)
    remaining_ids <- lapply(students_school, function(x) setdiff(x, unlist(cont_grade[[i]])))
    for(j in 1:length(remaining_ids)){
      remaining_ids[[j]] <- remaining_ids[[j]][sapply(remaining_ids[[j]], function(x) length(cont_grade[as.character(x)][[1]][[grade]])<n_cont[df_agent[df_agent$id==x, "grade"], grade])]
      if(length(remaining_ids[[j]])>0 && length(cont_grade[[i]][[j]])<n_cont[grade,j]){
        ind_students <- remaining_ids[[j]][sample.int(length(remaining_ids[[j]]), max(0, min(length(remaining_ids[[j]]), n_cont[grade,j]-length(cont_grade[[i]][[j]]))))]
        cont_grade[[i]][[j]] <- c(cont_grade[[i]][[j]], ind_students)
        # The next line is to ensure symmetric contact network
        for(x in 1:length(ind_students)) cont_grade[as.character(ind_students[x])][[1]][[grade]] <- id
      }
    }
    susc_grade[[i]] <- setdiff(unlist(cont_grade[[i]]), infected_ids)
    
    ### Contacts students have outside school
    cont_out <- unlist(c(cont_class[[i]], unlist(cont_grade[[i]])))
    remaining_ids <- setdiff(cont_out, cont_out_school[[i]])
    remaining_ids <- remaining_ids[which(unlist(lapply(cont_out_school[as.character(remaining_ids)], length))<n_cont_out_school)]
    if(length(remaining_ids)>0 && length(cont_out_school[[i]])<n_cont_out_school){
      cont_out_school[[i]] <- c(cont_out_school[[i]], remaining_ids[sample.int(length(remaining_ids), max(0, min(length(remaining_ids), n_cont_out_school-length(cont_out_school[[i]]))))])
      ind_students <- as.character(cont_out_school[[i]])
      cont_out_school[ind_students] <- lapply(cont_out_school[ind_students], function(x) unique(c(x,id)))
      susc_out_school[[i]] <- setdiff(cont_out_school[[i]], infected_ids)
    }
  }
  cont_grade <- lapply(cont_grade, unlist)
  cont_out_school <- lapply(cont_out_school, unique)
  # Checks
  all(unlist(lapply(cont_close, length))<=n_cont_close)
  all(unlist(lapply(cont_out_school, length))<=n_cont_out_school)
  
  
  # Contacts between teachers
  cont_tt <- cont_ts <- susc_ts <- vector(mode="list", length=length(teacher_ids))
  names(cont_tt) <- names(cont_ts) <- unique(df_teacher$id)
  for(i in 1:length(teacher_ids)){
    id <- teacher_ids[i]
    teachers <- setdiff(teacher_ids, id)
    remaining_ids <- setdiff(teachers, cont_tt[[i]])
    remaining_ids <- remaining_ids[which(unlist(lapply(cont_tt[as.character(remaining_ids)], length))<n_cont_teachers)]
    if(length(remaining_ids)>0 && length(cont_tt[[i]])<n_cont_teachers){
      ind_teachers <- remaining_ids[sample.int(length(remaining_ids), size=max(0, min(length(remaining_ids), n_cont_teachers-length(cont_tt[[i]]))))]
      cont_tt[[i]] <- unique(c(cont_tt[[i]], ind_teachers))
      cont_tt[ind_teachers] <- lapply(cont_tt[ind_teachers], function(x) unique(c(x,id)))
    }
  }
  all(unlist(lapply(cont_tt, length))<=n_cont_teachers)
  # Susceptible teachers
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
  # Initialize risk testing matrix
  # ============================================================================ #
  df_risk_testing <- as.data.frame(cbind(time=time_steps, 
                                         pos_tested=rep(0, length(time_steps)), 
                                         pos_students=rep(0,length(time_steps)), 
                                         pos_teachers=rep(0, length(time_steps))))
  risk_testing_list <- vector(mode = "list", length = length(time_steps))
  
  
  
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              df_teacher_gcs = df_teacher_gcs,
              df_screening = df_screening, 
              screening_list = screening_list, 
              df_risk_testing = df_risk_testing, 
              risk_testing_list = risk_testing_list, 
              cont_close = cont_close, 
              cont_class = cont_class, 
              cont_grade = cont_grade, 
              cont_out_school = cont_out_school,
              cont_teacher = cont_teacher, 
              cont_tt = cont_tt, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade, 
              susc_out_school = susc_out_school,
              susc_teacher = susc_teacher, 
              susc_tt = susc_tt))
}


