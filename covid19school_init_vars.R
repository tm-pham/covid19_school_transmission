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
                       adherence=numeric(),
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
                         adherence=numeric(),
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
df_teacher <- unique(df_teacher[,c("id","source","infector","inf","inc","state","iso","iso_time","recovered","vaccinated","quaran","location","week_day","adherence")])

# ---------------------------------------------------------------------------- #
# Initially infected teachers
# ---------------------------------------------------------------------------- #
teacher_ids <- unique(df_teacher$id)

# ---------------------------------------------------------------------------- #
# Initially recovered teachers
# ---------------------------------------------------------------------------- #
sero_prev_teacher <- 0.25
init_rec_teachers <- sample(df_teacher[df_teacher$state=="S", "id"], floor(sero_prev_teacher*nrow(df_teacher)))
df_teacher[df_teacher$id%in%init_rec_teachers, "location"] <- NA
df_teacher[df_teacher$id%in%init_rec_teachers, "recovered"] <- 1
df_teach_hist[df_teach_hist$id%in%init_rec_teachers,"state"] <- "R"

# ============================================================================ #
# Construct contact network
# ---------------------------------------------------------------------------- #
# For each student: choose the contacts
cont_close <- cont_class <- cont_grade <- cont_out_school <- cont_teacher <- vector(mode="list", length=length(student_ids))
susc_close <- susc_class <- susc_grade <- susc_other_grades <- susc_out_school <- susc_teacher <- vector(mode="list", length=length(student_ids))
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
  # cont_teacher[[i]] <- unique(df_teacher_gcs[df_teacher_gcs$grade==grade&df_teacher_gcs$class==class,"id"])
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
# susc_teacher <- lapply(cont_teacher, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
names(cont_close) <- names(cont_class) <- names(cont_grade) <- names(cont_out_school) <- names(cont_teacher) <- student_ids
names(susc_close) <- names(susc_class) <- names(susc_grade) <- names(susc_out_school) <- names(susc_teacher) <- student_ids

# Contacts between teachers
cont_tt <- lapply(df_teacher$id, function(x) sample(df_teacher$id[!df_teacher$id==x], size=n_cont_teachers))
names(cont_tt) <- unique(df_teacher$id)
susc_tt <- lapply(cont_tt, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
# Contacts teacher - student
cont_ts <- vector(mode="list", length=nrow(df_teacher))
names(cont_ts) <- unique(df_teacher$id)
susc_ts <- cont_ts
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

