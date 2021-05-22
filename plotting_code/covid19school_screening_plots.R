rm(list=ls())
source("covid19school_packages.R")
source("covid19school_plot_template.R")
source("covid19school_functions.R")
source("covid19school_vars.R")
source("covid19school_epidemic.R")

### Variables
iter <- 100

### Figures path
figuresPath <- "/figures/screening/"

### Scenarios
suffix <- c("_screening_weekly","_screening_weekly","_screening_weekly")
scenarios <- c("Screening weekly (full)",
               "Screening weekly (full)",
               "Screening weekly (full)")
length(suffix)==length(scenarios)
###
tol = 1e-5

###
sample_prop=0.5

### Time
unit=24
steps <- 8/24; time_period <- 4*7; time_steps <- seq(1, time_period, by = steps)
group_per_step <- rep(c(rep(1,(1/steps)*(24/unit)),rep(2,(1/steps)*(24/unit))), time_period/2)[1:length(time_steps)] # Assign groups to each time point, assuming 2 groups

### Types of individuals
types <- c("S", "PS", "IS", "IA", "R", "IH", "Q")

### External incidence rate for importations
external_prob = c(0.0006341289, 0.0006341289)

### Testing 
testing_mat <- matrix(c(c(1,0,0,0,0,0,0),
                        c(0,0,1,0,0,0,0),
                        c(0,0,0,0,1,0,0)), ncol=7, byrow=T)
colnames(testing_mat) <- day_names
nrow(testing_mat)==length(scenarios)

### Occupancy
occup_suffix <- c("full_occup",
                  "full_occup",
                  "full_occup")
occup <- c(1, 1, 1) # Occupancy level, options: 0.5, 1

### Flags for screening, risk-based testing and vaccination
scr_flags <- c(T,T,T); risk_flags <- c(F,F,F)
vacc_flag <- c(F,F,F)

# Outbreak size
outbreak_data_students <- outbreak_data_teachers <- vector(mode="list", length=length(scenarios))
outbreak_size_students <- outbreak_size_teachers <- rep(list(rep(0,iter)), length(scenarios))

# Absent days and absent individuals
abs_days_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
abs_days_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Peak number of infected individuals
peak_students <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))
peak_teachers <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))

# Outbreak size per "location"
outbreak_per_loc_st <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
outbreak_per_loc_teach <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

sec_cases_students <- sec_cases_teachers <- vector(mode="list", length=length(scenarios))
# ============================================================================ #
# Starting the simulations
# ============================================================================ #
for(scenario in 1:length(scenarios)){
  # Matrix output template
  mat <- matrix(0, nrow=length(time_steps), ncol=iter)
  ns <- nt <- rep(list(mat),7)
  names(ns) <- names(nt) <- types
  # Start simulation 
  start_time <- Sys.time()
  for(it in 1:iter){
    epidemic <- school.epidemic(seed=it, 
                                df_history, df_agent, df_teach_hist, df_teacher,
                                occup = occup[scenario],
                                steps = steps,
                                time_period = time_period, 
                                test_days = testing_mat[scenario,], 
                                day_names = day_names,
                                test_sens = test_sens, spec=spec, 
                                screening_flag = scr_flags[scenario], 
                                risk_based_flag = risk_flags[scenario],
                                vaccination_flag = vacc_flag[scenario],
                                external_prob = external_prob, 
                                cont_close = cont_close, cont_class = cont_class, cont_grade = cont_grade, 
                                cont_teacher = cont_teacher, cont_tt = cont_tt,
                                susc_close = susc_close, susc_class = susc_class, susc_grade = susc_grade, 
                                susc_out_school = susc_out_school, 
                                susc_teacher = susc_teacher, susc_tt = susc_tt, 
                                sample_prop=sample_prop)
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
    # For plotting
    for(j in 1:7){
      # Students
      ns[[j]][,it] <- sapply(unique(df_history$time), function(x) nrow(df_history %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
      # Teachers
      nt[[j]][,it] <- sapply(unique(df_teach_hist$time), function(x) nrow(df_teach_hist %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
    }
    
    # Outbreak size students
    outbreak_size_students[[scenario]][it] <- sum(!is.na(df_agent$infector))
    outbreak_size_teachers[[scenario]][it] <- sum(!is.na(df_teacher$infector))
    
    # Outbreak size per introduction
    outbreak_data_students[[scenario]][[it]] <- table(df_agent$source)
    outbreak_data_teachers[[scenario]][[it]] <- table(df_teacher$source)
    
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
    
    sec_cases_students[[scenario]][[it]] <- as.numeric(table(df_agent$infector))
    sec_cases_teachers[[scenario]][[it]] <- as.numeric(table(df_teacher$infector))
  }
  end_time <- Sys.time()
  run_time <- end_time-start_time
  print(paste0("Total runtime = ", run_time))
  
  # Saving output
  output<- list(ns, nt, df_history, df_agent, df_teach_hist, df_teacher, 
                outbreak_size_students, 
                outbreak_size_teachers, 
                absences_students)
  save(output, file=paste0("../results/", figuresPath, occup_suffix[scenario], suffix[scenario],".RData"))
  
  # PLOTTING
  # Students
  student_data <- transform.df(time_steps, iter, ns)
  student_plot <- ggplot(student_data, aes(x=time)) + 
    labs(title="Students") +
    facet_wrap(~variable, scales="free") + 
    scale_y_continuous(limits = c(0,NA)) + 
    geom_line(aes(y=mean, color=variable),size=0.9) + 
    geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper, fill=variable), alpha=0.3, color=NA) + 
    theme_bw() + 
    theme(legend.position = "none")
  student_plot
  ggsave(student_plot, file=paste0("../results", figuresPath, "students_plot_", occup_suffix[scenario], suffix[scenario],".pdf"), width=16, height=12)
  # Teachers
  teacher_data <- transform.df(time_steps, iter, nt)
  teacher_plot <- ggplot(teacher_data, aes(x=time)) + 
    labs(title="Teachers") + 
    facet_wrap(~variable, scales="free") + 
    scale_y_continuous(limits = c(0,NA)) + 
    geom_line(aes(y=mean, color=variable),size=0.9) + 
    geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper, fill=variable), alpha=0.3, color=NA) + 
    theme_bw() + 
    theme(legend.position = "none")
  teacher_plot
  ggsave(teacher_plot, file=paste0("../results", figuresPath, "teachers_plot_", occup_suffix[scenario], suffix[scenario], ".pdf"), width=16, height=12)
}

save.image(file=paste0(figuresPath, 'compareWeeklyScreening.RData'))


# ---------------------------------------------------------------------------- #
# Outbreak size 
# ---------------------------------------------------------------------------- #
# Students
plot.outbreak.size(outbreak_size_students, 
                   screening_day_names, 
                   iter=iter,
                   figuresPath=figuresPath, 
                   suffix="students",
                   occup_suffix="",
                   title="",
                   title_y="Total number of infected students")
# Teachers
plot.outbreak.size(outbreak_size_teachers, 
                   screening_day_names,
                   iter=iter,
                   figuresPath=figuresPath, 
                   suffix="teachers",
                   occup_suffix="",
                   title="",
                   title_y="Total number of infected teachers")


# ============================================================================ #
# Plot infections per location
# ============================================================================ #
outbreak_external_st <- lapply(outbreak_per_loc_st, function(x) as.data.frame(cbind(External=x[,3])))
melt(outbreak_external_st)

plot.inf.per.location(outbreak_external_st, 
                      screening_day_names,
                      figuresPath=figuresPath, 
                      suffix="students",
                      occup_suffix="",
                      title="",
                      title_y="Number of infected students")


plot.inf.per.location(outbreak_per_loc_st, 
                      screening_day_names,
                      figuresPath=figuresPath, 
                      suffix="students",
                      occup_suffix="",
                      title="",
                      title_y="Number of infected students")

plot.inf.per.location(outbreak_per_loc_teach, 
                      screening_day_names,
                      figuresPath=figuresPath, 
                      suffix="teachers",
                      occup_suffix="",
                      title="",
                      title_y="Number of infected teachers")


# ============================================================================ #
# Plot peak number of infections
# ============================================================================ #
peak_students <- list(peak_students[[1:scenario]])
names(peak_students) <- scenarios[1:scenario]
df_peak_students <- melt(peak_students)
colnames(df_peak_students) <- c("variable", "value", "scenario")
df_peak_students$scenario <- factor(df_peak_students$scenario, levels = scenarios[1:scenario])
peak_students_plot <- ggplot(df_peak_students[df_peak_students$variable=="Infected",], aes(x=scenario, y=value)) + 
  geom_boxplot() + 
  labs(y="Number of infections", title = "STUDENTS") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=18,angle=45,hjust=1.0))
peak_students_plot
ggsave(peak_students_plot, file=paste0("../results", figuresPath, "peak_students_plot_plot.pdf"), 
       width=16, height=9)

peak_teachers <- list(peak_teachers)
names(peak_teachers) <- scenarios
df_peak_teachers <- melt(peak_teachers)
colnames(df_peak_teachers) <- c("variable", "value", "scenario")
df_peak_teachers$scenario <- factor(df_peak_teachers$scenario, levels = scenarios[1:scenario])
peak_teachers_plot <- ggplot(df_peak_teachers[df_peak_teachers$variable=="Infected",], aes(x=scenario, y=value)) + 
  geom_boxplot() + 
  labs(y="Number of infections", title = "TEACHERS") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=18,angle=45,hjust=1.0))
peak_teachers_plot
ggsave(peak_teachers_plot, file=paste0("../results", figuresPath, "peak_teachers_plot.pdf"), 
       width=16, height=9)


# ============================================================================ #
# Plot absences
# ============================================================================ #
# Absent days
screening_day_names <- c("Monday","Wednesday","Friday")
# Students
plot.abs.days(abs_days_students, 
              screening_day_names,
              figuresPath=figuresPath, 
              suffix="student",
              occup_suffix="",
              title="",
              title_y="Number of absent days per student", 
              no_ind = nrow(df_agent))

# Teachers
plot.abs.days(abs_days_teachers, 
              screening_day_names,
              figuresPath=figuresPath, 
              suffix="teacher",
              occup_suffix="",
              title="",
              title_y="Number of absent days per teacher", 
              no_ind = nrow(df_teacher))

# Absent students
plot.abs(absences_students, 
         screening_day_names,
         figuresPath=figuresPath, 
         suffix="student",
         occup_suffix="",
         title="",
         title_y="Number of absent students")

# Absent teachers
plot.abs(absences_teachers, 
         screening_day_names,
         figuresPath=figuresPath, 
         suffix="student",
         occup_suffix="",
         title="",
         title_y="Number of absent teachers")


# ============================================================================ #
# Plotting outbreak size per introduction case
# Students
plot.outbreak.size.per.case(outbreak_data_students, 
                            screening_day_names,
                            figuresPath=figuresPath, 
                            suffix="student",
                            occup_suffix="",
                            title="",
                            title_y="Mean outbreak size per introduction")
# Teachers
plot.outbreak.size.per.case(outbreak_data_teachers, 
                            screening_day_names,
                            figuresPath=figuresPath, 
                            suffix="teacher",
                            occup_suffix="",
                            title="",
                            title_y="Mean outbreak size per introduction")


# ============================================================================ #
# SECONDARY CASES
# ============================================================================ #
# Students
plot.sec.cases(sec_cases_students, 
               screening_day_names,
               figuresPath=figuresPath, 
               suffix="students",
               occup_suffix="",
               title="",
               title_y="Number of secondary cases by students")

plot.sec.cases(sec_cases_teachers, 
               screening_day_names,
               figuresPath=figuresPath, 
               suffix="students",
               occup_suffix="",
               title="",
               title_y="Number of secondary cases by students")