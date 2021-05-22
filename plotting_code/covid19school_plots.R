# ============================================================================ #
# Plotting
# ============================================================================ #
source("covid19school_packages.R")
source("covid19school_plotting_functions.R")

name <- "simJ1"
load(paste0('../results/', name, '/', name, '1-6.RData'))
name <- "simA1"
figuresPath <- paste0("/", name, "/")
if(!file.exists(paste0(resultsPath, figuresPath))) dir.create(file.path(paste0(resultsPath, figuresPath)))
# if(!file.exists(paste0(resultsPath, "/figures/",folder))) dir.create(file.path(paste0(resultsPath, "/figures/",folder)))
(max <- length(outbreak_size_students))

scenarios <- c("Risk based testing (half)",
               "Half occup",
               "Full occup",
               "Vaccination (full)",
               "Screening twice weekly (full)", 
               "Screening twice + Vaccination (full)")
permutation = c(4, 2, 1, 3, 6, 5)

# ---------------------------------------------------------------------------- #
# Number of symptomatic students (~notified positive cases)
# ---------------------------------------------------------------------------- #
# Students
(symps_plot <- plot.symptomatic(symptomatic_students, 
                                scenarios = scenarios, 
                                permutation=permutation, 
                                figuresPath = figuresPath, 
                                suffix="students"))
(sympt_plot <- plot.symptomatic(symptomatic_teachers, 
                                scenarios = scenarios, 
                                permutation=permutation, 
                                figuresPath = figuresPath, 
                                suffix="teachers"))

mean(symptomatic_students[[4]]+symptomatic_teachers[[4]])
median(symptomatic_students[[4]]+symptomatic_teachers[[4]])

# ---------------------------------------------------------------------------- #
# Outbreak size 
# ---------------------------------------------------------------------------- #
(oss <- plot.outbreak.size(outbreak_size_students, 
                   scenarios[1:max], 
                   permutation = permutation,
                   iter=iter,
                   figuresPath=figuresPath, 
                   suffix="students",
                   occup_suffix="",
                   title="",
                   title_y="Number of school-related\ninfected students"))
# Teachers
(ost <- plot.outbreak.size(outbreak_size_teachers, 
                   scenarios[1:max], 
                   permutation=permutation,
                   iter=iter,
                   figuresPath=figuresPath, 
                   suffix="teachers",
                   occup_suffix="",
                   title="",
                   title_y="Number of school-related\ninfected teachers"))

(os_plot <- plot_grid(oss + theme(axis.text.x = element_blank()), ost, nrow=2, rel_heights=c(1,1.55)))
ggsave(os_plot, file=paste0("../results/",figuresPath, "outbreak_size_plot.pdf"), 
       width=16, height=14)

# Outbreak size vs introductions from community
outbreak_external_st <- lapply(outbreak_per_loc_st, function(x) as.data.frame(cbind(External=x[,3])))
(ose_plot <- plot.outbreak.size.vs.external(outbreak_size_students[permutation],
                                             outbreak_external_st[permutation],
                                             scenarios[1:max], 
                                             figuresPath=figuresPath,
                                             suffix="students",
                                             occup_suffix="",
                                             title="",
                                             title_y="Number of school-related infected"))

outbreak_external_teach <- lapply(outbreak_per_loc_teach, function(x) as.data.frame(cbind(External=x[,3])))
(oset_plot <- plot.outbreak.size.vs.external(outbreak_size_teachers[permutation],
                                            outbreak_external_teach[permutation],
                                            scenarios[1:max], 
                                            figuresPath=figuresPath,
                                            suffix="teachers",
                                            occup_suffix="",
                                            title="",
                                            title_y="Number of school-related infected"))

df_ratio_students <- lapply(1:length(outbreak_size_students), function(x) outbreak_size_students[[x]]/outbreak_external_st[[x]])
df_prop_students <- lapply(1:length(outbreak_size_students), function(x) sum(df_ratio_students[[x]]<=1)/nrow(df_ratio_students[[x]]))
unlist(df_prop_students[permutation])

df_ratio_teachers <- lapply(1:length(outbreak_size_teachers), function(x) outbreak_size_teachers[[x]]/outbreak_external_teach[[x]])
df_prop_teachers <- lapply(1:length(outbreak_size_teachers), function(x) sum(df_ratio_teachers[[x]]<=1, na.rm=T)/nrow(na.omit(df_ratio_teachers[[x]])))
unlist(df_prop_teachers[permutation])

df_ratio <- lapply(1:length(outbreak_size_teachers), function(x) (outbreak_size_teachers[[x]]+outbreak_size_students[[x]])/(outbreak_external_st[[x]]+outbreak_external_teach[[x]]))
df_prop <- lapply(1:length(outbreak_size_teachers), function(x) sum(df_ratio[[x]]<=1, na.rm=T)/nrow(na.omit(df_ratio[[x]])))
unlist(df_prop[permutation])

prop.outbreak.external(outbreak_size_students,
                       outbreak_external_st,
                       scenarios, 
                       permutation,
                       figuresPath=figuresPath,
                       suffix="students",
                       occup_suffix="",
                       title="",
                       title_y="Number of school-related infected")
# df_ratio <- df_ratio[permutation]
# names(df_ratio) <- scenarios
# head(melt(df_ratio))
# ggplot(melt(df_ratio), aes(y=value, x=L1)) + 
#   geom_violin() + 
#   geom_boxplot(width=0.1)

# ============================================================================ #
# Mean additional outbreak size
(add_oss <- plot.add.outbreak.size(outbreak_size_students, 
                       scenarios[1:max], 
                       permutation=permutation,
                       figuresPath=figuresPath, 
                       suffix="students",
                       occup_suffix="", 
                       title="", colors=colors_scenario))
# Mean additional outbreak size
(add_ost <- plot.add.outbreak.size(outbreak_size_teachers, 
                                  scenarios[1:max], 
                                  permutation=permutation,
                                  figuresPath=figuresPath, 
                                  suffix="teachers",
                                  occup_suffix=""))

unlist(lapply(outbreak_size_teachers, median))

add_os_plot <- plot_grid(add_oss + theme(axis.text.x = element_blank()), add_ost, nrow=2, rel_heights=c(1,1.55))
ggsave(add_os_plot, file=paste0("../results/",figuresPath, "additional_outbreak_size_plot.pdf"), 
       width=16, height=14)

# ============================================================================ #
# Plotting outbreak size per introduction case
# Students
plot.outbreak.size.per.case(outbreak_data_students[permutation], 
                            scenarios[1:max],
                            figuresPath=figuresPath, 
                            suffix="student",
                            occup_suffix="",
                            title="",
                            title_y="Mean outbreak size per introduction")
# Teachers
plot.outbreak.size.per.case(outbreak_data_teachers[permutation], 
                            scenarios[1:max],
                            figuresPath=figuresPath, 
                            suffix="teacher",
                            occup_suffix="",
                            title="",
                            title_y="Mean outbreak size per introduction")


# ============================================================================ #
# Plot infections per location
# ============================================================================ #
outbreak_external_st <- lapply(outbreak_per_loc_st, function(x) as.data.frame(cbind(External=x[,3])))
outbreak_external_teach <- lapply(outbreak_per_loc_teach, function(x) as.data.frame(cbind(External=x[,3])))

(ifc_plot <- plot.inf.per.location(outbreak_external_st, 
                      scenarios[1:max],
                      figuresPath=figuresPath, 
                      suffix="students",
                      occup_suffix="",
                      title="",
                      title_y="Number of infected students from community"))

(ifct_plot <- plot.inf.per.location(outbreak_external_teach, 
                      scenarios[1:max],
                      figuresPath=figuresPath, 
                      suffix="teachers",
                      occup_suffix="",
                      title="",
                      title_y="Number of infected teachers from community"))


# plot.inf.per.location(outbreak_per_loc_st, 
#                       scenarios[1:max],
#                       figuresPath=figuresPath, 
#                       suffix="students",
#                       occup_suffix="",
#                       title="",
#                       title_y="Number of infected students")
# 
# plot.inf.per.location(outbreak_per_loc_teach, 
#                       scenarios[1:scenario],
#                       figuresPath=figuresPath, 
#                       suffix="teachers",
#                       occup_suffix="",
#                       title="",
#                       title_y="Number of infected teachers")

# ============================================================================ #
# Plot absences
# ============================================================================ #
# Students
ads <- plot.abs.days(abs_days_students[permutation], 
              scenarios[1:max],
              figuresPath=figuresPath, 
              suffix="student",
              occup_suffix="",
              title="",
              title_y="Number of absent days per student", 
              no_ind = nrow(df_agent))

# Teachers
adt <- plot.abs.days(abs_days_teachers[permutation], 
              scenarios[1:max],
              figuresPath=figuresPath, 
              suffix="teacher",
              occup_suffix="",
              title="",
              title_y="Number of absent days per teacher", 
              no_ind = nrow(df_teacher))

abs_days_plot <- plot_grid(ads + theme(axis.text.x = element_blank()), adt, nrow=2, rel_heights=c(1,1.55))
ggsave(abs_days_plot, file=paste0("../results/",figuresPath, "abs_days_plot.pdf"), 
       width=16, height=14)

(ipas_plot <- plot.inf.per.agent(infector_students, scenarios, permutation, figuresPath, suffix="students"))
(ipat_plot <- plot.inf.per.agent(infector_teachers, scenarios, permutation, figuresPath, suffix="teachers"))
infectors <- lapply(1:length(infector_students), function(k) c(infector_students[[k]], infector_teachers[[k]]))
(ipa_plot <- plot.inf.per.agent(infectors, scenarios, permutation, figuresPath, suffix="combined"))

# Combine plots
paste0("../results/",figuresPath, name, "_plot.pdf")
pdf(file=paste0("../results/",figuresPath, name, "_plot.pdf"), width=16, height=14)
# symps_plot
# sympt_plot
os_plot
ose_plot
oset_plot
add_os_plot
# abs_days_plot
# ifc_plot
# ifct_plot
# ipa_plot
dev.off()



# Absent students
plot.abs(absences_students[permutation], 
         scenarios[1:max],
         figuresPath=figuresPath, 
         suffix="student",
         occup_suffix="",
         title="",
         title_y="Number of absent students")

# Absent teachers
plot.abs(absences_teachers[permutation], 
         scenarios[1:max],
         figuresPath=figuresPath, 
         suffix="teacher",
         occup_suffix="",
         title="",
         title_y="Number of absent teachers")


# ============================================================================ #
# SECONDARY CASES
# ============================================================================ #
# Students
plot.sec.cases(sec_cases_students[permutation], 
               scenarios[1:max],
               figuresPath=figuresPath, 
               suffix="students",
               occup_suffix="",
               title="",
               title_y="Number of secondary cases by students")

plot.sec.cases(sec_cases_teachers[permutation], 
               scenarios[1:max],
               figuresPath=figuresPath, 
               suffix="teachers",
               occup_suffix="",
               title="",
               title_y="Number of secondary cases by teachers")


plot.sec.cases.combined(sec_cases_students[permutation], 
                        scenarios[1:max],
                        figuresPath=figuresPath, 
                        suffix="students",
                        occup_suffix="",
                        title="",
                        title_y="Number of secondary cases by students")



# PLOTTING time series
# Students
student_data <- transform.df(time_steps, iter, ns_list[[1]])
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

