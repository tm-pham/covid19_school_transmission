rm(list=ls())
name <- "simJ1"
epi <- vector(mode="list", length=6)
for(k in 1:6){
  epi[[k]] <- mget(load(paste0("../results/", name, "/", name, "_", k, ".RData")))
}

epi[[1]]$sample_prop
epi[[1]]$scaling
epi[[1]]$cont_susc_scale
epi[[1]]$screening_adherence
epi[[1]]$risk_testing_adherence
# table(epi[[3]]$df_agent$infector)
# table(epi[[3]]$df_teacher$infector)
# epi[[3]]$outbreak_data_students
# table(epi[[3]]$df_agent[epi[[3]]$df_agent$location%in%c(0,1),"location"])
# epi[[3]]$outbreak_size_students[[3]]
# epi[[3]]$cont_grade
mean(unlist(lapply(epi[[1]]$cont_ts, function(x) length(x))))
mean(unlist(lapply(epi[[2]]$cont_ts, function(x) length(x))))
# mean(unlist(lapply(epi[[3]]$cont_ts, function(x) length(x))))
# mean(unlist(lapply(epi[[4]]$cont_ts, function(x) length(x))))
# mean(unlist(lapply(epi[[5]]$cont_ts, function(x) length(x))))
# mean(unlist(lapply(epi[[6]]$cont_ts, function(x) length(x))))
# epi[[3]]$infect
# epi[[4]]$scenario
# epi[[4]]$sample_prop[scenario]
# head(epi[[4]]$df_history)
# 
# lapply(epi[[2]]$contact_student_teachers[[2]], mean)

# epi[[1]]$cont_class[[1]]
# epi[[1]]$susc_teacher
# epi[[6]]$df_screening
# epi[[5]]$df_screening
# epi[[3]]$df_agent
# epi[[4]]$df_risk_testing
# scenarios

# sapply(1:length(cont_cass), function(x) all(epi[[2]]$cont_class[[x]]%in%epi[[2]]$df_agent[df_agent$group==1,"id"]))


paste0("../results/", name, "/", name, "_1.RData")
load(paste0("../results/", name, "/", name, "_1.RData"))

for(k in 2:6){
  outbreak_size_students[[k]] <- epi[[k]]$outbreak_size_students[[k]]
  outbreak_size_teachers[[k]] <- epi[[k]]$outbreak_size_teachers[[k]]
  outbreak_data_students[[k]] <- epi[[k]]$outbreak_data_students[[k]]
  outbreak_data_teachers[[k]] <- epi[[k]]$outbreak_data_teachers[[k]]
  outbreak_per_loc_st[[k]] <- epi[[k]]$outbreak_per_loc_st[[k]]
  outbreak_per_loc_teach[[k]] <- epi[[k]]$outbreak_per_loc_teach[[k]]
  abs_days_students[[k]] <- epi[[k]]$abs_days_students[[k]]
  absences_students[[k]] <- epi[[k]]$absences_students[[k]]
  sec_cases_students[[k]] <- epi[[k]]$sec_cases_students[[k]]
  ns_list[[k]] <- epi[[k]]$ns_list
  nt_list[[k]] <- epi[[k]]$nt_list
  abs_days_teachers[[k]] <- epi[[k]]$abs_days_teachers[[k]]
  absences_teachers[[k]] <- epi[[k]]$absences_teachers[[k]]
  sec_cases_teachers[[k]] <- epi[[k]]$sec_cases_teachers[[k]]
  infector_students[[k]] <- epi[[k]]$infector_students[[k]]
  infector_teachers[[k]] <- epi[[k]]$infector_teachers[[k]]
  contact_student_teachers[[k]] <- epi[[k]]$contact_student_teachers[[k]]
  symptomatic_students[[k]] <- epi[[k]]$symptomatic_students[[k]]
  symptomatic_teachers[[k]] <- epi[[k]]$symptomatic_teachers[[k]]
}

print(paste0("../results/", name, "/", name, "1-6.RData"))
save.image(paste0("../results/", name, "/", name, "1-6.RData"))


sect <- sec_cases_teachers[[4]]
secs <- sec_cases_students[[4]]
table(unlist(sec_cases_teachers[[4]]))
table(unlist(sec_cases_students[[4]]))


tab_sec_students <- lapply(epi[[4]]$sec_symp_students[[4]], function(x) table(x))
transm_rate_students <- vector(mode="numeric", length=length(tab_sec_students))
for(l in 1:length(tab_sec_students)){
  if(length(tab_sec_students[[l]])==1) transm_rate_students[l] <- 0
  else{
    transm_rate_students[l] <- sum(tab_sec_students[[l]][-1])/(sum(tab_sec_students[[l]]))
  }
}
mean(transm_rate_students)


tab_sec_teachers <- vector(mode="list", length=length(sec_cases_teachers[[4]]))
transm_rate_teachers <- vector(mode="numeric", length=length(tab_sec_teachers))
for(l in 1:length(tab_sec_teachers)){
  temp <- epi[[4]]$sec_symp_teachers[[4]][[l]]
  if(length(temp)>0){
    tab_sec_teachers[[l]] <- table(temp)
  }else tab_sec_teachers[[l]] <- numeric(0)
  if(length(tab_sec_teachers[[l]])==1) transm_rate_teachers[l] <- 0
  else{
    transm_rate_teachers[l] <- sum(tab_sec_teachers[[l]][-1])/(sum(tab_sec_teachers[[l]]))
  }
}
transm_rate_teachers
mean(transm_rate_teachers, na.rm=T)/mean(transm_rate_students)

transm_teachers <- transm_rate_teachers
transm_teachers[is.na(transm_teachers)] <- 0
mean(transm_teachers)/mean(transm_rate_students)

table(unlist(epi[[4]]$sec_symp_students[[4]]))
table(unlist(epi[[4]]$sec_symp_teachers[[4]]))

table(unlist(epiR[[4]]$sec_cases_students[[4]]))
table(unlist(epiR[[4]]$sec_cases_teachers[[4]]))

sum(table(unlist(epi[[4]]$sec_symp_students[[4]])))/(sum(table(unlist(epi[[4]]$sec_symp_teachers[[4]]))) + sum(table(unlist(epi[[4]]$sec_symp_students[[4]]))))
sum(table(unlist(epi[[4]]$sec_cases_students[[4]])))/(sum(table(unlist(epi[[4]]$sec_cases_teachers[[4]]))) + sum(table(unlist(epi[[4]]$sec_cases_students[[4]]))))

unlist(epi[[4]]$det_risk_students[[4]])
unlist(epi[[4]]$det_risk_teachers[[4]])

unlist(epi[[4]]$IA_quaran_students)
unlist(epi[[4]]$IS_quaran_students)

unlist(epi[[4]]$IA_iso_students)
unlist(epi[[4]]$IA_iso_teachers)






