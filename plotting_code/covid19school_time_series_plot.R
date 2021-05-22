path <- "/figures/test/"
file_names <- c("full_occup", 
                "half_occup",
                "full_occup_vaccination",
                "full_occup_risk_based_testing",
                "halg_occup_risk_based_testing",
                "full_occup_screening_twice_weekly",
                "full_occup_screening_weekly")

ns_list <- vector(mode="list", length=length(file_names))
nst_list <- vector(mode="list", length=length(file_names))
for(i in 1:length(file_names)){
  load(paste0(path, paste0(file_names[i], ".RData")))
  ns_list[[i]] <- ns
  nt_list[[i]] <- nt
}