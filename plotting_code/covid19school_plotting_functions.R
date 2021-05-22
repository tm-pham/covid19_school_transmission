# ============================================================================ #
# Plotting functions
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# Theme for ggplot 
# ---------------------------------------------------------------------------- #
theme_publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme_bw() 
   + theme(axis.line = element_line(colour="black"),
           axis.title = element_text(size=24),
           axis.text=element_text(size=22),
           strip.text = element_text(size=22),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.grid.minor=element_line(color="gray94", linetype = 'solid'),
           panel.grid.major=element_line(color="gray94", linetype = 'solid')
   ))
}

# ---------------------------------------------------------------------------- #
# Colors
# ---------------------------------------------------------------------------- #
colors_scenarios = c("#0e215b", 
                      "#07b4f9", 
                      "#e1359d", 
                      "#6ccf43",
                      "#ff8000", 
                      "#cc0000")


# ---------------------------------------------------------------------------- #
# Outbreak size 
# ---------------------------------------------------------------------------- #
plot.outbreak.size <- function(outbreak_size, 
                               scenarios, 
                               permutation=NULL,
                               iter=1, 
                               figuresPath="/figures/low_external_foi/", 
                               suffix="students",
                               occup_suffix="half_occup",
                               title="STUDENTS",
                               title_y="Total outbreak size", 
                               colors=NULL){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation]
  }
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- outbreak_size[[i]]
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  
  inf_plot <- ggplot(inf, aes(x=variable, y=value, fill=variable)) + 
                geom_violin(width=1.0) + 
                geom_boxplot(width=0.1, fill="white") + 
                labs(y=title_y, title = title) + 
                scale_fill_manual(values=colors) + 
                theme_publication() + 
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size=20,angle=45,hjust=1.0),
                      legend.position = "none")
  ggsave(inf_plot, file=paste0("../results", figuresPath, "outbreak_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(inf_plot)
}

# ---------------------------------------------------------------------------- #
# Outbreak size per introduction case
# ---------------------------------------------------------------------------- #
plot.outbreak.size.per.case <- function(outbreak_data, 
                                        scenarios, 
                                        figuresPath="/figures/low_external_foi/", 
                                        suffix="students",
                                        occup_suffix="half_occup",
                                        title="STUDENTS", 
                                        title_y="Mean outbreak size per introduction"){
  names(outbreak_data) <- scenarios
  df_outbreak <- reshape2::melt(lapply(outbreak_data, function(x) unlist(lapply(x, function(y) mean(y, na.rm=T)))))
  df_outbreak <- as.data.frame(df_outbreak)
  colnames(df_outbreak) <- c("value", "scenario")
  df_outbreak$scenario <- factor(df_outbreak$scenario, levels=names(outbreak_data))
  
  plot <- ggplot(df_outbreak, aes(y=value, x=scenario, fill=scenario)) + 
            geom_violin(wdith=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y, title=title) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(angle=45, hjust=1, size=18),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "outbreak_size_per_case_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

# ---------------------------------------------------------------------------- #
# Scatterplot outbreak size vs introductions from community
# ---------------------------------------------------------------------------- #
plot.outbreak.size.vs.external <- function(outbreak_size, 
                                           outbreak_external,
                                           scenarios, 
                                           figuresPath="/figures/sim/", 
                                           suffix="students",
                                           occup_suffix="half_occup",
                                           title="STUDENTS",
                                           title_y="Number of scool-related infected", 
                                           colors=colors_scenarios){
  names(outbreak_size) <- scenarios
  names(outbreak_external) <- scenarios
  df <- cbind(melt(outbreak_size)[2], melt(outbreak_size)[1], melt(outbreak_external)[2])
  colnames(df) <- c("scenario", "outbreak_size", "external")
  df <- as.data.frame(df)
  df$scenario <- factor(df$scenario, levels=scenarios)
  plot <- ggplot(df, aes(x=external,y=outbreak_size, color=scenario)) + 
    facet_wrap(~scenario) + 
    geom_point() + 
    geom_abline(slope=1, intercept=0) + 
    labs(y = paste(title_y, suffix), 
         x="Total number of introductions from community",
         title=title, colour="Strategies") + 
    scale_x_continuous(limits=c(0, max(df$outbreak_size))) +
    scale_color_manual(values=colors_scenarios[permutation]) +
    theme_publication() + 
    theme(legend.position = "none", 
          strip.text = element_text(size=16))
    # theme(legend.text = element_text(size=16),
    #       legend.title = element_text(size=18),
    #       legend.position = "right",
    #       legend.background = element_blank(),
    #       legend.box.background = element_rect(colour = "black", fill="white"))
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "outbreak_size_vs_external_", suffix, occup_suffix,"_plot.pdf"), 
         width=12, height=9)
  return(plot)
}


prop.outbreak.external <- function(outbreak_size, 
                                   outbreak_external,
                                   scenarios, 
                                   permutation,
                                   figuresPath="/figures/sim/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Number of scool-related infected", 
                                   colors=colors_scenarios){
  
  if(length(permutation)!=0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }
  df_ratio <- lapply(1:length(outbreak_size_students), function(x) outbreak_size_students[[x]]/outbreak_external_st[[x]])
  df_prop <- lapply(1:length(outbreak_size_students), function(x) sum(df_ratio[[x]]<=1)/nrow(df_ratio[[x]]))
  names(df_prop) <- scenarios
  df_plot <- melt(df_prop)
  names(df_plot) <- c("value", "scenario")
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  plot <- ggplot(df_plot, aes(x=scenario, y=value, fill=scenario)) + 
            geom_bar(stat="identity") + 
            theme_publication() + 
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_text(size=18,angle=45,hjust=1.0))
  plot
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Reduction of outbreak size with respect to first scenario in list
# ---------------------------------------------------------------------------- #
plot.red.outbreak.size <- function(outbreak_size, 
                                   scenarios, 
                                   figuresPath="/figures/low_external_foi/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Total outbreak size"){
  mean_outbreak_size <- unlist(lapply(outbreak_size, mean))
  red_os <- (mean_outbreak_size[1]-mean_outbreak_size)/mean_outbreak_size[1]
  
  df_red_os <- as.data.frame(cbind(scenario=scenarios, value=red_os))
  df_red_os$scenario <- factor(df_red_os$scenario, levels=scenarios)
  df_red_os <- df_red_os[-1,]
  
  plot <- ggplot(df_red_os, aes(x=scenario, y=100*as.numeric(value))) + 
            geom_bar(stat="identity") + 
            labs(y="Reduction w.r.t. full occupancy") + 
            theme_publication() + 
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_text(size=18,angle=45,hjust=1.0))
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "red_outbreak_size_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Additional outbreak size with respect to first scenario in list
# ---------------------------------------------------------------------------- #
plot.add.outbreak.size <- function(outbreak_size, 
                                   scenarios, 
                                   permutation=NULL,
                                   figuresPath="/figures/low_external_foi/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Total outbreak size", 
                                   colors=colors_scenarios){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation][-1]
  }else{
    colors <- colors[-1]
  }
  mean_outbreak_size <- unlist(lapply(outbreak_size, mean))
  add_os <- (mean_outbreak_size-mean_outbreak_size[1])*1000
  
  df_add_os <- as.data.frame(cbind(scenario=scenarios, value=add_os))
  df_add_os$scenario <- factor(df_add_os$scenario, levels=scenarios)
  df_add_os <- df_add_os[-1,]
  
  plot <- ggplot(df_add_os, aes(x=scenario, y=as.numeric(value), fill=scenario)) + 
    geom_bar(stat="identity") + 
    labs(y=paste("Increase in school-related number\n of infected", suffix, "(for 1000 schools)")) + 
    scale_fill_manual(values=colors) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(size=18,angle=45,hjust=1.0),
          axis.text.y = element_text(size=16),
          axis.title.y = element_text(size=20),
          legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "add_outbreak_size_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}

# ---------------------------------------------------------------------------- #
# Plot number of symptomatic cases 
# ---------------------------------------------------------------------------- #
plot.symptomatic <- function(symptomatic, 
                             scenarios, 
                             permutation=NULL, 
                             figuresPath, 
                             suffix="combined"){
  if(length(permutation)>0){
    symptomatic <- symptomatic[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- colors_scenarios
  names(symptomatic) <- scenarios
  df_symptomatic <- melt(symptomatic)
  names(df_symptomatic) <- c("value", "scenario")
  df_symptomatic$scenario <- factor(df_symptomatic$scenario, levels=scenarios)
  
  plot <- ggplot(df_symptomatic, aes(x=scenario, y=value, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=paste("Number of symptomatic",suffix)) + 
            scale_fill_manual(values=colors) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(angle=45, hjust=1, size=18),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "symptomatic_",suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}

# ---------------------------------------------------------------------------- #
# Number of infections per agent type
# ---------------------------------------------------------------------------- #
plot.inf.per.agent <- function(infector, 
                               scenarios, 
                               permutation=NULL, 
                               figuresPath, 
                               suffix=""){
  if(length(permutation)!=0){
    infector <- infector[permutation]
    names(infector) <- scenarios
    colors_scenarios <- colors_scenarios[permutation]
  }
  inf_list <- infectors_st <- ind_case_student <- ind_case_teacher <- prop_ind_student <- prop_ind_teacher <- vector(mode="list", length=length(infector))
  inf_list<-  lapply(infector, function(x) unlist(x))
  for(k in 1:length(infector)){
    infectors_st[[k]] <- names(inf_list[[k]])
    ind_case_teacher[[k]] <- sum(inf_list[[k]][stringr::str_detect(infectors_st[[k]], "T")])
    ind_case_student[[k]] <- sum(inf_list[[k]][!stringr::str_detect(infectors_st[[k]], "T")])
    prop_ind_student[[k]] <- ind_case_student[[k]]/(ind_case_student[[k]] + ind_case_teacher[[k]])
    prop_ind_teacher[[k]] <- 1-prop_ind_student[[k]]
  }
  names(prop_ind_student) <- names(prop_ind_teacher) <- scenarios
  df_ind_student <- melt(prop_ind_student)
  df_ind_teacher <- melt(prop_ind_teacher)
  names(df_ind_student) <- names(df_ind_teacher) <- c("value", "scenario")
  df <- cbind(scenario=df_ind_student$scenario, student=df_ind_student$value, teacher=df_ind_teacher$value)
  df <- as.data.frame(df)
  df[,-1] <- apply(df[,-1], 2, as.numeric)
  df$scenario <- factor(df$scenario, levels=scenarios)
  df_plot <- melt(df)
  plot <- ggplot(df_plot, aes(x=variable, y=value, fill=scenario)) + 
            facet_wrap(~scenario) + 
            labs(y=paste0("Proportion of index cases\n(", suffix,")"))+ 
            geom_bar(stat="identity") + 
            scale_fill_manual(values=colors_scenarios) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(), 
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "index_case_per_agent_plot_",suffix,".pdf"), 
         width=16, height=9)
  return(plot)
}



# ---------------------------------------------------------------------------- #
# Number of infections per location
# ---------------------------------------------------------------------------- #
plot.inf.per.location <- function(outbreak_per_loc, 
                                  scenarios, 
                                  figuresPath="/figures/low_external_foi/", 
                                  suffix="students",
                                  occup_suffix="half_occup",
                                  title="STUDENTS", 
                                  title_y="Number of infections"){
  # outbreak_per_loc <- list(outbreak_per_loc[[1:length(scenarios)]])
  names(outbreak_per_loc) <- scenarios
  df_outbreak_per_loc <- melt(outbreak_per_loc)
  colnames(df_outbreak_per_loc) <- c("variable", "value", "scenario")
  df_outbreak_per_loc$scenario <- factor(df_outbreak_per_loc$scenario, levels=names(outbreak_per_loc))
  plot <- ggplot(df_outbreak_per_loc, aes(x=scenario, y=value, fill=scenario)) + 
                 geom_violin(width=1.0) + 
                 geom_boxplot(width=0.1, fill="white") + 
                 labs(y=title_y, title = title) + 
                 scale_fill_manual(values=colors_scenarios[permutation]) + 
                 theme_publication() + 
                 theme(axis.title.x = element_blank(),
                       axis.text.x = element_text(angle=45, hjust=1,size=18),
                       legend.position = "none")
  ggsave(plot, file=paste0("../results", figuresPath, "inf_community_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Plot absent days per individual
# ---------------------------------------------------------------------------- #
plot.abs.days <- function(abs_days, 
                          scenarios, 
                          figuresPath="/figures/low_external_foi/", 
                          suffix="student",
                          occup_suffix="half_occup",
                          title="STUDENTS", 
                          title_y="Number of absent days per student",
                          no_ind){
  names(abs_days) <- scenarios
  df_abs_days <- melt(abs_days)
  df_abs_days$value <- df_abs_days$value/no_ind
  colnames(df_abs_days) <- c("variable", "value", "scenario")
  df_abs_days$scenario <- factor(df_abs_days$scenario, levels=names(abs_days))
  plot <- ggplot(df_abs_days%>%filter(variable=="Total absent"), aes(x=scenario, y=value, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=18,angle=45,hjust=1.0),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "absent_days_per_", suffix, occup_suffix, "_plot.pdf"),         width=16, height=9)
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Plot absent individuals
# ---------------------------------------------------------------------------- #
plot.abs <- function(absences, 
                     scenarios, 
                     figuresPath="/figures/low_external_foi/", 
                     suffix="students",
                     occup_suffix="half_occup",
                     title="STUDENTS", 
                     title_y="Number of absent students"){
  names(absences) <- scenarios
  df_abs <- melt(absences)
  df_abs <- df_abs[df_abs$variable=="Total absent",]
  colnames(df_abs) <- c("variable", "value", "scenario")
  df_abs$scenario <- factor(df_abs$scenario, levels=names(absences))
  plot <- ggplot(df_abs%>%filter(variable=="Total absent"), aes(x=scenario, y=value, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=18,angle=45,hjust=1.0),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "absent_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}



# ---------------------------------------------------------------------------- #
# Plot secondary cases
# ---------------------------------------------------------------------------- #
plot.sec.cases <- function(sec_cases, 
                           scenarios, 
                           figuresPath="/figures/low_external_foi/", 
                           suffix="students",
                           occup_suffix="half_occup",
                           title="STUDENTS", 
                           title_y="Number of secondary cases"){
  names(sec_cases) <- scenarios
  sec_data <- lapply(sec_cases, function(x) unlist(lapply(x, function(y) mean(y, na.rm=T))))
  df_sec <- as.data.frame(reshape2::melt(sec_data))
  colnames(df_sec) <- c("value", "scenario")
  df_sec$scenario <- factor(df_sec$scenario, levels=names(sec_cases))
  plot <- ggplot(df_sec, aes(y=value, x=scenario, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y, title=title) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=18,angle=45,hjust=1.0),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "sec_cases_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}


plot.sec.cases.combined <- function(sec_cases, 
                                     scenarios, 
                                     figuresPath="/figures/low_external_foi/", 
                                     suffix="students",
                                     occup_suffix="half_occup",
                                     title="STUDENTS", 
                                     title_y="Number of secondary cases"){
  names(sec_cases) <- scenarios
  sec_data <- lapply(sec_cases, function(x) unlist(x))
  mean_sec_data <- lapply(sec_data, function(x) mean(x))
  cl_sec_data <- lapply(sec_data, function(x) quantile(x, probs=0.025))
  cu_sec_data <- lapply(sec_data, function(x) quantile(x, probs=0.975))
  df_sec <- as.data.frame(reshape2::melt(mean_sec_data))
  colnames(df_sec) <- c("value", "scenario")
  df_sec$scenario <- factor(df_sec$scenario, levels=names(sec_cases))
  plot <- ggplot(df_sec, aes(y=value, x=scenario, fill=scenario)) + 
    # facet_wrap(~scenario) +
    # geom_violin(width=1.0) +
    geom_boxplot(width=0.1) + 
    labs(y=title_y, title=title) + 
    scale_fill_manual(values=colors_scenarios[permutation]) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45, hjust=1, size=18),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  plot
  ggsave(plot, file=paste0("../results", figuresPath, "sec_cases_combined_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

