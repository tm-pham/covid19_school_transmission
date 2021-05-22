contact_mat_school <- read.csv(paste0("../data/Mistry2021/Netherlands_country_level_F_school_setting_85.csv"),
                               header = F)

contact_mat_school <- 11.41*contact_mat_school


contact_mat_students <- round(contact_mat_school[12:17, 12:17], 1)
contact_mat_students
write.table(contact_mat_students, file="../data/Mistry2021/contact_mat_students.csv", 
            row.names = F, col.names = F, sep=",")

contact_mat_teachers <- round(contact_mat_school[20:65, 20:65], 1)
contact_tt <- as.data.frame(cbind(value=apply(contact_mat_teachers, 1, sum)))
contact_tt <- as.data.frame(cbind(age = as.numeric(rownames(contact_tt)), value=contact_tt$value))
contact_tt <- contact_tt[which(contact_tt$value!=0),]
contact_tt

contact_tt_plot <- ggplot(contact_tt, aes(x=age, y=value)) + 
                    geom_bar(stat="identity") + 
                    geom_hline(yintercept=mean(contact_tt$value), color="red") + 
                    geom_hline(yintercept=median(contact_tt$value), color="blue") + 
                    labs(y="Number of contacts") + 
                    theme_publication()
contact_tt_plot
ggsave(contact_tt_plot, file="../data/Mistry2021/contact_teachers_barplot.pdf", width=12, height=9)

contact_mat_stud_teachers <- round(contact_mat_school[12:17, 20:65], 1)
contact_stud_teachers <- as.data.frame(cbind(value=apply(contact_mat_stud_teachers, 1, sum)))
contact_stud_teachers <- as.data.frame(cbind(age = as.numeric(rownames(contact_stud_teachers)), 
                                             value=contact_stud_teachers$value))
contact_stud_teachers

contact_mat_teachers_stud <- round(contact_mat_school[20:65, 12:17], 1)
contact_teachers_stud <- as.data.frame(cbind(value=apply(contact_mat_teachers_stud, 1, sum)))
contact_teachers_stud <- as.data.frame(cbind(age = as.numeric(rownames(contact_teachers_stud)), 
                                             value=contact_teachers_stud$value))
contact_teachers_stud

