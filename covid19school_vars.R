# =========================================================================== #
# Fixed variables 
# =========================================================================== #
source("covid19school_contact_mat.R")
seed=12345

day_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# =========================================================================== #
# Disease characteristics
# =========================================================================== #
infec_scale = 1.0 # https://www.medrxiv.org/content/10.1101/2021.03.06.21252964v1.full.pdf

rec_time = 7 # Recovery period
iso_time = 7 # Isolation period (period that individuals have to isolate at home)
fp_iso_time = 1 # False-positives need to wait approx. 24 hours for PCR result
quaran_time = 10

# Reproduction number
R0 = 4.05
# scaling <- 1/avg_cont
scaling = 1.0

# Values for Infectiousness profile for time since infection
time_vec_hour = seq(0, 28 , by=1/24)
gen_time = dweibull(time_vec_hour, shape=1.6, scale=6.84)
infectivity = gen_time*R0*scaling
# infectivity <- c(0.0369, 0.0491, 0.0835, 0.1190, 
#                  0.1439, 0.1497, 0.1354, 0.1076, 0.0757, 0.0476, 0.0269, 0.0138, 0.0064, 0.0044)
# # Function for Infectiousness profile
# infectivity_fun <- smooth.spline(seq(1, length(infectivity)), infectivity, spar=0.2)

# =========================================================================== #
# Testing characteristics
# =========================================================================== #
spec = 1.0 # Specifcity, for now: 100% 

# PCR Test sensitivity for time since infection
test_sens <- 0.9*c(0, 0.333333333, 0.454545455, 0.875, 
               0.96, 0.96, 0.92, 0.76, 0.64, 0.56, 0.25, 0.181818182, 0.045454545, 0)
test_sens_fun = smooth.spline(seq(0,length(test_sens)-1,by=1), test_sens, spar=0.3)
pcr_test_sens <- c(0, 0.666666667, 0.727272727, 1, 1, 1, 1, 1, 0.96, 0.88, 
                   0.791666667, 0.909090909, 0.727272727, 0.9, 0.8, 0.526315789, 
                   0.384615385, 0)
pcr_test_sens_fun = smooth.spline(seq(0,length(pcr_test_sens)-1,by=1), pcr_test_sens, spar=0.3)

# Data frame with infectiousness and test sensitivity for every hour
df_inf_sens = as.data.frame(cbind(time=time_vec_hour, 
                                  infectiousness=infectivity, 
                                  sensitivity=predict(test_sens_fun, time_vec_hour)$y, 
                                  pcr_sens=predict(pcr_test_sens_fun, time_vec_hour)$y))
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x<0, 0, x)) # Force negative values to be zero
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x>1, 1, x))

# df_inf_sens = as.data.frame(cbind(time=time_vec_hour,
#                                   infectiousness=infectivity,
#                                   sensitivity=1,
#                                   pcr_sens=1))

# test_interval <- 1 # Testing interval is with respect to days
# screening_flag <- T
# risk_based_flag <- F

# =========================================================================== #
# General school characteristics
# =========================================================================== #
class_size <- c(23, 29, 23, 29, 30, 24) # Number of student in each class
n_grades <- 6 # Number of grades
n_class <- c(7,6,8,7,5,3) # Number of classes per grade
grade_size <- n_class*class_size # Total number of students per grade
n_students <- sum(grade_size) # Total number of students 

# Contact network
sample_contacts_daily <- F
# Contact matrix for students (from pilot study)
n_cont_vec <- c(4, 0, 0, 0, 0, 0, 
                2, 5, 1, 1, 0, 0, 
                1, 1, 5, 0, 0, 0, 
                2, 2, 3, 7, 1, 1, 
                0, 0, 0, 1, 6, 1, 
                0, 0, 0, 0, 1, 6)
n_cont <- matrix(n_cont_vec, nrow=6, byrow = T)
n_cont_close <- 1 # Number of close contacts in class (students are sitting in pairs)
n_cont_class <- diag(n_cont) # Number of contacts in class (excluding the close contact)
n_cont_grade <- 3 # Number of contacts outside class but in school (students of the same grade)
n_cont_out_school <- 2 # Number of contacts with students of the school but outside of school
n_cont_other <- 3 # Number of contacts with other individuals not from school
n_random <- 0
n_subjects <- 5 # Number of subjects per grade

n_quaran <- 2 # Number of students that need to be quarantined (from pilot study)
n_cont_teachers <- 6
n_teacher_cont_other <- 4

# ============================================================================ #
# Parameters for adherence
# NOT USED YET
# Note: May change to level of adherence per individuals instead of proportion 
# of adherence
adh_iso <- 0.9 # Proportion of individuals that adhere to isolation
adh_quaran <- 0.8 # Proportion of individuals that adhere to quarantine
