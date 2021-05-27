# =========================================================================== #
# Fixed variables 
# =========================================================================== #
seed=12345

day_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# =========================================================================== #
# Disease characteristics
# =========================================================================== #
infec_scale = 1.0 
rec_time = 7 # Recovery period
iso_time = 7 # Isolation period (period that individuals have to isolate at home)
fp_iso_time = 1 # False-positives need to wait approx. 24 hours for PCR result
quaran_time = 10

# Reproduction number
R0 = 1.05
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

### PCR Test sensitivity for time since infection
test_sens <- 0.9*c(0, 0.333333333, 0.454545455, 0.875, 
               0.96, 0.96, 0.92, 0.76, 0.64, 0.56, 0.25, 0.181818182, 0.045454545, 0)
test_sens_fun = smooth.spline(seq(0,length(test_sens)-1,by=1), test_sens, spar=0.3)
pcr_test_sens <- c(0, 0.666666667, 0.727272727, 1, 1, 1, 1, 1, 0.96, 0.88, 
                   0.791666667, 0.909090909, 0.727272727, 0.9, 0.8, 0.526315789, 
                   0.384615385, 0)
pcr_test_sens_fun = smooth.spline(seq(0,length(pcr_test_sens)-1,by=1), pcr_test_sens, spar=0.3)

### Data frame with infectiousness and test sensitivity for every hour
df_inf_sens = as.data.frame(cbind(time=time_vec_hour, 
                                  infectiousness=infectivity, 
                                  sensitivity=predict(test_sens_fun, time_vec_hour)$y, 
                                  pcr_sens=predict(pcr_test_sens_fun, time_vec_hour)$y))
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x<0, 0, x)) # Force negative values to be zero
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x>1, 1, x))

# =========================================================================== #
# General school characteristics
# =========================================================================== #
class_size <- c(23, 29, 23, 29, 30, 24) # Number of student in each class
n_grades <- 6                           # Number of grades
n_class <- c(7,6,8,7,5,3)               # Number of classes per grade
grade_size <- n_class*class_size        # Total number of students per grade
n_students <- sum(grade_size)           # Total number of students 

### Contact network
# Option for contacts of students to be sampled daily (not used)
sample_contacts_daily <- F

### Contact matrix for students (from pilot study)
### MEDIAN VALUES
# n_cont_same_class <- c(5,4,5,6,6,9)
# n_cont_vec <- c(1, 0, 0, 0, 0, 0,
#                 0, 2, 0, 0, 0, 0,
#                 0, 0, 5, 0, 0, 0,
#                 0, 0, 0, 7, 0, 0,
#                 0, 0, 0, 0, 7, 0,
#                 0, 0, 0, 0, 0, 7)

### MEAN values
n_cont_same_class <- c(6,5,6,8,8,11)
n_cont_vec <- c(3, 0, 0, 0, 0, 0,
                2, 5, 0, 0, 0, 0,
                1, 1, 5, 0, 0, 0,
                1, 1, 2, 7, 1, 1,
                0, 0, 0, 1, 7, 1,
                0, 0, 0, 1, 2, 7)
n_cont <- matrix(n_cont_vec, nrow=6, byrow = T)
n_cont_close <- 1 # Number of close contacts in class (students are sitting in pairs) 
n_cont_grade <- diag(n_cont) # Number of contacts outside class but in school (students of the same grade)
n_cont_out_school <- 2 # Number of contacts with students of the school but outside of school
n_cont_other <- 3 # Number of contacts with other individuals not from school
n_random <- 0
n_subjects <- 5 # Number of subjects per grade

n_quaran <- 2 # Number of students that need to be quarantined (from pilot study)
n_cont_teachers <- 6
n_teacher_cont_other <- 4


# =========================================================================== #
# Parameters (to be changed)
# =========================================================================== #
iter=1

### Results path
resultsPath="../../results/"
folder='simA11'
if(!file.exists(paste0(resultsPath,folder))) dir.create(file.path(paste0(resultsPath,folder)))
print(paste0("File path = ", resultsPath,folder))

### Scenarios
suffix <- c("", "", "_vaccination","_risk_based_testing","_screening_twice_vaccination","_screening_twice_weekly")
scenarios <- c("Full occup", 
               "Half occup", 
               "Vaccination (full)", 
               "Risk based testing (half)",
               "Screening twice + Vaccination (full)",
               "Screening twice weekly (full)")
if(length(suffix)!=length(scenarios)) print("Warning: Number of suffix and number of scenarios do not coincide!")

###
tol = 1e-5

###
no_int_vec = c(F,rep(F, 5))

### Proportion of effective contacts of teachers
# 0.18 for half occupancy, 0.13 for full occupancy
# sample_prop = c(0.1, 0.25, 0.1, 0.25, 0.1, 0.1)
sample_prop = c(0.13, 0.18, 0.13, 0.18, 0.13, 0.13)
# sample_prop = c(0.2, 0.3, 0.2, 0.3, 0.2, 0.2)

### Scaling factor for transmission probability for teacher-student contact
cont_susc_scale = c(0.5, 0.85)

### Scaling factor for transmission probability from teachers
prop_vaccinated = c(0, 0.85)

### External incidence rate for importations
external_prob = 1.5*c(0.0007965315, 0.0007965315)

### Adherence to screening
screening_adherence <- 0.5
risk_testing_adherence <- 0.5

### Compliance to isolation and quarantine
compliance_iso <- 1.0 # Proportion of individuals that adhere to isolation
compliance_quaran <- 1.0 # Proportion of individuals that adhere to quarantine

### Time
unit=24
steps = 8/24; time_period = 4*7; time_steps = seq(1, time_period, by = steps)
group_per_step = rep(c(rep(1,(1/steps)*(24/unit)),rep(2,(1/steps)*(24/unit))), time_period/2)[1:length(time_steps)] # Assign groups to each time point, assuming 2 groups

### Types of individuals
types <- c("S", "PS", "IS", "IA", "R", "IH", "Q")

### Screening days
testing_mat <- matrix(c(rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        rep(0, times=7), 
                        c(1,0,1,0,0,0,0),
                        c(1,0,1,0,0,0,0)), ncol=7, byrow=T)
colnames(testing_mat) <- day_names
if(nrow(testing_mat)!=length(scenarios)) print("Matrix for testing matrix does not have the right dimensions.")


### Occupancy
occup_suffix <- c("full_occup",
                  "half_occup",
                  "full_occup",
                  "half_occup",
                  "full_occup",
                  "full_occup")
occup <- c(1, 0.5, 
           1, 0.5,
           1, 1) # Occupancy level, options: 0.5, 1

### Flags for screening, risk-based testing and vaccination
scr_flags <- c(F,F,F,F,T,T); risk_flags <- c(F,F,F,T,F,F)
vacc_flag <- c(F,F,T,F,T,F)

## ============================================================================ #
# Absent days and absent individuals
abs_days_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_students <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
abs_days_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_teachers <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Peak number of infected individuals
peak_students <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))
peak_teachers <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))

# Infector
infector_students <- infector_teachers <- vector(mode="list", length=length(scenarios))

# Contacts between students and teachers
contact_student_teachers <- vector(mode="list", length=length(scenarios))

# Outbreak size
outbreak_data_students <- outbreak_data_teachers <- vector(mode="list", length=length(scenarios))
outbreak_size_students <- outbreak_size_teachers <- rep(list(rep(0,iter)), length(scenarios))

# Outbreak size per "location"
outbreak_per_loc_st <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
outbreak_per_loc_teach <- rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Symptomatic cases
symptomatic_students <- rep(list(rep(0,iter)), length(scenarios))
symptomatic_teachers <- rep(list(rep(0,iter)), length(scenarios))

# Secondary cases
sec_cases_students <- sec_cases_teachers <- vector(mode="list", length=length(scenarios))
sec_symp_students <- sec_symp_teachers <- vector(mode="list", length=length(scenarios))

# Detected students and teachers by risk-based testing
det_risk_students <- det_risk_teachers <- vector(mode="list", length=length(scenarios))

# Detected students and teachers by risk-based testing
IA_quaran_students <- IS_quaran_students <- vector(mode="list", length=length(scenarios))
IA_iso_teachers <- IA_iso_students <- vector(mode="list", length=length(scenarios))

# Number over time
ns_list <- vector(mode="list", length=length(scenarios))
nt_list <- vector(mode="list", length=length(scenarios))
new_symp <- rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))
epidemic_list <- vector(mode="list", length=length(scenarios))