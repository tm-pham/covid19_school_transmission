
n_stud <- 800 # Total number of students 
n_teach <- 100  # Total number of teachers
n_staff <- 20 # Total number of other staff
occup <- 0.3 # Occupancy level
prop_cons <- 0.5 # Proportion of individuals with informed consent (who agrees to be tested)
spec <- 0.996 # Specifcity 
time_period <- 4*7 # Time period: One month

class_size <- 25 # Number of student in each class
grade_size <- 250  # Number of students per grade
n_grades <- 5 # Number of grades
n_class <- 10 # Number of classes per grade
n_students <- grade_size*n_grades
n_close_cont <- 5

# Parameters for adherence
# May change to level of adherence per individuals instead of proportion 
# of adherence
adh_iso <- 0.9 # Proportion of individuals that adhere to isolation
adh_quaran <- 0.8 # Proportion of individuals that adhere to quarantine

# Initial status 
inf_0 <- 0.1 # Proportion of individuals that are initially infected
rec_0 <- 0.15 # Proportion of individuals that are initially immune (recovered)
vacc <- 0.3  # Proportion of individuals/teachers that are vaccinated
sym_prop <- c(0.2, 0.8) # Proportion of individuals (students, teachers) that are symptomatic


# =========================================================================== #
# Keep track of infected individuals
# =========================================================================== #
# Matrix names for 4D arrays
class_names <- sapply(1:n_class, function(x) paste0("Class",x))
stud_names <- sapply(1:class_size, function(x) paste0("Student",x))
stud_names <- sapply(1:grade_size, function(x) paste0("Student",x))
grade_names <- sapply(1:n_years, function(x) paste0("Grade",x))
day_names <- sapply(1:time_period, function(x) paste0("Day", x))

#`` 4D Array: Disease status of each individual per class, grade, day
dis_stat <- array(0, dim = c(n_class,class_size,n_grades,time_period),
                  dimnames = list(class_names, stud_names, grade_names, day_names))
# 4D Array: Isolation status of each individual per class, grade, day
iso_stat <- array(0, dim = c(n_class,class_size,n_grades,time_period),
                  dimnames = list(class_names, stud_names, grade_names, day_names))
# 4D Array: Quarantine status of each individual per class, grade, day
quaran_stat <- array(0, dim = c(n_class,class_size,n_grades,time_period),
                     dimnames = list(class_names, stud_names, grade_names, day_names))
# 4D Array: Newly infected individuals per class, grade, day
new_inf <- array(0, dim = c(n_class,class_size,n_grades,time_period),
                 dimnames = list(class_names, stud_names, grade_names, day_names))
# 4D Array: Newly symptomatic individuals per class, grade, day
new_symp <- array(0, dim = c(n_class,class_size,n_grades,time_period),
                 dimnames = list(class_names, stud_names, grade_names, day_names))

# 3D array: Total number of infected individuals per class, grade, day
n_infect_day <- array(0, dim = c(n_class, n_grades, time_period),
                      dimnames = list(class_names, grade_names, day_names))
# 3D array: Total number of isolated individuals per class, grade, day
n_isolated_day <- array(0, dim = c(n_class, n_grades, time_period),
                        dimnames = list(class_names, grade_names, day_names))
# 3D array: Total number of quarantined individuals per class, grade, day
n_quarant_day <- array(0, dim = c(n_class, n_grades, time_period),
                        dimnames = list(class_names, grade_names, day_names))
# 3D array: Total number of known infected individuals per class, grade, day
n_known_inf_day <- array(0, dim = c(n_class, n_grades, time_period),
                        dimnames = list(class_names, grade_names, day_names))
# 3D array: Total number of known asymptomatic individuals per class, grade, day
n_known_asymp_day <- array(0, dim = c(n_class, n_grades, time_period),
                           dimnames = list(class_names, grade_names, day_names))
# 3D array: Total number of known asymptomatic individuals per class, grade, day
n_pos_test_day <- array(0, dim = c(n_class, n_grades, time_period),
                        dimnames = list(class_names, grade_names, day_names))
# 3D array: Infected in class, in school (but outside of class), 
#           outside of school but school-linked, outside of school (not linked to school)
inf_source <- array(0, dim = c(n_class,class_size,n_grades),
                    dimnames = list(class_names, stud_names, grade_names))
# 3D array: Assignment of students to classes in each grade 
stud_class_day <- array(0, dim = c(grade_size,n_grades, time_period),
                        dimnames = list(stud_names, grade_names, day_names))

