library(tidyverse)
library(rstatix)
library(msm)
library(purrr)
library(ggpubr)
library(sn) 
library(dplyr)

#check



#Dataset creation for waiting list with x number of points per week

#to change number of points per simulation chnage number in functions below 
#to change parameters of simulation change numbers below

#length_of_waiting_list = waiting list at the start; number_of_donors_per_run = number of donors selected each week
#number_of_runs = number of weeks to run simulation (5 years = 260); additional_patients = number of additional patients to add each week;
#simulation_number = number of times to run whole simulation
length_of_waiting_list = 500
number_of_donors_per_run = 20
number_of_runs = 520
additional_patients = 20
simulation_number = 20



points_0= waiting_list_gen(additional_tbs_for_waiting= 0,  length_of_waiting_list = length_of_waiting_list, number_of_donors_per_run = number_of_donors_per_run,
                           number_of_runs = number_of_runs, additional_patients= additional_patients, simulation_number = simulation_number)


# This creates a combined file of waiting list and transplanted patients "transplanted_and_waiting_lists_combined"
# Need to separate into transplanted patient and waiting list patients

#This separates into the 2 tables
transplanted_patients_0_points= points_0[[1]]
waiting_list_0_points= points_0[[2]]


#add column for number of points per week
waiting_list_0_points$additional_points = "0 additional points"
transplanted_patients_0_points$additional_points = "0 additional points"



#########################

points_10= waiting_list_gen(additional_tbs_for_waiting= 10,  length_of_waiting_list = length_of_waiting_list, number_of_donors_per_run = number_of_donors_per_run,
                            number_of_runs = number_of_runs, additional_patients= additional_patients, simulation_number = simulation_number)

# This creates a combined file of waiting list and transplanted patients "transplanted_and_waiting_lists_combined"
# Need to separate into transplanted patient and waiting list patients

#This separates into the 2 tables
transplanted_patients_10_points= points_10[[1]]
waiting_list_10_points= points_10[[2]]

#add column for number of points per week
waiting_list_10_points$additional_points = "10 additional points"
transplanted_patients_10_points$additional_points = "10 additional points"



#########################

points_20= waiting_list_gen(additional_tbs_for_waiting= 20,  length_of_waiting_list = length_of_waiting_list, number_of_donors_per_run = number_of_donors_per_run,
                            number_of_runs = number_of_runs, additional_patients= additional_patients, simulation_number = simulation_number)


# This creates a combined file of waiting list and transplanted patients "transplanted_and_waiting_lists_combined"
# Need to separate into transplanted patient and waiting list patients

#This separates into the 2 tables
transplanted_patients_20_points= points_20[[1]]
waiting_list_20_points= points_20[[2]]

#add column for number of points per week
waiting_list_20_points$additional_points = "20 additional points"
transplanted_patients_20_points$additional_points = "20 additional points"



###########################

#Add the tables together
waiting_time_table = bind_rows(waiting_list_0_points, waiting_list_10_points, waiting_list_20_points)

waiting_time_table$sim_count = as.factor(waiting_time_table$sim_count)
waiting_time_table$additional_points = as.factor(waiting_time_table$additional_points)


transplanted_patient_table = bind_rows(transplanted_patients_0_points, transplanted_patients_10_points, 
                                       transplanted_patients_20_points)


transplanted_patient_table$sim_count = as.factor(transplanted_patient_table$sim_count)
transplanted_patient_table$additional_points = as.factor(transplanted_patient_table$additional_points)




#save table
#write.csv(waiting_time_table, "waiting_time_table_10yr.csv")
#write.csv(transplanted_patient_table, "transplanted_patient_table_10yr.csv")




##################
