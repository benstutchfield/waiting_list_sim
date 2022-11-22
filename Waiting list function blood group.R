


waiting_list_gen = function(additional_tbs_for_waiting, 
                            length_of_waiting_list, number_of_donors_per_run, number_of_runs, 
                            additional_patients, simulation_number
                            ){
  
library(tidyverse)
library(rstatix)
library(msm)
library(purrr)
library(ggpubr)
library(sn) 
library(dplyr)
library(hrbrthemes)
  
  
  
#TBS sample parameters
#tbs = rsn (skew normal distribution); xi= centre; omega = spread; alpha = skew
#adjust to create a transplanted population similar to 
# 36 month TBS report = median 1154; IQR= 1006-1302; range = -138 - 1627
#centre= 1270
#spread= 390
#skew= -7
#hidden_mean = 0.5


#additional scoring added to TBS for number of weeks on the list
additional_tbs_for_waiting = additional_tbs_for_waiting


#number of potential recipients and donors
n=100000

#remove x number of patients from waiting list
length_of_waiting_list= length_of_waiting_list

#set to number of transplants per week (1 run = 1 week)
number_of_donors_per_run = number_of_donors_per_run

#Number of new patients added to waiting list each week
additional_patients = additional_patients

#number of weeks on the list = number of runs (a run involves taking x number away and adding another x number); 5 years = 260 weeks
number_of_runs = number_of_runs

#ratio proceding to transplant or not c(proportion not transplanted, proportion transplanted)
ratio_proceed_to_transplant = c(0, 1)


#number of times to repeat simulation

simulation_number = simulation_number


#create table for later and add in some data
total_waiting_list = tibble()
total_waiting_list = total_waiting_list %>% add_column(recip_id = 0, blood_group_recip = "X", 
                                                       weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                       tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                       run_count = 1, sim_count = 0)

total_waiting_list = total_waiting_list %>% add_row(recip_id = 0, blood_group_recip = "X", 
                                                    weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                    tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                    run_count = 1, sim_count = 0)


#create table for later and add in some data
waiting_list_simulation = tibble()
waiting_list_simulation = waiting_list_simulation %>% add_column(recip_id = 0, blood_group_recip = "X", 
                                                                 weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                                 tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                                 run_count = 1, sim_count = 0)

waiting_list_simulation = waiting_list_simulation %>% add_row(recip_id = 0, blood_group_recip = "X", 
                                                              weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                              tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                              run_count = 1, sim_count = 0)


#create table for later and add in some data
transplanted_patient_simulation = tibble()
transplanted_patient_simulation = transplanted_patient_simulation %>% add_column(recip_id = 0, blood_group_recip = "X", 
                                                                                 weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                                                 tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                                                 run_count = 1, sim_count = 0)

transplanted_patient_simulation = transplanted_patient_simulation %>% add_row(recip_id = 0, blood_group_recip = "X", 
                                                                              weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                                              tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                                              run_count = 1, sim_count = 0)

for(i in 1:simulation_number)
  
{
  
  #create tibbles for different patient groups
  transplanted_patients = tibble()
  rejected_donors = tibble()
  
  
  #Potential recipient pool
  recip_id = 1:n
  
  #tbs = rtnorm(n, mean = 1100, sd=400)
 # tbs = rsn(n=n, xi=centre, omega=spread, alpha=skew, tau = hidden_mean)
  
  tbs= rst(n= n, xi= 1410 , omega= -405, alpha= 7, nu=Inf, dp=NULL)
  #hist(tbs)
  #?rsn
  tbs_additional = tbs
  
  #Blood group: Taken from 36 month report: transplanted patients O n=419  A n=442 B n=109  AB=76 
  blood_group_recip = sample(1:4, size = n, replace = TRUE, prob = c(.4006, .4226, .1042, .0727))
  
  
  ######################
  
  
  blood_group_recip = recode(blood_group_recip, "1" = "O", "2" = "A", "3" = "B", "4" = "AB")
  
  weight_recip = rtnorm(n, mean = 80, sd=5,lower= 40, upper= 110 )
  weight_upper_recip = weight_recip + 20
  weight_lower_recip = weight_recip - 20
  
  #adding a column to count number of runs waiting list patients involved in
  number_of_runs_involved = 1
  run_count = 0
  sim_count = 0
  
  recipients = tibble(recip_id, blood_group_recip, weight_recip, weight_upper_recip, 
                      weight_lower_recip, tbs,tbs_additional, number_of_runs_involved, 
                      run_count, sim_count)
  
  #Potential donors
  donor_id = 1:n
  
  #Blood group: Taken from 36 month report: transplanted patients O n=419  A n=442 B n=109  AB=76 
  blood_group_donor = sample(1:4, size = n, replace = TRUE, prob = c(.4006, .4226, .1042, .0727))
  
  blood_group_donor = recode(blood_group_donor, "1" = "O", "2" = "A", "3" = "B", "4" = "AB")
  
  weight_donor = rtnorm(n, mean = 80, sd=10,lower= 40, upper= 110 )
  
  #add column re proceding to transplant or not
  proceed_to_transplant = sample(c(0,1), size= n,replace = TRUE, prob = c(ratio_proceed_to_transplant))
  
  
  ###### if blood group = "O"  select number from this distribution; proceed to transplant sample = 80%; A = 90%; B = 90%; AB= 90%
  
  
  donors = tibble(donor_id, blood_group_donor, weight_donor, proceed_to_transplant)
  
  
  #Waiting list
  #randomly sample 20 rows to create waiting list
  waiting_list = recipients[sample(nrow(recipients), length_of_waiting_list), ]
  
  #to add in run count
  waiting_list = waiting_list %>% add_row(recip_id = 0, blood_group_recip = "X", 
                                          weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                          tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                          run_count = 1, sim_count = 1)
  
  starting_waiting_list = waiting_list
  
  
  #repeat loop of removing patients for transplant and replacing them with new waiting list patients
  for(i in 1:number_of_runs)
  {
    
    #create snap shot of waiting list each run, add a column for number of runs, add a counter 
    # based on blank recipient for number of runs to waiting list
    
    waiting_list_count = waiting_list
    
    waiting_list_count$run_count =    max(total_waiting_list$run_count) + 1
    
    #turn run counts into vector
    waiting_list_count$run_count = as.vector(unlist(waiting_list_count['run_count']))
    
    
    total_waiting_list = bind_rows(total_waiting_list, waiting_list_count)
    total_waiting_list$run_count = as.vector(unlist(total_waiting_list['run_count']))
    
    
    #sequentially remove x number of donors from waiting list
    
    for(i in 1:number_of_donors_per_run)
    {
      #Select next donor
      next_donor = donors[sample(nrow(donors), 1), ]
      
      
      #remove donor from donor pool
      donors = donors[!donors$donor_id %in% next_donor$donor_id, ]   
      
      
      #If donors$proceed_to_transplant = 1   then proceed
      if(next_donor$proceed_to_transplant == 1 ){
        
        #match donor blood group to recipient blood group [TO DO IF X NUMBER THEN PROCEED IF NOT, DO NOTHING]
        blood_group_match = waiting_list[waiting_list$blood_group_recip == next_donor$blood_group_donor,
        ]
        
        #match weight range
        #NEED TO RECORD UNMATCHED DONORS
        weight_match = blood_group_match[blood_group_match$weight_upper_recip >= next_donor$weight_donor &
                                           blood_group_match$weight_lower_recip <= next_donor$weight_donor, ]
        
        #highest score match from waiting list
        recip_select = weight_match %>% slice_max(tbs_additional)
        
        #create new table adding on selected recipients
        transplanted_patients = bind_rows(transplanted_patients, recip_select)
        
        #remove selected recipient from waiting list 
        waiting_list = waiting_list[!waiting_list$recip_id %in% recip_select$recip_id, ]
        
      } else {     rejected_donors = bind_rows(rejected_donors, next_donor)    }    
      #remove recipient from recipient pool
      #recipients = recipients[!recipients$recip_id %in% recip_select$recip_id, ]
      
    }
    
    #Select new patients to add to waiting list
    addition_to_waiting_list = sample(nrow(recipients), additional_patients)
    
    #Add new patients to list
    waiting_list = bind_rows(waiting_list, recipients[ addition_to_waiting_list   , ])
    
    #Remove additions to waiting list from recipient pool
    recipients = recipients[!recipients$recip_id %in% addition_to_waiting_list, ]
    
    #count how many runs patient has been involved in
    waiting_list$number_of_runs_involved = waiting_list$number_of_runs_involved + 1
    
    #add tbs points for length of time on the waiting list
    waiting_list$tbs_additional = waiting_list$tbs_additional + additional_tbs_for_waiting
    
  }

  
  #Add counter for number of simulation runs in total waiting list
  
  simulation_count = total_waiting_list
  simulation_count$sim_count =  max(waiting_list_simulation$sim_count) + 1
  simulation_count$sim_count = as.vector(unlist(simulation_count['sim_count']))
  
  waiting_list_simulation = bind_rows(waiting_list_simulation, simulation_count)
  waiting_list_simulation$sim_count = as.vector(unlist(waiting_list_simulation['sim_count']))
  
  
  #clear total waiting list after each run
  #total_waiting_list = tibble()
  
  total_waiting_list = tibble()
  
  total_waiting_list = total_waiting_list %>% add_column(recip_id = 0, blood_group_recip = "X", 
                                                         weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                         tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                         run_count = 1, sim_count = 0)
  
  total_waiting_list = total_waiting_list %>% add_row(recip_id = 0, blood_group_recip = "X", 
                                                      weight_recip = 0, weight_upper_recip = 0, weight_lower_recip = 0, 
                                                      tbs = -1000, tbs_additional = -1000, number_of_runs_involved = 1, 
                                                      run_count = 1, sim_count = 0)
  
  
  #transplanted patients simulation
  
  transplanted_patient_count = transplanted_patients
  
  transplanted_patient_count$sim_count = max(transplanted_patient_simulation$sim_count) + 1
  transplanted_patient_count$sim_count = as.vector(unlist(transplanted_patient_count['sim_count']))
  
  transplanted_patient_simulation = bind_rows(transplanted_patient_simulation, transplanted_patient_count)
  transplanted_patient_simulation$sim_count = as.vector(unlist(transplanted_patient_simulation['sim_count']))
}


#remove recip id 0 which is used to count number of waiting list runs
total_waiting_list = total_waiting_list[!(total_waiting_list$recip_id == 0),]
waiting_list = waiting_list[!(waiting_list$recip_id == 0),]

starting_waiting_list= starting_waiting_list[!(starting_waiting_list$recip_id == 0),]
transplanted_patient_simulation = transplanted_patient_simulation[!(transplanted_patient_simulation$recip_id == 0),]
waiting_list_simulation = waiting_list_simulation[!(waiting_list_simulation$recip_id == 0),]


transplanted_patient_simulation$sim_count = as.factor(transplanted_patient_simulation$sim_count)
transplanted_patient_simulation$sim_count = as.vector(transplanted_patient_simulation$sim_count)


#sim_output_combined = bind_rows( transplanted_patient_simulation, waiting_list_simulation )

transplanted_and_waiting_lists_combined = list(transplanted_patient_simulation, waiting_list_simulation )

return(transplanted_and_waiting_lists_combined)

}


