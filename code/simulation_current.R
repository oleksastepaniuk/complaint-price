

################################################################################
#####              Datasets that will store average results                #####
################################################################################

# create empty dataframe
sim_average <- matrix(ncol = groups_number+1, nrow = 4)
sim_average <- data.frame(sim_average)

# Rename columns
colnames(sim_average)[1] <- "profit_assumption"
for(i in 2:(groups_number+1)){
  colnames(sim_average)[i] <- paste("group",i-1,sep="_")
}
rm(i)


# name the first four rows
sim_average$profit_assumption[1:4] <- c("real_cond_goods", "real_cond_works", 
                                   "real_dec_goods", "real_dec_works")

# save empty dataset sim_average for further use
sim_average_empty <- sim_average

# insert real data for comparison
sim_average[sim_average$profit_assumption=="real_cond_goods",2:ncol(sim_average)] <- groups$goods_conditions
sim_average[sim_average$profit_assumption=="real_cond_works",2:ncol(sim_average)] <- groups$works_conditions
sim_average[sim_average$profit_assumption=="real_dec_goods",2:ncol(sim_average)] <- groups$goods_decisions
sim_average[sim_average$profit_assumption=="real_dec_works",2:ncol(sim_average)] <- groups$works_decisions

#####


################################################################################
#####           Run simmulation over all profit assumptions                #####
################################################################################

for(prof_assump in 2:ncol(probability)) {




  ################################################################################
  ##   Datasets that will store results of iterations per profit assumption     ##
  ################################################################################

  ##### Conditions; Goods & services
  ####################

  # Create empty dataframe
  sim_cond_goods <- matrix(ncol = groups_number+1, nrow = 0)
  sim_cond_goods <- data.frame(sim_cond_goods)

  # Rename columns
  colnames(sim_cond_goods)[1] <- "iteration_number"
  for(i in 2:(groups_number+1)){
    colnames(sim_cond_goods)[i] <- paste("group",i-1,sep="_")
  }
  rm(i)

  ##### 

  ##### Dataframes for other claims
  ####################

  # Conditions; Works
  sim_cond_works <- sim_cond_goods
  
  # Decisions; Goods & services
  sim_dec_goods <- sim_cond_goods

  # Decisions; Works
  sim_dec_works <- sim_cond_goods

  ##### 


  ################################################################################
  ###########                      Generate claims                     ###########
  ################################################################################

  # set number of iterations
  iterations_number <- 50

  for(iteration in 1:iterations_number) {

    ##### generate claims on conditions
    ####################

    # generate random variable that will determine whether there is a problem in conditions
    lots$condition_rand <- runif(nrow(lots))
  

    # check if there is a problem in conditions
    lots$condition_problem <- ifelse(lots$cpv_type=="Goods & services" &
                                   lots$condition_rand<=probability[probability$name=="problem_condition_goods",prof_assump],1,
                                 ifelse(lots$cpv_type=="Works" &
                                          lots$condition_rand<=probability[probability$name=="problem_condition_works",prof_assump],1,0))


    # determine if it is profitable to complain on conditions
    lots$condition_profit <- ifelse(lots$condition_problem==1,
                                lots$lot_value*
                                  probability[probability$name=="profit_perc_ev",prof_assump]*
                                  parameters$value[parameters$name=="prob_success_condition"]*
                                  parameters$value[parameters$name=="prob_winn_condition"], 0)
                                
    lots$condition_profit <- ifelse(lots$condition_problem==1,
                                ifelse(lots$cpv_type=="Goods & services",
                                       lots$condition_profit - parameters$value[parameters$name=="price_goods"],
                                       lots$condition_profit - parameters$value[parameters$name=="price_works"]), 0)
                                
                                
    # mark claim on condition if expected profit is positive
    lots$condition_claim <- ifelse(lots$condition_profit>0,1,0)

    ##### 
  
    ##### generate claims on decisions
    ####################

    # generate random variable that will determine whether there is a problem in decisions
    lots$decision_rand <- runif(nrow(lots))


    # check if there is a problem in decisions
    lots$decision_problem <- ifelse(lots$cpv_type=="Goods & services" &
                                   lots$decision_rand<=probability[probability$name=="problem_decision_goods",prof_assump],1,
                                 ifelse(lots$cpv_type=="Works" &
                                          lots$decision_rand<=probability[probability$name=="problem_decision_works",prof_assump],1,0))


    # determine if it is profitable to complain on decisions
    lots$decision_profit <- ifelse(lots$decision_problem==1,
                                lots$lot_value*
                                  probability[probability$name=="profit_perc_ev",prof_assump]*
                                  parameters$value[parameters$name=="prob_success_decision"]*
                                  parameters$value[parameters$name=="prob_winn_decision"], 0)

    lots$decision_profit <- ifelse(lots$decision_problem==1,
                                ifelse(lots$cpv_type=="Goods & services",
                                       lots$decision_profit - parameters$value[parameters$name=="price_goods"],
                                       lots$decision_profit - parameters$value[parameters$name=="price_works"]), 0)


    # mark claim on decision if expected profit is positive
    lots$decision_claim <- ifelse(lots$decision_profit>0,1,0)

    #####




    ################################################################################
    ###########                Store results of iteration                ###########
    ################################################################################

    # create subsets
    lots_goods <- subset(lots, lots$cpv_type=="Goods & services")
    lots_works <- subset(lots, lots$cpv_type=="Works")


    # Conditions; Goods & services
    sim_cond_goods[iteration,"iteration_number"] <- iteration
    sim_cond_goods[iteration,2:ncol(sim_cond_goods)] <- dcast(lots_goods, tender_group ~ condition_claim, length)[,"1"]


    # Conditions; Works
    sim_cond_works[iteration,"iteration_number"] <- iteration
    sim_cond_works[iteration,2:ncol(sim_cond_works)] <- dcast(lots_works, tender_group ~ condition_claim, length)[,"1"]


    # Decisions; Goods & services
    sim_dec_goods[iteration,"iteration_number"] <- iteration
    sim_dec_goods[iteration,2:ncol(sim_dec_goods)] <- dcast(lots_goods, tender_group ~ decision_claim, length)[,"1"]


    # Decisions; Works
    sim_dec_works[iteration,"iteration_number"] <- iteration
    sim_dec_works[iteration,2:ncol(sim_dec_works)] <- dcast(lots_works, tender_group ~ decision_claim, length)[,"1"]

    rm(lots_goods, lots_works)

    }

  rm(iteration)

  ################################################################################
  ###########           Average of iterations of iteration             ###########
  ################################################################################

  # create dataframe using sim_average_empty template
  sim_interm <- sim_average_empty

  # rename rows
  sim_interm$profit_assumption[1] <- paste(probability[probability$name=="profit_perc_ev",prof_assump],"cond_goods", sep="_")
  sim_interm$profit_assumption[2] <- paste(probability[probability$name=="profit_perc_ev",prof_assump],"cond_works", sep="_")
  sim_interm$profit_assumption[3] <- paste(probability[probability$name=="profit_perc_ev",prof_assump],"dec_goods", sep="_")
  sim_interm$profit_assumption[4] <- paste(probability[probability$name=="profit_perc_ev",prof_assump],"dec_works", sep="_")
  
  # calculate and store average results
  sim_interm[1,2:ncol(sim_average)] <- round(colMeans(sim_cond_goods[,2:ncol(sim_cond_goods)]),0)
  sim_interm[2,2:ncol(sim_average)] <- round(colMeans(sim_cond_works[,2:ncol(sim_cond_works)]),0)
  sim_interm[3,2:ncol(sim_average)] <- round(colMeans(sim_dec_goods[,2:ncol(sim_dec_goods)]),0)
  sim_interm[4,2:ncol(sim_average)] <- round(colMeans(sim_dec_works[,2:ncol(sim_dec_works)]),0)

  # add results to the database of results
  sim_average <- rbind(sim_average, sim_interm)

}

rm(prof_assump)
rm(sim_average_empty)
rm(sim_interm)
rm(sim_cond_goods, sim_cond_works, sim_dec_goods, sim_dec_works)

################################################################################
###########           Average of iterations of iteration             ###########
################################################################################


# change working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/results")

# save results
write.csv2(sim_average, "result_sim_average.csv")
write.csv2(groups, "result_groups.csv")

# return working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/modeling")



