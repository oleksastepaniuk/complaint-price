# import packages
library(reshape2)


################################################################################
#####              Datasets that will store average results                #####
################################################################################

# create empty dataframe
sim_avg_new <- matrix(ncol = groups_number+2, nrow = 4)
sim_avg_new <- data.frame(sim_avg_new)

# Rename columns
colnames(sim_avg_new)[1] <- "profit_assumption"
for(i in 2:(groups_number+1)){
  colnames(sim_avg_new)[i] <- paste("group",i-1,sep="_")
}
rm(i)
colnames(sim_avg_new)[groups_number+2] <- "payments_sum"

# name the first four rows
sim_avg_new$profit_assumption[1:4] <- c("real_cond_goods", "real_cond_works", 
                                        "real_dec_goods", "real_dec_works")

# save empty dataset sim_avg_new for further use
sim_avg_new_empty <- sim_avg_new

# insert real data for comparison
sim_avg_new[sim_avg_new$profit_assumption=="real_cond_goods",2:(ncol(sim_avg_new)-1)] <- groups$goods_conditions
sim_avg_new[sim_avg_new$profit_assumption=="real_cond_works",2:(ncol(sim_avg_new)-1)] <- groups$works_conditions
sim_avg_new[sim_avg_new$profit_assumption=="real_dec_goods",2:(ncol(sim_avg_new)-1)] <- groups$goods_decisions
sim_avg_new[sim_avg_new$profit_assumption=="real_dec_works",2:(ncol(sim_avg_new)-1)] <- groups$works_decisions

#####



################################################################################
#####           Run simmulation over all profit assumptions                #####
################################################################################

# choose profit assumption
# Variant 10: average profit = 10% of EV, probability of problems are calculated
# with this assumption
prof_assump <- 11

for(price_variant in 2:ncol(price_new)) {
  
  
  ################################################################################
  ##   Datasets that will store results of iterations per profit assumption     ##
  ################################################################################
  
  ##### Conditions; Goods & services
  ####################
  
  # Create empty dataframe
  sim_cond_goods <- matrix(ncol = groups_number+2, nrow = 0)
  sim_cond_goods <- data.frame(sim_cond_goods)
  
  # Rename columns
  colnames(sim_cond_goods)[1] <- "iteration_number"
  for(i in 2:(groups_number+1)){
    colnames(sim_cond_goods)[i] <- paste("group",i-1,sep="_")
  }
  rm(i)
  colnames(sim_cond_goods)[groups_number+2] <- "payments_sum"
  
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
  iterations_number <- 100
  
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
    
    lots$condition_payment <- ifelse(lots$condition_problem==1,
                                     lots$lot_value*price_new[price_new$name=="price_perc",price_variant],0)
    
    
    lots$condition_profit <- ifelse(lots$condition_problem==1,
                                    lots$condition_profit - lots$condition_payment, 0)
    
    
    
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
    
    lots$decision_payment <- ifelse(lots$decision_problem==1,
                                    lots$lot_value*price_new[price_new$name=="price_perc",price_variant],0)
    
    
    lots$decision_profit <- ifelse(lots$decision_problem==1,
                                   lots$decision_profit - lots$decision_payment, 0)    
    
    
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
    sim_cond_goods[iteration,2:(ncol(sim_cond_goods)-1)] <- if(nrow(lots_goods[lots_goods$condition_claim==1,])!=0){
      dcast(lots_goods, tender_group ~ condition_claim, length)[,"1"]
    } else {rep(0,ncol(sim_cond_goods)-2)}
    sim_cond_goods[iteration,ncol(sim_cond_goods)] <- sum(lots_goods$condition_payment*lots_goods$condition_claim)
    
    # Conditions; Works
    sim_cond_works[iteration,"iteration_number"] <- iteration
    sim_cond_works[iteration,2:(ncol(sim_cond_works)-1)] <- if(nrow(lots_works[lots_works$condition_claim==1,])!=0){
      dcast(lots_works, tender_group ~ condition_claim, length)[,"1"]
    } else {rep(0,ncol(sim_cond_works)-2)}
    sim_cond_works[iteration,ncol(sim_cond_works)] <- sum(lots_works$condition_payment*lots_works$condition_claim)
    
    # Decisions; Goods & services
    sim_dec_goods[iteration,"iteration_number"] <- iteration
    sim_dec_goods[iteration,2:(ncol(sim_dec_goods)-1)] <- if(nrow(lots_goods[lots_goods$decision_claim==1,])!=0){
      dcast(lots_goods, tender_group ~ decision_claim, length)[,"1"]
    } else {rep(0,ncol(sim_dec_goods)-2)}
    sim_dec_goods[iteration,ncol(sim_dec_goods)] <- sum(lots_goods$decision_payment*lots_goods$decision_claim)
    
    # Decisions; Works
    sim_dec_works[iteration,"iteration_number"] <- iteration
    sim_dec_works[iteration,2:(ncol(sim_dec_works)-1)] <- if(nrow(lots_works[lots_works$decision_claim==1,])!=0){
      dcast(lots_works, tender_group ~ decision_claim, length)[,"1"]
    } else {rep(0,ncol(sim_dec_works)-2)}
    sim_dec_works[iteration,ncol(sim_dec_works)] <- sum(lots_works$decision_payment*lots_works$decision_claim)
    
    rm(lots_goods, lots_works)
    
  }
  
  rm(iteration)
  
  ################################################################################
  ###########           Average of iterations of iteration             ###########
  ################################################################################
  
  # create dataframe using sim_avg_new_empty template
  sim_interm <- sim_avg_new_empty
  
  # rename rows
  sim_interm$profit_assumption[1] <- paste(round(price_new[price_new$name=="price_perc",price_variant]*100,2),"cond_goods", sep="_")
  sim_interm$profit_assumption[2] <- paste(round(price_new[price_new$name=="price_perc",price_variant]*100,2),"cond_works", sep="_")
  sim_interm$profit_assumption[3] <- paste(round(price_new[price_new$name=="price_perc",price_variant]*100,2),"dec_goods", sep="_")
  sim_interm$profit_assumption[4] <- paste(round(price_new[price_new$name=="price_perc",price_variant]*100,2),"dec_works", sep="_")
  
  # calculate and store average results
  sim_interm[1,2:ncol(sim_avg_new)] <- round(colMeans(sim_cond_goods[,2:ncol(sim_cond_goods)]),0)
  sim_interm[2,2:ncol(sim_avg_new)] <- round(colMeans(sim_cond_works[,2:ncol(sim_cond_works)]),0)
  sim_interm[3,2:ncol(sim_avg_new)] <- round(colMeans(sim_dec_goods[,2:ncol(sim_dec_goods)]),0)
  sim_interm[4,2:ncol(sim_avg_new)] <- round(colMeans(sim_dec_works[,2:ncol(sim_dec_works)]),0)
  
  # add results to the database of results
  sim_avg_new <- rbind(sim_avg_new, sim_interm)
  
}

rm(prof_assump)
rm(price_variant)
rm(sim_avg_new_empty)
rm(sim_interm)
rm(sim_cond_goods, sim_cond_works, sim_dec_goods, sim_dec_works)

################################################################################
###########           Average of iterations of iteration             ###########
################################################################################


# change working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/results")

# save results
write.csv2(sim_avg_new, "result_sim_avg_new.csv")

# return working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/modeling")





