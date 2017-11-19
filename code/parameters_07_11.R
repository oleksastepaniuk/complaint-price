
################################################################################
###########             Initial Settings. Downloading data           ###########
################################################################################

# import packages
library(reshape2)

# numbers are printed in a fixed notation, unless they are more than scipen digits wider
options(scipen=10)
# set the number of digits to print when printing numeric values to 3
options(digits=1)
# set thousands deliminator
options(big.mark = ",")


# working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/parameters")


# download data
complaints <- read.csv("complaints.csv", sep=";")
lots <- read.csv("lots.csv", sep=";")
winners <- read.csv("winners.csv", sep=";")
tenders <- read.csv("tenders.csv", sep=";")

#####



################################################################################
###########                  Prepare and merge data                  ###########
################################################################################

##### rename columns
####################

colnames(complaints) <- c("tender_date", "tender_id", "complaint_id", "complaint_status",
                          "complaint_date","procedure_type", "accuser_id","accuser_name",
                          "complaint_type", "tender_cpv2", "tender_status", "procurer_id", "tender_value")

colnames(lots) <- c("tender_id", "lot_number")

colnames(winners) <- c("tender_id", "lot_id", "winner_id", "winner_name")

colnames(tenders) <- c("tender_id", "lot_id", "lot_status", "tender_cpv2", "tender_value",
                       "procedure_type", "participant_number")

#####

##### change format of variables
####################

# complaints dataset
character_columns <- c("tender_id", "complaint_id", "complaint_status", "procedure_type",
                       "accuser_id", "accuser_name", "complaint_type", "tender_cpv2",
                       "tender_status", "procurer_id")
complaints[character_columns] <- sapply(complaints[character_columns], as.character)
rm(character_columns)

date_columns <- c("tender_date", "complaint_date")
complaints[date_columns] <- lapply(complaints[date_columns], as.Date, format="%d.%m.%Y")
rm(date_columns)


# lots dataset
lots$tender_id <- as.character(lots$tender_id)
lots$lot_number <- as.numeric(lots$lot_number)


# winners dataset
winners$tender_id <- as.character(winners$tender_id)
winners$lot_id <- as.character(winners$lot_id)
winners$winner_id <- as.character(winners$winner_id)
winners$winner_name <- as.character(winners$winner_name)

# tenders dataset
character_columns <- c("tender_id", "lot_id", "lot_status","tender_cpv2", "procedure_type")
tenders[character_columns] <- sapply(tenders[character_columns], as.character)
rm(character_columns)

#####

##### add number of lots to datasets
####################

complaints <- merge(complaints, lots, by="tender_id", all.x=TRUE)
winners <- merge(winners, lots, by="tender_id", all.x=TRUE)
tenders <- merge(tenders, lots, by="tender_id", all.x=TRUE)

#####

##### merge complaints and winners datasets
####################

# delete tenders with more than one lot in the winners database
# before 93256, after 60952
winners <- subset(winners, winners$lot_number==1)


# add winners of tenders with one lot to the complaints dataset
complaints <- merge(complaints, subset(winners, select=c(tender_id, winner_id)), 
                    by="tender_id", all.x=TRUE)

# new variable: is accuser a winner?
complaints$accuser_winner <- complaints$accuser_id==complaints$winner_id

#####

##### clean tenders dataset
####################

# delete tenders with more than one lot
# before 209477, after 129082
tenders <- subset(tenders, tenders$lot_number==1)


# delete tenders with no participants
# before 129082, after 96726
tenders <- subset(tenders, tenders$participant_number!=0)


# delete tenders with one participant
# if procedure is "Відкриті торги" or "Відкриті торги із публікацією англ. мовою"
# before 96726, after 76981
target_procedure <- c("Відкриті торги", "Відкриті торги", "Відкриті торги із публікацією англ. мовою")
tenders <- subset(tenders, tenders$procedure_type %in% target_procedure & tenders$participant_number>1 |
                    tenders$procedure_type=="Переговорна процедура (для потреб оборони)")
rm(target_procedure)    


# delete tenders with active lot
# before 76981, after 71498
tenders <- subset(tenders, tenders$lot_status!="Активний лот закупівлі")

#####

##### add cpv_type variable to complaints and tenders dataset
####################

complaints$cpv_type <- ifelse(complaints$tender_cpv2 == "45000000-7 Будівельні роботи та поточний ремонт",
                              "Works","Goods & services")

tenders$cpv_type <- ifelse(tenders$tender_cpv2 == "45000000-7 Будівельні роботи та поточний ремонт",
                           "Works","Goods & services")

#####

##### create dataset of complaints in tenders with one lot
####################

# before 2711, after 2190
complaints_one <- subset(complaints, complaints$lot_number == 1)

#####

##### create datasets with parameters and descriptive data
####################

# parameters dataset: variables that characterize existing system
parameters_name <- c("price_goods", "price_works",
                     "prob_winn_condition", "prob_winn_decision",
                     "prob_success_condition", "prob_success_decision")

parameters_value <- c(5000, 15000, NA, NA, NA, NA)

parameters <- data.frame("name"=parameters_name, "value"=parameters_value)
rm(parameters_name, parameters_value)


# prob_problem dataset: probability of problem in conditions/decision depending on
# the assumption about an average profit as % of expected value of lot
prob_problem_name <- c("profit_perc_ev", "multiplier_condition", "multiplier_decision",
                       "threshold_condition_goods", "threshold_condition_works",
                       "threshold_decision_goods", "threshold_decision_works",
                       "complaints",
                       "complaints_condition_goods", "complaints_condition_works", 
                       "complaints_decision_goods", "complaints_decision_works",
                       "tenders", 
                       "tenders_condition_goods", "tenders_condition_works",
                       "tenders_decision_goods", "tenders_decision_works",
                       "total_condition_goods", "total_condition_works",
                       "total_decision_goods", "total_decision_works",
                       "problem_condition_goods", "problem_condition_works",
                       "problem_decision_goods", "problem_decision_works")

prob_problem <- data.frame(name = prob_problem_name)
profit_initial <- 0.01
profit_step <- 0.01
number_of_variants <- 10

for (i in 1:number_of_variants) {
  column_i <- c(profit_initial, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                NA, NA, NA, NA, NA, NA, NA, NA)
  prob_problem[paste("variant_",i,sep="")] <- column_i
  profit_initial <- profit_initial + profit_step
}
rm(prob_problem_name, profit_initial, profit_step, number_of_variants, i, column_i)


# price_new dataset: new price of claim as a % of expected value of lot depending on
# the minimum profit and target probability of successful claim
price_new_name <- c("price_perc", "profit_minimum", "prob_success_minimum")

price_new <- data.frame(name = price_new_name)
prob_success_minimum <- 0.7
profit_minimum_initial <- 0.01
profit_step <- 0.01
number_of_variants <- 5

for (i in 1:number_of_variants) {
  column_i <- c(NA, profit_minimum_initial, prob_success_minimum)
  price_new[paste("variant_",i,sep="")] <- column_i
  profit_minimum_initial <- profit_minimum_initial + profit_step
}
rm(price_new_name, prob_success_minimum, profit_minimum_initial, profit_step,
   number_of_variants, i, column_i)
 

# description dataset
tenders_number <- nrow(subset(complaints, !duplicated(complaints$tender_id)))
tenders_number_one <- nrow(subset(complaints_one, !duplicated(complaints_one$tender_id)))

complaints_total <- nrow(complaints)
complaints_total_one <- nrow(complaints_one)

complaints_condition_goods <- nrow(subset(complaints_one, complaints_one$complaint_type=="На условия" &
                                            complaints_one$cpv_type=="Goods & services"))
complaints_condition_works <- nrow(subset(complaints_one, complaints_one$complaint_type=="На условия" &
                                            complaints_one$cpv_type=="Works"))


complaints_decision_goods <- nrow(subset(complaints_one, complaints_one$complaint_type=="На решение" &
                                           complaints_one$cpv_type=="Goods & services"))
complaints_decision_works <- nrow(subset(complaints_one, complaints_one$complaint_type=="На решение" &
                                           complaints_one$cpv_type=="Works"))


description <- data.frame("name"=c("tenders_number", "tenders_number_one", 
                                   "complaints_total", "complaints_total_one",
                                   "complaints_condition_goods", "complaints_condition_works", 
                                   "complaints_decision_goods", "complaints_decision_works"),
                          "value"=c(tenders_number, tenders_number_one, 
                                    complaints_total, complaints_total_one,
                                    complaints_condition_goods, complaints_condition_works, 
                                    complaints_decision_goods, complaints_decision_works))

description$name <- as.character(description$name)

rm(tenders_number, tenders_number_one, complaints_total, complaints_total_one, 
   complaints_condition_goods, complaints_condition_works, complaints_decision_goods, 
   complaints_decision_works)

#####

##### delete unnecesary datasets
####################

rm(lots, winners)

#####

################################################################################
###########            Probability of winn after success             ###########
################################################################################

##### probability of winning after succsessful complaint on conditions
####################

# 337 successful complaints on conditions in tenders with one lot
condition_success <- nrow(subset(complaints_one, complaints_one$complaint_type == "На условия" &
                                   complaints_one$complaint_status == "Удовлетворено"))

# 16 instances of successful complain on condition and winning
condition_success_win <- nrow(subset(complaints_one, complaints_one$complaint_type == "На условия" &
                                       complaints_one$complaint_status == "Удовлетворено" &
                                       complaints_one$accuser_winner == TRUE))

# 4.75% chance of winning after successful claim on conditions
parameters$value[parameters$name=="prob_winn_condition"] <- condition_success_win / condition_success

rm(condition_success_win)

#####

##### probability of winning after succsessful complaint on decision
####################

# 1026 successful complaints on decision in tenders with one lot
decision_success <- nrow(subset(complaints_one, complaints_one$complaint_type == "На решение" &
                                  complaints_one$complaint_status == "Удовлетворено"))

# 229 instances of successful complain on decision and winning
decision_success_win <- nrow(subset(complaints_one, complaints_one$complaint_type == "На решение" &
                                      complaints_one$complaint_status == "Удовлетворено" &
                                      complaints_one$accuser_winner == TRUE))

# 22.32% chance of winning after successful claim on decision
parameters$value[parameters$name=="prob_winn_decision"] <- decision_success_win / decision_success

rm(decision_success_win)

#####



################################################################################
###########                 Probability of success                   ###########
################################################################################

##### probability of complaint on conditions being successful
####################

# 642 complaints on conditions in tenders with one lot
condition_total <- description$value[description$name=="complaints_condition_goods"] +
  description$value[description$name=="complaints_condition_works"]

# 52.49% chance of complaint on conditions being successful
parameters$value[parameters$name=="prob_success_condition"] <- condition_success / condition_total

rm(condition_success, condition_total)

#####

##### probability of complaint on decision being successful
####################

# 1548 complaints on decision in tenders with one lot
decision_total <- description$value[description$name=="complaints_decision_goods"] +
  description$value[description$name=="complaints_decision_works"]

# 66.28% chance of complaint on conditions being successful
parameters$value[parameters$name=="prob_success_decision"] <- decision_success / decision_total

rm(decision_success, decision_total)

#####



################################################################################
###########                       Thresholds                         ###########
################################################################################

for(i in 2:ncol(prob_problem)){
  
  ##### Multipliers
  ####################
  
  # Conditions claims: profit * prob(success) * prob(winn)
  prob_problem[prob_problem$name=="multiplier_condition",i] <- 
    prob_problem[1,i] * parameters$value[parameters$name=="prob_success_condition"] *
    parameters$value[parameters$name=="prob_winn_condition"]
  
  # Decisions claims: profit * prob(success) * prob(winn)
  prob_problem[prob_problem$name=="multiplier_decision",i] <- 
    prob_problem[1,i] * parameters$value[parameters$name=="prob_success_decision"] *
    parameters$value[parameters$name=="prob_winn_decision"]
  
  
  ##### Thresholds: conditions
  ####################
  
  # case of goods
  prob_problem[prob_problem$name=="threshold_condition_goods",i] <- 
    parameters$value[parameters$name=="price_goods"] / 
    prob_problem[prob_problem$name=="multiplier_condition",i]
  
  # case of works
  prob_problem[prob_problem$name=="threshold_condition_works",i] <- 
    parameters$value[parameters$name=="price_works"] / 
    prob_problem[prob_problem$name=="multiplier_condition",i]

  
  ##### Thresholds: decisions
  ####################
  
  # case of goods
  prob_problem[prob_problem$name=="threshold_decision_goods",i] <- 
    parameters$value[parameters$name=="price_goods"] / 
    prob_problem[prob_problem$name=="multiplier_decision",i]
  
  # case of works
  prob_problem[prob_problem$name=="threshold_decision_works",i] <- 
    parameters$value[parameters$name=="price_works"] / 
    prob_problem[prob_problem$name=="multiplier_decision",i]


}
rm(i)

##### 



################################################################################
###########                 Probability of problem                   ###########
################################################################################


for(i in 2:ncol(prob_problem)){

    ##### mark tenders that fall into boundaries
    ####################

    # indicator of whether tender falls into boundaries
    complaints_one$complaint_boundary <- NA

    # boundary: claim on conditions, goods and services
    complaints_one$complaint_boundary <- ifelse(complaints_one$cpv_type=="Goods & services" & 
                                                complaints_one$complaint_type=="На условия" &
                                                complaints_one$tender_value>=prob_problem[prob_problem$name=="threshold_condition_goods",i],
                                                "condition_goods_yes",
                                                complaints_one$complaint_boundary)

    # boundary: claim on conditions, works
    complaints_one$complaint_boundary <- ifelse(complaints_one$cpv_type=="Works" & 
                                              complaints_one$complaint_type=="На условия" &
                                              complaints_one$tender_value>=prob_problem[prob_problem$name=="threshold_condition_works",i],
                                              "condition_works_yes",
                                              complaints_one$complaint_boundary)                                           

    # boundary: claim on decision, goods and services
    complaints_one$complaint_boundary <- ifelse(complaints_one$cpv_type=="Goods & services" & 
                                              complaints_one$complaint_type=="На решение" &
                                              complaints_one$tender_value>=prob_problem[prob_problem$name=="threshold_decision_goods",i],
                                              "decision_goods_yes",
                                              complaints_one$complaint_boundary)

    # boundary: claim on decision, works
    complaints_one$complaint_boundary <- ifelse(complaints_one$cpv_type=="Works" & 
                                              complaints_one$complaint_type=="На решение" &
                                              complaints_one$tender_value>=prob_problem[prob_problem$name=="threshold_decision_works",i],
                                              "decision_works_yes", complaints_one$complaint_boundary) 

    # boundary: replace NA with "no"
    complaints_one$complaint_boundary <- ifelse(is.na(complaints_one$complaint_boundary),
                                            "no", complaints_one$complaint_boundary)                                          
    #####


    ##### add data about sample that falls into boundaries to description dataset
    ####################

    # number of tenders within boundaries
    complaints_one_yes <- subset(complaints_one, complaint_boundary!="no")

    prob_problem[prob_problem$name=="tenders",i] <- nrow(subset(complaints_one_yes,
                                                    !duplicated(complaints_one_yes$tender_id)))


    # number of complaints within boundaries
    prob_problem[prob_problem$name=="complaints",i] <- nrow(complaints_one_yes)
    rm(complaints_one_yes)


    # number of complaints on conditions within boundaries, goods
    prob_problem[prob_problem$name=="complaints_condition_goods",i] <- nrow(subset(complaints_one, 
                                                      complaint_boundary=="condition_goods_yes"))

    # number of complaints on conditions within boundaries, works
    prob_problem[prob_problem$name=="complaints_condition_works",i] <- nrow(subset(complaints_one, 
                                                complaint_boundary=="condition_works_yes"))

    # number of complaints on decision within boundaries, works
    prob_problem[prob_problem$name=="complaints_decision_goods",i] <- nrow(subset(complaints_one, 
                                                complaint_boundary=="decision_goods_yes"))

    # number of complaints on decision within boundaries, works
    prob_problem[prob_problem$name=="complaints_decision_works",i] <- nrow(subset(complaints_one, 
                                                complaint_boundary=="decision_works_yes"))
    #####



    ##### calculate number of tenders with claims in each threshold group
    ####################

    # pivot table: first colummn - tender_id, other columns - values of complaint_boundary
    thresholds_pivot <- dcast(complaints_one, tender_id ~ complaint_boundary, length)

    # add results to description dataset
    prob_problem[prob_problem$name=="tenders_condition_goods",i] <- nrow(thresholds_pivot[thresholds_pivot$condition_goods_yes!=0,])

    prob_problem[prob_problem$name=="tenders_condition_works",i] <- nrow(thresholds_pivot[thresholds_pivot$condition_works_yes!=0,])

    prob_problem[prob_problem$name=="tenders_decision_goods",i] <- nrow(thresholds_pivot[thresholds_pivot$decision_goods_yes!=0,])

    prob_problem[prob_problem$name=="tenders_decision_works",i] <- nrow(thresholds_pivot[thresholds_pivot$decision_works_yes!=0,])


    # # Summary statistics: number of tenders with particular number of claims
    # # conclusion: number of tenders with more than one claim per type is very small
    # 
    # # max value of claims of one type per tender is 6
    # claims_max <- max(thresholds_pivot[,2:ncol(thresholds_pivot)])
    # 
    # # add rows with fake data to harmonize the value of factor variables
    # for (i in 0:claims_max) {
    #   thresholds_pivot[nrow(thresholds_pivot)+1,1] <- "fake"
    #   thresholds_pivot[nrow(thresholds_pivot),2:ncol(thresholds_pivot)] <- rep(i,ncol(thresholds_pivot)-1)
    # }
    # rm(i)
    # 
    # # change format of all columns except first one to factor
    # for(i in 2:ncol(thresholds_pivot)) {
    #   thresholds_pivot[,i] <- as.factor(thresholds_pivot[,i])
    # }
    # rm(i)
    # 
    # # delete "fake" rows
    # thresholds_pivot <- thresholds_pivot[thresholds_pivot$tender_id!="fake",]
    # 
    # # summary for threshold columns
    # summary(thresholds_pivot[2:ncol(thresholds_pivot)])
    # rm(claims_max)

    rm(thresholds_pivot)
}

#####


##### calculate total number of tenders within boundaries
####################

for(i in 2:ncol(prob_problem)){
  
    prob_problem[prob_problem$name=="total_condition_goods",i] <- nrow(subset(tenders, cpv_type=="Goods & services" & 
                                                  tender_value>=prob_problem[prob_problem$name=="threshold_condition_goods",i]))

    prob_problem[prob_problem$name=="total_condition_works",i] <- nrow(subset(tenders, cpv_type=="Works" & 
                                                  tender_value>=prob_problem[prob_problem$name=="threshold_condition_works",i]))

    prob_problem[prob_problem$name=="total_decision_goods",i] <- nrow(subset(tenders, cpv_type=="Goods & services" & 
                                                  tender_value>=prob_problem[prob_problem$name=="threshold_decision_goods",i]))

    prob_problem[prob_problem$name=="total_decision_works",i] <- nrow(subset(tenders, cpv_type=="Works" & 
                                                  tender_value>=prob_problem[prob_problem$name=="threshold_decision_works",i]))
}
rm(i)

#####


##### calculate probability of the problem
####################

for(i in 2:ncol(prob_problem)){
  
  prob_problem[prob_problem$name=="problem_condition_goods",i] <- 
    prob_problem[prob_problem$name=="tenders_condition_goods",i] /
    prob_problem[prob_problem$name=="total_condition_goods",i]

  prob_problem[prob_problem$name=="problem_condition_works",i] <- 
    prob_problem[prob_problem$name=="tenders_condition_works",i] /
    prob_problem[prob_problem$name=="total_condition_works",i]

  prob_problem[prob_problem$name=="problem_decision_goods",i] <- 
    prob_problem[prob_problem$name=="tenders_decision_goods",i] /
    prob_problem[prob_problem$name=="total_decision_goods",i]

  prob_problem[prob_problem$name=="problem_decision_works",i] <- 
    prob_problem[prob_problem$name=="tenders_decision_works",i] /
    prob_problem[prob_problem$name=="total_decision_works",i]
}
rm(i)

#####



################################################################################
###########                    New value of price                    ###########
################################################################################

for(i in 2:ncol(price_new)){
  
  # price of claim as % of EV with which gain of claim is >=0 if
  # profit as % of ev >= profit_minimum
  # probability of successful claim >= 70%
  # probability of winning >= prob_winn_decision
  price_new[price_new$name=="price_perc",i] <-
    price_new[price_new$name=="profit_minimum",i] *
    price_new[price_new$name=="prob_success_minimum",i] *
    parameters$value[parameters$name=="prob_winn_decision"]
}
rm(i)



################################################################################
###########                    Standart deviation                    ###########
################################################################################

stdv <- data.frame(value=c(0.01, 0.02, 0.03, 0.04, 0.05))




#####


# change working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/results")

# save results
write.csv2(complaints, "complaints.csv")
write.csv2(parameters, "result_paremeters.csv")
write.csv2(price_new, "result_price_new.csv")
write.csv2(prob_problem, "result_prob_problem.csv")
write.csv2(description, "result_description.csv")
write.csv2(stdv, "stdv.csv")

# return working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/parameters")



