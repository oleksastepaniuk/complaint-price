################################################################################
###########             Initial Settings. Downloading data           ###########
################################################################################

# import packages
library(reshape2)

# numbers are printed in a fixed notation, unless they are more than scipen digits wider
options(scipen=50)
# set the number of digits to print when printing numeric values to 3
options(digits=3)
# set thousands deliminator
options(big.mark = ",")

# working directory
setwd("C:/Users/Oleksa/Documents/KSE/Complaints/Calculations/results")

# download data
complaints <- read.csv2("complaints.csv", sep=";")
parameters <- read.csv2("result_paremeters.csv", sep=";")
probability <- read.csv2("result_prob_problem.csv", sep=";")
price_new <- read.csv2("result_price_new.csv", sep=";")

# working directory
setwd("C:/Users/V. Zakaliuzhnyi/Documents/KSE/Complaints/Calculations/modeling")

# download data
lots <- read.csv("modeling.csv", sep=";")
tenders <- read.csv("tenders_value.csv", sep=";")

#####



################################################################################
###########                         Prepare data                     ###########
################################################################################


##### rename columns
####################

colnames(lots) <- c("tender_id", "lot_id", "lot_status", "tender_cpv2", "lot_value",
                    "procedure_type", "participant_number" )

colnames(tenders) <- c("tender_id", "tender_status", "tender_cpv2", "tender_value")

#####

##### remove unnecesary columns and rows
####################

# remove first column with the index that is the result of saving dataframe to csv
complaints <- complaints[,-c(1)]
parameters <- parameters[,-c(1)]
probability <- probability[,-c(1)]
price_new <- price_new[,-c(1)]


# in the probability dataset keep only value of profit and probability of problems
target_rows <- c("profit_perc_ev", "problem_condition_goods", "problem_condition_works",
                 "problem_decision_goods", "problem_decision_works")

probability <- probability[probability$name %in% target_rows,]
rm(target_rows)

#####

##### change format of variables
####################

# complaints dataset
character_columns <- c("tender_id", "complaint_id", "complaint_status", "procedure_type",
                       "accuser_id", "accuser_name", "complaint_type", "tender_cpv2",
                       "tender_status", "procurer_id", "winner_id", "cpv_type")
complaints[character_columns] <- sapply(complaints[character_columns], as.character)
rm(character_columns)

date_columns <- c("tender_date", "complaint_date")
complaints[date_columns] <- lapply(complaints[date_columns], as.Date, format="%Y-%m-%d")
rm(date_columns)


# lots dataset
character_columns <- c("tender_id", "lot_id", "lot_status", "tender_cpv2", "procedure_type")
lots[character_columns] <- sapply(lots[character_columns], as.character)
rm(character_columns)


# parameters dataset
parameters$name <- as.character(parameters$name)


# probability dataset
probability$name <- as.character(probability$name)


# price_new dataset
price_new$name <- as.character(price_new$name)


# tenders dataset
character_columns <- c("tender_id", "tender_status", "tender_cpv2")
tenders[character_columns] <- sapply(tenders[character_columns], as.character)
rm(character_columns)

#####

##### complaints dataset: keep only complaints on tenders announced in 2017
####################

# before 2711, after 1951
complaints <- complaints[complaints$tender_date>="2017-01-01",]

#####

##### lots dataset: keep only lots, where complaints where possible
####################

# remove lots with 0 participants
# before 135628, after 100753
lots <- lots[lots$participant_number!=0,]

# remove lots with 1 participant and procedure type "Відкриті торги" or 
# "Відкриті торги із публікацією англ. мовою"
# before 100753, after 77486
target_procedures <- c("Відкриті торги", "Відкриті торги із публікацією англ. мовою")
lots <- subset(lots, lots$participant_number>1 & lots$procedure_type %in% target_procedures |
                      lots$procedure_type=="Переговорна процедура (для потреб оборони)")

rm(target_procedures)

#####

##### tenders dataset: keep only tenders which are in the complaints or lots dataset
####################

# dataset with all tenders_id from complaints and lots datasets
tenders_keep <- as.data.frame(append(complaints$tender_id, lots$tender_id))
tenders_keep$keep <- "keep"
colnames(tenders_keep) <- c("tender_id", "keep")
tenders_keep$tender_id <- as.character(tenders_keep$tender_id)


# remove duplicated tender_id
# before 79437, after 58522
tenders_keep <- subset(tenders_keep, !duplicated(tenders_keep$tender_id))


# merge tenders and tenders_keep
tenders <- merge(tenders, tenders_keep, by="tender_id", all.x=TRUE)


# keep only tenders with "keep" flag
# before 107512, after 58541
tenders <- subset(tenders, tenders$keep=="keep")


# remove duplicated tender_id
# 58541, after 58522
tenders <- subset(tenders, !duplicated(tenders$tender_id))

tenders$keep <- NULL
rm(tenders_keep)

#####



################################################################################
###########                  Groups of tenders by EV                 ###########
################################################################################

##### create dataset which will store the group thresholds and number of tenders
##### and complaints in each group in terms of goods & services and works
####################

groups_number <- 5
groups_name <- c()

for(i in 1:groups_number){
  groups_name[i] <- paste("group",i,sep="_")
}
rm(i)

groups <- data.frame("name"=groups_name, 
                     "goods_threshold"=rep(NA,groups_number),
                     "goods_tenders"=rep(NA,groups_number),
                     "goods_conditions"=rep(NA,groups_number),
                     "goods_decisions"=rep(NA,groups_number),
                     "works_threshold"=rep(NA,groups_number),
                     "works_tenders"=rep(NA,groups_number),
                     "works_conditions"=rep(NA,groups_number),
                     "works_decisions"=rep(NA,groups_number))
rm(groups_name)

#####

##### tenders dataset: divide tenders on "Goods & services" & "Works"
####################

tenders$cpv_type <- ifelse(tenders$tender_cpv2 == "45000000-7 Будівельні роботи та поточний ремонт",
                              "Works","Goods & services")

#####

##### tenders dataset: determine groups for Goods & services
####################

# selects only procurements of goods & services
# 51478 tenders
tenders_goods <- subset(tenders, tenders$cpv_type=="Goods & services")

# determine thresholds
first_quantile <- 1 / groups_number
next_quantile <- first_quantile

for(i in 1:nrow(groups)) {
  groups[i,"goods_threshold"] <- quantile(tenders_goods$tender_value, next_quantile)
  next_quantile <- next_quantile + first_quantile
}
rm(first_quantile, next_quantile, i)

#####

##### tenders dataset: determine groups for Works
####################

# selects only procurements of goods & services
# 7044 tenders
tenders_works <- subset(tenders, tenders$cpv_type=="Works")

# determine thresholds
first_quantile <- 1 / groups_number
next_quantile <- first_quantile

for(i in 1:nrow(groups)) {
  groups[i,"works_threshold"] <- quantile(tenders_works$tender_value, next_quantile)
  next_quantile <- next_quantile + first_quantile
}
rm(first_quantile, next_quantile, i)

#####

##### groups dataset: determine number of tenders in each group
####################

# for Goods & services
for(i in 1:nrow(groups)) {
  groups[i,"goods_tenders"] <- nrow(subset(tenders_goods, 
                                           tenders_goods$tender_value<=groups[i,"goods_threshold"]))
}
for(i in nrow(groups):2) {
  groups[i,"goods_tenders"] <- groups[i,"goods_tenders"]-groups[i-1,"goods_tenders"]
}
rm(i)


# for Works
for(i in 1:nrow(groups)) {
  groups[i,"works_tenders"] <- nrow(subset(tenders_works, 
                                           tenders_works$tender_value<=groups[i,"works_threshold"]))
}
for(i in nrow(groups):2) {
  groups[i,"works_tenders"] <- groups[i,"works_tenders"]-groups[i-1,"works_tenders"]
}
rm(i)

#####

##### tenders dataset: mark each tender as belonging to one of the groups
####################

# for Goods & services
tenders_goods$tender_group <- NA

for(i in 1:groups_number){
  tenders_goods$tender_group <- ifelse(is.na(tenders_goods$tender_group) & 
                                         tenders_goods$tender_value<=groups[i,"goods_threshold"],
                                       i, tenders_goods$tender_group)
}
rm(i)

# for Works
tenders_works$tender_group <- NA

for(i in 1:groups_number){
  tenders_works$tender_group <- ifelse(is.na(tenders_works$tender_group) & 
                                         tenders_works$tender_value<=groups[i,"works_threshold"],
                                       i, tenders_works$tender_group)
}
rm(i)

#####

##### tenders dataset: unite tenders_goods and tenders_works datasets
####################

tenders <- rbind(tenders_goods, tenders_works)
rm(tenders_goods, tenders_works)

#####

##### complaints & lots: add tender_group labels
####################

tenders_groups <- subset(tenders, select=c("tender_id", "tender_group"))

complaints <- merge(complaints, tenders_groups, by="tender_id", all.x=TRUE)

lots <- merge(lots, tenders_groups, by="tender_id", all.x=TRUE)

rm(tenders_groups)

#####

##### groups: add number of complaints per group
####################

# Conditions; Goods & services
# 580 complaints out of 1804
conditions_goods <- subset(complaints, complaints$complaint_type=="На условия" &
                             complaints$cpv_type=="Goods & services")

for(i in 1:nrow(groups)) {
  groups[i,"goods_conditions"] <- nrow(subset(conditions_goods, 
                                              conditions_goods$tender_group==i))
}

rm(i, conditions_goods)


# Decisions; Goods & services
# 1224 complaints out of 1804
decisions_goods <- subset(complaints, complaints$complaint_type=="На решение" &
                             complaints$cpv_type=="Goods & services")

for(i in 1:nrow(groups)) {
  groups[i,"goods_decisions"] <- nrow(subset(decisions_goods, 
                                              decisions_goods$tender_group==i))
}

rm(i, decisions_goods)
# sum(groups$goods_decisions)


# Conditions; Works
# 34 complaints out of 147 
conditions_works <- subset(complaints, complaints$complaint_type=="На условия" &
                             complaints$cpv_type=="Works")

for(i in 1:nrow(groups)) {
  groups[i,"works_conditions"] <- nrow(subset(conditions_works, 
                                              conditions_works$tender_group==i))
}

rm(i, conditions_works)


# Decisions; Works
# 113 complaints out of 147 
decisions_works <- subset(complaints, complaints$complaint_type=="На решение" &
                             complaints$cpv_type=="Works")

for(i in 1:nrow(groups)) {
  groups[i,"works_decisions"] <- nrow(subset(decisions_works, 
                                             decisions_works$tender_group==i))
}

rm(i, decisions_works)
sum(groups$works_decisions)

#####

##### lots: divide tenders on "Goods & services" & "Works"
####################

lots$cpv_type <- ifelse(lots$tender_cpv2 == "45000000-7 Будівельні роботи та поточний ремонт",
                           "Works","Goods & services")

#####



























