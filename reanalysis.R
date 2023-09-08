library(stringr)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

#opportunityPriorBlank_experiment1Seen_table <- read_csv("~/opportunityPriorBlank experiment1Seen-table300.csv", skip = 6) # current
#opportunityPriorBlank_experiment1Seen_table <- read_csv("~/opportunityPriorBlank experiment2100Seen-table.csv", skip = 6)
#opportunityPriorBlank_experiment1Seen_table <- read_csv("~/opportunityPriorBlank experiment1Seen25-table.csv", skip = 6) # current
opportunityPriorBlank_experiment1Seen_table <- read_csv("~/opportunityPriorBlank experiment1Seenp1515-table.csv", skip = 6) # current


df <- opportunityPriorBlank_experiment1Seen_table
df <- df %>% rename(step = `[step]`)
df <- df %>% rename(runNumber = `[run number]`)
df <- select(df, -contains("shirt-color"))
df <- subset(df, `vision-range` > 21)
dlong <- df %>% pivot_longer(cols=`[who] of thief 8`:`[seen] of person 207`, names_to = "feature", values_to = "value") # make long
dlong$agent <- str_extract_all(dlong$feature, "[0-9.]+")  # extract the agent id so we can widen per agent number
dlong$feature <- sub("\\].*", "]", dlong$feature)
dwide <- dlong %>% pivot_wider(names_from=feature, values_from=value)

dwide %>% group_by(runNumber)

dwide <- dwide %>% 
  group_by(runNumber) %>%
  mutate(
    `location-crimescene-x` = ifelse(`location-crimescene-x`==0, max(`location-crimescene-x`[`location-crimescene-x`!=0]), `location-crimescene-x`),
    `location-crimescene-y` = ifelse(`location-crimescene-y`==0, max(`location-crimescene-y`[`location-crimescene-y`!=0]), `location-crimescene-y`),
    crimetime = ifelse(crimetime==0, max(crimetime[crimetime!=0]), crimetime),
    getsmalln = ifelse(getsmalln==0, max(getsmalln[getsmalln!=0]), getsmalln))
dwide <- dwide %>% rename(csX = `location-crimescene-x`)
dwide <- dwide %>% rename(csY = `location-crimescene-y`)
dwide <- dwide %>% rename(cT = crimetime)
dwide$cT <- dwide$cT +1

dwide$dX = sqrt((dwide$csX-dwide$`[xcor]`)^2)
dwide$dY = sqrt((dwide$csY-dwide$`[ycor]`)^2)
dwide$dT = dwide$cT-dwide$step
dwide$radiusFromCS = sqrt((dwide$dX)^2 + (dwide$dY)^2)



calculate_opportunity_prior <- function(ag_id, dwideSub){
  
  x <- subset(dwideSub, agent==ag_id)
  x$radiusThief <- sqrt((x$dX)^2 + (x$dY)^2)
  x <- select(x, runNumber, step, radiusThief)
  dwideSub <- dwideSub %>% left_join(x, by=c("runNumber", "step"))
  dwideSub$inRadius <- ifelse(dwideSub$radiusFromCS <= dwideSub$radiusThief + 1, TRUE, FALSE)   # the patch resolution is 1, we do not know the distance precisely
  
  # evidence
  x <- subset(dwideSub, agent==ag_id)
  seenEv <- subset(x, step == cT)
  seenEv$`[seen]` <- ifelse(seenEv$`[seen]` == 0, -1, 1)
  seenEv$`[seen]` <- ifelse(seenEv$radiusFromCS < 1 & seenEv$`[seen]` != -1, 1, seenEv$`[seen]`)
  seenEv$`[seen]` <- ifelse(seenEv$radiusFromCS >= 1 & seenEv$`[seen]` != -1, 0, seenEv$`[seen]`)
  seenEv <- rename(seenEv, runNumber=runNumber, Eseen=`[seen]`)
  seenEv <- select(seenEv, runNumber, Eseen)
  
  
  
  x <- subset(dwideSub, agent==ag_id)
  seenObs <- subset(x, `[seen]` == 1)
  r_best <- seenObs %>% slice_min(n=1, abs(dT))
  r_best <- select(r_best, runNumber, agent, step, dT, dX, dY) # the best observation for a run occurred at (step), which is dT removed from CT, and the thief was dX and DY away from CS
  r_best <- rename(r_best, runNumber=runNumber, agent=agent, best_step=step, best_dtime=dT)
  
  best <- dwideSub%>% left_join(r_best, by=c("runNumber"))
  relevant_agents <- subset(best, best$cT-abs(best$best_dtime) <= best$step & best$step <= best$cT+abs(best$best_dtime)) # these are all relevant agents in the time-slice we're considering
  
  
  n <- relevant_agents %>%
    group_by(runNumber, agent.x) %>%
    summarise(
      sum(inRadius),
      total = ifelse(sum(inRadius) > 0, 1, 0))
  
  relSeen <- subset(relevant_agents, `[seen]` == 1)
  nSeen <- relSeen %>%
    group_by(runNumber, agent.x) %>%
    summarise(
      sum(inRadius),
      totalSeen = ifelse(sum(inRadius) > 0, 1, 0))
  
  oppPriorSeen <- nSeen %>% group_by(runNumber) %>% summarise(nSeen = sum(totalSeen))
  oppPrior <- n %>% group_by(runNumber) %>% summarise(n = sum(total))
  oppPriorSeenEV <- seenEv %>% group_by(runNumber) %>% summarise(seenEv= seenEv)
  
  
  oppPrior$seen <- oppPriorSeen$nSeen
  oppPrior$agent <- ag_id
  oppPrior$Eseen <- oppPriorSeenEV$seenEv
  
  
  return(as.data.frame(oppPrior))
}

dwideFull <- dwide

# number of runs:
RUN_NUM <- length(unique(dwideFull$runNumber))
n <- RUN_NUM
datalist = vector("list", length = n*3+1)

start_idx = 0 #1500 #0, 900



# shirt color proporsion
#table(dwideFull$`[shirt-color]`)
#7891/nrow(dwideFull)


for (run in seq(1 + start_idx, n+start_idx, 1)){
  dwideSub <- subset(dwideFull, runNumber == run)
  
  
  # select the actual thief
  ag_id <- 8
  xOp = calculate_opportunity_prior(ag_id, dwideSub)
  datalist[[run]] <- xOp
  
  # select a random innocent agent with the same profile
  ag_id <- 9
  xOp = calculate_opportunity_prior(ag_id, dwideSub)
  datalist[[run+(RUN_NUM)]] <- xOp

  # select an innocent agent with the that was at some point close to the crime scene `[shirt-color]` == 0
  y <- subset(dwideSub, agent > 9 & agent != 11)
  ystar <- y %>% slice_min(n=1, radiusFromCS) %>% filter(row_number()==1)
  ag_id <- unname(unlist(ystar$agent))
  xOp = calculate_opportunity_prior(ag_id, dwideSub)
  datalist[[run+(2*RUN_NUM)]] <- xOp
}

big_data = do.call(rbind, datalist)
big_data$type <- "guilty"
big_data$type <- ifelse(big_data$agent == 9, "innocentRandom", big_data$type)
big_data$type <- ifelse(big_data$agent != 9 & big_data$agent != 8, "innocentClose", big_data$type)

View(big_data)

 # add rows in the data for where we do not have complete observations
sequence_df <- big_data %>%
  group_by(type) %>%
  complete(runNumber = full_seq(1:RUN_NUM, 1)) %>%
  ungroup() %>%
  arrange(type, runNumber)

sequence_df <- sequence_df %>%
  mutate(n = ifelse(is.na(n), 200, n),
         seen = ifelse(is.na(seen), 200, seen),
         agent = ifelse(type == "guilty", 8, agent),
         agent = ifelse(type == "innocentRandom", 9, agent),
          agent = ifelse(is.na(agent), 0, agent))

# final completed data
completedBigData <- sequence_df




# calculate the evidence

#seenThief <- subset(dwideFull, agent==8)
#seenThief <- subset(dwideFull, step == cT)
#Eseen <- select(seenThief, runNumber, `[seen]`)

final_state <- dwideFull %>% group_by(runNumber) %>% slice(n())

View(final_state)

final_state <- final_state %>% select(runNumber, `wallet-owner`, getsmalln, getidsA, heterdaad)
final_state$walletOwner <- str_extract_all(final_state$`wallet-owner`, "\\d+")
final_state$smalln <- final_state$getsmalln
final_state$idsA <- final_state$getidsA

ev <- completedBigData %>% left_join(final_state, by=c("runNumber"))
ev$ownerSuspect <- ifelse(ev$agent == ev$walletOwner, TRUE, FALSE)
ev$guiltSuspect <- ifelse(ev$type == "guilty", "guilty", "innocent")
ev$heterdaad <- ifelse(ev$type != "guilty", 0, ev$heterdaad)

d <- ev %>% group_by(guiltSuspect) %>% 
  summarize(prob_match = mean(ownerSuspect))
d # the final probability for wallet matching

#guiltSuspect prob_match
#    <chr>             <dbl>
#  1 guilty             0.7 
#  2 innocent           0.05

#

#ev <- ev %>% left_join(Eseen, by=c("runNumber"))
#ev <- ev %>% rename(Eseen = `[seen]`)
#ev$Eseen <- as.integer(as.logical(ev$Eseen))

ev$E <- as.integer(as.logical(ev$ownerSuspect))
ev$totalAgents <- 200
ev$guilty <- 1/ev$n
ev$opp <- ev$n/ev$totalAgents

ev$guiltySeen <- 1/ev$seen
ev$oppSeen <- ev$seen/ev$totalAgents

ev$guiltySmalln <- 1/ev$smalln
ev$oppSmalln <- ev$smalln/ev$n

ev$guiltySeenSmalln <- 1/ev$smalln
ev$oppSeenSmalln <- ev$smalln/ev$seen


ev <- subset(ev, select = -c(`wallet-owner`))
ev$walletOwner <- as.character(ev$walletOwner)

test <- select(df, c("runNumber", "vision-range"))
t <-setNames(aggregate(test$`vision-range`, list(test$runNumber), FUN=mean), c("runNumber", "vision-range"))

ev <- t %>% left_join(ev, by=c("runNumber"))

write.csv(ev, "paramsNewp1515.csv", row.names=TRUE)
write.csv(ev, "paramsNewDraft3.csv", row.names=TRUE)


### plos ####
library(ggplot2)
library(tidyverse)



meltEV <- ev %>% pivot_longer(cols=n:seen, names_to = "NTYPE", values_to = "numSeen") # make long
ggplot(meltEV, aes(x=runNumber, y=numSeen)) + geom_point(aes(color=NTYPE)) # plot different distribution of num seen - we often do not have an image of thief



ev$differenceSeen <- ev$n - ev$seen # we expect this to be always positive...
ggplot(ev, aes(x=runNumber, y=differenceSeen)) + geom_point() #difference is always positive, seems to be correct.
ggplot(ev, aes(x=runNumber, y=n)) + geom_point(aes()) # plot different distribution of num seen - we often do not have an image of thief

View(ev)
