# Replication code for "Can Deliberative Minipublics Influence Public Opinion? Theory and Experimental Evidence"
# Goal: replicate the tables and figures in the online supplementary materials
# Dependencies: dlexpdata.Rdata, densities.Rdata

library("boot")
library("xtable")
library("survey")
library("Zelig")
library("ZeligChoice")

load("densities.Rdata")
load("dlexpdata.Rdata")

#-------------------------- Table B1 --------------------------#

dlexpdata$age4 <- NULL
dlexpdata$age4[dlexpdata$age < 32] <- 1
dlexpdata$age4[dlexpdata$age >= 32 & dlexpdata$age < 46] <- 2
dlexpdata$age4[dlexpdata$age >= 46 & dlexpdata$age < 59] <- 3
dlexpdata$age4[dlexpdata$age >= 59] <- 4

dlexpdata$educ4 <- NULL
dlexpdata$educ4[dlexpdata$educ == 1 | dlexpdata$educ == 2] <- 1
dlexpdata$educ4[dlexpdata$educ == 3 | dlexpdata$educ == 4] <- 2
dlexpdata$educ4[dlexpdata$educ == 5] <- 3
dlexpdata$educ4[dlexpdata$educ == 6] <- 4

densities2 <- NULL

densities2[["age4"]] <- prop.table(table(dlexpdata$age4))
densities2[["female"]] <- prop.table(table(dlexpdata$female))
densities2[["educ4"]] <- prop.table(table(dlexpdata$educ4))
densities2[["white"]] <- prop.table(table(dlexpdata$white))
densities2[["hispanic"]] <- prop.table(table(dlexpdata$hispanic))

recdata <- subset(dlexpdata, select = c(age4, female, educ4, white, hispanic))

sdesign.unweighted<- svydesign(ids = ~ 1, data = recdata)

# Table B1

sdesign<-svydesign(id=~1, weights=~psweights.trim, data=dlexpdata)

# Make table for appendix

table1 <- cbind(densities$age4, densities2$age4, prop.table(svytable(~ age4, sdesign)))
rownames(table1) <- c("Less than 32", "32 to 45", "46 to 58", "59 or older")

table2 <- cbind(densities$female, densities2$female, prop.table(svytable(~ female, sdesign)))
rownames(table2) <- c("Male", "Female")

table3 <- cbind(densities$educ4, densities2$educ4, prop.table(svytable(~ educ4, sdesign)))
rownames(table3) <- c("High school or less", "Some college", "4-year college degree", "Postgraduate degree")

table4 <- cbind(densities$white, densities2$white, prop.table(svytable(~ white, sdesign)))
rownames(table4) <- c("White", "Non-white")

table5 <- cbind(densities$hispanic, densities2$hispanic, prop.table(svytable(~ hispanic, sdesign)))
rownames(table5) <- c("Hispanic", "Non-hispanic")

TB1 <- rbind(table1, table2, table3, table4, table5) * 100
colnames(TB1) <- c("CPS Survey \n(California, July 2014)", "Qualtrics Survey (unweighted) \n(California, July 2014)", "Qualtrics Survey (weighted) \n(California, July 2014)")

xtable(TB1, digits = 2)

#-------------------------- Table C1 --------------------------#

dlexpdata$agree.num <- NA
dlexpdata$agree.num[dlexpdata$agree == "Approve strongly"] <- 5
dlexpdata$agree.num[dlexpdata$agree == "Approve somewhat"] <- 4
dlexpdata$agree.num[dlexpdata$agree == "Neither approve nor disapprove"] <- 3
dlexpdata$agree.num[dlexpdata$agree == "Disapprove somewhat"] <- 2
dlexpdata$agree.num[dlexpdata$agree == "Disapprove strongly"] <- 1

## Condition: no partisan cues

dlexpdata$treat1vs2 <- NA
dlexpdata$treat1vs2[dlexpdata$video == 0 & dlexpdata$party.cues == 0] <- 0
dlexpdata$treat1vs2[dlexpdata$video == 1 & dlexpdata$party.cues == 0] <- 1

tplot1 <- prop.table(table(dlexpdata$treat1vs2, dlexpdata$agree.num, useNA = "always"), 1)[1:2,]
tplot1 <- rbind(tplot1, tplot1[2,] - tplot1[1,])
colnames(tplot1) <- rev(c("I'm not sure", "Approve strongly", "Approve somewhat", "Neither approve nor disapprove", "Disapprove somewhat", "Disapprove strongly"))

#--- Obtain confidence intervals using bootstrapping

func.props.boot <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  tplot1 <- prop.table(table(d$treat1vs2, d$agree.num, useNA = "always"), 1)[1:2,]
  tplot1 <- rbind(tplot1, tplot1[2,] - tplot1[1,])
  return(tplot1)
}

set.seed(1000)

props.boot <- boot(data = dlexpdata, statistic= func.props.boot,R=1000, sim = "ordinary", simple = TRUE)

tplot1.ci.low <- matrix(apply(props.boot$t, 2, quantile, p = 0.025), nrow = 3, byrow = F)
tplot1.ci.high <- matrix(apply(props.boot$t, 2, quantile, p = 0.975), nrow = 3, byrow = F)

# Table with standard errors

tplot1.se <- matrix(apply(props.boot$t, 2, sd), nrow = 3, byrow = F)

tplot1.w.se <- t(rbind(tplot1, tplot1.se[3,])) * 100

colnames(tplot1.w.se) <- c("Condition I", "Condition II", "Difference (II-I)", "s.e.")
rownames(tplot1.w.se) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")

## Condition: partisan cues

dlexpdata$treat3vs4 <- NA
dlexpdata$treat3vs4[dlexpdata$video == 0 & dlexpdata$party.cues == 1] <- 0
dlexpdata$treat3vs4[dlexpdata$video == 1 & dlexpdata$party.cues == 1] <- 1

tplot2 <- prop.table(table(dlexpdata$treat3vs4, dlexpdata$agree.num, useNA = "always"), 1)[1:2,]
tplot2 <- rbind(tplot2, tplot2[2,] - tplot2[1,])
colnames(tplot2) <- rev(c("I'm not sure", "Approve strongly", "Approve somewhat", "Neither approve nor disapprove", "Disapprove somewhat", "Disapprove strongly"))

#--- Obtain confidence intervals using bootstrapping

func.props.boot <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  tplot2 <- prop.table(table(d$treat3vs4, d$agree.num, useNA = "always"), 1)[1:2,]
  tplot2 <- rbind(tplot2, tplot2[2,] - tplot2[1,])
  colnames(tplot2) <- rev(c("I'm not sure", "Approve strongly", "Approve somewhat", "Neither approve nor disapprove", "Disapprove somewhat", "Disapprove strongly"))
  return(tplot2)
}

props.boot <- boot(data = dlexpdata, statistic= func.props.boot,R=1000, sim = "ordinary", simple = TRUE)

tplot2.ci.low <- matrix(apply(props.boot$t, 2, quantile, p = 0.025), nrow = 3, byrow = F)
tplot2.ci.high <- matrix(apply(props.boot$t, 2, quantile, p = 0.975), nrow = 3, byrow = F)

# Table with standard errors

tplot2.se <- matrix(apply(props.boot$t, 2, sd), nrow = 3, byrow = F)

tplot2.w.se <- t(rbind(tplot2, tplot2.se[3,])) * 100

colnames(tplot2.w.se) <- c("Condition III", "Condition IV", "Difference (IV-III)", "s.e.")
rownames(tplot2.w.se) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")

TC1.upper <- tplot1.w.se[c(5, 4, 3, 2, 1, 6), ]

TC1.lower <- tplot2.w.se[c(5, 4, 3, 2, 1, 6), ]

# print Table C1

xtable(TC1.upper, digits = 1)
xtable(TC1.lower, digits = 1)

#-------------------------- Table C2 --------------------------#

## Condition: no deliberative cues

dlexpdata$treat1vs3 <- NA
dlexpdata$treat1vs3[dlexpdata$video == 0 & dlexpdata$party.cues == 0] <- 0
dlexpdata$treat1vs3[dlexpdata$video == 0 & dlexpdata$party.cues == 1] <- 1

tplot3 <- prop.table(svytable(~dlexpdata$treat1vs3 + dlexpdata$agree.num, sdesign, exclude=NULL, na.action=na.pass), 1)[1:2,]
tplot3 <- rbind(tplot3, tplot3[2,] - tplot3[1,])
colnames(tplot3) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")

#--- Obtain confidence intervals using bootstrapping

func.props.boot2 <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  tplot3 <- prop.table(table(d$treat1vs3, d$agree.num, useNA = "always"), 1)[1:2,]
  tplot3 <- rbind(tplot3, tplot3[2,] - tplot3[1,])
  return(tplot3)
}

set.seed(2000)

props.boot2 <- boot(data = dlexpdata, statistic= func.props.boot2, R=1000, sim = "ordinary", simple = TRUE, weights = dlexpdata$psweights.trim)

tplot3.ci.low <- matrix(apply(props.boot2$t, 2, quantile, p = 0.025), nrow = 3, byrow = F)
tplot3.ci.high <- matrix(apply(props.boot2$t, 2, quantile, p = 0.975), nrow = 3, byrow = F)

# Table with standard errors

tplot3.se <- matrix(apply(props.boot2$t, 2, sd), nrow = 3, byrow = F)

tplot3.w.se <- t(rbind(tplot3, tplot3.se[3,])) * 100

colnames(tplot3.w.se) <- c("Condition I", "Condition III", "Difference (III-I)", "s.e.")
rownames(tplot3.w.se) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")

TC2 <- tplot3.w.se[c(5, 4, 3, 2, 1, 6), ]

# print Table C2

xtable(TC2, digits = 1)

#-------------------------- Table C3 --------------------------#

tplot4 <- prop.table(svytable(~dlexpdata$treat1vs3 + dlexpdata$agree.num + dlexpdata$pid3, sdesign, exclude=NULL, na.action=na.pass), c(1, 3))[1:2,,1:3]
tplot4.list <- lapply(seq(dim(tplot4)[3]), function(x) tplot4[ , , x])
names(tplot4.list) <- c("dem", "ind", "rep")

for (i in 1:3) {
  tplot4.list[[i]] <- rbind(tplot4.list[[i]], tplot4.list[[i]][2,] - tplot4.list[[i]][1,])
  colnames(tplot4.list[[i]]) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")
}

#--- Obtain confidence intervals using bootstrapping

func.props.boot3 <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  tplot4 <- prop.table(table(d$treat1vs3, d$agree.num, d$pid3, useNA = "always"), c(1, 3))[1:2,,1:3]
  tplot4.list <- lapply(seq(dim(tplot4)[3]), function(x) tplot4[ , , x])
  names(tplot4.list) <- c("dem", "ind", "rep")
  
  for (i in 1:3) {
    tplot4.list[[i]] <- rbind(tplot4.list[[i]], tplot4.list[[i]][2,] - tplot4.list[[i]][1,])
    colnames(tplot4.list[[i]]) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")
  }
  
  return(cbind(tplot4.list[[1]],tplot4.list[[2]],tplot4.list[[3]] ))
}

set.seed(3000)

props.boot3 <- boot(data = dlexpdata, statistic= func.props.boot3,R=1000, sim = "ordinary", simple = TRUE, weights = dlexpdata$psweights.trim)

tplot4.ci.low <- array(apply(props.boot3$t, 2, quantile, p = 0.025), c(3, 6, 3))
tplot4.ci.high <- array(apply(props.boot3$t, 2, quantile, p = 0.975), c(3, 6, 3))

# Table with standard errors

tplot4.se <- array(apply(props.boot3$t, 2, sd), c(3, 6, 3))

tplot4.w.se <- NULL

for (i in 1:3) {
  tplot4.w.se[[i]] <- rbind(tplot4[,,i], tplot4[,,i][2,] - tplot4[,,i][1,])
  tplot4.w.se[[i]] <- t(rbind(tplot4.w.se[[i]], tplot4.se[3,,i])) * 100
  colnames(tplot4.w.se[[i]]) <- c("Condition I", "Condition III", "Difference (III-I)", "s.e.")
  rownames(tplot4.w.se[[i]]) <- c("Disapprove strongly", "Disapprove somewhat", "Neither approve nor disapprove", "Approve somewhat", "Approve strongly", "I'm not sure")
}

TC3.dem <- tplot4.w.se[[1]][c(5, 4, 3, 2, 1, 6),]
TC3.ind <- tplot4.w.se[[2]][c(5, 4, 3, 2, 1, 6),]
TC3.rep <- tplot4.w.se[[3]][c(5, 4, 3, 2, 1, 6),]

xtable(TC3.dem, digits = 1)
xtable(TC3.ind, digits = 1)
xtable(TC3.rep, digits = 1)

#-------------------------- Table C4 --------------------------#

dlexpdata$dem <- ifelse(dlexpdata$pid3 == 1, 1, 0)
dlexpdata$rep <- ifelse(dlexpdata$pid3 == 3, 1, 0)

# OLS

modl.ols <- lm(agree.num ~ video + rep + dem + party.cues + female + age + hispanic  +income + educ + video:party.cues, data = dlexpdata)

coefs.ols <- summary(modl.ols)$coefficients[,1:2]

# ordered logit

modl.ologit <- zelig(as.factor(agree.num) ~ video + rep + dem + party.cues + female + age + hispanic  +income + educ + video:party.cues, model = "ologit", data = dlexpdata)

coefs.ologit <- cbind(modl.ologit$get_coef()[[1]], sqrt(diag(modl.ologit$get_vcov()[[1]]))[1:length(modl.ologit$get_coef()[[1]])])

# make Table C4

Table.coefs <- matrix(c(t(rbind(coefs.ols[-1,], coefs.ologit))), ncol = 2, byrow = F)

rownames.inter <- c(t(matrix(rep(rownames(coefs.ols)[-1], 2), ncol = 2)))

rownames.inter <- paste(rownames.inter, c("coef","s.e."))

rownames(Table.coefs) <- rownames.inter

TC4 <- Table.coefs[c(1:8, 19:20, 9:18), ]

# print Table C4

xtable(TC4, digits = 2)

#-------------------------- Table D1 --------------------------#

data.nocues <- subset(dlexpdata, dlexpdata$video == 0 & dlexpdata$party.cues == 0)

sdesign <- svydesign(id=~1, weights=~psweights.trim, data = data.nocues)

table.all <- prop.table(svytable(~data.nocues$top.pref, sdesign, exclude=NULL, na.action=na.pass))

table.by.pid <- prop.table(svytable(~data.nocues$top.pref + data.nocues$pid3, sdesign, exclude=NULL, na.action=na.pass), 2)

TD1 <- rbind(table.all[c(1, 4, 2, 3)], t(table.by.pid[c(1, 4, 2, 3),1:3])) * 100
colnames(TD1) <- c("2-year Budget", "Unicameral Legislature", "Lower Majority", "Missing")
rownames(TD1) <- c("All respondents", "Democrats", "Independents", "Republicans")

# print Table D1

xtable(TD1, digits = 1)

#-------------------------- Table D2 --------------------------#

sdesign2 <- svydesign(id=~1, weights=~psweights.trim, data=dlexpdata)

t.rank.abs.1vs2 <- prop.table(svytable(~dlexpdata$treat1vs2 + dlexpdata$rank.lowmaj, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,] * 100
t.rank.abs.1vs2 <- t(rbind(t.rank.abs.1vs2, t.rank.abs.1vs2[2,] - t.rank.abs.1vs2[1,]))

t.rank.reluni.1vs2 <- prop.table(svytable(~dlexpdata$treat1vs2 + dlexpdata$pair.lowmaj.unicam, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,c(2, 1, 4, 3)] * 100
t.rank.reluni.1vs2 <- t(rbind(t.rank.reluni.1vs2, t.rank.reluni.1vs2[2,] - t.rank.reluni.1vs2[1,]))

t.rank.rel2y.1vs2 <- prop.table(svytable(~dlexpdata$treat1vs2 + dlexpdata$pair.lowmaj.2ybudg, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,c(3, 2, 1, 4)] * 100
t.rank.rel2y.1vs2 <- t(rbind(t.rank.rel2y.1vs2, t.rank.rel2y.1vs2[2,] - t.rank.rel2y.1vs2[1,]))

t.rank.1vs2 <- rbind(t.rank.abs.1vs2, t.rank.reluni.1vs2, t.rank.rel2y.1vs2)

#--- Obtain confidence intervals using bootstrapping

func.props.boot.1vs2 <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  
  t.rank.abs.1vs2 <- prop.table(table(d$treat1vs2, d$rank.lowmaj), 1)[1:2,] * 100
  t.rank.abs.1vs2 <- t(rbind(t.rank.abs.1vs2, t.rank.abs.1vs2[2,] - t.rank.abs.1vs2[1,]))
  
  t.rank.reluni.1vs2 <- prop.table(table(d$treat1vs2, d$pair.lowmaj.unicam), 1)[1:2,c(2, 1, 4, 3)] * 100
  t.rank.reluni.1vs2 <- t(rbind(t.rank.reluni.1vs2, t.rank.reluni.1vs2[2,] - t.rank.reluni.1vs2[1,]))
  
  t.rank.rel2y.1vs2 <- prop.table(table(d$treat1vs2, d$pair.lowmaj.2ybudg), 1)[1:2,c(3, 2, 1, 4)] * 100
  t.rank.rel2y.1vs2 <- t(rbind(t.rank.rel2y.1vs2, t.rank.rel2y.1vs2[2,] - t.rank.rel2y.1vs2[1,]))
  
  t.rank.1vs2 <- rbind(t.rank.abs.1vs2, t.rank.reluni.1vs2, t.rank.rel2y.1vs2)
  
  return(t.rank.1vs2)
}

set.seed(4000)

props.boot.1vs2 <- boot(data = dlexpdata, statistic= func.props.boot.1vs2,R=1000, sim = "ordinary", simple = TRUE, weights = dlexpdata$psweights.trim)

t.rank.1vs2.se <- matrix(apply(props.boot.1vs2$t, 2, sd), nrow = 12, byrow = F)

TD2 <- cbind(t.rank.1vs2, t.rank.1vs2.se[,3]) 

colnames(TD2) <- c("Condition I", "Condition II", "Difference (II-I)", "s.e.")
rownames(TD2) <- c("First", "Second", "Third", "Missing", "Rel. Uni.: First", "Rel. Uni.: Indifferent", "Rel. Uni.: Second", "Rel. Uni.: Missing", "Rel. 2-Yr. Bgt.: First", "Rel. 2-Yr. Bgt.: Indifferent", "Rel. 2-Yr. Bgt.: Second", "Rel. 2-Yr. Bgt.: Missing")

# print Table D2

xtable(TD2, digits = 1)

#-------------------------- Table D3 --------------------------#

set.seed(5000)

props.boot.1vs2.noweights <- boot(data = dlexpdata, statistic= func.props.boot.1vs2,R=1000, sim = "ordinary", simple = TRUE)

t.rank.1vs2.noweights <- props.boot.1vs2.noweights$t0

t.rank.1vs2.noweights.se <- matrix(apply(props.boot.1vs2.noweights$t, 2, sd), nrow = 12, byrow = F)

TD3 <- cbind(t.rank.1vs2.noweights, t.rank.1vs2.noweights.se[,3]) 

colnames(TD3) <- c("Condition I", "Condition II", "Difference (II-I)", "s.e.")
rownames(TD3) <- c("First", "Second", "Third", "Missing", "Rel. Uni.: First", "Rel. Uni.: Indifferent", "Rel. Uni.: Second", "Rel. Uni.: Missing", "Rel. 2-Yr. Bgt.: First", "Rel. 2-Yr. Bgt.: Indifferent", "Rel. 2-Yr. Bgt.: Second", "Rel. 2-Yr. Bgt.: Missing")

# print Table D3

xtable(TD3, digits = 1)

#-------------------------- Table D4 --------------------------#

t.rank.abs.3vs4 <- prop.table(svytable(~dlexpdata$treat3vs4 + dlexpdata$rank.lowmaj, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,] * 100
t.rank.abs.3vs4 <- t(rbind(t.rank.abs.3vs4, t.rank.abs.3vs4[2,] - t.rank.abs.3vs4[1,]))

t.rank.reluni.3vs4 <- prop.table(svytable(~dlexpdata$treat3vs4 + dlexpdata$pair.lowmaj.unicam, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,c(2, 1, 4, 3)] * 100
t.rank.reluni.3vs4 <- t(rbind(t.rank.reluni.3vs4, t.rank.reluni.3vs4[2,] - t.rank.reluni.3vs4[1,]))

t.rank.rel2y.3vs4 <- prop.table(svytable(~dlexpdata$treat3vs4 + dlexpdata$pair.lowmaj.2ybudg, sdesign2, exclude=NULL, na.action=na.pass), 1)[1:2,c(3, 2, 1, 4)] * 100
t.rank.rel2y.3vs4 <- t(rbind(t.rank.rel2y.3vs4, t.rank.rel2y.3vs4[2,] - t.rank.rel2y.3vs4[1,]))

t.rank.3vs4 <- rbind(t.rank.abs.3vs4, t.rank.reluni.3vs4, t.rank.rel2y.3vs4)

#--- Obtain confidence intervals using bootstrapping

func.props.boot.3vs4 <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  
  t.rank.abs.3vs4 <- prop.table(table(d$treat3vs4, d$rank.lowmaj), 1)[1:2,] * 100
  t.rank.abs.3vs4 <- t(rbind(t.rank.abs.3vs4, t.rank.abs.3vs4[2,] - t.rank.abs.3vs4[1,]))
  
  t.rank.reluni.3vs4 <- prop.table(table(d$treat3vs4, d$pair.lowmaj.unicam), 1)[1:2,c(2, 1, 4, 3)] * 100
  t.rank.reluni.3vs4 <- t(rbind(t.rank.reluni.3vs4, t.rank.reluni.3vs4[2,] - t.rank.reluni.3vs4[1,]))
  
  t.rank.rel2y.3vs4 <- prop.table(table(d$treat3vs4, d$pair.lowmaj.2ybudg), 1)[1:2,c(3, 2, 1, 4)] * 100
  t.rank.rel2y.3vs4 <- t(rbind(t.rank.rel2y.3vs4, t.rank.rel2y.3vs4[2,] - t.rank.rel2y.3vs4[1,]))
  
  t.rank.3vs4 <- rbind(t.rank.abs.3vs4, t.rank.reluni.3vs4, t.rank.rel2y.3vs4)
  
  return(t.rank.3vs4)
}

set.seed(6000)

props.boot.3vs4 <- boot(data = dlexpdata, statistic= func.props.boot.3vs4,R=1000, sim = "ordinary", simple = TRUE, weights = dlexpdata$psweights.trim)

t.rank.3vs4.se <- matrix(apply(props.boot.3vs4$t, 2, sd), nrow = 12, byrow = F)

TD4 <- cbind(t.rank.3vs4, t.rank.3vs4.se[,3]) 

colnames(TD4) <- c("Condition I", "Condition II", "Difference (II-I)", "s.e.")
rownames(TD4) <- c("First", "Second", "Third", "Missing", "Rel. Uni.: First", "Rel. Uni.: Indifferent", "Rel. Uni.: Second", "Rel. Uni.: Missing", "Rel. 2-Yr. Bgt.: First", "Rel. 2-Yr. Bgt.: Indifferent", "Rel. 2-Yr. Bgt.: Second", "Rel. 2-Yr. Bgt.: Missing")

# print Table D4

xtable(TD4, digits = 1)

#-------------------------- Table D5 --------------------------#

set.seed(7000)

props.boot.3vs4.noweights <- boot(data = dlexpdata, statistic = func.props.boot.3vs4, R=1000, sim = "ordinary", simple = TRUE)

t.rank.3vs4.noweights <- props.boot.3vs4.noweights$t0

t.rank.3vs4.noweights.se <- matrix(apply(props.boot.3vs4.noweights$t, 2, sd), nrow = 12, byrow = F)

TD5 <- cbind(t.rank.3vs4.noweights, t.rank.3vs4.noweights.se[,3]) 

colnames(TD5) <- c("Condition I", "Condition II", "Difference (II-I)", "s.e.")
rownames(TD5) <- c("First", "Second", "Third", "Missing", "Rel. Uni.: First", "Rel. Uni.: Indifferent", "Rel. Uni.: Second", "Rel. Uni.: Missing", "Rel. 2-Yr. Bgt.: First", "Rel. 2-Yr. Bgt.: Indifferent", "Rel. 2-Yr. Bgt.: Second", "Rel. 2-Yr. Bgt.: Missing")

# print Table D5

xtable(TD5, digits = 1)

#-------------------------- Table D6 --------------------------#

dlexpdata$top.choice <- ifelse(dlexpdata$rank.lowmaj == 1, 1, 0)

# OLS

modl.ols.top <- lm(top.choice ~ video + rep + dem + party.cues + female + age + hispanic  +income + educ + video:party.cues, data = dlexpdata)

coefs.ols.top <- summary(modl.ols.top)$coefficients[,1:2]

# logit

modl.logit.top <- zelig(top.choice ~ video + rep + dem + party.cues + female + age + hispanic  +income + educ + video:party.cues, model = "logit", data = dlexpdata)

coefs.logit.top <- cbind(modl.logit.top$get_coef()[[1]], sqrt(diag(modl.logit.top$get_vcov()[[1]]))[1:length(modl.logit.top$get_coef()[[1]])])

# make Table D6

Table.coefs.top <- matrix(c(t(rbind(coefs.ols.top[-1,], coefs.logit.top[-1,]))), ncol = 2, byrow = F)

rownames.model <- c(t(matrix(rep(rownames(coefs.logit.top)[-1], 2), ncol = 2)))

rownames.model <- paste(rownames.model, c("coef","s.e."))

rownames(Table.coefs.top) <- rownames.model

TD6 <- Table.coefs.top[c(1:8, 19:20, 9:18), ]

# print Table D6

xtable(TD6, digits = 2)

#-------------------------- Table D7 --------------------------#

# OLS

modl.ols.top2 <- lm(top.choice ~ video + rep + dem + video:rep + video:dem + party.cues:rep + party.cues:dem + party.cues + female + age + hispanic  +income + educ + video:party.cues + video:party.cues:dem + video:party.cues:rep, data = dlexpdata)

coefs.ols.top2 <- summary(modl.ols.top2)$coefficients[,1:2]

# logit

modl.logit.top2 <- zelig(top.choice ~ video + rep + dem + video:rep + video:dem + party.cues:rep + party.cues:dem + party.cues + female + age + hispanic  +income + educ + video:party.cues + video:party.cues:dem + video:party.cues:rep, model = "logit", data = dlexpdata)

coefs.logit.top2 <- cbind(modl.logit.top2$get_coef()[[1]], sqrt(diag(modl.logit.top2$get_vcov()[[1]]))[1:length(modl.logit.top2$get_coef()[[1]])])

# make Table D7

Table.coefs.top2 <- matrix(c(t(rbind(coefs.ols.top2[-1,], coefs.logit.top2[-1,]))), ncol = 2, byrow = F)

rownames.model2 <- c(t(matrix(rep(rownames(coefs.logit.top2)[-1], 2), ncol = 2)))

rownames.model2 <- paste(rownames.model2, c("coef","s.e."))

rownames(Table.coefs.top2) <- rownames.model2

TD7 <- Table.coefs.top2[c(1:2, 19:22,3:8,27:28,23:26,31:32,29:30,9:18), ]

# print Table D7

xtable(TD7, digits = 2)

#-------------------------- Table D8 --------------------------#

set.seed(8000)

effects.by.pid3 <- list(NULL, NULL)

for (j in 1:2) { for (i in 1:3) {
  
  if (i == 1) {d <- 1; r <- 0} else{ if (i == 2) {d <- 0; r <- 0} else {d <- 0; r <- 1}}
  
  x.low <- setx(modl.logit.top2, video = 0, party.cues = (j - 1), rep = r, dem = d, female = 1, hispanic = 0, fn = list(numeric = mean))
  x.high <- setx(modl.logit.top2, video = 1, party.cues = (j - 1), rep = r, dem = d, female = 1, hispanic = 0, fn = list(numeric = mean))
  s.out <- sim(modl.logit.top2, x = x.low, x1 = x.high)
  
  change.top.mean <- apply(s.out$`.->sim.out`$x1$fd[[1]], 2, mean)
  change.top.se <- apply(s.out$`.->sim.out`$x1$fd[[1]], 2, sd)
  
  effects.by.pid3[[j]] <- rbind(effects.by.pid3[[j]], c(change.top.mean, change.top.se) * 100)
  
}}

TD8 <- cbind(effects.by.pid3[[1]], effects.by.pid3[[2]])
rownames(TD8) <- c("Democrat", "Independent", "Republican")

# print Table D8

xtable(TD8, digits = 1)



