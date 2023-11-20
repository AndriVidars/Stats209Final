

loadData = function(){
  load("dlexpdata.rdata")
  dlexpdata$response <- NA
  dlexpdata$response[dlexpdata$agree == "Approve strongly"] <- 2
  dlexpdata$response[dlexpdata$agree == "Approve somewhat"] <- 1
  dlexpdata$response[dlexpdata$agree == "Neither approve nor disapprove"] <- 0
  dlexpdata$response[dlexpdata$agree == "Disapprove somewhat"] <- -1
  dlexpdata$response[dlexpdata$agree == "Disapprove strongly"] <- -2
  
  dlexp <- filter(dlexpdata, !is.na(response))
  
  dlexp$female = factor(dlexp$female)
  dlexp$white = factor(dlexp$white)
  
  dlexp$age_bucket <- NULL
  dlexp$age_bucket[dlexp$age < 30] <- "18_29"
  dlexp$age_bucket[dlexp$age %in% 30:39] <- "30_39"
  dlexp$age_bucket[dlexp$age %in% 40:49] <- "40_49"
  dlexp$age_bucket[dlexp$age %in% 50:59] <- "50_59"
  dlexp$age_bucket[dlexp$age %in% 60:69] <- "60_69"
  dlexp$age_bucket[dlexp$age >= 70] <- "70+"
  
  dlexp$income_bucket <- NULL
  dlexp$income_bucket[dlexp$income <= 5] <- "0_5"
  dlexp$income_bucket[dlexp$income %in% 6:10] <- "6_10"
  dlexp$income_bucket[dlexp$income %in% 11:15] <- "11_15"
  dlexp$income_bucket[dlexp$income > 15] <- "16+"
  
  # classification from code_supp.R
  dlexp$educ4[dlexp$educ == 1 | dlexp$educ == 2] <- 1 # high school
  dlexp$educ4[dlexp$educ == 3 | dlexp$educ == 4] <- 2 # some college
  dlexp$educ4[dlexp$educ == 5] <- 3 # 4-year college degree
  dlexp$educ4[dlexp$educ == 6] <- 4 # graduate degree
  
  return (dlexp)
}