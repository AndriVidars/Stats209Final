

loadData = function(){
  load("dlexpdata.rdata")
  dlexpdata$response <- NA
  dlexpdata$response[dlexpdata$agree == "Approve strongly"] <- 2
  dlexpdata$response[dlexpdata$agree == "Approve somewhat"] <- 1
  dlexpdata$response[dlexpdata$agree == "Neither approve nor disapprove"] <- 0
  dlexpdata$response[dlexpdata$agree == "Disapprove somewhat"] <- -1
  dlexpdata$response[dlexpdata$agree == "Disapprove strongly"] <- -2
  
  dlexp <- filter(dlexpdata, !is.na(response), !is.na(income))
  
  dlexp$female = factor(dlexp$female)
  dlexp$white = factor(dlexp$white)
  
  dlexp$age_bucket <- NULL
  dlexp$age_bucket[dlexp$age < 30] <- "18_29"
  dlexp$age_bucket[dlexp$age %in% 30:39] <- "30_39"
  dlexp$age_bucket[dlexp$age %in% 40:49] <- "40_49"
  dlexp$age_bucket[dlexp$age %in% 50:59] <- "50_59"
  dlexp$age_bucket[dlexp$age %in% 60:69] <- "60_69"
  dlexp$age_bucket[dlexp$age >= 70] <- "70+"
  
  incQuants = quantile(dlexp$income)
  dlexp$incomeQuant <- NULL
  dlexp$incomeQuant[dlexp$income <= incQuants[2]] <- "0-25"
  dlexp$incomeQuant[dlexp$income >  incQuants[2] & dlexp$income <= incQuants[3]] <- "25-50"
  dlexp$incomeQuant[dlexp$income >  incQuants[3] & dlexp$income <= incQuants[4]] <- "50-75"
  dlexp$incomeQuant[dlexp$income >  incQuants[4]] <- "75-100"
  
  # classification from code_supp.R
  dlexp$educ4[dlexp$educ == 1 | dlexp$educ == 2] <- 1 # high school
  dlexp$educ4[dlexp$educ == 3 | dlexp$educ == 4] <- 2 # some college
  dlexp$educ4[dlexp$educ == 5] <- 3 # 4-year college degree
  dlexp$educ4[dlexp$educ == 6] <- 4 # graduate degree
  
  dlexp$educationLevel[dlexp$educ == 1 | dlexp$educ == 2] <- "High School" # high school
  dlexp$educationLevel[dlexp$educ == 3 | dlexp$educ == 4] <- "Some College Experience" 
  dlexp$educationLevel[dlexp$educ == 5] <- "4-Year College Degree" 
  dlexp$educationLevel[dlexp$educ == 6] <- "Graduate Degree"
  
  dlexp$educLevel = as.factor(dlexp$educ4)

  dlexp$pol_party <- NULL
  dlexp$pol_party[dlexp$pid3 == 1] <- "Democrat"
  dlexp$pol_party[dlexp$pid3 == 2] <- "Independent"
  dlexp$pol_party[dlexp$pid3 == 3] <- "Republican"
  
  dlexp$Z = dlexp$video
  dlexp$Y = dlexp$response
  
  return (dlexp)
}

