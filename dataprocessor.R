setwd("/Users/liuj/icd9notconverted_everythingelseconverted")
hours <- c(1,2,3,6,12,18,24,48,72,96)
for(hour in hours)
{
  load(paste("temporal.extract.14.3.1.first.",hour,".hours.RData", sep=""))
  cohort[is.na(cohort)] <- -1
  
  catn1 = factor(cohort$ethnicity, labels=(1:length(levels(factor(cohort$ethnicity)))))
  cohort$ethnicity <- catn1
  
  gen2 = factor(cohort$gender, labels=(1:length(levels(factor(cohort$gender)))))
  cohort$gender <- gen2
  
  #cohort$icd9code = as.numeric(cohort$icd9code)
  
  save(cohort, file=paste("temporal.extract.14.3.2.first.",hour,".hours.rdata", sep= ""))
  write.csv(cohort, file = paste("temporal.extract.14.3.2.first.",hour,".hours.csv", sep = ""))
  
  rm(cohort, catn1, gen2, icd2)
}

#test0= cohort[cohort$icd9code=="1"
#              |cohort$icd9code=="2"
#              |cohort$icd9code=="5",]

#test1 <- subset(cohort, icd9code < 2000)




