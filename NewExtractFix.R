## Impute continuous variables 
## Saves three versions, one all variables imputed, one none, and one < 25% missing

library(Hmisc)
hours <- c(1,2,3,6,12,18,24,48,72,96)

for(hour in hours)
{
  load(paste("temporal.extract.14.3.first.",hour,".hours.RData", sep=""))
  colnames(cohort) <- gsub(" \\.", "\\.", colnames(cohort))
  
  cohort <- cohort[!is.na(cohort$category),]
  index <- grep("category", colnames(cohort))
  cohort <- cohort[,c((1:ncol(cohort))[-index], index)]
  
  impute <- colnames(cohort)[colSums(is.na(cohort)) != 0 & 
      sapply(cohort, is.numeric) & !grepl("elix\\.index", colnames(cohort))]
  f <- paste('`', impute, '`', sep="")
  f <- paste(f, collapse = " + ")
  f <- as.formula(paste("~", f))
  
  g <- aregImpute(f, data=cohort, n.impute=1, nk=4)
  imputed <- cohort
  trans <- impute.transcan(g, imputation=1, data=cohort, list.out=TRUE)
  imputed[names(trans)] <- trans
  
  full <- cohort
  full[is.na(full)] <- '?'
  
  partial <- cohort
  index <- which(colSums(is.na(partial)) < dim(partial)[1] / 4)
  partial[index] <- imputed[index]
  partial[is.na(partial)] <- '?'
  
  write.csv(full, file = paste("temporal.extract.14.3.first.",hour,".hours.all.discretized.csv", sep = ""))
  write.csv(partial, file = paste("temporal.extract.14.3.first.",hour,".hours.partial.discretized.csv", sep = ""))
  write.csv(imputed, file = paste("temporal.extract.14.3.first.",hour,".hours.none.discretized.csv", sep = ""))
}
