#Copyright: Script to extract meds, labs, and CPT codes from MIMIC2 for predictive analytics 

#Temporal Phenome Extractor: Script to extract meds, labs, and CPT codes from MIMIC2 for predictive analytics

#     Copyright (C) 2014  Jeremy L. Warner M.D., M.S.

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#Note: This is a script file and is not functionalized.  Individual sections are delineated by headers.
#      Please contact the author with any comments or clarifications: jeremy.warner@vanderbilt.edu
#      Attribution is appreciated.
#
#Current Version: 14.3
#Version Date: January 9, 2015
#History of Changes: Go to end of script for history of changes.
#
#############################
#KEY PARAMETERS IN THE SCRIPT
#include.maybes -> flag to include the possible complications (default = NO)
#c.match -> determines the planned ratio of controls to cases (default 1:1)
#interval -> the time period over which data is collected (default is 24 hours)
#med.param -> the medication features to retain (default is top 10%)
#cpt.param -> the procedure features to retain (default is top 10%)
#lab.param -> the lab features to retain (default are those measured in 50% of cohort) 
#############################

v = 14.3

setwd("~/Dropbox/PRIMES/PRIMES Shared")

#RPostgreSQL library for SQL database interface with MIMIC2
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")

#Create the connection. Password is in clear text; you must have MIMIC2 access.
conn <- dbConnect(drv, dbname="MIMIC2", user="mimic2", password="2CIMIM_2v6")

###Add some lines to the log file with a header
cat(paste('Temporal Phenome Extraction, Version ', v),
    file='log.txt',
    sep='\n',
    append=TRUE)
cat(paste('Directory under which extraction occurs:', getwd()),
    file='log.txt',
    sep='\n',
    append=TRUE)

##################
#1 Prepare dataset
##################

###Add a line to the log file with a timestamp at the beginning of the algorithm
cat(paste('Cohort feature collection beginning at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

#1 Run algorithm to determine length of hospital stay for all adult patients

#1.1 All adult admissions with admit and discharge dates
all.hadm <- dbGetQuery(conn, "SELECT distinct admissions.subject_id as subject_id, hadm_id, admit_dt, disch_dt 
	FROM mimic2v26.admissions
	WHERE admissions.subject_id in 
		(SELECT subject_id
			FROM mimic2v26.icustay_detail
			WHERE icustay_age_group = 'adult')")

###Add a line to the log file with total distinct admissions
cat(paste('Total distinct adult admissions:', dim(all.hadm)[1], 'admissions.'),
    file='log.txt',
    sep='\n',
    append=TRUE)

hadm <- paste(all.hadm$hadm_id, ',', sep='', collapse=' ')
hadm <- substr(hadm, start = 1, stop = nchar(hadm) - 1)

#1.1.1 All adult admissions with admission and discharge dates and at least one POE order
all.hadm.poe <- dbGetQuery(conn, "
	SELECT hadm_id, min(enter_dt) as first_poe, max(stop_dt) as last_poe 
	FROM mimic2v26.admissions JOIN mimic2v26.poe_order using(hadm_id)
	WHERE admissions.subject_id in 
		(SELECT subject_id
			FROM mimic2v26.icustay_detail
			WHERE icustay_age_group = 'adult')
	GROUP BY admissions.subject_id, hadm_id, admit_dt, disch_dt")

###Add a line to the log file with total admissions with at least one POE order
cat(paste('Total distinct adult admissions with at least one POE order:', dim(all.hadm.poe)[1], 'admissions.'),
    file='log.txt',
    sep='\n',
    append=TRUE)

#1.1.2 All adult admissions with admission and discharge dates and at least one medication event
if (FALSE) {
  #Broken code
all.hadm.meds <- dbGetQuery(conn, "
  SELECT hadm_id, min(charttime) as first_med, max(charttime) as last_med 
	FROM mimic2v26.admissions JOIN mimic2v26.medevents using(hadm_id)
	WHERE admissions.subject_id in 
		(SELECT subject_id
			FROM mimic2v26.icustay_detail
			WHERE icustay_age_group = 'adult')
	GROUP BY admissions.subject_id, hadm_id, admit_dt, disch_dt")

###Add a line to the log file with total admissions with at least one medication event
cat(paste('Total distinct adult admissions with at least one medication event:', dim(all.hadm.meds)[1], 'admissions.'),
    file='log.txt',
    sep='\n',
    append=TRUE)
}

#1.1.2 All adult admissions with at least one lab order within t -48 hours of admission 
all.hadm.lab <- dbGetQuery(conn, paste("
	SELECT admissions.hadm_id as hadm_id, min(charttime) as first_lab
	FROM mimic2v26.labevents JOIN mimic2v26.admissions using(subject_id) 
	WHERE admissions.hadm_id in (", hadm, ") AND
		charttime - admit_dt > interval '-48 hours'
	GROUP BY admissions.hadm_id")) 

###Add a line to the log file with total distinct admissions with a least one lab order
cat(paste('Total distinct adult admissions with at least one lab order:', dim(all.hadm.lab)[1], 'admissions.'),
    file='log.txt',
    sep='\n',
    append=TRUE)

#1.1.3 All adult discharges from the ICU 
all.hadm.icu <- dbGetQuery(conn, "SELECT subject_id, hadm_id, icustay_outtime as icu_out
	FROM mimic2v26.icustay_detail
	WHERE icustay_age_group = 'adult'") 

#1.2 The following code determines the first event for each hadm_id
all.hadm <- merge(all.hadm, all.hadm.lab, by='hadm_id')
all.hadm <- merge(all.hadm, all.hadm.poe, by='hadm_id', all.x = TRUE)

rm(all.hadm.lab, all.hadm.poe)

all.hadm$first.event <- as.POSIXct(NA)

all.hadm$first.event[difftime(all.hadm$first_lab, all.hadm$admit_dt, units = 'h') <= 24] <- 
	all.hadm$first_lab[difftime(all.hadm$first_lab, all.hadm$admit_dt, units = 'h') <= 24]

foo <- which(is.na(all.hadm$first.event))

#1.2.1 Some hadm have enter dates more than 24 hours after admit_dt; fall back to admit_dt/disch_dt
all.hadm$first.event[foo] <- all.hadm$first_poe[foo]
all.hadm$first.event[foo][difftime(all.hadm$first.event[foo], all.hadm$admit_dt[foo], units = 'h') > 24] <- NA

all.hadm$first.event[is.na(all.hadm$first.event)] <- all.hadm$admit_dt[is.na(all.hadm$first.event)]

#1.2.2 For patients who went from hospital to ICU and then discharged in the same day, use ICU discharge as out
#and first POE order as in. 
all.temp <- merge(all.hadm.icu, all.hadm, by='subject_id', all.x = TRUE)
all.temp$diff <- difftime(all.temp$icu_out, all.temp$disch_dt, units = 'days')
all.temp <- all.temp[which(all.temp$diff >= 0),]
all.temp <- all.temp[which(all.temp$diff <= 1),]
all.temp$diff <- difftime(all.temp$icu_out, all.temp$first.event, units = 'hours')
all.temp <- all.temp[order(all.temp$hadm_id.y),]

#1.3 The following code determines the last event for each hadm_id
all.hadm$last.event <- as.POSIXct(NA)

#1.3.1 Adds the times to the all.hadm table
all.hadm$last.event[which(all.hadm$hadm_id %in% all.temp$hadm_id.y)] = all.temp$icu_out

#1.3.2 Some hadm have stop dates less than 24 hours after disch_dt
foo <- which(is.na(all.hadm$last.event))

#1.3.3 Some hadm have enter dates more than 24 hours after admit_dt; fall back to admit_dt/disch_dt 
all.hadm$last.event[foo] <- all.hadm$last_poe[foo]
all.hadm$last.event[foo][difftime(all.hadm$last.event[foo], all.hadm$disch_dt[foo], units = 'h') > 24] <- NA

#1.3.4 Some have labs less than 24 hours after disch_dt
hadm <- all.hadm$hadm_id[is.na(all.hadm$last.event)]
hadm <- paste(hadm, ',', sep='', collapse=' ')
hadm <- substr(hadm, start = 1, stop = nchar(hadm) - 1)

last.lab <- dbGetQuery(conn, paste("
	SELECT admissions.hadm_id as hadm_id, max(charttime) as last_lab
	FROM mimic2v26.labevents JOIN mimic2v26.admissions using(subject_id) 
	WHERE admissions.hadm_id in (", hadm, ") AND
	charttime - disch_dt <= interval '24 hours' AND
	charttime - disch_dt > interval '0 hours'
	GROUP BY admissions.hadm_id")) 

last.lab <- last.lab[order(last.lab$hadm_id),]

all.hadm$last.event[which(all.hadm$hadm_id %in% last.lab$hadm_id)] = last.lab$last_lab

#1.3.5 Some will just default to the disch_dt
all.hadm$last.event[is.na(all.hadm$last.event)] <- all.hadm$disch_dt[is.na(all.hadm$last.event)]

#1.4 Calculate the difference, in hours, between discharge and admit times
all.hadm$diff <- difftime(all.hadm$last.event,all.hadm$first.event, units = 'hours')
all.hadm$diff <- as.numeric(all.hadm$diff)

#1.4.1 If the difference is a negative number, truncate to 0 hours
all.hadm$diff[all.hadm$diff < 0] <- 0

#1.5 Add demographics
subject <- unique(all.hadm$subject_id)
subject <- paste(subject, ',', sep='', collapse=' ')
subject <- substr(subject, start = 1, stop = nchar(subject) - 1)

#1.5.1 Get gender, age at time of admission, and date of death
all.demo <- dbGetQuery(conn, paste("
	SELECT subject_id, max(gender) as gender, min(icustay_admit_age) as age, min(dod) as dod
	FROM mimic2v26.icustay_detail
	WHERE subject_id in (", subject, ")
	GROUP BY subject_id"))

#1.5.2 Get ethnicity
all.eth <- dbGetQuery(conn, paste("
	SELECT subject_id, label as ethnicity
	FROM mimic2v26.d_demographicitems 
		JOIN mimic2v26.demographicevents using(itemid)
	WHERE category = 'ETHNICITY' AND
	subject_id in (", subject, ")"))

#1.5.3 Add gender, age, ethnicity to all.hadm
all.eth <- all.eth[!(duplicated(all.eth$subject_id)),]
all.demo <- merge(all.demo, all.eth, by = 'subject_id')
all.hadm <- merge(all.hadm, all.demo[,c('subject_id','gender','age','ethnicity','dod')], by = 'subject_id', all.x = TRUE)

#1.5.4 Get Elixhauser comorbidity scores
hadm <- unique(all.hadm$hadm_id)
hadm <- paste(hadm, ',', sep='', collapse=' ')
hadm <- substr(hadm, start = 1, stop = nchar(hadm) - 1)

elix <- dbGetQuery(conn, paste("
	SELECT *
	FROM mimic2v26.comorbidity_scores
	WHERE hadm_id in (", hadm, ")")) 

#1.5.5 Add Elixhauser comorbidity index (summation of scores) to all.hadm
all.hadm$elix.index <- 0
for (i in 1:dim(all.hadm)[1])
	all.hadm$elix.index[i] <- sum(elix[i,4:33])

#1.5.5 Add Elixhauser comorbidity index (summation of scores) to all.hadm
all.hadm$elix.index <- 0 
for (i in 1:dim(all.hadm)[1]) 
  all.hadm$elix.index[i] <- sum(elix[i,4:33])

#1.5.6 Aggregate ICD9 codes for patient. Each patient can have multiple ICD9 codes.
###Syntax is for version 8.4.8 PostgreSQL
### Output is all.ICD9 dataframe, 32067 obs. of 2 variables. 
#### hadm_id: int; (example, 2 3 4 5 6 7 8 9 ...)
#### ICD9string: chr; string of ICD9 codes for each patient. (example, "{V30.01, V05.3, V29.0}" "{038.9, 785.59...}")
all.ICD9 <- dbGetQuery(conn, paste(
  "
  SELECT hadm_ID, ARRAY_AGG(code) ICD9string
  FROM mimic2v26.ICD9
  GROUP BY  hadm_id 
  "))  

#1.5.7 Merge ICD9 codes with all.hadm
all.hadm <- merge(all.hadm, all.ICD9, by = 'hadm_id')

#Clean up
rm(all.demo, all.eth, all.hadm.icu, all.temp, elix, last.lab, all.ICD9)


##################
#2 Select cases
##################

###Add a line to the log file with a timestamp at the beginning of step 2
cat(paste('Case identification beginning at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

#2.1 Select cases using the curated list of likely hospital-acquired complications
#  derived from the temporal phenome analysis.

#2.1.1 Load the complication candidates into the workspace
#Decide whether to include the expanded list of complications (default = YES)
include.expanded = T

if(include.expanded)
{
  load(file = 'complication.candidates.expanded.RData')
  temp1 <- comp.candidates.expanded[comp.candidates.expanded$codec != '',c(1:3)]
  temp2 <- comp.candidates.expanded[,4:6]
  colnames(temp2) <- c('codec','description','category')
  comp.candidates <- rbind(temp1, temp2)
  comp.candidates$HAC <- 1
} else comp.candidates <- read.csv(file="complication.candidates.csv",
                                   header=T,
                                   stringsAsFactors=F)

#2.1.2 Decide whether to include the possible complications (default = NO)
include.maybes = F

if(include.maybes)
	foo.icd9 <- comp.candidates[comp.candidates$HAC %in% c(1:2),] else
	foo.icd9 <- comp.candidates[comp.candidates$HAC == 1,]

###Add lines to the log file about the candidate codes
cat('',file='log.txt',
    sep='\n',
    append=TRUE)
cat(paste('Possible complications were included (T/F):', include.maybes),
    file='log.txt',
    sep='\n',
    append=TRUE)

#2.1.3 Create the table of cases by category
cases.all <- data.frame(stringsAsFactors=F)
category <- integer()

for(i in 1:5)
{
  #These need to be represented as strings in this situation, since some ICD.9 codes have letters.  
  temp <-paste("'", foo.icd9$codec[foo.icd9$category == i], "',", sep="", collapse=" ")
  temp <- substr(temp, start = 1, stop = nchar(temp) - 1)
  temp2 <- dbGetQuery(conn, paste("
           SELECT distinct hadm_id 
           FROM mimic2v26.icd9 
	         WHERE code in (", 
          temp,
          ")"
                                  )
                      )
  cases.all <- rbind(cases.all, temp2)         
  category <- c(category, rep(i, dim(temp2)[1]))
}                   

cases.all$category <- category

cases.all <- merge(cases.all, all.hadm)

###Add a line to the log file of total potential cases.
cat('',file='log.txt',
    sep='\n',
    append=TRUE)
cat(paste('Total potential cases:', dim(cases.all)[1]),
    file='log.txt',
    sep='\n',
    append=TRUE)

#2.2 Summary measures of length of hospital stay, to be used for controls
cases.los <- quantile(x=cases.all$diff, probs=seq(0,1,0.01))

#2.3 Remove cases with short or long LOS (outliers, 1st and 99th percentiles)
cases <- cases.all[which(cases.all$diff >= cases.los[2] &
	cases.all$diff <= cases.los[100]),]

###Add a line to the log file of cases removed due to LOS outlier.
cat(paste('Total cases removed due to very short or very long LOS:', 
          dim(cases.all)[1] - dim(cases)[1]),
    file='log.txt',
    sep='\n',
    append=TRUE)

#2.4 Remove cases that don't have any POE or medication events
# temp <- dim(cases)[1]
# cases <- cases[!(is.na(cases$first_poe)),]
# 
# ###Add a line to the log file of cases removed due to lack of medication information.
# cat(paste('Total cases removed due to lack of medication information:', 
#           temp - dim(cases)[1]),
#     file='log.txt',
#     sep='\n',
#     append=TRUE)

###Add a line to the log file of number of excluded cases.
cat('',
    file='log.txt',
    sep='\n',
    append=TRUE)
cat(paste('Total excluded cases:', dim(cases.all)[1] - dim(cases)[1]),
    file='log.txt',
    sep='\n',
    append=TRUE)

###Add a line to the log file of number of included cases
cat('',
    file='log.txt',
    sep='\n',
    append=TRUE)
cat(paste('Total included cases:', dim(cases)[1]),
    file='log.txt',
    sep='\n',
    append=TRUE)

##################
#3 Select controls
##################

###Add a line to the log file with a timestamp at the beginning of step 3
cat(paste('Control selection beginning at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

#3.1 Prepares a list of case hadm_ids in PostgreSQL friendly format
hadm <- unique(cases.all$hadm_id)
hadm <- paste(hadm, ',', sep='', collapse=' ')
hadm <- substr(hadm, start = 1, stop = nchar(hadm) - 1)

controls.all <- dbGetQuery(conn, paste("
	SELECT distinct admissions.hadm_id  
	FROM mimic2v26.admissions 
		JOIN mimic2v26.icustay_detail USING(subject_id)
	WHERE admissions.hadm_id not in (", hadm, ") and
		icustay_age_group = 'adult'"))

controls.all <- merge(controls.all, all.hadm, by = 'hadm_id')

#Controls are category 0 = no complications
controls.all$category <- as.integer(0)

###Add a line to the log file of total potential controls.
cat('',file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Total potential controls:', dim(controls.all)[1]),
	file='log.txt',
	sep='\n',
	append=TRUE)

#3.2 Remove controls with short or long LOS, as defined by cases outliers, 
#	1st and 99th percentiles
controls <- controls.all[which(controls.all$diff >= cases.los[2] &
	controls.all$diff <= cases.los[100]),]

###Add a line to the log file of controls removed due to LOS outlier.
cat(paste('Total controls removed due to very short or very long LOS:', 
          dim(controls.all)[1] - dim(controls)[1]),
    file='log.txt',
    sep='\n',
    append=TRUE)

# #3.3 Remove controls that don't have any POE
# temp <- dim(controls)[1]
# controls <- controls[!(is.na(controls$first_poe)),]
# 
# ###Add a line to the log file of cases removed due to lack of POE.
# cat(paste('Total controls removed due to lack of POE data:', 
#           temp - dim(controls)[1]),
#     file='log.txt',
#     sep='\n',
#     append=TRUE)

###Add a line to the log file of number of excluded controls.
cat('',
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Total excluded controls:', dim(controls.all)[1] - dim(controls)[1]),
	file='log.txt',
	sep='\n',
	append=TRUE)

#3.4 Take sample of controls with a goal of matching a prespecified ratio to cases

#How many controls for each case? This parameter is a ratio.
#Default is 1:1 match
#Do not sample controls, include all

c.match <- 1

#Random sample of controls. Careful - this will be different each time the script runs.
sampler <- sample(1:dim(controls)[1])
controls <- controls[sampler[c(1:round(c.match*length(unique(cases$hadm_id))))],]

###Add a line to the log file of number of included controls
cat('',
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Total included controls:', dim(controls)[1]),
	file='log.txt',
	sep='\n',
	append=TRUE)

#3.5 Combine the cases and controls into one dataframe
combo <- rbind(cases, controls)

##################
#4 Data collection
##################

###Add a line to the log file with a timestamp at the beginning of step 4
cat(paste('Feature extraction beginning at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

#4.1 Collect information on cases and controls up to _interval_ hours

####THIS IS THE KEY MEASURE THAT YOU NEED TO WORK WITH
for (interval in c(1,2,3,6,12,18,24,48,72,96)) #Need to add loop 1
{  
#One-time definition of timepoint for data collection cutoff.
#You need to comment this out if you are going to use the for loop.
#interval <- 24

###Add a line to the log file
cat('',
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Time interval for feature collection:', interval, 'hours'),
	file='log.txt',
	sep='\n',
	append=TRUE)

#4.2 Start with an empty data frame
cohort <- data.frame(t(rep(NA,6)))
names(cohort) <- c('category','hadm_id','gender','age','ethnicity','elix.index')

#4.3 Go through each patient, grabbing features: labs, meds, and CPTs
for(i in 1:dim(combo)[1]) 	
	{ #start loop 1.1

	#4.3.1 First, check that the patient wasn't already discharged.
	#	If they have been discharged, the patient is thrown out (no data collection)
	if (as.numeric(combo$diff[i] - interval) > 0)

    { #Assuming they are not discharged, go ahead with the data collection

	#4.3.2 Labs within the defined time interval
	labs <- dbGetQuery(conn, paste("
		SELECT itemid, valuenum, value, charttime 
		FROM mimic2v26.labevents 
		WHERE subject_id =", combo$subject_id[i], " AND
	      	charttime >= timestamp '", combo$first_lab[i], "' AND 
		charttime <= timestamp '", combo$first_lab[i], "' + interval '", 
			interval, " hours'"))
 	
	#4.3.3 Medication orders (POE) within the defined time interval
	meds.1 <- dbGetQuery(conn, paste("
		SELECT distinct medication, route
		FROM mimic2v26.poe_order 
		WHERE subject_id =", combo$subject_id[i], " AND
	      	enter_dt >= timestamp '", combo$first_lab[i], "' AND 
		enter_dt <= timestamp '", combo$first_lab[i], "' + interval '", 
			interval, " hours'"))

	#4.3.4 Medication events (ICU only) within the defined time interval
  meds.2 <- dbGetQuery(conn, paste("
    SELECT distinct label as medication, route
		FROM mimic2v26.medevents join mimic2v26.d_meditems using(itemid)
		WHERE subject_id =", combo$subject_id[i], " AND
	      	charttime >= timestamp '", combo$first_lab[i], "' AND 
		charttime <= timestamp '", combo$first_lab[i], "' + interval '", 
	     interval, " hours'"))
	
	meds <-  rbind(meds.1,meds.2)
  
	#4.3.5 Procedures (CPTs) within the defined time interval. Description is the text
		#of the procedure, whereas label is a coded concept. This query joins the lookup
		#table of descriptions with the event table; removing the join would make this 
		#step a bit faster.
	cpt <- dbGetQuery(conn, paste("
		SELECT distinct description 
		FROM mimic2v26.procedureevents JOIN mimic2v26.d_codeditems USING(itemid)
		WHERE subject_id =", combo$subject_id[i], " AND
        		proc_dt >= timestamp '", combo$first_lab[i], "' AND 
		proc_dt <= timestamp '", combo$first_lab[i], "' + interval '", 
      		interval, " hours'"))
	
#   if (i ==1) #Write the first patient's labs, meds, CPTs to file for QA/QC
#   {
#     write.csv(labs, file='labs.qa.csv')
#     write.csv(meds, file='meds.qa.csv')
#     write.csv(cpt, file='cpt.qa.csv')
#   }
  
	#4.3.6 Make sure that there exists more than one feature before proceeding
	if (dim(labs)[1]+dim(meds)[1]+dim(cpt)[1] > 1) 
		{ #start loop 1.1.1
		patient <- data.frame(t(rep(NA,1)),stringsAsFactors = FALSE)
		colnames(patient) <- 'dummy'
    
    #4.3.5.1 Start with labs
    if(dim(labs)[1] > 0)
    {
		#list of unique labs
		u.labs <- unique(labs$itemid)
		for(j in 1:length(u.labs))
			{ #start loop 1.1.1.1
			lab.list <- which(labs$itemid == u.labs[j])
			na.test <- sum(is.na(labs$valuenum[lab.list]))
			if (sum(is.na(labs$valuenum[lab.list])) != length(is.na(labs$valuenum[lab.list]))) 
				na.test <- 0	#This is the case when >= 1 lab of a numeric is "not done"
			if (na.test == 0) 
				{ #start loop 1.1.1.1.1
				#This is a numeric feature
				if (length(lab.list) == 1) 
					{ #start loop 1.1.1.1.1.1 
					#TRUE - numeric feature only measured once in the timeframe
					patient[1,paste(u.labs[j],'.min',sep='')] <- labs$valuenum[lab.list]
					patient[1,paste(u.labs[j],'.median',sep='')] <- labs$valuenum[lab.list]
					patient[1,paste(u.labs[j],'.max',sep='')] <- labs$valuenum[lab.list]
					} #end loop 1.1.1.1.1.1
				if (length(lab.list) > 1) 
					{ #start loop 1.1.1.1.1.2
					#TRUE - numeric feature can calculate median, max, and min				
					patient[1,paste(u.labs[j],'.min',sep='')] <- min(labs$valuenum[lab.list])
					patient[1,paste(u.labs[j],'.max',sep='')] <- max(labs$valuenum[lab.list])
					patient[1,paste(u.labs[j],'.median',sep='')] <- median(labs$valuenum[lab.list])
					} #end loop 1.1.1.1.1.2
				} #end loop 1.1.1.1.1
			if (na.test == 1) 
				#A non-numeric feature that only occurs once in the timeframe
				patient[1,paste(u.labs[j],'.nn',sep='')] <- labs$value[lab.list]
			if (na.test > 1) 
				{ #start loop 1.1.1.1.2
				#Non-numeric that occurs more than once; choose the most frequent or if there is a tie,
				#the n most frequent
				temp <- summary(factor(labs$value[lab.list]))
				if (sum(temp == max(temp)) == 1) 
					patient[1,paste(u.labs[j],'.nn',sep='')] <- names(which(temp == max(temp)))
				if (sum(temp == max(temp)) > 1) 
					for (k in which(temp == max(temp))) 
					patient[1,paste(u.labs[j],'.nn',k,sep='')] <- names(temp)[k]					
				} #end loop 1.1.1.1.2
			} #end loop 1.1.1.1
    }
    
		#4.3.6 Create list of unique meds
		if(sum(!is.na(meds$medication)) > 0)
		{

#4.3.6 Rules to reduce duplicate meds

#4.3.6.1 Routes of administration which are equivalent

#Various oral routes      
meds$route <- gsub(meds$route, pattern = 'NG',
	replacement = 'PO')
meds$route <- gsub(meds$route, pattern = 'G TUBE',
	replacement = 'PO')
meds$route[grep(x=meds$route, pattern = 'ORAL')] <- 'PO'
meds$route[grep(x=meds$route, pattern = 'SL')] <- 'PO'
meds$route[grep(x=meds$route, pattern = 'J TUBE')] <- 'PO'

#Various intravenous routes (including continuous infusions)
meds$route <- gsub(meds$route, pattern = 'PB',
	replacement = 'IV')
meds$route[grep(meds$route, pattern = 'IV')] <- 'IV'
meds$route[grep(meds$route, pattern = 'IJ')] <- 'IV'

#Various ocular routes
meds$route[grep(x=meds$route, pattern = '[TH] EYE')] <- 'OU'
meds$route[grep(x=meds$route, pattern = 'OD')] <- 'OU'
meds$route[grep(x=meds$route, pattern = 'OS')] <- 'OU'

meds$route[grep(x=meds$route, pattern = 'AD')] <- 'AU'
meds$route[grep(x=meds$route, pattern = 'NEB')] <- 'IH'
meds$route[grep(x=meds$route, pattern = 'NU')] <- 'IN'
meds$route[grep(x=meds$route, pattern = 'PER')] <- 'SC'

#4.3.6.2 Misc.
meds$medication <- tolower(meds$medication)
#Remove (liquid)
meds$medication <- gsub(meds$medication, pattern = '[[:blank:]]\\(liquid\\)',
	replacement = '')
#Any sodium or magnesium except the first word is removed
meds$medication <- gsub(meds$medication, 
	pattern = '([[:alnum:][:punct:]])([[:space:]]sodium|magnesium)', replacement='\\1')
#Any hcl not the first word
meds$medication <- gsub(meds$medication, 
	pattern = '([[:alnum:][:punct:]])([[:space:]]hcl)', replacement='\\1')
#Remove (self med)
meds$medication <- gsub(meds$medication, pattern = '\\(self med\\)', replacement='')
#Remove *nf* (non-formulary)
meds$medication <- gsub(meds$medication, pattern = '\\*nf\\*', replacement='')
#Remove -k
meds$medication <- gsub(meds$medication, pattern = '-k', replacement='')

#4.3.6.3 Brand name conversions
meds$medication[which(meds$medication == 'aciphex')] <- 'rabeprazole'
meds$medication[which(meds$medication == 'actonel')] <- 'risedronate'
meds$medication[which(meds$medication == 'advair diskus')] <- 'fluticasone-salmeterol'
meds$medication[grep(meds$medication, pattern = 'afrin')] <- 'oxymetazoline'
meds$medication[which(meds$medication == 'aggrastat')] <- 'tirofiban'
meds$medication[which(meds$medication %in% c('alavert','claritin'))] <- 'loratadine'
meds$medication[which(meds$medication == 'amaryl')] <- 'glimepiride'
meds$medication[grep(meds$medication, pattern = 'amicar')] <- 'aminocaproic acid'
meds$medication[which(meds$medication == 'androgel')] <- 'testosterone'
meds$medication[which(meds$medication == 'anzemet')] <- 'dolasetron'
meds$medication <- gsub(meds$medication, pattern = 'augmentin suspension',
	replacement = 'amoxicillin-clavulanic acid')
meds$medication[which(meds$medication == 'angiomax')] <- 'bivalirudin'
meds$medication[which(meds$medication == 'atacand')] <- 'candesartan'
meds$medication[which(meds$medication == 'ativan')] <- 'lorazepam'
meds$medication[which(meds$medication == 'atrovent')] <- 'ipratropium'
meds$medication[which(meds$medication == 'avapro')] <- 'irbesartan'
meds$medication[grep(meds$medication, pattern = 'beconase')] <- 
	'beclomethasone dipropionate'
meds$medication[which(meds$medication == 'avelox')] <- 'moxifloxacin'
meds$medication[which(meds$medication == 'avodart (dutasteride)')] <- 'dutasteride'
meds$medication[which(meds$medication == 'bicitra')] <- 'sodium citrate'
meds$medication[which(meds$medication == 'catapres')] <- 'clonidine'
meds$medication[which(meds$medication == 'celebrex')] <- 'celecoxib'
meds$medication[which(meds$medication == 'cortro')] <- 'cosyntropin'
meds$medication[which(meds$medication == 'creon 10')] <- 'pancrease'
meds$medication[grep(meds$medication, pattern = 'diltiaz')] <- 'diltiazem'
meds$medication[which(meds$medication == 'diovan')] <- 'valsartan'
meds$medication[which(meds$medication == 'enbrel')] <- 'etanercept'
meds$medication[which(meds$medication == 'entocort ec')] <- 'budesonide'
meds$medication[which(meds$medication == 'evista')] <- 'raloxifene'
meds$medication[which(meds$medication == 'exelon')] <- 'rivastigmine'
meds$medication[grep(meds$medication, pattern = 'famvir')] <- 'famciclovir'
meds$medication[which(meds$medication == 'femara')] <- 'letrozole'
meds$medication[grep(x=meds$medication, pattern = 'flonas')] <- 'fluticasone propionate nasal'
meds$medication[grep(meds$medication, pattern = 'flovent')] <- 'fluticasone propionate 110mcg'
meds$medication[which(meds$medication %in%
                        c('gengraf','neoral'))] <- 'cyclosporine (neoral)'
meds$medication[which(meds$medication == 'gleevec')] <- 'imatinib'
meds$medication[which(meds$medication == 'glucophage xr')] <- 'metformin'
meds$medication[which(meds$medication == 'hepsera')] <- 'adefovir'
meds$medication[which(meds$medication %in% 
	c('hespan','hetastarch'))] <- 'hydoxyethyl starch'
meds$medication[which(meds$medication == 'keppra')] <- 'levetiracetam'
meds$medication[which(meds$medication == 'lantus')] <- 'insulin glargine'
meds$medication[which(meds$medication == 'lasix')] <- 'furosemide'
meds$medication[which(meds$medication == 'lescol')] <- 'fluvastatin'
meds$medication[which(meds$medication == 'levoxyl')] <- 'levothyroxine sodium'
meds$medication[which(meds$medication == 'lexapro')] <- 'escitalopram'
meds$medication[which(meds$medication == 'lyrica')] <- 'pregabalin'
meds$medication[which(meds$medication == 'mirapex')] <- 'pramipexole'
meds$medication[which(meds$medication == 'mobic')] <- 'meloxicam'
meds$medication[which(meds$medication == 'myfortic')] <- 'mycophenolate mofetil'
meds$medication[which(meds$medication == 'namenda')] <- 'memantine'
meds$medication[which(meds$medication == 'nasacort')] <- 'triamcinolone'
meds$medication[which(meds$medication %in%
                        c('prilosec','nexium'))] <- 'omeprazole'
meds$medication[which(meds$medication == 'niaspan')] <- 'niacin'
meds$medication[grep(meds$medication, pattern = 'pangest')] <- 'pancrease'
meds$medication[which(meds$medication == 'pletal')] <- 'cilostazol'
meds$medication[which(meds$medication == 'precedex')] <- 'dexmedetomidine'
meds$medication[which(meds$medication == 'procardia xl')] <- 'nifedipine cr'
meds$medication[which(meds$medication == 'procrit')] <- 'epoetin alfa'
meds$medication[which(meds$medication == 'proplex t')] <- 'factor ix complex'
meds$medication <- gsub(meds$medication, pattern = 'protonix',
	replacement = 'pantoprazole')
meds$medication[which(meds$medication == 'provera')] <- 'medroxyprogesterone acetate'
meds$medication[which(meds$medication == 'provigil')] <- 'modafinil'
meds$medication[which(meds$medication %in%
                        c('galantamine hydrobromide','reminyl'))] <- 'galantamine'
meds$medication[which(meds$medication == 'reopro')] <- 'abciximab'
meds$medication[which(meds$medication == 'revia')] <- 'naltrexone'
meds$medication[which(meds$medication == 'rilutek')] <- 'riluzole'
meds$medication[which(meds$medication == 'salagen')] <- 'pilocarpine'
meds$medication[which(meds$medication == 'sandostatin')] <- 'octreotide'
meds$medication[which(meds$medication == 'sensipar')] <- 'cinacalcet'
meds$medication[which(meds$medication == 'simulect')] <- 'basiliximab'
meds$medication[which(meds$medication == 'skelaxin')] <- 'metaxalone'
meds$medication[which(meds$medication == 'tarceva')] <- 'erlotinib'
meds$medication[which(meds$medication == 'tricor')] <- 'fenofibrate'
meds$medication <- gsub(meds$medication, pattern = 'unasyn',
	replacement = 'ampicillin-sulbactam')
meds$medication[grep(x=meds$medication, pattern = 'urocit')] <- 'potassium citrate'
meds$medication[which(meds$medication == 'valtrex')] <- 'valaciclovir'
meds$medication[which(meds$medication == 'valium')] <- 'diazepam'
meds$medication[grep(meds$medication, pattern = 'viokase')] <- 'pancrease'
meds$medication[which(meds$medication == 'vioxx')] <- 'rofecoxib'
meds$medication[which(meds$medication == 'voltaren')] <- 'diclofenac'
meds$medication[which(meds$medication == 'welchol')] <- 'colesevelam'
meds$medication[which(meds$medication == 'xigris')] <- 'drotrecogin alfa'
meds$medication[which(meds$medication == 'zebeta')] <- 'bisoprolol'
meds$medication[which(meds$medication == 'zelnorm')] <- 'tegaserod'
meds$medication[which(meds$medication == 'zocor')] <- 'simvastatin'
meds$medication[which(meds$medication == 'zofran')] <- 'ondansetron'
meds$medication[which(meds$medication == 'zonalon')] <- 'doxepin'
meds$medication[which(meds$medication == 'zymar')] <- 'gatifloxacin'
meds$medication[which(meds$medication == 'zyrtec')] <- 'cetirizine'


#4.3.6.4 Synonyms
meds$medication[which(meds$medication %in% 
	c('acetaminophen w/codeine','acetaminophen w/codeine elixir'))] <- 'acetaminophen-codeine'
meds$medication[which(meds$medication %in% 
	c('acetaminophen','acetaminophen (liquid)','acetaminophen (rectal)'))] <- 'acetaminophen'
meds$medication[grep(x=meds$medication, pattern = 'acetaminophen-')] <- 'butalbital-acet-caffeine'
meds$medication[grep(x=meds$medication, pattern = 'acetylc')] <- 'acetylcysteine'
meds$medication[grep(x=meds$medication, pattern = 'acyclo')] <- 'acyclovir'
meds$medication[grep(x=meds$medication, pattern = 'adefovir')] <- 'adefovir'
meds$medication[grep(x=meds$medication, pattern = 'albumin')] <- 'albumin'
meds$medication[grep(x=meds$medication, pattern = 'albuterol-')] <- 'duoneb'
meds$medication[grep(x=meds$medication, pattern = 'albuterol')] <- 'albuterol'
meds$medication[grep(x=meds$medication, pattern = 'amantadine')] <- 'amantadine'
meds$medication[grep(x=meds$medication, pattern = 'amlodipine')] <- 'amlodipine'
meds$medication[grep(x=meds$medication, pattern = 'amylase')] <- 'pancrease'
meds$medication[grep(x=meds$medication, pattern = 'anti-thymocyte')] <- 'atg'
meds$medication[grep(x=meds$medication, pattern = 'artificial tear')] <- 'artificial tears'
meds$medication[grep(x=meds$medication, pattern = 'ascorbic')] <- 'ascorbate'
meds$medication[grep(x=meds$medication, pattern = '-aspirin')] <- 'aggrenox'
meds$medication[grep(x=meds$medication, pattern = 'aspirin')] <- 'aspirin'
meds$medication[which(meds$medication %in%
	c('bacitracin ointment','bacitracin ophthalmic oint'))] <- 'bacitracin'
meds$medication[which(meds$medication == 'beclomethasone dipro. aq (nasal)')] <- 
	'beclomethasone dipropionate'
meds$medication[grep(x=meds$medication, pattern = 'bisacodyl')] <- 'bisacodyl'
meds$medication[grep(x=meds$medication, pattern = 'budesonide')] <- 'budesonide'
meds$medication[grep(x=meds$medication, pattern = 'bupivacaine')] <- 'bupivacaine'
meds$medication[grep(x=meds$medication, pattern = 'buproprion [s(]')] <- 'buproprion sr'
meds$medication[grep(x=meds$medication, pattern = 'calcium carb')] <- 'calcium carbonate'
meds$medication[grep(x=meds$medication, pattern = 'calcium re')] <- 'calcium gluconate'
meds$medication[grep(x=meds$medication, pattern = 'carbamazepine')] <- 'carbamazepine'
meds$medication[grep(x=meds$medication, pattern = 'carbid')] <- 'carbidopa-levodopa'
meds$medication[grep(x=meds$medication, pattern = 'caspo')] <- 'caspofungin'
meds$medication[grep(x=meds$medication, pattern = 'cefotetan')] <- 'cefotetan'
meds$medication[grep(x=meds$medication, pattern = 'cepacol')] <- 'cepacol'
meds$medication[grep(x=meds$medication, pattern = 'charcoal')] <- 'activated charcoal'
meds$medication[grep(x=meds$medication, pattern = 'chlorhex')] <- 'chlorhexidine'
meds$medication[which(meds$medication == 'ciprofloxacin 0.3% ophth soln')] <- 
	'ocular fluoroquinolone'
meds$medication[grep(x=meds$medication, pattern = 'cipro')] <- 'ciprofloxacin'
meds$medication[grep(x=meds$medication, pattern = 'cisatracurim')] <- 'cisatracurium'
meds$medication[grep(x=meds$medication, pattern = 'clobet')] <- 'clobetasol'
meds$medication[grep(x=meds$medication, pattern = 'clonidine')] <- 'clonidine'
meds$medication[grep(x=meds$medication, pattern = 'clopidogrel')] <- 'clopidogrel'
meds$medication[grep(x=meds$medication, pattern = 'codeine [sp]')] <- 'codeine'
meds$medication[which(meds$medication %in% 
	c('cyclosporine (continuous infusion for bmt)',
	'cyclosporine (neoral) modified',
	'cyclosporine modified (neoral)'))] <- 'cyclosporine (neoral)'
meds$medication[which(meds$medication == 'depak')] <- 'depakote'
meds$medication[grep(x=meds$medication, pattern = 'desmopr')] <- 'desmopressin'
meds$medication[grep(x=meds$medication, pattern = 'dexametha')] <- 'dexamethasone'
meds$medication[grep(x=meds$medication, pattern = 'didanos')] <- 'didanosine'
meds$medication[grep(x=meds$medication, pattern = 'dila|dilan')] <- 'phenytoin'
meds$medication[grep(x=meds$medication, pattern = 'disopyr')] <- 'disopyramide'
meds$medication[grep(x=meds$medication, pattern = 'valpr')] <- 'depakote'
meds$medication[grep(x=meds$medication, pattern = 'drotre')] <- 'drotrecogin alfa'
meds$medication[grep(x=meds$medication, pattern = 'docusate')] <- 'docusate'
meds$medication[grep(x=meds$medication, pattern = 'doxacurium')] <- 'doxacurium'
meds$medication[which(meds$medication %in% 
                        c('factor 9 complex','factor ix complex'))] <- 'factor ix complex'
meds$medication[grep(meds$medication, pattern = 'famotidine')] <- 'famotidine'
meds$medication[grep(meds$medication, pattern = 'fenofibrate')] <- 'fenofibrate'
meds$medication[grep(meds$medication, pattern = 'fentanyl')] <- 'fentanyl'
meds$medication[grep(x=meds$medication, pattern = 'ferrous sulfate')] <- 'feso4'
meds$medication[grep(x=meds$medication, pattern = 'fluocin')] <- 'fluocinonide'
meds$medication[grep(x=meds$medication, pattern = 'fluphen')] <- 'fluphenazine'
meds$medication[grep(x=meds$medication, pattern = 'one-salm')] <- 'fluticasone-salmeterol'
meds$medication[grep(x=meds$medication, pattern = 'gent[a]+')] <- 'gentamicin'
meds$medication[grep(x=meds$medication, pattern = 'glyb')] <- 'glyburide'
meds$medication[grep(x=meds$medication, pattern = 'guai')] <- 'guaifenesin'
meds$medication[which(meds$medication %in% 
	c('hep','heparin ','heparin sodium','heparin (preservative free)'))] <- 'heparin'
meds$medication[grep(x=meds$medication, pattern = 'flush')] <- 'line flush'
meds$medication[grep(x=meds$medication, pattern = 'hepatitis b i')] <- 'hepatitis b immune globulin'
meds$medication[grep(x=meds$medication, pattern = 'hydrocort')] <- 'hydrocortisone'
meds$medication[grep(x=meds$medication, pattern = 'hydromorph')] <- 'hydromorphone'
meds$medication[which(meds$medication %in% 
	c('hyoscy','l-hyoscyamine sl'))] <- 'hyoscyamine'
meds$medication[grep(x=meds$medication, pattern = 'hydroxide')] <- 'aluminum hydroxide'
meds$medication[grep(x=meds$medication, pattern = 'ibuprofen')] <- 'ibuprofen'
meds$medication[grep(x=meds$medication, pattern = 'imatinib')] <- 'imatinib'
meds$medication[grep(x=meds$medication, pattern = 'insulin human')] <- 'insulin'
meds$medication[grep(x=meds$medication, pattern = 'ipratropium')] <- 'ipratropium'
meds$medication[which(meds$medication == 'isosorbide dinitrate sa')] <- 
	'isosorbide dinitrate er'
meds$medication[grep(x=meds$medication, pattern = 'labet[ao]lol')] <- 'labetalol'
meds$medication[grep(x=meds$medication, pattern = 'lactulose')] <- 'lactulose'
meds$medication[grep(x=meds$medication, pattern = 'lamivudine-zidov')] <- 'lamivudine-zidovudine'
meds$medication[grep(meds$medication, pattern = 'lansoprazole')] <- 'lansoprazole'
meds$medication[grep(meds$medication, pattern = 'letrozole')] <- 'letrozole'
meds$medication[grep(meds$medication, pattern = 'levetirac')] <- 'levetiracetam'
meds$medication[grep(meds$medication, pattern = 'lidocain')] <- 'lidocaine'
meds$medication[grep(x=meds$medication, pattern = 'lithium')] <- 'lithium'
meds$medication[grep(x=meds$medication, pattern = 'lopina')] <- 'lopinavir-ritonavir'
meds$medication[which(meds$medication == 'ritonavir/lopinavir')] <- 'lopinavir-ritonavir'
meds$medication[grep(x=meds$medication, pattern = 'losartan')] <- 'losartan'
meds$medication[grep(x=meds$medication, pattern = 'maalox/')] <- 'magic mouthwash'
meds$medication[grep(x=meds$medication, pattern = 'magnesium sul')] <- 'magnesium sulfate'
meds$medication[grep(x=meds$medication, pattern = 'mannitol')] <- 'mannitol'
meds$medication[grep(x=meds$medication, pattern = 'megestrol')] <- 'megestrol'
meds$medication[grep(x=meds$medication, pattern = 'meperidine')] <- 'meperidine'
meds$medication[which(meds$medication == 'mesalamine (rectal)')] <- 'mesalamine enema'
meds$medication[grep(x=meds$medication, pattern = 'metformin')] <- 'metformin'
meds$medication[grep(x=meds$medication, pattern = 'methylpred')] <- 'methylprednisolone'
meds$medication[grep(x=meds$medication, pattern = 'ol xl')] <- 'metoprolol succinate xl'
meds$medication[grep(x=meds$medication, pattern = 'ol tar')] <- 'metoprolol'
meds$medication[which(meds$medication == 'metronidazole gel 0.75%-vaginal')] <- 'metrogel vaginal'
meds$medication[grep(x=meds$medication, pattern = 'metronidazole')] <- 'metronidazole'
meds$medication[grep(x=meds$medication, pattern = 'miconazole n')] <- 'miconazole'
meds$medication[grep(x=meds$medication, pattern = 'morphine')] <- 'morphine'
meds$medication[grep(x=meds$medication, pattern = 'multi')] <- 'multivitamins'
meds$medication[grep(x=meds$medication, pattern = 'mofetil')] <- 'mycophenolate mofetil'
meds$medication[grep(x=meds$medication, pattern = 'mupirocin')] <- 'mupirocin'
meds$medication[grep(x=meds$medication, pattern = 'nicot')] <- 'nicotine patch'
meds$medication[grep(x=meds$medication, pattern = 'rin oin')] <- 'nitropaste'
meds$medication[grep(x=meds$medication, pattern = 'norepi')] <- 'norepinephrine'
meds$medication[grep(x=meds$medication, pattern = 'nystatin')] <- 'nystatin'
meds$medication[grep(x=meds$medication, pattern = 'olanz')] <- 'olanzapine'
meds$medication[grep(x=meds$medication, pattern = 'ondans')] <- 'ondansetron'
meds$medication[which(meds$medication %in%
                        c('oxycodone (immediate release) ','oxycodone liquid'))] <- 'oxycodone'
meds$medication[grep(x=meds$medication, pattern = 'ondans')] <- 'ondansetron'
meds$medication[which(meds$medication %in% 
	c('oxycodone (sustained release)','oxycodone sr (oxycontin)'))] <- 'oxycontin'
meds$medication[grep(x=meds$medication, pattern = 'pancrease')] <- 'pancrease'
meds$medication[grep(x=meds$medication, pattern = 'papain-u')] <- 'papain-urea'
meds$medication[grep(x=meds$medication, pattern = 'pentam')] <- 'pentamidine'
meds$medication[grep(x=meds$medication, pattern = 'pentobarb')] <- 'pentobarbital'
meds$medication[grep(x=meds$medication, pattern = 'phentolamine')] <- 'phentolamine'
meds$medication[grep(x=meds$medication, pattern = 'phenytoin')] <- 'phenytoin'
meds$medication[grep(x=meds$medication, pattern = 'piper')] <- 'piperacillin-tazobactam'
meds$medication[grep(x=meds$medication, pattern = 'posaconazole')] <- 'posaconazole'
meds$medication[grep(x=meds$medication, pattern = 'potassium ch')] <- 'kcl'
meds$medication[grep(x=meds$medication, pattern = 'potassium ph')] <- 'k-phos'
meds$medication[grep(x=meds$medication, pattern = 'pramipexole')] <- 'pramipexole'
meds$medication[grep(x=meds$medication, pattern = 'prochlor')] <- 'compazine'
meds$medication[grep(x=meds$medication, pattern = 'propofol')] <- 'propofol'
meds$medication[grep(x=meds$medication, pattern = 'psyllium')] <- 'psyllium'
meds$medication[grep(meds$medication, pattern = 'ranitidine')] <- 'ranitidine'
meds$medication[grep(x=meds$medication, pattern = 'regular insulin')] <- 'insulin'
meds$medication[grep(x=meds$medication, pattern = 'risedronate')] <- 'risedronate'
meds$medication[grep(x=meds$medication, pattern = 'risperi')] <- 'risperidone'
meds$medication[grep(x=meds$medication, pattern = 'rocuro')] <- 'rocuronium'
meds$medication[grep(x=meds$medication, pattern = 'ol xin')] <- 'salmeterol'
meds$medication[grep(x=meds$medication, pattern = 'opium')] <- 'tincture of opium'
meds$medication[grep(x=meds$medication, pattern = 'silver sulfa')] <- 'silver sulfadiazine'
meds$medication[grep(x=meds$medication, pattern = 'simethicone')] <- 'simethicone'
meds$medication[grep(x=meds$medication, pattern = 'sodium bi')] <- 'sodium bicarbonate'
meds$medication[grep(meds$medication, pattern = 'sulfameth')] <- 'sulfameth-trimethoprim'
meds$medication[grep(x=meds$medication, pattern = 'tacro')] <- 'tacrolimus'
meds$medication[grep(x=meds$medication, pattern = 'tenofovir')] <- 'tenofovir'
meds$medication[grep(x=meds$medication, pattern = 'tetanus-diphtheria')] <- 'tetanus diphtheria vaccine'
meds$medication[grep(x=meds$medication, pattern = 'testosterone')] <- 'testosterone'
meds$medication[grep(x=meds$medication, pattern = 'theophyl')] <- 'theophylline'
meds$medication[grep(x=meds$medication, pattern = 'timolol')] <- 'timolol'
meds$medication[grep(x=meds$medication, pattern = 'topiramate')] <- 'topiramate'
meds$medication[grep(x=meds$medication, pattern = 'tramadol')] <- 'tramadol'
meds$medication[grep(x=meds$medication, pattern = 'triamcin')] <- 'triamcinolone'
meds$medication[grep(x=meds$medication, pattern = 'triaz')] <- 'triazolam'
meds$medication[grep(x=meds$medication, pattern = 'truvada')] <- 'emtricitabine-tenofovir'
meds$medication[grep(x=meds$medication, pattern = 'tucks hemorrhoidal')] <- 'anusol'
meds$medication[grep(x=meds$medication, pattern = 'valgan')] <- 'valganciclovir'
meds$medication[grep(x=meds$medication, pattern = 'vecuronium')] <- 'vecuronium'
meds$medication[grep(x=meds$medication, pattern = 'xopenex')] <- 'albuterol'

#4.3.6.5 Split combo meds into their individual components
if(any(grepl(x=meds$medication, pattern = 'butalbital-a')))
{
  meds$medication[grep(x=meds$medication, pattern = 'butalbital-a')] <- 'butalbital'
  meds <- rbind(meds, c('acetaminophen','PO'))
  meds <- rbind(meds, c('caffeine','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'emtricitabine-')))
{
  meds$medication[grep(x=meds$medication, pattern = 'emtricitabine-')] <- 'emtricitabine'
  meds <- rbind(meds, c('tenofovir','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'chlorpheniramine-h')))
{
  meds$medication[grep(x=meds$medication, pattern = 'chlorpheniramine-h')] <- 'chlorpheniramine'
  meds <- rbind(meds, c('hydrocodone','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'dextromethophan-g')))
{
  meds$medication[grep(x=meds$medication, pattern = 'dextromethophan-g')] <- 'dextromethophan'
  meds <- rbind(meds, c('guaifenesin','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'fluticasone-s')))
{
  meds$medication[grep(x=meds$medication, pattern = 'fluticasone-s')] <- 'fluticasone'
  meds <- rbind(meds, c('salmeterol','IH'))
}

if(any(grepl(x=meds$medication, pattern = 'glucovance (5mg-500mg)')))
{
  meds$medication[grep(x=meds$medication, pattern = 'glucovance (5mg-500mg)')] <- 'glyburide'
  meds <- rbind(meds, c('metformin','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'guaifenesin-c')))
{
  meds$medication[grep(x=meds$medication, pattern = 'guaifenesin-c')] <- 'guaifenesin'
  meds <- rbind(meds, c('codeine','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'guaifenesin-d')))
{
  meds$medication[grep(x=meds$medication, pattern = 'guaifenesin-d')] <- 'guaifenesin'
  meds <- rbind(meds, c('dextromethorphan','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'hydrocodone-a')))
{
  meds$medication[grep(x=meds$medication, pattern = 'hydrocodone-a')] <- 'hydrocodone'
  meds <- rbind(meds, c('acetaminophen','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'lamivudine-z')))
{
  meds$medication[grep(x=meds$medication, pattern = 'lamivudine-z')] <- 'lamivudine'
  meds <- rbind(meds, c('zidovudine','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'kaletra')))
{
  meds$medication[grep(x=meds$medication, pattern = 'kaletra')] <- 'lopinavir'
  meds <- rbind(meds, c('ritonavir','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'malarone')))
{
  meds$medication[grep(x=meds$medication, pattern = 'malarone')] <- 'atovaquone'
  meds <- rbind(meds, c('proguanil','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'oxycodone-ace')))
{
  meds$medication[grep(x=meds$medication, pattern = 'oxycodone-ace')] <- 'oxycodone'
  meds <- rbind(meds, c('acetaminophen','PO'))
}

if(any(grepl(x=meds$medication, pattern = 'triamterene-')))
{
  meds$medication[grep(x=meds$medication, pattern = 'triamterene-')] <- 'triamterene'
  meds <- rbind(meds, c('hydrochlorothiazide','PO'))
}

#4.3.6.6 Routes bioequivalent or same indication (or mistake)
meds$route[grep(x=meds$medication, pattern = 'acetazol')] <- 'PO'
meds$route[grep(x=meds$medication, pattern = 'warf')] <- 'POPR'
meds$route[grep(x=meds$medication, pattern = 'acetylcys')] <- 'POIV'
meds$route[which(meds$medication == 'acetaminophen')] <- 'POPR'
meds$route[which(meds$medication == 'acyclovir')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'albuterol')] <- 'IH'
meds$route[grep(x=meds$medication, pattern = 'allopurinol')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'alprazolam')] <- 'PO'
meds$route[grep(x=meds$medication, pattern = 'aminocaproic')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'amio')] <- 'POIV'
meds$route[which(meds$medication == 'ampicillin')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'artificial tears')] <- 'OU'
meds$route[grep(x=meds$medication, pattern = 'ascorbate')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'aspirin')] <- 'POPR'
meds$route[grep(x=meds$medication, pattern = 'azathioprine')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'azithro')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'benztr')] <- 'POIM'
meds$route[grep(x=meds$medication, pattern = 'bisacodyl')] <- 'POPR'
meds$route[grep(x=meds$medication, pattern = 'brimonidine')] <- 'OU'
meds$route[grep(x=meds$medication, pattern = 'budesonide')] <- 'IH'
meds$route[grep(x=meds$medication, pattern = 'bumetanide')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'calcitonin')] <- 'INIM'
meds$route[grep(x=meds$medication, pattern = 'calcitriol')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'calcium gluc')] <- 'IV'
meds$route[grep(x=meds$medication, pattern = 'celebrex')] <- 'PO'
meds$route[grep(x=meds$medication, pattern = 'ceftriaxone')] <- 'IVIM'
meds$route[which(meds$medication == 'chlorpromazine')] <- 'MULTI'
meds$route[which(meds$medication == 'chlorthiazide')] <- 'POIV'
meds$route[which(meds$medication == 'ciprofloxacin')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'clinda')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'clonidine')] <- 'POTD'
meds$route[grep(x=meds$medication, pattern = 'cyano')] <- 'POSC'
meds$route[which(meds$medication == 'cyclosporine (neoral)')] <- 'POIV'
meds$route[which(meds$medication == 'cyclosporine (sandimmune)')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'dantrolene')] <- 'POIV'
meds$route[which(meds$medication == 'depakote')] <- 'POIV'
meds$route[which(meds$medication == 'desmopressin')] <- 'MULTI'
meds$route[which(meds$medication == 'dexamethasone')] <- 'POIV'
meds$route[which(meds$medication == 'diazepam')] <- 'MULTI'
meds$route[which(meds$medication == 'digoxin')] <- 'POIV'
meds$route[which(meds$medication == 'diltiazem')] <- 'POIV'
meds$route[which(meds$medication == 'diphenhydramine')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'dolas')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'dopamine')] <- 'IV'
meds$route[grep(x=meds$medication, pattern = 'doxy')] <- 'POIV'
meds$route[which(meds$medication == 'epoetin alfa')] <- 'SCIV'
meds$route[which(meds$medication == 'erythromycin')] <- 'POIV'
meds$route[which(meds$medication == 'ethacrynic acid')] <- 'POIV'
meds$route[which(meds$medication == 'famotidine')] <- 'POIV'
meds$route[which(meds$medication == 'fentanyl patch')] <- 'TD'
meds$route[which(meds$medication == 'filgrastim')] <- 'SC'
meds$route[grep(x=meds$medication, pattern = 'fluc')] <- 'POIV'
meds$route[which(meds$medication == 'fluphenazine')] <- 'POIM'
meds$route[grep(x=meds$medication, pattern = 'folic')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'furos')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'gent')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'glucagon')] <- 'IVSC'
meds$route[grep(x=meds$medication, pattern = 'halop')] <- 'MULTI'
meds$route[grep(x=meds$medication, pattern = 'hydral')] <- 'POIV'
meds$route[which(meds$medication == 'hydrocortisone' &
	meds$route %in% c('PO','IV'))] <- 'POIV'
meds$route[which(meds$medication == 'hydromorphone')] <- 'MULTI'
meds$route[which(meds$medication == 'hydroxyzine')] <- 'POIM'
meds$route[which(meds$medication == 'insulin')] <- 'SCIV'
meds$route[which(meds$medication == 'ketorolac')] <- 'IVIM'
meds$route[which(meds$medication == 'labetalol')] <- 'POIV'
meds$route[which(meds$medication == 'lactulose')] <- 'POPR'
meds$route[grep(x=meds$medication, pattern = 'levet')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'levoflox')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'levothy')] <- 'POIV'
meds$route[which(meds$medication == 'line flush')] <- 'IV'
meds$route[grep(x=meds$medication, pattern = 'linez')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'loraz')] <- 'MULTI'
meds$route[which(meds$medication == 'magnesium sulfate')] <- 'IV'
meds$route[which(meds$medication == 'meperidine')] <- 'IVIM'
meds$route[which(meds$medication == 'methadone')] <- 'POIV'
meds$route[which(meds$medication == 'methotrexate')] <- 'POSC'
meds$route[grep(x=meds$medication, pattern = 'ene blue')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'methylpred')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'metoclop')] <- 'POIV'
meds$route[which(meds$medication == 'metoprolol')] <- 'POIV'
meds$route[which(meds$medication == 'metronidazole')] <- 'POIV'
meds$route[which(meds$medication == 'midazolam')] <- 'IV'
meds$route[which(meds$medication == 'morphine')] <- 'MULTI'
meds$route[which(meds$medication == 'multivitamins')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'mofetil')] <- 'POIV'
meds$route[which(meds$medication == 'naloxone')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'nesir')] <- 'IVIJ'
meds$route[which(meds$medication == 'nicotine patch')] <- 'TD'
meds$route[grep(x=meds$medication, pattern = 'octreo')] <- 'SCIV'
meds$route[which(meds$medication == 'olanzapine')] <- 'PO'
meds$route[grep(x=meds$medication, pattern = 'ondans')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'pantop')] <- 'POIV'
meds$route[which(meds$medication == 'pentamidine')] <- 'IHIV'
meds$route[grep(x=meds$medication, pattern = 'phenob')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'phenylephrine')] <- 'IV'
meds$route[grep(x=meds$medication, pattern = 'phenytoin')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'phytonad')] <- 'MULTI'
meds$route[which(meds$medication == 'kcl')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'procain')] <- 'POIV'
meds$route[which(meds$medication == 'compazine')] <- 'MULTI'
meds$route[which(meds$medication == 'promethazine')] <- 'POIV'
meds$route[which(meds$medication == 'propofol')] <- 'IV'
meds$route[grep(x=meds$medication, pattern = 'propran')] <- 'POIV'
meds$route[which(meds$medication == 'pyridoxine')] <- 'IV'
meds$route[which(meds$medication == 'rifampin')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'sodium bicarb')] <- 'POIV'
meds$route[grep(x=meds$medication, pattern = 'sodium poly')] <- 'POPR'
meds$route[grep(x=meds$medication, pattern = 'sulfameth')] <- 'POIV'
meds$route[which(meds$medication == 'thiamine')] <- 'POIV'
meds$route[which(meds$medication == 'torsemide')] <- 'POIV'
meds$route[which(meds$medication == 'verapamil')] <- 'POIV'
meds$route[which(meds$medication == 'voriconazole')] <- 'POIV'
meds$route[which(meds$medication == 'ziprasidone')] <- 'POIM'
#End of rules

#4.3.6.6 Trim leading and trailing white spaces
meds$medication <- sub("^\\s+", "", meds$medication) 
meds$medication <- sub("\\s+[^[:alnum:][:punct:]]+", "", meds$medication)

#4.3.6.7 Finally, combine duplicate medications 
			u.meds <- unique(meds[!(is.na(meds$medication)),])

			for(j in 1:dim(u.meds)[1])
				patient[1,paste(u.meds$medication[j],'.',
					u.meds$route[j],'.rx',sep='')] <- T 

#The following applies to special categories which are left out of the generic pipeline.
#
#			#Checks for any antibiotics
#			if (sum(duplicated(rbind(low.risk.abx,u.meds))) >= 1) {
#				patient[1,'low.risk.abx.rx'] <- T
#				} else patient[1,'low.risk.abx.rx'] <- F
#			#Checks for high risk antibiotics
#			if (sum(duplicated(rbind(high.risk.abx,u.meds))) >= 1) {
#				patient[1,'high.risk.abx.rx'] <- T
#				} else patient[1,'high.risk.abx.rx'] <- F
#			#Checks for any PPI or H2 blocker
#			if (sum(duplicated(rbind(any.ppi.h2,u.meds))) >= 1) {
#				patient[1,'any.ppi.h2.rx'] <- T
#				} else patient[1,'any.ppi.h2.rx'] <- F

			}

    #4.3.7 Create a list of unique CPT codes
		if(dim(cpt)[1] > 0)
		{
    #Trim trailing white spaces  
    cpt$description <- sub("\\s+[^[:alnum:][:punct:]]+", "", cpt$description)
		
    u.cpt <- unique(cpt)
    
    for(j in 1:dim(u.cpt)[1])
		  patient[1,paste(u.cpt$description[j],'.cpt',sep='')] <- T 
		}
    
    #4.3.8 Add the patient to the cohort table 
		patient <- patient[1,2:dim(patient)[2]]
		#Add new column names, if any, to cohort table
		cohort[,colnames(patient)[!(colnames(patient) %in% colnames(cohort))]] <- NA
		case.match <- colnames(cohort)[which(colnames(cohort) %in% colnames(patient))]
		patient <- patient[,case.match]
		cohort[i,c(1:6,which(colnames(cohort) %in% colnames(patient)))] <- 
			cbind(combo[i,c('category','hadm_id','gender','age','ethnicity','elix.index')],patient)
	
		} #3
	}
	} #1

###Add a line to the log file with a timestamp
print('Cohort feature collection now complete')
cat(paste('Cohort feature extraction completed at', timestamp()),
	file='log.txt',
	sep='\n',
	append=TRUE)

#OPTIONAL Saves the cohort with ALL measured features
#save(cohort, file = paste('cohort.', v, '.RData', sep=''))

##################
#5 Post-processing
##################

###Add a line to the log file with a timestamp at the beginning of step 5
cat(paste('Post-processing beginning at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

#5.1 Parameters to only include features that are measured in a certain number
#	of cases. This makes the predictive analytics in weka tractable.

feature.na <- rep(0,dim(cohort)[2])
for(i in 1:length(feature.na)) 
	feature.na[i] <- sum(is.na(cohort[,i]))/dim(cohort)[1]

index.meds <- grep(colnames(cohort), pattern = '.rx')
index.cpt <- grep(colnames(cohort), pattern = '.cpt')
index.labs <- which(!(1:dim(cohort)[2] %in% c(1:6,index.meds,index.cpt)) == T)

#5.1.1 This parameter is what proportion of medication values need to be TRUE
#Default: top 10%
med.param <- quantile(feature.na[index.meds],probs=seq(0,1,.1))[2]
pointer.meds <- which(feature.na[index.meds] <= med.param)

#5.1.2 This parameter is what proportion of CPT values need to be TRUE
#Default: top 10%
cpt.param <- quantile(feature.na[index.cpt],probs=seq(0,1,.1))[2]
pointer.cpt <- which(feature.na[index.cpt] <= cpt.param)

#5.1.3 This parameter is what proportion of lab values need to be FINITE
#Default: 50%
lab.param <- 0.5
pointer.labs <- which(feature.na[index.labs] <= (1 - lab.param))

cohort <- cohort[,c(1:6,
	index.labs[pointer.labs],
	index.meds[pointer.meds],
	index.cpt[pointer.cpt])]

#5.2 Replaces NA's in the medication columns with FALSE if there is at least one record
#of medication for a patient
for (i in 1:dim(cohort)[1]) {
	if (any(!(is.na(cohort[i,grep(colnames(cohort), pattern = '.rx')]))))
		{
		index <- grep(colnames(cohort), pattern = '.rx')
		pointer <- which(is.na(cohort[i,index]))
		cohort[i,index[pointer]] <- F
		}
	}

#5.3 Replaces NA's in the procedure columns with FALSE if there is at least one record
#of a procedure for the patient
for (i in 1:dim(cohort)[1]) {
	if (any(!(is.na(cohort[i,grep(colnames(cohort), pattern = '.cpt')]))))
		{
		index <- grep(colnames(cohort), pattern = '.cpt')
		pointer <- which(is.na(cohort[i,index]))
		cohort[i,index[pointer]] <- F
		}
	}

#5.4 Fix the gender and ethnicity columns - might be unnecessary.
#cohort$gender <- all.hadm$gender[which(all.hadm$hadm_id %in% cohort$hadm_id)]
#cohort$ethnicity <- all.hadm$ethnicity[which(all.hadm$hadm_id %in% cohort$hadm_id)]

#5.5 Save summary information to log file
cat('',
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Total included cases:',sum(combo$category!=0)),
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Total included controls:',sum(combo$category==0)),
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Laboratory test features with at least ',
		signif(100*(lab.param),2),
		'% result rate: ', length(pointer.labs),sep=''),
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Medication features with at least ',
		signif(100*(1 - med.param),2),
		'% prescription rate: ', length(pointer.meds),sep=''),
	file='log.txt',
	sep='\n',
	append=TRUE)
cat(paste('Procedure features with at least ',
		signif(100*(1 - cpt.param),2),
		'% occurrence rate: ', length(pointer.cpt),sep=''),
	file='log.txt',
	sep='\n',
	append=TRUE)

#5.6 Saves the work, ready for Weka?
save(all.hadm,cohort,
	file = paste("temporal.extract.", v, ".first.", interval,".hours.RData",sep=""))

#Inner loop cleanup (Needs to be expanded!)
rm(med.param,cpt.param,lab.param,index,pointer)

###Add a line to the log file with a timestamp at the end of post-processing
cat(paste('Pipeline execution complete at', timestamp()),
    file='log.txt',
    sep='\n',
    append=TRUE)

} #This is where the interval loop ends. Uncomment to activate the loop.

###################
#HISTORY OF CHANGES
#
#Changes in Version 14.3
# 1. Added section 1.5.6 Aggregate ICD9 codes for each patient. Each patient can have multiple ICD9 codes.
# 2. Added section 1.5.7 Merge ICD9 codes with all.hadm
# 3. Added all.ICD9 to Cleanup list on command rm
# 4. Removed random sampling of controls
#
#Changes in Version 14.2
#1. Added Creative Commons Attribution-NonCommercial 4.0 International License information
#2. Added categories to the HAC codes 
#
#Changes in Version 14.1
#1. Add option to include expanded complication list
#
#Changes in Version 14
#1. Added medevents table to the extraction pipeline
#2. Added new medication combination rules
#
#Changes in Version 13
#1. Includes CPT
#2. Includes a lot of new notations
#3. Fold multiple separate FOR loops into one FOR loop with cases and controls
#4. Delineated the key parameters throughout the script
#5. Made the code easier to read by breaking up dense statements across lines
#
#Changes in Version 12
#1. Only allows controls with POE and excludes cases with no POE
#2. Relaxes outlier restriction to 1th/99th percentiles
#
#Changes in Version 11
#1. Restrict cases to central 95% LOS but not by NA
#2. Disassociate case and control offsets
#3. Added ICD-9-CM exclusion to controls
#4. Re-defined aggregate abx columns slightly
#
#Changes in Version 10
#1. Restrict cases to central 95% of all cases per LOS and NA% 
#2. Match controls to cases defined by above
#3. Include forwards and backwards sensitivity analyses
#4. Add special aggregate medication columns: all abx, hi risk abx, ppi/h2
#
#Changes in Version 9
#1. Groups are defined in the cdif.x.R file
#2. Writes to a log file instead of the screen
#3. Censors patients completely (vs. partially) in the sensitivity analysis
#4. Adds route to medication feature
#
#Changes in Version 8
#1. Adds medications
#2. Allows med features that occur in at least 5% of cases
#3. Keeps lab criterion at 50% NA
#4. Adds verbosity
#
#Changes in Version 7
#1. Re-defined min/max/median measures
#2. Fixed gender and ethnicity information
#3. Includes backwards/forwards and sensitivity analysis all in the same file
#
#Changes in Version 6 (a, b)
#1. Changed sensitivity analysis to hourly
#2. A: Forward, B: Backward
#3. Restricts controls stays between 0-100th percentile of cases
#4. Fixed time offset
#
#Changes in Version 5
#1. Includes demographics (age, race, gender, Elixhauser) as features
#2. Retains hadm_id info with controls
#3. Just uses 95% NA criterion
#4. Switches sensitivity analysis to forward time
#
#Changes in Version 4
#1. Calculate Elixhauser comorbidity index for cases
#2. Exclude early cases from controls
#3. Match controls by median NA, as opposed to mean NA
#4. Sensitivity analysis
#5. Restrict controls stays between 25-75th percentile of cases
#
#Changes in Version 3
#1. Simplified code
#2. Restrict controls stays between 0-100th percentile of cases
#3. Added a variable to define time offset
