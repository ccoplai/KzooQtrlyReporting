# Quarterly Analytics Reporting
# CP&D

#Using Permits, Inspections, Enforcement/Complaints, and Violation datasets from BS&A

# Quarterly data is comapred to 3-year historical average for comparison

# 4th Q, 2016 compared to 2014-2016 historical data


# Load in parcels data

parcels <- read.csv(file = "parcels_final.csv", head = T, sep = ",")
names(parcels)
# remove unnecessary columns
keepcols <- c(1, 11:13, 20, 25:26, 29:32, 34, 36, 37, 39:43, 47)
parcels <- parcels[, keepcols]

# ** would be nice to have year built, taxable value (over time), etc. in parcel data **


# rename columns
names(parcels)
require(plyr)

parcels <- rename(parcels, c("FIELD2" = "DDA", "NAME" = "Zoning", "NAME_1" = "Neighborhood", "GDS_NAME" = "TRACT_NAME"))
View(parcels)

## ENFORCEMENTS / COMPLAINTS

# Start with enforcements/complaints
# load and merge both

enforcement <- read.csv(file = "EnforcementListREPORT14-17.csv", head = T, sep = ",")
complaints <- read.csv(file = "ComplaintTrackingnt14-17.csv", head = T, sep = ",")

names(enforcement)
names(complaints)
complaints <- merge(enforcement, complaints, by.x = "Enforcement..", by.y = "Case..", all.x.y = T)


# merge with parcel data by address

parcelcomp <- merge(complaints, parcels, by.x.y = "Address", na.rm = T)
View(parcelcomp)


# Developing new variables

# Open Date
require(reshape2)
require(stringr)

opendate <- data.frame(do.call('rbind', strsplit(as.character(parcelcomp$Date.Filed), '/', fixed = T)))

names(opendate)[1] <- "openM"
names(opendate)[2] <- "openD"
names(opendate)[3] <- "openY"

# factor and order for appropriate graphing

opendate$openY <- factor(opendate$openY, levels = c(2014:2017), ordered = T)
opendate$openM <- factor(opendate$openM, levels = c(1:12), ordered = T)
opendate$openD <- factor(opendate$openD, levels = c(1:31), ordered = T)


# join opendate back in
parcelcomp <- cbind(parcelcomp, opendate)

# now close date
# first create data frame where date.closed is not NA (i.e., there is a date.closed value)
closedate <- parcelcomp[!(is.na(parcelcomp$Date.Closed) | parcelcomp$Date.Closed == ""), ]

# keep only enforcement # (for join purposes) and date.closed
names(closedate)
keepcols <- c(2, 6)
closedate <- closedate[, keepcols]


# now separate out close date field into d, m, y
closedate <- data.frame(closedate$Enforcement.., do.call('rbind', strsplit(as.character(closedate$Date.Closed), '/', fixed = T)))

names(closedate)[1] <- "Enforcement.."
names(closedate)[2] <- "closeM"
names(closedate)[3] <- "closeD"
names(closedate)[4] <- "closeY"
names(closedate)

# factor and order for appropriate graphing

closedate$closeY <- factor(closedate$closeY, levels = c(2014:2017), ordered = T)
closedate$closeM <- factor(closedate$closeM, levels = c(1:12), ordered = T)
closedate$closeD <- factor(closedate$closeD, levels = c(1:31), ordered = T)

# join back in
parcelcomp <- merge(parcelcomp, closedate, by.x.y = "Enforcement..", all.x = T)

# number of days open
parcelcomp$daysopen <- as.Date(as.character(parcelcomp$Date.Closed), format = "%m/%d/%Y") - as.Date(as.character(parcelcomp$Date.Filed), format = "%m/%d/%Y")

# for the sake of analysis, change any negative days open to 0.
parcelcomp$daysopen <- ifelse(parcelcomp$daysopen < 0, 0, parcelcomp$daysopen)



View(parcelcomp)


# examine existing categories
aggbycat <- aggregate(Enforcement.. ~ Category, data = parcelcomp, FUN = length)
View(aggbycat)
write.csv(aggbycat, "Q1aggbycat.csv")
# examine status
aggbystatus <- aggregate(Enforcement.. ~ Status.x, data = parcelcomp, FUN = length)
View(aggbystatus)

# examine by inspector
aggbyinspector <- aggregate(Enforcement.. ~ Inspector.x, data = parcelcomp, FUN = length)
View(aggbyinspector)
write.csv(aggbyinspector, "Q1aggbyinspector.csv")
# examine by owner
aggbyowner <- aggregate(Enforcement.. ~ Owner, data = parcelcomp, FUN = length)
View(aggbyowner)

# examine by PIN
aggbyPIN <- aggregate(Enforcement.. ~ PIN, data = parcelcomp, FUN = length)
write.csv(aggbyPIN, "Q1aggbyPIN.csv")
View(aggbyPIN)
View(parcelcomp)

# create Q subset
parcelcomp1Q <- parcelcomp

# select those complaints with issued date of jan - mar, 2017 (1st Q)
parcelcomp1Q <- subset(parcelcomp1Q, parcelcomp1Q$openY == "2017" & parcelcomp1Q$openM <= 3)
View(parcelcomp1Q)

# add dummy variable field to parcelcomp for 1 if Y 1Q and 0 if No

parcelcomp$Q1YN <- ifelse(parcelcomp$openY == "2017" & parcelcomp$openM <= 3, 1, 0)
parcelcomp$Q1YN <- as.factor(parcelcomp$Q1YN)
View(parcelcomp)
write.csv(parcelcomp, "Q1parcelcomp.csv")



# find historical averages for comlpaints by different sub-categories
require(sqldf)
require(ggplot2)

# start with group by MONTH
# historical previous 3 years, 4Q
compmonthHist <- sqldf("select openM, count(PIN) as NumComplaints,
                       sum(daysopen) / count(daysopen) as AvgDaysOpen,
                       count(PIN) / 3 as AvgComplaints
                       from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (1, 2, 3) group by openM")


# most recent quarter
# problem of Avgdaysopen calc even though most permits from month 3, 2017 were still open as of this report
# after further analysis, not a problem, NAs are not included in calculation.
compmonth1Q <- sqldf("select openM, count(PIN) as NumComplaints,
                     sum(daysopen) / count(daysopen) as AvgDaysOpen, count(PIN) / 1 as AvgComplaints
                     from parcelcomp where openY = 2017 AND openM IN (1, 2, 3) group by openM")


# rbind and add dummy var for 4Q
compmonth <- rbind(compmonthHist, compmonth1Q)
compmonth$Q1 <- as.factor(c(rep(0,3), rep(1,3)))
compmonth$Q1 <- as.factor(ifelse(compmonth$Q1 == 1, "2017", "2014-2016"))
View(compmonth)
write.csv(compmonth, "Q1compmonth.csv")

# group by type / CATEGORY of complaints
compcatHist <- sqldf("select Category, count(PIN) as NumComplaints,
                     sum(daysopen) / count(daysopen) as AvgDaysOpen,
                     count(PIN) / 3 as AvgComplaints
                     from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (1, 2, 3) group by Category")


# most recent quarter
compcat1Q <- sqldf("select Category, count(PIN) as NumComplaints,
                   sum(daysopen) / count(daysopen) as AvgDaysOpen,
                   count(PIN) / 1 as AvgComplaints 
                   from parcelcomp where openY = 2017 AND openM IN (1, 2, 3) group by Category")


# rbind and add dummy var for 1Q
compcat <- rbind(compcatHist, compcat1Q)
compcat$Q1 <- as.factor(c(rep(0, 14),rep(1, 10)))
compcat$Q1 <- as.factor(ifelse(compcat$Q1 == 1, "2017", "2014-2016"))
compcat$AvgDaysOpen <- as.numeric(compcat$AvgDaysOpen)
View(compcat)


# number of complaints by month in 17 comapred to historical avg ***
ggplot(compmonth, aes(x = openM, y = AvgComplaints, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Month") + ylab("Average # of Cases") 


# GRAPHING
# number of complaints by category in 2017 Q1 compared to three-year historical avg.***
ggplot(data = subset(compcat, AvgComplaints > 1), aes(x = Category, y = AvgComplaints, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ylab("Average # of Cases")

# daysopen
ggplot(data = subset(compcat, AvgComplaints > 1 & AvgDaysOpen < 300), aes(x = Category, y = AvgDaysOpen, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ylab("Average Days Complaint Stays Open")


# NEIGHBORHOOD
compneighbHist <- sqldf("select Neighborhood, count(PIN) as NumComplaints,
                        sum(daysopen) / count(daysopen) as AvgDaysOpen,
                        count(PIN) / 3 as AvgComplaints
                        from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (1, 2, 3) group by Neighborhood")

dim(compneighbHist)
# most recent quarter
compneighb1Q <- sqldf("select Neighborhood, count(PIN) as NumComplaints,
                      sum(daysopen) / count(daysopen) as AvgDaysOpen,
                      count(PIN) / 1 as AvgComplaints 
                      from parcelcomp where openY = 2017 AND openM IN (1,2,3) group by Neighborhood")
dim(compneighb1Q)

# rbind and add dummy var for 1Q
compneighb <- rbind(compneighbHist, compneighb1Q)
compneighb$Q1 <- as.factor(c(rep(0, 21),rep(1, 19)))
compneighb$Q1 <- as.factor(ifelse(compneighb$Q1 == 1, "2017", "2014-2016"))
View(compneighb)
write.csv(compneighb, "Q1compneighb.csv")

# graphing, avg # per neighborhood
ggplot(data = subset(compneighb, AvgComplaints > 1), aes(x = Neighborhood, y = AvgComplaints, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Cases")

# daysopen
ggplot(data = subset(compneighb, AvgComplaints > 1), aes(x = Neighborhood, y = AvgDaysOpen, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()


# Zoning
compzoningHist <- sqldf("select Zoning, count(PIN) as NumComplaints,
                        sum(daysopen) / count(daysopen) as AvgDaysOpen,
                        count(PIN) / 3 as AvgComplaints
                        from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (1,2,3) group by Zoning")

dim(compzoningHist)
# most recent quarter
compzining1Q <- sqldf("select Zoning, count(PIN) as NumComplaints,
                      sum(daysopen) / count(daysopen) as AvgDaysOpen,
                      count(PIN) / 1 as AvgComplaints 
                      from parcelcomp where openY = 2017 AND openM IN (1,2,3) group by Zoning")
dim(compzining1Q)

# rbind and add dummy var for 1Q
compzoning <- rbind(compzoningHist, compzining1Q)
compzoning$Q1 <- as.factor(c(rep(0, 14),rep(1, 14)))
compzoning$Q1 <- as.factor(ifelse(compzoning$Q1 == 1, "2017", "2014-2016"))
View(compzoning)

# graphing, avg # per zoning district
ggplot(data = subset(compzoning, AvgComplaints > 1), aes(x = Zoning, y = AvgComplaints, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Cases")

# daysopen
ggplot(data = subset(compzoning, AvgComplaints > 1), aes(x = Zoning, y = AvgDaysOpen, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()


# TRACT
comptractHist <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumComplaints,
                       sum(daysopen) / count(daysopen) as AvgDaysOpen,
                       count(PIN) / 3 as AvgComplaints
                       from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (1,2,3) group by TRACT_NAME")

dim(comptractHist)
# most recent quarter
comptract1Q <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumComplaints,
                     sum(daysopen) / count(daysopen) as AvgDaysOpen,
                     count(PIN) / 1 as AvgComplaints 
                     from parcelcomp where daysopen != 'NA' AND openY = 2017 AND openM IN (1,2,3) group by TRACT_NAME")
dim(comptract1Q)

# rbind and add dummy var for 4Q
comptract <- rbind(comptractHist, comptract4Q)
keeprows <- c(2:41)
comptract <- comptract[keeprows,]
comptract$Q1 <- as.factor(c(rep(0, 21),rep(1, 19)))
comptract$Q1 <- as.factor(ifelse(comptract$Q1 == 1, "2017", "2014-2016"))
View(comptract)

# graphing, avg # per tract
ggplot(data = subset(comptract, AvgComplaints > 1), aes(x = TRACT_NAME, y = AvgComplaints, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()

# daysopen
ggplot(data = subset(comptract, AvgComplaints > 1), aes(x = TRACT_NAME, y = AvgDaysOpen, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()




# DETAIL for Q1 ONLY, No COMPARISON to HISTORICAL AVG
# category and month
ggplot(parcelcomp1Q, aes(openM, fill = Category)) + geom_bar()

# category and month by neighborhood ***
ggplot(parcelcomp1Q, aes(openM, fill = Category)) + geom_bar() + 
  facet_wrap(~ Neighborhood, scales = "free_y") + xlab("Month") + ylab("# of Cases")

# zoning and month by neighborhood **
ggplot(parcelcomp1Q, aes(openM, fill = Zoning)) + geom_bar() + 
  facet_wrap(~ Neighborhood, scales = "free_y") + xlab("Month") + ylab("# of Cases")

# category and month by zoning
ggplot(parcelcomp1Q, aes(openM, fill = Category)) + geom_bar() + facet_wrap(~ Zoning, scales = "free_y")

# category and month by Inspector **
ggplot(parcelcomp1Q, aes(openM, fill = Category)) + geom_bar() + 
  facet_wrap(~ Inspector.x, scales = "free_y") + xlab("Month") + ylab("# of Cases")
# neighborhood and month by inspector **
ggplot(parcelcomp1Q, aes(openM, fill = Inspector.x)) + geom_bar() + 
  facet_wrap(~ Neighborhood, scales = "free_y")+ xlab("Month") + ylab("# of Cases")
# zoning and month by inspector
ggplot(parcelcomp1Q, aes(openM, fill = Zoning)) + geom_bar() + facet_wrap(~ Inspector.x, scales = "free_y")

ggplot(parcelcomp1Q, aes(openM, fill = Status.x)) + geom_bar() + facet_wrap(~ Inspector.x, scales = "free_y")



# days open
#**
ggplot(parcelcomp1Q, aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(-1,75) + xlab("Days Open") + ylab("# of Cases")

summary(parcelcomp1Q$daysopen, na.rm = T)

# days open by category **
ggplot(subset(parcelcomp1Q, daysopen > 1), aes(x = Category, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 100) + labs(y = "Days Open", x = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5))

#Days open by category w/ average as density instead of boxplot
ggplot(parcelcomp1Q, aes(x = daysopen, colour = Category)) + 
  geom_density() + facet_wrap(~ Category, scales = "free_y") + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5)) +
  geom_vline(data = compcat4Q, aes(xintercept = AvgDaysOpen, colour = Category), linetype = "dashed", size = 1)



# days open by zoning
ggplot(parcelcomp1Q, aes(x = Zoning, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 75) + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5))

# days open by neighborhood
ggplot(parcelcomp1Q, aes(x = Neighborhood, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 75) + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5))

# days open by neighborhood w/ average as density
ggplot(parcelcomp1Q, aes(x = daysopen, colour = Neighborhood)) + 
  geom_density() + facet_wrap(~ Neighborhood, scales = "free_y") + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5)) +
  geom_vline(data = compneighb1Q, aes(xintercept = AvgDaysOpen, colour = Neighborhood), linetype = "dashed", size = 1)

ggplot(parcelcomp1Q, aes(x = daysopen, colour = Neighborhood)) + 
  geom_histogram() + facet_wrap(~ Neighborhood, scales = "free_y") + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5)) +
  geom_vline(data = compneighb1Q, aes(xintercept = AvgDaysOpen, colour = Neighborhood), linetype = "dashed", size = 1)


# days open by inspector **
ggplot(parcelcomp1Q, aes(x = Inspector.x, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 75) + labs(y = "Days Open", x = "Inspectors", title = "Days Open by Inspector") + theme(plot.title = element_text(hjust = .5))





#Looking ahead -- Historical (2014-2016) Q3, what to expect
# Historical Q3 avg complaints by category
compcatHistQ3 <- sqldf("select Category, count(PIN) as NumComplaints,
                       sum(daysopen) / count(daysopen) as AvgDaysOpen,
                       count(PIN) / 3 as AvgComplaints
                       from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (7, 8, 9) group by Category")

View(compcatHistQ3)

# number of complaints by category in Q2 historical avg **
ggplot(compcatHistQ3, aes(x = Category, y = AvgComplaints)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Complaints Q3 (2014-2016)")

# Historical Q3 by month
compmonthHistQ3 <- sqldf("select openM, count(PIN) as NumComplaints,
                         sum(daysopen) / count(daysopen) as AvgDaysOpen,
                         count(PIN) / 3 as AvgComplaints
                         from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (7, 8, 9) group by openM")

View(compmonthHistQ3)

# number of complaints by month in 17 comapred to historical avg **
ggplot(compmonthHistQ3, aes(x = openM, y = AvgComplaints)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Month") + ylab("Average Complaints Q3 (2014-2016)")

# Historical Q3 by neighborhood
compneighbHistQ3 <- sqldf("select Neighborhood, count(PIN) as NumComplaints,
                          sum(daysopen) / count(daysopen) as AvgDaysOpen,
                          count(PIN) / 3 as AvgComplaints
                          from parcelcomp where daysopen != 'NA' AND openY IN (2014, 2015, 2016) AND openM IN (7,8,9) group by Neighborhood")

View(compneighbHistQ3)

# number of complaints by month in 17 comapred to historical avg **
ggplot(compneighbHistQ3, aes(x = Neighborhood, y = AvgComplaints)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Complaints Q3 (2014-2016)")


# subset Q3 for additional analysis
# category and neighborhood by month
Q3Hist <- subset(parcelcomp, (openY == "2014" | openY == "2015" | openY == "2016") & (openM == 7 | openM == 8 | openM == 9))

# graph by month and category and neighborhood **
ggplot(Q3Hist, aes(openY, fill = Category)) + geom_bar() + 
  facet_wrap(~ Neighborhood, scales = "free_y")  + xlab("Year") + ylab("Enforcement Cases in Q3")

# graph by year and month
ggplot(Q3Hist, aes(openY, fill = openM)) + geom_bar() + facet_wrap(~ openM)
















# PERMITS
# PERMITS merge with parcel data by address

permits <- read.csv(file = "PermitList14-17.csv", head = T, sep = ",")
names(permits)
parcelperm <- merge(permits, parcels, by.x.y = "Address", all.x.y = T)


# Developing new variables

# Date Issued
require(reshape2)
require(stringr)

dateissued <- data.frame(do.call('rbind', strsplit(as.character(parcelperm$Date.Issued), '/', fixed = T)))

names(dateissued)[1] <- "issuedM"
names(dateissued)[2] <- "issuedD"
names(dateissued)[3] <- "issuedY"

# factor and order for appropriate graphing

dateissued$issuedM <- factor(dateissued$issuedM, levels = c(1:12), ordered = T)
dateissued$issuedD <- factor(dateissued$issuedD, levels = c(1:31), ordered = T)
dateissued$issuedY <- factor(dateissued$issuedY, levels = c(2014:2017), ordered = T)

# join date back in
parcelperm <- cbind(parcelperm, dateissued)


# numeric version of amount.billed variable
parcelperm$BilledNum <- sub('.', '', parcelperm$Amount.Billed)
parcelperm$BilledNum <- as.numeric(parcelperm$BilledNum)
View(parcelperm)


# examine existing categories
aggbycat <- aggregate(Permit.. ~ Category, data = parcelperm, FUN = length)
View(aggbycat)
write.csv(aggbycat, "Q1aggbycatP.csv")

# examine applicant
aggbyapplicant <- aggregate(Permit.. ~ Applicant.Name, data = parcelperm, FUN = length)
View(aggbyapplicant)
write.csv(aggbyapplicant, "Q1aggbyapplicantP.csv")

# examine avg billed by category
aggbycatavg <- aggregate(BilledNum ~ Category, data = parcelperm, FUN = mean)
View(aggbycatavg)

# agg by PIN
aggbyPIN <- aggregate(Permit.. ~ PIN, data = parcelperm, FUN = length)
View(aggbyPIN)
write.csv(aggbyPIN, "Q1aggbyPINP.csv")

# new categories to simplify

parcelperm$newcat <- ifelse(parcelperm$Category == "Comm, Multi-Family Alteration", "Commercial",
                            ifelse(parcelperm$Category == "Commercial foundation only", "Commercial",
                                   ifelse(parcelperm$Category == "Commercial, Addition", "Commercial",
                                          ifelse(parcelperm$Category == "Commercial, Fence", "Commercial",
                                                 ifelse(parcelperm$Category == "Commercial, Miscellaneous", "Commercial",
                                                        ifelse(parcelperm$Category == "Commercial, New Structure", "Commercial",
                                                               ifelse(parcelperm$Category == "Commercial, Roofing", "Commercial",
                                                                      ifelse(parcelperm$Category == "Commercial, Siding", "Commercial",
                                                                             ifelse(parcelperm$Category == "Commercial,Interior Alteration", "Commercial",
                                                                                    ifelse(parcelperm$Category == "Demo Commercial Structure", "Commercial",
                                                                                           ifelse(parcelperm$Category == "Sign", "Commercial",
                                                                                                  ifelse(parcelperm$Category == "Demo Res. Other Than a Bld.", "Residential",
                                                                                                         ifelse(parcelperm$Category == "Demo Residential Accessory", "Residential",
                                                                                                                ifelse(parcelperm$Category == "Demo Residential Dwelling", "Residential",
                                                                                                                       ifelse(parcelperm$Category == "Res, Swimming Pool", "Residential",
                                                                                                                              ifelse(parcelperm$Category == "Residential Roof", "Residential",
                                                                                                                                     ifelse(parcelperm$Category == "Residential, Addition", "Residential",
                                                                                                                                            ifelse(parcelperm$Category == "Residential, Deck", "Residential",
                                                                                                                                                   ifelse(parcelperm$Category == "Residential, Fence", "Residential",
                                                                                                                                                          ifelse(parcelperm$Category == "Residential, Garage (attached)", "Residential",
                                                                                                                                                                 ifelse(parcelperm$Category == "Residential, Garage (detached)", "Residential",
                                                                                                                                                                        ifelse(parcelperm$Category == "Residential, Interior Remodel", "Residential",
                                                                                                                                                                               ifelse(parcelperm$Category == "Residential, Miscellaneous", "Residential",
                                                                                                                                                                                      ifelse(parcelperm$Category == "Residential, Mobile Home Set", "Residential",
                                                                                                                                                                                             ifelse(parcelperm$Category == "Residential, Multi-family new", "Residential",
                                                                                                                                                                                                    ifelse(parcelperm$Category == "Residential, New Home", "Residential",
                                                                                                                                                                                                           ifelse(parcelperm$Category == "Residential, New modular home", "Residential",
                                                                                                                                                                                                                  ifelse(parcelperm$Category == "Residential, Pole Barn", "Residential",
                                                                                                                                                                                                                         ifelse(parcelperm$Category == "Residential, Reroof & Residing", "Residential",
                                                                                                                                                                                                                                ifelse(parcelperm$Category == "Residential, Siding", "Residential",
                                                                                                                                                                                                                                       ifelse(parcelperm$Category == "Electrical", "Mech, Elec, Plumb",
                                                                                                                                                                                                                                              ifelse(parcelperm$Category == "Mechanical", "Mech, Elec, Plumb",
                                                                                                                                                                                                                                                     ifelse(parcelperm$Category == "Plumbing", "Mech, Elec, Plumb",
                                                                                                                                                                                                                                                            "Other")))))))))))))))))))))))))))))))))

View(parcelperm)





# create Q subset
parcelperm1Q <- parcelperm

# select those pertmits with issued date of jan-mar, 2017 (1st Q)
parcelperm1Q <- subset(parcelperm1Q, parcelperm1Q$issuedY == "2017" & parcelperm1Q$issuedM <= 3)
View(parcelperm1Q)

# add dummy variable to parcelperm for 1 if Y 4Q and 0 if No
parcelperm$Q1YN <- ifelse(parcelperm$issuedY == "2017" & parcelperm$issuedM <= 3, 1, 0)
parcelperm$Q1YN <- as.factor(parcelperm$Q1YN)

write.csv(parcelperm, "Q1parcelperm.csv")


# agg by new cat
aggbynewcat <- aggregate(Permit.. ~ newcat, data = parcelperm1Q, FUN = length)
aggbynewcat

# find historical averages for comlpaints by different sub-categories
require(sqldf)
require(ggplot2)

# by month
permmonthHist <- sqldf("select issuedM, count(PIN) as NumPermits,
                       count(PIN)/3 as AvgPermits,
                       sum(BilledNum) / 3 as AvgBilledMonth,
                       sum(BilledNum) as TotalBilled
                       from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by issuedM")

permmonth1Q <- sqldf("select issuedM, count(PIN) as NumPermits,
                     count(PIN)/ 1 as AvgPermits,
                     sum(BilledNum) / 1 as AvgBilledMonth,
                     sum(BilledNum) as TotalBilled
                     from parcelperm where PIN != 'NA' AND issuedY = 2017 AND issuedM IN (1,2,3) group by issuedM")

# rbind and add dummy var for 4Q
permmonth <- rbind(permmonthHist, permmonth1Q)
permmonth$Q1 <- as.factor(c(rep(0,3), rep(1,3)))
permmonth$Q1 <- as.factor(ifelse(permmonth$Q1 == 1, "2017", "2014-2016"))
View(permmonth)
write.csv(permmonth, "Q1permmonth.csv")

# number of permits by month in 16 comapred to historical avg **
ggplot(permmonth, aes(x = issuedM, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Month") + ylab("Average # of Permits")
# $ billed total for each month (avg for historical) **
ggplot(permmonth, aes(x = issuedM, y = AvgBilledMonth, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Month") + ylab("Avg. Amount Billed for All Permits")

# by Category (newcat)
permcatHist <- sqldf("select newcat, count(PIN) as NumPermits,
                     count(PIN)/3 as AvgPermits,
                     sum(BilledNum) / count(BilledNum) as AvgBilled,
                     sum(BilledNum)/3 as TotalBilled
                     from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by newcat")

permcath1Q <- sqldf("select newcat, count(PIN) as NumPermits,
                    count(PIN)/ 1 as AvgPermits,
                    sum(BilledNum) / count(BilledNum) as AvgBilled,
                    sum(BilledNum) as TotalBilled
                    from parcelperm where PIN != 'NA' AND issuedY = 2017 AND issuedM IN (1,2,3) group by newcat")

sum(permcath1Q$TotalBilled)
sum(permcatHist$TotalBilled)
# rbind and add dummy var for 4Q
permcat <- rbind(permcatHist, permcath1Q)
permcat$Q1 <- as.factor(c(rep(0,4), rep(1,4)))
permcat$Q1 <- as.factor(ifelse(permcat$Q1 == 1, "2017", "2014-2016"))
View(permcat)

# number of permits by category in 16 comapred to historical avg **
ggplot(permcat, aes(x = newcat, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Broad Category") + ylab("Average # of Permits")
# $ billed total for each category (avg for historical) **
ggplot(permcat, aes(x = newcat, y = TotalBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) +  xlab("Month") + ylab("Avg. Amount Billed for All Permits")
# Average $ billed  for each category (avg for historical)
ggplot(permcat, aes(x = newcat, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())


# by SUBSET each category (e.g., subset only Res, show each permit comparison)
# Res
permcatHistRes <- sqldf("select Category, count(PIN) as NumPermits,
                        count(PIN)/3 as AvgPermits,
                        sum(BilledNum) / count(BilledNum) as AvgBilled,
                        sum(BilledNum)/3 as TotalBilled
                        from parcelperm where newcat = 'Residential' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Category")

permcath1QRes <- sqldf("select Category, count(PIN) as NumPermits,
                       count(PIN)/ 1 as AvgPermits,
                       sum(BilledNum) / count(BilledNum) as AvgBilled,
                       sum(BilledNum) as TotalBilled
                       from parcelperm where newcat = 'Residential' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Category")

dim(permcatHistRes)
dim(permcath1QRes)
# rbind
permcatRes <- rbind(permcatHistRes, permcath1QRes)
permcatRes$Q1 <- as.factor(c(rep(0,11), rep(1,12)))
permcatRes$Q1 <- as.factor(ifelse(permcatRes$Q1 == 1, "2017", "2014-2016"))
View(permcatRes)

# number of permits by category in 17 comapred to historical avg
ggplot(subset(permcatRes, AvgPermits > 0), aes(x = Category, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# Average $ billed  for each category (avg for historical)
ggplot(subset(permcatRes, AvgPermits > 0), aes(x = Category, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip() + ylab("Average $ Billed for Single Permit")

# by subset comm
permcatHistComm <- sqldf("select Category, count(PIN) as NumPermits,
                         count(PIN)/3 as AvgPermits,
                         sum(BilledNum) / count(BilledNum) as AvgBilled,
                         sum(BilledNum)/3 as TotalBilled
                         from parcelperm where newcat = 'Commercial' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Category")

permcath1QComm <- sqldf("select Category, count(PIN) as NumPermits,
                        count(PIN)/ 1 as AvgPermits,
                        sum(BilledNum) / count(BilledNum) as AvgBilled,
                        sum(BilledNum) as TotalBilled
                        from parcelperm where newcat = 'Commercial' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Category")

dim(permcatHistComm)
dim(permcath1QComm)
# rbind
permcatComm <- rbind(permcatHistComm, permcath1QComm)
permcatComm$Q1 <- as.factor(c(rep(0,8), rep(1,7)))
permcatComm$Q1 <- as.factor(ifelse(permcatComm$Q1 == 1, "2017", "2014-2016"))
View(permcatComm)

# number of permits by category in 16 comapred to historical avg
ggplot(subset(permcatComm, AvgPermits > 0), aes(x = Category, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# Average $ billed  for each category (avg for historical)
ggplot(subset(permcatComm, AvgPermits > 0), aes(x = Category, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip() + ylab("Average $ Billed for Single Permit")

# by subset MEP
permcatHistCMEP <- sqldf("select Category, count(PIN) as NumPermits,
                         count(PIN)/3 as AvgPermits,
                         sum(BilledNum) / count(BilledNum) as AvgBilled,
                         sum(BilledNum)/3 as TotalBilled
                         from parcelperm where newcat = 'Mech, Elec, Plumb' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Category")

permcath1QMEP <- sqldf("select Category, count(PIN) as NumPermits,
                       count(PIN)/ 1 as AvgPermits,
                       sum(BilledNum) / count(BilledNum) as AvgBilled,
                       sum(BilledNum) as TotalBilled
                       from parcelperm where newcat = 'Mech, Elec, Plumb' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Category")

# rbind
permcatMEP <- rbind(permcatHistCMEP, permcath1QMEP)
permcatMEP$Q1 <- as.factor(c(rep(0,3), rep(1,3)))
permcatMEP$Q1 <- as.factor(ifelse(permcatMEP$Q1 == 1, "2017", "2014-2016"))
View(permcatMEP)

# number of permits by category in 16 comapred to historical avg
ggplot(subset(permcatMEP, AvgPermits > 0), aes(x = Category, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# Average $ billed  for each category (avg for historical)
ggplot(subset(permcatMEP, AvgPermits > 0), aes(x = Category, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()+ ylab("Average $ Billed for Single Permit")

# by subset Other
permcatHistOther <- sqldf("select Category, count(PIN) as NumPermits,
                          count(PIN)/3 as AvgPermits,
                          sum(BilledNum) / count(BilledNum) as AvgBilled,
                          sum(BilledNum)/3 as TotalBilled
                          from parcelperm where newcat = 'Other' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Category")

permcath1QOther <- sqldf("select Category, count(PIN) as NumPermits,
                         count(PIN)/ 1 as AvgPermits,
                         sum(BilledNum) / count(BilledNum) as AvgBilled,
                         sum(BilledNum) as TotalBilled
                         from parcelperm where newcat = 'Other' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Category")

dim(permcatHistOther)
dim(permcath1QOther)
# rbind
permcatOther <- rbind(permcatHistOther, permcath1QOther)
permcatOther$Q1 <- as.factor(c(rep(0,9), rep(1,7)))
permcatOther$Q1 <- as.factor(ifelse(permcatOther$Q1 == 1, "2017", "2014-2016"))
View(permcatOther)

# number of permits by category in 16 comapred to historical avg
ggplot(subset(permcatOther, AvgPermits > 0), aes(x = Category, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# Average $ billed  for each category (avg for historical)
ggplot(subset(permcatOther, AvgPermits > 0), aes(x = Category, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()+ ylab("Average $ Billed for Single Permit")


# by NEIGHBORHOOD 
permneighbHist <- sqldf("select Neighborhood, count(PIN) as NumPermits,
                        count(PIN)/3 as AvgPermits,
                        sum(BilledNum) / count(BilledNum) as AvgBilled,
                        sum(BilledNum)/3 as TotalBilled
                        from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Neighborhood")

permneighbh1Q <- sqldf("select Neighborhood, count(PIN) as NumPermits,
                       count(PIN)/ 1 as AvgPermits,
                       sum(BilledNum) / count(BilledNum) as AvgBilled,
                       sum(BilledNum) as TotalBilled
                       from parcelperm where PIN != 'NA' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Neighborhood")

# rbind and add dummy var for 4Q
permneighb <- rbind(permneighbHist, permneighbh1Q)
permneighb$Q1 <- as.factor(c(rep(0,22), rep(1,22)))
permneighb$Q1 <- as.factor(ifelse(permneighb$Q1 == 1, "2017", "2014-2016"))
View(permneighb)

write.csv(permneighb, "Q1permneighb.csv")

# number of permits by category in 16 comapred to historical avg **
ggplot(permneighb, aes(x = Neighborhood, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# $ billed total for each category (avg for historical)
ggplot(permneighb, aes(x = Neighborhood, y = TotalBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()
# Average $ billed  for each category (avg for historical)
ggplot(permneighb, aes(x = Neighborhood, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()


# by ZONING 
permzoneHist <- sqldf("select Zoning, count(PIN) as NumPermits,
                      count(PIN)/3 as AvgPermits,
                      sum(BilledNum) / count(BilledNum) as AvgBilled,
                      sum(BilledNum)/3 as TotalBilled
                      from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by Zoning")

permzone1Q <- sqldf("select Zoning, count(PIN) as NumPermits,
                    count(PIN)/ 1 as AvgPermits,
                    sum(BilledNum) / count(BilledNum) as AvgBilled,
                    sum(BilledNum) as TotalBilled
                    from parcelperm where PIN != 'NA' AND issuedY = 2017 AND issuedM IN (1,2,3) group by Zoning")

dim(permzoneHist)
dim(permzone1Q)
# rbind and add dummy var for 4Q
permzone <- rbind(permzoneHist, permzone1Q)
permzone$Q1 <- as.factor(c(rep(0,16), rep(1,14)))
permzone$Q1 <- as.factor(ifelse(permzone$Q1 == 1, "2017", "2014-2016"))
View(permzone)

# number of permits by category in 16 comapred to historical avg **
ggplot(subset(permzone, AvgPermits > 1), aes(x = Zoning, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average # of Permits")
# $ billed total for each category (avg for historical)
ggplot(subset(permzone, AvgPermits > 1), aes(x = Zoning, y = TotalBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()
# Average $ billed  for each category (avg for historical)
ggplot(subset(permzone, AvgPermits > 1), aes(x = Zoning, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()


# by TRACT 
permtractHist <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits,
                       count(PIN)/3 as AvgPermits,
                       sum(BilledNum) / count(BilledNum) as AvgBilled,
                       sum(BilledNum)/3 as TotalBilled
                       from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (1,2,3) group by TRACT_NAME")

permtract1Q <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits,
                     count(PIN)/ 1 as AvgPermits,
                     sum(BilledNum) / count(BilledNum) as AvgBilled,
                     sum(BilledNum) as TotalBilled
                     from parcelperm where PIN != 'NA' AND issuedY = 2017 AND issuedM IN (1,2,3) group by TRACT_NAME")

dim(permtractHist)
dim(permtract1Q)
# rbind and add dummy var for 4Q
permtract <- rbind(permtractHist, permtract1Q)
permtract$Q1 <- as.factor(c(rep(0,21), rep(1,21)))
permtract$Q1 <- as.factor(ifelse(permtract$Q1 == 1, "2017", "2014-2016"))
View(permtract)

# number of permits by tract in 16 comapred to historical avg 
ggplot(subset(permtract, AvgPermits > 1), aes(x = TRACT_NAME, y = AvgPermits, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()
# $ billed total for each tract (avg for historical)
ggplot(subset(permtract, AvgPermits > 1), aes(x = TRACT_NAME, y = TotalBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()
# Average $ billed  for each tract (avg for historical)
ggplot(subset(permtract, AvgPermits > 1), aes(x = TRACT_NAME, y = AvgBilled, fill = Q1)) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()



# DETAIL
#GRAPHING JUST 1Q NO COMPARISON TO HISTORICAL
ggplot(parcelperm1Q, aes(issuedM, fill = newcat)) + geom_bar()

#category and month by neighborhood **
ggplot(parcelperm1Q, aes(issuedM, fill = newcat)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") + xlab("Month") + ylab("# of Permits") + scale_fill_discrete(name = "Broad\nCategory")

# zoning and month by neighborhood **
ggplot(parcelperm1Q, aes(issuedM, fill = Zoning)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y")+ xlab("Month") + ylab("# of Permits") + scale_fill_discrete(name = "Zoning")

# category and month by zoning
ggplot(parcelperm1Q, aes(issuedM, fill = newcat)) + geom_bar() + facet_wrap(~ Zoning, scales = "free_y")

# subset category graphing
#res 
#overall **
ggplot(subset(parcelperm1Q, newcat == "Residential"), aes(issuedM, fill = Category)) + geom_bar() + xlab("Month") + ylab("# of Permits") 
#by neighborhood **
ggplot(subset(parcelperm1Q, newcat == "Residential"), aes(issuedM, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") + xlab("Month") + ylab("# of Permits") 
#by zoning
ggplot(subset(parcelperm1Q, newcat == "Residential"), aes(issuedM, fill = Category)) + geom_bar()+ facet_wrap(~ Zoning, scales = "free_y")

# comm
#overall **
ggplot(subset(parcelperm1Q, newcat == "Commercial"), aes(issuedM, fill = Category)) + geom_bar()+ xlab("Month") + ylab("# of Permits") 
#by neighborhood **
ggplot(subset(parcelperm1Q, newcat == "Commercial"), aes(issuedM, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y")+ xlab("Month") + ylab("# of Permits") 
#by zoning
ggplot(subset(parcelperm1Q, newcat == "Commercial"), aes(issuedM, fill = Category)) + geom_bar()+ facet_wrap(~ Zoning, scales = "free_y")

# MEP
#overall **
ggplot(subset(parcelperm1Q, newcat == "Mech, Elec, Plumb"), aes(issuedM, fill = Category)) + geom_bar()+ xlab("Month") + ylab("# of Permits") 
#by neighborhood **
ggplot(subset(parcelperm1Q, newcat == "Mech, Elec, Plumb"), aes(issuedM, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y")+ xlab("Month") + ylab("# of Permits") 
#by zoning
ggplot(subset(parcelperm1Q, newcat == "Mech, Elec, Plumb"), aes(issuedM, fill = Category)) + geom_bar()+ facet_wrap(~ Zoning, scales = "free_y")

# Other
#overall **
ggplot(subset(parcelperm1Q, newcat == "Other"), aes(issuedM, fill = Category)) + geom_bar()+ xlab("Month") + ylab("# of Permits") 
#by neighborhood **
ggplot(subset(parcelperm1Q, newcat == "Other"), aes(issuedM, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y")+ xlab("Month") + ylab("# of Permits") 
#by zoning
ggplot(subset(parcelperm1Q, newcat == "Other"), aes(issuedM, fill = Category)) + geom_bar()+ facet_wrap(~ Zoning, scales = "free_y")










#Looking ahead -- Historical (2014-2016) Q3, what to expect
# Historical Q3 avg permits by category
permcatHistQ3 <- sqldf("select newcat, Category, count(PIN) as NumPermits,
                       count(PIN)/3 as AvgPermits,
                       sum(BilledNum) / count(BilledNum) as AvgBilled,
                       sum(BilledNum)/3 as TotalBilledQAnnual
                       from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (7,8,9) group by Category")
View(permcatHistQ3)

# number of complaints by category in Q2 historical avg
ggplot(subset(permcatHistQ3, newcat == "Residential"), aes(x = Category, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Permits Q3 (2014-2016)")

ggplot(subset(permcatHistQ3, newcat == "Commercial"), aes(x = Category, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Permits Q3 (2014-2016)")

ggplot(subset(permcatHistQ3, newcat == "Mech, Elec, Plumb"), aes(x = Category, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Permits Q3 (2014-2016)")

ggplot(subset(permcatHistQ3, newcat == "Other"), aes(x = Category, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Permits Q3 (2014-2016)")



ggplot(permcatHistQ3, aes(x = Category, y = TotalBilledQAnnual)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()


# Historical Q3 by month
permmonthHistQ3 <- sqldf("select issuedM, count(PIN) as NumPermits,
                         count(PIN)/3 as AvgPermits,
                         sum(BilledNum) / count(BilledNum) as AvgBilled,
                         sum(BilledNum)/3 as TotalBilledQAnnual
                         from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (7,8,9) group by issuedM")
View(permmonthHistQ3)

# number of complaints by month in 16 comapred to historical avg
ggplot(permmonthHistQ3, aes(x = issuedM, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + xlab("Month") + ylab("Average Permits Q2 (2014-2016)")

ggplot(permmonthHistQ3, aes(x = issuedM, y = TotalBilledQAnnual)) + 
  geom_bar(stat = "identity", position = position_dodge()) 

# Historical Q3 by neighborhood
permneighbHistQ3 <- sqldf("select Neighborhood, count(PIN) as NumPermits,
                          count(PIN)/3 as AvgPermits,
                          sum(BilledNum) / count(BilledNum) as AvgBilled,
                          sum(BilledNum)/3 as TotalBilledQAnnual
                          from parcelperm where PIN != 'NA' AND issuedY IN (2014, 2015, 2016) AND issuedM IN (7,8,9) group by Neighborhood")
View(permneighbHistQ3)

# number of complaints by month in 16 comapred to historical avg
ggplot(permneighbHistQ3, aes(x = Neighborhood, y = AvgPermits)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip() + ylab("Average Permits Q3 (2014-2016)")


# subset Q3 for additional analysis
# category and neighborhood by month
Q3Histperm <- subset(parcelperm, (issuedY == "2014" | issuedY == "2015" | issuedY == "2016") & (issuedM == 7 | issuedM == 8 | issuedM == 9))

# graph by month and category and neighborhood
ggplot(Q3Histperm, aes(issuedY, fill = newcat)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") + xlab("Year") + ylab("Complaints in Q3") + scale_fill_discrete(name = "Broad\nCategory")

# subset and categorize for each newcat by neighborhood
#Residential
ggplot(subset(Q3Histperm, newcat == "Residential"), aes(issuedY, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") 

ggplot(subset(Q3Histperm, newcat == "Residential"), aes(x = Category, fill = issuedM)) + geom_bar() + coord_flip() + ylab("Average Permits Q3 (2014-2016)")

#Commercial
ggplot(subset(Q3Histperm, newcat == "Commercial"), aes(issuedY, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") 

#MEP
ggplot(subset(Q3Histperm, newcat == "Mech, Elec, Plumb"), aes(issuedY, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") 

#Other
ggplot(subset(Q3Histperm, newcat == "Other"), aes(issuedY, fill = Category)) + geom_bar() + facet_wrap(~ Neighborhood, scales = "free_y") 


# graph by year and month
ggplot(Q3Histperm, aes(issuedY, fill = issuedM)) + geom_bar() + facet_wrap(~ issuedM)

















# permit census tract file for mapping
permtract2 <- merge(permtractHist, permtract1Q, by = "TRACT", all.x.y = T)
permtract2$DiffPercperm <- (permtract2$AvgPermits.y - permtract2$AvgPermits.x) / permtract2$AvgPermits.x
View(permtract2)

# complaint census tract file for mapping
comptract2 <- merge(comptractHist, comptract1Q, by = "TRACT", all.x.y = T)
comptract2$DiffPerccomp <- (comptract2$AvgComplaints.y - comptract2$AvgComplaints.x) / comptract2$AvgComplaints.x
View(comptract2)

comppermtract <- merge(permtract2, comptract2, by = "TRACT", all.x.y = T)
View(comppermtract)
# MAPPING


# # of complaints by census tract

# % more or less in Q1 2017 comapred to average historical (red = less, green = more)
# i.e., if there were 200 in Q1 2017, and 100 average historically, this would be dark green,
# compared to if there were 2000 in Q4 2016 and 1500 average, this would be lighter green 


# # of permits by census tract
# % more or less in Q1 2017 comapred to average historical

#MAPPING
require(rgdal)
require(sp)
require(ggplot2)
require(ggmap)
tracts_kzoo <- readOGR(dsn = ".", "Tracts")
hoods_kzoo <- readOGR(dsn = ".", "City_Neighborhoods")

proj4string(tracts_kzoo)
tracts_kzoo = spTransform(tracts_kzoo, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

names(tracts_kzoo)
names(hoods_kzoo)

tracts_kzoo <- fortify(tracts_kzoo, region = "TRCT_KEY")
hoods_kzoo <- fortify(hoods_kzoo, region = "NAME")


hoods_kzoo <- merge(hoods_kzoo, permneighb, by.x = 'id', by.y = 'NAME', all.x = T)
hoods_kzoo <- hoods_kzoo[order(hoods_kzoo$order),]
View(hoods_kzoo)

#join in permit data
tracts_kzoo <- merge(tracts_kzoo, comppermtract, by.x = 'id', by.y = 'TRACT', all.x = T)
tracts_kzoo <- tracts_kzoo[order(tracts_kzoo$order),]
View(tracts_kzoo)


#KZOO BASE
kzoo<-get_map(location=c(left = -85.71350, bottom = 42.22343, right = -85.47947, top = 42.34078))
kzoo<-ggmap(kzoo)
kzoo

#PERMITS
numperm <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumPermits.y), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q1 2017)')
numperm

percchange <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = DiffPercperm), data = tracts_kzoo)+ geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='% Change in Permits\nAvg Q1 2014-2016\nto Q1 2017')
percchange

#COMPLAINTS
numcomp <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumComplaints.y), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Complaints (Q1 2017)')
numcomp

percchange <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = DiffPerccomp), data = tracts_kzoo)+ geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='% Change in Complaints\nAvg Q1 2014-2016\nto Q1 2017')
percchange






















# CREATING new Variables


#predicting number of permits in a given tract
permtract16 <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits16
                     from parcelperm where PIN != 'NA' AND issuedY = 2016 AND issuedM IN (10, 11, 12) group by TRACT_NAME")

permtract15 <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits15
                     from parcelperm where PIN != 'NA' AND issuedY = 2015 AND issuedM IN (10, 11, 12) group by TRACT_NAME")
permtract14 <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits14
                     from parcelperm where PIN != 'NA' AND issuedY = 2014 AND issuedM IN (10, 11, 12) group by TRACT_NAME")
permtract13 <- sqldf("select TRACT, TRACT_NAME, count(PIN) as NumPermits13
                     from parcelperm where PIN != 'NA' AND issuedY = 2013 AND issuedM IN (10, 11, 12) group by TRACT_NAME")


permtract <- merge(permtract16, permtract15, by = "TRACT", all.x.y = T)
permtract <- merge(permtract, permtract14, by = "TRACT", all.x = T)
permtract <- merge(permtract, permtract13, by = "TRACT", all.x = T)
names(permtract)
keepcols <- c(1, 2, 3, 5, 7, 9)
permtract <- permtract[,keepcols]
View(permtract)

# na to 0
permtract[is.na(permtract)] <- 0

# put together test / train datasets
permtractx <- as.matrix(permtract[,4:6])
permtracty <- permtract$NumPermits16

trainx <- permtractx[1:10,]
testx <- permtractx[11:21,]
trainy <- permtracty[1:10]
testy <- permtracty[11:21]

# regression as baseline
lmout <- lm(trainy ~ trainx)
summary(lmout)
# see how regression does in predicting 2016 values
yhat.r <- cbind(1, testx) %*% lmout$coefficients
yhat.r

mse.reg <- sum((testy - yhat.r)^2) / nrow(testx)
mse.reg


# now use machine learning
lambdalevels <- 10^seq(7, -2, length = 100)
require(glmnet)
cv.lasso.mod = cv.glmnet(trainx, trainy, alpha = 1, lambda = lambdalevels)
yhat.1 <- predict(cv.lasso.mod$glmnet.fit, s = cv.lasso.mod$lambda.min, newx = testx)
yhat.1

mse.las <- sum((testy - yhat.1)^2) / nrow(testx)
mse.las

plot(yhat.1, testy)

diff <- yhat.1 - testy
diff
mean(abs(diff))

tss <- sum((testy - mean(trainy))^2)
sse.reg <- sum((testy - yhat.r)^2)
sse.las <- sum((testy - yhat.1)^2)
r2.r <- (tss - sse.reg) / tss
r2.1 <- (tss - sse.las) / tss
r2.r
r2.1



#COMPARE BY TESTING ON 3-YEAR HISTORICAL AVG
View(permtractHist)
View(permtract4Q)

permtract2 <- merge(permtractHist, permtract4Q, by = "TRACT", all.y = T)
names(permtract2)
keepcols <- c(1:2, 4, 8)
permtract2 <- permtract2[,keepcols]
View(permtract2)


cor(permtract2$AvgPermits.x, permtract2$NumPermits.y)
plot(permtract2$AvgPermits.x, permtract2$NumPermits.y)

diff <- permtract2$AvgPermits.x - permtract2$NumPermits.y
mean(abs(diff))


# try predicting by category

permcat13 <- sqldf("select Category, count(PIN) as NumPermits
                   from parcelperm where issuedY == 2013 AND issuedM IN (10, 11, 12) group by Category")
permcat14 <- sqldf("select Category, count(PIN) as NumPermits
                   from parcelperm where issuedY == 2013 AND issuedM IN (10, 11, 12) group by Category")
permcat15 <- sqldf("select Category, count(PIN) as NumPermits
                   from parcelperm where issuedY == 2013 AND issuedM IN (10, 11, 12) group by Category")
permcat16 <- sqldf("select Category, count(PIN) as NumPermits
                   from parcelperm where issuedY == 2013 AND issuedM IN (10, 11, 12) group by Category")





















# assessing data
assess16 <- read.csv(file = "2016Assess.csv", head = T, sep = ",")
keepcols <- c(1:3, 5:7, 9, 11:13, 16:18, 21:22)
assess16 <- assess16[, keepcols]
nam <- names(assess16)
names(assess16) <- str_c(nam, '16')

assess16 <- merge(parcels, assess16, by.x = "PIN", by.y = "PIN16", all.y = T)
View(assess16)

NeighbTV2 <- sqldf("select NAME, count(TVpAcre11) as NumParcels,
                   sum(TVpAcre11)/count(TVpAcre11) as AvgTVpAcre11,
                   sum(TVperAcre)/count(TVperAcre) as AvgTVpAcre16
                   from parcels1116 where NAME != 'NA' group by NAME")






# ASSESSING DATA TIE-IN

#load in assessing data from 2011-2016. Keep only most recent 2016 data, keep TV/SEV for all years
assess11 <- read.csv(file = "2011Assess.csv", head = T, sep = ",")
names(assess11)
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess11 <- assess11[, keepcols]

require(stringr)
nam <- names(assess11)
names(assess11) <- str_c(nam, '11')

assess12 <- read.csv(file = "2012Assess.csv", head = T, sep = ",")
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess12 <- assess12[, keepcols]
nam <- names(assess12)
names(assess12) <- str_c(nam, '12')

assess13 <- read.csv(file = "2013Assess.csv", head = T, sep = ",")
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess13 <- assess13[, keepcols]
nam <- names(assess13)
names(assess13) <- str_c(nam, '13')

assess14 <- read.csv(file = "2014Assess.csv", head = T, sep = ",")
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess14 <- assess14[, keepcols]
nam <- names(assess14)
names(assess14) <- str_c(nam, '14')

assess15 <- read.csv(file = "2015Assess.csv", head = T, sep = ",")
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess15 <- assess15[, keepcols]
nam <- names(assess15)
names(assess15) <- str_c(nam, '15')

assess16 <- read.csv(file = "2016Assess.csv", head = T, sep = ",")
keepcols <- c(1, 3, 5:7, 9, 11:13, 16:18, 21:22)
assess16 <- assess16[, keepcols]
nam <- names(assess16)
names(assess16) <- str_c(nam, '16')

# merge into longitudinal file
assess <- merge(assess11, assess12, by.x = "PIN11", by.y = "PIN12", all.y = T)
assess <- merge(assess, assess13, by.x = "PIN11", by.y = "PIN13", all.y = T)
assess <- merge(assess, assess14, by.x = "PIN11", by.y = "PIN14", all.y = T)
assess <- merge(assess, assess15, by.x = "PIN11", by.y = "PIN15", all.y = T)
assess <- merge(assess, assess16, by.x = "PIN11", by.y = "PIN16", all.y = T)

# calc each year change in TV and % change
assess$TVChange11_12 <- assess$TV201112 - assess$TV201111
assess$TVChange12_13 <- assess$TV201113 - assess$TV201112
assess$TVChange13_14 <- assess$TV201114 - assess$TV201113
assess$TVChange14_15 <- assess$TV201115 - assess$TV201114
assess$TVChange15_16 <- assess$TV201116 - assess$TV201115

assess$TVPercChange11_12 <- assess$TVChange11_12 / assess$TV201111
assess$TVPercChange12_13 <- assess$TVChange12_13 / assess$TV201112
assess$TVPercChange13_14 <- assess$TVChange13_14 / assess$TV201113
assess$TVPercChange14_15 <- assess$TVChange14_15 / assess$TV201114
assess$TVPercChange15_16 <- assess$TVChange15_16 / assess$TV201115

class(assess$TVPercChange11_12)

View(assess)
# create combined assessing / parcel data file
assess_parc <- merge(parcels, assess, by.x = "PIN", by.y = "PIN11", all.x = T)


# create combined assessing / parcel / and 2013-2016 permits data file
assess_parc_perm <- merge(parcelperm, assess, by.x = "PIN", by.y = "PIN11", all.x.y = T)
View(assess_parc_perm)

ggplot(assess_parc_perm, aes(x = newcat, y = TVPercChange15_16)) + geom_boxplot()
ggplot(assess_parc_perm, aes(x = TVPercChange15_16)) + geom_histogram(binwidth = .5) + facet_wrap(~ Neighborhood)

ggplot(subset(assess_parc_perm, newcat == "Residential"), aes(Category, fill = TVChange15_16)) + geom_bar()




meanTV <- aggregate(TVChange15_16 ~ Category, data = assess_parc_perm, FUN = mean, na.rm = T)
View(meanTV)







# then look at historical 2nd Q to give predictions for upcoming
# could look at Q1 results / use previous to predict and compare success

# merge in 2011-2016 assessing data and look for correlation/regress to see 
# permit type X predicts growth in TV, high share of neighborhood violations of type X 
# predicts low TV, etc., could build in Census Data by Tract as well





