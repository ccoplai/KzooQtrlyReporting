#Quarterly Analysis, KZOO BSA data
#START TO BUILD IN "AVERAGES" OVER TIME FOR NEXT RUN
#TO SEE HOW MONTHLY AVERAGE COMPARED TO HISTORICAL TRENDS
#OR NEIGHBORHOOD AVERAGE, ETC.

#PARCELS
#assessing parcel data (incomplete - taxable parcels only)
parcels <- read.csv(file = "2016Parcels.csv", head = T, sep = ",")
names(parcels)
#parcel data, all parcels in city
parcels2 <- read.csv(file = "ParcelList_1.csv", head = T, sep = ",")

parcels2 <- read.csv(file = "parcelsCTNeighb.csv", head = T, sep = ",")
names(parcels2)
dim(parcels2)

#replace address ending in "AV" with "AVE" to match BS&A address field

parcels2 <- lapply(parcels2, function(x){
  gsub(" AV", " AVE", x)
})

View(parcels2)


#keep pnum, total acres, ownercity, taxpayercity, propclass, mborsev, mbortax, zoning
#resbyrbuilt and cibyearbuilt
keepcols <- c(1, 2, 8, 9, 15, 16, 18, 19, 20, 21, 22:25, 29:30)
parcels <- parcels[,keepcols]




#complaints 4Q
complaints <- read.csv(file = "ComplaintTrackingntQ12017.csv", head = T, sep = ",")
Joinp <- read.csv(file = "EnforcementListREPORTQ12017.csv", head = T, sep = ",")
names(complaints)
dim(complaints)
names(Joinp)
dim(Joinp)

complaints <- merge(complaints, Joinp, by.x = "Case..", by.y = "Enforcement..", all.x = T)
names(complaints)
keepcols <- c(1:7, 10)
complaints <- complaints[,keepcols]
View(complaints)

#join in parcel data (both assessing - incomplete and all parcel)
#to keep all records
compparcall <- merge(complaints, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x = T, na.rm = T)

#to keep only complaints /enforcement where there is an address match (538 out of 587)
compparc <- merge(complaints, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x.y = T, na.rm = T)
dim(compparc)
names(compparc)
compparc2 <- merge(compparc, parcels, by.x = "PIN", by.y = "Parcels.pnum", all.x = T)
names(compparc2)
dim(compparc2)
View(compparc2)

#remove NAs WAIT ON THIS FOR NOW
compparc2 <- compparc2[!(is.na(compparc2$Date.Closed) | compparc2$Date.Closed == ""), ]
dim(compparc2)




#permits 1Q 2017
permits <- read.csv(file = "PermitListQ12017.csv", head = T, sep = ",")
names(permits)
dim(permits)

#merge all parcels first
#to keep all cases
permparcall <- merge(permits, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x = T, na.rm = T)
dim(permparcall)
#to keep only cases where address match is possible (567 out of 623)
permparc2 <- merge(permits, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x.y = T, na.rm = T)
dim(permparc2)
names(permparc2)

#merge additional parcel data
permparc3 <- merge(permparc2, parcels, by.x = "PIN", by.y = "Parcels.pnum", all.x = T)
dim(permparc3)
names(permparc3)

#Remove NAs for date issued
permparc3 <- permparc3[!(is.na(permparc3$Date.Issued) | permparc3$Date.Issued == ""), ]
dim(permparc3)
View(permparc3)





#ANALYSIS


#PERMITS
names(permparc3)
View(permparc3)

#ownership kzoo & MI
permparc3$OwnerKzoo <- ifelse(permparc3$ParcelMaster.ownercity == "KALAMAZOO", 1, 0)
permparc3$OwnerMI <- ifelse(permparc3$ParcelMaster.ownerstate == "MI", 1, 0)

#aggregate by category
aggbycat <- aggregate(PIN ~ Category, data = permparc3, FUN = length)
View(aggbycat)


#date issued

dateissued <- data.frame(do.call('rbind', strsplit(as.character(permparc3$Date.Issued), '/', fixed = T)))

names(dateissued)[1] <- "issuedM"
names(dateissued)[2] <- "issuedD"
names(dateissued)[3] <- "issuedY"

dateissued$issuedM <- factor(dateissued$issuedM, levels = c(1:12), ordered = T)
dateissued$issuedD <- factor(dateissued$issuedD, levels = c(1:31), ordered = T)
dateissued$issuedY <- factor(dateissued$issuedY, levels = c(2013:2017), ordered = T)

permparc3 <- cbind(permparc3, dateissued)
names(permparc3)
View(permparc3)

#aggregate permits by zoning
aggpermbyzoning <- aggregate(issuedY ~ ParcelMaster.zoning, data = permparc3, FUN = length)
View(aggpermbyzoning)


#Keep only parcels non-res property class (want to analyze business impact)
permparcsub <- subset(permparc3, permparc3$CLASS == "CI" | permparc3$CLASS == "CM" | 
                        permparc3$CLASS == "CV" | permparc3$CLASS == "EX" | permparc3$CLASS == "II" | 
                        permparc3$CLASS == "IV" | permparc3$CLASS == "RF")

View(permparcsub)

#agg by month
aggbymonth <- aggregate(Category ~ issuedM, data = permparcsub, FUN = length)
View(aggbymonth)

#join in x,y coords
xy <- read.csv(file = "parcelsxyQ117.csv", head = T, sep = ",") 

require(plyr)
addxy <- join(permparcsub, xy, by = "PIN", type = "left", match = "all")

write.csv(addxy, "Q12017AllPermits.csv")
#USE ABOVE FOR GRAPHING BY PERMIT



#aggregate permits per parcel
aggpermbyparcel <- aggregate(issuedY ~ PIN, data = permparcsub, FUN = length)
colnames(aggpermbyparcel)[2] <- "Number of Permits"
View(aggpermbyparcel)

aggbyparcelperm <- aggregate(permparcsub$Permit.., list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm)[2] <- "Permit #"
aggbyparcelperm2 <- aggregate(permparcsub$Category, list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm2)[2] <- "Category"
aggbyparcelperm3 <- aggregate(permparcsub$Applicant.Name, list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm3)[2] <- "Applicant"
aggbyparcelperm4 <- aggregate(permparcsub$Date.Issued, list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm4)[2] <- "Date Issued"
aggbyparcelperm5 <- aggregate(permparcsub$Amount.Billed, list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm5)[2] <- "Amount Billed"
aggbyparcelperm6 <- aggregate(permparcsub$description, list(permparcsub$PIN), paste, collapse = ", ")
colnames(aggbyparcelperm6)[2] <- "Description"

#now merge all back together
aggparcperm <- merge(aggbyparcelperm, aggbyparcelperm2, by = "Group.1", all.x.y = T)
aggparcperm <- merge(aggparcperm, aggbyparcelperm3, by = "Group.1", all.x.y = T)
aggparcperm <- merge(aggparcperm, aggbyparcelperm4, by = "Group.1", all.x.y = T)
aggparcperm <- merge(aggparcperm, aggbyparcelperm5, by = "Group.1", all.x.y = T)
aggparcperm <- merge(aggparcperm, aggbyparcelperm6, by = "Group.1", all.x.y = T)
aggparcperm <- merge(aggparcperm, aggpermbyparcel, by.x = "Group.1", by.y = "PIN", all.x.y = T)

#merge back in other parcel data to finalize
aggparcperm <- merge(aggparcperm, parcels2, by.x = "Group.1", by.y = "PIN", all.x = T)
aggparcperm <- merge(aggparcperm, parcels, by.x = "Group.1", by.y = "Parcels.pnum", all.x = T)

View(aggparcperm)
#EXPORT for MAPPING BY PARCEL
write.csv(aggparcperm, "Q12017PermitsF.csv")







names(permparcsub)
#GRAPHING
require(ggplot2)
#give levels to D field for correct ordering
permparcsub$issuedD <- factor(permparcsub$issuedD, levels = c(1:31), ordered = T)

#number of permits by day **
ggplot(data = subset(permparcsub, issuedY != 1997), aes(issuedD)) + geom_bar() + facet_wrap(~ issuedM) +
  labs(x = "Day Issued", y = "# of Permits", title = "Number of Permits Issued - 1st Qtr 2017") + theme(plot.title = element_text(hjust = .5)) +
  scale_x_discrete(breaks = levels(permparcsub$issuedD)[c(T, rep(F, 1))])


#by category
ggplot(permparcsub, aes(issuedM, fill = Category)) + geom_bar() + 
  labs(x = "Year Issued", y = "# of Permits", title = "Number of Permits Issued by Year and Type") + theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete(name = "Category")

#wrapped by month ***
ggplot(permparcsub, aes(x = issuedM)) + geom_bar() + facet_wrap(~ Category, ncol = 5) +
  labs(x = "Month Issued", y = "# of Permits", title = "Number of Permits by Month and Type") +
  theme(plot.title = element_text(hjust = .5))

ggplot(permparcsub, aes(issuedM, fill = Category)) + geom_bar() + 
  labs(x = "Month Issued", y = "# of Permits", title = "Number of Permits Issued by Month and Type") + theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete(name = "Category")


#by neighb
#wrapped by neighb **
ggplot(data = subset(permparcsub, NAME != 'NA'), aes(x = issuedM)) + geom_bar() + facet_wrap(~ NAME, ncol = 4) + 
  labs(x = "Month Issued", y = "# of Permits", title = "# of Permits by Neighborhood") + theme(plot.title = element_text(hjust = .5))

#wrapped by zoning
ggplot(data = subset(permparcsub, ParcelMaster.zoning != 'NA'), aes(x = issuedM)) + geom_bar() + facet_wrap(~ ParcelMaster.zoning, ncol = 4)







names(permparcsub)
View(permparcsub)
#AGGREGATE ANALYSIS
require(sqldf)

#NEIGHB
#pull permit data by neighb
permneighb <- sqldf("select NAME, count(Address) as NumPermits
                    from permparcsub where NAME != 'NA' group by NAME")
permneighb
keeprows <- c(2:18)
permneighb <- permneighb[keeprows,]
View(permneighb)
#pull parcel data by neighb, such as num parcels, 


aggbycat <- aggregate(PIN ~ Category, data = permparcsub, FUN = length)
View(aggbycat)


#TRACT
permtract <- sqldf("select TRACT, count(Address) as NumPermits
                   from permparcsub where TRACT != 'NA' group by TRACT")


View(permtract)

tractdata <- permtract
tractdata <- merge(permtract, comptract, by = "TRACT", all.x.y. = T)
View(tractdata)


#GRAPHING AGGREGATE DATA
ggplot(tractdata, aes(x = ))







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

tracts_kzoo <- merge(tracts_kzoo, permtract, by.x = 'id', by.y = 'TRACT', all.x = T)
tracts_kzoo <- tracts_kzoo[order(tracts_kzoo$order),]
View(tracts_kzoo)

#KZOO BASE
kzoo<-get_map(location=c(left = -85.71350, bottom = 42.22343, right = -85.47947, top = 42.34078))
kzoo<-ggmap(kzoo)
kzoo

#PERMITS
numperm <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumPermits), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q1 2017)')
numperm

