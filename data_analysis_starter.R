library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)

setwd("~/Desktop/SWB/project_231")

survey2019 <- read_excel("2019 Members survey full data.xlsx")

survey2019 = as.data.frame(survey2019)[2:dim(survey2019)[1],]

survey2019colnames <- read.csv("~/Desktop/SWB/project_231/2019 Column Rename.csv")

colnames(survey2019) = as.vector(as.matrix(survey2019colnames["clean_column"]))


#### Pivoting example, combining update preference columns into single column

updatePrefColumns = str_detect(colnames(survey2019), "^Q06")

coalescedUpdatePref = do.call(coalesce, survey2019[updatePrefColumns])

table(coalescedUpdatePref)


survey2023 <- read_excel("2023 Members survey full download.xlsx")

survey2023colnames <- read.csv("~/Desktop/SWB/project_231/2023 Column Rename.csv")

colnames(survey2023) = as.vector(as.matrix(survey2023colnames["clean_column"]))


#### Pivoting example, combining revenue columns into single column

revenueColumns = str_detect(colnames(survey2023), "^Q08")

coalescedRev = do.call(coalesce, survey2023[revenueColumns])

table(coalescedRev)




### Funding source
### 2019: Q11, source type ; Q12: Donor name

### 2023: Q10, Funding source types; Q11, Donor name

# How have the primary funding sources for organizations evolved between the two surveys?

uniformDonorMapping = c("institutional donors"="institutional donors",
                        "insitutional donors" ="institutional donors",
                        "sub-grants from ngos or un agencies"="sub-grants from NGOs or UN agencies",
                        "sub-grants from other ingos or un agencies"="sub-grants from NGOs or UN agencies",
                        "foundations"="foundations",
                        "membership fees"="membership fees",
                        "funds (hif, start etc.)"="funds (hif, start etc.)",
                        "funds (start, hif,...)"="funds (hif, start etc.)",
                        "private donors"="private donors",
                        "paid for services"="commercial revenue",
                        "other"="other",
                        "commercial revenue"="commercial revenue")

fundSourceCol2019 = str_detect(colnames(survey2019), "^Q11")

fundSourceDat2019 = survey2019[,fundSourceCol2019]

### Removing open response column
fundSourceDat2019 = fundSourceDat2019[, 1:(dim(fundSourceDat2019)[2]-1)]


fundSourceCol2023 = str_detect(colnames(survey2023), "^Q10")

foundSourceDat2023 = survey2023[,fundSourceCol2023]

### Removing open response column
foundSourceDat2023 = foundSourceDat2023[, 1:(dim(foundSourceDat2023)[2]-1)]


sourceNames2019 = as.vector(tolower(apply(fundSourceDat2019, MARGIN=2, 
                                          FUN=function(x) unique(x[!is.na(x)]))))

numSource2019 = as.vector(apply(fundSourceDat2019, MARGIN=2, FUN=table))

names(numSource2019) = uniformDonorMapping[sourceNames2019]
 
propSource2019 = numSource2019/dim(fundSourceDat2019)[1]

propSource2019 = propSource2019[order(names(propSource2019))]




sourceNames2023 = as.vector(tolower(apply(foundSourceDat2023, MARGIN=2, 
                                          FUN=function(x) unique(x[!is.na(x)]))))

numSource2023 = as.vector(apply(foundSourceDat2023, MARGIN=2, FUN=table))

names(numSource2023) = uniformDonorMapping[sourceNames2023]

numSource2023["private_donors"] = 0

propSource2023 = numSource2023/dim(foundSourceDat2023)[1]

propSource2023 = propSource2023[order(names(propSource2023))]


fundSourceDat = cbind.data.frame(propSource2019, propSource2023)

fundSourceDat = cbind(rownames(fundSourceDat), data.frame(fundSourceDat, row.names=NULL))

colnames(fundSourceDat)[1] = "source"

fundSourceDatLong = fundSourceDat %>% pivot_longer(cols=colnames(.)[2:dim(.)[2]],
                               names_to="year", values_to="proportion")

fundSourceDatLong$year = as.numeric(str_extract(fundSourceDatLong$year, "[0-9]{4}$"))


ggplot(fundSourceDatLong, aes(x=year, y=100*proportion, 
                              col=source))+geom_line(lwd=1.5) + 
  theme_bw(base_size=16) + xlab("Year") +ylab("Proportion (%)") + 
  scale_x_continuous(breaks=c(2019, 2023)) + 
  ggtitle("Funding Source, Over time") + 
   scale_color_discrete(name = "Funding Source Type") + 
  theme(legend.text=element_text(size=rel(0.75)),
        plot.title = element_text(hjust = 0.5))


# Have there been significant shifts in the proportions of funding from government grants, private donors, 
# corporate sponsorships, or other sources?

# Is there any correlation between the evolution of turnover and sources of funding?






### Member Typology
### 2019: Q8 (legal status); 2023 Q07 (legal status)
legalStatusCol2019 = str_detect(colnames(survey2019), "^Q08")

legalStatusDat2019 = survey2019[, legalStatusCol2019]

### Removing open response column
legalStatusDat2019 = legalStatusDat2019[, 1:(dim(legalStatusDat2019)[2]-1)]

legalStatusDat2019 = tolower(legalStatusDat2019)

numLegalStatus2019 = table(legalStatusDat2019)


foundationCountData = t(cbind.data.frame(
  c(2017, dim(survey2019)[1]),
  c(2018, dim(survey2019)[1]),
  c(2019, dim(survey2019)[1]), 
                                       c(2023, dim(survey2023)[1])))

rownames(foundationCountData) = NULL

foundationCountData= data.frame(foundationCountData)

colnames(foundationCountData) = c("year", "num_orgs")












legalStatusCol2023 = str_detect(colnames(survey2023), "^Q07")

legalStatusDat2023 = survey2023[, legalStatusCol2023]

### Removing open response column
legalStatusDat2023 = legalStatusDat2023[, 1:(dim(legalStatusDat2023)[2]-1)]


legalStatus2023 = as.vector(tolower(apply(legalStatusDat2023, MARGIN=2, 
                                          FUN=function(x) unique(x[!is.na(x)]))))

numLegalStatus2023 = as.vector(apply(legalStatusDat2023, MARGIN=2, FUN=table))

names(numLegalStatus2023) = legalStatus2023




### Note: map response -> other
### Both "non governmental organisations" and "charity" map to 
### "independent non-governmental organization (e.g. association, 501.c, registered charity...)"
statuses2019 = c( "foundation", 
                  "non governmental organisations",
                  "incorporated or hosted agency (ngo status)", 
                  "other (please specify)",
                  "social enterprise", 
                  "charity",
                  "response",
                  "non governmental organisations")


statuses2023 = c("foundation", 
                 "independent non-governmental organization (e.g. association, 501.c, registered charity...)",
                 "hosted project (e.g. in an ngo or another entity)",
                 "other",
                 "social enterprise (e.g. b-corp, co-op, cic...)",
                 "independent non-governmental organization (e.g. association, 501.c, registered charity...)",
                 "other",
                 "commercial entity")

uniformMapping = tolower(c("foundation"="foundation",
                   "independent non-governmental organization (e.g. association, 501.c, registered charity...)"=
                     "NGO",
                   "non governmental organisations"="NGO",
                   "incorporated or hosted agency (ngo status)"="NGO hosted project",
                   "hosted project (e.g. in an ngo or another entity)"="NGO hosted project",
                   "other (please specify)"="other",
                   "other"="other",
                   "social enterprise (e.g. b-corp, co-op, cic...)"="social enterprise",
                   "social enterprise"="social enterprise",
                   "charity"="NGO",
                   "commercial entity"="commercial entity"))


for (i in 1:length(numLegalStatus2019)){
  
  
  names(numLegalStatus2019)[i] = uniformMapping[i]
  
}



for (i in 1:length(numLegalStatus2023)){
  
  
  names(numLegalStatus2023)[i] = uniformMapping[i]
  
}
  

propLegalStatus2019 = numLegalStatus2019/dim(survey2019)[1]
  
propLegalStatus2023 = numLegalStatus2023/dim(survey2023)[1]



legalStatusDat = cbind.data.frame(numLegalStatus2019, numLegalStatus2023)

colnames(legalStatusDat) = c("status", "propLegalStatus2019", "propLegalStatus2023")

legalStatusDatLong = legalStatusDat %>% pivot_longer(cols=colnames(.)[2:dim(.)[2]],
                                                   names_to="year", values_to="count")

legalStatusDatLong$year = as.numeric(str_extract(legalStatusDatLong$year, "[0-9]{4}$"))

legalStatusDatLong = legalStatusDatLong %>% group_by(status, year) %>% summarize(total = sum(count)) %>%
 ungroup()

legalStatusDatLong = legalStatusDatLong %>% inner_join(foundationCountData)

legalStatusDatLong$proportion = legalStatusDatLong$total/legalStatusDatLong$num_orgs




ggplot(legalStatusDatLong, aes(x=year, y=proportion, col=status))+geom_line(lwd=1.5) + 
  theme_bw(base_size=16) + xlab("Year") +ylab("Proportion (%)") + 
  scale_x_continuous(breaks=c(2019, 2023)) + 
  ggtitle("Legal Status, Over time") + 
  scale_color_discrete(name = "Status Type") + 
  theme(legend.text=element_text(size=rel(0.75)),
        plot.title = element_text(hjust = 0.5))








### Number of employees
### 2019 was a binned response (Q13), 2023 was an open response (Q12)
### Convert 2023 data to 2019 bins
paidEmployCol2019 = str_detect(colnames(survey2019), "^Q13")

paidEmployDat2019 = survey2019[, paidEmployCol2019]

paidEmployDat2019 = str_replace_all(paidEmployDat2019, " ","")

paidEmployCount2019 = table(paidEmployDat2019)


paidEmployCol2023 = str_detect(colnames(survey2023), "^Q12")

paidEmployDat2023 = survey2023[, paidEmployCol2023]

paidEmployDat2023 = as.numeric(as.matrix(paidEmployDat2023))

paidEmployDat2023[c(2, 26, 48)]= c(20, 129, 1000)

bins = c("0-5", "6-10", "11-20", "21-50", "51-100", "101-200", "201+")

employeeBinIndexes = cut(paidEmployDat2023, c(0, 6, 11, 21, 51, 101, 201, 1000000), include.lowest=TRUE,
    right=FALSE, labels=FALSE)

paidEmployDat2023 = bins[employeeBinIndexes]


paidEmployCount2023 = table(paidEmployDat2023)

### GNC Technical Alliance (row 2), Technical Support Team: 20

# CLEAR Global alliance (row 26): 129

### Internews (row 48): 1000

### Bins: 0-5, 6-10, 11-20, 21-50, 51-100, 101-200, 201+
paidEmployDat = cbind.data.frame(names(paidEmployCount2019), as.numeric(unname(paidEmployCount2019)), 
                                 as.numeric(unname(paidEmployCount2023)))



colnames(paidEmployDat) = c("bin", "paidEmployCount2019", "paidEmployCount2023")

paidEmployDatLong = paidEmployDat %>% pivot_longer(cols=colnames(.)[2:dim(.)[2]],
                                                     names_to="year", values_to="count")

paidEmployDatLong$year = as.numeric(str_extract(paidEmployDatLong$year, "[0-9]{4}$"))

paidEmployDatLong = paidEmployDatLong %>% inner_join(foundationCountData, by="year")

paidEmployDatLong$proportion = paidEmployDatLong$count/paidEmployDatLong$num_orgs

paidEmployDatLong$bin=factor(paidEmployDatLong$bin, levels=bins)



ggplot(paidEmployDatLong, aes(x=bin, y=100*proportion, col=factor(year), group=factor(year)))+geom_line(lwd=1.5) + 
  theme_bw(base_size=16) + xlab("Year") +ylab("Proportion (%)") + 
  ggtitle("Paid Employee Prevalence, Over time") + 
  scale_color_discrete(name = "Year") + 
  theme(legend.text=element_text(size=rel(0.75)),
        plot.title = element_text(hjust = 0.5))











### Geographic locations

### 2019: Q2

locCol2019 = str_detect(colnames(survey2019), "^Q02")

locDat2019 = survey2019[, locCol2019]

simpleMapping2019 = c("Geneva, Switzerland"="Switzerland", "Geneva"="Switzerland",
                  "Copenhagen, Denmark"="Denmark", "Globally, Geneva"="Global",
                  "New York, USA"="USA", "UK"="United Kingdom",
                  "Switzeralnd"="Switzerland", "Oxford, UK"="United Kingdom",
                  "Melbourne, Australia"="Australia",
                  "Vienna, Austria" = "Austria", "United States"="USA",
                  "Berlin, Germany"="Germany",
                  "United States of America"="USA",
                  "U.K."="United Kingdom", "Madrid, Spain"="Spain",
                  "Chimmels, Park Road, Dartington Hall, Totnes, Devon, UK"="United Kingdom",
                  "x"="other", "Washington, DC, United States"="USA",
"We are registered in Switzerland but we do not have an office. We are a virtual organization with collaborators in many different countries who all work from home."="Global",
"England and Wales"="United Kingdom", "DC, London, Paris"="United Kingdom",
"London, UK"="United Kingdom", "Global Network registered in UK"="Global",
"Washington DC / Marseille, France"="France",
"Spain (me), US (the consortium lead agency) and globally (the team)"="Global",
"London"="United Kingdom",
"Global with Secretariat hosted in NY, Geneva, DC, Norway, Paris, US cities, Chile, London, Portugal, France."="Global")


### 2023: Q2

locCol2023 = str_detect(colnames(survey2023), "^Q02")

locDat2023 = survey2023[, locCol2023]

simpleMapping2023 = c("Farringdon, United Kingdom"="United Kingdom", "Berlin, Germany"="Germany",
                      "London and Madrid"="United Kingdom", "We are a globally dispersed team."="Global",
                      "Geneva, Switzerland"="Switzerland", "Geneva"="Switzerland",
                      "Chilmark, UK"="United Kingdom", "London, UK"="United Kingdom", "UK"="United Kingdom","Marseille"="France",
                      "Copenhagen"="Denmark", "Vienna"="Austria", 
                      "Melbourne, Australia"="Australia", "Corsier-sur-Vevey"="Switzerland",
                      "Oxfordshire, UK" ="United Kingdom", 
                      "Lyon, France"="France", "geneva"="Switzerland", "Chicago, IL, USA"="USA",
                      "Alexandria, VA, USA (American Statistical Assocation)"="USA",
                      "Lausanne, Switzerland"="Switzerland",
                      "None, but hosted by Action Against Hunger Canada"="Canada",
                      "London UK"="United Kingdom",
                      "Birmingham, United Kingdom"="United Kingdom",
                      "Multiple locations: US, Fiji, Australia, Turkiye, etc."="Global",
                      "London"="United Kingdom", "Madrid, Spain"="Spain",
                      "Chancery Lane, Central London, UK"="United Kingdom",
                      "Nowhere: dispersed organization; registered in the US"="Global",
                      "United States"="USA", "Virginia, USA"="USA",
                      "Madrid"="Spain",
                      "Arcata, California, USA & London, UK" ="United Kingdom",
                      "Devon, UK"="United Kingdom", "France and Spain"="France",
                      "Director position is based in Geneva"="Switzerland")



remapped2019 = c()

neededStuff2019 = as.vector(as.matrix(locDat2019))

for (i in neededStuff2019){
  
  if (any(i == names(simpleMapping2019))){
    
    remapped2019 = c(remapped2019, unname(simpleMapping2019[i]))
    
  }else{
    
    
    remapped2019 = c(remapped2019, i)
  }
  
}









remapped2023 = c()

neededStuff2023 = as.vector(as.matrix(locDat2023[,1]))

for (i in neededStuff2023){
  
  if (any(i == names(simpleMapping2023))){
    
    remapped2023 = c(remapped2023, unname(simpleMapping2023[i]))
    
  }else{
    
    
    remapped2023 = c(remapped2023, i)
  }
  
}

table(remapped2019)

### Proportion of orgs that identify with global north
(dim(survey2019)[1]-3)/dim(survey2019)[1]

(dim(survey2023)[1]-3)/dim(survey2023)[1]


















### Annual Budget

### 2019: Q9 (2017) Q10 (2018)
### Note: BUDGET was used

revCols2017 = str_detect(colnames(survey2019), "^Q09")

revDat2017 = survey2019[, revCols2017]


revCols2018 = str_detect(colnames(survey2019), "^Q10")

revDat2018 = survey2019[, revCols2018]





### 2023: Q8
### Note: REVENUE was used
revCols2023 = str_detect(colnames(survey2023), "^Q08")

revDat2023 = survey2023[, revCols2023]

colnames(revDat2023)

revVec2023 = unname(unlist(apply(revDat2023, MARGIN=1, FUN=function(x) x[!is.na(x)])))

revVec2023 = str_replace_all(revVec2023, "Â£|â€“|,", replace="")

revVec2023 = str_replace_all(revVec2023, "5 million", replace="5000000")

revVec2023 = str_replace_all(revVec2023, "  | - ", replace="-")

revMap = c("Less than £100,000"="0-500000", 
           "£100,001 - £500,000"="0-500000",
           "0-200000"="0-500000",
           "200001-500000"="0-500000",
           "£500,001 - £1,000,000"="500001-2000000",
           "£1,000,001 to £2,000,000" = "500001-2000000",
           "500001-2000000"="500001-2000000",
           "£2,000,001 to £5,000,000"="2000001-5000000",
           "2000001-5000000"="2000001-5000000",
           "£5,000,001 +"="5000001+",
           "5000001-10000000"="5000001+",
           "10000000+"="5000001+")


rev2017Recode = unname(revMap[revDat2017])

rev2018Recode = unname(revMap[revDat2018])

rev2023Recode = unname(revMap[revVec2023])

cbind(unname(revVec2023), unname(revMap[revVec2023]))

table(rev2017Recode)

table(rev2018Recode)

table(rev2023Recode)




revDat = cbind.data.frame(names(table(rev2017Recode)), 
                          as.numeric(unname(table(rev2017Recode))),
                                 as.numeric(unname(table(rev2018Recode))), 
                                 as.numeric(unname(table(rev2023Recode))))



colnames(revDat) = c("bin", "paidEmployCount2017",
                            "paidEmployCount2018", "paidEmployCount2023")

revDatLong = revDat %>% pivot_longer(cols=colnames(.)[2:dim(.)[2]],
                                                   names_to="year", values_to="count")

revDatLong$year = as.numeric(str_extract(revDatLong$year, "[0-9]{4}$"))

revDatLong =revDatLong %>% inner_join(foundationCountData, by="year")

revDatLong$proportion = revDatLong$count/revDatLong$num_orgs

revDatLong$bin=factor(revDatLong$bin, levels=
                        c("0-500000", "500001-2000000",
                          "2000001-5000000", "5000001+"))



ggplot(revDatLong, aes(x=bin, y=proportion, col=factor(year), group=factor(year)))+geom_line(lwd=1.5) + 
  theme_bw(base_size=16) + xlab("Year") +ylab("Proportion (%)") + 
  ggtitle("Revenue of Organizations, Over time") + 
  scale_color_discrete(name = "Year") + 
  theme(legend.text=element_text(size=rel(0.75)),
        plot.title = element_text(hjust = 0.5))





















### 2019: Formal collab, Q25; Informal Collab: Q26
### 2023: SQ03
formalCollabCols2019 = str_detect(colnames(survey2019), "^Q25")

formalCollab2019 = survey2019[, formalCollabCols2019]

formalCollab2019 = formalCollab2019[, 1:(dim(formalCollab2019)[2]-1)]

formalAgreementCounts2019 = apply(formalCollab2019, MARGIN=1, FUN= function(x) sum(!is.na(x)))



informalCollabCols2019 = str_detect(colnames(survey2019), "^Q26")

informalCollab2019 = survey2019[, informalCollabCols2019]

informalCollab2019 = informalCollab2019[, 1:(dim(informalCollab2019)[2]-1)]

informalAgreementCounts2019 = apply(informalCollab2019, MARGIN=1, FUN= function(x) sum(!is.na(x)))

collabProps2019 = c(sum(formalAgreementCounts2019 >0) / length(formalAgreementCounts2019 ),
  sum(informalAgreementCounts2019 >0) / length(informalAgreementCounts2019 ))

names(collabProps2019 ) = c("formal", "informal")




collabCols2023 = str_detect(colnames(survey2023), "^SQ03")

formalCollab2023 = survey2023[, collabCols2023]

formalCollab2023 = formalCollab2023[, c(2,3)]

collabProps2023 = apply(formalCollab2023, MARGIN=2, FUN=function(x) table(x)/length(x))

names(collabProps2023) = c("formal", "informal")




### End users

### 2023, Q17; 
endUseCols2023 = str_detect(colnames(survey2023), "^Q17")

endUseDat2023 = survey2023[, endUseCols2023]

endUseDat2023 = endUseDat2023[,1:(dim(endUseDat2023)[2]-1)]

endUseVec2023 = unname(unlist(apply(endUseDat2023, MARGIN=1, FUN=function(x) x[!is.na(x)])))

### Give a table for this one
table(endUseVec2023)/dim(survey2019)[1]

### 2019; None










