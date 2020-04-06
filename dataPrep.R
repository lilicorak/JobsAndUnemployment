
#install.packages("plyr")

require(plyr)

# unemployment data
# seasonally adjusted unemployment data
adjUnemp <- subset(read.csv("workingData/14100287.csv", head=TRUE, sep=","), 
                    select=c(REF_DATE, GEO, Labour.force.characteristics, Sex, Age.group, Statistics, Data.type, VALUE),
                      Labour.force.characteristics %in% c("Unemployment", "Unemployment rate") &
                      Statistics == "Estimate" &
                      Data.type == "Seasonally adjusted" &
                      Age.group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"))

adjUnemp$Statistics <- revalue(adjUnemp$Labour.force.characteristics,
                               c("Unemployment" = "Number unemployed (x1,000)", 
                                 "Unemployment rate" = "Official unemployment rate, seasonally adjusted"))

adjUnemp$refPeriod <- as.Date(paste0(adjUnemp$REF_DATE,"-01"))

adjUnemp <- subset(adjUnemp, select=-c(REF_DATE, Labour.force.characteristics, Data.type))


# non-seasonally adjusted supplementary unemployment rates
suppUnemp <- subset(read.csv("workingData/14100077.csv", head=TRUE, sep=","), 
                        select=c(REF_DATE, GEO, Supplementary.unemployment.rates, Sex, Age.group, VALUE),
                        Supplementary.unemployment.rates %in% c("R4 - official rate", 
                                                                "R8 - plus discouraged searchers, waiting group, portion of involuntary part-timers") &
                          Age.group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"))

suppUnemp$Statistics <- revalue(suppUnemp$Supplementary.unemployment.rates, 
                                            c("R4 - official rate" = "Official unemployment rate, not seasonally adjusted",
                                              "R8 - plus discouraged searchers, waiting group, portion of involuntary part-timers" =
                                                "Comprehensive unemployment rate, not seasonally adjusted "))

suppUnemp$refPeriod <- as.Date(paste0(suppUnemp$REF_DATE,"-01"))

suppUnemp <- subset(suppUnemp, select=-c(REF_DATE, Supplementary.unemployment.rates))


# unemployment by reason for leaving job
reasonUnemp <- subset(read.csv("workingData/14100125.csv", head=TRUE, sep=","), 
                    select=c(REF_DATE, GEO, Reason, Characteristics, Sex, Age.group, VALUE),
                    Reason != "Have not worked in the last year" &
                      Reason != "Never worked" &
                      Reason != "Total, all reasons" &
                      Characteristics == "Unemployed" &
                      Age.group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"))

reasonUnemp$Statistics <- reasonUnemp$Reason

reasonUnemp$refPeriod <- as.Date(paste0(reasonUnemp$REF_DATE,"-01"))

reasonUnemp <- subset(reasonUnemp, select=-c(REF_DATE, Reason, Characteristics))

# short unemployment estimate
shortUnemp <- subset(read.csv("workingData/14100342.csv", head=TRUE, sep=","), 
                   select=c(REF_DATE, GEO, Duration.of.unemployment, Sex, Age.group, Statistics, Data.type, VALUE),
                     Duration.of.unemployment == "1 to 4 weeks" &
                     Statistics == "Estimate" &
                     Data.type == "Seasonally adjusted" &
                     Age.group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"))

shortUnemp$Statistics <- "Number unemployed one month or less (x1,000)"

shortUnemp$refPeriod <- as.Date(paste0(shortUnemp$REF_DATE,"-01"))

shortUnemp <- subset(shortUnemp, select=-c(REF_DATE, Duration.of.unemployment, Data.type))

# export final unemployment file
unempData <- rbind(adjUnemp, suppUnemp, reasonUnemp, shortUnemp)

unempData$GEO <- revalue(unempData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                      "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                      "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                      "British Columbia" = "BC"))

write.csv(unempData, file="data/unempFinalData.csv",na="",row.names = F)





# employment data by NAICS
naicsEmp <- subset(read.csv("workingData/14100355.csv", head=TRUE, sep=","), 
                                select=c(REF_DATE, GEO, Supplementary.unemployment.rates, Sex, Age.group, VALUE))



# employment data by demographics
14100287

# employment job tenure
14100050

# export final employment data file