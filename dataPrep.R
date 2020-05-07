
# create wide data files rather than long

#install.packages(c("plyr", "cansim", "tidyr"))

require(plyr)
require(cansim)
require(tidyr)

# unemployment data
# seasonally adjusted unemployment data
adjUnemp <- subset(get_cansim("14-10-0287", refresh = TRUE),
                    select=c(REF_DATE, GEO, `Labour force characteristics`, Sex, `Age group`, Statistics, `Data type`, VALUE),
                    `Labour force characteristics` %in% c("Unemployment", "Unemployment rate") &
                      Statistics == "Estimate" &
                      `Data type` == "Seasonally adjusted" &
                      `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                        as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

adjUnemp$Statistics <- revalue(adjUnemp$`Labour force characteristics`,
                               c("Unemployment" = "Number unemployed (x1,000)", 
                                 "Unemployment rate" = "Official unemployment rate, seasonally adjusted"))

adjUnemp$refPeriod <- as.Date(paste0(adjUnemp$REF_DATE,"-01"))

adjUnemp <- subset(adjUnemp, select=-c(REF_DATE, `Labour force characteristics`, `Data type`))


# non-seasonally adjusted supplementary unemployment rates
suppUnemp <- subset(get_cansim("14-10-0077", refresh = TRUE),
                        select=c(REF_DATE, GEO, `Supplementary unemployment rates`, Sex, `Age group`, VALUE),
                        `Supplementary unemployment rates` %in% c("R4 - official rate", 
                                                                "R8 - plus discouraged searchers, waiting group, portion of involuntary part-timers") &
                          `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                          as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

suppUnemp$Statistics <- revalue(suppUnemp$`Supplementary unemployment rates`, 
                                            c("R4 - official rate" = "Official unemployment rate, not seasonally adjusted",
                                              "R8 - plus discouraged searchers, waiting group, portion of involuntary part-timers" =
                                                "Comprehensive unemployment rate, not seasonally adjusted"))

suppUnemp$refPeriod <- as.Date(paste0(suppUnemp$REF_DATE,"-01"))

suppUnemp <- subset(suppUnemp, select=-c(REF_DATE, `Supplementary unemployment rates`))


# unemployment by reason for leaving job
reasonUnemp <- subset(get_cansim("14-10-0125", refresh = TRUE),
                      select=c(REF_DATE, GEO, Reason, Characteristics, Sex, `Age group`, VALUE),
                      Reason != "Have not worked in the last year" &
                      Reason != "Never worked" &
                      Reason != "Total, all reasons" &
                      Characteristics == "Unemployed" &
                      `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                      as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

reasonUnemp$Statistics <- paste0("Number unemployed, ", reasonUnemp$Reason)

reasonUnemp$refPeriod <- as.Date(paste0(reasonUnemp$REF_DATE,"-01"))

reasonUnemp <- subset(reasonUnemp, select=-c(REF_DATE, Reason, Characteristics))

# short unemployment estimate
shortUnemp <- subset(get_cansim("14-10-0342", refresh = TRUE),
                     select=c(REF_DATE, GEO, `Duration of unemployment`, Sex, `Age group`, Statistics, `Data type`, VALUE),
                     `Duration of unemployment` == "1 to 4 weeks" &
                     Statistics == "Estimate" &
                     `Data type` == "Seasonally adjusted" &
                     `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                      as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

shortUnemp$Statistics <- "Number unemployed one month or less (x1,000)"

shortUnemp$refPeriod <- as.Date(paste0(shortUnemp$REF_DATE,"-01"))

shortUnemp <- subset(shortUnemp, select=-c(REF_DATE, `Duration of unemployment`, `Data type`))

# export final unemployment file
unempData <- rbind(adjUnemp, suppUnemp, reasonUnemp, shortUnemp)

unempDataWide <- spread(unempData, key=Statistics, value=VALUE)

unempDataWide$GEO <- revalue(unempDataWide$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                      "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                      "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                      "British Columbia" = "BC"))

write.csv(unempDataWide, file="data/unempFinalDataWide.csv",na="",row.names = F)


### Employment data

# number and rate employed
# 14100287
adjEmp <- subset(get_cansim("14-10-0287", refresh = TRUE),
                 select=c(REF_DATE, GEO, `Labour force characteristics`, Sex, `Age group`, Statistics, `Data type`, VALUE),
                 `Labour force characteristics` %in% c("Employment", "Employment rate") &
                   Statistics == "Estimate" &
                   `Data type` == "Seasonally adjusted" &
                   `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                    as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

adjEmp$Statistics <- revalue(adjEmp$`Labour force characteristics`,
                               c("Employment" = "Number employed (x1,000)", 
                                 "Employment rate" = "Employment rate, seasonally adjusted"))

adjEmp$refPeriod <- as.Date(paste0(adjEmp$REF_DATE,"-01"))

adjEmp <- subset(adjEmp, select=-c(REF_DATE, `Labour force characteristics`, `Data type`))


# number employed 1 to 3 months
shortEmp <- subset(get_cansim("14-10-0050", refresh = TRUE),
                 select=c(REF_DATE, GEO, `Job tenure`, Sex, `Age group`, `Type of work`, VALUE),
                 `Job tenure` == "1 to 3 months" &
                   `Type of work` == "Both full and part-time employment" &
                   `Age group` %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over") &
                    as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

shortEmp$Statistics <- revalue(shortEmp$`Job tenure`,
                             c("1 to 3 months" = "Number employed three months or less (x1,000)"))

shortEmp$refPeriod <- as.Date(paste0(shortEmp$REF_DATE,"-01"))

shortEmp <- subset(shortEmp, select=-c(REF_DATE, `Job tenure`, `Type of work`))

# employment data by NAICS
naicsEmp <- subset(get_cansim("14-10-0355", refresh = TRUE),
                   select=c(REF_DATE, GEO, `North American Industry Classification System (NAICS)`, `Data type`, Statistics, VALUE),
                   `Data type` == "Seasonally adjusted" &
                     Statistics == "Estimate" &
                      as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

naicsEmp$Statistics <- paste0("Number employed, ", gsub(" \\[.*", "", naicsEmp$`North American Industry Classification System (NAICS)`))

naicsEmp$refPeriod <- as.Date(paste0(naicsEmp$REF_DATE,"-01"))

naicsEmp$Sex <- "Both sexes"

naicsEmp$`Age group` <- "15 years and over"

naicsEmp <- subset(naicsEmp, select=-c(REF_DATE, `North American Industry Classification System (NAICS)`, `Data type`))

# actual hours worked by NAICS
hoursEmp <- subset(get_cansim("14-10-0289", refresh = TRUE),
                   select=c(REF_DATE, GEO, `North American Industry Classification System (NAICS)`, Statistics, VALUE),
                   Statistics == "Estimate" &
                     as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

hoursEmp$Statistics <- paste0("Actual hours worked, ", gsub(" \\[.*", "", hoursEmp$`North American Industry Classification System (NAICS)`))

hoursEmp$refPeriod <- as.Date(paste0(hoursEmp$REF_DATE,"-01"))

hoursEmp$Sex <- "Both sexes"

hoursEmp$`Age group` <- "15 years and over"

hoursEmp <- subset(hoursEmp, select=-c(REF_DATE, `North American Industry Classification System (NAICS)`))

# Number employed in major CMAs by CMA
cmaEmp <- subset(get_cansim("14-10-0295", refresh = TRUE),
                 select=c(REF_DATE, GEO, `Labour force characteristics`, Statistics, `Data type`, VALUE),
                 Statistics == "Estimate" &
                   `Labour force characteristics` == "Employment" &
                   `Data type` == "Seasonally adjusted" &
                   as.Date(paste0(REF_DATE,"-01")) >= as.Date("2000-01-01"))

cmaEmp$Statistics <- revalue(cmaEmp$`Labour force characteristics`,
                             c("Employment" = "Number employed (x1,000)"))

cmaEmp$refPeriod <- as.Date(paste0(cmaEmp$REF_DATE,"-01"))

cmaEmp$Sex <- "Both sexes"

cmaEmp$`Age group` <- "15 years and over"

cmaEmp <- subset(cmaEmp, select=-c(REF_DATE, `Labour force characteristics`, `Data type`))

# export final employment data file
empData <- rbind(adjEmp, shortEmp, naicsEmp, hoursEmp, cmaEmp)

empDataWide <- spread(empData, key=Statistics, value=VALUE)

empDataWide$GEO <- revalue(empDataWide$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                          "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                          "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                          "British Columbia" = "BC"))

write.csv(empDataWide, file="data/empFinalDataWide.csv",na="",row.names = F)

