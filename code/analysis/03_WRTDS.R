#
# Author: Brock Kamrath
# Date: 2023 May 3
# Objective: Analysis script for EGERT/WRTDS
#

library(EGRET)

############################
# Gather discharge data:
siteID <- "04197100" # Honey Creek at Melmore, OH
startDate <- "" #Gets earliest date
endDate <- "2021-09-30"

# Gather sample data:
Sample <- readUserSample(filePath = here("data","processed"), fileName = "HoneySRP.csv")

#Gets earliest date from Sample record:
#This is just one of many ways to assure the Daily record
#spans the Sample record
startDate <- min(as.character(Sample$Date)) 

# Gather discharge data:
# Gather sample data:
Daily <- readUserDaily(filePath = here("data","processed"), fileName = "HoneyQ.csv")
#Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
# Gather site and parameter information:

# Here user must input some values for
# the default (interactive=TRUE)
parametercd = "00671"
INFO<- readNWISInfo(siteID, parametercd,interactive = FALSE)

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)

eList <- modelEstimation(eList)

# save results
savePath <- paste(here("data","processed","WRTDS_Output"),"/", sep = "")
saveResults(savePath, eList) 


# Check errors
errorStats(eList)

#################################################################################


flowDuration(eList, qUnit=1)

# Return to water year:
eList <- setPA(eList)

yearStart <- 2015
yearEnd <- 2021

plotConcTimeDaily(eList, yearStart, yearEnd)
plotFluxTimeDaily(eList, yearStart, yearEnd)

plotConcPred(eList)
plotFluxPred(eList)

plotResidPred(eList)
plotResidQ(eList, qUnit=1)

boxResidMonth(eList)

boxConcThree(eList)

plotConcHist(eList)
plotFluxHist(eList)


#Contour plots:
clevel<-seq(0,0.4,0.05)
yearStart <- 1985
yearEnd <- 2021

plotContours(eList, yearStart,yearEnd,qBottom=0.05,
             qTop = 130, contourLevels = clevel)


plotDiffContours(eList, year0 = 2010,
                 year1 = 2021,
                 qBottom = 0.01,
                 qTop = 120,
                 maxDiff = 0.25)

# how do the results look for complete water years
yearly_wrtds <- tableResults(eList)



pairResults

monthly <-  attr(pairResults, "byMonth")

knitr::kable(monthly, digits = 2)
monthly

plotMonthTrend(pairResults)


# How do these results look for each season?
eList_w <- setPA(eList, paStart = 3, paLong = 4)
winter <- tableResults(eList_w)

plotConcHist(eList_w)

Flux <- data.frame(eList$Daily$Date, eList$Daily$FluxDay)

eList <- WRTDSKalman(eList, niter = 10)
summary(eList$Daily)

summary(eList$Sample)


