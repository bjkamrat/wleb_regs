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
Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
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
yearStart <- 2000
yearEnd <- 2021

plotContours(eList, yearStart,yearEnd,qBottom=0.05,
             qTop = 125, contourLevels = clevel)


plotDiffContours(eList, year0 = 2014,
                 year1 = 2020,
                 qBottom = 0.05,
                 qTop = 120,
                 maxDiff = 0.25)
