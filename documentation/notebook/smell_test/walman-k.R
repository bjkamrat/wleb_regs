
load("data/processed/WRTDS_Output/NA.Phosphate.RData")

# pair results 


plotMonthTrend(pairResults)


eList_K <- WRTDSKalman(eList, niter = 200)

print(summary(eList_K$Daily))


AnnualResults <- setupYears(eList_K$Daily)


plotWRTDSKalman(eList_K)
