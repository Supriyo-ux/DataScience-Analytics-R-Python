##Stationarity test start#################################
tseries::adf.test(myvector,alternative = c("stationary"),
                  trunc((length(myvector)-1)^(1/3)))
##Stationarity test end###################################
