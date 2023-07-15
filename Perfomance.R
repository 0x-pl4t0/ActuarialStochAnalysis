prices=Cl(BRDG)
ret=prices/Lag(prices)-1
delta=0.005
signal=ifelse(ret>delta,1,0)
head(signal)
signal=reclass(signal,prices)
chartSeries(BRDG, type="line")
addTA(signal,type="s", col="red")

charts.PerformanceSummary(ret)
