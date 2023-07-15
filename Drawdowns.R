getSymbols("BRDG")

prices=BRDG$BRDG.Close
prices=prices["2018-01-01::"]
#Calculate indicators for trading strategy
prices=na.omit(prices)
CCI20=CCI(prices,20) ###Comoddity chanel index Overbought or oversold

RSI14=RSI(prices,14)
DEMA10=DEMA(prices,10,v=1)
DEMA10c=prices-DEMA10
DEMA10c=DEMA10c/.0001
### CREATION OF TRADING RULE

buy.signal =ifelse(RSI14<30 & CCI20 > -290 & CCI20 < -100 & DEMA10c > -40 & DEMA10c < 750,1 ,NA)

sma=SMA(prices,n=1)
signal=Lag(ifelse(sma$SMA < buy.signal,1,-1))

head(signal)

retSMA=ROC(prices)*signal

plot(sma)
table.Drawdowns(retSMA)
