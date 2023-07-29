### INSTALLATION OF PACKAGE ###

install.packages("IBrokers")
library(IBrokers)

### Connection###

tws = twsConnect(port =7496)

isConnected(tws)

twsConnectionTime(tws)

#twsDisconnect(tws)

### ACCOUNT INFO ###
accountInfo = reqAccountUpdates(tws)
head(accountInfo)


### CONTRACTS

#twsContract(0,"AAPL","STK")
#twsEquity()
#twsOption()
#twsFuture()

### MARKET DATA ###

#Define the Contract
security = twsSMART("AAPL")


reqMktData(tws,security)


aapl_data = reqHistoricalData(tws,security)

print(head(aapl_data))

twsDisconnect(tws)
