#### SETUP ####
SETWD(/Users/kruri/Desktop/untitled folder 4)
install.packages("quantmod")
install.packages("psych")
install.packages("ggplot2")

library(quantmod)
library(psych)
library(ggplot2)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

#### PORTFOLIO BUILDING ####
symbols=c("MFC","BANC","BYFC", "PROV")

portfolio =lapply(symbols, function(x){
  dailyReturn(na.omit(getSymbols(x,
                                  from=as.Date("2017-01-01"),
                                  auto.assign = F)))
})

portfolio =do.call(merge.xts,portfolio)
colnames(portfolio)= c("ManuLife", "WaterDrop", "Broadway", "Provident")
head(portfolio)

eq_wts= c(0.10,0.10,0.10,0.10)

portfolioreturn = Return.portfolio(portfolio,
                                   weights = eq_wts,
                                   rebalance_on = "months",
                                   verbose = T)
portfolioreturn

portfolioanualret = Return.annualized(
  portfolioreturn$returns)
portfolioanualret

StdDev.annualized(portfolioreturn$returns)

table.AnnualizedReturns(portfolioreturn$returns,
                        Rf= 0.065/12)
#### PORTFOLIO CHART ####

charts.PerformanceSummary(portfolio)
charts.RollingPerformance(portfolioreturn$returns)
chart.Histogram(portfolioreturn$returns,
                 methods= c("add.density",
                            "add.normal"))
chart.Boxplot(portfolioreturn$returns)
skewness(portfolioreturn$returns)

kurtosis(portfolioreturn$returns)

chart.Correlation(portfolio)

cor(portfolio)

#### PORTFOLIO RATIOS ####

###Min Exp return - 20%
###-ve sortino risk reward not equivalent
SortinoRatio(portfolioreturn$returns, MAR=0.20)

### BENCHMARK Sensex / Nifty

getSymbols("^NDX", from=as.Date("2017-01-01"))
niftyreturn = Return.calculate(NSEI$NSEI.Close)

#more or less of retunr of fund
InformationRatio(portfolioreturn$returns, niftyreturn)

#fund ability to get higher return
barplot(TrackingError(portfolioreturn$returns,niftyreturn))

#how well portfolio tracks market
BetaCoVariance(portfolioreturn$returns,niftyreturn)

CAPM.beta(portfolioreturn$returns,niftyreturn, Rf =0.065)

#DOWNSIDE RISK MEASRE

VaR(portfolioreturn$returns). #95 confidence level
ES(portfolioreturn$returns)

volatilitybucket=StdDev(na.omit(portfolio),
                        portfolio_method = "component",
                        weights = eq_wts)
cbind(eq_wts,volatility=volatilitybucket$pct_contrib_StdDev)

#### Portfolio optimisation ####

library(tseries)

optimumportfolio=portfolio.optim(na.omit(portfolio))

pf_wts=optimumportfolio$pw

names(pf_wts)=colnames(portfolio)

round(pf_wts, digits=3)

barplot(pf_wts)

#Portfolio weights target percent more than AvgRet

optimumportfolio2=portfolio.optim(na.omit(portfolio),
                                  pm=1.1*mean(mean(na.omit(
                                    portfolio))))

pf_wts2=optimumportfolio2$pw

names(pf_wts2)=colnames(portfolio)
round(pf_wts2,digits=2)
barplot(pf_wts2)

#PORTFOLIO OPTIMISATION _ 2 CONSTRAINTS

port_spec=portfolio.spec(colnames(portfolio))
port_spec

port_spec=add.constraint(portfolio= port_spec,
                         type = "full_investment")

port_spec=add.constraint(portfolio=port_spec,
                         type= "long_only")

#OBJECTIVE 1 MAX RETURNS

port_spec=add.objective(portfolio= port_spec,
                        type="return",
                        name= "mean")

#MIN RISK

port_spec=add.objective(portfolio = port_spec,
                        type="risk",
                        name="StdDev")

print(port_spec)
optimumfinal=optimize.portfolio(na.omit(portfolio),
                                portfolio = port_spec,
                                optimize_method = "random",
                                trace=T)

optimumfinal

chart.RiskReward(optimumfinal,
                 risk.col="StdDev",
                 return.col = "mean",
                 chart.assets=TRUE)
chart.Weights(optimumfinal)


####SHARPE FUNCTION####
sharpeFactorEstimator <- 
  function(x, spec=NULL, ...)
  {
    # Sharpe Single Index Model:
    data <- getDataPart(x)
    factors <- attr(x, "factors")
    nScenarios <- nrow(data)
    X.mat <- cbind(rep(1, times=nScenarios), factors)
    G.hat <- solve(qr(X.mat), data)
    beta.hat <- G.hat[2, ]
    eps.hat <- data - X.mat %*% G.hat
    diagD.hat <- diag(crossprod(eps.hat) / (nScenarios-2))
    mu <- G.hat[1, ] + G.hat[2, ] * colMeans(factors)  
    Sigma <- var(factors)[[1]] * (beta.hat %o% beta.hat) + diag(diagD.hat)
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
  }

####SHARPE FUNCTION####
class(portfolio)
sharpeFactorEstimator(na.omit(portfolio))

