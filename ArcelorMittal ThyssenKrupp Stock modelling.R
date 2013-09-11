    # # Modelling of the stock prices/log-returns of ArcelorMittal and ThyssenKrupp
    # # Copyright (C) 2012-2013 Stephan E. Becker

    # # This program is free software: you can redistribute it and/or modify
    # # it under the terms of the GNU General Public License as published by
    # # the Free Software Foundation, either version 3 of the License, or
    # # (at your option) any later version.

    # # This program is distributed in the hope that it will be useful,
    # # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # # GNU General Public License for more details.

    # # You should have received a copy of the GNU General Public License
    # # along with this program.  If not, see <http://www.gnu.org/licenses/>.



##Stock price modelling of ArcelorMittal and ThyssenKrupp

##necessary packages:
install.packages("moments")
install.packages("rugarch")
install.packages("fGarch")

library(moments)
library(rugarch)
library(fGarch)


##Part 1-2: Read data set and create the needed time sets/vectors; Compute the log-returns

#preperation for ArcelorMittal
AM.tmp<- read.csv("ArcelorMittal.csv") ## read ArcelorMittal data
am.tmp<- AM.tmp$Close ## Close data
n.am<- length(am.tmp) ## length of am 
am<- log(am.tmp[n.am:1]) ## ’invert ’ am in time + log!
d.am<- 100*diff(am) 
nd.am<-n.am-1 ## length of log -returns
time.n.am<- as.Date(AM.tmp$Date)[n.am:1+1] ## create time (’inverted ’)for 1:n
time.nd.am<- as.Date(AM.tmp$Date)[nd.am:1+1] ## create time (’inverted’) for 1:nd

#preperation for ThyssenKrupp
TK.tmp<- read.csv("ThyssenKrupp.csv") ## read ThyssenKrupp data
tk.tmp<- TK.tmp$Close ## Close data
n.tk<- length(tk.tmp) ## length of tk 
tk<- log(tk.tmp[n.tk:1]) ## ’invert ’ tk in time + log!
d.tk<- 100*diff(tk) 
nd.tk<-n.tk-1 ## length of log -returns
time.n.tk<- as.Date(TK.tmp$Date)[n.tk:1+1] ## create time (’inverted ’)for 1:n
time.nd.tk<- as.Date(TK.tmp$Date)[nd.tk:1+1] ## create time (’inverted’) for 1:nd


## the original TK data - for comparison only!!
#preperation for ThyssenKrupp original date - this is not used for modeling later on!
TKo.tmp<- read.csv("ThyssenKrupp - original.csv") ## read ThyssenKrupp data
tko.tmp<- TKo.tmp$Close ## Close data
n.tko<- length(tko.tmp) ## length of tk 
tko<- log(tko.tmp[n.tko:1]) ## ’invert ’ tk in time + log!
d.tko<- 100*diff(tko) 
nd.tko<-n.tko-1 ## length of log -returns
time.n.tko<- as.Date(TKo.tmp$Date)[n.tko:1+1] ## create time (’inverted ’)for 1:n
time.nd.tko<- as.Date(TKo.tmp$Date)[nd.tko:1+1] ## create time (’inverted’) for 1:nd

plot(time.n.tko,tko.tmp[n.tko:1],type="l",main="Stock prices of ThyssenKrupp - original data",xlab="Time",ylab="Stock price") ##tk.tmp must be inverted!
savePlot(filename="Stock prices of ThyssenKrupp - original",type="eps")

plot(time.nd.tko,d.tko,type="l",main="Log-returns ThyssenKrupp - original data",xlab="Time",ylab="log returns") ##d.tk is already inverted
savePlot(filename="Log-returns ThyssenKrupp - original",type="eps")

hist(d.tko, col=7, breaks=50, freq=FALSE, xlab="log-returns", main="Log-returns of ThyssenKrupp - original data")
savePlot(filename="Histogram of the log-returns of ThyssenKrupp - original",type="eps")

par(mfrow=c(2,2))
acf(d.tko, lwd=2, main="ACF: log-returns ThyssenKrupp")
pacf(d.tko, lwd=2, main="PACF: log-returns ThyssenKrupp")
acf(d.tko^2, lwd=2, main="ACF: squared log-returns ThyssenKrupp")
pacf(d.tko^2, lwd=2, main="PACF: squared log-returns ThyssenKrupp")
par(mfrow=c(1,1))
savePlot(filename="ACF_PACF_log-returns of ThyssenKrupp - original",type="eps")









##Part 3: basic properties of data:


#a) plot the original time series 

# pdf("Stock_prices.pdf") ##creating a pdf file
plot(time.n.am,am.tmp[n.am:1],type="l",main="Stock prices of ArcelorMittal",xlab="Time",ylab="Stock price") ##am.tmp must be inverted!
savePlot(filename="Stock prices of ArcelorMittal",type="eps")
plot(time.n.tk,tk.tmp[n.tk:1],type="l",main="Stock prices of ThyssenKrupp",xlab="Time",ylab="Stock price") ##tk.tmp must be inverted!
savePlot(filename="Stock prices of ThyssenKrupp",type="eps")
# dev.off()


#b) plot the log-returns

# pdf("Stock_returns.pdf")
plot(time.nd.am,d.am,type="l",main="Log-returns ArcelorMittal",xlab="Time",ylab="log returns") ##d.am is already inverted
savePlot(filename="Log-returns ArcelorMittal",type="eps")
plot(time.nd.tk,d.tk,type="l",main="Log-returns ThyssenKrupp",xlab="Time",ylab="log returns") ##d.tk is already inverted
savePlot(filename="Log-returns ThyssenKrupp",type="eps")
# dev.off()
 

#c) histogram of the log-returns

# pdf("Histogram_log-returns.pdf")
hist(d.am, col=7, breaks=50, freq=FALSE, xlab="log-returns", main="Log-returns of ArcelorMittal")
savePlot(filename="Histogram of the log-returns of ArcelorMittal",type="eps")
hist(d.tk, col=7, breaks=50, freq=FALSE, xlab="log-returns", main="Log-returns of ThyssenKrupp")
savePlot(filename="Histogram of the log-returns of ThyssenKrupp",type="eps")
# dev.off()


#d) create the ACF, PACF of the log-returns

# pdf("ACF_PACF_log-returns.pdf")
par(mfrow=c(2,2)) ## multiplot
acf(d.am, lwd=2, main="ACF: log-returns ArcelorMittal")
pacf(d.am, lwd=2, main="PACF: log-returns ArcelorMittal")
acf(d.tk, lwd=2, main="ACF: log-returns ThyssenKrupp")
pacf(d.tk, lwd=2, main="PACF: log-returns ThyssenKrupp")
par(mfrow=c(1,1)) ## normal plot option
savePlot(filename="ACF_PACF_log-returns",type="eps")
# dev.off()

#e) create the ACF, PACF of the squared log-returns

# pdf("ACF_PACF_squared_log-returns.pdf")
par(mfrow=c(2,2)) ## multiplot
acf(d.am^2, lwd=2, main="ACF: squared log-returns ArcelorMittal")
pacf(d.am^2, lwd=2, main="PACF: squared log-returns ArcelorMittal")
acf(d.tk^2, lwd=2, main="ACF: squared log-returns ThyssenKrupp")
pacf(d.tk^2, lwd=2, main="PACF: squared log-returns ThyssenKrupp")
par(mfrow=c(1,1)) ## normal plot option
savePlot(filename="ACF_PACF_squared_log-returns",type="eps")
# dev.off()

# skewness, kurtosis, mean etc

library("moments")

summary(d.am)
sd(d.am)
skewness(d.am)
kurtosis(d.am)

summary(d.tk)
sd(d.tk)
skewness(d.tk)
kurtosis(d.tk)


## 2 in 1 - for reporting only!
par(mfrow=c(1,2))
#plot(time.n.tko,tko.tmp[n.tko:1],type="l",main="Stock prices of ThyssenKrupp - original data",xlab="Time",ylab="Stock price") ##tk.tmp must be inverted!
plot(time.n.am,am.tmp[n.am:1],type="l",main="Stock prices of ArcelorMittal",xlab="Time",ylab="Stock price") ##am.tmp must be inverted!
plot(time.n.tk,tk.tmp[n.tk:1],type="l",main="Stock prices of ThyssenKrupp",xlab="Time",ylab="Stock price") ##tk.tmp must be inverted!
#savePlot(filename="Stock prices of am-tk 2in1",type="eps")

plot(time.nd.am,d.am,type="l",main="Log-returns of ArcelorMittal",xlab="Time",ylab="log returns") ##d.am is already inverted
plot(time.nd.tk,d.tk,type="l",main="Log-returns of ThyssenKrupp",xlab="Time",ylab="log returns") ##d.tk is already inverted
#savePlot(filename="Log-returns am-tk 2in1",type="eps")

hist(d.am, col=7, breaks=50, freq=FALSE, xlab="log-returns", main="Log-returns of ArcelorMittal")
hist(d.tk, col=7, breaks=50, freq=FALSE, xlab="log-returns", main="Log-returns of ThyssenKrupp")
#savePlot(filename="Histogram log-returns am-tk 2in1",type="eps")
#savePlot(filename="Log-returns am-tk 2in2",type="eps")
par(mfrow=c(1,1))













##Part 4: Start to model.

##function to check the best possible AIC-values for certain parameters, ARMA
model_fitting_ARMA<-function(d.xx){  ## takes the log-returns time series as input
  best_model<-arima0(d.xx, order=c(1,0,0), include.mean=FALSE)
  min_aic<-best_model$aic
  
  includemean_bool<-c(FALSE,TRUE)
  arOrder_range<-1:5  ## AR or MA must be at least one!
  maOrder_range<-0:5  ## AR or MA must be at least one!
  
  for(iM in includemean_bool){
    for(arO in arOrder_range){
      for(maO in maOrder_range){
        arma_model<-arima0(d.xx, order=c(arO,0,maO), include.mean=iM)
        if(arma_model$aic<min_aic){
          min_aic<-arma_model$aic
          best_model<-arma_model
          print(min_aic)
        }
      }
    }
  return(best_model)
  }
}


library(rugarch)
ctrl<- list(RHO = 1, DELTA = 1e-8, MAJIT = 100, MINIT = 650,TOL = 1e-6) ## the control list

##function to check the best possible BIC-values for certain parameters, GARCH
model_fitting<-function(d.xx){  ## takes the log-returns time series as input
	best_model<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder = c(0, 0),include.mean = FALSE))
	garch.fit <- ugarchfit(data = d.xx, spec = garch.spec , solver = "solnp", solver.control = ctrl)
	min_bayes<-infocriteria(garch.fit)[2]
  
	garchModel_type<-c("sGARCH", "gjrGARCH", "apARCH","iGARCH","csGARCH")		##c("sGARCH", "fGARCH", "eGARCH", "gjrGARCH", "apARCH","iGARCH","csGARCH") #f and e produce errors
	garchOrder_range<-1:1		##counts double for computing time!
	armaOrder_range<-0:0		##counts double!
	includemean_bool<-c(FALSE)
	distribution_model_type<-c("std")		##c("norm","snorm","std","sstd","ged","sged","nig","ghyp","jsu")

	for(gM in garchModel_type){
		for(gO1 in garchOrder_range){
		  for(gO2 in garchOrder_range){
		    for(aO1 in armaOrder_range){
		      for(aO2 in armaOrder_range){
		        for(iM in includemean_bool){
		          for(dmt in distribution_model_type){
		            garch.spec <- ugarchspec(variance.model = list(model = gM, garchOrder = c(gO1,gO2)), mean.model=list(armaOrder = c(aO1, aO2),include.mean = iM), distribution.model = dmt)
		            garch.fit <- ugarchfit(data = d.xx, spec = garch.spec , solver = "solnp", solver.control = ctrl)
		            if(infocriteria(garch.fit)[2] < min_bayes){
		              print("min_bayes:");print(min_bayes)		##debugging
		              print("infocriteria(garch.fit)[2]:");print(infocriteria(garch.fit)[2])		##debugging
		              min_bayes<-infocriteria(garch.fit)[2]
		              best_model<-garch.spec
		            }
		          }
		        }
		      }
		    }
		  }
		}
	}
	return(best_model)
}


##Part 5a: determining a model for AM:

best_arma.AM<-model_fitting_ARMA(d.am)
best_arma.AM$aic

#using the previously defined function, !!!MAY TAKE VERY LONG!!!:
# best_model.AM<-model_fitting(d.am)	##may run for a while, check model_fitting settings

#garch.spec <- best_model.AM
#garch.fit <- ugarchfit(data = d.am, spec = garch.spec , solver = "solnp", solver.control = ctrl)
#infocriteria(garch.fit)[2] ##shows only Bayes IC
#garch.fit


#current best model for AM (from model_fitting(d.am) function):

garch.spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder = c(0, 0),include.mean = FALSE),distribution.model = "std")  ## include mean TRUE has better BIC, but results in strange forecasting behaviour

garch.spec.AM <- garch.spec
garch.fit.AM <- ugarchfit(data = d.am, spec = garch.spec.AM , solver = "solnp", solver.control = ctrl)
infocriteria(garch.fit.AM)[2] ##shows only Bayes IC
garch.fit.AM



##Part 5a: determining a model for TK:

best_arma.TK<-model_fitting_ARMA(d.tk)
best_arma.TK$aic


##using the previously defined function, !!!MAY TAKE VERY LONG!!!:
# best_model.TK<-model_fitting(d.tk)	##may run for a while, check model_fitting settings
# 
# garch.spec <- best_model.TK
# garch.fit <- ugarchfit(data = d.tk, spec = garch.spec , solver = "solnp", solver.control = ctrl)
# infocriteria(garch.fit)[2] ##shows only Bayes IC
# garch.fit


##current best model for TK

garch.spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder = c(0, 0),include.mean = FALSE),distribution.model = "ged")

garch.spec.TK <- garch.spec
garch.fit.TK <- ugarchfit(data = d.tk, spec = garch.spec.TK , solver = "solnp", solver.control = ctrl)
infocriteria(garch.fit.TK)[2] ##shows only Bayes IC
garch.fit.TK














##Part 5b: Check model assumptions


##ArcelorMittal model
# pdf("model_assumption_checks_AM.pdf")

# library(evir)

##distribution check for AM
sres.AM<-residuals(garch.fit.AM,stand=TRUE) ## data must be standardized!!
hist(sres.AM , breaks=50,col=7, freq=FALSE ,main="Residuals in the ArcelorMittal-model",xlab="Standardised Estimated Residuals")
support.AM<-seq(min(sres.AM), max(sres.AM),length=1000) ##freq=FALSE, otherwise it doesnt work
lines(support.AM ,dt(support.AM,0.8), lwd=2)
savePlot(filename="Histogram of Residuals in the ArcelorMittal-model",type="eps")
# qqnorm(sres.AM, main="Q-Q Plot of ArcelorMittal-model residuals")
# qqline(sres.AM , col=2, lwd=2)
# savePlot(filename="Q-Q Plot of ArcelorMittal-model residuals",type="eps")
# ks.test(sres.AM , "pnorm")
# hill.est.AM<-hill(sres.AM, end=length(sres.AM)/log(length(sres.AM)))
# grid()
# abline(2,0)
# lines(smooth.spline(hill.est.AM,spar=1))
# savePlot(filename="hill plot ArcelorMittal-model",type="eps")

##independence check for AM
par(mfrow=c(2,1))
acf(sres.AM,main="ACF: residuals in AM-model")
pacf(sres.AM,main="PACF: residuals in AM-model")
savePlot(filename="ACF-PACF_ArcelorMittal-model",type="eps")
Box.test(sres.AM, type="Ljung-Box", lag=1)
Box.test(sres.AM, type="Ljung-Box", lag=2)


##homoscedasticity check for AM
par(mfrow=c(2,1))
acf(sres.AM^2, main="ACF: squared residuals in AM-model")
acf(abs(sres.AM), main="ACF: absolute residuals in AM-model")
savePlot(filename="squared-absolute-ACF_ArcelorMittal-model",type="eps")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
acf(sres.AM,main="ACF: residuals in AM-model")
pacf(sres.AM,main="PACF: residuals in AM-model")
acf(sres.AM^2, lag =50,main="ACF: squared residuals in AM-model")
acf(abs(sres.AM), lag=50,main="ACF: absolute residuals in AM-model")
savePlot(filename="ACF-PACF_sq-abs_ArcelorMittal-model",type="eps")
par(mfrow=c(1,1))

# dev.off()



##ThyssenKrupp model
# pdf("model_assumption_checks_TK.pdf")

# install.packages("fGarch")
library(fGarch)

##distribution check for TK
sres.TK<-residuals(garch.fit.TK,stand=TRUE) ## data must be standardized!!
hist(sres.TK, breaks=50,col=7, freq=FALSE ,main="Residuals in the ThyssenKrupp-model",xlab="Standardised Estimated Residuals")
support.TK<-seq(min(sres.TK), max(sres.TK),length=1000) ##freq=FALSE, otherwise it doesnt work
# lines(support.TK,dged(support.TK,nu=0.5), lwd=2)
# lines(support.TK,dnorm(support.TK), lwd=2
lines(support.AM ,dt(support.AM,0.8), lwd=2)

savePlot(filename="Histogram of Residuals in the ThyssenKrupp-model",type="eps")
# qqnorm(sres.TK, main="Q-Q Plot of ThyssenKrupp-model residuals")
# qqline(sres.TK, col=2, lwd=2)
# savePlot(filename="Q-Q Plot of ThyssenKrupp-model residuals",type="eps")
# ks.test(sres.TK, "pnorm")
# hill.est.TK<-hill(sres.TK, end=length(sres.TK)/log(length(sres.TK)))
# grid()
# abline(2,0)
# lines(smooth.spline(hill.est.TK,spar=1))
# savePlot(filename="hill plot ThyssenKrupp-model",type="eps")


##independence check for TK
par(mfrow=c(2,1))
acf(sres.TK,main="ACF: residuals in TK-model")
pacf(sres.TK,main="PACF: residuals in TK-model")
savePlot(filename="ACF-PACF_ThyssenKrupp-model",type="eps")
Box.test(sres.TK, type="Ljung-Box", lag=1)
Box.test(sres.TK, type="Ljung-Box", lag=2)


##homoscedasticity check for TK
par(mfrow=c(2,1))
acf(sres.TK^2, main="ACF: squared residuals in TK-model")
acf(abs(sres.TK), main="ACF: absolute residuals in TK-model")
savePlot(filename="squared-absolute-ACF_ThyssenKrupp-model",type="eps")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
acf(sres.TK,main="ACF: residuals in TK-model")
pacf(sres.TK,main="PACF: residuals in TK-model")
acf(sres.TK^2, lag =50,main="ACF: squared residuals in TK-model")
acf(abs(sres.TK), lag=50,main="ACF: absolute residuals in TK-model")
savePlot(filename="ACF-PACF_sq-abs_ThyssenKrupp-model",type="eps")
par(mfrow=c(1,1))
# dev.off()


## 2 in 1 - for reporting only!

##distribution check
par(mfrow=c(1,2))
sres.AM<-residuals(garch.fit.AM,stand=FALSE)
hist(sres.AM , breaks=50,col=7, freq=FALSE ,main="Residuals in the AM-model",xlab="Standardised Estimated Residuals")
support.AM<-seq(min(sres.AM), max(sres.AM),length=1000) ##freq=FALSE, otherwise it doesnt work
lines(support.AM ,dt(support.AM,0.8), lwd=2)
sres.TK<-residuals(garch.fit.TK,stand=FALSE)
hist(sres.TK, breaks=50,col=7, freq=FALSE ,main="Residuals in the TK-model",xlab="Standardised Estimated Residuals")
support.TK<-seq(min(sres.TK), max(sres.TK),length=1000) ##freq=FALSE, otherwise it doesnt work
lines(support.AM ,dt(support.AM,0.8), lwd=2)
savePlot(filename="Histogram of Residuals am-tk model 2",type="eps")
par(mfrow=c(1,1))


##independence
par(mfrow=c(2,2))
acf(sres.AM,main="ACF: residuals in AM-model")
pacf(sres.AM,main="PACF: residuals in AM-model")
acf(sres.TK,main="ACF: residuals in TK-model")
pacf(sres.TK,main="PACF: residuals in TK-model")
savePlot(filename="ACF-PACF_AM-TK-models",type="eps")
par(mfrow=c(1,1))

##homoscedasticity
par(mfrow=c(2,2))
acf(sres.AM^2, main="ACF: squared residuals in AM-model")
acf(abs(sres.AM), main="ACF: absolute residuals in AM-model")
acf(sres.TK^2, main="ACF: squared residuals in TK-model")
acf(abs(sres.TK), main="ACF: absolute residuals in TK-model")
savePlot(filename="squared-absolute-ACF_AM-TK-models",type="eps")
par(mfrow=c(1,1))














##Part 6: Do the forecast

n.pred<-3 ##number of forecast days
M<-10000 ## Define Monte -Carlo sample size
seed<-92358054

## for reporting
par(mfrow=c(2,2))


##Forecasting log-returns for ArcelorMittal

# pdf("forecasted_log-returns_AM.pdf")

#renamed variables
garch.fit<-garch.fit.AM
time.nd<-time.nd.am
nd<-nd.am
xd<-d.am

#forecasting
forc.am <- ugarchforecast(garch.fit , n.ahead=n.pred)
forc<- as.data.frame(forc.am)
forc

time.forc<-ForwardDates(as.character(time.nd), n.ahead=n.pred, date.format="%Y-%m-%d")
time.nd.total<- c(time.nd, time.forc)
n.total<- length(time.nd.total)
n.last<- 50 ## last observed values to plot
region<- (nd-n.last):n.total ## total region
region.obs<- (nd-n.last):nd ## observed
region.forc<- (nd+1):n.total ## forecast

plot(time.nd.total[region.obs],xd[region.obs], type="o", col="steelblue", main="Forecasted log-returns of ArcelorMittal", xlab="Time", ylab="log-returns", xlim=c(time.nd.total[min(region)], time.nd.total[max(region)]),ylim=c( -7 ,7))
grid() ## add grid
abline(0,0, lty=3) ## add x-axis
fit.val<-fitted(garch.fit) ## extract fitted values
lines(time.nd.total[region.obs], fit.val[region.obs] , type="l", lwd=2)
lines(time.nd.total[region.forc], forc$series , type="o", lwd=1, lty=3, pch=21, bg="green3")

#quantiles
coef(garch.fit)
alpha =0.05
est.coefs<-coef(garch.fit)
skew.est<- est.coefs["skew"] #only needed for skeweddistribution assumptions
shape.est<- est.coefs["shape"] #only needed for shapeddistribution assumptions (e.g. t-distribution)
quant<-qdist( "std", c(alpha/2,1-alpha/2), shape=shape.est )
lines(time.nd.total[region.obs], (fit.val+quant [1]*sigma(garch.fit))[region.obs] , col="red", type="l")
lines(time.nd.total[region.obs], (fit.val+quant [2]*sigma(garch.fit))[region.obs] , col="red", type="l")
lines(time.nd.total[region.forc], forc$series+quant [1]*forc$sigma , col="red", type="l", lty=2)
lines(time.nd.total[region.forc], forc$series+quant [2]*forc$sigma , col="red", type="l", lty=2)
# legend("top", c("Observed values", "Fitted values", "Predictedvalues", paste(100*(1-alpha),"% quantiles (obs.)"), paste(100*(1-alpha),"% quantiles (pred.)", sep="")) , lwd=c(1,2,1,1,1), lty=c(3,1,2,1,2), col=c("steelblue", 1,1,"red","red"), pt.bg=c("lightblue",0,"green3" ,0,0), pch=c(21,46,21,46,46), bg=rgb(1,1,0.8) , cex=1) ## pch=46 for ’nothing ’

#savePlot(filename="forecasted_log-returns_AM",type="eps")
# dev.off()



##Forecasting log-returns for ThyssenKrupp

# pdf("forecasted_log-returns_TK.pdf")

#renamed variables
garch.fit<-garch.fit.TK
time.nd<-time.nd.tk
nd<-nd.tk
xd<-d.tk

#forecasting
forc.tk <- ugarchforecast(garch.fit , n.ahead=n.pred)
forc<- as.data.frame(forc.tk)
forc

time.forc<-ForwardDates(as.character(time.nd), n.ahead=n.pred, date.format="%Y-%m-%d")
time.nd.total<- c(time.nd, time.forc)
n.total<- length(time.nd.total)
n.last<- 50 ## last observed values to plot
region<- (nd-n.last):n.total ## total region
region.obs<- (nd-n.last):nd ## observed
region.forc<- (nd+1):n.total ## forecast

plot(time.nd.total[region.obs],xd[region.obs], type="o", col="steelblue", main="Forecasted log-returns of ThyssenKrupp", xlab="Time", ylab="log-returns", xlim=c(time.nd.total[min(region)], time.nd.total[max(region)]),ylim=c( -7 ,7))
grid() ## add grid
abline(0,0, lty=3) ## add x-axis
fit.val<-fitted(garch.fit) ## extract fitted values
lines(time.nd.total[region.obs], fit.val[region.obs] , type="l", lwd=2)
lines(time.nd.total[region.forc], forc$series , type="o", lwd=1, lty=3, pch=21, bg="green3")

#quantiles
coef(garch.fit)
alpha =0.05
est.coefs<-coef(garch.fit)
skew.est<- est.coefs["skew"] #only needed for skeweddistribution assumptions
shape.est<- est.coefs["shape"] #only needed for shapeddistribution assumptions (e.g. t-distribution)
quant<-qdist( "ged", c(alpha/2,1-alpha/2), shape=shape.est ) ## quantile distribution must match model distribution!
lines(time.nd.total[region.obs], (fit.val+quant [1]*sigma(garch.fit))[region.obs] , col="red", type="l")
lines(time.nd.total[region.obs], (fit.val+quant [2]*sigma(garch.fit))[region.obs] , col="red", type="l")
lines(time.nd.total[region.forc], forc$series+quant [1]*forc$sigma , col="red", type="l", lty=2)
lines(time.nd.total[region.forc], forc$series+quant [2]*forc$sigma , col="red", type="l", lty=2)
# legend("top", c("Observed values", "Fitted values", "Predictedvalues", paste(100*(1-alpha),"% quantiles (obs.)"), paste(100*(1-alpha),"% quantiles (pred.)", sep="")) , lwd=c(1,2,1,1,1), lty=c(3,1,2,1,2), col=c("steelblue", 1,1,"red","red"), pt.bg=c("lightblue",0,"green3" ,0,0), pch=c(21,46,21,46,46), bg=rgb(1,1,0.8) , cex=1) ## pch=46 for ’nothing ’

#savePlot(filename="forecasted_log-returns_TK",type="eps")
# dev.off()



##Forecasting stock prices for ArcelorMittal

# pdf("forecasted_stock-prices_AM.pdf")

#renamed variables
garch.fit<-garch.fit.AM
time.n<-time.n.am
time.nd<-time.nd.am
nd<-nd.am
n<-n.am
xd<-d.am
x<-am

#forecasting stock prices
garch.sim<-ugarchsim(garch.fit , n.sim=n.pred , m.sim=M,startMethod="sample", rseed=seed)
simulated<-as.matrix(as.data.frame(garch.sim , which="series"))
cum.simulated<-apply(simulated ,2,cumsum) 
cum.forecast<-x[n]+apply(cum.simulated ,1,mean) ## last value +mean of simulated ones
alpha<-0.05
cum.forecast.quant<-x[n] + apply(cum.simulated , 1, quantile, c(alpha/2, 1-alpha/2))

time.forc<-ForwardDates(as.character(time.nd), n.ahead=n.pred ,date.format="%Y-%m-%d")
time.n.total<- c(time.n, time.forc)
n.total<- length(time.n.total)
n.last<- 50
region<- (n-n.last):n.total ## total region
region.obs<- (n-n.last):n
region.forc<- (n+1):n.total
xlim.min<- time.n.total[min(region)]
xlim.max<- time.n.total[max(region)]
ylim.min<- min( exp(x[region.obs]), exp(cum.forecast.quant))
ylim.max<- max( exp(x[region.obs]), exp(cum.forecast.quant))
plot(time.n.total[region.obs],exp(x[region.obs]), type="o", col="steelblue", main="Forecasted stock prices of ArcelorMittal", xlab="Time",ylab="Stock price", sub=paste("M =", M, "seed =", seed), xlim=c(xlim.min,xlim.max), ylim=c(0,30))  ##ylim=c(ylim.min, ylim.max) ##ylim=c(0,30)
grid()
abline(0,0, lty=3)
fit.val<-x-c(0,residuals(garch.fit)) ## the zero because of the’differentiation ’

# lines(time.n.total[region.obs], exp(fit.val)[region.obs] , type="l", lwd=2)

lines(time.n.total[region.forc], exp(cum.forecast) , type="o",lwd=1, lty=3, pch=21, bg="green3")

# coef(garch.fit)
# alpha<-0.05
# est.coefs<-coef(garch.fit)
# skew.est<- est.coefs["skew"]
# shape.est<- est.coefs["shape"]
# quant<-qdist( "std", c(alpha/2,1-alpha/2), shape=shape.est)
# est.sigma<- c(sigma(garch.fit)[1], sigma(garch.fit) )
# lines(time.n.total[region.obs], exp(fit.val+quant[1]*est.sigma)[region.obs] , col="red", type="l")
# lines(time.n.total[region.obs], exp(fit.val+quant[2]*est.sigma)[region.obs] , col="red", type="l")
# lines(time.n.total[region.forc], exp(cum.forecast.quant[1,]) ,col="red", type="l", lty=2)
# lines(time.n.total[region.forc], exp(cum.forecast.quant[2,]) ,col="red", type="l", lty=2)
# legend("topleft", c("Observed values", "Fitted values", "Predicted values", paste(100*(1-alpha),"% quantiles (obs.)"), paste(100*(1-alpha),"% quantiles (pred.)", sep="")) , lwd=c(1,2,1,1,1), lty=c(3,1,2,1,2), col=c("steelblue", 1,1,"red","red"), pt.bg=c("lightblue",0,"green3" ,0,0), pch=c(21,46,21,46,46), bg=rgb(1,1,0.8) , cex =1.2) ## pch=46 for ’nothing ’

#savePlot(filename="forecasted_stock-prices_AM",type="eps")
# dev.off()



##Forecasting stock prices for ThyssenKrupp

# pdf("forecasted_stock-prices_TK.pdf")

#renamed variables
garch.fit<-garch.fit.TK
time.n<-time.n.tk
time.nd<-time.nd.tk
nd<-nd.tk
n<-n.tk
xd<-d.tk
x<-tk

#forecasting stock prices
garch.sim<-ugarchsim(garch.fit , n.sim=n.pred , m.sim=M,startMethod="sample", rseed=seed)
simulated<-as.matrix(as.data.frame(garch.sim , which="series"))
cum.simulated<-apply(simulated ,2,cumsum) 
cum.forecast<-x[n]+apply(cum.simulated ,1,mean) ## last value +mean of simulated ones
alpha<-0.05
cum.forecast.quant<-x[n] + apply(cum.simulated , 1, quantile, c(alpha/2, 1-alpha/2))

time.forc<-ForwardDates(as.character(time.nd), n.ahead=n.pred ,date.format="%Y-%m-%d")
time.n.total<- c(time.n, time.forc)
n.total<- length(time.n.total)
n.last<- 50
region<- (n-n.last):n.total ## total region
region.obs<- (n-n.last):n
region.forc<- (n+1):n.total
xlim.min<- time.n.total[min(region)]
xlim.max<- time.n.total[max(region)]
ylim.min<- min( exp(x[region.obs]), exp(cum.forecast.quant))
ylim.max<- max( exp(x[region.obs]), exp(cum.forecast.quant))
plot(time.n.total[region.obs],exp(x[region.obs]), type="o", col="steelblue", main="Forecasted stock prices of ThyssenKrupp", xlab="Time",ylab="Stock price", sub=paste("M =", M, "seed =", seed), xlim=c(xlim.min,xlim.max),ylim=c(0,30)) ##ylim=c(ylim.min, ylim.max) ) ##ylim=c(0,30)
grid()
abline(0,0, lty=3)
fit.val<-x-c(0,residuals(garch.fit)) ## the zero because of the’differentiation ’

# lines(time.n.total[region.obs], exp(fit.val)[region.obs] , type="l", lwd=2)

lines(time.n.total[region.forc], exp(cum.forecast) , type="o",lwd=1, lty=3, pch=21, bg="green3")

# coef(garch.fit)
# alpha<-0.05
# est.coefs<-coef(garch.fit)
# skew.est<- est.coefs["skew"]
# shape.est<- est.coefs["shape"]
# quant<-qdist( "std", c(alpha/2,1-alpha/2), shape=shape.est)
# est.sigma<- c(sigma(garch.fit)[1], sigma(garch.fit) )
# lines(time.n.total[region.obs], exp(fit.val+quant[1]*est.sigma)[region.obs] , col="red", type="l")
# lines(time.n.total[region.obs], exp(fit.val+quant[2]*est.sigma)[region.obs] , col="red", type="l")
# lines(time.n.total[region.forc], exp(cum.forecast.quant[1,]) ,col="red", type="l", lty=2)
# lines(time.n.total[region.forc], exp(cum.forecast.quant[2,]) ,col="red", type="l", lty=2)
# legend("topleft", c("Observed values", "Fitted values", "Predicted values", paste(100*(1-alpha),"% quantiles (obs.)"), paste(100*(1-alpha),"% quantiles (pred.)", sep="")) , lwd=c(1,2,1,1,1), lty=c(3,1,2,1,2), col=c("steelblue", 1,1,"red","red"), pt.bg=c("lightblue",0,"green3" ,0,0), pch=c(21,46,21,46,46), bg=rgb(1,1,0.8) , cex =1.2) ## pch=46 for ’nothing ’

#savePlot(filename="forecasted_stock-prices_TK",type="eps")
# dev.off()


## 2 in 1 - for reporting only!

#savePlot(filename="forecasts 2x2",type="eps")
par(mfrow=c(1,1))





