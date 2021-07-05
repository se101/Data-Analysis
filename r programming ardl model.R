df <- read.csv(file.choose())
head(df)

# declare the time series variables     

p <- ts(df$Prev.Close, start = c(2015,1,1), frequency = 12)
o <- ts(df$Open, start = c(2015,1,1), frequency = 12)

# plot the data

ts.plot(p)
ts.plot(o)

# check for stationary 
library(aTSA)
stationary.test(p,method="pp")
stationary.test(o,method="pp")
################
data("df")
check.ts <-is.ts(df) # "is structured as time series?"
check.ts

df.ts <- ts(df, start=c(2015,1), end=c(2015,12),frequency=12) 
df.ts # Check the Data
df.ts.tab <- cbind(df.ts, lag(df.ts[,2], -1), diff(df.ts[,2], lag=1),
                     lag(df.ts[,1], -1), lag(df.ts[,1], -2), lag(df.ts[,1], -3))
df.ts.tab

library(kableExtra)
kable(head(df.ts.tab), caption="The `df` dataset with differences and lags", col.names=c("p","o","oL1","do","pL1","pL2","pL3"))

install.packages("knitr")
library(dynlm)
dfL3.dyn <- dynlm(d(o)~L(p, 0:3), data=df.ts)
summary(dfL3.dyn)
dfL2.dyn <- dynlm(d(o)~L(p, 0:2), data=df.ts)
summary(dfL2.dyn)
library(kableExtra)
kable(tidy(summary(okunL3.dyn)), digits=4, caption="The `okun` distributed lag model
with three lags")


plL3 <- broom::glance(dfL3.dyn )[c("r.squared","statistic","AIC","BIC")]
plL2 <- broom::glance(dfL2.dyn )[c("r.squared","statistic","AIC","BIC")]
tabl <- rbind(glL3, as.numeric(glL2))

knitr::kable (tabl, caption="Goodness-of-fit statistics for `df` models")

plot(df.ts[,1], ylab="Prev Close")
plot(df.ts[,2], ylab="Open")

ppL1 <- data.frame(cbind(df.ts[,1], lag(df.ts[,1],-1)))
names(ppL1) <- c("p","pL1")
plot(ppL1)
meang <- mean(ppL1$p, na.rm=TRUE)
abline(v=meang, lty=2)
abline(h=mean(ppL1$pL1, na.rm=TRUE), lty=2)

ppL2 <- data.frame(cbind(df.ts[,1], lag(df.ts[,1],-2)))
names(ppL2) <- c("p","pL2")
plot(ppL2)
meang <- mean(ppL2$p, na.rm=TRUE)
abline(v=meang, lty=2)
abline(h=mean(ppL2$pL2, na.rm=TRUE), lty=2)

Du <- diff(phill.ts[,"o"])

previous_close_stocks <- df.ts[,1]
acf(previous_close_stocks)

library(dynlm)
df.dyn <- dynlm(p~diff(o),data=df.ts)
residualdf <- resid(df.dyn)
plot(residualdf)
abline(h=0, lty=2)
corrgm <- acf(residualdf)
plot(corrgm)

library(IMTest)
library(BGData)
library(zoo)
library("lmtest")
a <- bgtest(df.dyn, order=1, type="F", fill=0)
b <- bgtest(df.dyn, order=1, type="F", fill=NA)
c <- bgtest(df.dyn, order=4, type="Chisq", fill=0)
d <- bgtest(df.dyn, order=4, type="Chisq", fill=NA)
dfr <- data.frame(rbind(a[c(1,2,4)], b[c(1,2,4)], c[c(1,2,4)], d[c(1,2,4)]))
dfr <- cbind(c("1, F, 0", "1, F, NA", "4, Chisq, 0", "4, Chisq, NA"), dfr)
names(dfr)<-c("Method", "Statistic", "Parameters", "p-Value")
dfr
kable(dfr, caption="Breusch-Godfrey test for the Phillips example")
dwtest(df.dyn)

library(sandwich)
s0 <- coeftest(df.dyn)
s1 <- coeftest(df.dyn, vcov.=vcovHAC(df.dyn))
s2 <- coeftest(df.dyn, vcov.=NeweyWest(df.dyn))
s3 <- coeftest(df.dyn, vcov.=kernHAC(df.dyn))
tbl <- data.frame(cbind(s0[c(3,4)],s1[c(3,4)], s2[c(3,4)],s3[c(3,4)]))
names(tbl) <- c("Incorrect","vcovHAC", "NeweyWest", "kernHAC")
row.names(tbl) <- c("(Intercept", "Du")
kable(tbl, digits=3, caption="Comparing standard errors for the Phillips
model")

ac <- acf(ehat, plot=FALSE)
ac$acf[2:6]

library(dynlm)
df.gen <- dynlm(p~L(p)+d(o)+L(d(o)), data=df.ts)
s.gen <- summary(df.gen)
df1.gen <- dynlm(p~lag(p, -1)+diff(o)+lag(diff(o), -1), data=df.ts)
summary(df.gen)
summary(df1.gen)
kable(tidy(df.gen), caption="Using dynlm with L and d operators")
kable(tidy(df1.gen), caption="Using dynlm with lag and diff operators")

df.ar2 <- dynlm(p~L(p)+L(p,2), data=df.ts)
summary(df.ar2)
kable(tidy(df.ar2), digits=4, caption="Autoregressive model of order 2 using the
dataset $okun$")

res.ar2 <- resid(df.ar2)
library(forecast)
Acf(res.ar2, lag.max=12)

aics <- rep(0,5)
bics <- rep(0,5)
y <- okun.ts[,1]
for (i in 1:5){
  ari <- dynlm(y~L(y,1:i), start=i)
  aics[i] <- AIC(ari)
  bics[i] <- BIC(ari)
}
tbl <- data.frame(rbind(aics, bics))
names(tbl) <- c("1","2","3","4","5")
row.names(tbl) <- c("AIC","BIC")
tbl
kable(tbl, digits=1, align='c', caption="Lag order selection for an AR model")

library(dynlm)
y <- df.ts[,1]
p.ar2 <- dynlm(y~L(y, 1:2))
summary(p.ar2)
kable(tidy(g.ar2), caption="The AR(2) growth model")

library(forecast)
ar2p <- ar(y, aic=FALSE, order.max=2, method="ols")
fcst <- data.frame(forecast(ar2p, 3))
fcst
kable(fcst, digits=3, caption="Forcasts for the AR(2) growth model")
plot(forecast(ar2p,3))
