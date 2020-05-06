library(forecast)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(tseries)

set.seed(1)
d <- arima.sim(
  n=400,
  model=list(order=c(0,1,3),ma=c(-0.5,0.3,0.3)),
  sd=sqrt(1)
)

plot(d, main="ARIMA(2,0,2)")

set.seed(1)
dd <- arima.sim(
  n=400,
  model=list(order=c(2,1,2), ar=c(0.5,0.4), ma=c(-0.5,0.3)),
  sd=sqrt(1)
)

plot(dd, main="蜥????驕守ｨ具ｼ哂RIMA(2,1,2)")

# 鬟幄｡梧ｩ????荵怜ｮ｢謨ｰ縺ｮ???繝ｼ繧ｿ
data <- window(AirPassengers, end=c(1958, 12))

# 季節周期成分を分離する
plot(stl(AirPassengers,s.window ="per"))

# 単位根検定を行う
adf.test(AirPassengers)

ggtsdisplay(data)

plot(data, main="鬟幄｡梧ｩ????荵怜ｮ｢謨ｰ???繝ｼ繧ｿ")

# AIC蝓ｺ貅悶〒繝｢???繝ｫ繧剃ｽ懊ｋ
model.1 <- auto.arima(
  d,
  ic="aic",
  trace=T,
  stepwise=F,
  approximation=F,
  start.p=0,
  start.q=0,
  start.P=0,
  start.Q=0
)

tsdiag(model.1)

# 遏ｭ?悄莠域ｸｬ
forecast(model.1, level = c(50,95), h = 10)

plot(forecast(model.1, level = c(50,95), h = 10),
     shadecols=c("yellow", "orange"),
     xlim=c(380, 410),
     lwd=2)

# 髟ｷ譛滉ｺ域ｸｬ
forecast(model.1, level = c(50,95), h = 100)?
plot(forecast(model.1, level = c(50,95), h = 100),
     shadecols=c("yellow", "orange"))

# 蟷ｳ???蛟､繧貞ｼ輔￥
abline(h=mean(d))

# ???????驕守ｨ????繝｢???繝ｪ繝ｳ繧ｰ
model.2 <- auto.arima(
  dd,
  ic="aic",
  trace=T,
  stepwise=F,
  approximation=F
)

# 蟄｣遽螟牙虚???繝ｼ繧ｿ縺ｫ蟇ｾ縺吶ｋ繝｢???繝ｪ繝ｳ繧ｰ
model.Air <- auto.arima(
  data,
  ic="aic",
  trace=T,
  stepwise=T,
  approximation=F,
  allowdrift=F,
  start.p=0,
  start.q=0,
  start.P=0,
  start.Q=0,
)

# trace縺後♀縺九＠??????繝ｼ繧ｿ縺ｮAIC繧定ｪｿ縺ｹ???
arima(data,
      order=c(1,1,0),
      seasonal=list(order=c(1,1,1))
)

# ?IC縺御ｸ逡ｪ菴弱＞繝｢???繝ｫ縺ｮAIC
arima(data,
      order=c(1,1,0),
      seasonal=list(order=c(0,1,0))
)

# Python???繧ｭ繧ｹ繝郁ｼ峨▲縺ｦ???繧倶ｾ具ｼ亥柱???驕守ｨ九〒縺ｪ??????
ARIMA302<-arima(data,
      order=c(3,0,2),
      seasonal=list(order=c(0,0,0))
)

ARIMA302

checkresiduals(ARIMA302)

tsdiag(ARIMA302)

plot(ARIMA302)

# Python???繧ｭ繧ｹ繝郁ｼ峨▲縺ｦ???繧倶ｾ具ｼ亥柱????驕守ｨ九〒縺ゅｋ???
ARIMA411<-arima(data,
                order=c(4,1,1),
                seasonal=list(order=c(0,0,0))
)

checkresiduals(ARIMA411)

tsdiag(ARIMA411)

plot(
  forecast(model.Air, level = c(95), h = 24),
  shadecols=c("yellow"),
  fcol=2,
  flwd=2,
  xlim=c(1949, 1961),
  ylim=c(100, 605),
  type="o",
  xaxt = "n",
  main="ARIMA(1,1,0)(0,1,0)[12]"
)

lines(AirPassengers, type="o", lwd=2)

Axis(
  at = seq(1958, 1961),
  side = 1
)

# ARIMA(4,1,1)縺ｧ縺ｮ莠域ｸｬ
plot(
  forecast(ARIMA411, level = c(95), h = 100),
  shadecols=c("yellow"),
  fcol=2,
  flwd=2,
  # xlim=c(1949, 1961),
  #ylim=c(100, 600),
  type="o",
  #xaxt =?"n",
  main="ARIMA(3,0,2)"
)


lines(AirPassengers, type="o", lwd=2)

abline(h=mean(data))

#Axis(
  #at = seq(1949, 1961),
  #side = 1
#)
