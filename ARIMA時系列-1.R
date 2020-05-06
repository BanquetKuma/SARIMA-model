# 必要なライブラリのインポート
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

plot(dd, main="ARIMA(2,1,2)")

data <- window(AirPassengers, end=c(1958, 12))

# 季節周期成分を分離する
plot(stl(AirPassengers,s.window ="per"))

# 単位根検定を行う
adf.test(AirPassengers)

ggtsdisplay(data)


# AICによるモデル選択（遊び）
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

# 予測
forecast(model.1, level = c(50,95), h = 10)

plot(forecast(model.1, level = c(50,95), h = 10),
     shadecols=c("yellow", "orange"),
     xlim=c(380, 410),
     lwd=2)

# 長期予測
forecast(model.1, level = c(50,95), h = 100)
plot(forecast(model.1, level = c(50,95), h = 100),
     shadecols=c("yellow", "orange"))

# 平均値をグラフに引く
abline(h=mean(d))

# AICによるモデル選択（遊び2）
model.2 <- auto.arima(
  dd,
  ic="aic",
  trace=T,
  stepwise=F,
  approximation=F
)

# AICによるモデル選択（本番）
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

# AICを個別に確認
arima(data,
      order=c(1,1,0),
      seasonal=list(order=c(1,1,1))
)

# AICを個別に確認
arima(data,
      order=c(1,1,0),
      seasonal=list(order=c(0,1,0))
)

# ARIMA（3,0,2）でモデル構築
ARIMA302<-arima(data,
      order=c(3,0,2),
      seasonal=list(order=c(0,0,0))
)

ARIMA302

checkresiduals(ARIMA302)

tsdiag(ARIMA302)

plot(ARIMA302)

# AICが最小となったARIMA（4,1,1）でモデル構築
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

# ARIMA(4,1,1)の予測結果をプロット
plot(
  forecast(ARIMA411, level = c(95), h = 100),
  shadecols=c("yellow"),
  fcol=2,
  flwd=2,
  type="o",
  main="ARIMA(3,0,2)"
)

lines(AirPassengers, type="o", lwd=2)

abline(h=mean(data))
