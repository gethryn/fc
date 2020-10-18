library(forecast)
model <- Arima(ts(rpois(100,45),freq=12), order=c(1,1,1), seasonal=c(1,1,1),
               fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))
foo <- simulate(model, nsim=36)
fit <- Arima(foo, order=c(1,1,1), seasonal=c(1,1,1))
forecast(auto.arima(foo), h=6) %>% autoplot()
forecast(fit, h=6) %>% autoplot()
