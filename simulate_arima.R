library(forecast)
model <- Arima(ts(rpois(100,45),freq=12), order=c(1,1,1), seasonal=c(1,1,1),
               fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))
foo <- simulate(model, nsim=36)
fit <- Arima(foo, order=c(1,1,1), seasonal=c(1,1,1))
forecast(auto.arima(foo), h=6) %>% autoplot()
forecast(fit, h=6) %>% autoplot()


library(smooth) 
# https://cran.r-project.org/web/packages/smooth/vignettes/simulate.html
# https://cran.r-project.org/web/packages/smooth/vignettes/smooth.html

ourSimulation <- sim.es("ANN", frequency=12, obs=120)
plot(ourSimulation)

ourSimulation <- sim.es("MNN", frequency=12, obs=120, probability=0.2, initial=10, persistence=0.1)
plot(ourSimulation)

ourSimulation <- sim.ces("s",frequency=24, obs=240, nsim=1)
plot(ourSimulation)

ourSimulation$initial[c(1:5,20:24),] <- 0
ourSimulation <- sim.ces("s",frequency=24, obs=120, nsim=1, initial=ourSimulation$initial, randomizer="rt", df=4)
plot(ourSimulation)

ourSimulation <- sim.ces("f",frequency=12, obs=240, nsim=10)
plot(ourSimulation)

ourSimulation <- sim.sma(36,frequency=12, obs=36, nsim=1)
plot(ourSimulation)



# generate a quarterly series from a local level plus seasonal model
require(stsm)
pars <- c(var1 = 300, var2 = 10, var3 = 100)
m <- stsm.model(model = "llm+seas", y = ts(seq(120), frequency = 4), 
                pars = pars, nopars = NULL,)
ss <- char2numeric(m)
set.seed(123)
y <- datagen.stsm(n = 120, model = list(Z = ss$Z, T = ss$T, H = ss$H, Q = ss$Q), 
                  n0 = 20, freq = 4, old.version = TRUE)$data
plot(y, main = "data generated from the local-level plus seasonal component")
