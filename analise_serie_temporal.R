library(tidyverse)
library(zoo)
library(trend)        # Mann-Kendall, Pettitt
library(seasonal)     # X13-ARIMA-SEATS (opcional)
library(bfast)        # Detecção de quebras estruturais
library(Kendall)
library(zyp)

dt <- fread("17130000_mensal.csv")

dt <- mutate(dt, 
             data      = as.Date(mes),                       
             cota_media      = suppressWarnings(as.numeric(cota_media)))   

# ordenar
setorder(dt, data)

# criar componentes auxiliares
dt[, `:=`(
  ano = year(data),
  mes = month(data)
)]

ggplot(dt, aes(x = data, y = cota_media)) +
  geom_line() +
  labs(title = "Série temporal - nível do rio")

# criar objeto ts
ts_cota <- ts(dt$cota, 
              start = c(min(dt$ano), min(dt$mes)), 
              frequency = 12)

# decomposição STL
decomp <- stl(ts_cota, s.window = "periodic")

# plot
plot(decomp)

trend <- decomp$time.series[, "trend"]
residual <- decomp$time.series[, "remainder"]

dt[, trend := as.numeric(trend)]
dt[, resid := as.numeric(residual)]


mk <- MannKendall(dt$trend)
mk


mk_mod <- zyp.trend.vector(dt$cota, method = "yuepilon")
mk_mod


sen <- sens.slope(dt$cota)
sen



### anomalias

# climatologia mensal
clim <- dt[, .(
  media = mean(cota_media, na.rm = TRUE),
  sd = sd(cota_media, na.rm = TRUE)
), by = mes]

# juntar
dt <- merge(dt, clim, by = "mes")

# calcular anomalia
dt[, z := (cota_media - media) / sd]
anomalias <- dt[abs(z) > 2]
extremos <- dt[abs(z) > 3]

ggplot(dt, aes(x = data, y = cota_media)) +
  geom_line() +
  geom_point(data = dt[abs(z) > 2], color = "red", size=3) +
  labs(title = "Anomalias de nível do rio")


pettitt.test(dt$cota_media)
