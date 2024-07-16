library(forecast)
library(lubridate)

# Leitura e Preparação da Base de Dados
base = read.csv2("ipca.csv")
base_ts = ts(base$IPCA, frequency=12, start=c(1994,1))

# Visualização gráfica
plot(base_ts,type="s")

# Suavização Exponencial Simples
SES = ses(base_ts)

# Suavização Exponencial de Holt
HOLT = holt(base_ts)

# Suavização Exponencial de Holt-Winter
HW_ad = hw(base_ts, seasonal = "additive")

# Resumos Estatísticos dos Modelos
summary(SES)
summary(HOLT)
summary(HW_ad)

# Análise Gráfica
HWa.predito = summary(HW_ad)
lines(fitted(HW_ad), col="blue")
lines(HWa.predito$mean, col="red", lwd=2)
abline(v = c(2023, 10), col = "purple", lty = 2)
