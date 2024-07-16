# Leitura da base de dados
base = read.csv2("car_base.csv", dec=".")

# Retirada dos valores 4wd de drivewheel
base_sem_4wd = base[base$drivewheel != "4wd",]

# Tira a amostra de tamanho 120 da base de dados
set.seed(29072003)
amostra = base_sem_4wd[sample(nrow(base_sem_4wd), 120),]

# Cria e mostra um modelo
modelo = lm(price ~ carwidth * drivewheel, data = amostra)
summary(modelo)

# Mostra o modelo depois de retirar a interação
modelo_sem_interacao = lm(price ~ carwidth + drivewheel, data = amostra)
summary(modelo_sem_interacao)

# Cria gráfico de interação com o modelo antigo
library(ggplot2)
ggplot(data = modelo, aes(x = carwidth, y = price, color = drivewheel)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gráfico da Interação",
       x = "Largura do Carro",
       y = "Preço")

# Define os valores das variáveis independentes para o carro
carro_especifico = data.frame(carwidth = 70, drivewheel = "fwd")

# Constrói e mostra os intervalos de confiança e de predição, respectivamente
conf_interval <- predict(modelo_sem_interacao, newdata = carro_especifico, interval = "confidence", level = 0.95)
pred_interval <- predict(modelo_sem_interacao, newdata = carro_especifico, interval = "prediction", level = 0.95)

conf_interval
pred_interval
