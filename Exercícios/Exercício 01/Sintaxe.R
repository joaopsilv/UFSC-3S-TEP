# Leitura da base de dados
base = read.csv2("apartamento.csv", dec=".")

# Converte os valores de Local para:
#   -> 0 = Região menos valorizada;
#   -> 1 = Região mais valorizada;
base$Local = ifelse(base$Local == 1, 1, 0)

# Tira a amostra de tamanho 80 da base de dados
amostra = base[sample(nrow(base), 80),]

# Cria e mostra um modelo inicial com todas as variáveis
modelo_inicial = lm(Valor ~ Area + Idade + Energia + Local, data=amostra)
summary(modelo_inicial)

# Mostra o modelo depois de retirar a variável energia
modelo_ajustado = lm(Valor ~ Area + Idade + Local, data=amostra)
summary(modelo_ajustado)

# Análise de Resíduos do modelo
plot(fitted(modelo_ajustado), rstandard(modelo_ajustado))
abline(0,0)
library(car)
qqPlot(modelo_ajustado)