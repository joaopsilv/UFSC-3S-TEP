# Leitura da base de dados
base = read.csv2("selecao.csv", dec=".")

# Tira a amostra de tamanho 1000 da base de dados
set.seed(29072003)
amostra = base[sample(nrow(base), 1000),]

# Cria um modelo zero
modelo_zero_1 = lm(y ~ 1, data=amostra)

# Constrói um modelo adequado
modelo_adequado_1 = step(modelo_zero_1,list(lower = ~ 1,
                upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),
        direction="forward")
summary(modelo_adequado_1)

# Análise de resíduos do modelo
plot(fitted(modelo_adequado_1), rstandard(modelo_adequado_1))
abline(0,0)

# Ajustes de modelos
modelo_ajustado_1 = lm(log(y) ~ x7+x10+x9+x6+x4+x5+x8, data=amostra)
summary(modelo_ajustado_1)

plot(fitted(modelo_ajustado_1), rstandard(modelo_ajustado_1))
abline(0,0)

modelo_ajustado_2 = lm(log(y) ~ x7+x10+log(x9)+x6+x4+x5+x8, data=amostra)
summary(modelo_ajustado_2)

plot(fitted(modelo_ajustado_2), rstandard(modelo_ajustado_2))
abline(0,0)

# Constrói um novo modelo adequado
modelo_zero_2 = lm(log(y) ~ 1, data=amostra)
modelo_adequado_2 = step(modelo_zero_2,list(lower = ~ 1,
                                            upper = ~ x7+x10+log(x9)+x6+x4+x5+x8),
                         direction="forward")
summary(modelo_adequado_2)

plot(fitted(modelo_adequado_2), rstandard(modelo_adequado_2))
abline(0,0)