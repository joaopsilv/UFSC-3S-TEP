# Leitura da base de dados
base = read.csv2("titanic_data.csv", dec=".")

# Tira a amostra de tamanho 300 da base de dados
set.seed(29072003)
amostra = base[sample(nrow(base), 300),]

# Classifica as variáveis qualitativas independentes como fatores
amostra$Pclass = as.factor(amostra$Pclass)
amostra$Sex = as.factor(amostra$Sex)
amostra$Embarked = as.factor(amostra$Embarked)

# Cria um modelo zero e constrói um modelo adequado
modelo_zero = glm(Survived ~ 1, family = binomial(), data=amostra)
modelo_adequado = step(modelo_zero,list(lower = ~ 1,
                                        upper = ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked),
                       direction="forward")
summary(modelo_adequado)

# Razão de Chances
OR = data.frame(exp(modelo_adequado$coefficients))
IC = data.frame(exp(confint(modelo_adequado)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
IC_OR