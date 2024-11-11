library(dplyr)
library(kableExtra)
library(ggplot2)
library(jtools)
library(equatiomatic)
library(PerformanceAnalytics)
library(corrplot)
library(caret)
################################################################################
#                            LOAD DATASET                                      #
################################################################################
df <- read.csv("data/co2_emissions_canada.csv", sep=",")

summary(df)

names(df) <- c("make", "model", "class", "engine", "cylinders", "transmission",
               "fuel.type", "fuel.c.city", "fuel.c.hwy", "fuel.c.comb", 
               "fuel.mpg", "co2_emissions")

#seleciona as variáveis de interesse
aux <- df[,c(4,5,8,9,10,11,12)]

################################################################################
#                    AVALIA CORRELAÇÃO ENTRE AS VARIÁVEIS                      #
################################################################################
#Avalia a correlação entre as variáveis
chart.Correlation(aux, histogram=FALSE, method = "pearson")

aux <- df[,c("co2_emissions", "fuel.mpg")]
aux$fuel.mpg2 <- aux$fuel.mpg ^2
aux$fuel.mpg3 <- aux$fuel.mpg ^3

#Estimando o modelo
modelo <- lm(formula = co2_emissions ~ fuel.mpg+fuel.mpg2+fuel.mpg3, data = aux)
modelo <- lm(formula = co2_emissions ~ fuel.mpg, data = aux)

#Observando os parâmetros do modelo_tempodist
summary(modelo)

#coeficientes do modelo
modelo$coefficients

#equação da reta y = B0 + B1x / y = a + bx
extract_eq(modelo, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
df$predito <- modelo$fitted.values
df$residuo <- modelo$residuals

# R2
R2(df$predito, df$co2_emissions)

# Calcular o Erro Médio Absoluto (MAE)
mae <- mean(abs(df$co2_emissions - df$predito))
print(paste("MAE:", mae))

# Calcular o Erro Quadrático Médio (MSE)
mse <- mean((df$co2_emissions - df$predito)^2)
print(paste("MSE:", mse))

# Calcular a Raiz do Erro Quadrático Médio (RMSE)
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))

#Dispersão
ggplot(aux, aes(x = co2_emissions, y = fuel.mpg3)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  labs(x = "X", y = "Y", title = paste("R²:",
                                     round(((cor(aux$co2_emissions, aux$fuel.mpg3))^2),4))) +
  scale_color_manual("Legenda:",
                     values = c("#55C667FF", "grey50", "#440154FF")) +
  theme_classic()

#analise resíduos
#se houver padrão, indicativo de não linearidade dos dados
ggplot(df, aes(x = co2_emissions, y = residuo)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  theme_classic()

#Fazendo predições 
predict(object = modelo, data.frame(fuel.mpg = 43))

################################################################################
#                            LOAD DATASET2                                      #
################################################################################
df <- read.csv("data/co2_emissions_canada.csv", sep=",")

summary(df)

names(df) <- c("make", "model", "class", "engine", "cylinders", "transmission",
               "fuel.type", "fuel.c.city", "fuel.c.hwy", "fuel.c.comb", 
               "fuel.mpg", "co2_emissions")

#seleciona as variáveis de interesse
aux <- df[,c(4,5,8,9,10,11,12)]

################################################################################
#                    AVALIA CORRELAÇÃO ENTRE AS VARIÁVEIS                      #
################################################################################
#Avalia a correlação entre as variáveis
chart.Correlation(aux, histogram=FALSE, method = "pearson")

#Estimando o modelo
modelo <- lm(formula = co2_emissions ~ fuel.mpg+cylinders+engine, data = df)

#Observando os parâmetros do modelo_tempodist
summary(modelo)

#coeficientes do modelo
modelo$coefficients

#equação da reta y = B0 + B1x / y = a + bx
extract_eq(modelo, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
df$yhat    <- modelo$fitted.values
df$residuo <- modelo$residuals

# R2
R2(df$yhat, df$co2_emissions)

# Calcular o Erro Médio Absoluto (MAE)
mae <- mean(abs(df$co2_emissions - df$yhat))
print(paste("MAE:", mae))

# Calcular o Erro Quadrático Médio (MSE)
mse <- mean((df$co2_emissions - df$yhat)^2)
print(paste("MSE:", mse))

# Calcular a Raiz do Erro Quadrático Médio (RMSE)
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))

#erro médido em percentual
rmse / mean(df$co2_emissions) * 100

#Fazendo predições 
predict(object = modelo, data.frame(fuel.mpg = 50, cylinders = 4, engine = 2))


################################################################################
#                            LOAD DATASET3                                     #
################################################################################
df <- read.csv("data/dados_paises.csv", sep=",")

summary(df)

################################################################################
#                    AVALIA CORRELAÇÃO ENTRE AS VARIÁVEIS                      #
################################################################################
#Avalia a correlação entre as variáveis
chart.Correlation(df[,2:10], method = "pearson")

#Estimando o modelo
modelo <- lm(formula = life_expec ~ child_mort+gdpp, data = df)

#Observando os parâmetros do modelo_tempodist
summary(modelo)

#coeficientes do modelo
modelo$coefficients

#equação da reta y = B0 + B1x / y = a + bx
extract_eq(modelo, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
df$yhat    <- modelo$fitted.values
df$residuo <- modelo$residuals

# R2
R2(df$yhat, df$life_expec)

# Calcular o Erro Médio Absoluto (MAE)
mae <- mean(abs(df$life_expec - df$yhat))
print(paste("MAE:", mae))

# Calcular o Erro Quadrático Médio (MSE)
mse <- mean((df$life_expec - df$yhat)^2)
print(paste("MSE:", mse))

# Calcular a Raiz do Erro Quadrático Médio (RMSE)
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))

#analise resíduos
#se houver padrão, indicativo de não linearidade dos dados
ggplot(df, aes(x = life_expec, y = residuo)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  theme_classic()

#Fazendo predições 
predict(object = modelo, data.frame(child_mort = 10, gdpp = 100))








atual <- c(110, 120, 115, 125, 130, 118, 121, 124, 122, 117, 119, 123, 126, 128, 113, 127, 129, 116, 114, 111, 125, 123, 120, 118, 119, 122, 124, 126, 127, 130)
novo <- c(100, 105, 98, 102, 104, 101, 99, 97, 96, 100, 103, 99, 98, 101, 102, 95, 97, 96, 100, 103, 99, 97, 101, 98, 100, 102, 99, 103, 100, 104)

t.test(atual, novo)



