library(caTools)
library(pROC)
library(factoextra)
library(dplyr)
library(caret)

##########################################
#         LOAD FILE
##########################################

#load prepared dataset
df <- read.csv("data/titanic.csv", sep = ",")

#converte Sex para inteiro. 1:masculino, 0:feminino
df$Sex <- ifelse(df$Sex == "male", 1, 0)

df$Survived <- as.factor(df$Survived)

#trata NAs idade
df$Age[is.na(df$Age)] <- mean(df$Age, na.rm = T)
sum(is.na(df$Age))

set.seed(1)

split <- sample.split(df, SplitRatio = 0.7) 

treino <- subset(df, split == "TRUE") 
teste  <- subset(df, split == "FALSE") 

#implementando o modelo
modelo <- glm(Survived ~ Pclass+Sex+Age, data = treino, family = "binomial")

summary(modelo)

predictions <- predict(modelo, teste, type="response")
teste$pred <- round(predictions, 5)

pred.class <- ifelse(predictions < 0.5, "Died", "Survived")
teste$Survived_class <- as.factor(ifelse(teste$Survived == 1, "Survived", "Died"))
teste$pred.class <- pred.class

table(pred.class)
table(teste$Survived_class)

# Model accuracy
mean(pred.class == teste$Survived_class)

#Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
confusionMatrix(as.factor(teste$Survived_class), as.factor(pred.class))

ROC <- roc(response = teste$Survived, predictions, plot = TRUE, print.auc = TRUE)




################################################
# Análise de cluster - demonstração de como
# associar aprendizado não supervisionado com
# aprendizado supervisionado
################################################
df <-  read.csv("data/titanic.csv", sep = ",")

#converte Sex para inteiro. 1:masculino, 0:feminino
df$Sex <- ifelse(df$Sex == "male", 1, 0)

#trata NAs idade
df$Age[is.na(df$Age)] <- mean(df$Age, na.rm = T)
sum(is.na(df$Age))

aux <- as.data.frame(scale(df[,c(3,5,6)]))

set.seed(1)

#elbow
fviz_nbclust(aux, kmeans, method = "wss", k.max = 10)

set.seed(1)

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(aux, centers = 3, nstart = 10)

#visualização dos clusters
fviz_cluster(cluster_kmeans, aux, palette = "Set2", 
             ggtheme = theme(legend.text = element_text(size = 16)))

#Criando variável categórica para indicação do cluster no banco de dados
df$Cluster <- as.factor(cluster_kmeans$cluster)

#ANÁLISE
df$Sex <- ifelse(df$Sex == 1, "M", "F")

#Matriz de contingência
table(df$Cluster, ifelse(df$Survived==1,"Survived", "Died"))
table(df$Sex, ifelse(df$Survived==1,"Survived", "Died"))

#Média Fare e Age
df %>% group_by(Cluster) %>% 
  summarise_at(vars("Fare", "Age"), mean)

hist(df$Age)

#total sobreviventes por cluster
df %>%
  group_by(Cluster, Survived) %>% 
  summarise(Total = n()) %>%
  ggplot(aes(x=Cluster, y=Total, fill=Survived)) + 
  geom_bar(stat = "identity")+
  geom_hline(yintercept=mean(df$Survived==1), col="red")+
  xlab("Cluster") +
  ylab("?")

#proporção sobreviventes por cluster
df %>% group_by(Cluster) %>% 
  summarise(Perc_Survived = mean(Survived==1)*100) %>%
  ggplot(aes(x=Cluster, y=Perc_Survived, fill=Perc_Survived)) + 
  geom_bar(stat = "identity")+
  geom_hline(yintercept=mean(df$Survived==1)*100, col="red")+
  xlab("Cluster") +
  ylab("Proporção")

#Distribuição Sexo
df %>% select(Cluster, Sex) %>% 
  ggplot(aes(x=Cluster, fill=Sex)) + 
  geom_bar(stat = "count")+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  xlab("Cluster") +
  ylab("Proporção")


#proporção sobreviventes por cluster
df %>% group_by(Cluster, Sex, Survived) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=Cluster, y=count, fill=Sex)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  xlab("Cluster") +
  ylab("Proporção") +
  facet_wrap(~Survived, nrow = 2)



