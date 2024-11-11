library(factoextra)
library(cluster)
library(dplyr)

################################################
################################################
# Importing the dataset
df = read.csv('data/mall.csv')
aux = as.data.frame(scale(df[4:5]))

#Definição do número de cluster pelo método elbow
fviz_nbclust(aux, kmeans, method = "wss", k.max = 10)

set.seed(1) # for reproducibility

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(aux, centers = 4, nstart = 10)

#visualização dos clusters
fviz_cluster(cluster_kmeans, aux, palette = "Set2", 
             ggtheme = theme(legend.text = element_text(size = 16)))

#Criando variável categórica para indicação do cluster no banco de dados
df$cluster <- as.factor(cluster_kmeans$cluster)

#análise
analise <- df %>% group_by(cluster) %>%
  summarise(across(3:5, mean))

df %>%
  ggplot(aes(x=cluster, y=Age, fill=cluster)) + 
  geom_boxplot() +
  #geom_hline(yintercept=mean(df$Avg_Credit_Limit), col="red")+
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab("Limite CC")


################################################
################################################

#carrega o dataset
df <- read.csv("data/cartao_credito.csv", sep = ",")

aux <- data.frame(scale(df[]))

#elbow
fviz_nbclust(aux, kmeans, method = "wss", k.max = 10)

set.seed(1)

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(aux, centers = 4, nstart = 10)

#visualização dos clusters
fviz_cluster(cluster_kmeans, aux, palette = "Set2", 
             ggtheme = theme(legend.text = element_text(size = 16)))

#Criando variável categórica para indicação do cluster no banco de dados
df$cluster <- as.factor(cluster_kmeans$cluster)

#análise
analise <- df %>% group_by(cluster) %>%
  summarise(across(3:7, mean))

df %>%
  ggplot(aes(x=cluster, y=Avg_Credit_Limit, fill=cluster)) + 
  geom_boxplot() +
  #geom_hline(yintercept=mean(df$Avg_Credit_Limit), col="red")+
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab("Limite CC")

################################################
################################################
df <-  read.csv("data/spotify.csv", sep = ",")

#seleciona uma parte do dataset
n <- nrow(df)
indices <- sample(1:n, size = 0.1 * n, replace = FALSE)
df <- df[indices, ]

aux <- as.data.frame(scale(df[,c(12,13,15)]))

set.seed(1)

#elbow
fviz_nbclust(aux, kmeans, method = "wss", k.max = 10)

set.seed(1)

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(aux, centers = 4, nstart = 10)

#visualização dos clusters
fviz_cluster(cluster_kmeans, aux, palette = "Set2", 
             ggtheme = theme(legend.text = element_text(size = 16)))

#Criando variável categórica para indicação do cluster no banco de dados
df$cluster <- as.factor(cluster_kmeans$cluster)

#análise
analise <- df %>% group_by(cluster) %>%
  summarise(across(12:23, mean))

df %>%
  ggplot(aes(x=cluster, y=loudness, fill=cluster)) + 
  geom_boxplot() +
  #geom_hline(yintercept=mean(df$Avg_Credit_Limit), col="red")+
  theme(legend.position="none") +
  xlab("Cluster") +
  ylab("?")


################################################
# Faça uma análise de cluster do dataset wine
################################################













