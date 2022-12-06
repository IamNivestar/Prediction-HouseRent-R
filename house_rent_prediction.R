##### Amaury Ribeiro 
#### https://github.com/IamNivestar/Prediction-HouseRent-R
#### Predição de aluguel

library(dplyr)
                                  ##carregando base de dados

setwd("C:/Users/Amaury/Desktop/Prediction-HouseRent-R")
df <- read.csv("House_Rent_Dataset.csv", header = TRUE, encoding = "UTF-8")
View(df)
str(df)

                                      ### Limpeza ###

dates <- as.Date(scan( text = df$Posted.On, what = ""), format="%Y-%m-%d")
dates
max(dates) - min(dates)

# não irei considerar o tempo em que foi publicado já que a diferença não ultrapassa 3 meses,
#  caso fossem anos, poderiam haver um influencia externa de uma temporada para outra, como inflação, etc...
df$Posted.On <- NULL

#renomeando algumas colunas
names(df) <- c("BHK", "Rent","Size","Floor", "Area_type","Area_locality", "City", "Furnishing_status", 
               "Tenant_preferred", "Bathroom", "Contact")
View(df)

# retirando a palavra Area desnecessária
df$Area_type <- gsub(" Area", "", df$Area_type)
unique(df$Area_type)
View(df)

unique(df$Contact)
# retirando a palavra Contact desnecessária
df$Contact <- gsub("Contact ", "", df$Contact)
unique(df$Contact)
View(df)

#para andares terra ('ground') vou simbolizar com o zero, "Lower Basement" com -1 e "Upper basemente" como -2
unique(df$Floor)
# retirando a palavra Contact desnecessária
df$Floor <- gsub("Ground", "0", df$Floor)
df$Floor <- gsub("Lower Basement", "-1", df$Floor)
df$Floor <- gsub("Upper Basement", "-2", df$Floor)
unique(df$Floor)
View(df)

#a coluna floor também contém duas informações, irei separa-los

library(stringr)

head(df$Floor)
head(sub(".*of ", "", df$Floor)) #observando se o padrão foi corretamento obtido

df$Total_floor <- sub(".*of ", "", df$Floor)  #salvando o andar extraido em um nova coluna
df <- df %>%  
  relocate(Total_floor, .after = Floor) #pipeline para mudar o local da coluna

df[df$Floor == "-2",] #verificando se os imóveis sem o andar informado foram coletados
df[df$Floor == "-1",] #não houve problemas nos imóveis lower ou upper basement
df[df$Floor == "0",] # houve um caso de um andar terra acabou salvando o total de andares como zero

df <- df %>%   
  mutate( Total_floor = ifelse(Floor == '0', '1', Total_floor)) #, irei corrigir manualmente por ser único

df %>%    #não houve valores nulos na nova coluna
    filter(is.na(Total_floor))

df$Total_floor <- as.numeric(df$Total_floor)  #convertendo para númerico para verificar dados estatisticos
summary(df$Total_floor)

values_test <- c("-2 out of 3", "3 out of 22", "36 out of 81") #testando um padrão para obter o andar atual
str_extract(values_test, "-(\\d+)|(\\d+)")

head(df$Floor)
head(str_extract(df$Floor, "-(\\d+)|(\\d+)"))

df$Current_floor <- str_extract(df$Floor, "-(\\d+)|(\\d+)")
df <- df %>% 
  relocate(Current_floor, .after = Floor)

df$Current_floor <- as.numeric(df$Current_floor)  #convertendo para númerico para verificar dados estatisticos
summary(df$Current_floor)

df$Floor <- NULL  #não precisarei mais dessa coluna pois suas informações ja foram processadas em outros

df[is.na(df),]
str(df)

                      ### salvando processamento ####

write.table(df, file = "house_rent_processed.csv", row.names = F, sep = ",", fileEncoding = "UTF-8")

#########################################################################################################

library(dplyr)
                           ### load new df

setwd("C:/Users/Amaury/Desktop/Prediction-HouseRent-R")
df <- read.csv("house_rent_processed.csv", header = TRUE, encoding = "UTF-8")

                    
                      ## plotando gráficos e informações sobre os dados ##

install.packages("gridExtra")
library(gridExtra)


#aluguel média por cidade
Average_rent <- df %>%
  group_by(City) %>%
  summarise(mean(Rent))

View(Average_rent)

png("average_rent_city.png", height = 50*nrow(Average_rent), width = 200*ncol(Average_rent))
grid.table(Average_rent)
dev.off()

summary (size)

library(ggplot2)

count_floor <- table(df$Total_floor)
View(count_floor)

par(mar=c(3, 3, 3, 1)) #customizando barplot
barplot(count_floor, ) #como esperado, imóveis com poucos andares são bem mais frequentes
 
ggplot(as.data.frame(table(count_floor)), aes(x=count_floor, y= Freq ))+
  geom_bar(stat = 'identity')

barplot( table(df$Furnishing_status), col = c("#E69F00", "#56B4E9", "#009E73") )

size_bhk <- df %>% 
  group_by(BHK) %>% 
  summarize(meanSize = mean(Size))

ggplot(size_bhk, aes( x = BHK, y = meanSize ) )+ geom_bar(stat = 'identity')
       

boxplot(df$Rent, horizontal=TRUE, col='steelblue', xlab="Rent") 
# remover o outlier de 3500000

boxplot(df$Rent[df$Rent < 3500000], horizontal=TRUE, col='steelblue',  xlab="Rent") 
# como é possível observar o dados possuem muitos poucas casas com valores muito altos, isso prejudica
# o modelo e a análise de distribuição.

# percentil 90# preco
aluguel_90p <- quantile(df$Rent,0.9)


boxplot(df$Rent[df$Rent <  aluguel_90p], horizontal=TRUE, col='steelblue', xlab="Rent") 

ggplot(df[df$Rent < aluguel_90p,], aes(x=Area_type, y=Rent, fill=Area_type)) + 
  geom_boxplot( )

ggplot(as.data.frame(table(df$Furnishing_status)), aes(x=Furnishing_status, y= Freq))+
  geom_bar(stat = 'identity')

quantile(df$Rent)
?hist
png(file="plots/histogram_rent_90p.png", width=900, height=900, res=100)
hist(df$Rent[ df$Rent < aluguel_90p], breaks = 5, labels = T, col='orange', main="Histogram of Rent", xlab =  "Rent")
dev.off();


                                    ### Machine Learning   -- Previsão e Resultados ###


aluguel_90p <- quantile(df$Rent,0.9)
df <- df[df$Rent < aluguel_90p,]  #passarei a usar o dataset filtrado


# correlação #

dim(df)

?cor
cor(df$Rent, df$Size, method="spearman")
cor(df$Rent, df$Total_floor, method = "spearman")
cor(df$Rent, df$Current_floor, method= "spearman")
cor(df$Rent, df$Bathroom, method= "spearman")
cor(df$Rent, df$BHK, method= "spearman")

#preprocessamento, mudando tipo das colunas

str(df)

#transformando variaveis categoricas
df$BHK <- as.factor(df$BHK)
df$Current_floor <- as.factor(df$Current_floor)
df$Total_floor <- as.factor(df$Total_floor)
df$Area_type <- as.factor(df$Area_type)
df$City <- as.factor(df$City)
df$Furnishing_status <- as.factor(df$Furnishing_status)
df$Tenant_preferred <- as.factor(df$Tenant_preferred)
df$Bathroom <- as.factor(df$Bathroom)
df$Contact <- as.factor(df$Contact)
#df$Area_locality <- as.factor(df$Area_locality) #não transformarei em factor devido ao limite de levels no modelo GBM

df <- df %>% 
  relocate(Rent, .after = Contact) #realocando aluguel para o final para facilitar a visualizacao

str(df)

# separação treinamento e teste #

dim(df)


new_df <- data.frame(df)
new_df$Area_locality <- as.numeric(as.factor(new_df$Area_locality))
str(new_df)

set.seed(4) #definindo semente pois vou buscar valores aleatórios
samples_rows <- sample(1:length(df$BHK), length(df$BHK)*0.7) # 70% para treino
train_set = new_df[samples_rows,]  # dados de treino
View(train_set )

test_set = new_df[-samples_rows,] # o restante é teste  
dim(train_set)
dim(test_set)

                                     # modelo - Gradient Boosting Regression ##

library(gbm)

gbm_model <- gbm(Rent ~ ., data = train_set,
                 distribution = "gaussian",
                 cv.folds = 10,
                 shrinkage = .01,
                 n.minobsinnode = 10,
                 n.trees = 500)

sqrt(min(gbm_model$cv.error))
gbm.perf(gbm_model, method = "cv")

par(mar = c(5, 8, 1, 1))
summary(
  gbm_model, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
        
gbm_model.training <-predict(gbm_model, train_set)
gbm_model.testing <- predict(gbm_model, test_set)


plot(train_set$Rent, gbm_model.training, col = "blue")
plot(test_set$Rent, gbm_model.testing, col = "red")

MAE(new_df$Rent, predict(gbm_model))
RMSE(new_df$Rent, predict(gbm_model))

cor_train <- cor(train_set$Rent, gbm_model.training)
cor_test <- cor(test_set$Rent, gbm_model.testing)
cat(cor_train^2, cor_test^2) # R2 Score

test_set$predicted <- gbm_model.testing
test_set$porcent_error <- round(test_set$predicted / test_set$Rent, 2)
test_set$porcent_error <- abs(test_set$porcent_error-1)
Error_gbm <- summary(test_set$porcent_error)
Error_gbm

                    #### Segundo Modelo GBM mas com variáveis com pior desempenho removidas ####

new_df <- data.frame(df)
new_df$Area_locality <- NULL
new_df$Tenant_preferred <- NULL
new_df$Area_type <- NULL
new_df$Furnishing_status <- NULL

str(new_df)

train_set = new_df[samples_rows,]  # dados de treino
test_set = new_df[-samples_rows,] # o restante é teste

gbm_model <- gbm(Rent ~ ., data = train_set,
                 distribution = "gaussian",
                 cv.folds = 10,
                 shrinkage = .01,
                 n.minobsinnode = 10,
                 n.trees = 500)

sqrt(min(gbm_model$cv.error))
gbm.perf(gbm_model, method = "cv")

par(mar = c(5, 8, 1, 1))
summary(
  gbm_model, 
  cBars = 10,
  method = relative.influence, 
  las = 2
)

gbm_model.training <-predict(gbm_model, train_set)
gbm_model.testing <- predict(gbm_model, test_set)

plot(train_set$Rent, gbm_model.training, col = "blue")
plot(test_set$Rent, gbm_model.testing, col = "red")

MAE(new_df$Rent, predict(gbm_model))
RMSE(new_df$Rent, predict(gbm_model))

cor_train <- cor(train_set$Rent, gbm_model.training)
cor_test <- cor(test_set$Rent, gbm_model.testing)
cat(cor_train^2, cor_test^2) # R2 Score

test_set$predicted <- gbm_model.testing
test_set$porcent_error <- round(test_set$predicted / test_set$Rent, 2)
test_set$porcent_error <- abs(test_set$porcent_error-1)
Error_gbm <- summary(test_set$porcent_error)
Error_gbm

                                    ### Modelo - linear regression ###

new_df <- data.frame(df)

new_df$Area_locality <- NULL
new_df$Tenant_preferred <- NULL
new_df$Area_type <- NULL
new_df$Furnishing_status <- NULL

train_set = new_df[samples_rows,]  # dados de treino
test_set = new_df[-samples_rows,] # o restante é teste  


library(caret)

lm_model <- lm(Rent ~ ., data = train_set)

summary(lm_model)

                                       ###  modelo arvores de regressão ##

new_df <- data.frame(df)

new_df$Area_locality <- NULL
new_df$Tenant_preferred <- NULL
new_df$Area_type <- NULL
new_df$Furnishing_status <- NULL

train_set = new_df[samples_rows,]  # dados de treino
test_set = new_df[-samples_rows,] # o restante é teste  

library(rpart)

tree_model <- rpart( Rent ~ .,
                     data= train_set,
                     control = rpart.control(cp=0.001))

tree_model.training <- predict(tree_model, train_set)
tree_model.testing <- predict(tree_model, test_set)

cor_train <- cor(train_set$Rent, tree_model.training)
cor_test <- cor(test_set$Rent, tree_model.testing)
cat(cor_train^2, cor_test^2) #r2Score

test_set$predicted <- tree_model.testing
test_set$porcent_error <- round(test_set$predicted / test_set$Rent, 2)
test_set$porcent_error <- abs(test_set$porcent_error-1)

Error_sum <- summary(test_set$porcent_error)
Error_sum

                                      ##### Modelo SVMR ######


library(e1071)
library(caret)

new_df <- data.frame(df)
new_df$Area_locality <- NULL
new_df$Tenant_preferred <- NULL
new_df$Area_type <- NULL

train_set = new_df[samples_rows,]  # dados de treino
test_set = new_df[-samples_rows,] # o restante é teste

svm_model <- svm(Rent ~ ., data=train_set)

summary(svm_model)

svm_model.training <-predict(svm_model, train_set)
svm_model.testing <- predict(svm_model, test_set)

par(mar = c(5, 5, 1, 1))
plot(train_set$Rent, svm_model.training, col = "blue", )
abline(coef = c(0, 1))
plot(test_set$Rent, svm_model.testing, col = "red")
abline(coef = c(0, 1))

MAE(new_df$Rent, predict(svm_model))
RMSE(new_df$Rent, predict(svm_model))

cor_train <- cor(train_set$Rent, svm_model.training)
cor_test <- cor(test_set$Rent, svm_model.testing)
cat(cor_train^2, cor_test^2) # R2 Score

test_set$predicted <- svm_model.testing
test_set$porcent_error <- round(test_set$predicted / test_set$Rent, 2)
test_set$porcent_error <- abs(test_set$porcent_error-1)
Error_svm <- summary(test_set$porcent_error)
Error_svm
