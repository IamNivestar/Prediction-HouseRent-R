##### Amaury Ribeiro 
#### https://github.com/IamNivestar/Prediction-HouseRent-R
#### Predição de aluguel


#carregando base de dados

setwd("C:/Users/Amaury/Desktop/Prediction-HouseRent-R")
df <- read.csv("House_Rent_Dataset.csv", header = TRUE, encoding = "UTF-8")
View(df)
str(df)

### Limpeza ###

dates <- as.Date(scan( text = df$Posted.On, what = ""), format="%Y-%m-%d")
dates
max(dates) - min(dates)

# não irei considerar o tempo em que foi publicado já que a diferença não ultrapassa 3 meses,
# caso fossem anos, poderiam haver um influencia externa de uma temporada para outra, como inflação, etc...
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
library(dplyr)

df$Total_floor <- sub(".*of ", "", df$Floor) 

  
df[df$Floor == "-2",] #verificando se os imóveis sem o andar informado foram coletados
df[df$Floor == "-1",] #não houve problemas nos imóveis lower ou upper basement
df[df$Floor == "0",] # houve um caso de um andar sendo salvo como zero, irei corrigir manualmente por ser único

df <- df %>%   #pipeline para modificação
  mutate( Total_floor = ifelse(Floor == '0', 1, Total_floor))

df %>%    #não houve valores nulos na nova coluna
    filter(is.na(Total_floor))

df$Total_floor <- as.numeric(df$Total_floor)  #convertendo para númerico
summary(df$Total_floor)

df[is.na(df),]
str(df)


## plotando gráficos e informações sobre os dados ##

#aluguel média por cidade
Average_rent <- df %>%
  group_by(City) %>%
  summarise(mean(Rent))

View(Average_rent)

summary (size)

library(ggplot2)


count_floor <- table(df$Total_floor)
View(count_floor)

par(mar=c(3, 3, 3, 1))
barplot(count_floor, ) #como esperado, imóveis com poucos andares são bem mais frequentes

ggplot(as.data.frame(table(count_floor)), aes(x=count_floor, y= Freq))+
  geom_bar(stat = 'identity')


### Machine Learning ###
# separação treinamento e teste #

dim(df)

set.seed(4)
sample()

samples_rows <- sample(1:length(df$BHK), length(df$BHK)*0.7)
train = df[samples_rows,]  # pegando 70% para treino
View(train)

teste = df[-samples_rows,] # o restante é teste

# correlação #

pairs(df)

?cor
cor(df$Rent, y=df$Size, method="spearman")

install.packages("pheatmap")
library("pheatmap")

pheatmap(houses_cor, display_numbers = TRUE)


### criando modelos ##

library(rpart)

modelo <- rpart( Rent ~ .,
                 data= train,
                 control = rpart.control(cp=0))

## Previsões e Resultados ##
