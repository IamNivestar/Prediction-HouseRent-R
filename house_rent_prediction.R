
#carregando base de dados

setwd("C:/Users/Amaury/Desktop/house_rent_prediction_model")
df <- read.csv("House_Rent_Dataset.csv", header = TRUE, encoding = "UTF-8")
View(df)
str(df)

# Limpeza : 

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


#pipeline para criação do aluguel média por cidade
Average_rent <- df %>%
  group_by(City) %>%
  summarise(mean(Rent))

View(Average_rent)
  
library("dplyr")