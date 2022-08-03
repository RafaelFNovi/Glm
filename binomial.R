library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

dataset <- read_excel("C:/Users/r316998/OneDrive - Cargill Inc/San Antonio/Bases/Matriz Nutron 2019-2021.xlsx", 
                      sheet = "Inputs", skip = 1)

dataset <- dataset %>% select("Pollito 1ra Real","Edad","lote",
                              "Fertilidad Real","Planta")

dataset<- dataset %>%
  group_by(lote,Edad,Planta) %>%
  summarize("Pollito 1ra Real" = mean(`Pollito 1ra Real`, na.rm = TRUE),
          n = n())

dataset <- arrange(dataset, lote, Edad, Planta)   # Garantir a base ordenada

dataset <- na.omit(dataset)
dataset$Planta <- as.factor(dataset$Planta)


ec1 <- as.vector(NULL); ec2 <- as.vector(NULL); ec3 <- as.vector(NULL); ec4 <- as.vector(NULL)

n <- length(dataset$`Pollito 1ra Real`)

for(i in 1:n){
  if(i<=4){
    ec1[i] <- NA
  }
  else if(dataset$lote[i] == dataset$lote[i-4]){
    
    ec1[i] <- dataset$`Pollito 1ra Real`[i-1]
    ec2[i] <- dataset$`Pollito 1ra Real`[i-2]
    ec3[i] <- dataset$`Pollito 1ra Real`[i-3]
    ec4[i] <- dataset$`Pollito 1ra Real`[i-4]
  }
  
  else{
    ec1[i] <- NA
    ec2[i] <- NA
    ec3[i] <- NA
    ec4[i] <- NA
  }
}

dataset$ec1 <- ec1; dataset$ec2 <- ec2; dataset$ec3 <- ec3; dataset$ec4 <- ec4

dataset$lote <- as.factor(dataset$lote)

dataset <- na.omit(dataset)

################ All dataset
model <- glm(`Pollito 1ra Real`~  ec1 + ec2 + ec3 + Edad + lote, 
             data = dataset, family = "binomial")

summary(model)

# z <- -1.17359 + 0.9*3.66864 + 0.9*1.40258 + 0.9*-0.89731 + 50*(-0.01671)
# y <- 1/(1+exp(-z))

# predict(object = model, 
#         data.frame(ec1 = 0.9, ec2 = 0.9, ec3 = 0.9 , Edad = 50), 
#         type = "response")

predito <- predict(model, dataset, type="response")
dataset$predito <- predito
dataset$residuo <- dataset$`Pollito 1ra Real`-dataset$predito
hist(dataset$residuo)
summary(dataset$residuo)
plot(dataset$Edad,dataset$residuo)
plot(dataset$`Pollito 1ra Real`,dataset$predito)

######### Filtrando uma amostra de 200
#dataset <- tail(dataset,200)
dataset_f <- dataset %>% filter(lote == 102)
p <- ggplot(dataset_f,aes(Edad, `Pollito 1ra Real`))+
  geom_point(data=dataset_f,aes(Edad, `Pollito 1ra Real`),colour="blue")+
  geom_smooth(data=dataset_f,aes(Edad, `Pollito 1ra Real`),colour="blue",se=FALSE)+
  geom_point(data=dataset_f, aes(Edad,predito),shape=8)+
  geom_smooth(data=dataset_f, aes(Edad,predito),colour="black",se=FALSE)
p
ggplotly(p)




