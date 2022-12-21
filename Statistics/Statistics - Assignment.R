############################################################################################################
##########          Trabalho Pratico - AED                                 #################################
##########          IGTI - Professor Mairon Chaves                         #################################
############################################################################################################


# Builds the dataset
dados <- data.frame(
  Preco = c(368.384514890573, 446.850186825816, 
            414.72765691978, 434.291090918223, 436.652686535348, 457.65797344255, 
            490.694346597566, 474.881781399868, 458.462395897205, 412.719412673294, 
            448.799032112411, 352.040747235864, 449.461858221104, 416.150953927119, 
            416.499426750268, 551.315803331779, 462.126789471159, 515.957335395508, 
            467.598697162974, 339.548470369391), 
  Portas = c("duas_portas", "quatro_portas", "duas_portas", "quatro_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "duas_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "quatro_portas", "duas_portas", 
             "quatro_portas", "duas_portas", "quatro_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "quatro_portas"),
  Ar_Condicionado = c("sem_ar_condicionado",  "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "sem_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "sem_ar_condicionado"),
  Quadrimestre = c("segundo_quadrimestre","segundo_quadrimestre", "segundo_quadrimestre", "segundo_quadrimestre", 
                   "segundo_quadrimestre", "terceiro_quadrimestre", "primeiro_quadrimestre", 
                   "primeiro_quadrimestre", "terceiro_quadrimestre", "segundo_quadrimestre", 
                   "terceiro_quadrimestre", "segundo_quadrimestre", "terceiro_quadrimestre", 
                   "segundo_quadrimestre", "segundo_quadrimestre", "primeiro_quadrimestre", 
                   "terceiro_quadrimestre", "primeiro_quadrimestre", "primeiro_quadrimestre", 
                   "segundo_quadrimestre"), 
  Idade_Locatario = c(23, 18, 28, 21, 18, 21, 18, 20, 25, 29, 18, 33, 20, 21, 18, 21, 18, 20, 25, 29),
  Quilometragem = c(957.442780544097, 829.533278217768, 923.300215829467, 871.519116905113, 930.704105677958, 554.696695914233, 501.941059782271, 
                    665.435074822519, 568.24079543466, 930.704105677958, 554.696695914233, 
                    829.533278217768, 665.435074822519, 871.519116905113, 930.704105677958, 
                    351.547138218644, 501.941059782271, 447.872006186523, 568.24079543466, 
                    930.704105677958), 
  Dolar = c(4.41147933990862, 5.63014407874318, 
            8.80557934010615, 4.260591319988649, 6.93416279643155, 1.61130694543154, 
            2.57813244655973, 4.66666728709914, 1.6846066723224, 7.33872353619711, 
            4.52300814589177, 2.96689816205009, 9.91448182957733, 8.55577847959413, 
            5.93424935955983, 5.55775429484673, 6.94475470863839, 4.74330294976712, 
            4.723306965757987, 4.7010894862212))

View(dados)

hist(dados$Preco)
boxplot(dados$Preco)
summary(dados$Preco)

# Boxplot between Preco and Quadrimestre
boxplot(dados$Preco~ dados$Quadrimestre)

# Analisys of variance
anova <- aov(Preco ~ Quadrimestre, data = dados)
summary(anova)

# Boxplot between Preco and Portas
boxplot(dados$Preco ~ dados$Portas)

# Test t Student
t.test(dados$Preco ~ dados$Portas , 
       paired = FALSE,
       alternative = 'two.sided',
       conf.level = 0.95
)

plot(y = dados$Preco ,
     x = dados$Quilometragem,
     pch = 16)

# Correlation coeficient
cor(dados$Preco, dados$Quilometragem)

# Adjusts linear regression of Preco in function of Quilometragem
regressao_linear <- lm(Preco ~ Quilometragem, data = dados)
summary(regressao_linear)

#Analise descritiva da variavel quilometragem
summary(dados$Quilometragem)

# Correlation coeficient
sd(dados$Quilometragem) / mean(dados$Quilometragem)

# Gets relationship between Dolar and Preco
plot(y = dados$Preco,
     x = dados$Dolar,
     pch = 16)
cor(dados$Preco, dados$Dolar)

regressao_linear <- lm(Preco ~ Dolar, data = dados)
summary(regressao_linear)

