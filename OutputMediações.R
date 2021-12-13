################Pacotes##############
library(haven)
library(memisc)
library(desc)
library(descr)
library(sjlabelled)
library(labelled)
library(plyr)
library(tigerstats)
library(sjPlot)
library(coefplot)
library(olsrr)

####Atualização do artigo####

###Uruguai
##2014
Uru2014 <- read_dta("Uru2014.dta")
Uru2014 <- remove_all_labels(Uru2014)
#Tolerância
Uru2014$TolerHomo <- Uru2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2014$Denom <-  recode(Uru2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))

table(Uru2014$Denom)
# 1   2   3   4 
# 534 173 272 422 
Uru2014$Denom <- recode(Uru2014$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Uru2014$Denom <- as.factor(Uru2014$Denom)
Uru2014$Denom <- as.numeric(Uru2014$Denom)

#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2014$IntRelig <- recode(Uru2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2014, mean)
aggregate(TolerHomo ~ AtRelig, Uru2014, mean)
summary(Uru2014$TolerHomo)


modelo4U <- lm(TolerHomo ~ Denom + IntRelig, data = Uru2014)
summary(modelo4U)

tab_model(modelo4U, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

plot(modelo4U)
save(Uru2014, file = "Uru2014.RData")

obj1 <- coefplot(modelo4U, title = "2014",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 


ols_vif_tol(modelo4U)
#     Variables Tolerance      VIF
#1    Denom2 0.8379132 1.193441
#2    Denom3 0.6928497 1.443314
#3    Denom4 0.5910941 1.691778
#4  IntRelig 0.6304810 1.586091

###Uruguai
##2016
Uru2016 <- read_dta("Uru2016.dta")
Uru2016 <- remove_all_labels(Uru2016)
#Tolerância
Uru2016$TolerHomo <- Uru2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2016$Denom <-  recode(Uru2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Uru2016$Denom <- as.factor(Uru2016$Denom)
Uru2016$Denom <- as.numeric(Uru2016$Denom)

#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2016$AtRelig <- recode(Uru2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2016$IntRelig <- recode(Uru2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2016, mean)
aggregate(TolerHomo ~ AtRelig, Uru2016, mean)
summary(Uru2016$TolerHomo)

Uru2016$Denom <- recode(Uru2016$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

modelo5U <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Uru2016)
summary(modelo5U)

tab_model(modelo5U, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

plot(modelo5U)
save(Uru2016, file = "Uru2016.RData")


Obj2 <- coefplot(modelo5U, title = "2016/2017",
                xlab = "Valor",
                ylab = "Coefficient",
                color = "black",
                outerCI = T,
                innerCI = 5)

Obj2 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo5U)
#    Variables Tolerance      VIF
#1    Denom2 0.8752521 1.142528
#2    Denom3 0.9496094 1.053065
#3    Denom4 0.9783484 1.022131
#4   AtRelig 0.7855116 1.273056
#5  IntRelig 0.8325516 1.201127


save(Uru2016, file = "Uru2016.RData")
write.csv2(Uru2016, file = "Uru2016.csv")
save(Uru2016, file = "Uru2016.RData")

###Uruguai
##2018
Uru2018 <- read_dta("Uru2018.dta")
Uru2018 <- remove_all_labels(Uru2018)
#Tolerância
Uru2018$TolerHomo <- Uru2018$d5
Uru2018$TolerHomo  <- as.factor(Uru2018$TolerHomo)


#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2018$Denom <-  recode(Uru2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,7,11,77), 4 <-c(4))
Uru2018$Denom <- as.factor(Uru2018$Denom)
Uru2018$Denom <- as.numeric(Uru2018$Denom)

Uru2018$Denom <- recode(Uru2018$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Uru2018$Denom2 <-  recode(Uru2018$Denom, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
Uru2018$Denom2 <- as.factor(Uru2018$Denom2)
Uru2018$Denom <- as.numeric(Uru2018$Denom)

table(Uru2018$Denom2)

#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2018$AtRelig <- recode(Uru2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2018$IntRelig <- recode(Uru2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2018, mean)
aggregate(TolerHomo ~ AtRelig, Uru2018, mean)
summary(Uru2018$TolerHomo)

modelo6U <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Uru2018)
summary(modelo6U)

tab_model(modelo6U, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")


modelo6.1U <- lm(TolerHomo ~ Denom2 + AtRelig + IntRelig, data = Uru2018)
summary(modelo6.1U)

tab_model(modelo6.1U, show.ci = F, auto.label = T,
          show.se= T, collapse.se = T, wrap.labels= 60, p.style = "numeric_stars")


plot(modelo6U)
save(Uru2018, file = "Uru2018.RData")

Obj3 <- coefplot(modelo6U, title = "2018",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo6U)

#Variables Tolerance      VIF
#1    Denom2 0.6999874 1.428597
#2    Denom3 0.8432750 1.185853
#3    Denom4 0.8722756 1.146427
#4   AtRelig 0.6361181 1.572035
#5  IntRelig 0.7601519 1.315527



save(Uru2018, file = "Uru2018.RData")

modeloC.U <- lm(TolerHomo ~ Denom2 + AtRelig + IntRelig +
                   Ed_sup + Idade + Sexo + Dem, data = Uru2018)
summary(modeloC.U)

tab_model(modeloC.U, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60,
          p.style = "stars")
library(coefplot)

obj1 <- coefplot(modeloC.U, intercept=F, title = "Coefficient Plot",
         xlab = "Value",
         ylab = "Coefficient",
         color = "black",
         outerCI = T,
         innerCI = 0)

obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) +
 


####Brasil####


###Brasil
##2014
Bra2014 <- read_dta("Bra2014.dta")
Bra2014 <- remove_all_labels(Bra2014)
#Tolerância
Bra2014$TolerHomo <- Bra2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2014$Denom <-  recode(Bra2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2014$Denom <- as.factor(Bra2014$Denom)
Bra2014$Denom <- as.numeric(Bra2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2014$IntRelig <- recode(Bra2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)

Bra2014$Denom <- recode(Bra2014$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2014, mean)
aggregate(TolerHomo ~ AtRelig, Bra2014, mean)
summary(Bra2014$TolerHomo)

modelo5B <- lm(TolerHomo ~ Denom + IntRelig, data = Bra2014)
summary(modelo5B)

tab_model(modelo5B, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")


save(Bra2014, file = "Bra2014.RData")

plot(modelo5B)


Obj1 <- coefplot(modelo5B, title = "2014",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo5B)
#    Variables Tolerance      VIF
#1    Denom2 0.9381114 1.065971
#2    Denom3 0.9827409 1.017562
#3    Denom4 0.7660922 1.305326
#4  IntRelig 0.7781532 1.285094


###Brasil
##2017
Bra2017 <- read_dta("Bra2017.dta")
Bra2017 <- remove_all_labels(Bra2017)
#Tolerância
Bra2017$TolerHomo <- Bra2017$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2017$Denom <-  recode(Bra2017$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2017$Denom <- as.factor(Bra2017$Denom)

Bra2017$Denom <- recode(Bra2017$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)


Bra2017$Denom <- as.numeric(Bra2017$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2017$AtRelig <- recode(Bra2017$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2017$IntRelig <- recode(Bra2017$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2017, mean)
aggregate(TolerHomo ~ AtRelig, Bra2017, mean)
summary(Bra2017$TolerHomo)

modelo6B <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Bra2017)
summary(modelo6B)

tab_model(modelo6B, show.ci = F, auto.label = T,
          show.se= T, collapse.se = T, wrap.labels= 60, p.style = "numeric_stars")

save(Bra2017, file = "Bra2017.RData")
plot(modelo6B)


Obj2 <- coefplot(modelo6B, title = "2016/2017",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj2 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo6B)
#    Variables Tolerance      VIF
#1    Denom2 0.8204226 1.218884
#2    Denom3 0.9716091 1.029220
#3    Denom4 0.7658948 1.305662
#4   AtRelig 0.7322115 1.365726
#5  IntRelig 0.7513698 1.330903


save(Bra2017, file = "Bra2017.RData")
write.csv2(Bra2017, file = "Bra2017.csv")
save(Bra2017, file = "Bra2017.csv")


###Brasil
##2019
Bra2019 <- read_dta("Bra2019.dta")
Bra2019 <- remove_all_labels(Bra2019)
#Tolerância
Bra2019$TolerHomo <- Bra2019$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2019$Denom <-  recode(Bra2019$q3cn, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2019$Denom <- as.factor(Bra2019$Denom)

Bra2019$Denom <- recode(Bra2019$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)


Bra2019$Denom <- as.numeric(Bra2019$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2019$AtRelig <- recode(Bra2019$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2019$IntRelig <- recode(Bra2019$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2019, mean)
aggregate(TolerHomo ~ AtRelig, Bra2019, mean)
summary(Bra2019$TolerHomo)

modelo7B <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Bra2019)
summary(modelo7B)

tab_model(modelo7B, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Bra2019, file = "Bra2019.RData")
plot(modelo7B)

Obj3 <- coefplot(modelo7B, title = "2019",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo7B)

#Variables Tolerance      VIF
#1    Denom2 0.7681741 1.301788
#2    Denom3 0.9828428 1.017457
#3    Denom4 0.7583313 1.318685
#4   AtRelig 0.6656762 1.502232
#5  IntRelig 0.7378571 1.355276

#El Salvador####

###El Salvador 
##2014
Els2014 <- read_dta("Els2014.dta")
Els2014 <- remove_all_labels(Els2014)
#Tolerância
Els2014$TolerHomo <- Els2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2014$Denom <-  recode(Els2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Els2014$Denom <- as.factor(Els2014$Denom)

Els2014$Denom <- recode(Els2014$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)


Els2014$Denom <- as.numeric(Els2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2014$IntRelig <- recode(Els2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2014, mean)
aggregate(TolerHomo ~ AtRelig, Els2014, mean)
summary(Els2014$TolerHomo)

modelo7E <- lm(TolerHomo ~ Denom + IntRelig, data = Els2014)
summary(modelo7E)

tab_model(modelo7E, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Els2014, file = "Els2014.RData")
plot(modelo7E)


Obj1 <- coefplot(modelo7E, title = "2014",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo7E)
#Variables Tolerance      VIF
#1    Denom2 0.8774729 1.139636
#2    Denom3 0.9726349 1.028135
#3    Denom4 0.6712068 1.489854
#4  IntRelig 0.7094091 1.409624


###El Salvador 
##2016
Els2016 <- read_dta("Els2016.dta")
Els2016 <- remove_all_labels(Els2016)
#Tolerância
Els2016$TolerHomo <- Els2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2016$Denom <-  recode(Els2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Els2016$Denom <- as.factor(Els2016$Denom)

Els2016$Denom <- recode(Els2016$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Els2016$Denom <- as.numeric(Els2016$Denom)

#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2016$AtRelig <- recode(Els2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2016$IntRelig <- recode(Els2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2016, mean)
aggregate(TolerHomo ~ AtRelig, Els2016, mean)
summary(Els2016$TolerHomo)

modelo8E <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Els2016)
summary(modelo8E)

tab_model(modelo8E, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")


Obj2 <- coefplot(modelo8E, title = "2016/2017",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj2 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 
save(Els2016, file = "Els2016.RData")
plot(modelo8E)

ols_vif_tol(modelo8E)
#Variables Tolerance      VIF
#1    Denom2 0.8780713 1.138860
#2    Denom3 0.9163724 1.091259
#3    Denom4 0.7512763 1.331068
#4   AtRelig 0.8201267 1.219324
#5  IntRelig 0.7740093 1.291974


save(Els2016, file = "Els2016.RData")
write.csv2(Els2016, file = "Els2016.csv")
save(Els2016, file = "Els2016.RData")

###El Salvador 
##2018
Els2018 <- read_dta("Els2018.dta")
Els2018 <- remove_all_labels(Els2018)
#Tolerância
Els2018$TolerHomo <- Els2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2018$Denom <-  recode(Els2018$q3c, 1 <- 1, 2 <- c( 2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Els2018$Denom <- as.factor(Els2018$Denom)

Els2018$Denom <- recode(Els2018$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Els2018$Denom <- as.numeric(Els2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2018$AtRelig <- recode(Els2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2018$IntRelig <- recode(Els2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2018, mean)
aggregate(TolerHomo ~ AtRelig, Els2018, mean)
summary(Els2018$TolerHomo)

modelo9E <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Els2018)
summary(modelo9E)

tab_model(modelo9E, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Els2018, file = "Els2018.RData")
plot(modelo9E)

Obj3 <- coefplot(modelo5U, title = "2018",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo9E)
#Variables Tolerance      VIF
#1    Denom2 0.8659627 1.154784
#2    Denom3 0.9974606 1.002546
#3    Denom4 0.6291071 1.589554
#4   AtRelig 0.6872393 1.455097
#5  IntRelig 0.7391323 1.352938



#Guatemala####
###Guatemala 
##2014
Gua2014 <- read_dta("Gua2014.dta")
Gua2014 <- remove_all_labels(Gua2014)
#Tolerância
Gua2014$TolerHomo <- Gua2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2014$Denom <-  recode(Gua2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Gua2014$Denom <- as.factor(Gua2014$Denom)

Gua2014$Denom <- recode(Gua2014$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)


Gua2014$Denom <- as.numeric(Gua2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2014$IntRelig <- recode(Gua2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)

Gua2014$IntRelig2 <- as.factor(Gua2014$IntRelig)

#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2014, mean)
aggregate(TolerHomo ~ AtRelig, Gua2014, mean)
summary(Gua2014$TolerHomo)

modelo5G <- lm(TolerHomo ~ Denom + IntRelig, data = Gua2014)
summary(modelo5G)

tab_model(modelo5G, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Gua2014, file = "Gua2014.RData")
plot(modelo5G)

Obj1 <- coefplot(modelo5G, title = "2014",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo5G)
# Tolerance      VIF
#1    Denom2 0.9197477 1.087255
#2    Denom3 0.9736555 1.027057
#3    Denom4 0.8253995 1.211535
#4  IntRelig 0.8501646 1.176243

#Variance Inflation Factors
##2016
Gua2016 <- read_dta("Gua2016.dta")
Gua2016 <- remove_all_labels(Gua2016)
#Tolerância
Gua2016$TolerHomo <- Gua2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2016$Denom <-  recode(Gua2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Gua2016$Denom <- as.factor(Gua2016$Denom)

Gua2016$Denom <- recode(Gua2016$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Gua2016$Denom <- as.numeric(Gua2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2016$AtRelig <- recode(Gua2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2016$IntRelig <- recode(Gua2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2016, mean)
aggregate(TolerHomo ~ AtRelig, Gua2016, mean)
summary(Gua2016$TolerHomo)

modelo6G <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Gua2016)
summary(modelo6G)

tab_model(modelo6G, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Gua2016, file = "Gua2016.RData")
plot(modelo6G)

Obj3 <- coefplot(modelo6G, title = "2016/2017",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo6G)
#Variables Tolerance      VIF
#1    Denom2 0.9372437 1.066958
#2    Denom3 0.9772816 1.023247
#3    Denom4 0.8541497 1.170755
#4   AtRelig 0.8282015 1.207436
#5  IntRelig 0.8399669 1.190523


save(Gua2016, file = "Gua2016.RData")
write.csv2(Gua2016, file = "Gua2016.csv")
save(Gua2016, file = "Gua2016.RData")


###Guatemala 
##2018
Gua2018 <- read_dta("Gua2018.dta")
Gua2018 <- remove_all_labels(Gua2018)
#Tolerância
Gua2018$TolerHomo <- Gua2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2018$Denom <-  recode(Gua2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Gua2018$Denom <- as.factor(Gua2018$Denom)

Gua2018$Denom <- recode(Gua2018$Denom, "Católico" <- 1,
                        "Protestante" <- 2, "Outras" <- 3, 
                        "Ateu/Agnóstico" <-4)

Gua2018$Denom <- as.numeric(Gua2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2018$AtRelig <- recode(Gua2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2018$IntRelig <- recode(Gua2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2018, mean)
aggregate(TolerHomo ~ AtRelig, Gua2018, mean)
summary(Gua2018$TolerHomo)


modelo7G <- lm(TolerHomo ~ Denom + AtRelig + IntRelig, data = Gua2018)
summary(modelo7G)

tab_model(modelo7G, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

save(Gua2018, file = "Gua2018.RData")
plot(modelo7G)

Obj3 <- coefplot(modelo7G, title = "2019",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

Obj3 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 

ols_vif_tol(modelo7G)
#Variables Tolerance      VIF
#1    Denom2 0.9202459 1.086666
#2    Denom3 0.9961014 1.003914
#3    Denom4 0.7999728 1.250043
#4   AtRelig 0.7763166 1.288134
#5  IntRelig 0.7855966 1.272918


save(Gua2018, file = "Gua2018.RData")



