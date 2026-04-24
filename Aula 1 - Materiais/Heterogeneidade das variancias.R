#install.packages("tidyverse") #Usar isso ao inves de library pra baixar pacotes
library(tidyverse) #manipulação de dados (aka 'data carpentry')
#library(gvlma) #teste pra pressupostos do modelo
library(broom) #summarizar resultados de modelos complexos
library(nlme) #modelagem das variancias
library(car) #analise e visualizacao de modelos lineares
library(emmeans) #teste de médias
library(openxlsx) #abrir arquivos de excel
library(multcomp)#teste de medias
library(ggplot2)

getwd()
setwd("C:/Users/julia.barra/OneDrive - University of Maine System/UMaine/Teaching/Analise de dados - PPGA-CS/Aula 1 - Materiais")


##############################
#####     Regressao      #####
##############################

View(iris)
str(iris)

ggplot(iris) +
  aes(x = Petal.Width,
      y = Petal.Length) +
  geom_point(
   # aes(color=Species)
    ) 

mreg <- lm(Petal.Length ~ Petal.Width, data = iris)
plot(mreg)

summary(mreg)


##############################
#####        DIC         #####
##############################


ggplot(iris) +
  aes(x = Species,
      y = Petal.Length) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = .3)

m1 <- lm(Petal.Length ~ Species, data = iris)
plot(m1)

bartlett.test(Petal.Length ~ Species, data = iris) #heterogeneity
#H0: (hipótese nula): variâncias são iguais (homogêneas)
#H1: variâncias são diferentes

shapiro.test(residuals(m1)) #normality
#H0: (hipótese nula): normalidade Ok
#H1: nao normal
# bem sensivel ao tamanho da amostra (n)

augment(m1) #package broom
# estabelece os diagnosticos ponto a ponto
diagnosticos <- augment(m1)

ggplot(diagnosticos) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()
#geom_point()

###############################
####  Log transformation  ####
##############################

m2 <- lm(sqrt(Petal.Length) ~ Species, data = iris) #transformacao raiz quadrada
plot(m2)
m2 <- lm(log(Petal.Length) ~ Species, data = iris)
qqPlot(residuals(m2)) #car
plot(m2)

diagnosticos2 <- augment(m2)

ggplot(diagnosticos2) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()
#geom_point()

##############################
#####        DBC         #####
##############################

data<-read.xlsx("K_acumulo.xlsx")
View(data)
data<-data%>%
  filter(Solucao!="sem") # >, <, ==

str(data)
data$Solucao<-as.factor(data$Solucao)
#data$Dose<-as.factor(data$Dose)
data$Bloco<-as.factor(data$Bloco)
str(data)

ggplot(data) +
  aes(x = Dose,
      y = K,
      color=Solucao) +
 # geom_smooth(method="lm") +
  geom_jitter(width = .3)+
facet_grid(Solucao~.)

m.1<- lm(K~Bloco + Dose * Solucao, data=data)

plot(m.1)

diagnosticos3 <- augment(m.1)

ggplot(diagnosticos3) +
  aes(x = Dose, 
      y = .std.resid,
      color=Dose) +
  geom_jitter()


m.3<- lm(log(K)~Bloco + Dose * Solucao, data=data)
diagnosticos4 <- augment(m.3)

ggplot(diagnosticos4) +
  aes(x = Dose, 
      y = .std.resid) +
  geom_jitter()

################################################
#########.  Uso de estruturas de variancia #####
################################################
m.4<- gls(K~Bloco + Dose * Solucao, data=data,
          weights = varIdent(form = ~1|Dose))#modelo mais complexo

data$Residuos<- residuals(m.4, type="normalized")

ggplot(data) +
  aes(x = Dose, 
      y = Residuos) +
  geom_jitter()

plot(m.4)
qqPlot(residuals(m.4))

m0<- gls(K~Bloco + Dose * Solucao, data=data) #modelo simples
anova(m0,m.4)

anova(m.4)
summary(m.4)#explore mainly the variance function
plot(emmeans(m.4,~Dose|Solucao,  mode = "df.error"))

ggplot(data) +
  aes(x = Dose,
      y = K,
      #color=Solucao
      ) +
  geom_smooth(method="lm") +
  geom_jitter(width = .3)#+
  #facet_grid(Solucao~.)

###############################################
########.  Considerando dose como fator.  #####http://127.0.0.1:27482/graphics/plot_zoom_png?width=757&height=338
########         & teste de media         ##### 
###############################################

m.4<- gls(K~Bloco + factor(Dose) * Solucao, data=data,
          weights = varIdent(form = ~1|Dose))#
anova(m.4)
summary(m.4)
teste<-emmeans(m.4,~Dose, mode = "df.error") 
teste<-cld(teste, Letters=letters)

cld(emmeans(m.4,~Dose, mode = "df.error"), Letters=letters) 


str(teste)
write.xlsx(teste, "Teste de medias.xlsx")

