library(openxlsx)
library(tidyverse)
library (car)
library(broom)
library(lme4)
library(nlme)
library(lmerTest)
library(emmeans)
library(multcompView)
library(multcomp)
library(viridis)#palette

data<- read.xlsx("Altura.xlsx")
head(data)
data$Tratamento<-as.factor(data$Tratamento)
data$Bloco<-as.factor(data$Bloco)
data$DAS <- as.factor (data$DAS)



ggplot(data) +
  aes(y = Altura,
      x = Tratamento,
      color=DAS) +
  geom_jitter(width = .3)#+
facet_grid(.~DAS)

m1<-lme(Altura~ Tratamento*DAS, random= ~1|Bloco/Tratamento,
        data=data, na.action = na.omit)
m2<-lme(Altura~Tratamento*DAS, random= ~1|Bloco/Tratamento,
          correlation = corAR1(form=~1|Bloco/Tratamento),
        data=data)
anova(m1,m2)

data$Residuals1<- residuals(m1, type="normalized")
data$Residuals2<- residuals(m2, type="normalized")

ggplot(data) +
  aes(y = Residuals1,
      x = Tratamento,
      color=DAS) +
  geom_jitter(width = .3)

ggplot(data) +
  aes(y = Residuals2,
      x = Tratamento,
      color=DAS) +
  geom_jitter(width = .3)

qqPlot(residuals(m1))
qqPlot(residuals(m2))

anova(m1)
anova(m2)#a interacao é menos forte, ja que o efeito do tempo é considerado


cld(emmeans(m2,~ Tratamento|DAS), Letters=letters,adjust="Tukey", reverse=T)# is better with the RM model

plot2<-cld(emmeans(m2,~ Tratamento|DAS), Letters=letters,adjust="bonf", reverse=T)# is better with the RM model

ggplot(plot2, aes(x=DAS, y=emmean, 
                        group=Tratamento, color=Tratamento)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Altura de planta") +
  theme_bw() +
  labs(y = expression(paste(" Altura (cm)")), 
       x = 'DAS',
       color = "Fertilizer")

