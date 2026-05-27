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

data<- read.xlsx("Lixiviação.xlsx", sheet="Analysis")
head(data)
data$TRT<-as.factor(data$TRT)
data$Block<-as.factor(data$Block)
data$Leaching <- as.factor (data$Leaching)

print(data)
#=====data#============================= Cumulative =========================='
cumulative<- data%>%
  group_by(Block, TRT)%>%
  summarise(Ktot=sum(Kleach),.groups = "drop")


ktot<- lm(Ktot~ Block + TRT,
          data=cumulative, na.action = na.omit)

augment(ktot) #package broom
diagnosticos <- augment(ktot)

ggplot(diagnosticos) +
  aes(x = TRT, 
      y = .std.resid) +
  geom_jitter()

qqPlot(residuals(ktot))
plot(ktot)
anova(ktot)

cld(emmeans(ktot,~ TRT), Letters=letters,adjust="tukey", reverse=T)

#===================== Plot cumulative =========================;

plot1<-cld(emmeans(ktot,~ TRT), Letters=letters,adjust="Bonf", reverse=T)

str(plot1)

ggplot(plot1, aes(x=TRT, y = emmean, fill=TRT))+
  theme_bw()+
  geom_bar(stat = 'identity', color = 'black')+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.3)+
  labs(y = expression(paste("Cum. K leaching (mg ", L^{-1},")")),
       x="", fill="Fertilizante",
       title = "") +
  scale_fill_viridis(discrete = TRUE) 
  

#============================ ANOVA RESIDUAL MODELING ======================================'

ggplot(data) +
  aes(y = Kleach,
      x = TRT,
      color=TRT) +
#  geom_smooth(method="lm") +
  geom_jitter(width = .3)+
facet_grid(.~Leaching)

m1<-gls(Kleach~TRT*Leaching, data=data)#Modelo errado
anova(m1)

# ----------- Modelagem adequada das unidades experimentais (MR) no modelo:
#======= Identity (default):
m1.1<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT, #PlotID
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
anova(m1.1)
anova(m1, m1.1)

#======= Diagonal:
m2<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        weights = varIdent(form = ~ 1 | Leaching),
        #weights = varIdent(form = ~ 1 | TRT),
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
# varIdent(~1|Leaching) falhou (singularidade),
# possivelmente por poucos dados ou confusão com TRT.
# Usando varIdent(~1|TRT), que ajusta bem
# e permite variância residual diferente por tratamento.

#======= Autocorrelation 1st order:
m3<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
          correlation = corAR1(form=~1|Block/TRT),
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)

# ======= Compound Symmetry:
m4<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        correlation = corCompSymm(form=~Leaching|Block/TRT),
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)

# ======= Unstructured: 
m5<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        correlation = corCompSymm(form=~1|Block/TRT),
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)

anova(m1.1,m2,m3,m4,m5)

data$Residuals1<- residuals(m1.1, type="normalized")
data$Residuals2<- residuals(m3, type="normalized")

ggplot(data) +
  aes(y = Residuals1,
      x = Leaching,
      color=TRT) +
  geom_jitter(width = .3)

ggplot(data) +
  aes(y = Residuals2,
      x = Leaching,
      color=TRT) +
  geom_jitter(width = .3)

qqPlot(residuals(m1))
qqPlot(residuals(m3))

anova(m1.1)
anova(m3)#a interação é menos forte, já que a correlação entre as obs. é modelada

cld(emmeans(m1.1,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)
cld(emmeans(m3,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)

plot2<-cld(emmeans(m3,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)# is better with the RM model

ggplot(plot2, aes(x=Leaching, y=emmean, 
                        group=TRT, color=TRT)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Potassium Leaching") +
  theme_bw() +
  labs(y = expression(paste(" Leaching (mg ", L^{-1}, ")")), 
       x = 'Leaching event',
       color = "Fertilizer")

