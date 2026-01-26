
#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análise exploratória univariadas de variáveis qualitativas e 
# quantitativas por meio de tabelas e gráficos.
#
# Execute os comandos, discuta o que eles fazem, comente o código e 
# busque maneiras de customizar os gráficos e tabelas
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", 
                  "instrucao", "filhos", "salario", 
                  "anos", "meses", "regiao")

head(dados)
names(dados)

#-----------------------------------------------------------------------

dados$estado_civil

table(dados$estado_civil)
tabela1 <- table(dados$estado_civil)
tabela1
sum(tabela1)

#-----------------------------------------------------------------------

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)

#-----------------------------------------------------------------------

tabela2*100
tabela3 <- tabela2*100
tabela3
sum(tabela3)

#-----------------------------------------------------------------------

plot(tabela1)
plot(tabela2)
plot(tabela3)

#-----------------------------------------------------------------------

barplot(tabela1)
barplot(tabela2)
barplot(tabela3)

#-----------------------------------------------------------------------

barplot(tabela1, horiz=T)
barplot(tabela2, horiz=T)
barplot(tabela3, horiz=T)

#-----------------------------------------------------------------------

pie(tabela1)
pie(tabela2)
pie(tabela3)

#-----------------------------------------------------------------------

tabela4 <- table(dados$estado_civil, rep(1,36))
barplot(tabela4)
barplot(tabela4, horiz = T)

#-----------------------------------------------------------------------

tabela5 <- prop.table(tabela4)
barplot(tabela4)
barplot(tabela4, horiz = T)

#-----------------------------------------------------------------------

tabela6 = data.frame(estado_civil = names(tabela1),
                     freq = as.vector(tabela1),
                     freq_r = as.vector(tabela2))

tabela6
tabela6[3,1] <- "TOTAL"
tabela6[3,2] <- sum(tabela6$freq, na.rm = T)
tabela6[3,3] <- sum(tabela6$freq_r, na.rm = T)
tabela6

names(tabela6)
names(tabela6) <- c("Estado civil",
                    "Freq. absoluta",
                    "Freq. Relativa")
tabela6

#-----------------------------------------------------------------------

dados$salario

breaks <- seq(4,24,2)

classes <- cut(dados$salario, 
               breaks = breaks, 
               include.lowest = TRUE, 
               right = FALSE)

table(classes)
tabela7 <- table(classes)
tabela7
sum(tabela7)

#-----------------------------------------------------------------------

prop.table(tabela7)
tabela8 <- prop.table(tabela7)
tabela8
sum(tabela8)

#-----------------------------------------------------------------------

tabela8*100
tabela9 <- tabela8*100
tabela9
sum(tabela9)

#-----------------------------------------------------------------------

tabela7

barplot(tabela7, space = 0)
hist(dados$salario)
hist(dados$salario, probability = T)

densidade <- density(dados$salario)
plot(densidade)

hist(dados$salario, probability = T)
lines(densidade)

boxplot(dados$salario)
boxplot(dados$salario, horizontal = T)

#-----------------------------------------------------------------------

tabela10 = data.frame(faixas= names(tabela7),
                     freq = as.vector(tabela7),
                     freq_r = as.vector(tabela8))

tabela10
tabela10[11,1] <- "TOTAL"
tabela10[11,2] <- sum(tabela10$freq, na.rm = T)
tabela10[11,3] <- sum(tabela10$freq_r, na.rm = T)
tabela10

names(tabela10)
names(tabela10) <- c("Faixas",
                    "Freq. absoluta",
                    "Freq. Relativa")
tabela10

#-----------------------------------------------------------------------

dados$salario

sum(dados$salario)/length(dados$salario)
mean(dados$salario)

median(dados$salario)

quantile(dados$salario)
quantile(dados$salario, seq(0,1,0.1))
quantile(dados$salario)[4] - quantile(dados$salario)[1]

min(dados$salario)
max(dados$salario)
range(dados$salario)
max(dados$salario) - min(dados$salario)
diff(range(dados$salario))

dados$salario - mean(dados$salario)
dados$salario - median(dados$salario)

da_media <- abs(dados$salario - mean(dados$salario))
da_mediana <- abs(dados$salario - median(dados$salario))

mean(da_media)
mean(da_mediana)

var(dados$salario)
sqrt(var(dados$salario))
sd(dados$salario)

sd(dados$salario)/var(dados$salario)

(dados$salario - mean(dados$sal))/sd(dados$salario)
scale(dados$salario)
escore <- scale(dados$salario)
mean(escore)
sd(escore)

dados$instrucao
tabela <- table(dados$instrucao)
pi <- prop.table(tabela)
log_pi <- log(pi)
h1 <- -(sum(pi*log_pi))
h1

#-----------------------------------------------------------------------

dados$estado_civil
dados$instrucao

#-----------------------------------------------------------------------

table(dados$estado_civil, dados$instrucao)
table(dados$instrucao, dados$estado_civil)

#-----------------------------------------------------------------------

tabela1 <- table(dados$estado_civil, 
                 dados$instrucao)
tabela1
sum(tabela1)

addmargins(tabela1)

t(tabela1)

#-----------------------------------------------------------------------

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)
addmargins(tabela2)
t(tabela2)

#-----------------------------------------------------------------------

prop.table(tabela1, margin = 1)
tabela3 <- prop.table(tabela1, margin = 1)
tabela3
addmargins(tabela3)
t(tabela3)

#-----------------------------------------------------------------------

prop.table(tabela1, margin = 2)
tabela4 <- prop.table(tabela1, margin = 2)
tabela4
addmargins(tabela4)

#-----------------------------------------------------------------------

tabela1

barplot(tabela1, beside = F, legend.text = T)
barplot(t(tabela1), beside = F, legend.text = T)

barplot(tabela1, beside = T, legend.text = T)
barplot(t(tabela1), beside = T, legend.text = T)

barplot(t(tabela3), legend.text = T)
barplot(tabela4, legend.text = T)

#-----------------------------------------------------------------------

tabela1
tabela1_margens <- addmargins(tabela1)
tabela1_margens

esperados <- (tabela1_margens[1:2,4] %*% t(tabela1_margens[3,1:3]))/sum(tabela1)
sum(((tabela1 - esperados)^2)/(esperados))

#-----------------------------------------------------------------------

cor(dados$salario, dados$anos)
cor(dados$salario, dados$anos, method = "pearson")
cor(dados$salario, dados$anos, method = "spearman")
cor(dados$salario, dados$anos, method = "kendall")

plot(salario ~ anos, data = dados)
lm(salario ~ anos, data = dados)
coeficientes <- lm(salario ~ anos, data = dados)
abline(coeficientes, col = 2)

#-----------------------------------------------------------------------

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = mean)

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = sd)

tapply(X = dados$salario,
       INDEX = dados$instrucao,
       FUN = summary)

boxplot(salario~instrucao, data = dados)

grau1 <- subset(dados, instrucao == "1o Grau")
grau2 <- subset(dados, instrucao == "2o Grau")
grau3 <- subset(dados, instrucao == "Superior")

par(mfrow = c(1,3))
hist(grau1$salario)
hist(grau2$salario)
hist(grau3$salario)

plot(density(grau1$salario))
plot(density(grau2$salario))
plot(density(grau3$salario))

par(mfrow = c(1,1))
plot(density(grau1$salario), xlim = c(0,30))
lines(density(grau2$salario), col= 2)
lines(density(grau3$salario), col = 4)

#----------------------------------------------------------------------
