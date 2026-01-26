#-----------------------------------------------------------------------
rm(list = ls()) # Limpa o ambiente de trabalho do R, removendo todos os objetos anteriores.
#-----------------------------------------------------------------------

# --- Seção 1: Configuração Inicial e Carregamento dos Dados ---

# Leitura dos dados
dados <- read.csv("https://raw.githubusercontent.com/lineu96/aed-compolitica/refs/heads/master/encontros/dados%20-%20Respostas%20ao%20formul%C3%A1rio%201.csv") # Carrega o conjunto de dados "milsa.csv" diretamente de uma URL.

# Correção dos nomes das colunas
names(dados) <- c("data-hora", 
                  "idade",
                  "peso", 
                  "altura", 
                  "escolaridade",
                  "aed", 
                  "genero") # Renomeia as colunas para facilitar o entendimento e uso.

# Primeiras linhas
head(dados) # Exibe as primeiras 6 linhas do dataframe para uma inspeção inicial dos dados.

#---
# --- Seção 2: Análise Exploratória de Variáveis Qualitativas ---

dados$genero # Acessa a coluna 'genero' para visualização.

## 2.1. Tabelas de Frequência para Variáveis Qualitativas

### 2.1.1. Frequência Absoluta
table(dados$genero) # Gera uma tabela de frequência absoluta para a variável 'genero'.
tabela_genero_freq_abs <- table(dados$genero) # Armazena a tabela de frequência absoluta em 'tabela_genero_freq_abs'.
tabela_genero_freq_abs # Exibe 'tabela_genero_freq_abs'.
sum(tabela_genero_freq_abs) # Soma as frequências para verificar o total de observações.

### 2.1.2. Frequência Relativa
prop.table(tabela_genero_freq_abs) # Calcula as proporções (frequências relativas) a partir de 'tabela_genero_freq_abs'.
tabela_genero_freq_rel <- prop.table(tabela_genero_freq_abs) # Armazena as frequências relativas em 'tabela_genero_freq_rel'.
tabela_genero_freq_rel # Exibe 'tabela_genero_freq_rel'.
sum(tabela_genero_freq_rel) # Soma as proporções para verificar se o total é 1 (ou próximo de 1).

### 2.1.3. Frequência Percentual
tabela_genero_freq_rel*100 # Converte as proporções de 'tabela_genero_freq_rel' em percentuais.
tabela_genero_freq_perc <- tabela_genero_freq_rel*100 # Armazena os percentuais em 'tabela_genero_freq_perc'.
tabela_genero_freq_perc # Exibe 'tabela_genero_freq_perc'.
sum(tabela_genero_freq_perc) # Soma os percentuais para verificar se o total é 100.

## 2.2. Gráficos para Variáveis Qualitativas

### 2.2.1. Gráficos de Linhas Simples (plot)
plot(tabela_genero_freq_abs) # Gera um gráfico de linhas simples usando a função 'plot' para frequências absolutas.
plot(tabela_genero_freq_rel) # Gera um gráfico de linhas simples para frequências relativas.
plot(tabela_genero_freq_perc) # Gera um gráfico de linhas simples para percentuais.

### 2.2.2. Gráficos de Barras (barplot)
barplot(tabela_genero_freq_abs) # Gera um gráfico de barras usando a função 'barplot' para frequências absolutas.
barplot(tabela_genero_freq_rel) # Gera um gráfico de barras para frequências relativas.
barplot(tabela_genero_freq_perc) # Gera um gráfico de barras para percentuais.

### 2.2.3. Gráficos de Barras Horizontais
barplot(tabela_genero_freq_abs, horiz=T) # Gera um gráfico de barras horizontais para frequências absolutas.
barplot(tabela_genero_freq_rel, horiz=T) # Gera um gráfico de barras horizontais para frequências relativas.
barplot(tabela_genero_freq_perc, horiz=T) # Gera um gráfico de barras horizontais para percentuais.

### 2.2.4. Gráficos de Setores
pie(tabela_genero_freq_abs) # Gera um gráfico de setores para frequências absolutas.
pie(tabela_genero_freq_rel) # Gera um gráfico de setores para frequências relativas.
pie(tabela_genero_freq_perc) # Gera um gráfico de setores para percentuais.

### 2.2.5. Gráficos de Barras Empilhadas (Usando um pequeno "Truque")
# Cria uma tabela de frequência que, ao incluir uma coluna "vazia" (com um valor repetido para todas as observações),
# força a função `barplot()` a criar um gráfico de barras empilhadas para uma única variável qualitativa.
tabela_genero_dummy <- table(dados$genero, rep('',nrow(dados))) # 'rep('',nrow(dados)' atua como uma segunda variável dummy.
barplot(tabela_genero_dummy) # Gera um gráfico de barras empilhadas, onde cada segmento representa uma categoria de estado civil.
barplot(tabela_genero_dummy, horiz = T) # Gera o mesmo gráfico, mas com barras horizontais empilhadas.

tabela_genero_dummy_prop <- prop.table(tabela_genero_dummy) # Calcula as proporções para 'tabela_ec_dummy'.
barplot(tabela_genero_dummy_prop) # Gera gráfico de barras empilhadas com proporções.
barplot(tabela_genero_dummy_prop, horiz = T) # Gera gráfico de barras horizontais empilhadas com proporções.

## 2.3. Organização de Tabelas de Frequência

# Cria um dataframe para organizar as tabelas de frequência de forma mais apresentável.
tabela_genero_final <- data.frame(genero = names(tabela_genero_freq_abs), # Nomes das categorias.
                              freq = as.vector(tabela_genero_freq_abs), # Frequências absolutas.
                              freq_r = as.vector(tabela_genero_freq_rel)) # Frequências relativas.

tabela_genero_final # Exibe o dataframe inicial.
tabela_genero_final[4,1] <- "TOTAL" # Adiciona a linha "TOTAL" na primeira coluna.
tabela_genero_final[4,2] <- sum(tabela_genero_final$freq, na.rm = T) # Calcula e adiciona a soma das frequências absolutas.
tabela_genero_final[4,3] <- sum(tabela_genero_final$freq_r, na.rm = T) # Calcula e adiciona a soma das frequências relativas.
tabela_genero_final # Exibe o dataframe com a linha de total.

names(tabela_genero_final) # Exibe os nomes atuais das colunas.
names(tabela_genero_final) <- c("Gênero",
                            "Freq. absoluta",
                            "Freq. Relativa") # Renomeia as colunas para melhor legibilidade.
tabela_genero_final # Exibe o dataframe final com os novos nomes.

#---
# --- Seção 3: Análise Exploratória de Variáveis Quantitativas ---

dados$idade # Acessa a coluna 'salario' para visualização.

## 3.1. Tabelas de Frequência para Variáveis Quantitativas (com Classes)

breaks <- seq(10,60,10) # Define os pontos de corte (quebras) para as classes de intervalo da idade.

classes <- cut(dados$idade, # Divide a variável 'salario' em intervalos (classes).
               breaks = breaks, # Utiliza os pontos de corte definidos.
               include.lowest = TRUE, # Inclui o valor mais baixo no primeiro intervalo.
               right = FALSE) # Define que os intervalos são fechados à esquerda e abertos à direita.

table(classes) # Gera uma tabela de frequência para as classes de salário.
tabela_idade_classes_abs <- table(classes) # Armazena a tabela de frequência em 'tabela_idade_classes_abs'.
tabela_idade_classes_abs # Exibe 'tabela_sal_classes_abs'.
sum(tabela_idade_classes_abs) # Soma as frequências para verificar o total de observações.

prop.table(tabela_idade_classes_abs) # Calcula as proporções (frequências relativas) a partir de 'tabela_idade_classes_abs'.
tabela_idade_classes_rel <- prop.table(tabela_idade_classes_abs) # Armazena as frequências relativas em 'tabela_idade_classes_rel'.
tabela_idade_classes_rel # Exibe 'tabela_sal_classes_rel'.
sum(tabela_idade_classes_rel) # Soma as proporções para verificar se o total é 1.

tabela_idade_classes_rel*100 # Converte as proporções de 'tabela_sal_classes_rel' em percentuais.
tabela_idade_classes_perc <- tabela_idade_classes_rel*100 # Armazena os percentuais em 'tabela_sal_classes_perc'.
tabela_idade_classes_perc # Exibe 'tabela_sal_classes_perc'.
sum(tabela_idade_classes_perc) # Soma os percentuais para verificar se o total é 100.

## 3.2. Gráficos para Variáveis Quantitativas

### 3.2.1. Histograma e Densidade
tabela_idade_classes_abs # Exibe a tabela de frequência das classes de idade

barplot(tabela_idade_classes_abs, space = 0) # Gera um gráfico de barras sem espaço para as classes.
hist(dados$idade) # Gera um histograma padrão da variável 'salario'.
hist(dados$idade, probability = T) # Gera um histograma de densidade (com a área total igual a 1).

densidade <- density(dados$idade) # Estima a função de densidade de probabilidade para 'salario'.
plot(densidade) # Plota a curva de densidade estimada.

hist(dados$idade, probability = T) # Gera novamente o histograma de densidade.
lines(densidade) # Adiciona a curva de densidade ao histograma existente.

### 3.2.2. Boxplot
boxplot(dados$idade) # Gera um boxplot (diagrama de caixa) para a variável 'salario'.
boxplot(dados$idade, horizontal = T) # Gera um boxplot horizontal para 'salario'.

## 3.3. Organização de Tabelas de Frequência para Variáveis Quantitativas

# Cria um dataframe para organizar as tabelas de frequência para variáveis quantitativas.
tabela_idade_final <- data.frame(faixas= names(tabela_idade_classes_abs), # Nomes das faixas de salário.
                               freq = as.vector(tabela_idade_classes_abs), # Frequências absolutas das faixas.
                               freq_r = as.vector(tabela_idade_classes_rel)) # Frequências relativas das faixas.

tabela_idade_final # Exibe o dataframe inicial.
tabela_idade_final[nrow(tabela_idade_final)+1,1] <- "TOTAL" # Adiciona a linha "TOTAL" na primeira coluna (considerando 10 faixas + 1 para o total).
tabela_idade_final[nrow(tabela_idade_final),2] <- sum(tabela_idade_final$freq, na.rm = T) # Calcula e adiciona a soma das frequências absolutas.
tabela_idade_final[nrow(tabela_idade_final),3] <- sum(tabela_idade_final$freq_r, na.rm = T) # Calcula e adiciona a soma das frequências relativas.
tabela_idade_final # Exibe o dataframe com a linha de total.

names(tabela_idade_final) # Exibe os nomes atuais das colunas.
names(tabela_idade_final) <- c("Faixas",
                             "Freq. absoluta",
                             "Freq. Relativa") # Renomeia as colunas para melhor legibilidade.
tabela_idade_final # Exibe o dataframe final com os novos nomes.

#---
# --- Seção 4: Medidas Resumo (Posição e Dispersão) ---

dados$idade # Acessa a coluna 'salario' para visualização.

## 4.1. Medidas de Posição (Tendência Central e Separatrizes)

### 4.1.1. Média Aritmética
sum(dados$idade)/length(dados$idade) # Calcula a média do salário manualmente.
mean(dados$idade) # Calcula a média do salário usando a função 'mean()'.

### 4.1.2. Mediana
median(dados$idade) # Calcula a mediana do salário.

### 4.1.3. Quartis e Decis
quantile(dados$idade) # Calcula os quartis (0%, 25%, 50%, 75%, 100%) do salário.
quantile(dados$idade, seq(0,1,0.1)) # Calcula os decis de 0% a 100% (a cada 10%).
quantile(dados$idade)[4] - quantile(dados$idade)[2] # Calcula a Amplitude Interquartil (Q3 - Q1).

## 4.2. Medidas de Dispersão (Variabilidade)

### 4.2.1. Amplitude Total
min(dados$idade) # Encontra o valor mínimo do salário.
max(dados$idade) # Encontra o valor máximo do salário.
range(dados$idade) # Retorna o mínimo e o máximo do salário.
max(dados$idade) - min(dados$idade) # Calcula a amplitude (máximo - mínimo).
diff(range(dados$idade)) # Outra forma de calcular a amplitude total.

### 4.2.2. Desvios Absolutos
dados$idade - mean(dados$idade) # Calcula os desvios de cada salário em relação à média.
dados$idade - median(dados$idade) # Calcula os desvios de cada salário em relação à mediana.

da_media <- abs(dados$idade - mean(dados$idade)) # Calcula o desvio absoluto em relação à média.
da_mediana <- abs(dados$idade - median(dados$idade)) # Calcula o desvio absoluto em relação à mediana.

### 4.2.3. Desvio Médio Absoluto (DMA)
mean(da_media) # Calcula o Desvio Médio Absoluto (DMA) em relação à média.
mean(da_mediana) # Calcula o Desvio Médio Absoluto (DMA) em relação à mediana.

### 4.2.4. Variância e Desvio Padrão
var(dados$idade) # Calcula a variância do salário.
sqrt(var(dados$idade)) # Calcula o desvio padrão a partir da raiz quadrada da variância.
sd(dados$idade) # Calcula o desvio padrão do salário usando a função 'sd()'.

### 4.2.5. Coeficiente de Variação (CV)
sd(dados$idade)/mean(dados$idade) # Calcula o coeficiente de variação.

### 4.2.6. Escores Z
(dados$idade - mean(dados$idade))/sd(dados$idade) # Calcula os escores Z.
scale(dados$idade) # Função mais eficiente para calcular os escores Z.
escore <- scale(dados$idade) # Armazena os escores Z em 'escore'.
mean(escore) # Calcula a média dos escores Z (deve ser aproximadamente 0).
sd(escore) # Calcula o desvio padrão dos escores Z (deve ser aproximadamente 1).

### 4.2.7. Entropia de Shannon (Variabilidade para Qualitativas)
dados$genero # Acessa a coluna 'instrucao'.
tabela_genero_freq_abs <- table(dados$genero) # Tabela de frequência para 'instrucao'.
pi <- prop.table(tabela_genero_freq_abs) # Proporções (frequências relativas).
log_pi <- log(pi) # Logaritmo natural das proporções.
h <- -(sum(pi*log_pi)) # Calcula a entropia de Shannon, uma medida de variabilidade para variáveis qualitativas.
h # Exibe o valor da entropia.

#---
# --- Seção 5: Análises Bivariadas (Explorando Relações entre Variáveis) ---

dados$aed # Acessa a coluna 'estado_civil'.
dados$escolaridade # Acessa a coluna 'instrucao'.

## 5.1. Tabelas de Contingência (Qualitativa vs. Qualitativa)

table(dados$aed, dados$escolaridade) # Tabela de contingência entre 'estado_civil' e 'instrucao'.
table(dados$escolaridade, dados$aed) # Tabela de contingência com as variáveis invertidas.

### 5.1.1. Tabela de Frequência Absoluta
tabela_contingencia_abs <- table(dados$aed,
                                 dados$escolaridade) # Armazena a tabela de contingência em 'tabela_contingencia_abs'.
tabela_contingencia_abs # Exibe 'tabela_contingencia_abs'.
sum(tabela_contingencia_abs) # Soma total das observações na tabela de contingência.

addmargins(tabela_contingencia_abs) # Adiciona somas de linha e coluna (margens) à 'tabela_contingencia_abs'.
t(tabela_contingencia_abs) # Transpõe a tabela (inverte linhas e colunas).

### 5.1.2. Tabela de Proporção Geral
prop.table(tabela_contingencia_abs) # Calcula as proporções em relação ao total geral da tabela.
tabela_contingencia_rel <- prop.table(tabela_contingencia_abs) # Armazena as proporções gerais em 'tabela_contingencia_rel'.
tabela_contingencia_rel # Exibe 'tabela_contingencia_rel'.
sum(tabela_contingencia_rel) # Soma as proporções para verificar o total (deve ser 1).
addmargins(tabela_contingencia_rel) # Adiciona somas de linha e coluna às proporções gerais.
t(tabela_contingencia_rel) # Transpõe a tabela de proporções gerais.

### 5.1.3. Tabela de Proporção por Linha
prop.table(tabela_contingencia_abs, margin = 1) # Calcula as proporções por linha (soma de cada linha = 1).
tabela_contingencia_linha <- prop.table(tabela_contingencia_abs, margin = 1) # Armazena as proporções por linha em 'tabela_contingencia_linha'.
tabela_contingencia_linha # Exibe 'tabela_contingencia_linha'.
addmargins(tabela_contingencia_linha) # Adiciona somas de linha e coluna (as somas das linhas serão 1).
t(tabela_contingencia_linha) # Transpõe a tabela de proporções por linha.

### 5.1.4. Tabela de Proporção por Coluna
prop.table(tabela_contingencia_abs, margin = 2) # Calcula as proporções por coluna (soma de cada coluna = 1).
tabela_contingencia_coluna <- prop.table(tabela_contingencia_abs, margin = 2) # Armazena as proporções por coluna em 'tabela_contingencia_coluna'.
tabela_contingencia_coluna # Exibe 'tabela_contingencia_coluna'.
addmargins(tabela_contingencia_coluna) # Adiciona somas de linha e coluna (as somas das colunas serão 1).

## 5.2. Gráficos para Análises Bivariadas (Qualitativa vs. Qualitativa)

tabela_contingencia_abs # Exibe a tabela de contingência original.

barplot(tabela_contingencia_abs, beside = F, legend.text = T) # Gráfico de barras empilhadas para 'tabela_contingencia_abs' com legenda.
barplot(t(tabela_contingencia_abs), beside = F, legend.text = T) # Gráfico de barras empilhadas com a tabela transposta.

barplot(tabela_contingencia_abs, beside = T, legend.text = T) # Gráfico de barras agrupadas para 'tabela_contingencia_abs' com legenda.
barplot(t(tabela_contingencia_abs), beside = T, legend.text = T) # Gráfico de barras agrupadas com a tabela transposta.

barplot(t(tabela_contingencia_linha), legend.text = T) # Gráfico de barras para proporções por linha (transposto para melhor visualização).
barplot(tabela_contingencia_coluna, legend.text = T) # Gráfico de barras para proporções por coluna.

## 5.3. Qui-Quadrado (associação entre qualitativas)

tabela_contingencia_abs # Exibe a tabela de contingência original.
tabela_contingencia_margens <- addmargins(tabela_contingencia_abs) # Tabela de contingência com as somas marginais.
tabela_contingencia_margens # Exibe a tabela com as margens.

# Calcula as frequências esperadas para o teste Qui-quadrado de independência.
# (linha total * coluna total) / total geral
esperados <- (tabela_contingencia_margens[1:2,4] %*% t(tabela_contingencia_margens[3,1:3]))/sum(tabela_contingencia_abs)
# Calcula a estatística Qui-quadrado (soma do quadrado da diferença entre observado e esperado, dividido pelo esperado).
sum(((tabela_contingencia_abs - esperados)^2)/(esperados)) # Não é o teste qui-quadrado completo, mas o cálculo da estatística.

## 5.4. Correlação e Regressão Linear Simples (Quantitativa vs. Quantitativa)

cor(dados$peso, dados$altura) # Calcula a correlação de Pearson entre 'salario' e 'anos' (padrão).
cor(dados$peso, dados$altura, method = "pearson") # Especifica o método de Pearson para correlação linear.
cor(dados$peso, dados$altura, method = "spearman") # Calcula a correlação de Spearman (não-paramétrica, baseada em ranks).
cor(dados$peso, dados$altura, method = "kendall") # Calcula a correlação de Kendall (não-paramétrica, baseada em concordância/discordância).

plot(peso ~ altura, data = dados) # Cria um gráfico de dispersão de 'salario' em função de 'anos'.
lm(peso ~ altura, data = dados) # Ajusta um modelo de regressão linear simples.
coeficientes <- lm(peso ~ altura, data = dados) # Armazena os resultados do modelo de regressão.
abline(coeficientes, col = 2) # Adiciona a linha de regressão ao gráfico de dispersão (em vermelho).

## 5.5. Análise Comparativa de Variáveis Quantitativas por Grupo (Quantitativa vs. Qualitativa)

### 5.5.1. Medidas Resumo por Grupo (tapply)
# Aplica uma função (FUN) a subconjuntos de um vetor (X), definidos por um fator (INDEX).
tapply(X = dados$altura, # Variável quantitativa de interesse.
       INDEX = dados$genero, # Variável categórica para agrupar os dados.
       FUN = mean) # Calcula a média do salário para cada nível de 'instrucao'.

tapply(X = dados$altura,
       INDEX = dados$genero,
       FUN = sd) # Calcula o desvio padrão do salário para cada nível de 'instrucao'.

tapply(X = dados$altura,
       INDEX = dados$genero,
       FUN = summary) # Gera um sumário estatístico completo do salário para cada nível de 'instrucao'.

### 5.5.2. Boxplots Comparativos
boxplot(altura~genero, data = dados) # Gera boxplots comparativos do salário por nível de instrução.

### 5.5.3. Histograma e Densidade por Grupo
feminino <- subset(dados, genero == "Feminino") # Cria um subconjunto de dados para '1o Grau'.
masculino <- subset(dados, genero == "Masculino") # Cria um subconjunto de dados para '2o Grau'.
prefiro_nao_responder <- subset(dados, genero == "Prefiro não responder") # Cria um subconjunto de dados para 'Superior'.

par(mfrow = c(1,3)) # Define o layout da área de plotagem para 1 linha e 3 colunas.
hist(feminino$altura) # Histograma do salário para o grupo "1o Grau".
hist(masculino$altura) # Histograma do salário para o grupo "2o Grau".
hist(prefiro_nao_responder$altura) # Histograma do salário para o grupo "Superior".

plot(density(feminino$altura)) # Plota a curva de densidade do salário para "1o Grau".
plot(density(masculino$altura)) # Plota a curva de densidade do salário para "2o Grau".
plot(density(prefiro_nao_responder$altura)) # Plota a curva de densidade do salário para "Superior".

par(mfrow = c(1,1)) # Reseta o layout da área de plotagem para uma única figura.
plot(density(feminino$altura), xlim = c(100,250)) # Plota a densidade do "1o Grau" com limites x definidos.
lines(density(masculino$altura), col= 2) # Adiciona a densidade do "2o Grau" (em vermelho) ao gráfico existente.
lines(density(prefiro_nao_responder$altura), col = 4) # Adiciona a densidade do "Superior" (em azul) ao gráfico existente.
