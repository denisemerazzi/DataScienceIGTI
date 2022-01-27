#Exercício 1 de estatística computacional com R - IGTI
#
#Forma o conjunto de dados historico contendo vinte locacoes sorteadas aleatoriamente do banco de dados e a armazena
#em um data frame chamado dados

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


#Variável resposta em função do preço: Histograma do preço:
#Observa-se aqui que o gráfico apresenta uma distribuição normal
#Os dados são distribuídos em torno do eixo central
hist(dados$Preco)

#Boxplot do preco: é possível observar um outlier superior e um inferior; 
#Existe uma locação que o preço foi muito acima do esperado e uma locação cujo preço foi muito abaixo do esperado 
boxplot(dados$Preco)

#Estatisticas descritivas do preço:
# A mediana indica que o preço varia 50% abaixo e 50% acima de 447,8
summary(dados$Preco)


#Boxplot entre o Preco e Quadrimestre
#O preço do primeiro quadrimestre apresenta a maior mediana
boxplot(dados$Preco~ dados$Quadrimestre)

#Análise de variancia
# Rejeita-se a hipótese nula, quando pvalue menor que alfa
# A hipótese nula nos diz que não existe relação entre as variávies
# Condiderando-se um alfa de 95% de confiança (alfa=0,05):
# Observa-se pvalue=0,000126, ou seja, menor que alfa:
# Há evidências para rejeitar a hipótese nula das médias, ou seja:
# Pelo menos um dos quadrimestres possui o preço médio diferente dos demais. 
anova <- aov(Preco ~ Quadrimestre, data = dados)
summary(anova)

#Explore a relação entre as variáveis Preço e Portas, responda:

boxplot(dados$Preco ~ dados$Portas)

#Test t de Student: Preços e portas:
#Considerando 95% de confiança o valor do pvalue=0,88884 (88,8%) é maior que alfa
#Não há evidências para rejeitar a hipótese nula de igualdade de médias
#Não existe diferença significativa entre o preço médio do aluguel do veículo com duas portas 
#Em relação (comparação) com o preço médio do veículo de quatro portas
t.test(dados$Preco ~ dados$Portas , 
       paired = FALSE, #amostras nao pareadas
       alternative = 'two.sided', #bilateral
       conf.level = 0.95 #95% de confianca
)

#Relação entre as variáveis Preço e Quilometragem:
#Pelo gráfico de dispersão, identifica-se que existe a relação linear entre o Preço e a Quilometragem
#Essa relação é linear negativa, pois à medida que a quilometragem aumenta o preço diminui. 

plot(y = dados$Preco ,
     x = dados$Quilometragem,
     pch = 16)

#Coeficiente de correlacao
#o valor do coeficiente de correlação linear de Pearson entre o Preço e a Quilometragem
#é de -0,82,isso nos informa que é uma correlação negativa alta.
cor(dados$Preco, dados$Quilometragem)

#Regressao linear do Preco em funcao da Quilometragem
# O R2 é de 67,76%, ou seja, a variável Quilometragem consegue explicar 67,76% da variação do Preço.
regressao_linear <- lm(Preco ~ Quilometragem, data = dados)
summary(regressao_linear)

#Analise descritiva da variavel quilometragem
# O primeiro quartil é 554,7, isso nos diz que até 25% dos veículos alugados possuem quilometragem até 554,7.
# Oterceiro quartil é 925,2, isso nos diz que até 75% dos veículos alugados possuem quilometragem até 925,2.

summary(dados$Quilometragem)

#Coeficiente de variacao
#O coeficiente de variação é 27,74%, ou seja, 
#os valores da variável quilometragem variam em média 27,74% em torno de sua média.
sd(dados$Quilometragem) / mean(dados$Quilometragem)

#Explore a correlacao entre o Dolar e o Preco
#O gráfico de dispersão não apresenta nenhum padrão entre as duas variáveis, ou seja, na medida que o Dólar aumenta,
#o Preço não cresce nem decresce, ou seja, não há correlação entre as duas variáveis.
#O valor do coeficiente de correlação linear de Pearson é -0,06 (próximo do zero), que indica ausência de correlação linear.
#Não é possível prever o preço com base no dólar:
#Não seria possível, pois o p valor do coeficiente beta do Dolar é de 0,77 (77%), ou seja, 
#independente do nível de significância adotado, 
#a variável Dolar não exerce influência significativa na variável Preco,
#portanto, não é possível prever o Preço baseado no Dólar.
plot(y = dados$Preco,
     x = dados$Dolar,
     pch = 16)

cor(dados$Preco, dados$Dolar)

regressao_linear <- lm(Preco ~ Dolar, data = dados)
summary(regressao_linear)
