#Exerc�cio 1 de estat�stica computacional com R - IGTI

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


#Vari�vel resposta em fun��o do pre�o: Histograma do pre�o:
#Observa-se aqui que o gr�fico apresenta uma distribui��o normal
#Os dados s�o distribu�dos em torno do eixo central
hist(dados$Preco)

#Boxplot do preco: � poss�vel observar um outlier superior e um inferior; 
#Existe uma loca��o que o pre�o foi muito acima do esperado e uma loca��o cujo pre�o foi muito abaixo do esperado 
boxplot(dados$Preco)

#Estatisticas descritivas do pre�o:
# A mediana indica que o pre�o varia 50% abaixo e 50% acima de 447,8
summary(dados$Preco)


#Boxplot entre o Preco e Quadrimestre
#O pre�o do primeiro quadrimestre apresenta a maior mediana
boxplot(dados$Preco~ dados$Quadrimestre)

#An�lise de variancia
# Rejeita-se a hip�tese nula, quando pvalue menor que alfa
# A hip�tese nula nos diz que n�o existe rela��o entre as vari�vies
# Condiderando-se um alfa de 95% de confian�a (alfa=0,05):
# Observa-se pvalue=0,000126, ou seja, menor que alfa:
# H� evid�ncias para rejeitar a hip�tese nula das m�dias, ou seja:
# Pelo menos um dos quadrimestres possui o pre�o m�dio diferente dos demais. 
anova <- aov(Preco ~ Quadrimestre, data = dados)
summary(anova)

#Explore a rela��o entre as vari�veis Pre�o e Portas, responda:

boxplot(dados$Preco ~ dados$Portas)

#Test t de Student: Pre�os e portas:
#Considerando 95% de confian�a o valor do pvalue=0,88884 (88,8%) � maior que alfa
#N�o h� evid�ncias para rejeitar a hip�tese nula de igualdade de m�dias
#N�o existe diferen�a significativa entre o pre�o m�dio do aluguel do ve�culo com duas portas 
#Em rela��o (compara��o) com o pre�o m�dio do ve�culo de quatro portas
t.test(dados$Preco ~ dados$Portas , 
       paired = FALSE, #amostras nao pareadas
       alternative = 'two.sided', #bilateral
       conf.level = 0.95 #95% de confianca
)

#Rela��o entre as vari�veis Pre�o e Quilometragem:
#Pelo gr�fico de dispers�o, identifica-se que existe a rela��o linear entre o Pre�o e a Quilometragem
#Essa rela��o � linear negativa, pois � medida que a quilometragem aumenta o pre�o diminui. 

plot(y = dados$Preco ,
     x = dados$Quilometragem,
     pch = 16)

#Coeficiente de correlacao
#o valor do coeficiente de correla��o linear de Pearson entre o Pre�o e a Quilometragem
#� de -0,82,isso nos informa que � uma correla��o negativa alta.
cor(dados$Preco, dados$Quilometragem)

#Regressao linear do Preco em funcao da Quilometragem
# O R2 � de 67,76%, ou seja, a vari�vel Quilometragem consegue explicar 67,76% da varia��o do Pre�o.
regressao_linear <- lm(Preco ~ Quilometragem, data = dados)
summary(regressao_linear)

#Analise descritiva da variavel quilometragem
# O primeiro quartil � 554,7, isso nos diz que at� 25% dos ve�culos alugados possuem quilometragem at� 554,7.
# Oterceiro quartil � 925,2, isso nos diz que at� 75% dos ve�culos alugados possuem quilometragem at� 925,2.

summary(dados$Quilometragem)

#Coeficiente de variacao
#O coeficiente de varia��o � 27,74%, ou seja, 
#os valores da vari�vel quilometragem variam em m�dia 27,74% em torno de sua m�dia.
sd(dados$Quilometragem) / mean(dados$Quilometragem)

#Explore a correlacao entre o Dolar e o Preco
#O gr�fico de dispers�o n�o apresenta nenhum padr�o entre as duas vari�veis, ou seja, na medida que o D�lar aumenta,
#o Pre�o n�o cresce nem decresce, ou seja, n�o h� correla��o entre as duas vari�veis.
#O valor do coeficiente de correla��o linear de Pearson � -0,06 (pr�ximo do zero), que indica aus�ncia de correla��o linear.
#N�o � poss�vel prever o pre�o com base no d�lar:
#N�o seria poss�vel, pois o p valor do coeficiente beta do Dolar � de 0,77 (77%), ou seja, 
#independente do n�vel de signific�ncia adotado, 
#a vari�vel Dolar n�o exerce influ�ncia significativa na vari�vel Preco,
#portanto, n�o � poss�vel prever o Pre�o baseado no D�lar.
plot(y = dados$Preco,
     x = dados$Dolar,
     pch = 16)

cor(dados$Preco, dados$Dolar)

regressao_linear <- lm(Preco ~ Dolar, data = dados)
summary(regressao_linear)