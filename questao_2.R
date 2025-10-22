#QUESTÃO 2

#1)

#colunas da tabela

idade <- c(28,34,46,26,37,29,51,31,39,43,58,44,25,23,52,42,48,33,38,46)
nacionalidade = c("italiana", "inglesa", "belga", "espanhola", "italiana", "espanhola",
                  "francesa", "belga", "italiana", "italiana", "italiana", "inglesa",
                  "francesa", "espanhola", "italiana", "alemã", "francesa", "italiana",
                  "alemã", "italiana")
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1,
          2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2,8,21,1,15,3,28,5,13,20,32,23,1,0,29,18,19,7,12,23)

tabela <- data.frame(Idade = idade, Nacionalidade = nacionalidade, Renda = renda,
                    Experiencia = experiencia)

print(tabela)

#media, mediana e desvio padrão para idade, renda e experiência
media_idade <- mean(tabela$Idade)
cat("Média da idade: ", media_idade, "\n")
media_renda <- mean(tabela$Renda)
cat("Média da renda desejada: ", media_renda, "\n")
media_exp   <- mean(tabela$Experiencia)
cat("Média da experiência: ", media_exp, "\n")

mediana_idade <- median(tabela$Idade)
cat("Mediana da idade: ", mediana_idade, "\n")
mediana_renda <- median(tabela$Renda)
cat("Mediana da renda desejada: ", mediana_renda, "\n")
mediana_exp   <- median(tabela$Experiencia)
cat("Mediana da experiência: ", mediana_exp, "\n")

dp_idade <- sd(tabela$Idade)
cat("Desvio padrão da idade: ", dp_idade, "\n")
dp_renda <- sd(tabela$Renda)
cat("Desvio padrão da renda desejada: ", dp_renda, "\n")
dp_exp   <- sd(tabela$Experiencia)
cat("Desvio padrão da experiência: ", dp_exp, "\n")

#2)

tabela_alema <- tabela[tabela$Nacionalidade == "alemã", ]
media_alema_renda <- mean(tabela_alema$Renda)
cat("Renda alemã média desejada: ", media_alema_renda, '\n')
media_alema_exp <- mean(tabela_alema$Experiencia)
cat("Experiencia alemã média: ", media_alema_exp, '\n')

tabela_italiana <- tabela[tabela$Nacionalidade == "italiana", ]
media_italiana_renda <- mean(tabela_italiana$Renda)
cat("Renda italiana média desejada: ", media_italiana_renda, '\n')
media_italiana_exp <- mean(tabela_italiana$Experiencia)
cat("Experiencia italiana média: ", media_italiana_exp, '\n')

tabela_belga <- tabela[tabela$Nacionalidade == "belga", ]
media_belga_renda <- mean(tabela_belga$Renda)
cat("Renda belga média desejada: ", media_belga_renda, '\n')
media_belga_exp <- mean(tabela_belga$Experiencia)
cat("Experiencia belga média: ", media_belga_exp, '\n')

tabela_espanhola <- tabela[tabela$Nacionalidade == "espanhola", ]
media_espanhola_renda <- mean(tabela_espanhola$Renda)
cat("Renda espanhola média desejada: ", media_espanhola_renda, '\n')
media_espanhola_exp <- mean(tabela_espanhola$Experiencia)
cat("Experiencia espanhola média: ", media_espanhola_exp, '\n')

tabela_francesa <- tabela[tabela$Nacionalidade == "francesa", ]
media_francesa_renda <- mean(tabela_francesa$Renda)
cat("Renda francesa média desejada: ", media_francesa_renda, '\n')
media_francesa_exp <- mean(tabela_francesa$Experiencia)
cat("Experiencia francesa média: ", media_francesa_exp, '\n')

tabela_inglesa <- tabela[tabela$Nacionalidade == "inglesa", ]
media_inglesa_renda <- mean(tabela_inglesa$Renda)
cat("Renda inglesa média desejada: ", media_inglesa_renda, '\n')
media_inglesa_exp <- mean(tabela_inglesa$Experiencia)
cat("Experiencia inglesa média: ", media_inglesa_exp, '\n')

#retorna o índice da posição que contém o maior valor
nacionalidades <- c("alema", 'belga', 'espanhola', 'francesa', 'inglesa', 'italiana')

maior_renda_nacionalidade <- which.max(c(media_alema_renda, media_belga_renda, media_espanhola_renda,
                              media_francesa_renda, media_inglesa_renda, media_italiana_renda))
cat("Maior renda desejada: ", nacionalidades[maior_renda_nacionalidade], '\n')

maior_exp_nacionalidade <- which.max(c(media_alema_exp, media_belga_exp, media_espanhola_exp, media_francesa_exp,
                            media_inglesa_exp, media_italiana_exp))
cat("Maior experiência: ", nacionalidades[maior_exp_nacionalidade], '\n')

#3)

#Gráfico de dispersão
plot(x = experiencia, y = renda,
     main = "Gráfico de dispersão Exp x Renda desejada",
     xlab = "Experiência (anos)",
     ylab = "Renda desejada (x1000 Euros)",
     col = 'green',
     pch = 19
     )

#Coeficiente de correlação de Pearson

coef_pearson <- cor(x = experiencia, y = renda)
cat("Coeficiente de correlação de Pearson: ", coef_pearson, '\n')

#valor Retornado --> 0.4977672, não dá indícios de uma correlação forte.

#4)

tabela_filtrada <- tabela[tabela$Renda < 2 & tabela$Experiencia >= 10, ]

cat("Exatamente", length(tabela_filtrada), " candidatos 
    desejam menos de 2000 euros com pelo menos 10 anos de exp.\n")
print(tabela_filtrada)

#5)

#histograma das idades, por nacionalidade:

hist(tabela_alema$Idade, 
     main = "Idades dos alemães",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'red',
     border = 'darkred'
    )

hist(tabela_belga$Idade, 
     main = "Idades dos belgas",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'lightyellow',
     border = 'yellow'
)

hist(tabela_espanhola$Idade, 
     main = "Idades dos espanhois",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'grey',
     border = 'darkgrey'
)

hist(tabela_francesa$Idade, 
     main = "Idades dos franceses",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'pink',
     border = 'lightpink'
)

hist(tabela_inglesa$Idade, 
     main = "Idades dos ingleses",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'violet',
     border = 'darkviolet'
)

hist(tabela_italiana$Idade, 
     main = "Idades dos italianos",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'black',
     border = 'grey'
)

#Histograma das rendas desejadas, por nacionalidade

hist(tabela_alema$Renda, 
     main = "Rendas desejadas pelos alemães",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'red',
     border = 'darkred'
)

hist(tabela_belga$Renda, 
     main = "Rendas desejadas pelos belgas",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'lightyellow',
     border = 'yellow'
)

hist(tabela_espanhola$Renda, 
     main = "Rendas desejadas pelos espanhois",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'grey',
     border = 'darkgrey'
)

hist(tabela_francesa$Renda, 
     main = "Rendas desejadas pelos franceses",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'pink',
     border = 'lightpink'
)

hist(tabela_inglesa$Renda, 
     main = "Rendas desejadas pelos ingleses",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'violet',
     border = 'darkviolet'
)

hist(tabela_italiana$Renda, 
     main = "Rendas desejadas pelos italianos",
     ylab = "Densidade",
     xlab = "Idades",
     freq = FALSE,
     col = 'black',
     border = 'grey'
)

#boxplots

boxplot(Idade ~ Nacionalidade,
        data = tabela,
        main = "Boxplot comparativo",
        col = "skyblue",
        ylab = 'Idade',
        xlab = "Nacionalidade"
        )

#fim da questão 2





