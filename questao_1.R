#Questão 1

#1)

emissoes_diarias <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8, 21.9, 10.5,
                     17.3, 6.2, 18.0, 22.9, 24.6, 19.4, 12.3, 15.9, 20.1, 17.0, 22.3, 27.5,
                     23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 20.9, 21.4, 18.0, 24.3, 11.8, 17.9,
                     18.7, 12.8, 15.5, 19.2, 13.9, 28.6, 19.4, 21.6, 13.5, 24.6, 20.0, 24.1,
                     9.0, 17.6, 25.7, 20.1, 13.2, 23.7, 10.7, 19.0, 14.5, 18.1, 31.8, 28.5,
                     22.7, 15.2, 23.0, 29.6, 11.2, 14.7, 20.5, 26.6, 13.3, 18.1, 24.8, 26.1,
                     7.7, 22.5, 19.3, 19.4, 16.7, 16.9, 23.5, 18.4)

#medidas de tendência central

media_emissoes <- mean(emissoes_diarias) #se tiver valores ausentes (NA), retornaria NA tbm
cat("Média das emissões: ", media_emissoes, "\n")

emissoes_ordenadas <- sort(emissoes_diarias, decreasing = FALSE) #define crescente ou não
cat("Estatísticas de ordem: ", emissoes_ordenadas, "\n")

mediana_emissoes <- median(emissoes_diarias)
cat("Mediana das emissões:", mediana_emissoes, "\n")

tabela_freq_emissoes <- table(emissoes_diarias)
cat("Tabela de frequência:\n", tabela_freq_emissoes, "\n")
#observando a tabela de frequência, percebe-se que a moda corresponde ao valor 19.4, que
#aparece 3x.
moda = names(tabela_freq_emissoes)[which.max(tabela_freq_emissoes)] 
#names retorna os rótulos, nomes (19.4) que estão por cima da tabela de frequência
#enquanto 'which.max' retorna o índice (posição) do maior valor da tabela de freq. (3)
cat("Moda das emissões:", moda, "\n")

#medidas de dispersão

std_emissoes = sd(emissoes_diarias)
cat("Desvio padrão:", std_emissoes, "\n")
#o coeficiente de variação (cv) se obtém assim: (std/mean)*100
cv_emissoes = (std_emissoes/media_emissoes)*100
cat("Coeficiente de variação", cv_emissoes, '\n')

#2)

#histograma

hist(emissoes_diarias, breaks = seq(5, 35, by = 5), col = 'lightgreen',
                  main = "Histograma das emissões diárias",
                  freq = FALSE,
                  xlab = "Emissão de gás poluente",
                  ylab = "Densidade",
                  border = 'darkgreen',
)

#Diria que há uma distribuição que se aproxima de uma simétrica. 


#boxplot

boxplot(emissoes_diarias,
        main = "Boxplot das emissões de gás poluente",
        ylab = "Emissões",
        col = "lightblue",
        border ="darkblue"
)

#sem valores atipicos, todos os pontos acima do LI, e abaixo do LS

#3 

q1 <- quantile(emissoes_diarias, probs = 0.25)
cat("Q1: ", q1, '\n')

q2 <- quantile(emissoes_diarias, probs = 0.50)
cat("Q2:", q2, '\n')

q3 <- quantile(emissoes_diarias, probs = 0.75)
cat("Q3:", q3, '\n')

IQR <- q3 - q1
cat("Intervalo inter-quartil:", IQR, '\n')

ls <- q3 + 1.5*IQR
cat("Limite superior:", ls, '\n')

li <- q1 - 1.5*IQR
cat("Limite inferior:", li, '\n')

#o resultado confirma nossa intuição quanto a detecção de possíveis outliers baseando-se
#no boxplot

#4) 

emissoes_diarias_filtradas = emissoes_diarias[emissoes_diarias > 25]
cat("Valores acima de 25: ", emissoes_diarias_filtradas, '\n')
cat("Temos, ao todo:", length(emissoes_diarias), "emissões,", length(emissoes_diarias_filtradas), "delas acima de 25\n")

proporcao = length(emissoes_diarias_filtradas)/length(emissoes_diarias)

cat("Proporção das emissões acima de 25 unidades: ", proporcao, "ou", proporcao*100,"%", "\n")

#FIM DA QUESTÃO 1


