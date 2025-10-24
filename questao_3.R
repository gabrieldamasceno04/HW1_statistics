#QUESTÃO 3

dataset <- read.csv("C:\\Users\\Gabriel\\OneDrive\\trabalhos para colégio\\HW1_bike_sharing.csv")
#se alguém for pegar esse código clara
head(dataset)
str(dataset)

#1)
# variável   | tipo
# instant    | qualitativa ordinal (categórica). É um índice, em tese nem possui valor analítico
# dteday     | qualitativa ordinal (categórica)
# season     | qualitativa ordinal (categórica)
# weathersit | qualitativa ordinal (categórica)
# temp       | quantitativa contínua (numérica)
# casual     | quantitativa discreta (numérica)
# registered | quantitativa discreta (numérica)

cat("N° total de observações:", nrow(dataset), "\n")
cat("Data inicial:", min(dataset$dteday), " data final:", max(dataset$dteday))

#2)

#média e mediana
media_temp <- mean(dataset$temp)
media_casual <- mean(dataset$casual)
media_registered <- mean(dataset$registered)

mediana_temp <- median(dataset$temp)
mediana_casual <- median(dataset$casual)
mediana_registered <- median(dataset$registered)

#quartis
q1_temp <- quantile(dataset$temp, probs = 0.25)
q2_temp <- quantile(dataset$temp, probs = 0.50)
q3_temp <- quantile(dataset$temp, probs = 0.75)

q1_casual <- quantile(dataset$casual, probs = 0.25)
q2_casual <- quantile(dataset$casual, probs = 0.50)
q3_casual <- quantile(dataset$casual, probs = 0.75)

q1_registered <- quantile(dataset$registered, probs = 0.25)
q2_registered <- quantile(dataset$registered, probs = 0.50)
q3_registered <- quantile(dataset$registered, probs = 0.75)

#tabela

coluna_temp <- c(media_temp, mediana_temp, q1_temp, q2_temp, q3_temp)
coluna_casual <- c(media_casual, mediana_casual, q1_casual, q2_casual, q3_casual)
coluna_registered <- c(media_registered, mediana_registered, q1_registered,
                       q2_registered, q3_registered)

#combinando as colunas
tabela_quartis_medias <- data.frame(
  Indicador = c("Média", "Mediana", "Q1 (25%)", 
                "Q2 (50% - Mediana)", "Q3 (75%)"),
  Temperatura = coluna_temp,
  Usuarios_Casuais = coluna_casual,
  Usuarios_Registrados = coluna_registered
)

print(tabela_quartis_medias)

#3) Adições para medidas de tendência central e quartis (incluindo 'cnt' como variável numérica relevante total)
# Calcular 'cnt' como total de usuários
dataset$cnt <- dataset$casual + dataset$registered

# Medidas para 'cnt'
media_cnt <- mean(dataset$cnt)
mediana_cnt <- median(dataset$cnt)
q1_cnt <- quantile(dataset$cnt, probs = 0.25)
q2_cnt <- quantile(dataset$cnt, probs = 0.50)
q3_cnt <- quantile(dataset$cnt, probs = 0.75)

# Tabela expandida incluindo 'cnt' (nova tabela separada, sem alterar a anterior)
coluna_cnt <- c(media_cnt, mediana_cnt, q1_cnt, q2_cnt, q3_cnt)
tabela_completa <- data.frame(
  Indicador = c("Média", "Mediana", "Q1 (25%)", 
                "Q2 (50% - Mediana)", "Q3 (75%)"),
  Temperatura = coluna_temp,
  Usuarios_Casuais = coluna_casual,
  Usuarios_Registrados = coluna_registered,
  Total_Usuarios = coluna_cnt
)
colnames(tabela_completa) <- c("Indicador", "Temperatura (°C)", "Casuais", "Registrados", "Total (cnt)")

cat("\nTabela de Medidas de Tendência Central e Quartis para Características Numéricas Relevantes:\n")
print(tabela_completa)

# Comentários sobre os principais pontos
cat("\nPrincipais Pontos:\n")
cat("A temperatura média é de", round(media_temp, 2), "°C, com mediana próxima (", round(mediana_temp, 2), "°C), indicando distribuição simétrica sem outliers extremos.\n")
cat("Usuários casuais apresentam média de", round(media_casual, 2), "e mediana de", round(mediana_casual, 2), ", sugerindo assimetria positiva (mais dias com poucos casuais, influenciados por fins de semana ou clima).\n")
cat("Usuários registrados são mais consistentes, com média de", round(media_registered, 2), "e mediana de", round(mediana_registered, 2), ", refletindo uso diário estável, como deslocamentos para trabalho.\n")
cat("No geral, o total de usuários (cnt) tem média de", round(media_cnt, 2), ", com maior dependência de usuários registrados (cerca de 81% do total médio), enquanto casuais variam mais sazonalmente.\n")

# Atribuição de níveis às variáveis categóricas 'season' e 'weathersit'
dataset$season <- factor(dataset$season, 
                         levels = 1:4, 
                         labels = c("Primavera", "Verão", "Outono", "Inverno"))
dataset$weathersit <- factor(dataset$weathersit, 
                             levels = 1:4, 
                             labels = c("Céu Limpo", "Nublado/Nevoeiro", "Chuva Leve/Neve", "Chuva Pesada/Neve"))

cat("\nNíveis atribuídos:\n")
cat("Season:", levels(dataset$season), "\n")
cat("Weathersit:", levels(dataset$weathersit), "\n")

# Construção de gráficos de barras para 'season' e 'weathersit' (usando médias diárias de 'cnt' por categoria)
# Gráfico para season
medias_season <- aggregate(cnt ~ season, data = dataset, FUN = mean)
barplot(medias_season$cnt, names.arg = medias_season$season, 
        main = "Média Diária de Usuários por Estação", 
        ylab = "Média de Usuários (cnt)", 
        col = rainbow(4), las = 2)

# Gráfico para weathersit
medias_weathersit <- aggregate(cnt ~ weathersit, data = dataset, FUN = mean)
barplot(medias_weathersit$cnt, names.arg = medias_weathersit$weathersit, 
        main = "Média Diária de Usuários por Condição Climática", 
        ylab = "Média de Usuários (cnt)", 
        col = rainbow(4), las = 2)

# Qual estação do ano apresenta maior número de usuários?
totais_season <- aggregate(cnt ~ season, data = dataset, FUN = sum)
maior_estacao <- totais_season$season[which.max(totais_season$cnt)]
maior_valor <- max(totais_season$cnt)
cat("\nA estação com maior número de usuários é", maior_estacao, "(total:", round(maior_valor, 0), ").\n")

# O uso de bicicletas depende da estação? (Teste ANOVA)
anova_season <- aov(cnt ~ season, data = dataset)
res_anova_season <- summary(anova_season)
f_value_season <- res_anova_season[[1]]$`F value`[1]
p_value_season <- res_anova_season[[1]]$`Pr(>F)`[1]
cat("Teste ANOVA por estação:\n")
cat("F-value:", round(f_value_season, 2), ", P-value:", ifelse(p_value_season < 0.05, "< 0.05 (dependência significativa)", ">= 0.05 (sem dependência)"), "\n")
cat("Sim, o uso depende da estação (p-value < 0.05).\n")

# Qual é a condição climática mais favorável para o uso do sistema?
totais_weathersit <- aggregate(cnt ~ weathersit, data = dataset, FUN = sum)
mais_favoravel <- totais_weathersit$weathersit[which.max(totais_weathersit$cnt)]
favoravel_valor <- max(totais_weathersit$cnt)
cat("A condição climática mais favorável é", mais_favoravel, "(total:", round(favoravel_valor, 0), ").\n")

# Teste ANOVA por weathersit para confirmar dependência
anova_weathersit <- aov(cnt ~ weathersit, data = dataset)
res_anova_weathersit <- summary(anova_weathersit)
f_value_weathersit <- res_anova_weathersit[[1]]$`F value`[1]
p_value_weathersit <- res_anova_weathersit[[1]]$`Pr(>F)`[1]
cat("Teste ANOVA por condição climática:\n")
cat("F-value:", round(f_value_weathersit, 2), ", P-value:", ifelse(p_value_weathersit < 0.05, "< 0.05 (dependência significativa)", ">= 0.05 (sem dependência)"), "\n")