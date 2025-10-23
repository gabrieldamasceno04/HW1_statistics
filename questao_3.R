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

#3) Vaz faz daqui para baixo: 
