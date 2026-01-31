#Programa de Pós-Graduação em Políticas Públicas
#Disciplina: Métodos Quantitativos
#Nome: Manuel Mfinda Pedro Marques
#Data: 27/11/2025
#MQ101 - Lista 05

#Exercício 1 – Probabilidade como frequência de longo prazo (moeda viesada)

set.seed(123)

# Probabilidade de cara
p_cara <- 0.3

# Tamanhos de amostra
n_vec  <- c(1, 10, 100, 1000, 10000)

# Data frame para armazenar os resultados
result <- data.frame(
  n      = n_vec,
  p_cara = NA_real_
)

# TODO: completar o loop para simular os lançamentos
for (i in seq_along(n_vec)) {
  x <- rbinom(n=1, size = n_vec[i], prob = 0.3)
  result$p_cara[i] <- x/n_vec[i]
}

# TODO: inspecionar result
print(result)

# TODO: fazer um gráfico de linha da proporção de caras vs n

#plot(...)

#Bruna
plot(result$n, result$p_cara, type = "o", col = "blue",
     xlab = "Tamanho da amostra (n)",
     ylab = "Proporção de caras",
     main = "Proporção de caras vs tamanho da amostra")

#Celia - outra opção de plotar

plot(result$n, result$p_cara, 
     type = "b", 
     pch = 19, 
     col = "blue",
     xlab = "Tamanho da amostra (n)",
     ylab = "Proporção de caras",
     main = "Proporção de caras vs tamanho da amostra")

abline(h = p_cara, lty = 2, col = "red")  # linha da probabilidade teórica

#Exercício 2 - Bernoulli, Binomial e probabilidades exatas (satisfação em saúde)

set.seed(123)

# Parâmetros
p <- 0.65
n <- 20

# (1) Simulação de uma amostra
y <- rbinom(n, size = 1, prob = p) #aleatorio
print(y)
# (2) Contar o número de satisfeitos
num_satisfeitos <- sum(y) #contagem de pessoas satisfeitas

# (3a) Probabilidade P(S = 12) pela Binomial
prob_12 <- dbinom(12, size = n, prob = p)
print(prob_12)

# (3b) Probabilidade P(S >= 12)
prob_12_ou_mais <- sum(dbinom(12:n, size = n, prob = p))
print(prob_12_ou_mais)

# ou:
# prob_12_ou_mais <- 1 - pbinom(11, size = n, prob = p)

# TODO: imprimir os resultados e escrever a interpretação fora do código.
print(prob_12)
print(prob_12_ou_mais)

#Exercício 3 - Probabilidade condicional e independência (base saúde)

set.seed(123)

N <- 5000

dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
)

# (2) Estimar as probabilidades

P_fumante <- mean(dados_saude$fumante == 1)
print(P_fumante) #0.2112
P_fumante_F <- mean(dados_saude$fumante[dados_saude$sexo == "F"] == 1)
print(P_fumante_F)#0.2178506
P_fumante_M <- mean(dados_saude$fumante[dados_saude$sexo == "M"] == 1)
print(P_fumante_M)#.2031042


# (4) Gráfico de barras da proporção de fumantes por sexo

install.packages("dplyr")
install.packages("ggplot2")

# Dica: usar dplyr + ggplot2
library(dplyr); 
library(ggplot2)

tab_fumo <- dados_saude |>
  dplyr::group_by(sexo) |>
  dplyr::summarise(prop_fumante = mean(fumante))

ggplot(tab_fumo, aes(x = sexo, y = prop_fumante, fill=sexo)) +
  geom_col()
scale_fill_manual(values = c("F" = "pink", "M" = "blue"))  
labs(
  x = "sexo",
  y = "Proporção de fumantes",
  title ="Proporção de fumantes por sexo")


# TODO: escrever interpretação fora do código.
#Exercício 4– Probabilidade conjunta e regra do produto (saúde)

# Atenção: este exercício supõe que dados_saude já foi criado no Exercício 3.

# (1) Probabilidades marginais e conjunta

P_hipertenso <- mean(dados_saude$hipertenso == 1)
print(P_hipertenso) #0.3014
P_fumante    <- mean(dados_saude$fumante == 1)
print(P_fumante) #0.2112
P_hip_e_fum  <- mean(dados_saude$hipertenso == 1 & dados_saude$fumante == 1)
print(P_hip_e_fum) #0.0658

# (2) Probabilidade condicional

P_hip_dado_fum <- mean(dados_saude$hipertenso[dados_saude$fumante == 1] == 1)
print(P_hip_dado_fum) #0.311553

# (3) Produto P(hipertenso = 1 | fumante = 1) * P(fumante = 1)
P_produto <- P_hip_dado_fum * P_fumante
print(P_produto) #0.0658
# TODO: comparar P_hip_e_fum e P_produto, e interpretar.
#Exercício 5 –  Bayes “de bolso” em triagem de benefícios

set.seed(123)

P_F         <- 0.02
P_T_dado_F  <- 0.9
P_T_dado_Fc <- 0.05
P_Fc        <- 1 - P_F

# (1) Cálculo analítico de P(F|T)
P_F_dado_T <- (P_T_dado_F * P_F) /
  (P_T_dado_F * P_F + P_T_dado_Fc * P_Fc)
print(P_F_dado_T) #0.2686567

# (2) Simulação
N <- 100000

fraude <- rbinom(N, 1, P_F)
alerta <- ifelse(
  fraude == 1,
  rbinom(N, 1, P_T_dado_F),
  rbinom(N, 1, P_T_dado_Fc))

# (3) Estimar empiricamente P(F|Alerta)
P_empirico <- mean(fraude[alerta == 1] == 1)
print(P_empirico) #0.2637992

# TODO: comparar P_F_dado_T e P_empirico, e interpretar em texto.  

c(Empirico = P_empirico,
  Analitico = P_F_dado_T,
  Diferencia = P_empirico - P_F_dado_T)

#   Empirico    Analitico   Diferencia 
#   0.263799178  0.268656716 -0.004857538 

# Exercício 6 – Teorema Central do Limite com renda

set.seed(123)

N <- 100000
renda_pop <- rgamma(N, shape = 2, rate = 1/2500)

# Função auxiliar para simular médias
simular_medias <- function(n, n_rep = 5000) {
  medias <- numeric(n_rep)
  for (i in seq_len(n_rep)) {
    amostra <- sample(renda_pop, n, replace = TRUE)
    medias[i] <- mean(amostra)
  }
  medias 
}

medias_n30  <- simular_medias(30)
medias_n200 <- simular_medias(200)

# TODO: produzir histogramas para as distribuições de médias
par(mfrow = c(2, 1))
hist(medias_n30,  main = "Médias amostrais de renda (n = 30)")
hist(medias_n200, main = "Médias amostrais de renda (n = 200)")
par(mfrow = c(1, 1))

#Exercício 6 (Teorema Central do Limite)
# ---------------------------------------------------------
# Exercício 6 — Teorema Central do Limite (TCL) com renda
# ---------------------------------------------------------

set.seed(123)

# 1. Gerar população assimétrica
N <- 100000
renda_pop <- rgamma(N, shape = 2, rate = 1/2500)

# ---------------------------------------------------------
# 2. Função que simula médias amostrais
# ---------------------------------------------------------

simular_medias <- function(n, n_rep = 5000) {
  medias <- numeric(n_rep)
  for (i in seq_len(n_rep)) {
    amostra <- sample(renda_pop, n, replace = TRUE)
    medias[i] <- mean(amostra)
  }
  return(medias)
}

# ---------------------------------------------------------
# 3. Simulações para n = 30 e n = 200
# ---------------------------------------------------------

medias_n30  <- simular_medias(30)
medias_n200 <- simular_medias(200)

# ---------------------------------------------------------
# 4. Histogramas
# ---------------------------------------------------------

par(mfrow = c(2, 1))  # layout com 2 gráficos, um em cima do outro

hist(medias_n30,
     main = "Distribuição das Médias Amostrais da Renda (n = 30)",
     xlab = "Média da Renda",
     col = "skyblue",
     border = "white")

hist(medias_n200,
     main = "Distribuição das Médias Amostrais da Renda (n = 200)",
     xlab = "Média da Renda",
     col = "salmon",
     border = "white")

par(mfrow = c(1, 1))  # volta ao normal

#Exercício 7  Intervalo de Confiança para proporção.
set.seed(123)

# Caso não tenha salvo dados_saude, recriar:
N <- 5000
dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
)

# Tamanho da amostra
n <- 400

# (1) Amostra aleatória simples
amostra_saude <- dados_saude[sample(1:nrow(dados_saude), n), ]

# (2) Proporção amostral de hipertensos
p_hat <- mean(amostra_saude$hipertenso)

# Erro padrão aproximado
SE_p <- sqrt(p_hat * (1 - p_hat) / n)

# Intervalo de confiança de 95%
IC_95 <- c(
  inferior = p_hat - 1.96 * SE_p,
  superior = p_hat + 1.96 * SE_p
)

# (3) Proporção verdadeira na população
p_verdadeiro <- mean(dados_saude$hipertenso)

# Mostrar resultados
p_hat
SE_p
IC_95
p_verdadeiro

#Exercício 8 Correlação, Regressão Simples e Inferência (Educação)
set.seed(123)

# (1) Gerar base simulada
N <- 2000

dados_educacao <- data.frame(
  ideb        = rnorm(N, mean = 5.5, sd = 0.7),
  gasto_aluno = rnorm(N, mean = 6000, sd = 1500)
)

# Introduzir correlação positiva leve entre gasto e IDEB
dados_educacao$ideb <- dados_educacao$ideb +
  0.0002 * (dados_educacao$gasto_aluno - 6000)

# (2) Correlação de Pearson
cor_ideb_gasto <- cor(dados_educacao$ideb, dados_educacao$gasto_aluno)

# (3) Regressão simples
modelo <- lm(ideb ~ gasto_aluno, data = dados_educacao)

# Resumo da regressão
summary(modelo)

# Intervalo de confiança do coeficiente angular
IC_coef <- confint(modelo)

# Exibir resultados
cor_ideb_gasto
IC_coef
#Exercício 9 — Margem de erro e tamanho amostral (TSE)
set.seed(123)

# (1) Criar população de municípios
N <- 5000

dados_tse <- data.frame(
  id_mun       = 1:N,
  prop_partido = rbeta(N, shape1 = 10, shape2 = 15)  # proporção "verdadeira"
)

# (2) Parâmetros da pesquisa
n_amostra <- 600
n_rep     <- 1000

# (3) Vetor para armazenar as proporções amostrais simuladas
p_hat_vec <- numeric(n_rep)

# (4) Simulação repetida 1000 vezes
for (r in 1:n_rep) {
  
  # sortear um município
  id_escolhido <- sample(dados_tse$id_mun, 1)
  
  # proporção verdadeira desse município
  p_mun <- dados_tse$prop_partido[id_escolhido]
  
  # sortear 600 eleitores — variável Bernoulli
  votos <- rbinom(n_amostra, size = 1, prob = p_mun)
  
  # proporção amostral
  p_hat_vec[r] <- mean(votos)
}

# (5) Margem de erro "de praxe" (pior caso)
ME <- 1.96 * sqrt(0.25 / n_amostra)
ME

# (6) Histograma das proporções simuladas
hist(p_hat_vec,
     breaks = 30,
     col = "lightblue",
     main = "Distribuição de p̂ (proporção amostral)",
     xlab = "p̂")
abline(v = mean(p_hat_vec), col = "red", lwd = 2)   # média observada

#Exercício 10 — Regressão e distribuição amostral do coeficiente (TSE)
set.seed(123)

# (1) Criar base populacional de municípios
N <- 5000

dados_tse <- data.frame(
  id_mun      = 1:N,
  renda_media = rnorm(N, mean = 2500, sd = 600)
)

# Gerar proporção de votos dependente da renda, com ruído
dados_tse$prop_partido <- plogis(
  -1 + 0.0006 * dados_tse$renda_media + rnorm(N, 0, 0.3)
)

# (2) Parâmetros da pesquisa simulada
n_mun_amostra <- 300     # número de municípios a serem pesquisados
n_eleitores   <- 400     # tamanho da amostra por município
n_rep         <- 500     # número de simulações repetidas

# Vetor para armazenar os coeficientes angulares estimados
coef_angular <- numeric(n_rep)

# (3) Loop de simulação
for (r in 1:n_rep) {
  
  # (4) Sortear 300 municípios
  mun_sorteados <- sample(dados_tse$id_mun, n_mun_amostra)
  base_pesq <- dados_tse[dados_tse$id_mun %in% mun_sorteados, ]
  
  # Criar vetor para armazenar p_hat
  p_hat <- numeric(n_mun_amostra)
  
  # Para cada município, simular votos e calcular proporção
  for (i in seq_len(n_mun_amostra)) {
    p_true <- base_pesq$prop_partido[i]
    votos  <- rbinom(n_eleitores, size = 1, prob = p_true)
    p_hat[i] <- mean(votos)
  }
  
  # Inserir p_hat na base
  base_pesq$p_hat <- p_hat
  
  # (5) Ajustar regressão simples
  modelo <- lm(p_hat ~ renda_media, data = base_pesq)
  
  # Guardar coeficiente angular
  coef_angular[r] <- coef(modelo)[2]
}

# (6) Histograma dos coeficientes angulares
hist(
  coef_angular,
  breaks = 30,
  col = "lightblue",
  main = "Distribuição dos coeficientes angulares estimados",
  xlab  = "Coeficiente angular (β̂)"
)

abline(v = mean(coef_angular), col = "red", lwd = 2)
#############################################################
# 12) Observações Finais
############################################################
# Esta lista integra diferentes dimensões da Estatística —
# estatística descritiva, visualização de dados, medidas de
# associação, correlação, regressão simples, probabilidade
# e introdução à inferência estatística.
#
# Cada etapa fornece instrumentos essenciais para analisar
# fenômenos sociais e subsidiar decisões no campo das políticas
# públicas.
#
# Ao interpretar os resultados, vá além dos números: uma média,
# um coeficiente ou um gráfico só ganham sentido quando
# conectados a problemas reais, como:
#   - alocação de recursos;
#   - identificação de grupos vulneráveis;
#   - priorização de políticas setoriais;
#   - avaliação de programas sociais;
#   - auditoria e monitoramento de metas;
#   - revisão de estratégias governamentais.
#
# Por isso, em todas as respostas, priorize interpretações
# traduzidas para a linguagem de políticas públicas,
# destacando como cada análise pode apoiar decisões concretas
# no planejamento, implementação ou avaliação de políticas.
#
# O objetivo final é desenvolver uma leitura crítica e aplicada
# da evidência estatística, fortalecendo a tomada de decisão
# baseada em dados.
############################################################





