# Nome: Manuelmfinda pedro Marques
#Matricula:21202510196
# Métodos Quantitativos
# Lista 05
install.packages("tidyverse")
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
  x <- rbinom(n = 1, size = n_vec[i], prob = 0.3)
  result$p_cara[i] <- x/n_vec[i]
}

# TODO: inspecionar result
print(result)

# TODO: fazer um gráfico de linha da proporção de caras vs n
plot(result$n, result$p_cara, type="b",
pch=19,
col="blue",
xlab= "Tamanho da amostra",
ylab= "Proporção de caras",
main= "Simulação")
abline(h = p_cara, lty = 2, col = "red")

# Exercício 3 - Bernoulli
set.seed(123)

# Parâmetros
p <- 0.65
n <- 20

# (1) Simulação de uma amostra
 y <- rbinom(n, size = 1, prob = p)

# (2) Contar o número de satisfeitos
 num_satisfeitos <- sum(y)
 print(num_satisfeitos)

# (3a) Probabilidade P(S = 12) pela Binomial
 prob_12 <- dbinom(12, size = n, prob = p)
 print(prob_12)

# (3b) Probabilidade P(S >= 12)
 prob_12_ou_mais <- sum(dbinom(12:n, size = n, prob = p))
 print(prob_12_ou_mais)
 
# ou:
prob_12_ou_mais <- 1 - pbinom(11, size = n, prob = p)
print(prob_12_ou_mais)

# TODO: imprimir os resultados e escrever a interpretação fora
print(prob_12_ou_mais)
print(prob_12_ou_mais)

# Exercício 3

set.seed(123)

N <- 5000

dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
)

# (2) Estimar as probabilidades
 P_fumante <- mean(dados_saude$fumante == 1)
 print(P_fumante)
 
 P_fumante_F <- mean(dados_saude$fumante[dados_saude$sexo == "F"] == 1)
 print(P_fumante_F)
 
 P_fumante_M <- mean(dados_saude$fumante[dados_saude$sexo == "M"] == 1)
 print(P_fumante_M)

# (4) Gráfico de barras da proporção de fumantes por sexo
# Dica: usar dplyr + ggplot2
 install.packages("dplyr")
 install.packages("ggplot2")
library(dplyr) 
library(ggplot2)
  tab_fumo <- dados_saude |>
   dplyr::group_by(sexo) |>
   dplyr::summarise(prop_fumante = mean(fumante))

 ggplot(tab_fumo, aes(x = sexo, y = prop_fumante,fill=sexo)) +
   geom_col()+
   scale_fill_manual(values =c("F"="pink", "M" = "lightblue")) +
   labs(x="Sexo",
        y="Proporção do fumantes",
        title="Proporção dos fumantes por sexo"
         )
 
# TODO: escrever interpretação fora do código.
 # O gráfico demonstraque a quantidade de fumantes do sexo feminino supera o do sexo masculino, na amostra que foi selecionada.
 
# Exercício 5

 # Atenção: este exercício supõe que dados_saude jÃ¡ foi criado no Exercício 3.
 
 # (1) Probabilidades marginais e conjunta
  P_hipertenso <- mean(dados_saude$hipertenso == 1)
  print(P_hipertenso)
  
  P_fumante    <- mean(dados_saude$fumante == 1)
  print(P_fumante)
  
  P_hip_e_fum  <- mean(dados_saude$hipertenso == 1 & dados_saude$fumante == 1)
  print(P_hip_e_fum)
 
 # (2) Probabilidade condicional
 P_hip_dado_fum <- mean(dados_saude$hipertenso[dados_saude$fumante == 1] == 1)
 print(P_hip_dado_fum)
 
 # (3) Produto
  P_produto <- P_hip_dado_fum * P_fumante
  print( P_produto)
 
 # TODO: comparar P_hip_e_fum e P_produto, e interpretar.
  # O valor de 0,0654 corresponde a 6,54% da amostra, portanto sendo a proporção de indivíduos que são hipertensos e fumantes ao mesmo tempo.
  # 31,05% dos indivíduos da amostra são hipertensos dentre os que cultivam o hábito de fumar.
  # Se o número de indivíduos hipertensos e fumantes é 6,54% e a quantidade de fumantes que são hipertensos é 31,05%, para compreender quem são os
  #fumantes da amostra através do cálculo, aplica-se uma fórmula:
  # Grupo dos hipertensos e fumantes = Grupo dos fumantes que são hipertensos * Grupo dos fumantes não hipertensos
  # O resultado deste cálculo é 21,06% são apenas fumantes.
 
  # Exercício 6 
  
  set.seed(123)
  
  P_F         <- 0.02
  P_T_dado_F  <- 0.9
  P_T_dado_Fc <- 0.05
  P_Fc        <- 1 - P_F
  
  # (1) Cálculo analítico de P(F|T)
 P_F_dado_T <- (P_T_dado_F * P_F) /
   (P_T_dado_F * P_F + P_T_dado_Fc * P_Fc)
  
  # (2) Simulação
  N <- 100000
  
  fraude <- rbinom(N, 1, P_F)
     alerta <- ifelse(
     fraude == 1,
     rbinom(N, 1, P_T_dado_F),
     rbinom(N, 1, P_T_dado_Fc)
   )
  
  # (3) Estimar empiricamente P(F|Alerta)
   P_empirico <- mean(fraude[alerta == 1] == 1)
   print(P_empirico)
  
  # TODO: comparar P_F_dado_T e P_empirico, e interpretar em texto.
   c(Empirico=P_empirico,
     Analitico= P_F_dado_T,
     Diferença= P_empirico-P_F_dado_T)
   
   
#   Exercício 7
   
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
    
#    Exercício 8
    
    set.seed(123)
    
    # Caso não tenha salvo dados_saude, recriar:
     N <- 5000
     dados_saude <- data.frame(
       sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
       fumante    = rbinom(N, 1, 0.22),
       hipertenso = rbinom(N, 1, 0.30)
     )
   
         n <- 400
    
    # (1) Amostra aleatória simples
     amostra_saude <- dados_saude[sample(1:nrow(dados_saude), n), ]
    
    # (2) Proporção amostral e IC
     p_hat <- mean(amostra_saude$hipertenso)
     SE_p  <- sqrt(p_hat * (1 - p_hat) / n)
     IC_95 <- c(
       inferior = p_hat - 1.96 * SE_p,
       superior = p_hat + 1.96 * SE_p
     )
     
    # (3) Proporção verdadeira na população
     p_verdadeiro <- mean(dados_saude$hipertenso)
     
     head(dados_saude)
     str(dados_saude)
     
     table(dados_saude$sexo)
     table(dados_saude$fumante)
     table(dados_saude$hipertenso)
     
     prop.table(table(dados_saude$sexo))
     prop.table(table(dados_saude$fumante))
     prop.table(table(dados_saude$hipertenso))
    
    # TODO: comparar p_hat, IC_95 e p_verdadeiro em texto.
     
     #p_hat: em uma amostra de 400 pacientes, 33% são hipertensos, ou seja, 132 pacientes.
     #IC_95:  os valores apresentados com Named num [1:2] 0,284  0,376 representam o intervalo de confiança, considerando que o valor de p_hat é 33%, os número de hipertensos na amostra analisada por variar entre 28,4 e 37,6%. 
     # p_verdadeiro revela qual é a proporção de hipertensos dentre os 5000 pacientes, população total analisada na simulação. 
     
     # Exercício 9 Correlação, regressão simples e inferência (educação)

     set.seed(123)
     
     N <- 2000
     
     dados_educacao <- data.frame(
       ideb        = rnorm(N, mean = 5.5, sd = 0.7),
       gasto_aluno = rnorm(N, mean = 6000, sd = 1500)
     )
     
     # Introduzir correlação leve (opcional)
     dados_educacao$ideb <- dados_educacao$ideb +
       0.0002 * (dados_educacao$gasto_aluno - 6000)
     
     # (2) Correlação de Pearson
      cor_ideb_gasto <- cor(dados_educacao$ideb, dados_educacao$gasto_aluno)
      cor_ideb_gasto
      print(cor_ideb_gasto)
     
     # (3) Regressão simples
      modelo <- lm(ideb ~ gasto_aluno, data = dados_educacao)
      summary(modelo)
      confint(modelo)
     
     # TODO: interpretar os coeficientes, valor-p e IC em texto.
      
    # Sobre o coeficiente de correlação de Pearson, o valor de (+)0,38 indica uma correlação positiva fraca. 
    # Busquei uma fonte que houve alguma classificação para os valores desta correlação: https://ole.uff.br/wp-content/uploads/sites/419/2019/04/Aula_03_a_Pearson.pdf e 
      # https://utstat.utoronto.ca/brunner/oldclass/378f16/readings/CohenPower.pdf.
    # O coeficiente da regressão linear tem o intercepto estimado (Ideb) no valor de 4,34 considerando-se um suposto valor zero de gasto por aluno.
    # Considerando um valor de investimento de R$1,00/aluno, a média aumentada do Ideb seria 1.942e-04, ou seja, 0,0001942.
    # O valor-p com valor extremamente baixo (<2e-16) indica uma relação estatistica entre gasto/aluno e o índice do Ideb de causalidade significativa.
    # Através do cálculo do intervalo de confiança 95% pela regressão linear(código lm) , cujos valores são acima de 0, sendo positivos (0.0001740586 a 0.0002143202).
    # O valor 'multiple R-squared" = 0,1519 demonstra que 15,19% de variação do Ideb ocorre por influência dos gastos, lembrando que há outros fatores que impactam este índice.  