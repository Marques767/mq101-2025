# MQ101 - Lista 02
# Aula: 08
# Nome: Manuel Mfinda Pedro Marques
# Data: 03/12/2025
# 1. Instalação e Carregamento de Pacotes
# O comando de instalação deve ser executado apenas uma vez.
# remova o '#' e execute 'install.packages("tidyverse")' se for a primeira vez.
# install.packages("tidyverse") 

# Carrega os pacotes necessários: 'tibble' e 'knitr' (tidyverse inclui tibble)
# Embora 'tidyverse' carregue 'tibble', é comum carregar 'knitr' separadamente.
# Como você usou 'library(ggplot2)' no rascunho, vou incluir o carregamento do 'tidyverse' (pacote que inclui ggplot2).
library(tidyverse)
library(knitr)

# Configuração de reprodutibilidade
set.seed(123)

# 2. Definição do Data Frame (tibble) com os conceitos
# Este bloco de código define a tabela 'conceitos'.
conceitos <- tibble::tribble(
  ~Objeto,      ~Exemplo_SP2024,
  "População",  "Todos os eleitores aptos de São Paulo",
  "Amostra",    "2.000 eleitores entrevistados pelo survey sintético",
  "Parâmetro",  "Proporção verdadeira de votos em cada candidato no 2º turno",
  "Estatística","Proporção observada de intenção de voto na amostra"
)

# 3. Exibição da Tabela Formatada (knitr::kable)
# Este bloco exibe o resultado formatado no console ou em um documento R Markdown.
knitr::kable(
  conceitos,
  caption = "Objetos Centrais da Inferência Estatística: O Caso Eleitoral em SP 2024",
  col.names = c("Objeto Estatístico", "Exemplo (Eleição SP 2024)"), 
  align = 'll',
  format = "markdown" # Usa o formato markdown para melhor visualização no console/web
)

# 1. Carregar a biblioteca 'tidyverse' (inclui ggplot2 e tibble)
library(tidyverse) 

# 2. Criar o Data Frame (tibble) para População e Amostra
pop_amostra <- tibble(
  grupo = c("População de Eleitores", "Amostra do Survey"), # Padronizando nomes
  tamanho = c(100, 10) # Usando uma escala para ilustrar a diferença (100 para 10)
)
#Podemos representar graficamente a ideia de população e amostra de forma esquemática.
# 1. Carregar a biblioteca 'tidyverse' (inclui ggplot2 e tibble)
library(tidyverse) 

# 2. Criar o Data Frame (tibble) para População e Amostra
pop_amostra <- tibble::tibble(
  # Definindo a ordem das categorias: Amostra primeiro, População depois
  grupo = factor(c("Amostra do survey", "População de eleitores"), 
                 levels = c("Amostra do survey", "População de eleitores")),
  tamanho = c(10, 100) # Atenção: a ordem dos tamanhos deve seguir a ordem definida acima
)

# 3. Gerar e Exibir o Gráfico de Barras
# Usando o tema padrão do ggplot2 e cores simples para replicar a imagem
ggplot(pop_amostra, aes(x = grupo, y = tamanho)) + 
  geom_col() + # Usa a largura padrão e cor cinza escura padrão
  scale_y_continuous(breaks = seq(0, 100, 25)) + # Ajusta os 'breaks' (0, 25, 50, 75, 100)
  labs(
    x = NULL, # Remove o título do eixo X
    y = "Tamanho (escala ilustrativa)", # Mantém o título do eixo Y exato da imagem
    title = "População de eleitores vs. amostra do survey" # Mantém o título exato da imagem
  ) +
  theme_minimal() + # O gráfico original usa um tema parecido com o 'theme_minimal' mas sem legenda
  theme(
    legend.position = "none", # Remove a legenda de cores
    panel.grid.major.x = element_blank() # Remove as linhas verticais (se presentes no tema)
  )
#Seção 2 – Construindo o survey sintético (n = 2.000) (≈ 12 minutos)
#Vamos simular uma pesquisa de intenção de voto com 2.000 eleitores de São Paulo.Nos bastidores, vamos supor que o apoio verdadeiro aos candidatos seja aproximadamente:
#Boulos: 48%
#Ricardo Nunes: 44%
#Branco/Nulo/Indeciso: 8%
n_main <- 2000

dados_sp <- tibble(
  id = 1:n_main,
  intencao_voto = sample(
    c("Boulos", "Ricardo Nunes", "Branco/Nulo/Indeciso"),
    size = n_main,
    replace = TRUE,
    prob = c(0.48, 0.44, 0.08)
  ),
  idade = round(rnorm(n_main, mean = 45, sd = 16)),
  sexo = sample(c("F", "M"), size = n_main, replace = TRUE, prob = c(0.52, 0.48)),
  escolaridade = sample(
    c("Fundamental", "Médio", "Superior"),
    size = n_main,
    replace = TRUE,
    prob = c(0.25, 0.45, 0.30)
  )
) |>
  mutate(
    idade = pmin(pmax(idade, 16), 90)
  )

head(dados_sp)
# Exemplo organizado dos primeiros registros

tibble::tibble(
  id = c(1, 2, 3, 4, 5, 6),
  intencao_voto = c(
    "Boulos",
    "Ricardo Nunes",
    "Boulos",
    "Ricardo Nunes",
    "Branco/Nulo/Indeciso",
    "Boulos"
  ),
  idade = c(29, 28, 45, 43, 16, 62),
  sexo = c("F", "F", "F", "M", "F", "M"),
  escolaridade = c(
    "Superior",
    "Superior",
    "Superior",
    "Médio",
    "Superior",
    "Fundamental"
  )
)
library(dplyr)
library(knitr)
library(tibble)

tab_voto <- dados_sp %>%
  count(intencao_voto, name = "n") %>%    # contador por categoria
  arrange(desc(n)) %>%
  bind_rows(                              # adiciona linha total
    tibble(intencao_voto = "Total", n = sum(.$n))
  )

kable(
  tab_voto,
  caption = "Distribuição de intenção de voto na amostra simulada (n = 2.000)"
)
#Gráfico de barras com as proporções observadas:
dados_sp |>
  count(intencao_voto) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = intencao_voto, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Categoria de intenção de voto",
    y = "Proporção na amostra",
    title = "Intenção de voto na eleição de SP 2024 (dados simulados)"
  )
#Seção 3 – Da amostra à população: o que é inferência? (≈ 10 minutos)
prop_voto <- dados_sp |>
  count(intencao_voto) |>
  mutate(prop = n / sum(n))

knitr::kable(
  prop_voto,
  digits = 3,
  caption = "Proporções amostrais de intenção de voto"
)
ggplot(prop_voto, aes(x = intencao_voto, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Categoria de intenção de voto",
    y = "Proporção amostral",
    title = "Proporções amostrais de intenção de voto (SP 2024, dados simulados)"
  )
#Seção 4 – Intervalos de confiança para proporção: Boulos e Ricardo Nunes (≈ 20 minutos)
#4.1 IC para proporção de votos em Boulos
# Contagem de votos para Boulos
n_boulos <- sum(dados_sp$intencao_voto == "Boulos")

p_hat_boulos <- n_boulos / n_main

se_boulos <- sqrt(p_hat_boulos * (1 - p_hat_boulos) / n_main)
ME_95_boulos <- 1.96 * se_boulos

ic_manual_boulos <- c(
  inferior = p_hat_boulos - ME_95_boulos,
  superior = p_hat_boulos + ME_95_boulos
)

ic_manual_boulos
#Agora usando a função prop.test():
teste_boulos <- prop.test(n_boulos, n_main, conf.level = 0.95, correct = FALSE)
teste_boulos
#Resumo em tabela:
resumo_boulos <- tibble(
  candidato = "Boulos",
  n = n_boulos,
  p_hat = p_hat_boulos,
  ic_inf = teste_boulos$conf.int[1],
  ic_sup = teste_boulos$conf.int[2],
  largura_ic = ic_sup - ic_inf
)

knitr::kable(
  resumo_boulos,
  digits = 3,
  caption = "IC de 95% para a proporção de votos em Boulos (dados simulados)"
)
#Gráfico do IC de Boulos:
ggplot(resumo_boulos, aes(y = candidato, x = p_hat)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proporção de intenção de voto (IC 95%)",
    y = NULL,
    title = "IC de 95% para intenção de voto em Boulos"
  )
#4.2 IC para proporção de votos em Ricardo Nunes
n_nunes <- sum(dados_sp$intencao_voto == "Ricardo Nunes")
p_hat_nunes <- n_nunes / n_main

teste_nunes <- prop.test(n_nunes, n_main, conf.level = 0.95, correct = FALSE)

resumo_nunes <- tibble(
  candidato = "Ricardo Nunes",
  n = n_nunes,
  p_hat = p_hat_nunes,
  ic_inf = teste_nunes$conf.int[1],
  ic_sup = teste_nunes$conf.int[2],
  largura_ic = ic_sup - ic_inf
)

resumo_ic_candidatos <- bind_rows(resumo_boulos, resumo_nunes)

knitr::kable(
  resumo_ic_candidatos,
  digits = 3,
  caption = "ICs de 95% para proporções de intenção de voto em Boulos e Ricardo Nunes"
)

ggplot(resumo_ic_candidatos, aes(y = candidato, x = p_hat)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proporção de intenção de voto (IC 95%)",
    y = NULL,
    title = "Intervalos de confiança para intenção de voto (SP 2024, dados simulados)"
  )
#Seção 5 – O que muda quando aumentamos o tamanho da amostra? (≈ 10 minutos)
simula_ic <- function(n, p_true = 0.48) {
  votos_boulos <- rbinom(1, size = n, prob = p_true)
  teste <- prop.test(votos_boulos, n, conf.level = 0.95, correct = FALSE)
  
  tibble(
    n = n,
    votos_boulos = votos_boulos,
    p_hat = teste$estimate[[1]],
    ic_inf = teste$conf.int[1],
    ic_sup = teste$conf.int[2],
    largura_ic = ic_sup - ic_inf
  )
}

ic_amostras <- purrr::map_dfr(c(500, 2000, 3000), simula_ic)

knitr::kable(
  ic_amostras,
  digits = 3,
  caption = "ICs de 95% para proporção de votos em Boulos em diferentes tamanhos de amostra"
)
ggplot(ic_amostras, aes(x = factor(n), y = largura_ic, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Tamanho da amostra (n)",
    y = "Largura do IC",
    title = "Efeito do tamanho da amostra na largura do IC (proporção de votos em Boulos)"
  )
ggplot(ic_amostras, aes(y = factor(n), x = p_hat)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proporção de intenção de voto (IC 95%)",
    y = "Tamanho da amostra",
    title = "ICs de 95% para Boulos em diferentes tamanhos de amostra"
  )
#Seção 6 – IC para proporção e IC para média em outros contextos (≈ 20 minutos)
#6.1 Proporção: disposição a pagar mais por gasolina para proteger o meio ambiente

library(dplyr)
library(knitr)
library(tibble)

# Criar a base (se já tiver, pode pular)
n_gasolina <- 1000
dados_gasolina <- tibble(
  id = 1:n_gasolina,
  pagar_mais = sample(
    c("Sim", "Não"),
    size = n_gasolina,
    replace = TRUE,
    prob = c(0.4, 0.6)
  )
)

# Contagem e total manualmente
tab_gasolina <- dados_gasolina %>%
  count(pagar_mais, name = "n") %>%
  arrange(desc(n)) %>%
  bind_rows(tibble(pagar_mais = "Total", n = sum(.$n)))

# Exibir formatado
knitr::kable(
  tab_gasolina,
  caption = "Distribuição de respostas: pagar mais pela gasolina para proteger o meio ambiente"
)
dados_gasolina |>
  count(pagar_mais) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = pagar_mais, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Resposta",
    y = "Proporção na amostra",
    title = "Disposição a pagar mais pela gasolina (dados simulados)"
  )
#Intervalo de confiança para a proporção de pessoas que responderam “Sim”:
n_sim <- sum(dados_gasolina$pagar_mais == "Sim")
teste_gasolina <- prop.test(n_sim, n_gasolina, conf.level = 0.95, correct = FALSE)

resumo_gasolina <- tibble(
  resposta = "Sim",
  n_sim = n_sim,
  n_total = n_gasolina,
  p_hat = teste_gasolina$estimate[[1]],
  ic_inf = teste_gasolina$conf.int[1],
  ic_sup = teste_gasolina$conf.int[2]
)

knitr::kable(
  resumo_gasolina,
  digits = 3,
  caption = "IC de 95% para proporção de pessoas que pagariam mais pela gasolina"
)
ggplot(resumo_gasolina, aes(y = resposta, x = p_hat)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proporção (IC 95%)",
    y = NULL,
    title = "IC de 95% para disposição a pagar mais pela gasolina"
  )
#6.2 Média: número de horas de TV por dia
#“Em média, quantas horas de TV você assiste por dia?”
n_tv <- 1000

dados_tv <- tibble(
  id = 1:n_tv,
  horas_tv = rnorm(n_tv, mean = 3, sd = 1.2)
) |>
  mutate(
    horas_tv = pmax(horas_tv, 0)  # não permite valores negativos
  )

summary(dados_tv$horas_tv)
descr_tv <- tibble(
  n = n_tv,
  media = mean(dados_tv$horas_tv),
  desvio_padrao = sd(dados_tv$horas_tv),
  minimo = min(dados_tv$horas_tv),
  maximo = max(dados_tv$horas_tv)
)

knitr::kable(descr_tv, digits = 2, caption = "Estatísticas descritivas: horas de TV por dia")
ggplot(dados_tv, aes(x = horas_tv)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Horas de TV por dia",
    y = "Frequência",
    title = "Distribuição de horas de TV por dia (dados simulados)"
  )
#Intervalo de confiança para a média de horas de TV:
teste_tv <- t.test(dados_tv$horas_tv, conf.level = 0.95)

ic_tv <- tibble(
  estimativa = teste_tv$estimate[[1]],
  ic_inf = teste_tv$conf.int[1],
  ic_sup = teste_tv$conf.int[2]
)

knitr::kable(ic_tv, digits = 2, caption = "IC de 95% para a média de horas de TV por dia")
ggplot(ic_tv, aes(y = "Média horas TV", x = estimativa)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.1) +
  labs(
    x = "Horas de TV por dia (IC 95%)",
    y = NULL,
    title = "IC de 95% para a média de horas de TV por dia"
  )
#Seção 7 – Bootstrap e interpretação de IC (≈ 10 minutos)
B <- 1000  # número de reamostragens

mediana_amostral <- median(dados_tv$horas_tv)

medianas_boot <- replicate(
  B,
  median(sample(dados_tv$horas_tv, size = n_tv, replace = TRUE))
)

ic_boot <- quantile(medianas_boot, probs = c(0.025, 0.975))

ic_boot
resumo_boot <- tibble(
  estatistica = "Mediana horas TV",
  mediana_amostral = mediana_amostral,
  ic_inf_boot = ic_boot[1],
  ic_sup_boot = ic_boot[2]
)

knitr::kable(resumo_boot, digits = 2, caption = "IC bootstrap (percentis) para a mediana de horas de TV")
resumo_boot <- tibble(
  estatistica = "Mediana horas TV",
  mediana_amostral = mediana_amostral,
  ic_inf_boot = ic_boot[1],
  ic_sup_boot = ic_boot[2]
)

knitr::kable(resumo_boot, digits = 2, caption = "IC bootstrap (percentis) para a mediana de horas de TV")
tibble(mediana_boot = medianas_boot) |>
  ggplot(aes(x = mediana_boot)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = mediana_amostral, linetype = "dashed") +
  geom_vline(xintercept = ic_boot[1], linetype = "dotted") +
  geom_vline(xintercept = ic_boot[2], linetype = "dotted") +
  labs(
    x = "Mediana (bootstrap)",
    y = "Frequência",
    title = "Distribuição bootstrap das medianas de horas de TV"
  )
#Seção 8 – Teste de hipóteses para proporção: Boulos está na frente? (≈ 15 minutos)
#8.1 Estrutura do teste
alpha <- 0.05

teste_boulos_h0 <- prop.test(
  x = n_boulos,
  n = n_main,
  p = 0.5,
  alternative = "greater",
  conf.level = 0.95,
  correct = FALSE
)

teste_boulos_h0
#Resumo dos elementos centrais do teste:
resumo_teste_boulos <- tibble(
  candidato = "Boulos",
  n_boulos = n_boulos,
  n_total = n_main,
  p_hat = teste_boulos_h0$estimate[[1]],
  p_hipotese = 0.5,
  p_valor = teste_boulos_h0$p.value,
  rejeita_H0 = teste_boulos_h0$p.value < alpha
)

knitr::kable(
  resumo_teste_boulos,
  digits = 4,
  caption = "Teste de hipótese H₀: p_Boulos = 0,5 (alternativa: p_Boulos > 0,5)"
)
ic_teste_boulos <- tibble(
  candidato = "Boulos",
  p_hat = teste_boulos_h0$estimate[[1]],
  ic_inf = teste_boulos_h0$conf.int[1],
  ic_sup = teste_boulos_h0$conf.int[2]
)

ggplot(ic_teste_boulos, aes(y = candidato, x = p_hat)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.1) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proporção (IC 95%)",
    y = NULL,
    title = "IC de 95% para Boulos e valor hipotético de 50% (H₀)"
  )
#Seção 9 – Erros tipo I e tipo II (com simulação) (≈ 10 minutos)
#9.1 Simulação de erro tipo I (H₀ verdadeira)
B_sim <- 500
alpha <- 0.05
n_simul <- 2000
p_true_H0 <- 0.5

rejeicoes_H0 <- replicate(B_sim, {
  votos_boulos <- rbinom(1, size = n_simul, prob = p_true_H0)
  teste <- prop.test(
    x = votos_boulos,
    n = n_simul,
    p = 0.5,
    alternative = "greater",
    conf.level = 0.95,
    correct = FALSE
  )
  teste$p.value < alpha
})

prop_erro_tipo1 <- mean(rejeicoes_H0)

prop_erro_tipo1

resultado_tipo1 <- tibble(
  cenario = "H0 verdadeira (p = 0,5)",
  n = n_simul,
  B_sim = B_sim,
  proporcao_rejeicoes = prop_erro_tipo1
)


resultado_tipo1 <- tibble(
  cenario = "H0 verdadeira (p = 0,5)",
  n = n_simul,
  B_sim = B_sim,
  proporcao_rejeicoes = prop_erro_tipo1
)

knitr::kable(
  resultado_tipo1,
  digits = 3,
  caption = "Estimativa empírica da taxa de erro tipo I (H₀ verdadeira)"
)
#9.2 Simulação de poder (H₀ falsa, p_Boulos > 0,5)
p_true_H1 <- 0.53

rejeicoes_H1 <- replicate(B_sim, {
  votos_boulos <- rbinom(1, size = n_simul, prob = p_true_H1)
  teste <- prop.test(
    x = votos_boulos,
    n = n_simul,
    p = 0.5,
    alternative = "greater",
    conf.level = 0.95,
    correct = FALSE
  )
  teste$p.value < alpha
})

prop_poder <- mean(rejeicoes_H1)

resultado_poder <- tibble(
  cenario = "H0 falsa (p = 0,53)",
  n = n_simul,
  B_sim = B_sim,
  proporcao_rejeicoes = prop_poder
)

knitr::kable(
  bind_rows(resultado_tipo1, resultado_poder),
  digits = 3,
  caption = "Erro tipo I (H₀ verdadeira) e poder (H₀ falsa)"
)

bind_rows(resultado_tipo1, resultado_poder) |>
  mutate(cenario = factor(cenario)) |>
  ggplot(aes(x = cenario, y = proporcao_rejeicoes)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Cenário",
    y = "Proporção de rejeições",
    title = "Erro tipo I (H₀ verdadeira) e poder (H₀ falsa)"
  )
#Seção 10 – Síntese e conexão com a prática em R e na pesquisa (≈ 5 minutos)
sintese <- tibble::tribble(
  ~Conceito,                 ~Exemplo_da_aula,
  "IC para proporção",       "Votos em Boulos e Ricardo Nunes; gasolina vs. meio ambiente",
  "IC para média",           "Horas de TV por dia",
  "Bootstrap",               "Mediana de horas de TV",
  "Teste de hipóteses",      "H₀: p_Boulos = 0,5",
  "Erro tipo I",             "Concluir vantagem de Boulos quando ela não existe",
  "Erro tipo II / poder",    "Não detectar vantagem de Boulos quando ela existe",
  "Efeito do tamanho da amostra", "Comparação n = 500, 2.000, 3.000 para IC de Boulos"
)

knitr::kable(sintese, caption = "Síntese: conceitos e exemplos trabalhados na aula")
fluxo <- tibble(
  etapa = factor(
    c("Amostra do survey", "Estatística (p̂, média)", "IC e teste de H₀", "Interpretação substantiva"),
    levels = c("Amostra do survey", "Estatística (p̂, média)", "IC e teste de H₀", "Interpretação substantiva")
  ),
  pos = 1:4
)

ggplot(fluxo, aes(x = pos, y = 1, label = etapa)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(vjust = -1) +
  scale_x_continuous(breaks = 1:4) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Fluxo da inferência estatística na aula")
