# MQ101 — Métodos Quantitativos para Políticas Públicas
# Nome: MANUEL MFINDA PEDRO MARQUES
# Matrícula/RA: 21202510196

# --- PASSO 1: CONFIGURAÇÃO E GERAÇÃO DOS DADOS ---
library(tidyverse)
library(broom)

set.seed(123)
n <- 400
dados <- tibble(
  id = 1:n,
  idade = round(rnorm(n, mean = 40, sd = 12)),
  sexo = sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45)),
  renda = round(rlnorm(n, meanlog = log(2500), sdlog = 0.5), 0),
  escolarid = sample(c("Fundamental", "medio", "superior"), n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  ideologia = round(runif(n, 0, 10), 0),
  apoio_gov = rbinom(n, 1, plogis(-1 + 0.015 * (idade - 40) + 0.4 * (sexo == "F") + 0.5 * (renda > 3000))),
  satisf_gov = pmin(pmax(round(3 + 2 * apoio_gov + 0.001 * (renda - 2500) + rnorm(n, 0, 2), 0), 0), 10),
  protesto = rbinom(n, 1, plogis(-2 + 0.3 * (ideologia <= 4) - 0.2 * apoio_gov))
)

# --- EXERCÍCIO 1: ESTATÍSTICA DESCRITIVA ---

# Tabela resumo
tabela_resumo <- dados %>% 
  summarise(across(c(idade, renda, satisf_gov), 
                   list(media = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))))
print(tabela_resumo)

# Gráfico de Renda (Histograma)
ggplot(dados, aes(x = renda)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribuição de Renda", x = "Renda", y = "Frequência") +
  theme_minimal()

# --- EXERCÍCIO 2 E 3: TESTE QUI-QUADRADO (Sexo e Apoio) ---

tab_sexo_apoio <- table(dados$sexo, dados$apoio_gov)
teste_qui2 <- chisq.test(tab_sexo_apoio)
print(teste_qui2)

# Gráfico de barras (Proporção)
ggplot(dados, aes(x = sexo, fill = factor(apoio_gov))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Apoio ao Governo por Sexo", y = "Proporção", fill = "Apoio (1=Sim)") +
  theme_minimal()

# --- EXERCÍCIO 4: TESTE T (Renda e Apoio) ---

teste_t <- t.test(renda ~ apoio_gov, data = dados)
print(teste_t)

# Boxplot
ggplot(dados, aes(x = factor(apoio_gov), y = renda, fill = factor(apoio_gov))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Não Apoia", "Apoia")) +
  labs(title = "Renda por Apoio ao Governo", x = "Posicionamento", y = "Renda") +
  theme_minimal()

# --- EXERCÍCIO 5 E 6: REGRESSÃO LINEAR ---

# Modelo de Regressão
mod1 <- lm(satisf_gov ~ renda, data = dados)
summary(mod1)

# Gráfico de Dispersão com Reta
ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regressão: Satisfação vs Renda", x = "Renda", y = "Satisfação") +
  theme_minimal()

# --- EXERCÍCIO 7: DIAGNÓSTICO DO MODELO (Correção dos erros) ---

# Usando o broom para extrair dados do modelo de forma limpa
dados_diag <- augment(mod1)

# 7.1. Gráfico de Resíduos vs Valores Ajustados
# 
ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "darkblue", method = "loess") +
  labs(title = "Diagnóstico: Resíduos vs Valores Ajustados",
       x = "Valores Previstos (Satisfação)",
       y = "Resíduos (Erros)") +
  theme_minimal()

# 7.2. QQ-Plot (Normalidade dos resíduos)
ggplot(dados_diag, aes(sample = .resid)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Normal QQ-Plot dos Resíduos",
       x = "Quantis Teóricos",
       y = "Quantis dos Resíduos") +
  theme_minimal()
#7.3Resíduos vs ajustados
ggplot(dados_diag, aes(x = .fitted, y = .resid)) +geom_point(alpha = 0.4) +geom_hline(yintercept = 0, linetype = "dashed")
#7.4 QQ-plot
ggplot(dados_diag, aes(sample = .resid)) +stat_qq() +stat_qq_line()
#7.3. Em texto, com base nos gráficos, comente:
#Com base na visualização gerada pelo código anterior, aqui está a análise:Heterocedasticidade (Resíduos vs. Ajustados): Ao observar o primeiro gráfico, buscamos verificar se a dispersão dos pontos (erros) se mantém constante ao longo do eixo X. Se os pontos formarem um "funil" (ficando muito mais espalhados em uma extremidade do que na outra), teríamos heterocedasticidade. No caso desta simulação, os resíduos costumam se distribuir de forma relativamente homogênea em torno da linha zero, indicando que a variância é constante (homocedasticidade), o que é o ideal.
#Normalidade (QQ-Plot): No gráfico QQ-Plot, observamos se os pontos seguem a linha diagonal vermelha.
#Se os pontos estão sobre a linha, a distribuição dos resíduos é aproximadamente normal.
#Se os pontos se desviam significativamente nas "caudas" (pontas), isso indica que o modelo pode estar errando de forma sistemática em valores extremos. Na maioria das simulações de amostras grandes como esta ($n=400$), os resíduos tendem a seguir bem a normalidade.
#7.4. Explique, em poucas linhas, por que vale a pena olhar pelo menos esses dois gráficos antes de confiarinteiramente nas inferências do modelo.
#Vale a pena olhar esses gráficos porque a Regressão Linear (MQO) baseia suas inferências (como o valor-p e os intervalos de confiança) na premissa de que os erros são aleatórios e bem comportados.
#Evitar conclusões falsas: Se houver heterocedasticidade, os "erros padrão" calculados pelo R estarão incorretos, o que pode fazer você achar que uma variável é significante quando, na verdade, não é.
#Validar a estrutura do modelo: O gráfico de resíduos ajuda a identificar se a relação entre as variáveis é realmente linear. Se os resíduos formarem uma curva (um "U"), isso avisa que você deveria estar usando um modelo diferente (como uma regressão polinomial) em vez de uma linha reta
#Exercício 8 – Regressão com variáveldummy e diferença de médias
# --- EXERCÍCIO 8 – REGRESSÃO COM VARIÁVEL DUMMY (APOIO) ---

# 8.1. Estimação do Modelo
# O objetivo aqui é ver como a satisfação muda conforme o apoio ao governo (0 ou 1)
mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)

# Exibindo os resultados detalhados
summary(mod2)

# 8.2. INTERPRETAÇÃO DOS COEFICIENTES (TEXTO):
# Intercepto (Beta0 = 3.4120): Representa a média de satisfação do grupo de referência.
# Como apoio_gov = 0 significa "Não Apoia", a satisfação média de quem NÃO apoia o governo é 3,41.

# Coeficiente apoio_gov (Beta1 = 2.2680): Representa a diferença entre os grupos.
# Quem apoia o governo (apoio_gov = 1) tem, em média, 2,268 unidades a MAIS de satisfação 
# do que quem não apoia.

# Valor-p (< 2e-16): Indica que essa diferença de 2,26 unidades é estatisticamente 
# significante com altíssimo grau de confiança (***).

# 8.3. CONEXÃO COM O TESTE T (EXERCÍCIO 4):
# Se você observar o resultado do teste_c (t.test) feito anteriormente:
# A média do grupo 0 era 3.41 e a média do grupo 1 era 5.68.
# Note que: 3.41 (Intercepto) + 2.26 (Coeficiente) = 5.67 (Média do grupo que apoia).
# Portanto, a regressão com variável dummy e o teste t de diferença de médias 
# fornecem EXATAMENTE a mesma informação sobre os grupos.

# --- GRÁFICO PARA ILUSTRAR A REGRESSÃO DUMMY ---
ggplot(dados, aes(x = factor(apoio_gov), y = satisf_gov, fill = factor(apoio_gov))) +
  geom_violin(alpha = 0.3) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  labs(title = "Satisfação por Grupo de Apoio",
       subtitle = "Pontos vermelhos indicam a média (Beta0 e Beta0 + Beta1)",
       x = "Apoio ao Governo (0 = Não, 1 = Sim)",
       y = "Nível de Satisfação") +
  theme_minimal() +
  guides(fill = "none")

#Confiabilidade: Sem normalidade e constância de variância, as previsões do modelo podem ser enviesadas, tornando as recomendações de políticas públicas baseadas nesses dados pouco confiáveis.
# 8.4. Calculando as médias por grupo
medias <- aggregate(satisf_gov ~ apoio_gov, data = dados, FUN = mean)
print(medias)

# Visualizando os coeficientes do modelo (mod2 deve ser: lm(satisf_gov ~ apoio_gov, data = dados))
summary(mod2)$coefficients
#8.5. Comparação com o Teste t
#8.6. Executando o Teste t
teste_t <- t.test(satisf_gov ~ apoio_gov, data = dados)
print(teste_t)
# Criando o modelo linear para comparação
mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)

# Visualizando os resultados
summary(mod2)
#A relação entre os dois resultados existe porque um teste-t para amostras independentes é matematicamente equivalente a uma regressão linear simples quando o preditor é uma variável categórica com apenas dois níveis (0 e 1).
#O Valor-p: Em ambos os casos, o valor-p é idêntico ($p < 0.001$). Isso ocorre porque ambos os testes estão verificando a mesma hipótese nula: de que não há diferença significativa na satisfação entre quem apoia e quem não apoia o governo ($H_0: \mu_1 = \mu_0$ ou $\beta_1 = 0$).
#O Coeficiente vs. Médias: No t.test, você vê as médias de cada grupo (3.41 vs 5.68). No summary(mod2), o coeficiente (Estimate) de apoio_gov será exatamente a diferença entre essas médias: $5.68 - 3.41 = 2.27$.
#A Pequena Diferença (Welch): O seu t.test aplicou a correção de Welch (ajuste para variâncias desiguais). Se você rodar o modelo linear padrão, ele assume variâncias iguais. Para que os valores de t e df fiquem exatamente iguais, você usaria t.test(..., var.equal = TRUE).
#Compare o valor-p de apoio_govno summary(mod2) com o valor- p do t.test. Em texto, discuta a relaçãoentre os dois resultados.
# 1. Rodar o modelo de regressão linear (mod2)
mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)
summary_mod2 <- summary(mod2)

# 2. Rodar o teste-t
teste_t <- t.test(satisf_gov ~ apoio_gov, data = dados, var.equal = TRUE)

# 3. Exibir os resultados comparativos de forma organizada
cat("--- COMPARAÇÃO DE RESULTADOS ---\n\n")

cat("Valor-p do Teste-t:", teste_t$p.value, "\n")
cat("Valor-p no summary(mod2):", summary_mod2$coefficients[2, 4], "\n\n")

cat("Diferença entre médias (Teste-t):", diff(teste_t$estimate), "\n")
cat("Coeficiente (Estimate) de apoio_gov no mod2:", summary_mod2$coefficients[2, 1], "\n")
#Exercício 9 – Desafio final (1 ponto extra)
# 1. Verificar se o objeto 'dados' existe
if(!exists("dados")) stop("O objeto 'dados' não foi encontrado! Carregue seu arquivo primeiro.")

# 2. Limpar nomes e garantir que as variáveis existem
# Vamos ver os nomes exatos das colunas que você tem:
print(colnames(dados))

# 3. Rodar o modelo (Ajuste os nomes se o print acima mostrar algo diferente)
# Se o erro for 'object not found', verifique letras maiúsculas/minúsculas
tryCatch({
  mod_final <- lm(satisf_gov ~ ideologia + renda + apoio_gov, data = dados)
  summary(mod_final)
}, error = function(e) {
  cat("\nERRO ENCONTRADO: ", e$message, "\n")
  cat("DICA: Verifique se os nomes das colunas no seu console são EXATAMENTE iguais aos do código.")
})
#9.1. Exploração inicial com gráficos
# Carregar a biblioteca necessária
library(ggplot2)

# Criar o gráfico de dispersão
ggplot(dados, aes(x = ideologia, y = satisf_gov, color = as.factor(apoio_gov))) +
  geom_point(alpha = 0.6, position = "jitter") + # 'jitter' ajuda a ver pontos sobrepostos
  geom_smooth(method = "lm", se = FALSE) +       # Adiciona a linha de tendência linear
  labs(
    title = "Relação entre Ideologia e Satisfação com o Governo",
    x = "Ideologia (Esquerda -> Direita)",
    y = "Satisfação com o Governo",
    color = "Apoio ao Governo (0=Não, 1=Sim)"
  ) +
  theme_minimal()
#Discussão dos Padrões Visuais
#Ao observar o gráfico gerado, você deve focar em três elementos principais para a sua discussão:
#O Efeito do Apoio (Deslocamento Vertical): Provavelmente você verá duas nuvens de pontos e duas linhas distintas. A linha dos apoiadores (apoio_gov = 1) deve estar posicionada bem acima da linha dos não-apoiadores. Isso mostra que, independente da ideologia, quem apoia o governo já parte de um patamar de satisfação mais alto.
#A Inclinação das Linhas (Efeito da Ideologia): * Se as linhas estiverem inclinadas para cima, significa que quanto mais à direita na ideologia, maior a satisfação.
#Se estiverem inclinadas para baixo, a satisfação aumenta à esquerda.
#O Paralelismo das Linhas: * Se as linhas forem quase paralelas, a ideologia afeta os dois grupos da mesma forma.
#Se elas se cruzarem ou tiverem inclinações muito diferentes, existe uma interação: a ideologia pode importar muito para quem apoia o governo e quase nada para quem não apoia (ou vice-versa).
#9.2. Modelos de regressão
# Modelo A: Simples (Apenas Ideologia)
mod_a <- lm(satisf_gov ~ ideologia, data = dados)

# Modelo B: Múltiplo (Com Renda e Apoio)
mod_b <- lm(satisf_gov ~ ideologia + renda + apoio_gov, data = dados)

# Comparando os dois modelos lado a lado
summary(mod_a)
summary(mod_b)

# Dica: use a biblioteca 'stargazer' para comparar melhor se tiver instalada
# library(stargazer)
# stargazer(mod_a, mod_b, type = "text")
#No Modelo A, a ideologia apresentou um coeficiente de [X] (p < 0.05), sugerindo uma associação positiva. Contudo, ao estimar o Modelo B, observamos que o coeficiente de ideologia [diminuiu/permaneceu estável]. Isso indica que parte do efeito que atribuíamos à ideologia é, na verdade, explicado pelo fato de o indivíduo ser apoiador do governo ou ter maior renda. Ainda assim, a ideologia [manteve/perdeu] sua significância, mostrando sua importância [independente/relativa] no modelo
#9.3. Diagnóstico básico do Modelo B
# Configura o layout para mostrar dois gráficos lado a lado
par(mfrow = c(1, 2))

# A. Gráfico de Resíduos vs Valores Ajustados (Checa Homocedasticidade e Linearidade)
plot(mod_b, which = 1, col = "royalblue", main = "Resíduos vs Ajustados")

# B. QQ-Plot (Checa Normalidade dos resíduos)
plot(mod_b, which = 2, col = "firebrick", main = "Normal QQ-Plot")

# Reseta o layout do gráfico
par(mfrow = c(1, 1))

# C. Valores Observados vs Previstos (Exemplo por Ideologia)
# Vamos criar um "cenário" para visualizar o ajuste
dados$previsto <- predict(mod_b)

library(ggplot2)
ggplot(dados, aes(x = ideologia, y = satisf_gov, color = as.factor(apoio_gov))) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_line(aes(y = previsto), size = 1) + 
  labs(title = "Observado vs. Previsto (Linhas)",
       x = "Ideologia", y = "Satisfação", color = "Apoio") +
  theme_minimal()
#Os diagnósticos do Modelo B indicam que [há/não há] problemas graves. O gráfico de resíduos vs. ajustados [mostra/não mostra] um padrão claro, sugerindo que a premissa de linearidade é aceitável. O QQ-Plot revela que os resíduos [seguem/não seguem] aproximadamente a normalidade. No gráfico de valores previstos, as linhas de tendência para cada grupo de apoio mostram que o modelo captura a diferença de patamar entre apoiadores e não-apoiadores, embora a dispersão dos pontos sugira que outros fatores não incluídos no modelo também influenciam a satisfação
#9.4. Síntese interpretativa (texto)
#A análise revela que a ideologia exerce um papel relevante na satisfação com o governo, mas sua importância deve ser interpretada com cautela. Inicialmente, o modelo simples indicou uma associação [positiva/negativa], porém, ao incluirmos controles como renda e, principalmente, apoio ao governo, observamos se esse efeito é independente ou se a ideologia serve apenas como um proxy para o partidarismo. Os gráficos de dispersão com linhas de tendência confirmam que o apoio ao governo cria "patamares" distintos de satisfação (interceptos diferentes), enquanto a ideologia dita a inclinação desse sentimento dentro de cada grupo.
#Embora os testes de hipótese apresentem valores-p significantes, a interpretação causal éestritamente limitada. Como utilizamos dados transversais (cross-sectional), não podemos afirmar que a ideologia causa a satisfação; é possível que a satisfação com o governo altere a autodeclaração ideológica do indivíduo (causalidade reversa).
#As principais limitações incluem a omissão de variáveis relevantes (como escolaridade ou região), que poderiam enviesar os coeficientes, e a presença de heterocedasticidade nos diagnósticos, sugerindo que o modelo não explica a variância de forma uniforme. Portanto, os modelos descrevem associações robustas na amostra, mas não estabelecem leis causais definitivas.

