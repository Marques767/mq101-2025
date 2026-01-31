# SCRIPT: aula03_pratica.R
# TEMA: Lista #03: Gráficos e Visualização de Dados (ggplot2)
#matricula:21202510196
# AUTOR: Manuel Mfinda Pedro Marques
# DATA: Novembro/2025
## 0️⃣ Limpar ambiente e Pacotes
rm(list = ls())

# Instale apenas se necessrio:
install.packages(c("tidyverse","readr","ggplot2","scales","viridis","
electionsBR"))
set.seed(101)

library(tidyverse)

# 1 (0)Aquecimento– Tabela vs. Gráfico (dados internos)

# Objetivo. Comparar leitura em tabela e gráfico.

# Passos:
# 1- Carregar a base de dados interna do R: cars;

# 2- Exibir as 10 primeiras linhas (tabela);

# 3- Produzir um gráfico de dispersão (speed x dist);

# 4 - Interpretar a diferença entre os formatos.


#Bloco R (carregar dados): 
data(cars) # base interna do R
head(cars, 10) # formate como tabela no seu .Rmd se desejar

# Visualização do gráfico
plot(cars$speed, cars$dist, 
     main = "Relação entre Velocidade e Distância de Parada",
     xlab = "Velocidade (mph)", 
     ylab = "Distância de Parada (ft)", 
     pch = 19, col = "darkblue")

# 2 (1) Distribuições univariadas e grupos
#  Objetivo. Visualizar forma da distribuição e diferenças entre grupos.

Base. mtcars.
# Bloco R (carregar/preparar dados): 
data(mtcars)
mtcars <- mtcars |>
  mutate(cyl = as.factor(cyl))

#  3 (2) Série temporal simples
# Objetivo. Identificar tendência no tempo.
#  Escolha uma base.

# Opção A: AirPassengers (mensal)
# Bloco R (preparar dados):

ap < - tibble(
  date = as.Date(time(AirPassengers)),
  n    = as.numeric(AirPassengers),
  year = format(date, "%Y"),
  month = format(date, "%m")
)

# Opção B: airquality (diário, facet por mês)
Bloco R (preparar dados):
  aq < - airquality |>
  as_tibble() |>
  drop_na (Ozone) |>
  mutate(Month = factor(Month),
         Day = as.integer(Day))


# 4 (3) Relações bivariadas e transformações
#  Objetivo. Relacionar mpg e wt; testar tendências (lm/loess) e escala log quando fizer sentido.
# Base. mtcars.
#  Bloco adicional: não há; use o objeto do Item (1).

# 5 (4) Facetas (comparar subgrupos)
# Objetivo. Comparar padrões entre transmissões.
# Base. mtcars.

# Bloco R (preparar fator):
mtcars <- mtcars |>
  mutate(am = factor(am, labels = c("Automtico","Manual")))

# 6 (5) Simulação I– Correlação controlada
# Objetivo. Visualizar como ρ altera o “aperto” da nuvem.
# Passos. Simule três níveis de correlação (ρ = 0.2,0.6,0.9); faça três dispersões; compare.

# Bloco R (simular dados):
n <- 1000; rhos <- c(0.2, 0.6, 0.9)
sim <- purrr::map_dfr(rhos, \(rho) {
  x <- rnorm(n); e <- rnorm(n)
  y <- rho*x + sqrt(1- rho^2)*e
  tibble(rho = rho, x = x, y = y)
})

# 7 (6) Simulação II– Diferenças entre grupos
# Objetivo. Comparar distribuições com médias e SD diferentes.
#  Passos. Simule grupo A: N(0,1); grupo B: N(1,1.8); faça histogramas/densidades/boxplot/violin; interprete.

# Bloco R (simular dados):
n <- 1000 
df_grupos <- tibble(
  grupo = rep(c("A","B"), each = n),
  valor = c(rnorm(n, 0, 1), rnorm(n, 1, 1.8))
)

#  8 (7) educ_saude.csv– exploração e gráficos
# Objetivo. Relacionar uma variável de educação e uma de saúde.
# Passos. Importe; explore; escolha variáveis; faça histogramas/densidades/boxplots/dispersões/facetas; interprete.

# Bloco R (carregar dados):
library(readr)
educ <- read_csv("educ_saude.csv")
glimpse(educ)
# Ajuste nomes de variveis conforme seu arquivo

# 9 (8) educ_saude.csv– figura final

# Objetivo. Contar uma história em um gráfico (título, eixos, legenda, caption, escala adequada).
# Bloco adicional: não há.

# 10 (9) Desafio (pontos extras)– Eleições 2024 (Município de São Paulo) com TSE + electionsBR

# Meta. Baixar dados oficiais das eleições 2024 (município de São Paulo), calcular taxas
# por zona eleitoral: abstenção, brancos, nulos; construir proxy de renda por escolaridade
# do eleitorado e investigar relação com essas taxas.
# Você fará as visualizações. Abaixo: apenas pipeline de baixar → filtrar → agregar.

# Bloco R (preparar ambiente):
library(electionsBR)
library(scales)

# Bloco R (baixar resultados por município e zona– 2024, SP):
res_sp <- electionsBR::vote_mun_zone(2024, uf = "SP")
# Verifique nomes de colunas:
# names(res_sp); glimpse(res_sp)

# BlocoR(filtrarmunicípiodeSãoPaulo):
res_sp_mun<-res_sp|>
  mutate(NM_MUNICIPIO=toupper(NM_MUNICIPIO))|>
  filter(NM_MUNICIPIO=="SO PAULO")

# BlocoR(agregar: taxasporzonaeleitoral):
denom<-if("QT_TOTAL_VOTOS" %in%names(res_sp_mun))"QT_TOTAL_VOTOS"else"
 QT_COMPARECIMENTO"

tx_zona <-res_sp_mun|>
  group_by(NR_ZONA)|>
  summarise(
    aptos=sum(QT_APTOS,na.rm=TRUE),
    comp =sum(QT_COMPARECIMENTO,na.rm=TRUE),
    abst =sum(QT_ABSTENCOES,na.rm=TRUE),
    br =sum(QT_VOTOS_BRANCOS,na.rm=TRUE),
    nu =sum(QT_VOTOS_NULOS,na.rm=TRUE),
    denom_votos=sum(.data[[denom]],na.rm=TRUE),
    .groups="drop"
  )|>
  mutate(
    tx_abst =abst/aptos,
    tx_branco=br /denom_votos,
    tx_nulo =nu /denom_votos
  )

# BlocoR(perfildoeleitor–escolaridadeporzona):
vp<-electionsBR::voter_profile(2024) #podesergrande;filtredepois
vp_sp_mun<-vp|>
  filter(SG_UF=="SP")|>
  mutate(NM_MUNICIPIO=toupper(NM_MUNICIPIO))|>
  filter(NM_MUNICIPIO=="SO PAULO")|>
  mutate(
    DS_GRAU_ESCOLARIDADE=toupper(DS_GRAU_ESCOLARIDADE),
    sup_ou_mais=DS_GRAU_ESCOLARIDADE%in%
      c("SUPERIORCOMPLETO","SUPERIORINCOMPLETO","POS-GRADUAO")
  )|>
  group_by(NR_ZONA)|>
  summarise(
    eleitores_total=sum(QT_ELEITORES_PERFIL,na.rm=TRUE),
    eleitores_sup =sum(QT_ELEITORES_PERFIL[sup_ou_mais],na.rm=TRUE),
    share_sup =eleitores_sup/eleitores_total,
    .groups = "drop"
  )

# Bloco R (integrar bases para análise):
df_final <- tx_zona |>
  left_join(vp_sp_mun, by = "NR_ZONA")
# Agora produza 3 disperses: share_sup x tx_abst; share_sup x tx_branco;
# share_sup x tx_nulo.

# Use eixos percentuais. Interprete em 8--12 linhas. Discuta limitaes.

