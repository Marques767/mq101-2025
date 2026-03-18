#------------------------------------------------------------------------------#
#Desafio de Replicação:avaliando os dados e as evidências publicadas
#Métodos Quantitativos - Professor Ricardo Ceneviva
#Discentes: Debora Rodrigues Pereira Dias– Matrícula no 21202510146
#            Manuel Mfinda Pedro Marques – Matrícula no 21202510196
#DESIGUALDADES SOCIOESPACIAIS DE ACESSO A OPORTUNIDADES NAS CIDADES BRASILEIRAS – 2019
# Artigo: TD 2535 - IPEA (2019) | Recorte: Curitiba/PR
#------------------------------------------------------------------------------#

# Instala o "motor" de instalação
if (!require("remotes")) install.packages("remotes")

# Instala o aopdata diretamente do IPEA
remotes::install_github("ipeaGIT/aopdata", subdir = "r-package", upgrade = "never")

#Copie e cole este código (Carregar e Baixar Dados)
#Após a instalação terminar, cole este bloco para finalmente baixar os dados da pesquisa:

# Carregar o pacote
library(aopdata)
# 1. SETUP E CARREGAMENTO
library(aopdata)
library(ggplot2)
library(sf)
library(data.table)

# 2. Download dos dados
# Baixando dados de Curitiba para transporte público (2019)
dados_curitiba <- read_access(city='Curitiba', mode='public_transport', year=2019, geometry=TRUE)
setDT(dados_curitiba)
dados_sf <- st_as_sf(dados_curitiba)

# 3. REPLICAÇÃO (Verifiability - 60 min)
# Trabalho (Baseado no Mapa 5A do artigo original)
ggplot(data = dados_sf) + geom_sf(aes(fill = CMATT60), color = NA) + 
  scale_fill_viridis_c(option = "viridis") + theme_minimal() +
  labs(title = "Replicação Mapa 5A: Trabalho (60 min)", fill = "Empregos")

# Escola (Mapa 5B)
ggplot(data = dados_sf) + geom_sf(aes(fill = CMAET60), color = NA) + 
  scale_fill_viridis_c(option = "magma") + theme_minimal() +
  labs(title = "Replicação Mapa 5B: Educação (60 min)", fill = "Escolas")

# 4. TESTES DE ROBUSTEZ (Robustness - 30 min)
# Trabalho 30 min
ggplot(data = dados_sf) + geom_sf(aes(fill = CMATT30), color = NA) + 
  scale_fill_viridis_c(option = "viridis") + theme_minimal() +
  labs(title = "Robustez Trabalho: Acesso em 30 min")

# Escola 30 min 
ggplot(data = dados_sf) + geom_sf(aes(fill = CMAET30), color = NA) + 
  scale_fill_viridis_c(option = "magma") + theme_minimal() +
  labs(title = "Robustez Escola: Acesso em 30 min")

# 5. ANÁLISE ESTATÍSTICA E GENERALIZAÇÃO (Palma Ratio)
# Cálculo ponderado pela população (P001) para os decis de renda (R003)
p_rico_esc <- dados_curitiba[R003 == 10, weighted.mean(CMAET60, P001, na.rm=T)]
p_pobre_esc <- dados_curitiba[R003 <= 4, weighted.mean(CMAET60, P001, na.rm=T)]
p_rico_trab <- dados_curitiba[R003 == 10, weighted.mean(CMATT60, P001, na.rm=T)]
p_pobre_trab <- dados_curitiba[R003 <= 4, weighted.mean(CMATT60, P001, na.rm=T)]

cat("--- RESULTADOS FINAIS ---\n")
cat("Palma Ratio Educação (60 min):", p_rico_esc / p_pobre_esc, "\n")
cat("Palma Ratio Trabalho (60 min):", p_rico_trab / p_pobre_trab, "\n")

# 6. EXIBIÇÃO DOS RESULTADOS (Saída de Verificação)
# ------------------------------------------------------------------------------
cat("\n===============================================================\n")
cat("       RESULTADOS DA REPLICAÇÃO - CURITIBA (2019)            \n")
cat("===============================================================\n")

# Imprime os valores do Palma Ratio
cat("\n>>> INDICADORES DE DESIGUALDADE (PALMA RATIO):\n")
cat("Acessibilidade Educacional (60 min):", round(p_rico_esc / p_pobre_esc, 2), "\n")
cat("Acessibilidade ao Trabalho (60 min):", round(p_rico_trab / p_pobre_trab, 2), "\n")

cat("\n>>> INTERPRETAÇÃO:\n")
cat("Valores acima de 1.0 indicam vantagem para o grupo de renda alta.\n")
cat("===============================================================\n")

# 7. INFORMAÇÕES DO AMBIENTE (Reprodutibilidade T3)
# ------------------------------------------------------------------------------
# O comando abaixo é vital para o Marco T3 do Professor Ceneviva
print(sessionInfo())