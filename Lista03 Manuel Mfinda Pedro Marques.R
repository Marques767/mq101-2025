# "MQ101 — Exercício 01"
#author: "Manuel Mfinda Pedro Marques"
#Matricula:21202510196
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r preparacao, message=FALSE, warning=FALSE}
library(tidyverse)
set.seed(202501)

# Tentativa 1: Dentro da pasta data
# Tentativa 2: Na pasta raiz (caso o arquivo esteja 'solto')
if (file.exists("data/educ_saude.csv")) {
  dados <- read_csv("data/educ_saude.csv")
} else if (file.exists("educ_saude.csv")) {
  dados <- read_csv("educ_saude.csv")
} else {
  # Se não achar em lugar nenhum, ele cria um dado vazio para o Knit não travar
  dados <- data.frame(aviso = "Arquivo não encontrado")
}
```


## Exercício 1: R como calculadora — ordem das operações

**Objetivo:** Executar expressões e entender a ordem das operações (HOPR cap. 1).

```{r exercicio1}
# Executando as operações solicitadas
10 + 2
(10 + 2) * 3
((10 + 2) * 3 - 6) / 3
```
Resposta 1: Os parênteses mudam a ordem das operações, garantindo que certas partes da expressão sejam resolvidas antes das demais. Isso altera diretamente o resultado final, como vimos nos exemplos acima. Em políticas públicas e ciências sociais, esse controle é essencial porque indicadores compostos (como taxas ajustadas ou índices) dependem de cálculos bem estruturados para evitar interpretações equivocadas.

## Exercício 2: Objetos e nomeação

**Objetivo:** Compreender como o R diferencia objetos por nomes (HOPR cap. 1).

```{r exercicio2}
# O R diferencia maiúsculas de minúsculas (case-sensitive)
x <- 1:6
Name <- 1
name <- 0

# Testando a soma com objetos de nomes similares
Name + 1
name + 1
```
Resposta 2: No R, Name e name são objetos diferentes porque o software distingue maiúsculas de minúsculas. Assim, Name <- 1 cria um objeto independente de name <- 0. Quando somamos, cada um retorna resultados distintos (2 e 1), mostrando que a nomeação precisa ser consistente para evitar erros em análises. Portanto, a distinção entre maiúsculas e minúsculas é fundamental para evitar confusões na criação e manipulação de objetos.

## Exercício 3: Sorteio (sample) e reprodutibilidade

**Objetivo:** Compreender o papel da função `set.seed()` na geração de números aleatórios (HOPR cap. 1–2).

```{r exercicio3}
# Teste 1: Com set.seed(123)
set.seed(123)
die <- 1:6
sample(die, size = 2, replace = TRUE)

# Teste 2: Reexecutando com a mesma semente para verificar reprodutibilidade
set.seed(123)
sample(die, size = 2, replace = TRUE)

# Teste 3: Removendo o set.seed (o resultado deve mudar)
sample(die, size = 2, replace = TRUE)

# Teste 4: Mudando a semente para 999
set.seed(999)
sample(die, size = 2, replace = TRUE)
```
Resposta 3: A função set.seed() fixa a sequência aleatória usada pelo R, garantindo que os resultados sejam sempre os mesmos quando o código é repetido. Isso é fundamental para a reprodutibilidade: com a semente definida, qualquer pessoa que rodar o script obterá o mesmo sorteio. Sem o set.seed(), cada execução gera números diferentes, o que dificulta a comparação e validação de análises em políticas públicas ou ciências sociais. Como demonstrado nos testes, a semente 123 sempre repete o resultado 3 6, enquanto a remoção ou alteração da semente gera novos valores.

## Exercício 4: Sua primeira função

**Objetivo:** Aprender a definir funções e utilizar argumentos (HOPR cap. 1).

```{r exercicio4}
# Definição da função roll2
roll2 <- function(bones = 1:6) {
  # Sorteia dois valores do vetor 'bones' com reposição e os armazena em 'dice'
  dice <- sample(bones, size = 2, replace = TRUE)
  # Retorna a soma dos valores sorteados
  sum(dice)
}

# Testes da função roll2
roll2()      # Teste com valor padrão (dados de 6 lados)
roll2(1:20)  # Teste com dados de 20 lados

# Implementação do BÔNUS: função soma3
soma3 <- function(bones = 1:6) {
  # Sorteia três valores do vetor 'bones' com reposição
  dice <- sample(bones, size = 3, replace = TRUE)
  # Retorna a soma dos três valores
  sum(dice)
}

# Teste da função soma3
soma3()
```
Resposta 4: A função roll2 define um conjunto de valores possíveis através do argumento bones = 1:6. Em seguida, a função sample() sorteia dois números desse vetor com reposição e guarda o resultado no objeto dice. Por fim, a linha sum(dice) soma os dois valores sorteados e retorna o resultado final. Assim, roll2() simula o lançamento de dois dados comuns, enquanto roll2(1:20) demonstra a flexibilidade de alterar o intervalo de valores (como dados de RPG). A função bônus soma3 segue a mesma lógica, mas amplia o sorteio para três números.

## Exercício 5: Ajuda e Exemplos

**Objetivo:** Aprender a consultar a documentação oficial do R (HOPR cap. 2).

```{r exercicio5}
# Consultando a ajuda e rodando exemplos da função sample
# ?sample      # Abre a documentação (comentado para o Knit não travar)
example(sample)
```
Resposta 5: A função sample() sorteia elementos de um vetor. Seus principais argumentos são: x (vetor base), size (quantidade de elementos), replace (define se pode repetir valores: TRUE com reposição, FALSE sem reposição) e prob (probabilidades associadas). Para consultar ajuda, usa-se ?sample, e para ver exemplos práticos, example(sample). Esses recursos garantem que o usuário saiba aplicar corretamente os argumentos em diferentes contextos.

##Exercício 6: Simulação e Histograma
**Objetivo**: Gerar distribuições de dados e visualizar através de histogramas (HOPR cap. 1–2).

```{r exercicio 6}
set.seed(42)
somas <- replicate(10000, roll2())

# Gerando o histograma no relatório
hist(somas, 
     main = "Soma de dois dados (10.000 lançamentos)", 
     xlab = "Soma", 
     col = "steelblue", 
     border = "white")

# Estatísticas descritivas
mean(somas)
sd(somas)
```
Resposta 6: O histograma mostra que a soma de dois dados segue uma distribuição triangular, concentrada em torno de 7. A média obtida é próxima de 7 e o desvio-padrão em torno de 2,4, refletindo a variabilidade moderada. Valores extremos (2 e 12) aparecem pouco, enquanto 6–8 são os mais frequentes. Esse comportamento confirma que a soma segue uma distribuição simétrica, útil para compreender fenômenos aleatórios em políticas públicas.

##Exercício 7: Tipos Básicos
**Objetivo**: Identificar os tipos de dados atômicos no R (HOPR cap. 3).

```{r exercicio 7}
dbl <- c(1.5, 2.0)            # numéricos (double)
int <- c(1L, 2L)              # inteiros
chr <- c("saude", "educacao") # texto (character)
lgl <- c(TRUE, FALSE)         # lógico (booleano)
```
Resposta 7: A função str() revela a estrutura interna de cada objeto. Aqui vemos quatro tipos básicos: numérico (double), inteiro, texto (character) e lógico. Cada tipo tem usos práticos: números para cálculos, inteiros para contagens, texto para categorias e lógicos para condições em análises. É uma ferramenta essencial para garantir que os dados estejam no formato correto antes da análise.


##Exercício 8: Data.frame e Mini-base Municipal
**Objetivo**: Criar e inspecionar estruturas de dados tabulares (HOPR cap. 3).

```{r exercicio 8}
# Criação do Baralho
faces <- c("ace","two","three","four","five","six","seven",
           "eight","nine","ten","jack","queen","king")
suits <- c("spades","hearts","diamonds","clubs")
deck  <- data.frame(
  face  = rep(faces, times = 4),
  suit  = rep(suits, each = 13),
  value = rep(1:13, times = 4)
)

# Criação da Mini-base municipal
set.seed(2025)
municipios <- paste0("Mun_", sprintf("%02d", 1:10))
dados_munic <- data.frame(
  municipio        = municipios,
  gasto_saude_pc   = round(runif(10, 200, 1200), 2),
  taxa_evasao      = round(runif(10, 0.00, 0.20), 3),
  taxa_desemprego  = round(rnorm(10, 0.12, 0.03), 3)
)

nrow(deck); ncol(deck)
summary(dados_munic)
str(list(dbl = dbl, int = int, chr = chr, lgl = lgl))
```
Resposta 8: O baralho possui 52 linhas e 3 colunas. A mini-base municipal possui 10 linhas e 4 colunas. O summary() revela estatísticas descritivas (mínimo, máximo, média e quartis), permitindo comparar municípios e identificar variações nos indicadores, o que é fundamental para análises exploratórias iniciais.

##Exercício 9: Seleção e Filtros
**Objetivo**: Extrair subconjuntos de dados (HOPR cap. 4).

```{r exercicio 9}
# Exemplos de seleção no baralho
deck[1, ]                                
deck[-(1:48), ]                          

# Filtro de municípios com evasão alta
evaz_alta <- dados_munic[ dados_munic$taxa_evasao > 0.10, ] 
evaz_alta
nrow(evaz_alta)
```
Resposta 9: Os filtros permitem selecionar subconjuntos relevantes. No exemplo, deck[1, ] retorna a primeira linha e deck[-(1:48), ] as últimas quatro. O filtro evaz_alta identificou 4 municípios com taxa de evasão superior a 10%. Essa técnica é essencial para identificar casos críticos em indicadores sociais.

##Exercício 10: Modificando Valores e NA
**Objetivo**: Manipular valores existentes e tratar dados ausentes (HOPR cap. 5).

```{r exercicio 10}
# Modificando valores
deck2 <- deck
deck2$value[c(13, 26, 39, 52)] <- 14

# Tratando NAs
vals <- c(NA, 1:5)
mean(vals, na.rm = TRUE)

dados_m2 <- dados_munic
dados_m2$taxa_evasao[3] <- NA
mean(dados_m2$taxa_evasao, na.rm = TRUE)
```

Resposta 10: O argumento na.rm = TRUE faz com que funções como mean() ignorem valores ausentes (NA). Sem ele, qualquer operação com um NA retorna NA. Esse recurso é vital em políticas públicas, onde dados incompletos são comuns e precisam ser tratados para não invalidar a estatística do grupo.

##Exercício 11: Funções que Guardam Estado (Opcional)
**Objetivo**: Compreender o conceito de 'closures' e ambientes no R (HOPR cap. 6).

```{r exercicio 11}
setup <- function(deck_init) {
  DECK <- deck_init
  DEAL <- function() {
    card <- DECK[1, , drop = FALSE]
    DECK <<- DECK[-1, , drop = FALSE]
    return(card)
  }
  SHUFFLE <- function() {
    idx <- sample(seq_len(nrow(deck_init)), size = nrow(deck_init))
    DECK <<- deck_init[idx, , drop = FALSE]
    invisible(NULL)
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
cards$deal()
cards$deal()
```
Resposta 11: Funções que guardam estado mantêm uma memória interna atualizada a cada chamada usando o operador <<-. No exemplo, o baralho "lembra" quais cartas já foram retiradas. Um análogo em PP/CS seria uma função que simula uma fila de espera ou execução orçamentária, onde o saldo é atualizado a cada operação sem perder o histórico do processo.

##Finalização


```{r}
sessionInfo()