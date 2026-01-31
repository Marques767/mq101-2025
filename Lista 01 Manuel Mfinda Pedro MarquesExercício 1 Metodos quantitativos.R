
# Curso: Pós_Graduação em Políticas Públicas

# Disciplina: Métodos Quantitativos


# MQ101 - Lista 01
# Nome: Manuel Mfinda Pedro Marques
# Data: 22/09/2025
# Descrição : Exercicios sobre fundamentos do R (HOPR , Partes I II )
#matricula:21202510196
# Exercicios 

# Exercício 1 — R como calculadora

10 + 2
(10 + 2) * 3
((10 + 2) * 3 - 6) / 3

# Explique em 3–5 linhas como os parênteses afetam o resultado. Relacione com cálculo de indicadores em PP/CS.
# Resposta: Os parênteses permitem estabelecer a ordem de execução das operaçõe, isto é, qual delas deve acontecer por primeiro e assim em seguida.
# Na primeira linha, a soma é direta porque não tem parênteses: 10 + 2 = 12.
# Na segunda linha, por possuir parenteses e ter o sinal de multiplicação, prineiro resolve-se o que está dentro de pârenteses e depois a multiplicação: (10 + 2) * 3 = 36.
# Na terceira linha, por ter pârenteses dentro de pârenteses, cada grupo de parênteses organiza a sequência de cálculos, resultando em 10.
# Sem parênteses, o R segue a precedência matemática padrão que é: multiplicação, seguido da divisão e por fim a soma e a subtração. 


# Exercício 2 — Objetos e nomeação

Name <- 1
name <- 0
Name + 1
name + 1

# Explique em 2–4 linhas a diferençaa entre Name e name.
# Resposta: Em linguagem R, a nomenclatura das variáveis tem sentido diferenciado quando escrevemos com letras maiúsculas ou minúsculas. 
# No caso acima, Name e name são dois objetos distintos: Name, que tem a inicial maiúscula vale 1 e name que tem a inicial minúscula vale 0. 
# Por isso, Name + 1 dá 2, e name + 1 dá 1. 
# É importante ter cuidado com a forma como nomeamos variáveis para evitar confusão.


# Exercício 3 — Sorteio e reprodutibilidade

set.seed(123)
die <- 1:6
sample(die, size = 2, replace = TRUE)

# Compare com e sem o uso de set.seed().
# Resposta: O comando set.seed() garante que os números aleatórios gerados sejam sempre os mesmos, tornando o sorteio reprodutível. Sem set.seed(), cada execução pode gerar um resultado diferente. 
# Isso é essencial em ciência de dados para garantir que outras pessoas possam reproduzir os mesmos resultados dos seus scripts.


# Exercício 4 — Sua primeira função

roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

roll2()
roll2(1:20)

# Explique cada linha da função. Bônus: crie soma3().
# Resposta: A linha roll2 <- function(bones = 1:6) { Cria uma função chamada roll2.
# Ela recebe um argumento chamado bones, que por padrão é 1:6 (ou seja, os números de um dado comum).

# Na linha dice <- sample(bones, size = 2, replace = TRUE), faz um sorteio de dois valores do vetor bones com reposição (os dois valores podem ser iguais) e armazena o resultado no objeto dice. 

# A linha sum(dice) é responsável pela soma dos dois valores sorteados. 

# A linha roll2() executa a função usando o valor padrão (1:6), simulando o lançamento de dois dados comuns e somando o resultado.

# A linha roll2(1:20) executa a função usando os números de 1 a 20, simulando dois dados de 20 lados, e somando o resultado.



# O Bônus é uma nova função onde no lugar de sortear 2 números em vez, ele irá sortear 3 e depois somá-los.

soma3 <- function(bones = 1:6) {
  dice <- sample(bones, size = 3, replace = TRUE)
  sum(dice)
}

soma3()
soma3(1:10)


# Exercício 5 — Ajuda e exemplos
? sample
example ( sample )

# Resuma os argumentos de sample() e como consultar ajuda.

# Resposta
# ?sample abre a documentação da função no RStudio, onde podemos ver o que ela faz, quais são os argumentos disponíveis e exemplos de uso.

# Example(sample) roda exemplos automáticos que ajudam a entender como a função funciona na prática.


# Exercício 6 — Simulação e histograma

set.seed(42)
somas <- replicate(10000, roll2())
length(somas)
hist(somas)
mean(somas); sd(somas)

# Inclua histograma, média e desvio padrão no PDF. Interprete em 4–6 linhas.
# A simulação de 10.000 lançamentos de dois dados mostra que os resultados seguem uma distribuição em forma de sino (triangular), com valores mais frequentes no centro (como 7) e menos frequentes nas extremidades (2 e 12).
# A média das somas aproxima-se de 7, que é o valor esperado quando se lança dois dados. O desvio padrão indica o grau de dispersão das somas em torno da média. O histograma ilustra bem essa distribuição simétrica.


# Exercício 7 — Tipos básicos

dbl <- c(1.5 , 2.0)                
int <- c(1L , 2L)                  
chr <- c("saude", "educacao")     
lgl <- c(TRUE , FALSE)            

str(list(dbl = dbl, int = int, chr = chr, lgl = lgl))

# Explique em 4–6 linhas o que str() revela.

#Resposta
# A função str() mostra a estrutura de um objeto de forma resumida. No exemplo, vemos uma lista com quatro componentes: dbl (números decimais), int (inteiros), chr (texto) e lgl (valores lógicos). 
# Cada elemento é identificado pelo seu tipo (como <dbl>, <int>, <chr>, <lgl>) e pelos valores contidos em cada vetor. 
# Isso ajuda a entender rapidamente a composição e o tipo de dados de um objeto, o que é essencial para evitar erros em análises.



# Exercício 9 — Seleção e filtros
deck[1, ]                          
deck[c(1, 3, 5), c("face", "suit")]  
deck[-(1:48), ]

subset_hearts <- deck[deck$suit == "hearts", ]
nrow(subset_hearts)
# Exercício 8 — Data.frame

faces <- c("ace", "two", "three", "four", "five", "six", 
           "seven", "eight", "nine", "ten", "jack", "queen", "king")

suits <- c("spades", "hearts", "diamonds", "clubs")

deck <- data.frame(
  face = rep(faces, times = 4),
  suit = rep(suits, each = 13),
  value = rep(1:13, times = 4)
)

nrow(deck)         
head(deck, 10)     

# Mini - base municipal
set.seed(2025)
municipios <- paste0("Mun_", sprintf("%02d", 1:10))

dados_munic <- data.frame(
  municipio = municipios,
  gasto_saude_pc = round(runif(10, 200, 1200), 2),
  taxa_evasao = round(runif(10, 0.00, 0.20), 3),
  taxa_desemprego = round(rnorm(10, 0.12, 0.03), 3)
)

head(dados_munic)
summary(dados_munic)


evaz_alta <- dados_munic[dados_munic$taxa_evasao > 0.10, ]
evaz_alta

# Exercício 10 — Modificando valores e NA
deck2 <- deck
deck2$value[c(13, 26, 39, 52)] <- 14
head(deck2, 13)  

vals <- c(NA, 1:5)
mean(vals)                    
mean(vals, na.rm = TRUE)     
dados_m2 <- dados_munic
dados_m2$taxa_evasao[3] <- NA
dados_m2$gasto_saude_pc[7] <- NA

mean(dados_m2$taxa_evasao)                
mean(dados_m2$taxa_evasao, na.rm = TRUE)  

# Exercício 11 — (Opcional) Funções que “guardam estado”
setup <- function(deck_init) {
  
  DECK <- deck_init  # Copia o baralho original
  
  DEAL <- function() {
    card <- DECK[1, , drop = FALSE]     # Seleciona a primeira carta
    DECK <<- DECK[-1, , drop = FALSE]   # Remove essa carta do "baralho"
    card                                # Retorna a carta
  }
  
  SHUFFLE <- function() {
    idx <- sample(seq_len(nrow(deck_init)), size = nrow(deck_init))
    DECK <<- deck_init[idx, , drop = FALSE]  # Embaralha e atualiza DECK
    invisible(NULL)
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

# Criar o baralho interativo
cards <- setup(deck)

# Usar o baralho
cards$deal()      # Puxa a 1ª carta
cards$deal()      # Puxa a 2ª carta
cards$shuffle()   # Embaralha o baralho
cards$deal()      # Puxa nova 1ª carta (após embaralhar)

# Exercício 12 — Mini-projeto integrador: Saúde
set.seed(123)

# Função sem viés
pressao_saude <- function() {
  demanda <- sample(1:6, 1, TRUE)
  equipe <- sample(1:6, 1, TRUE)
  insumos <- sample(1:6, 1, TRUE)
  demanda + equipe + insumos
}

# Simular 10.000 vezes
prs <- replicate(10000, pressao_saude())
hist(prs, col = "skyblue", main = "Pressão sobre saúde (sem viés)")
mean(prs)       # Média
sd(prs)         # Desvio padrão

# Probabilidade com viés: mais chance de sair "6" na demanda
prob_demanda <- c(rep(1/8, 5), 3/8)

pressao_vies <- function() {
  demanda <- sample(1:6, 1, TRUE, prob = prob_demanda)
  equipe <- sample(1:6, 1, TRUE)
  insumos <- sample(1:6, 1, TRUE)
  demanda + equipe + insumos
}

# Simular com viés
prs_bias <- replicate(10000, pressao_vies())
hist(prs_bias, col = "tomato", main = "Pressão sobre saúde (com viés)")

# Comparação das médias
mean(prs_bias) - mean(prs)  # Diferença de médias



