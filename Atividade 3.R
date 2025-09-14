library(scales)
library(RKaggle)
library(tidyverse)
library(ggplot2)

dataset_kaggle <-"mosapabdelghany/medical-insurance-cost-dataset"
dados <- "dados"
dir.create(dados)
bd_download <- sprintf("kaggle datasets download -d %s -p %s --unzip", dataset_kaggle, dados)

system(bd_download)

banco <- read_csv("dados/insurance.csv") %>% 
  rename(
    idade = "age",
    sexo = "sex",
    imc = "bmi",
    filhos = "children",
    fumante = "smoker",
    regiao = "region",
    conta = "charges"
  )

dataset_kaggle <-"mosapabdelghany/medical-insurance-cost-dataset"
dados <- "dados"
dir.create(dados)
bd_download <- sprintf("kaggle datasets download -d %s -p %s --unzip", dataset_kaggle, dados)
system(bd_download)

banco <- read_csv("dados/insurance.csv") %>% 
  rename(
    idade = "age",
    sexo = "sex",
    imc = "bmi",
    filhos = "children",
    fumante = "smoker",
    regiao = "region",
    conta = "charges"
  )


idade_plot <- ggplot(banco, aes(idade)) +
  geom_histogram(breaks = c(seq(from = min(idade), to = max(idade), by = 3)), color = "black", fill = "skyblue") + 
  scale_x_continuous(breaks = c(seq(from = min(banco$idade), to = max(banco$idade), by = 3))) +
  labs(x = "Idade",
       y = "Frequencia")+
  theme_classic()

idade_plot

mediana_conta <- median(banco$conta)

densidade_conta <- ggplot(banco, aes(x = conta)) +
  geom_density(fill = "#2E86C1", color = "#1B4F72", alpha = 0.6) +
  geom_vline(
    aes(xintercept = mediana_conta),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = mediana_conta * 1.1, 
    y = 0.00002,             
    label = paste("Mediana: $", round(mediana_conta, 2)),
    color = "red",
    hjust = 0 
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = percent
  ) +
  labs(
    title = "Distribuição dos Custos dos Planos",
    subtitle = "Estimativa de Densidade para a variável 'conta'",
    x = "Valor da Conta ($)",
    y = "Densidade",
  ) +
  theme_minimal()
densidade_conta

regiao_boxplot <- ggplot(banco, aes(x = regiao, y = conta)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(from = 0, to = max(banco$conta), by = 5000)) +
  labs(
    title = "Comparação de Custos por Região",
    x = "Região",
    y = "Valor da Conta ($)"
  )

regiao_boxplot

filhos_boxplot <- ggplot(banco, aes(x = as.factor(filhos), y = imc)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(
    title = "Comparação do IMC por Quantidade de Filhos",
    x = "Filhos",
    y = "IMC"
  )

filhos_boxplot
