library(tidyverse)
library(scales)
###### DICIONARIO ###### 
# idade =  age: Age of primary beneficiary (int)
# 
# sexo =  sex: Gender of beneficiary (male, female)
# 
# imc = bmi: Body Mass Index, a measure of body fat based on height and weight (float)
# 
# filhos = children: Number of children covered by health insurance (int)
# 
# fumante = smoker: Smoking status of the beneficiary (yes, no)
# 
# regiao = region: Residential region in the US (northeast, northwest, southeast, southwest)
# 
# conta = charges: Medical insurance cost billed to the beneficiary (float)



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

summary(banco)
str(banco)


fumante_banco <- banco %>%
  group_by(fumante) %>%
  summarise(
    custo_medio = mean(conta),
    custo_mediano = median(conta),
    qtd = n()
  ) %>% mutate(proporcao = percent(qtd/sum(qtd)), fumante = case_when(
    fumante == "yes" ~ "Sim",
    fumante == "no" ~ "Não",
    TRUE ~ fumante
  ))

plot_fumante <- ggplot(banco, aes(x = fumante, y = conta, fill = fumante)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "Comparação de Custos de Plano: Fumantes vs. Não Fumantes",
    x = "Hábito de Fumar",
    y = "Custos do Seguro (em $)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)+
  scale_x_discrete(labels = c("no" = "Não", "yes" = "Sim"))

plot_fumante

resultado_teste_t <- t.test(conta ~ fumante, data = banco)
print("Resultado do Teste T para a Hipótese 1:")
print(resultado_teste_t)

imc_banco <- banco %>%
  group_by(imc) %>%
  summarise(
    custo_medio = mean(conta),
    custo_mediano = median(conta),
    qtd = n()
  ) %>% mutate(proporcao = percent(qtd/sum(qtd)))


imc_plot <- ggplot(banco, aes(x = imc, y = conta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relação entre IMC e Valor do Plano",
    x = "IMC",
    y = "Custos do Plano (em $)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

imc_plot

resultado_correlacao <- cor.test(banco$imc, banco$conta)
print("Resultado do Teste de Correlação para a Hipótese 2:")
print(resultado_correlacao)




ggplot(banco, aes(x = imc, y = conta, color = fumante)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interação: IMC, Custos e Hábito de Fumar",
    x = "IMC",
    y = "Custos do Plano (em $)",
    color = "Fumante"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

idade_banco <- banco %>%
  group_by(idade) %>%
  summarise(
    custo_medio = mean(conta),
    custo_mediano = median(conta),
    qtd = n()
  ) %>% mutate(proporcao = percent(qtd/sum(qtd)))

idade_plot <- ggplot(banco, aes(x = idade, y = conta)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", color = "green", se = FALSE) + 
  labs(
    title = "Relação entre Idade e Valor do Plano",
    x = "Idade",
    y = "Custo do Plano (em $)"
  ) +
  theme_minimal()

idade_plot



correlacao_idade <- cor.test(banco$idade, banco$conta)
print("Resultado do Teste de Correlação para a Hipótese 3:")
print(correlacao_idade)
