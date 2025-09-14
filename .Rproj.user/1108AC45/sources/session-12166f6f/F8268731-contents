if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(here) == F) install.packages("here"); require(here)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(scales) == F) install.packages("scales"); require(scales)

theme_set(theme_linedraw()+
            theme(panel.grid = element_blank()))

getwd()
setwd("/Users/joaohenriquebarbosa/Desktop/RBOT_passira")

passira <- read_xlsx("Formulário Pesquisa em Rede - RBOT _ PASSIRA (respostas).xlsx",
                     col_types = c("date","guess","guess","date","guess","numeric","guess","guess","guess",
                                   "guess","guess","numeric","guess","guess","numeric",
                                   "numeric","numeric","numeric","numeric","guess","guess","guess","guess","guess",
                                   "guess",
                                   "guess","guess","guess","guess","guess","guess","numeric","numeric","numeric",
                                   "numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric","numeric",
                                   "numeric","guess","guess","guess",
                                   "guess","guess","guess","guess"))


  colnames(passira) <- c("id","evento","nome_pesquisador","data_pesquisa","res_turista","idade",
                         "estado","municipio","pais","pernoite","cidade","qtd_noite",
                         "hospedagem","transporte","acesso","rodovia","hosp_rank","gastronomia",
                         "limpeza","atividades","outra_atividade","motivo","outra_motivo","oficina",
                         "interacao","outra_atracao","divulgacao","nome_rede","nome_rede_qual","acompanhante",
                         "outra_acompanhante","gasto_alimento","gasto_transp","gasto_compras",
                         "gasto_cultura","qtd_gastos","dia_evento","avaliacao_local","avaliacao_infra",
                         "avaliacao_org","avaliacao_divulg","avaliacao_comida","avaliacao_preco",
                         "avaliacao_produtos","avaliacao_prog","avaliacao_representacao",
                         "avaliacao_diversidade","avaliacao_aproximacao","avaliacao_acolhimento",
                         "avaliacao_seguranca","avaliacao_acessibilidade","avaliacao_limpeza",
                         "avaliacao_geral","melhor_ativ","volta","sexo","lgbt","raca","escolaridade",
                         "renda")


cidade <- passira %>% group_by(cidade) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))


nome_pesquisador <- passira %>% group_by(nome_pesquisador) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))


idade <- passira %>% group_by(idade) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))

idade_semna <- na.omit(idade)


ggplot(idade_semna %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(idade, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black") +
  theme(axis.text = element_text(angle = 10)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.5) +  
  expand_limits(y = c(0, 20)) +
  coord_flip()+
  labs(y = "Respostas", x = "Idade")
max(idade_semna$idade)
min(idade_semna$idade)
mean(idade_semna$idade)
ggsave("imagem1.png")

municipio <- passira %>% group_by(municipio) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd))) %>% drop_na()

ggplot(municipio %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(municipio, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black") +
  theme(axis.text = element_text(angle = 10)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.5) +  
  expand_limits(y = c(0, 150)) +
  coord_flip()+
  labs(y = "Respostas", x = "Município")
ggsave("municipio.png")


estado <- passira %>% group_by(estado) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))

ggplot(estado %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(estado, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black") +
  theme(axis.text = element_text(angle = 0)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -1) +  
  expand_limits(y = c(0, 300)) +
  labs(y = "Respostas", x = "Estado")
ggsave("estado.png")

pais <- passira %>% group_by(pais) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))


transporte <- passira %>% group_by(transporte) %>% 
  summarise(qtd=n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)),
         transporte = ifelse(transporte == "Ônibus fretado/Excursão", "Ônibus Fretado",
                             ifelse(transporte == "Ônibus rodoviário /Regular", "Ônibus",
                                    ifelse(transporte == "NR/NS","Não Respondeu", transporte))))

ggplot(transporte %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(transporte, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black") +
  theme(axis.text.x  = element_text(angle = 15)) +
  geom_text(aes(label = paste0(qtd, "\n(", prop, ")")),
            size = 3, vjust = -.2) +  
  expand_limits(y = c(0, 190), x= c(0,9)) +
  labs(y = "Respostas", x = "Meio de Transporte")
ggsave("transporte.png")

qtd_noite <- passira %>% group_by(qtd_noite) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))

qtd_noite_semNa <- na.omit(qtd_noite)

ggplot(qtd_noite_semNa %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(qtd_noite, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black") +
  theme(axis.text = element_text(angle = 0)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -.5) +  
  expand_limits(y = c(0, 70)) +
  labs(y = "Respostas", x = "Quantidade de Noites")+
  coord_flip()
ggsave("qtd_noite.png")
qtd_noite[is.na(qtd_noite)] <- 0                                

qtd_noite <- qtd_noite %>%
  group_by(qtd_noite) %>%
  summarise(
    qtd = sum(qtd),
    prop = sum(as.numeric(sub("%", "", prop))) / 100 
  ) %>%
  mutate(prop = scales::percent(prop))                        


qtd_noite <- qtd_noite %>% slice(-8)

hospedagem <- passira %>% group_by(hospedagem) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd))) %>% 
  mutate(hospedagem = ifelse(hospedagem == "NR/NS", "Não Sabe ou Não Respondeu", hospedagem))


ggplot(hospedagem %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(hospedagem, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black") +
  theme(axis.text = element_text(angle = 0)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -.7) +  
  expand_limits(y = c(0, 170)) +
  coord_flip()+
  labs(y = "Respostas", x = "Hospedagem")
ggsave("hospedagem.png")

pernoite <- passira %>% group_by(pernoite) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))


ggplot(pernoite %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(pernoite, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black") +
  theme(axis.text = element_text(angle = 0)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -1) +  
  expand_limits(y = c(0, 180)) +
  labs(y = "Respostas", x = "Hospedagem")
ggsave("pernoite.png")

res_turista <- passira %>% group_by(res_turista) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))

ggplot(res_turista %>% arrange(-qtd) %>% head(10), 
       aes(x = reorder(res_turista, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black") +
  theme(axis.text = element_text(angle = 0)) +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -1) +  
  expand_limits(y = c(0, 100)) +
  labs(y = "Respostas", x = "Perfil do Participante")
ggsave("res_turista.png")




escolaridade <- passira %>% group_by(escolaridade) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)),
                                escolaridade=ifelse(escolaridade == "Pós-Graduação/Mestrado/Doutorado", "Pós-Graduação", escolaridade))

ggplot(escolaridade, aes(reorder(escolaridade, qtd),qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  theme(axis.text = element_text(angle = 6))+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -0.5)+
  expand_limits(y = c(0,100))+
  labs(y = "Respostas", x = "Escolaridade")
ggsave("escolaridade.png")



renda <- passira %>% group_by(renda) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)),
                                renda = ifelse (renda == "Entre 1 SM e 3 SM (R$ 1.420,01 a R$ 4.260,00)", "1 SM",
                                                ifelse(renda == "Até 1 SM (R$ 1.420,00)", "Até 1 SM",
                                                       ifelse(renda == "Entre 3 SM e 5 SM (R$ 4.260,01 a R$ 7.100,00)", "3 a 5 SM",
                                                              ifelse(renda == "Entre 5 SM e 7 SM (R$ 7.100,01 a R$ 9.940 ,00)", "5 a 7 SM",
                                                                     ifelse(renda == "Entre 7 SM e 10 SM (R$ 9.940,01 a R$ 14.200,00)", "7 a 10 SM",
                                                                            ifelse(renda == "Acima de 10 SM (R$ 14.200,01)", "Acima de 10 SM", renda))))))) %>% 
  drop_na()



ggplot(renda, aes(x=reorder(renda, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  theme(axis.text = element_text(angle = 10))+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -1)+
  expand_limits(y = c(0,100))+
  labs(y = "Quantidade", x = "Renda")


ggsave("renda.png")


avaliacao_infra <- passira %>% group_by(avaliacao_infra) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_infra, aes(x=  factor(avaliacao_infra), y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,190))+
  labs(y = "Quantidade", x = "Nota da Infraestrutura")+
  coord_flip()
ggsave("avaliacao1.png")



avaliacao_local <- passira %>% group_by(avaliacao_local) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_local, aes(x=reorder(avaliacao_local, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,210))+
  labs(y = "Quantidade", x = "Nota do Local")+
  coord_flip()
ggsave("avaliacao2.png")

avaliacao_org <- passira %>% group_by(avaliacao_org) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_org, aes(x=reorder(avaliacao_org, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,200))+
  labs(y = "Quantidade", x = "Nota da Organização")+
  coord_flip()
ggsave("avaliacao3.png")


avaliacao_divulg <- passira %>% group_by(avaliacao_divulg) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_divulg, aes(x=reorder(avaliacao_divulg, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,170))+
  labs(y = "Quantidade", x = "Nota da Divulgação")+
  coord_flip()
ggsave("avaliacao4.png")

avaliacao_comida <- passira %>% group_by(avaliacao_comida) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_comida, aes(x=reorder(avaliacao_comida, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,160))+
  labs(y = "Quantidade", x = "Nota da Comida")+
  coord_flip()
ggsave("avaliacao5.png")

avaliacao_preco <- passira %>% group_by(avaliacao_preco) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_preco, aes(x=reorder(avaliacao_preco, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,160))+
  labs(y = "Quantidade", x = "Nota do Preço")+
  coord_flip()
ggsave("avaliacao6.png")

avaliacao_produtos <- passira %>% group_by(avaliacao_produtos) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_produtos, aes(x=reorder(avaliacao_produtos, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,190))+
  labs(y = "Quantidade", x = "Nota dos Produtos")+
  coord_flip()
ggsave("avaliacao7.png")


avaliacao_prog <- passira %>% group_by(avaliacao_prog) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_prog, aes(x=reorder(avaliacao_prog, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,190))+
  labs(y = "Quantidade", x = "Nota da Programação")+
  coord_flip()
ggsave("avaliacao8.png")

avaliacao_representacao <- passira %>% group_by(avaliacao_representacao) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_representacao, aes(x=reorder(avaliacao_representacao, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,210))+
  labs(y = "Quantidade", x = "Nota de Representação Cultural")+
  coord_flip()
ggsave("avaliacao9.png")

avaliacao_diversidade <- passira %>% group_by(avaliacao_diversidade) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_diversidade, aes(x=reorder(avaliacao_diversidade, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,200))+
  labs(y = "Quantidade", x = "Nota de Diversidade")+
  coord_flip()
ggsave("avaliacao10.png")


avaliacao_aproximacao <- passira %>% group_by(avaliacao_aproximacao) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_aproximacao, aes(x=reorder(avaliacao_aproximacao, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,210))+
  labs(y = "Quantidade", x = "Aprx Cultura Local")+
  coord_flip()
ggsave("avaliacao11.png")


avaliacao_acolhimento <- passira %>% group_by(avaliacao_acolhimento) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_acolhimento, aes(x=reorder(avaliacao_acolhimento, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,230))+
  labs(y = "Quantidade", x = "Nota de Acolhimento")+
  coord_flip()
ggsave("avaliacao12.png")

hospedagem <- passira %>% group_by(hospedagem) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_seguranca, aes(x=reorder(avaliacao_seguranca, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,100))+
  labs(y = "Quantidade", x = "Nota da Segurança")+
  coord_flip()


avaliacao_seguranca <- passira %>% group_by(avaliacao_seguranca) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_seguranca, aes(x=reorder(avaliacao_seguranca, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,210))+
  labs(y = "Quantidade", x = "Nota da Segurança")+
  coord_flip()
ggsave("avaliacao13.png")

avaliacao_acessibilidade <- passira %>% group_by(avaliacao_acessibilidade) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_acessibilidade, aes(x=reorder(avaliacao_acessibilidade, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#8F66FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,220))+
  labs(y = "Quantidade", x = "Nota da Acessibilidade")+
  coord_flip()
ggsave("avaliacao14.png")


avaliacao_limpeza <- passira %>% group_by(avaliacao_limpeza) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd)))%>% 
  drop_na()

ggplot(avaliacao_limpeza, aes(x=reorder(avaliacao_limpeza, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,220))+
  labs(y = "Quantidade", x = "Nota de Limpeza")+
  coord_flip()
ggsave("avaliacao15.png")


avaliacao_geral <- passira %>% group_by(avaliacao_geral) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd))) %>% 
  drop_na()


ggplot(avaliacao_geral, aes(x=reorder(avaliacao_geral, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#59E24D", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -0.1)+
  expand_limits(y = c(0,220))+
  labs(y = "Quantidade", x = "Avaliação Geral")+
  coord_flip()
ggsave("avaliacao16.png")

res_turista <- passira %>% group_by(res_turista) %>% 
  summarise(qtd=n()) %>% mutate(prop = percent(qtd/sum(qtd))) %>% 
  drop_na()


ggplot(res_turista, aes(x=reorder(res_turista, qtd),y=qtd))+
  geom_bar(stat = "identity", width = 0.6, fill = "#0068FF", color = "black")+
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -0.5)+
  expand_limits(y = c(0,220))+
  labs(y = "Quantidade", x = "Situação")
ggsave("avaliacao17.png")
