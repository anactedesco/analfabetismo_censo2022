library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr) 
library(sidrar)
library(showtext) 
library(tidyverse)
library(janitor) 
library(grid) 


options(scipen = 99999)


#####
#Para 2022
analfab22abl <- get_sidra( x= "9543",
                           geo = "City",
                           geo.filter = "4200101",
                           variable = 2513,
                           format = 2) 

#Filtrando os dados e calculando a taxa de analfabetismo
analfabetismoabl <- analfab22abl %>%
  filter(`Variável` == "Taxa de alfabetização das pessoas de 15 anos ou mais de idade") %>% 
  mutate(Taxa_Analfabetismo = 100 - `Valor`) %>%                                            
  select(`Sexo`, `Idade`, `Cor ou raça`, Taxa_Analfabetismo, `Ano`)  

#Definindo as faixas de idade 
faixas_idade <- c("Total", "15 a 19 anos", "20 a 24 anos", "25 a 34 anos", 
                  "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
                  "65 anos ou mais", "75 anos ou mais", "80 anos ou mais")

#Filtrando os dados para incluir apenas as faixas de idade desejadas e renomear as categorias de cor/raça
analfabetismo_ajustado <- analfabetismoabl %>%
  filter(`Idade` %in% faixas_idade & 
           `Sexo` != "Total" & 
           `Cor ou raça` != "Amarela") %>%
  mutate(`Cor ou raça` = case_when(
    `Cor ou raça` == "Branca" ~ "Brancos",             
    `Cor ou raça` == "Indígena" ~ "Indígenas",         
    `Cor ou raça` %in% c("Preta", "Parda") ~ "Negros", 
    TRUE ~ `Cor ou raça`),
    Idade = factor(`Idade`, levels = faixas_idade)) %>% 
  select(`Sexo`, `Idade`, `Cor ou raça`, Taxa_Analfabetismo, `Ano`)

#Verificando se existem duplicações após a combinação de "Preta" e "Parda" em "Negros"
duplicados <- analfabetismo_ajustado %>%
  group_by(`Sexo`, `Idade`, `Cor ou raça`) %>%
  filter(n() > 1)
print(duplicados)

#Se houver duplicações, podemos agregá-las
analfabetismo_ajustado <- analfabetismo_ajustado %>%
  group_by(`Sexo`, `Idade`, `Cor ou raça`, `Ano`) %>%
  summarise(Taxa_Analfabetismo = mean(Taxa_Analfabetismo, na.rm = TRUE)) %>% 
  ungroup()
print(head(analfabetismo_ajustado))

#Ordenando os gráficos no facet_wrap
ordem_cor <- c("Total", "Brancos", "Negros", "Indígenas")

#Finalmente, plotando o gráfico de taxa de analfabetismo por faixa de idade, sexo e cor/raça 
ggplot(analfabetismo_ajustado, aes(x = `Idade`, y = Taxa_Analfabetismo, fill = `Sexo`)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label = ifelse(Taxa_Analfabetismo == 0, "", round(Taxa_Analfabetismo, 0))),
            position = position_dodge2(width = 0.9, preserve = "single"),
            vjust = -0.5, hjust = 0.5, size = 3, color = "#65b7dd", fontface = "bold", family = "Montserrat") + 
  scale_fill_manual(values = c("Homens" = "#fded02", "Mulheres" = "#9b1627")) + # Definir as cores das barras
  labs(title = "Gráfico 2: Taxa de analfabetismo em Abelardo Luz (SC) por faixa de idade, sexo e cor - 2022",
       x = "Faixa de idade",
       y = "Taxa de analfabetismo (%)",
       caption = "Fonte: Censo Demográfico/IBGE (2022). Elaboração @abelardoluzemdados") + 
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),         #Definir a fonte padrão para todo o texto como Montserrat
    axis.text.x = element_text(angle = 45, hjust = 1),  #Inclinar os rótulos do eixo X para melhor leitura
    panel.grid.major = element_blank(),                 #Remover todas as linhas de grade principais
    panel.grid.minor = element_blank(),                 #Remover todas as linhas de grade menores
    panel.border = element_blank(),                     #Remover as bordas do panel
    strip.background = element_blank(),                 #Remover fundo das etiquetas dos facets
    strip.text = element_text(size = 10, face = "bold"),#Ajustar tamanho e estilo das etiquetas dos facets
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),#Ajustar margem ao redor do plot
    panel.spacing = unit(1, "lines"),                   #Aumentar espaçamento entre os panels
    axis.line.x = element_line(color = "black"),        #Adicionar linha no eixo X
    axis.line.y = element_line(color = "black"),        #Adicionar linha no eixo Y
    axis.ticks = element_blank(),                       #Remover todas as marcações nos eixos X e Y
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10), #Centralizar e estilizar a nota de rodapé
    legend.position = "top",                            #Posicionar a legenda no topo
    legend.title = element_blank(),                     #Remover o título da legenda
    legend.text = element_text(size = 10),              #Ajustar o tamanho do texto da legenda
    legend.justification = "center",                    #Centralizar a legenda
    legend.direction = "horizontal",                    #Organizar a legenda horizontalmente
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold") #Centralizar o título do gráfico
    ) +
  facet_wrap(~ factor(`Cor ou raça`, levels = ordem_cor), ncol = 2, strip.position = "top", scales = "free") +  #Criar gráficos separados por cor ou raça em duas colunas, ordenados
  ylim(0, 100) + #Definir o limite do eixo Y de 0 a 100 para todos os gráficos
  coord_cartesian(clip = "off") +
  scale_x_discrete(labels = c( "Total" = "Total",
                               "15 a 19 anos" = "15 a 19 anos",
                               "20 a 24 anos" = "20 a 24 anos",
                               "25 a 34 anos" = "25 a 34 anos",
                               "35 a 44 anos" = "35 a 44 anos",
                               "45 a 54 anos" = "45 a 54 anos",
                               "55 a 64 anos" = "55 a 64 anos",
                               "65 anos ou mais" = "65 a 74 anos",
                               "75 anos ou mais" = "75 a 79 anos",
                               "80 anos ou mais" = "80 anos ou mais"))


######
##Agora faremos para Santa Catarina
#Obtendo os dados para o estado de Santa Catarina
analfab22sc <- get_sidra(x = "9543",
                         geo = "State",
                         geo.filter = "42",  #Código do estado de Santa Catarina
                         variable = 2513,
                         format = 2) 

#Filtrando os dados e calculando a taxa de analfabetismo
analfabetismo_sc <- analfab22sc %>%
  filter(`Variável` == "Taxa de alfabetização das pessoas de 15 anos ou mais de idade") %>%
  mutate(Taxa_Analfabetismo = 100 - `Valor`) %>%
  select(`Sexo`, `Idade`, `Cor ou raça`, Taxa_Analfabetismo, `Ano`)

#Filtrando os dados para incluir apenas as faixas de idade desejadas e renomear as categorias de cor/raça
analfabetismo_sc_ajustado <- analfabetismo_sc %>%
  filter(`Idade` %in% faixas_idade & 
           `Sexo` != "Total" & 
           `Cor ou raça` != "Amarela") %>%
  mutate(`Cor ou raça` = case_when(
    `Cor ou raça` == "Branca" ~ "Brancos",             
    `Cor ou raça` == "Indígena" ~ "Indígenas",         
    `Cor ou raça` %in% c("Preta", "Parda") ~ "Negros", 
    TRUE ~ `Cor ou raça`),
    Idade = factor(`Idade`, levels = faixas_idade)) %>%
  select(`Sexo`, `Idade`, `Cor ou raça`, Taxa_Analfabetismo, `Ano`)

#Verificando se existem duplicações após a combinação de "Preta" e "Parda" em "Negros"
duplicados_sc <- analfabetismo_sc_ajustado %>%
  group_by(`Sexo`, `Idade`, `Cor ou raça`) %>%
  filter(n() > 1)
print(duplicados_sc)

#Se houver duplicações, podemos agregá-las
analfabetismo_sc_ajustado <- analfabetismo_sc_ajustado %>%
  group_by(`Sexo`, `Idade`, `Cor ou raça`, `Ano`) %>%
  summarise(Taxa_Analfabetismo = mean(Taxa_Analfabetismo, na.rm = TRUE)) %>%
  ungroup()
print(head(analfabetismo_sc_ajustado))

#Criando o gráfico para o estado de Santa Catarina
ggplot(analfabetismo_sc_ajustado, aes(x = `Idade`, y = Taxa_Analfabetismo, fill = `Sexo`)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label = ifelse(Taxa_Analfabetismo == 0, "", round(Taxa_Analfabetismo, 0))),
            position = position_dodge2(width = 0.9, preserve = "single"),
            vjust = -0.5, hjust = 0.5, size = 3, color = "#65b7dd", fontface = "bold", family = "Montserrat") + 
  scale_fill_manual(values = c("Homens" = "#fded02", "Mulheres" = "#9b1627")) +
  labs(title = "Gráfico 1: Taxa de analfabetismo em Santa Catarina por faixa de idade, sexo e cor - 2022",
       x = "Faixa de idade",
       y = "Taxa de analfabetismo (%)",
       caption = "Fonte: Censo Demográfico/IBGE (2022). Elaboração @abelardoluzemdados") + 
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    panel.spacing = unit(1, "lines"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.justification = "center",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  facet_wrap(~ factor(`Cor ou raça`, levels = ordem_cor), ncol = 2, strip.position = "top", scales = "free") +
  ylim(0, 100) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(labels = c( "Total" = "Total",
    "15 a 19 anos" = "15 a 19 anos",
    "20 a 24 anos" = "20 a 24 anos",
    "25 a 34 anos" = "25 a 34 anos",
    "35 a 44 anos" = "35 a 44 anos",
    "45 a 54 anos" = "45 a 54 anos",
    "55 a 64 anos" = "55 a 64 anos",
    "65 anos ou mais" = "65 a 74 anos",
    "75 anos ou mais" = "75 a 79 anos",
    "80 anos ou mais" = "80 anos ou mais"))


