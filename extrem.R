################## Atos  Extremistas Violentos #################################

## Pacotes 

library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library("geobr")
library(ggplot2)
library(psych)

### Função 

tema_graf <- function() {
  tema <-  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
    

  labs <-    labs( color = "Vítimas", 
                  caption='Fonte: MEC - Relatório - Ataques às Escolas no Brasil.
       Elaboração dos Autores.')
  
  return(list(tema,  labs))
}

tema_grafII <- function() {
  tema <-  theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  labs <-    labs( color = "Tipos", x = " ", y = " ",
                   caption='Fonte: Fórum Brasileiro de Segurança Pública.
    Elaboração dos Autores.')
  
  return(list(tema,  labs))
}

Atosv <- read_excel("extrem.xlsx",   sheet = "Atosv")

Casos <- Atosv %>% 
  group_by(Ano) %>% 
  summarise(Casos = n(),
             Mortos= sum(Mortos),
            Feridos = sum(Feridos))

ggplot(Casos, aes(x = Ano, y = Casos, color = "Casos")) +
  geom_line(size = 1, color = "#606060") +
  geom_smooth(method = "lm", se = FALSE, color = "#A2A19F", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2002, 2023, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  tema_graf()+labs(title = "Grafico 01 - Número de Atos Extremistas Violentos em Escolas")
 


ggplot(Casos, aes(x=Ano )) +
  geom_line(aes(y = Feridos, color = "Feridos"), size = 1) +
  geom_line(aes(y =Mortos , color = "Mortos"), size = 1) +
  geom_line(aes(y = Feridos, color = "Feridos"), size = 1) +
  scale_color_manual(values = c("Mortos" = "#A2A19F", "Feridos" = "#606060"))+
  scale_x_continuous(breaks = seq(2002, 2023, 1))+
  scale_y_continuous(
    breaks = seq(0,50, 5), limits = c(0,50))+
  tema_graf()+labs(title = "Grafico 02 - Atos Extremistas Violentos nas Esoclas -
              Número de Mortos e Feridos (2002-2023)")




ufv <-  Atosv %>% 
  group_by(uf) %>% 
  summarise(n = sum(Total) ) 

ufv %>% 
  ggplot(aes(x = reorder(uf, n), y = n, fill="#A2A19F")) +
  geom_bar(stat = "identity", color="#A2A19F", fill="#A2A19F") +  coord_flip()+
  scale_y_continuous(breaks = seq(0,50, 5), limits = c(0,50))+
  geom_text(aes(label = n), hjust = -0.5, color = "black", size = 3) +
  labs(title = "Gráfico 02 - Atos Extremistas Violentes em Escolas - Número de 
                              Vítmas Fatais por Estado")  + guides(fill=FALSE)+
  tema_graf()+xlab(" ")


##################### Crimes de Ódio - Racismo Injúria e Transfobia #############################

Extrem <- read_excel("extrem.xlsx", sheet = "geral")

Extrem %>% 
  filter(uf == "BR") %>% 
ggplot(aes(x=Ano )) +
  geom_line(aes(y = Racismon, color = "Racismo"), size = 1) +
  geom_line(aes(y =Injurian , color = "Injúria"), size = 1) +
  geom_smooth(aes(y = Racismon), method = "lm", se = FALSE, 
              color = "#A2A19F", linetype = "dashed") + 
  geom_smooth(aes(y = Injurian), method = "lm", se = FALSE, 
              color = "#3F5B72", linetype = "dashed") + 
  scale_color_manual(values = c("Racismo" = "#A2A19F", "Injúria" = "#3F5B72"))+
  scale_x_continuous(breaks = seq(2002, 2023, 1))+
  scale_y_continuous(
    breaks = seq(0,20000, 2000), limits = c(0,20000))+
  labs(title = "Gráfico 01 - Número de Registros de Racismo e Injúria Racial
                                         (2018-2022)") +
  tema_grafII()
 


Extrem %>% 
  filter(uf == "BR") %>% 
  ggplot(aes(x=Ano )) +
  geom_line(aes(y = lesaon, color = "Lesão"), size = 1) +
  geom_line(aes(y =homcidion , color = "Homicídio"), size = 1) +
  geom_smooth(aes(y = lesaon), method = "lm", se = FALSE, 
              color = "#3F5B72", linetype = "dashed") + 
  geom_smooth(aes(y = homcidion), method = "lm", se = FALSE, 
              color = "#A2A19F", linetype = "dashed") + 
  theme_classic()+
  scale_color_manual(values = c("Lesão" = "#3F5B72", "Homicídio" = "#A2A19F"))+
  scale_x_continuous(breaks = seq(2002, 2023, 1))+
  scale_y_continuous(
    breaks = seq(0,2500, 500), limits = c(0,2500))+
  labs(title = "Gráfico 02 - Registros de Lesão Corporal e Homicídio (LGBTQIA+)
                                                  (2018-2022)")+
  tema_grafII()
  

Extrem %>% 
  filter(uf == "BR") %>% 
  ggplot(aes(x=Ano )) +
  geom_line(aes(y = transfn, color = "Transfobia"), size = 1) +
  geom_line(aes(y =homcidion , color = "Homicídio"), size = 1) +
  geom_smooth(aes(y = transfn), method = "lm", se = FALSE, 
              color = "#3F5B72", linetype = "dashed") + 
  geom_smooth(aes(y = homcidion), method = "lm", se = FALSE, 
              color = "#A2A19F", linetype = "dashed") + 
  theme_classic()+
  scale_color_manual(values = c("Transfobia" = "#3F5B72", "Homicídio" = "#A2A19F"))+
  scale_x_continuous(breaks = seq(2002, 2023, 1))+
  scale_y_continuous(
    breaks = seq(0,800, 50), limits = c(0,800))+
  labs(title = "Gráfico 03 - Registros de Transfobia e Homicídio (LGBTQIA+)
                                              (2018-2022)") +
  tema_grafII()


## Estados

uf <- Extrem %>% 
  group_by(uf) %>% 
  summarise(Racismo = round(mean(Rascismotx, na.rm = T),2),
            Injúria = round(mean(Injuriatx, na.rm = T),2),
            Transfobia = round(mean(transftx, na.rm = T),2))

ggplot(uf, aes(x = reorder(uf, Racismo), y = Racismo)) +
  geom_bar(stat = "identity", width = 0.8, color="#A2A19F", fill= "#A2A19F") +
  coord_flip()+theme(axis.text.y = element_text(size = 9))+
  geom_text(aes(label = Racismo), hjust = -0.5, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(0,25, 5), limits = c(0,25))+
  labs(title = "Gráfico 04 - Média da Taxa de Registros de Racismo nos Estados")+
  geom_bar(data = . %>% filter(uf == "BR"), stat = "identity", color = "#606060",
           fill = "#606060", width = 0.9) +tema_grafII()



ggplot(uf %>% filter(uf != "ES"), aes(x = reorder(uf, Injúria), y = Injúria)) +
  geom_bar(stat = "identity", width = 0.8, color="#A2A19F", fill= "#A2A19F") +
  geom_text(aes(label = Injúria), hjust = -0.5, color = "black", size = 3) +
 coord_flip()+theme(axis.text.y = element_text(size = 9))+
  scale_y_continuous(breaks = seq(0,35, 5), limits = c(0,35))+
  labs(title = "Gráfico 05 - Média da Taxa de Registros de Injúria Racial nos Estados
                                          (2018-2022)")+
  geom_bar(data = . %>% filter(uf == "BR"), stat = "identity", color = "#606060",
           fill = "#606060", width = 0.9)+tema_grafII()


ggplot(uf %>% filter(uf != "RJ" & uf != "BA" & uf!= "MA" & uf != "TO" &
                       uf != "RN" & uf != "SC" & uf!= "SP"), aes(x = reorder(uf, Transfobia), y = Transfobia)) +
  geom_bar(stat = "identity", width = 0.8, color="#A2A19F", fill= "#A2A19F") +
  geom_text(aes(label = Transfobia), hjust = -0.5, color = "black", size = 3) +
  theme_minimal()+coord_flip()+theme(axis.text.y = element_text(size = 9))+
  scale_y_continuous(
    breaks = seq(0,2.5, .5), limits = c(0,2.5))+
  labs(title = "Gráfico 05 - Média da Taxa de Registros de Transfobia nos Estados
                                          (2020-2022)")+
  geom_bar(data = . %>% filter(uf == "BR"), stat = "identity", color = "#606060",
           fill = "#606060", width = 0.9) +tema_grafII()


######################### Crimes de Ódio - Internet ################################

odio <- read_excel("extrem.xlsx", sheet = "Odio")
odio2022 <- read_excel("extrem.xlsx", sheet = "Odio2022")
misog <- read_excel("extrem.xlsx", sheet = "misogeniasafernet")

ggplot(odio, aes(x = Ano, y = n/1000, color = "n")) +
  geom_line(size = 1, color = "#606060") +
  geom_smooth(method = "lm", se = FALSE, color = "#A2A19F", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2017, 2022, 1)) +ylab("Mil")+
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 350)) +
  tema_graf()+labs(title = "Gráfico 01 - Número de Crimes de Ódio Denunciados
                                    na Internet (2017-2022)", caption = 'Fonte: Safernet.
    Elaboração dos Autores.')



ggplot(odio2022, aes(x = reorder(Tipo, n/1000), y = n/1000)) +
  geom_bar(stat = "identity", width = 0.8, color="#A2A19F", fill= "#A2A19F") +
  coord_flip()+theme(axis.text.y = element_text(size = 9))+
  geom_text(aes(label = n/1000), hjust = -0.5, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(0,100, 10), limits = c(0,100))+
  tema_grafII()+ylab("Mil")+
  labs(title = "Gráfico 04 -  Número de Crimes de Ódio Denunciados
                              Na Internet por Tipo (2022)", 
       caption = 'Fonte: Safernet.
    Elaboração dos Autores.' )+
  geom_bar(data = . %>% filter(Tipo == "Misogenia"), stat = "identity", color = "#606060",
           fill = "#606060", width = 0.9) 


ggplot(misog, aes(x = Ano, y = n/1000, color = "n")) +
  geom_line(size = 1, color = "#606060") +
  geom_smooth(method = "lm", se = FALSE, color = "#A2A19F", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2017, 2022, 1)) +ylab("Mil")+
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50)) +
  tema_graf()+labs(title = "Grafico 01 - Número de Crimes de Ódio Denunciados - Misogenia
                                                        (2017-2022)", caption = 'Fonte: Safernet.
    Elaboração dos Autores.')+xlab(" ")
