## Código - Artigo ##

## Baseado no Código - TCC - João Vítor Rocha da Silva ##

## Pacotes ####

library(readr)
library(dplyr)
library(httr)
library(XML)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(plotly)
library(zoo)
library(forecast)
library(fpp3)
library(prophet)

##

#### PARTE 1 - EXTRAÇÃO DOS DADOS ####

## Usando o pacote nbastat para puxar os dados do Basketball-Reference

library(nbastatR)

## Aumentando a conexao do sistema

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

## Puxando os dados de 2014 a 2022 (Atualização do Artigo: Puxar até a última temporada realizada (2024))

dados <- game_logs(seasons = c(2014:2022)) ## Mudar aqui para 2024

## Selecionando apenas as variáveis a serem estudadas 

dados <- dados %>% select(yearSeason, slugTeam, numberGameTeamSeason, isWin,
                          namePlayer, minutes, pctFG3, pctFG2, pctFT, treb, 
                          ast, stl, blk, tov, pf, pts, plusminus) 

## NA nesses casos serão tradados como 0

dados <- dados %>% replace(is.na(.), 0)

dados$isWin <- ifelse(dados$isWin == "TRUE", 1, 0)

## Fazendo o Split para trabalhar com os dados por temporada 

dados = split(dados, f = dados$yearSeason)

#### PARTE 2 - CRIAÇÃO DAS COMPONENTES PRINCIPAIS DE CADA JOGADOR ####

## É importante levar em consideração que o código está todo repetido para todos os anos ao invés de um loop que pudesse automatizar o processo 
## A razão para a escrita do código assim foi que durante a escrita do TCC cada temporada era validada e explorada individualmente para garantir que todas as informações estavam lá

dados2014 <- dados[[1]]

#_______________________________________________________________________________________#
## Começando a estruturação dos dados por 2014 para depois repetir para os próximos anos
#_______________________________________________________________________________________#

dados2014 <- bind_rows(dados2014)

## Separando por jogador para criar as variáveis de interesse que não vem no conjunto de dados original

dados2014 <- split(dados2014, f = dados2014$namePlayer)

for (i in 1:length(dados2014)){
  
  aux <- dados2014[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2014[[i]] <- aux
  
}

## Juntando os dados novamente

dados2014 <- bind_rows(dados2014)

## Apagando as variáveis que não vão ser utilizadas 

dados2014 <- dados2014 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes 

## Nesse caso, dados faltantes são os jogos que o jogador não participou na temporada, como a NBA tem uma temporada de 82 jogos por equipe, um jogador tem que ter 82 linhas no dataset anual
## Entretanto, se ele não participa de algum jogo, temos que completar essas informações e adicionar os zeros, para que durante os cálculos da PCA eles não sejam privilegiados

dados2014 <- split(dados2014, f = dados2014$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2014
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2014)){
  
  teste <- dados2014[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2014[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2014)){
  
  aux2 <- dados2014[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2014[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2014 <- bind_rows(dados2014)

## Separando pelos jogadores para fazer as médias móveis

## Agora que temos as séries de 82 jogos prontas, incluindo o que os jogadores deixaram de fazer, ou seja, obtiveram 0 em tudo já que não jogaram, podemos calcular as médias móveis

dados2014 <- split(dados2014, f = dados2014$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos
## Em média, na NBA, uma equipe em 2 semanas joga 7 jogos, então é um bom balizador da tendência da performance dos jogadores

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2014)){
  
  aux <- dados2014[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2014[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2014[[i]]$namePlayer[1],
                          slugTeam = dados2014[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2014[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada de 2 semanas 

## Agora que temos as médias móveis que indicam os números dos jogadores durante um período de 2 semanas, 
## podemos fazer o indicador de performance desse período e criar nossas séries temporais

PCA2014 <- NULL
variancia2014 <- NULL ## Armazenando a porcentagem da variância explicada para ser utilizada mais para frente ##

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F) ## Importante lembrar que essa PCA é feita não-padronizada intencionalmente 
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2014 <- rbind(variancia2014, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1]) (Se quiser ver os coeficientes da PCA para uma melhor exploração)
  PCA2014[[i]] <- aux
  PCA2014[[i]] <- PCA2014[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2014[[i]])))
  
}

## Visualização da PCA ##

fviz_pca_biplot(aux2, title = "", geom.ind = "transparent", axes = c(1, 2), repel = T) +
  theme_minimal() + geom_point() + geom_text_repel(aes(label = ifelse(aux$pts > 24,
                                                                      as.character(aux$namePlayer), "")), 
                                                   point.padding = 0.5) 

## Resultado final para 2014 = PCA 2014

#__________________#
## Fazendo para 2015
#__________________#

dados2015 <- dados[[2]]

## Começando a estruturação dos dados por 2015 para depois repetir para os próximos anos

dados2015 <- bind_rows(dados2015)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2015 <- split(dados2015, f = dados2015$namePlayer)

for (i in 1:length(dados2015)){
  
  aux <- dados2015[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2015[[i]] <- aux
  
}

## Juntando os dados novamente

dados2015 <- bind_rows(dados2015)

## Apagando as variáveis que não vão ser utilizadas 

dados2015 <- dados2015 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2015 <- split(dados2015, f = dados2015$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2015
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2015)){
  
  teste <- dados2015[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2015[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2015)){
  
  aux2 <- dados2015[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2015[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2015 <- bind_rows(dados2015)

## Separando pelos jogadores para fazer as médias móveis

dados2015 <- split(dados2015, f = dados2015$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2015)){
  
  aux <- dados2015[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2015[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2015[[i]]$namePlayer[1],
                          slugTeam = dados2015[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2015[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2015 <- NULL
variancia2015 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2015 <- rbind(variancia2015, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2015[[i]] <- aux
  PCA2015[[i]] <- PCA2015[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2015[[i]])))
  
}

#__________________#
## Fazendo para 2016 
#__________________#

dados2016 <- dados[[3]]

## Começando a estruturação dos dados por 2016 para depois repetir para os próximos anos

dados2016 <- bind_rows(dados2016)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2016 <- split(dados2016, f = dados2016$namePlayer)

for (i in 1:length(dados2016)){
  
  aux <- dados2016[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2016[[i]] <- aux
  
}

## Juntando os dados novamente

dados2016 <- bind_rows(dados2016)

## Apagando as variáveis que não vão ser utilizadas 

dados2016 <- dados2016 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2016 <- split(dados2016, f = dados2016$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2016
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2016)){
  
  teste <- dados2016[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2016[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2016)){
  
  aux2 <- dados2016[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2016[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2016 <- bind_rows(dados2016)

## Separando pelos jogadores para fazer as médias móveis

dados2016 <- split(dados2016, f = dados2016$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2016)){
  
  aux <- dados2016[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2016[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2016[[i]]$namePlayer[1],
                          slugTeam = dados2016[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2016[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2016 <- NULL
variancia2016 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2016 <- rbind(variancia2016, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2016[[i]] <- aux
  PCA2016[[i]] <- PCA2016[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2016[[i]])))
  
}

#__________________#
## Fazendo para 2017
#__________________#

dados2017 <- dados[[4]]

## Começando a estruturação dos dados por 2017 para depois repetir para os próximos anos

dados2017 <- bind_rows(dados2017)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2017 <- split(dados2017, f = dados2017$namePlayer)

for (i in 1:length(dados2017)){
  
  aux <- dados2017[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2017[[i]] <- aux
  
}

## Juntando os dados novamente

dados2017 <- bind_rows(dados2017)

## Apagando as variáveis que não vão ser utilizadas 

dados2017 <- dados2017 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2017 <- split(dados2017, f = dados2017$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2017
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2017)){
  
  teste <- dados2017[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2017[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2017)){
  
  aux2 <- dados2017[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2017[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2017 <- bind_rows(dados2017)

## Separando pelos jogadores para fazer as médias móveis

dados2017 <- split(dados2017, f = dados2017$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2017)){
  
  aux <- dados2017[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2017[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2017[[i]]$namePlayer[1],
                          slugTeam = dados2017[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2017[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2017 <- NULL
variancia2017 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2017 <- rbind(variancia2017, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2017[[i]] <- aux
  PCA2017[[i]] <- PCA2017[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2017[[i]])))
  
}

#__________________#
## Fazendo para 2018
#__________________#

dados2018 <- dados[[5]]

## Começando a estruturação dos dados por 2018 para depois repetir para os próximos anos

dados2018 <- bind_rows(dados2018)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2018 <- split(dados2018, f = dados2018$namePlayer)

for (i in 1:length(dados2018)){
  
  aux <- dados2018[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2018[[i]] <- aux
  
}

## Juntando os dados novamente

dados2018 <- bind_rows(dados2018)

## Apagando as variáveis que não vão ser utilizadas 

dados2018 <- dados2018 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2018 <- split(dados2018, f = dados2018$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2018
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2018)){
  
  teste <- dados2018[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2018[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2018)){
  
  aux2 <- dados2018[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2018[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2018 <- bind_rows(dados2018)

## Separando pelos jogadores para fazer as médias móveis

dados2018 <- split(dados2018, f = dados2018$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2018)){
  
  aux <- dados2018[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2018[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2018[[i]]$namePlayer[1],
                          slugTeam = dados2018[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2018[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2018 <- NULL
variancia2018 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2018 <- rbind(variancia2018, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2018[[i]] <- aux
  PCA2018[[i]] <- PCA2018[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2018[[i]])))
  
}

#__________________#
## Fazendo para 2019
#__________________#

dados2019 <- dados[[6]]

## Começando a estruturação dos dados por 2019 para depois repetir para os próximos anos

dados2019 <- bind_rows(dados2019)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2019 <- split(dados2019, f = dados2019$namePlayer)

for (i in 1:length(dados2019)){
  
  aux <- dados2019[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2019[[i]] <- aux
  
}

## Juntando os dados novamente

dados2019 <- bind_rows(dados2019)

## Apagando as variáveis que não vão ser utilizadas 

dados2019 <- dados2019 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2019 <- split(dados2019, f = dados2019$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2019
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2019)){
  
  teste <- dados2019[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2019[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2019)){
  
  aux2 <- dados2019[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2019[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2019 <- bind_rows(dados2019)

## Separando pelos jogadores para fazer as médias móveis

dados2019 <- split(dados2019, f = dados2019$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2019)){
  
  aux <- dados2019[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2019[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2019[[i]]$namePlayer[1],
                          slugTeam = dados2019[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2019[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2019 <- NULL
variancia2019 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2019 <- rbind(variancia2019, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2019[[i]] <- aux
  PCA2019[[i]] <- PCA2019[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2019[[i]])))
  
}

#__________________#
## Fazendo para 2020
#__________________#

dados2020 <- dados[[7]]

## Começando a estruturação dos dados por 2020 para depois repetir para os próximos anos

dados2020 <- bind_rows(dados2020)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2020 <- split(dados2020, f = dados2020$namePlayer)

for (i in 1:length(dados2020)){
  
  aux <- dados2020[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2020[[i]] <- aux
  
}

## Juntando os dados novamente

dados2020 <- bind_rows(dados2020)

## Apagando as variáveis que não vão ser utilizadas 

dados2020 <- dados2020 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2020 <- split(dados2020, f = dados2020$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2020
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2020)){
  
  teste <- dados2020[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2020[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2020)){
  
  aux2 <- dados2020[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2020[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2020 <- bind_rows(dados2020)

## Separando pelos jogadores para fazer as médias móveis

dados2020 <- split(dados2020, f = dados2020$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2020)){
  
  aux <- dados2020[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2020[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2020[[i]]$namePlayer[1],
                          slugTeam = dados2020[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2020[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2020 <- NULL
variancia2020 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2020 <- rbind(variancia2020, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2020[[i]] <- aux
  PCA2020[[i]] <- PCA2020[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2020[[i]])))
  
}

#__________________#
## Fazendo para 2021
#__________________#

dados2021 <- dados[[8]]

## Começando a estruturação dos dados por 2021 para depois repetir para os próximos anos

dados2021 <- bind_rows(dados2021)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2021 <- split(dados2021, f = dados2021$namePlayer)

for (i in 1:length(dados2021)){
  
  aux <- dados2021[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2021[[i]] <- aux
  
}

## Juntando os dados novamente

dados2021 <- bind_rows(dados2021)

## Apagando as variáveis que não vão ser utilizadas 

dados2021 <- dados2021 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2021 <- split(dados2021, f = dados2021$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2021
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2021)){
  
  teste <- dados2021[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2021[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2021)){
  
  aux2 <- dados2021[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2021[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2021 <- bind_rows(dados2021)

## Separando pelos jogadores para fazer as médias móveis

dados2021 <- split(dados2021, f = dados2021$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2021)){
  
  aux <- dados2021[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2021[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2021[[i]]$namePlayer[1],
                          slugTeam = dados2021[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2021[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2021 <- NULL
variancia2021 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2021 <- rbind(variancia2021, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2021[[i]] <- aux
  PCA2021[[i]] <- PCA2021[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2021[[i]])))
  
}

#__________________#
## Fazendo para 2022
#__________________#

dados2022 <- dados[[9]]

## Começando a estruturação dos dados por 2022 para depois repetir para os próximos anos

dados2022 <- bind_rows(dados2022)

## Separando por jogador para fazer as variáveis que não vem automaticamente

dados2022 <- split(dados2022, f = dados2022$namePlayer)

for (i in 1:length(dados2022)){
  
  aux <- dados2022[[i]]
  aux$POSS <- aux$stl - aux$tov
  dados2022[[i]] <- aux
  
}

## Juntando os dados novamente

dados2022 <- bind_rows(dados2022)

## Apagando as variáveis que não vão ser utilizadas 

dados2022 <- dados2022 %>% select(- stl,  -tov)

## Separando pelos jogadores para completar os dados faltantes

dados2022 <- split(dados2022, f = dados2022$namePlayer)

## Criando o complete para dados faltantes

complete <- c(1:82)
complete <- as.data.frame(complete)
colnames(complete) <- "numberGameTeamSeason"
complete$yearSeason <- 2022
complete$pctFG3 <- 0
complete$pctFG2 <- 0
complete$pctFT <- 0
complete$ast <- 0
complete$pf <- 0
complete$plusminus <- 0
complete$isWin <- 0
complete$minutes <- 0
complete$treb <- 0
complete$blk <- 0
complete$pts <- 0
complete$POSS <- 0
complete$namePlayer <- "0"
complete$slugTeam <- "COMP"

## Join para completar

for (i in 1:length(dados2022)){
  
  teste <- dados2022[[i]]
  
  aux <- anti_join(complete, teste, by = c("numberGameTeamSeason"))
  
  aux2 <- union(teste, aux)
  
  aux2 <- aux2 %>% arrange(numberGameTeamSeason)
  
  aux2$namePlayer <- aux2$namePlayer[1]
  
  aux2$namePlayer[aux2$namePlayer == "0"] <- NA
  
  aux2 <- aux2 %>%
    fill(namePlayer, .direction = "down") %>%
    fill(namePlayer, .direction = "up")
  
  dados2022[[i]] <- aux2
  
}

## Completando os dados dos times COMP

for (i in 1:length(dados2022)){
  
  aux2 <- dados2022[[i]]
  aux2$slugTeam[aux2$slugTeam == "COMP"] <- NA
  
  aux2 <- aux2 %>%
    fill(slugTeam, .direction = "down") %>%
    fill(slugTeam, .direction = "up")
  
  aux2$isWin <- cumsum(aux2$isWin)
  
  dados2022[[i]] <- aux2
  
}

## Juntando os dados novamente

dados2022 <- bind_rows(dados2022)

## Separando pelos jogadores para fazer as médias móveis

dados2022 <- split(dados2022, f = dados2022$namePlayer)

## Fazendo a lista final com as medias da janela de 7 em 7 jogos 

medias_2_semanas <- NULL
aux2 <- data.frame()

for(i in 1:length(dados2022)){
  
  aux <- dados2022[[i]]
  
  for(j in 1:76){
    
    indice <- c(which(aux$numberGameTeamSeason == j | aux$numberGameTeamSeason == j+1 |
                        aux$numberGameTeamSeason == j+2 |
                        aux$numberGameTeamSeason == j+3 | aux$numberGameTeamSeason == j+4 |
                        aux$numberGameTeamSeason == j+5 |
                        aux$numberGameTeamSeason == j+6))
    aux <- aux[indice, ] %>% select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                    treb, ast, blk, pf, pts, plusminus, POSS)
    aux <- colMeans(aux)
    aux <- data.frame(matrix(aux, ncol = 12)) 
    colnames(aux) <- colnames(dados2022[[i]] %>% 
                                select(isWin, minutes, pctFG3, pctFG2, pctFT,
                                       treb, ast, blk, pf, pts, plusminus, POSS))
    aux <- aux %>% mutate(id = j, namePlayer = dados2022[[i]]$namePlayer[1],
                          slugTeam = dados2022[[i]]$slugTeam[j])
    
    aux2 <- rbind(aux2, aux)
    
    aux <- dados2022[[i]]
    
  }
  
  medias_2_semanas[[i]] <- aux2
  aux2 <- data.frame()
  
}

## Juntando os dados e separando por ID

medias_2_semanas <- bind_rows(medias_2_semanas)

medias_2_semanas <- split(medias_2_semanas, f = medias_2_semanas$id)

## Fazendo a PCA de cada rodada

PCA2022 <- NULL
variancia2022 <- NULL

for (i in 1:length(medias_2_semanas)){
  
  aux <- medias_2_semanas[[i]]
  aux2 <- PCA(aux %>% 
                select(isWin, plusminus, minutes, pctFG3, pctFG2, pctFT, treb, ast, POSS, blk, pf, pts) ,
              graph = F, scale.unit = F)
  aux <- aux %>% mutate(PC1 = get_pca_ind(aux2)$coord[, 1])
  variancia2022 <- rbind(variancia2022, aux2$eig[1, 2])
  # coeficientes_1_semana <- rbind(coeficientes_1_semana, aux2$var$coord[, 1])
  PCA2022[[i]] <- aux
  PCA2022[[i]] <- PCA2022[[i]] %>%
    arrange(desc(PC1)) %>% mutate(Ranking = c(1:nrow(PCA2022[[i]])))
  
}

#__________________#
## Fazendo para 2023
#__________________#

## Adicionar 23 aqui

#__________________#
## Fazendo para 2023
#__________________#

## Adicionar 24 aqui
#### PARTE 3 - CRIAÇÃO DO DATASET FINAL PARA ANÁLISE ####

## Agora que foi tudo verificado, podemos juntar tudo no dataset final ##

PCA2014 <- bind_rows(PCA2014)
PCA2014$Year <- 2014

PCA2015 <- bind_rows(PCA2015)
PCA2015$Year <- 2015

PCA2016 <- bind_rows(PCA2016)
PCA2016$Year <- 2016

PCA2017 <- bind_rows(PCA2017)
PCA2017$Year <- 2017

PCA2018 <- bind_rows(PCA2018)
PCA2018$Year <- 2018

PCA2019 <- bind_rows(PCA2019)
PCA2019$Year <- 2019

PCA2020 <- bind_rows(PCA2020)
PCA2020$Year <- 2020

PCA2021 <- bind_rows(PCA2021)
PCA2021$Year <- 2021

PCA2022 <- bind_rows(PCA2022)
PCA2022$Year <- 2022

## Adicionando os anos mais recentes 

# Adicionar aqui 2023 e 2024

## Montando o Dataset

dataset <- rbind(PCA2014, PCA2015, PCA2016, PCA2017, PCA2018, PCA2019, PCA2020, PCA2021, PCA2022)

## Salvando o dataset em CSV para eventual disponibilzação

write_csv(dataset, "dataset.csv")

## Variancias Explicadas pela PCA para Gráfico ##

variancias = data.frame(variancia2014, variancia2015, variancia2016,
                        variancia2017, variancia2018, variancia2019,
                        variancia2020, variancia2021, variancia2022) ## Adicionar 2023 e 2024

colnames(variancias) = c(2014:2022) ## Atualizar

variancias = melt(variancias)

colnames(variancias) = c('Year', '% of explained variance')

## Gráfico das Variâncias Explicadas ao Longo do Tempo ##

ggplot(variancias) + geom_boxplot(aes(x = Year, y = `% of explained variance`, fill = Year)) + theme_classic() 



#### PARTE 4 - CRIAÇÃO DAS SÉRIES TEMPORAIS HIERÁRQUICAS #####

## Hora de Construir as Séries Temporais dos Jogadores
## Lembrando que não desejamos avaliar os times pelos jogadores que não tem volumes de minuto baixo, pois eles podem prejudicar ou beneficar os times com performances outliers
## Então, sabendo que 90% em média dos minutos de um time da NBA são divididos entre 8 jogadores, selecionaremos estes jogadores para fazer parte da análise 
## Isso levanta um ponto bem importante também, que é se as equipes estão escalando entre os 8 jogadores mais presentes os melhores em ranking
## Ou se estão por alguma razão poupando seus talentos (Ponto da Eficiênci de Utilização de Jogadores)

#___________________________________________________________#
## Selecionando os 8 melhores jogadores em minutos dos times
#___________________________________________________________#

df <- dataset %>% group_by(Year, slugTeam, id) %>% arrange(desc(minutes)) %>% 
  slice(1:8) %>% ungroup() %>% 
  select(Year, id, namePlayer, slugTeam, Ranking)

## Montando as séries temporais hierárquicas a partir dos jogadores 

#_____________________________#
## Fazendo as séries dos times
#_____________________________#

df_times <-  NULL

series_times <- df %>% split(df , f = df$slugTeam)

for (i in names(series_times)){
  
  aux <- series_times[[i]] %>% split(series_times[[i]], f = series_times[[i]]$Year)
  aux3 <- data.frame()
  for (j in names(aux)){
    
    aux2 <- aux[[j]] %>% group_by(id) %>% summarize(RankingTime = sum(as.numeric(Ranking)))
    aux2$slugTeam <- i
    aux2$Year <- j
    aux3 <- rbind(aux3, aux2)
    
  }
  
  aux3$ts <- c(1:nrow(aux3))
  df_times[[i]] = aux3
  
}

df_times <- bind_rows(df_times)

df_times$id <- ifelse(df_times$id == "1", "01", df_times$id)
df_times$id <- ifelse(df_times$id == "2", "02", df_times$id)
df_times$id <- ifelse(df_times$id == "3", "03", df_times$id)
df_times$id <- ifelse(df_times$id == "4", "04", df_times$id)
df_times$id <- ifelse(df_times$id == "5", "05", df_times$id)
df_times$id <- ifelse(df_times$id == "6", "06", df_times$id)
df_times$id <- ifelse(df_times$id == "7", "07", df_times$id)
df_times$id <- ifelse(df_times$id == "8", "08", df_times$id)
df_times$id <- ifelse(df_times$id == "9", "09", df_times$id)

df_times$ts <- paste0(df_times$Year,".", df_times$id)

df_times$ts <- as.numeric(df_times$ts)

## Salvando dataset para possível disponibilização

write_csv(df_times, "dataset_times.csv")

## Gráfico ##

plot_ly(df_times, type = "scatter", mode = "lines") %>% 
  add_trace(x = ~ts, y = ~RankingTime, color = ~slugTeam)

#________________________________#
## Fazendo as séries das divisões
#________________________________#

Divisions <- c("Atlantic", "Central", "South East", "North West", "Pacific", "South West")

Atlantic <- df_times[df_times$slugTeam == "TOR" | 
                       df_times$slugTeam == "NYK" |
                       df_times$slugTeam == "PHI" |
                       df_times$slugTeam == "BRK" |
                       df_times$slugTeam == "BOS" , ]

Atlantic$slugTeam <- "Atlantic"

Atlantic <-  Atlantic %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

Central <- df_times[df_times$slugTeam == "MIL" | 
                      df_times$slugTeam == "CLE" |
                      df_times$slugTeam == "CHI" |
                      df_times$slugTeam == "DET" |
                      df_times$slugTeam == "IND" , ]

Central$slugTeam <- "Central"

Central <-  Central %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

SouthEast <- df_times[df_times$slugTeam == "ATL" | 
                        df_times$slugTeam == "CHA" |
                        df_times$slugTeam == "MIA" |
                        df_times$slugTeam == "ORL" |
                        df_times$slugTeam == "WAS" , ]

SouthEast$slugTeam <- "South East"

SouthEast <-  SouthEast %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

NorthWest <- df_times[df_times$slugTeam == "DEN" | 
                        df_times$slugTeam == "MIN" |
                        df_times$slugTeam == "POR" |
                        df_times$slugTeam == "OKC" |
                        df_times$slugTeam == "UTA" , ]

NorthWest$slugTeam <- "North West"

NorthWest <-  NorthWest %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

Pacific <- df_times[df_times$slugTeam == "GSW" | 
                      df_times$slugTeam == "LAC" |
                      df_times$slugTeam == "LAL" |
                      df_times$slugTeam == "SAC" |
                      df_times$slugTeam == "PHO" , ]

Pacific$slugTeam <- "Pacific"

Pacific <-  Pacific %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

SouthWest <- df_times[df_times$slugTeam == "DAL" | 
                        df_times$slugTeam == "HOU" |
                        df_times$slugTeam == "MEM" |
                        df_times$slugTeam == "NOP" |
                        df_times$slugTeam == "SAS" , ]

SouthWest$slugTeam <- "South West"

SouthWest <-  SouthWest %>% group_by(slugTeam, ts, Year) %>% summarise(RankingDiv = sum(RankingTime))

df_divisoes <- rbind(Atlantic, Central, SouthEast, NorthWest, Pacific, SouthWest)

## Salvando dataset para possível disponibilização

write_csv(df_divisoes, "dataset_divisoes.csv")

## Gráfico ##

plot_ly(df_divisoes, type = "scatter", mode = "lines") %>% 
  add_trace(x = ~ts, y = ~RankingDiv, color = ~slugTeam)

ggplot(df_divisoes, aes(x = ts, y = RankingDiv)) + geom_line(aes(color = slugTeam))

#_________________________#
## Séries das conferencias
#_________________________#

Eastern <- df_divisoes[df_divisoes$slugTeam == "Atlantic" | 
                         df_divisoes$slugTeam == "Central" |
                         df_divisoes$slugTeam == "SouthEast", ]

Eastern$slugTeam <- "Eastern"

Eastern <-  Eastern %>% group_by(slugTeam, ts, Year) %>% summarise(RankingConf = sum(RankingDiv))

Western <- df_divisoes[df_divisoes$slugTeam == "North West" | 
                         df_divisoes$slugTeam == "Pacific" |
                         df_divisoes$slugTeam == "South West", ]

Western$slugTeam <- "Western"

Western <-  Western %>% group_by(slugTeam, ts, Year) %>% summarise(RankingConf = sum(RankingDiv))

df_conferencias <- rbind(Eastern, Western)

## Salvando dataset para possível disponibilização

write_csv(df_conferencias, "dataset_conferencias")

## Gráfico ##

plot_ly(df_conferencias, type = "scatter", mode = "lines") %>% 
  add_trace(x = ~ts, y = ~RankingConf, color = ~slugTeam)

ggplot(df_conferencias, aes(x = ts, y = RankingConf)) + geom_line(aes(color = slugTeam))

#______________#
## Ranking Liga
#______________#

df_liga <- df_conferencias
df_liga$slugTeam <- "NBA"
df_liga <-  df_liga %>% group_by(slugTeam, ts, Year) %>% summarise(RankingNBA = sum(RankingConf))

## Salvando dataset para possível disponibilização

write_csv(df_liga, "dataset_liga")

plot_ly(df_liga, type = "scatter", mode = "lines") %>% 
  add_trace(x = ~ts, y = ~RankingNBA, color = ~slugTeam)

ggplot(df_liga, aes(x = ts, y = RankingNBA)) + geom_line(aes(color = slugTeam))
#### PARTE 5 - MODELOS DE PREVISÃO PONTUAL ####

#_______#
## ARIMA 
#_______#

## Parte 1 - Times

arima_simples_df_times = data.frame()

for (time in unique(df_times$slugTeam)) {
  
  for(ano in unique(df_times$Year)){
    
    team = df_times[df_times$slugTeam == time & df_times$Year == ano, ]
    
    team_data <- ts(team$RankingTime, start = 1, frequency = 14)
    
    arima_model <- auto.arima(team_data)
    
    predictions <- forecast(arima_model, h = 76)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(predictions$mean)),
                                   Pred = as.numeric(predictions$mean), Year = ano, ts = team$ts)
    
    arima_simples_df_times <- rbind(arima_simples_df_times, team_predictions)
    
  }
  
}

## Parte 2 - Liga

arima_simples_df_liga = data.frame()

for (time in unique(df_liga$slugTeam)) {
  
  for(ano in unique(df_liga$Year)){
    
    team = df_liga[df_liga$slugTeam == time & df_liga$Year == ano, ]
    
    team_data <- ts(team$RankingNBA, start = 1, frequency = 14)
    
    arima_model <- auto.arima(team_data)
    
    predictions <- forecast(arima_model, h = 76)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(predictions$mean)),
                                   Pred = as.numeric(predictions$mean), Year = ano, ts = team$ts)
    
    arima_simples_df_liga <- rbind(arima_simples_df_liga, team_predictions)
    
  }
  
}

#_____#
## ETS 
#_____#

## Parte 1 - Times

ets_simples_df = data.frame()

for (time in unique(df_times$slugTeam)) {
  
  for(ano in unique(df_times$Year)){
    
    team = df_times[df_times$slugTeam == time & df_times$Year == ano, ]
    
    team_data <- ts(team$RankingTime, start = 1, frequency = 14)
    
    arima_model <- ets(team_data, model = "ZZZ")
    
    predictions <- forecast(arima_model, h = 76)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(predictions$mean)),
                                   Pred = as.numeric(predictions$mean), Year = ano, ts = team$ts)
    
    ets_simples_df <- rbind(ets_simples_df, team_predictions)
    
  }
  
}


## Parte 2 - Liga

ets_simples_df_liga = data.frame()

for (time in unique(df_liga$slugTeam)) {
  
  for(ano in unique(df_liga$Year)){
    
    team = df_liga[df_liga$slugTeam == time & df_liga$Year == ano, ]
    
    team_data <- ts(team$RankingNBA, start = 1, frequency = 14)
    
    arima_model <- ets(team_data, model = "ZZZ")
    
    predictions <- forecast(arima_model, h = 76)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(predictions$mean)),
                                   Pred = as.numeric(predictions$mean), Year = ano, ts = team$ts)
    
    ets_simples_df_liga <- rbind(ets_simples_df_liga, team_predictions)
    
  }
  
}

#_________#
## PROPHET
#_________#

## Parte 1 - Times

prophet_df <- data.frame()

for (time in unique(df_times$slugTeam)) {
  for (ano in unique(df_times$Year)) {
    
    team <- df_times[df_times$slugTeam == time & df_times$Year == ano, ]
    
    team_data <- data.frame(ds = seq(as.Date("2014-01-01"), by = "day", length.out = 76), y = team$RankingTime)
    
    prophet_model <- prophet(team_data, yearly.seasonality = F, weekly.seasonality = F, daily.seasonality = F)
    
    future <- make_future_dataframe(prophet_model, periods = 76)
    
    forecast <- predict(prophet_model, future)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(forecast$yhat[!is.na(forecast$yhat)])),
                                   Pred = forecast$yhat[!is.na(forecast$yhat)],
                                   Year = ano,
                                   ts = team$ts)
    
    prophet_df <- rbind(prophet_df, team_predictions)
  }
}

## Parte 2 - Liga

prophet_df_liga <- data.frame()

for (time in unique(df_liga$slugTeam)) {
  
  for(ano in unique(df_liga$Year)){
    
    team = df_liga[df_liga$slugTeam == time & df_liga$Year == ano, ]
    
    team_data <- data.frame(ds = seq(as.Date("2014-01-01"), by = "day", length.out = 76), y = team$RankingNBA)
    
    prophet_model <- prophet(team_data, yearly.seasonality = F, weekly.seasonality = F, daily.seasonality = F)
    
    future <- make_future_dataframe(prophet_model, periods = 76)
    
    forecast <- predict(prophet_model, future)
    
    team_predictions <- data.frame(slugTeam = rep(time, length(forecast$yhat[!is.na(forecast$yhat)])),
                                   Pred = forecast$yhat[!is.na(forecast$yhat)],
                                   Year = ano,
                                   ts = team$ts)
    
    prophet_df_liga <- rbind(prophet_df_liga, team_predictions)
  }
}