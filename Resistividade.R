#Gráfico de Resistividades

Res <- ggplot(perfil, aes(x=prof_m)) + 
  # 	a matriz de dados é "perfil", abcissa é prof_m e ordenada é Resistividade (P, M, R)
  
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  
  # 	define-se como gráfico de linha de cor definida por variável
  scale_x_reverse() + geom_line(aes(y = RESPROF, colour = "RESPROF")) +
  geom_line(aes(y = RESMED, colour = "RESMED")) +
  geom_line(aes(y = RESRASA, colour = "RESRASA")) +
  scale_colour_manual("", 
                      breaks = c("RESPROF", "RESMED", "RESRASA"),
                      values = c("red", "magenta", "maroon4")) +
  # 	a escala de "prof_m" é revertida (menor no alto, maior na base)
  coord_flip()+
  
  # 	gira-se o sistema de coordenadas ("prof_m" agora está na ordenada)
  #scale_y_continuous(trans='log10') +
  scale_y_log10(breaks = 10^(0:3),
                minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))
  ) +
  # annotation_logticks(sides = "l") +
  
  ggtitle(label="Resistividade")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle =element_text(hjust = 0.5))+
  theme(legend.justification = c(1, 0), legend.position = c(1, 0), legend.text=element_text(size=7))+
  #   centraliza o título ao longo do topo
  labs(x= "Profundidade"~(m), y = "Rt"~(Omega*m))
#   adiciona títulos mais significativos para os eixos
Res
