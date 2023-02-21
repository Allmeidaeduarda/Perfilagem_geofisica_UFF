#Gráfico do perfil Sônico

Son <- ggplot(perfil , aes(x=prof_m, y=SONICO)) + 
  # 	a matriz de dados é "perfil", abcissa é prof_m e ordenada é SONICO
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  ylim(range(perfil$SONICO)) +
  xlim(range(perfil$prof_m)) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  
    geom_line(colour = "gray0", size = 0.5) +
  # 	define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # 	a escala de "prof_m" é revertida (menor no alto, maior na base)
  coord_flip()+
  # 	gira-se o sistema de coordenadas ("prof_m" agora está na ordenada)
  
  ggtitle(label="Vagarosidade")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle =element_text(hjust = 0.5))+
  #   centraliza o título ao longo do topo
  labs(x= "Profundidade"~(m), y = "dT"~(mu*s/ft))
#   adiciona títulos mais significativos para os eixos
Son

Son + scale_y_continuous(minor_breaks = seq(5*(min(perfil$SONICO %/% 5)),
                                            as.integer(1 + max(perfil$SONICO)) ,
                                            by = 1)) 
