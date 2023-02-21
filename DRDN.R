# Cálculo de DR/DN
#Esta curva é obtida por uma combinação dos perfis de densidade (RHOB) e porosidade de nêutrons (NPHI), a partir de
#um procedimento de normalização de escalas.


#DR/DN = [(RHOB-2)/0.05]-[(0.45-NPHI)/0.03]


perfil$DRDN <- ((perfil$DENSIDADE-2.0)/0.05) - ((0.45-perfil$NEUTRAO/100)/0.03)
#Cria a coluna respectiva no data frame perfil 

#Cria o gráfico de DRDN

# Classes:  Arenito, DRDN < 0
#           Folhelho, DRDN >= 0

plot_DRDN <- ggplot(perfil, aes(x=prof_m, y=DRDN)) + 
  # 	a tabela "perfil" é a base de dados, "DEPT" é a abcissa e "DRDN" é a ordenada
  #   com "subset" estabelecemos uma condição para os dados de entrada no gráfico.
  #   no caso, apenas as linhas do volume original armazenado em Namorado em que POCO é igual a "NA01A" serão plotadas.
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  # coord_cartesian(xlim = range (Namorado$GR), ylim = range(Namorado$COTA), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "darkblue", size = 0.5) +
  # 	define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # 	a escala de "DEPT" é revertida (menor no alto, maior na base)
  coord_flip()+
  # 	gira-se o sistema de coordenadas ("DEPT" agora está na ordenada)
  
  ggtitle(label="DR/DN ", subtitle = "Well-perfil")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle =element_text(hjust = 0.5))+
  #   centraliza o título ao longo do topo
  labs(x= "Depth"~(m), y = "DR/DN") + 
  geom_line(colour="red",aes (y=0))

#   Exibir o gráfico 
plot_DRDN 
