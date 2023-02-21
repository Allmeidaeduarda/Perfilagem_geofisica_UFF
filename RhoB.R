rhoB <- ggplot(perfil, aes(x=prof_m, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "DENSIDADE" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$DENSIDADE), ylim = range(perfil$prof_m), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "blue", size = 0.4) +
  # 	define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
  # 	gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Densidade(g/cm3)")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

# O gráfico foi armazenado na variável "p2"
# Se invocarmos p2, teremos o gráfico na janela "Plots" no quadro inferior direito do RStudio
rhoB

# Usando o menu "Export" da aba de "Plots", o gráfico estático pode ser gravado como imagem
# ou PF, ajustando as suas dimensões manualmente.
# Ele também pode ser visto numa janela de "zoom" que também pode ser manipulada

# 	USANDO O PLOTLY para obter gráfico dinâmico interativo
rhoB2 <- ggplotly(rhoB)
rhoB2

# Gerando um histograma para analisar DENSIDADE

DENSIDADE_histo50 <-ggplot(perfil, aes(x=DENSIDADE)) + 
  geom_histogram(aes(y=..density..), bins=50, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(DENSIDADE))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Densidade"~(g/cm3)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Densidade (50 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

DENSIDADE_histo50

DENSIDADE_histo50 + geom_density(alpha = 0.2, bw = 0.01)
