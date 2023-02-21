# 	Importar dados do Excel
library(readxl)
Exercicio_headerOK_EDT_2018 <- read_excel("Exercicio_headerOK_EDT_2018.xlsx")
# View(Exercicio_headerOK_EDT_2018)

# 	Renomear dataframe para tornar mais simples a digitação
perfil <- Exercicio_headerOK_EDT_2018

# 	Substituir os valores nulos (-999.25) pela constante lógica "NA" que indica 'dados ausentes'
perfil[perfil==-999.25] <-NA

# 	carregar bilblioteca "ggplot2" e "plotly"
require(ggplot2)
library(plotly)

# 	Raios Gama com todos os pontos
p1 <- ggplot(perfil, aes(x=prof_m, y=GR)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "blue", size = 0.4) +
  # 	define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
  # 	gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama (API)")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo
p1


# 	USANDO O PLOTLY para obter gráfico interativo
pp1 <- ggplotly(p1)
pp1

# 	Classificando ARN/FLH com corte simples de Raios Gama. Escolhendo valor de corte
#	no gráfico interativo gerado no ggplot2 + Plotly

#	Adota-se valor de corte = 70 API
valor_de_corte <- 70

# 	Criar novas colunas para armazenar "ARN" e "FLH"
perfil$ARN <- as.numeric("NA")
perfil$FLH <- as.numeric("NA")
# 	As colunas são criadas à direita e preenchidas com a constante lógica "NA" que indica 'dados ausentes'
#   O comando "as.numeric" indica que os campos da coluna são números

#	Loop para comparar GR com valor_de_corte e armazenar GR nas colunas adequadas

#	Limite do loop é dado pelo número de linhas do data frame
# O comando "dim(perfil)" dá o vetor resultado [776  19] : 776 linhas, 19 colunas
# Deste modo, "dim(perfil)[1] retorna o o valor armazenado na posição '1' do vetor
# que é o número de linhas da matriz
linhas <- dim(perfil)[1]

for (i in  1:linhas){       # Início do loop 'for'
  
      # O loop if-else compara linha a linha se o valor presente é maior que 70 API
	    # Se o teste for TRUE, replica-se o 'gama' na coluna 'FLH'
	    # Se FALSE, executa-se o comando em 'else' (armazena-se em 'ARN')
  # Início do loop if-else	 
   if(Exercicio_headerOK_EDT_2018$GR[i] > valor_de_corte ) {
    perfil$FLH [i] <- perfil$GR[i]
  }
  else {
      if(Exercicio_headerOK_EDT_2018$GR[i] > 0){
	  perfil$ARN[i]<- perfil$GR[i]
      }}
}

# Note que para essa operação lógica estou usando o data frame original com nulos (-999.25).
# Se usarmos o dataframe "perfil" onde substituímos o nulo por "NA", haverá uma mensagem de erro
# pois algumas linhas não têm dados e não podem ser usadas para o teste lógico.
# Não há nenhum efeito prático nos resultados porém a função de compilar do RStudio não rodará.

# Entretanto, o armazenamento se dá no data frame "perfil" pois as células com "NA" são 'puladas'
# quando o gráfico é feito

p2 <- ggplot(subset(perfil, perfil$ARN != "NA"), aes(x=prof_m, y=GR)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  # o comando 'subset' gera uma matriz na memória a partir do data frame "perfil"
  # "perfil" é o primeiro argumento no comando e define o data frame analisado
  # a condição perfil$ARN diferente de "NA" separa todos os pontos classificados
  # como ARN e plota a linha correspondente da coluna "GR"
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_point(colour = "blue") +
  # define-se como gráfico de pontos de cor azul
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
# gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama - Arenitos")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo
p2

# O gráfico gerado só tem pontos...
# Vamos aproveitar para ver como funciona a biblioteca 'plotly' neste caso.

# USANDO O PLOTLY
pp2 <- ggplotly(p2)
pp2

# Vamos repetir tudo usando 'geom_line' para obter um gráfico de linha

p3 <- ggplot(subset(perfil, perfil$ARN != "NA"), aes(x=prof_m, y=GR)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  # o comando 'subset' gera uma matriz na memória a partir do data frame "perfil"
  # "perfil" é o primeiro argumento no comando e define o data frame analisado
  # a condição perfil$ARN diferente de "NA" separa todos os pontos classificados
  # como ARN e plota a linha correspondente da coluna "GR"
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "blue", size = 0.4) +
  # define-se como gráfico de pontos de cor azul
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
# gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama - Arenitos")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

p3

# USANDO O PLOTLY
pp3 <- ggplotly(p3)
pp3

# Ainda há um problema pois a linha não é interrompida onde não há dados...
# Vamos plotar então diretamente a coluna "FLH" do data frame "perfil" em verde escuro

p4 <- ggplot(perfil, aes(x=prof_m, y=FLH)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "FLH" (GR do folhelho) é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$FLH, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "darkgreen", size = 0.4) +
  # 	define-se como gráfico de linha de cor verde escura
  scale_x_reverse() +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()
# 	gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama - Folhelhos")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

p4

# USANDO O PLOTLY
pp4 <- ggplotly(p4)
pp4

# Agora, vamos plotar então diretamente a coluna "ARN" do data frame "perfil" em laranja

p5 <- ggplot(perfil, aes(x=prof_m, y=ARN)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "ARN" (GR do arenito) é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$ARN, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "orange", size = 0.4) +
  # 	define-se como gráfico de linha de cor laranja
  scale_x_reverse() +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
# 	gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama - Arenitos")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

p5

# USANDO O PLOTLY
pp5 <- ggplotly(p5)
pp5


# Gráficos de linhas sobrepostos
p6 <- ggplot(perfil, aes(x=prof_m, y=GR)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "darkgreen", size = 0.4) +
  # define-se como gráfico de linha de cor verde escura
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  # Inserir novo gráfico sobreposto ao anterior: nova *geom_line" especificando novo *data=*
  geom_line(data = perfil, colour = "orange", aes(x=prof_m, y=ARN))+
  # Inserir a linha de corte em vermelho
  geom_line(colour = "red", aes(y=70))+
  
  ggtitle("Raios Gama - ARN/FLH")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

p6

# USANDO O PLOTLY
pp6 <- ggplotly(p6)
pp6

# O comando *subplot* da biblioteca plotly reúne os graficos em um unico painel.
painel1 <- subplot(pp1, pp2, pp3, pp4, pp5,pp6)
painel1

# Vamos reduzir apenas aos dois perfis que nos interessam agora
painel2 <- subplot(pp1, pp6)
painel2

# Para sobrepor RhoB x PhiN, não é possível usar a mesma estratégia porém ...
# As duas variáveis estão em escalas diferentes e devemos usar um 'truque' para resolver isso

# Recalcular PhiN somente para sobrepor na escala 45 a -15, sobre RhoB de 2 a 3
# Criar nova coluna para armazenar N_ggplot
perfil$N_ggplot <- "NA"

# Você notou que essa nova coluna não foi definida explicitamente como numérica?
# Como ela armazenará o resultado de uma operação aritmética,
# o R assumirá que a coluna é numérica.

# Reescalar PhiN para mapeá-lo diretamente na escala de densidade de 2 a 3
perfil$N_ggplot <- ((45-perfil$NEUTRAO)/60 + 2)

# Sobreposição RhoB x PhiN
p7 <- ggplot(perfil, aes(x=prof_m, y=DENSIDADE)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "DENSIDADE" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim (2,3), ylim = range(perfil$prof_m, na.rm=TRUE), expand = FALSE) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "blue", size = 0.4) +
  # define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  # Inserir novo gráfico sobreposto ao anterior: nova *geom_line" especificando novo *data=*
  geom_line(data = perfil, colour = "red", aes(x=prof_m, y=N_ggplot))+
  
  ggtitle("Densidade/Neutrão")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo


p7
# USANDO O PLOTLY
#library(plotly)
pp7 <- ggplotly(p7)
pp7

# Vamos montar um novo painel para analisar comparativamente
painel3 <- subplot(pp1, pp6, pp7)
painel3

# É evidente que o simples corte no GR não conseguiu classificar todos os corpos de
# arenito e folhelho com fidelidade. Veja os cruzamentos de RhoB e PhiN para confirmar.
# Vamos tentar um novo corte para GR e verificar se temos melhor resultado.
# Para realizar a tarefa, é só copiar o trecho correspondente anterior do código e ajustá-lo

#####     Segundo Corte com GR - INÍCIO      #####

#	Adota-se valor de corte = 120 API (cruzamento RhoBxPhiN com menor Gama)
valor_de_corte <- 120

# 	Criar novas colunas para armazenar "ARN" e "FLH"
perfil$ARN120 <- as.numeric("NA")
perfil$FLH120 <- as.numeric("NA")
# 	As colunas são criadas à direita e preenchidas com a constante lógica "NA" que indica 'dados ausentes'
#   O comando "as.numeric" indica que os campos da coluna são números

#	Loop para comparar GR com valor_de_corte e armazenar GR nas colunas adequadas

#	Limite do loop é dado pelo número de linhas do data frame
linhas <- dim(perfil)[1]

for (i in  1:linhas){       # Início do loop 'for'
  
  # O loop if-else compara linha a linha se o valor presente é maior que 120 API
  # Se o teste for TRUE, replica-se o 'gama' na coluna 'FLH'
  # Se FALSE, executa-se o comando em 'else' (armazena-se em 'ARN')
  # Início do loop if-else	 
  if(Exercicio_headerOK_EDT_2018$GR[i] > valor_de_corte ) {
    perfil$FLH120 [i] <- perfil$GR[i]
  }
  else {
    if(Exercicio_headerOK_EDT_2018$GR[i] > 0){
      perfil$ARN120[i]<- perfil$GR[i]
    }}
}

#####     Segundo Corte com GR - FIM        #####

# Para fazer um novo gráfico de arenitos e folhelhos, basta replicar o código e ajustá-lo também

#####     Novo gráfico com o corte em 120API - INÍCIO       #####

# Gráficos de linhas sobrepostos
p8 <- ggplot(perfil, aes(x=prof_m, y=GR)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "darkgreen", size = 0.4) +
  # define-se como gráfico de linha de cor verde escura
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  # Inserir novo gráfico sobreposto ao anterior: nova *geom_line" especificando novo *data=*
  geom_line(data = perfil, colour = "orange", aes(x=prof_m, y=ARN120))+
  # Inserir a linha de corte em vermelho
  geom_line(colour = "red", aes(y=120))+
  
  ggtitle("Raios Gama - ARN/FLH")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

p8

# USANDO O PLOTLY
pp8 <- ggplotly(p8)
pp8

# Vamos montar um novo painel para analisar os resultados
painel4 <- subplot(pp1, pp8, pp7)
painel4

#####     Novo gráfico com o corte em 120API - FIM       #####


# Como pode-se analisar, o corte de 120 API é bastante bom para esta classificação
# Desse modo, podemos considerar que 120 API é o indicador da mudança faciológica
# de arenitos para folhelhos (GR_Max).
# Já temos então um dos limites para o cálculo da Argilosidade com GR.
# O 'lag' entre RhoB e PhiN não é típico de arenitos com argilosidade zero e podemos
# decidir arbitrariamente por um Gama Mínimo de arenito convencional de 37 API
# Com tais limites, Argilosidade com Raios Gama pode ser calculada como:

#             (GR_lido - GR_Min) / (GR_Max - GR_Min)

# Primeiro, faremos a operação sem condições ao longo de toda a coluna de GR

# Criar nova coluna numérica para armazenar resultados
perfil$VSH_GR1 <- as.numeric("NA")

# Armazenar os valores-limite
GR_Min <- 37
GR_Max <- 120

# Aplicar a formulação para calcular VSH_GR1 (argilosidade GR em toda a coluna)
#              (GR_lido - GR_Min) / (GR_Max - GR_Min)
perfil$VSH_GR1 <- (perfil$GR - GR_Min) /  (GR_Max - GR_Min)

# Vamos plotar para ver o resultado

p9 <- ggplot(perfil, aes(x=prof_m, y=VSH_GR1)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "VS_GR1" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$VSH_GR1, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "darkgrey", size = 0.4) +
  # define-se como gráfico de linha de cor verde escura
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  # Inserir a linha de corte em vermelho
  geom_line(colour = "red", aes(y=1))+
  
  ggtitle("VSH-GR")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

  p9

# USANDO O PLOTLY
pp9 <- ggplotly(p9)
pp9

# Um novo painel para comparar
painel5 <- subplot(pp1, pp8, pp7, pp9)
painel5

# Calculando DRDN

# Inicializar a coluna DRDN no data frame "perfil"
perfil$DRDN <- as.numeric("NA")

# Calcular DRDN : [(RhoB - 2) / 0.05] - [(0.45 - Nphi)/0.03]
# Classes:  Arenito, DRDN < 0
#           Folhelho, DRDN >= 0

perfil$DRDN <- ((perfil$DENSIDADE-2)/0.05) - ((0.45 - perfil$NEUTRAO/100)/0.03)

# Vamos plotar para analisar

p10 <- ggplot(perfil, aes(x=prof_m, y=DRDN)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "DRDN" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$DRDN, na.rm=TRUE), ylim = range(perfil$prof_m, na.rm=TRUE), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "cyan", size = 0.4) +
  # define-se como gráfico de linha de cor verde ciano e espessura de linha ~1pt
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  # Inserir a linha de corte em vermelho
  geom_line(colour = "red", aes(y=0))+
  
  ggtitle("DRDN")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
    #   centraliza o título ao longo do topo
  p10
  
  # Para melhorar a visualização, podemos preencher em amarelo as áreas correspondentes aos arenitos
  # no gráfico de DRDN.
  # O comando "geom_ribbon()" do ggplot2 permite gerar essa representação mas precisamos de uma
  # matriz de dados numérica intermediária somente para armazenar os pontos com DRDN negativo.
  # O data frame intermediário será denominado "fita_DRDN" e terá duas colunas somente. Na primeira
  # coluna, será armazenada a profundidade (prof_m) e na segunda a variável "DRDN".
  
  fita_DRDN<- perfil[c(1,19)]
  head(fita_DRDN)
    # O data frame é inicializado replicando as colunas correspondentes do data frame "perfil"
  
  fita_DRDN[,2] <- as.numeric(ifelse(fita_DRDN$DRDN < 0, fita_DRDN$DRDN, "NA"))
  # com o comando vetorial 'ifelse' percorre-se a coluna "fita_DRDN$DRDN" e executa-se a 
  # operação lógica ('variável < 0'). Se o resultado for TRUE, a segunda coluna do dataframe
  # se mantém inalterada (re-armazena-se "fita_DRDN$DRDN" na coluna e linha correspondente);
  # se FALSE, armazena-se "NA" na célula.
  # O comando "as.numeric" impõe que a coluna resultante seja preenchida com uma variável numérica.
  
    p10 <- p10 + geom_ribbon(data= fita_DRDN, aes(ymin=DRDN, ymax=0, x=prof_m),fill = "yellow", alpha=0.3)
  # prenche as áreas do gráfico com DRDN negativo (ARN). O arquivo de dados intermediário é explicitado
  # por "data = fita_DRDN" dentro do comando "geom_ribbon ()" e os componentes do gráfico estão em "aes()".
  # A cor do preenchimento ('fill') e a transparência ('alpha') devem se especificados fora de 'aes' de 
  # acordo com a sintaxe.
  
    
  p10
  
##########################################################  
  
# Sobrepor DRDN e o gráfico composto Rho-Phi  
  
  p10+
    geom_line(data = perfil, aes(x=prof_m, y=DENSIDADE)) + 
    # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "DENSIDADE" é a ordenada
    #theme_linedraw() +
    # aplica um tema para o gráfico sem plano de fundo, somente com linhas
    coord_cartesian(xlim (2,3), ylim = range(perfil$prof_m, na.rm=TRUE), expand = FALSE) +
    # os limites do gráfico são explicitados para os limites dos dados
    geom_line(colour = "blue", size = 0.4) +
    # define-se como gráfico de linha de cor azul
    scale_x_reverse() +
    # a escala de "Depth" é revertida (menor no alto, maior na base)
    coord_flip() +
    # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
    
    # Inserir novo gráfico sobreposto ao anterior: nova *geom_line" especificando novo *data=*
    geom_line(data = perfil, colour = "red", aes(x=prof_m, y=N_ggplot))+
    
    ggtitle("Densidade/Neutrão")+
    #   adiciona um título no topo do gráfico
    theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo
    
##########################################################  

    # Os gráficos podem ser gravados também 
ggsave("teste.pdf", width = 8.267, height = 11.6925)
ggsave("teste.svg", device="svg",  width = 8.267, height = 11.6925)

# USANDO O PLOTLY
pp10 <- ggplotly(p10)
pp10
# Observar que há alguma incompatibilidade entre a biblioteca 'plotly' e "geom_ribbon" do ggplot2.

# Um novo painel para comparar
painel6 <- subplot(pp1, pp8, pp7, pp9, pp10)
painel6

# Para gerar um pdf com uma suite especifica de perfis, podemos usar a função "multiplot"

# MÚLTIPLOS GRÁFICOS NUM ÚNICO PAINEL
## multiplot()
######################################################################

# link: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

######################################################################

# Exemplo de uso : multiplot(p1, p8, p9,p10, cols=4)

multiplot(p1, p8, p9,p10, cols=4)

# Como a função "multiplot" somente 'imprime' os gráficos no display,
# seu resultado não é diretamente armazenável no R.
# Por exemplo, "suite <- multiplot(p1, p8, p9,p10, cols=4)" simplesmente imprime o gráfico 
# no display e gera uma variável nula que não o armazena.
suite <- multiplot(p1, p8, p9,p10, cols=4)
suite
# Se você quiser gravar o gráfico em pdf ou outros tipos de imagem, use o menu "Export" no RStudio


# Salvar o espaço de trabalho da parte 1 para reutilizá-lo na parte seguinte
# Para recarregar o 'workspace' utiliza-se o comando " load("meu_arquivo_da_Parte_1.RData")
save.image(file = "parte1.RData")

