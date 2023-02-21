# Recarregar as variáveis do espaço de trabalho da fase inicial
load (file = "AV_2020_carregamento.RData")

# 	carregar bilbliotecas "ggplot2" e "plotly"

#   O R é baseado em bibliotecas desenvolvidas pela Comunidade e que executam de modo
#   transparente e simples muitas funções que podem ser mais complexas usando apenas
#   as funcionalidades básicas da linguaguem.
#   O GGPLOT2 é uma biblioteca especializada para gráficos que permite a execução tanto
#   de simples gráficos de linha ou pontos como a superposição de operadores estatísticos
#   importantes para análise e com um acabamento profissional.
#   A biblioteca PLOTLY permite transformar muitos desses gráficos em verões interativas
#   que facilitam a interpretação, visualização e apresentação de suas análises.

require(ggplot2)
library(plotly)

# 	Raios Gama com todos os pontos unidos por linhas
#   Os gráficos serão apenas numerados sequencialmente para facilitar seu uso posterior
#   p1 = "plot 1"

p1 <- ggplot(perfil, aes(x=prof_m, y=GR)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$prof_m), expand = F) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "black", size = 0.4) +
  # 	define-se como gráfico de linha de cor azul
  scale_x_reverse() +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  coord_flip()+
  # 	gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
  
  ggtitle("Raios Gama (API)")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
  #   centraliza o título ao longo do topo

# O gráfico foi armazenado na variável "p1"
# Se invocarmos p1, teremos o gráfico na janela "Plots" no quadro inferior direito do RStudio
p1

# Usando o menu "Export" da aba de "Plots", o gráfico estático pode ser gravado como imagem
# ou PF, ajustando as suas dimensões manualmente.
# Ele também pode ser visto numa janela de "zoom" que também pode ser manipulada

# 	USANDO O PLOTLY para obter gráfico dinâmico interativo
pp1 <- ggplotly(p1)
pp1

# A exportação de um gráfico interativo é mais complexa e não cabe neste exercício
# Veremos depois que o "script" pode ser gravado com todos os seus comando, comentários e
# gráficos de um modo bastante simples para um arquivo HTML.


# Resumo Estatístico do Raio Gama

summary(perfil$GR)

# Gerando um histograma para analisar GR

GR_histo50 <-ggplot(perfil, aes(x=GR)) + 
  geom_histogram(aes(y=..density..), bins=50, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(GR))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total (50 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

GR_histo50

GR_histo20 <-ggplot(perfil, aes(x=GR)) + 
  geom_histogram(aes(y=..density..), bins=20, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(GR))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total (20 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

GR_histo20

GR_histo100 <-ggplot(perfil, aes(x=GR)) + 
  geom_histogram(aes(y=..density..), bins=100, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(GR))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total (100 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

GR_histo100

GR_histo200 <-ggplot(perfil, aes(x=GR)) + 
  geom_histogram(aes(y=..density..), bins=200, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(GR))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total (200 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo

GR_histo200

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

# Juntando os três gráficos em uma página

multiplot(GR_histo50, GR_histo20, GR_histo100, GR_histo200, cols=2)

# Usando o histograma com 50 classes (ou "bins"), podemos sobrepor a curva
# empírica da função de densidade de probabilidade ( em inglês, EPDF - Empirical
# Probability Density Function usando diversas janelas com kernel gaussiano.

GR_histo50 + geom_density(alpha = 0.2, bw = 1)

multiplot (GR_histo50 + geom_density(alpha = 0.2, bw = 1)+ ggtitle("Histograma - Raio Gama Total (50 bins, w=1)"),
           GR_histo50 + geom_density(alpha = 0.2, bw = 2)+ ggtitle("Histograma - Raio Gama Total (50 bins, w=2)"), 
           GR_histo50 + geom_density(alpha = 0.2, bw = 3)+ ggtitle("Histograma - Raio Gama Total (50 bins, w=3)"), 
           GR_histo50 + geom_density(alpha = 0.2, bw = 4)+ ggtitle("Histograma - Raio Gama Total (50 bins, w=4)"), 
           cols=2)
# 	Classificando ARN/FLH com corte simples de Raios Gama. Escolhendo valor de corte
#	no gráfico interativo gerado no ggplot2 + Plotly

#	Adota-se valor de corte = 70 API
# Na análise preliminar dos histogramas, 70 API parece um bom valor de corte
valor_de_corte <- 70

#################

## GERE OS HISTOGRAMAS PAR OUTRAS VARIÁVEIS USANDO "CÓPIA, COLAGEM, EDICÃO
## PARA ISSO, O NOTEPAD++ PODE SER MUITO PRÁTICO.

# 	Criar novas colunas para armazenar "ARN" e "FLH"
#   Em matrizes pequenas como esta aqui, não há grande problema em não reservar um espaço pré-alocado
#   mas é sempre uma boa prática poi isso demanda muito mais processamento em matrizes grandes.
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
   if(Dados_para_Avaliacao$GR[i] > valor_de_corte ) {
    perfil$FLH [i] <- perfil$GR[i]
  }
  else {
      if(Dados_para_Avaliacao$GR[i] > 0){
	  perfil$ARN[i]<- perfil$GR[i]
      }}
}

# Note que para essa operação lógica estou usando o data frame original com nulos (-999.25).
# Se usarmos o dataframe "perfil" onde substituímos o nulo por "NA", haverá uma mensagem de erro
# pois algumas linhas não têm dados e não podem ser usadas para o teste lógico.
# Não há nenhum efeito prático nos resultados porém a função de compilar do RStudio não rodará.

# Entretanto, o armazenamento se dá no data frame "perfil" pois as células com "NA" são 'puladas'
# quando o gráfico é feito

# p2 <- ggplot(subset(perfil, perfil$ARN != "NA"), aes(x=prof_m, y=GR)) + 
#   # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
#   # o comando 'subset' gera uma matriz na memória a partir do data frame "perfil"
#   # "perfil" é o primeiro argumento no comando e define o data frame analisado
#   # a condição perfil$ARN diferente de "NA" separa todos os pontos classificados
#   # como ARN e plota a linha correspondente da coluna "GR"
#   theme_linedraw() +
#   # aplica um tema para o gráfico sem plano de fundo, somente com linhas
#   coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$prof_m), expand = F) +
#   # os limites do gráfico são explicitados para os limites dos dados
#   geom_point(colour = "blue") +
#   # define-se como gráfico de pontos de cor azul
#   scale_x_reverse() +
#   # a escala de "Depth" é revertida (menor no alto, maior na base)
#   coord_flip()+
# # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
#   
#   ggtitle("Raios Gama - Arenitos")+
#   #   adiciona um título no topo do gráfico
#   theme(plot.title = element_text(hjust=0.5))
#   #   centraliza o título ao longo do topo
# p2
# 
# # O gráfico gerado só tem pontos...
# # Vamos aproveitar para ver como funciona a biblioteca 'plotly' neste caso.
# 
# # USANDO O PLOTLY
# pp2 <- ggplotly(p2)
# pp2

# O resumo estatístico da variável ARN
summary(perfil$ARN)

# O histograma da nova variável

ARN_histo50 <-ggplot(perfil, aes(x=ARN)) + 
  geom_histogram(aes(y=..density..), bins=50, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(ARN))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total dos Arenitos (50 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
ARN_histo50

# Um painel com a curva EPDF sobreposta

multiplot (ARN_histo50 + geom_density(alpha = 0.2, bw = 1)+ ggtitle("Histograma - Raio Gama Total dos Arenitos (50 bins, w=1)"),
           ARN_histo50 + geom_density(alpha = 0.2, bw = 2)+ ggtitle("Histograma - Raio Gama Total dos Arenitos (50 bins, w=2)"), 
           ARN_histo50 + geom_density(alpha = 0.2, bw = 3)+ ggtitle("Histograma - Raio Gama Total dos Arenitos (50 bins, w=3)"), 
           ARN_histo50 + geom_density(alpha = 0.2, bw = 4)+ ggtitle("Histograma - Raio Gama Total dos Arenitos (50 bins, w=4)"), 
           cols=2)
# Analisemos os Folhelhos com este corte de 70 API

# O resumo estatístico da variável ARN
summary(perfil$FLH)

# O histograma da nova variável

FLH_histo50 <-ggplot(perfil, aes(x=FLH)) + 
  geom_histogram(aes(y=..density..), bins=50, color="green", fill="chartreuse") +
  geom_vline(aes(xintercept=mean(na.omit(FLH))),
             color="blue", linetype="dashed", size=1) +
  scale_x_continuous(name = "Raio Gama"~(API)) + 
  scale_y_continuous(name = "Probabilidade") +
  ggtitle("Histograma - Raio Gama Total dos Folhelhos (50 bins)") +
  #adiciona um fundo branco ao gráfico
  theme_linedraw() +   
  #   adiciona um título no topo do gráfico - centralizado
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
FLH_histo50

# Um painel com a curva EPDF sobreposta

multiplot (FLH_histo50 + geom_density(alpha = 0.2, bw = 1)+ ggtitle("Histograma - Raio Gama Total dos Folhelhos (50 bins, w=1)"),
           FLH_histo50 + geom_density(alpha = 0.2, bw = 2)+ ggtitle("Histograma - Raio Gama Total dos Folhelhos (50 bins, w=2)"), 
           FLH_histo50 + geom_density(alpha = 0.2, bw = 3)+ ggtitle("Histograma - Raio Gama Total dos Folhelhos (50 bins, w=3)"), 
           FLH_histo50 + geom_density(alpha = 0.2, bw = 4)+ ggtitle("Histograma - Raio Gama Total dos Folhelhos (50 bins, w=4)"), 
           cols=2)
