# Recarregar as variáveis do espaço de trabalho da Parte 1
load (file = "parte1.RData")

# Só para verificar que está carregado, vamos reexibir o perfil DRDN gerado no final na parte anterior
p10

# Note que a bibliotecas que foram carregadas com "library()" ou "require()" não estão no workspace
# A função "multiplot", também definida na parte inicial, também deve ser carregada novamente se
# for necessário usá-la.

# Vamos, então, repor o 'workspace' ao estado anterior,
# carregando as bibliotecas:
require(ggplot2)
library(plotly)

# rodando novamente o código da função "multiplot"

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



###########################################################################################
#                                                                                         #
# AGORA, O ESPAÇO DE TRABALHO FOI TOTALMENTE RESTAURADO E PODEMOS TRABALHAR MAIS UM POUCO #
#                                                                                         #
###########################################################################################

# Podemos examinar rapidamente as primeiras linhas do data frame de trabalho:
head(perfil)
# ou ver um resumo estatístico do data frame "perfil"
summary(perfil)

# Vamos fazer alguns cross-plots.

GR_Rho1 <- ggplot(perfil, aes(x=GR, y=DENSIDADE))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  #geom_point(aes(colour = NEUTRAO)) +          #Define a coluna que será usada para a cor do ponto
  #scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
  #                       midpoint = mean(perfil$NEUTRAO, na.rm = TRUE), name = "PhiN")+
  #Define um gradiente de cores
  scale_x_continuous(name ="Gama (API)") +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  scale_y_continuous(name="Densidade (g/cc") +
  #		o nome do parâmetro é usado para identificar o eixo da ordenada
  theme_linedraw() +
  #Define um gráfico simples sem cor de fundo
  ggtitle("Gama x Densidade")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
GR_Rho1
GR_Rho <- GR_Rho1 + geom_point(aes(colour = NEUTRAO)) + 
        scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
      midpoint = mean(perfil$NEUTRAO, na.rm = TRUE), name = "PhiN")
GR_Rho

GR_Phi1 <- ggplot(perfil, aes(x=GR, y=NEUTRAO))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  #geom_point(aes(colour = DENSIDADE)) +          #Define a coluna que será usada para a cor do ponto
  #scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
  #                       midpoint = mean(perfil$DENSIDADE, na.rm = TRUE), name = "Densidade")+
  #Define um gradiente de cores
  scale_x_continuous(name ="Gama (API)") +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  scale_y_continuous(name="Porosidade-neutrão (%)") +
  #		o nome do parâmetro é usado para identificar o eixo da ordenada
  theme_linedraw() +
  #Define um gráfico simples sem cor de fundo
  ggtitle("Gama x Porosidade-neutrão")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
GR_Phi1
GR_Phi <- GR_Phi1 + geom_point(aes(colour = DENSIDADE)) + 
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(perfil$DENSIDADE, na.rm = TRUE), name = "Densidade")
GR_Phi

GR_Son1 <- ggplot(perfil, aes(x=GR, y=SONICO))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  #geom_point(aes(colour = DENSIDADE)) +          #Define a coluna que será usada para a cor do ponto
  #scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
  #                       midpoint = mean(perfil$DENSIDADE, na.rm = TRUE), name = "Densidade")+
  #Define um gradiente de cores
  scale_x_continuous(name ="Gama (API)") +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  scale_y_continuous(name="dT sônico (us/ft)") +
  #		o nome do parâmetro é usado para identificar o eixo da ordenada
  theme_linedraw() +
  #Define um gráfico simples sem cor de fundo
  ggtitle("Gama x Sônico")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
GR_Son1 
GR_Son <- GR_Son1 + geom_point(aes(colour = DENSIDADE)) + 
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(perfil$DENSIDADE, na.rm = TRUE), name = "Densidade")
GR_Son
ggplotly(GR_Son)
multiplot (GR_Rho, GR_Phi, GR_Son, cols = 3)


RhodT1 <-  ggplot(perfil, aes(x=DENSIDADE, y=SONICO))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  #geom_point(aes(colour = DENSIDADE)) +          #Define a coluna que será usada para a cor do ponto
  #scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
  #                       midpoint = mean(perfil$DENSIDADE, na.rm = TRUE), name = "Densidade")+
  #Define um gradiente de cores
  scale_x_continuous(name ="Densidade(g/cc))") +
  # 	a escala de "Depth" é revertida (menor no alto, maior na base)
  scale_y_continuous(name="dT sônico (us/ft)") +
  #		o nome do parâmetro é usado para identificar o eixo da ordenada
  theme_linedraw() +
  #Define um gráfico simples sem cor de fundo
  ggtitle("Densidade x dT")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
RhodT1 
RhodT <- RhodT1 + geom_point(aes(colour = GR)) + 
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(na.omit(perfil$GR)), name = "Gama")
RhodT

# Como você pode ver, ainda não fizemos nenhum cálculo de porosidade e temos RhoB e dT medidos
# Inicialmente, vamos calcular Phi_rho e Phi_sonico para toda a extensão do perfil

# Com o perfil densidade podemos usar a fórmula simples com Rho_matriz arenito e 
# Rho_fluido água.
#         Phi_rho = (Rho_matriz - Rho_lido)/(Rho_matriz - Rho_fluido)
# Inicializaremos a densidade da matriz e do fluido e a coluna para armazenar Phi_rho.
Rho_matriz <- 2.65
Rho_fluido <- 1
perfil$Phi_rho <- as.numeric("NA")

# A cada ponto medido, teremos perfil$DENSIDADE no data frame
perfil$Phi_rho <- ((Rho_matriz - perfil$DENSIDADE) / (Rho_matriz - Rho_fluido))

# Vamos repetir o processo com o perfil sônico
#         Phi_sonico = (dT_medido - dT_matriz) / (dT_fluido - dT_matriz)
# Como são arenitos consolidados da base do Terciário, 
# dT_matriz é o padrão usual de 55.5 us/ft
# e o fluido é água salgada com 189 us/ft

# Inicialização
dT_matriz <- 55.5
dT_fluido <- 189
perfil$Phi_sonico <- as.numeric("NA")

#Cálculo
perfil$Phi_sonico <- ((perfil$SONICO - dT_matriz)/(dT_fluido - dT_matriz))

# Comparando os resultados num gráfico de dispersão ('cross plot')
xplot1 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_sonico))+  #Inicializa definindo aes() com as variáveis em x e y
          geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
                                                      #forma e tamanho dos pontos
          geom_point(aes(colour = ARN120)) +          #Define a coluna que será usada para a cor do ponto
          scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                                  midpoint = mean(perfil$ARN120, na.rm = TRUE))+
                                                      #Define um gradiente de cores
          theme_linedraw()                            #Define um gráfico simples sem cor de fundo
xplot1

# USANDO O PLOTLY
pxplot1 <- ggplotly(xplot1)
pxplot1

# Adicionar a linha diagonal de correlação (1,1)
xplot1 + geom_abline(intercept = 0, colour = "red")

# USANDO O PLOTLY com a linha diagonal
pxplot1 <- ggplotly(xplot1 + geom_abline(intercept = 0, colour = "red"))
pxplot1

# Para guardar xplot1 completo com a linha diagonal:
xplot1 <- xplot1 + geom_abline(intercept = 0, colour = "red")

# Aproveitando este pedaço de código, vamos fazer variações do gráfico de dispersão
# Com as mesmas variáveis do xplot1, vamos mudar o parâmetro de cor para PhiN
xplot2 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_sonico))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  geom_point(aes(colour = NEUTRAO)) +          #Define a coluna que será usada para a cor do ponto
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(perfil$NEUTRAO, na.rm = TRUE))+
  #Define um gradiente de cores
  theme_linedraw()+                            #Define um gráfico simples sem cor de fundo
  geom_abline(intercept = 0, colour = "red")
xplot2

# USANDO O PLOTLY
pxplot2 <- ggplotly(xplot2)
pxplot2

# Novo gráfico com resistividade como variável para cor
xplot3 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_sonico))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  geom_point(aes(colour = RESPROF)) +          #Define a coluna que será usada para a cor do ponto
  #scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
  #                       midpoint = mean(perfil$NEUTRAO, na.rm = TRUE))+
  #Define um gradiente de cores
  theme_linedraw()+                            #Define um gráfico simples sem cor de fundo
  geom_abline(intercept = 0, colour = "red")
xplot3

# Como se vê, a escala de cor não é muito boa pois não consideramos a tendência exponencial da resistividade
# Vamos gerar um novo dataframe com as resistividades log-transformadas para usar na escala de cor
str(perfil) # Rever a estrutura do data frame "perfil"

# Para a escala de cores baseada no log10 da resistividade, separamos inicialmente a coluna com profundidade
# (prof_m) - coluna 1 e as colunas com resistividade - colunas de 5 a 7 (5:7)
# O comando "as.data,frame()" define que as colunas serão replicadas mantendo a mesma estrutura.
res_cores <- as.data.frame(perfil[c(1,5:7)])
# Na sequencia, armazenas os valores de resistividade log-transformados sobrescritos no data frame criado.
res_cores[2] <- a <- log10(res_cores$RESPROF)
res_cores[3] <- a <- log10(res_cores$RESMED)
res_cores[4] <- a <- log10(res_cores$RESRASA)

# Repetindo xplot3 com nova escala de cor
xplot3 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_sonico))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  geom_point(aes(colour = res_cores$RESPROF)) +          #Define a coluna que será usada para a cor do ponto
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(res_cores$RESPROF, na.rm = TRUE))+
  #Define um gradiente de cores
  theme_linedraw()+                            #Define um gráfico simples sem cor de fundo
  geom_abline(intercept = 0, colour = "red")
xplot3

# USANDO O PLOTLY
pxplot3 <- ggplotly(xplot3)
pxplot3

# Um novo painel para comparar
painel7 <- subplot(pxplot1, pxplot2, pxplot3)
painel7

# Uma nova suite para impressão com a função "multiplot"
suite2 <- multiplot(xplot1, xplot2,xplot3, cols=3)
suite2

# Como há muitos pontos sobrepostos, é difícil avaliar a densidade dos 'clusters' ou aglomerações.
# A função "geom_density_2d()" sobrepõe um mapa de contorno com as densidades dos 'clusters'.
# Esta função é uma composição da função densidade de probabilidade ao longo dos dois eixos.
# Densidade de Phi_rho, porosidade a partir do perfil densidade, e de Phi_sonico, a partir do sonico:
multiplot(ggplot(perfil, aes(x=Phi_sonico)) + geom_density()+coord_flip(),
          ggplot(perfil, aes(x=Phi_rho)) + geom_density())
# O resultado em 2D
xplot3 + geom_density_2d()

# O último gráfico pode ser gravado num pdf, A4, paisagem 
ggsave("phi_rho_sonico_resprof.pdf", width = 11.6925, height = 8.267)

# Outra relação importante é aquela entre o Phi_rho e o Phi_N
# Como a medida do perfil foi feita com a calibração para calcário,
# Phi_N é corrigido em +4% para termos o valor na matriz arenito.

# Criar nova coluna em "perfil" para armazenar "Phi_N"
perfil$Phi_N <- as.numeric("NA")

# Fazer a transformação
 perfil$Phi_N <- ((perfil$NEUTRAO + 4)/100)
# perfil$Phi_N <- ((perfil$NEUTRAO)/100) # Você pode refazer o plot sem a correção de matriz e analisar.
# Basta retirar o "#" de comentário da linha acima
 
# Repetindo o gráfico de dispersão ('cross plot') com Phi_rho e Phi_N
xplot4 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_N))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  geom_point(aes(colour = ARN120)) +          #Define a coluna que será usada para a cor do ponto
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(perfil$ARN120, na.rm = TRUE))+
  #Define um gradiente de cores
  theme_linedraw()  +                         #Define um gráfico simples sem cor de fundo
  geom_abline(intercept = 0, colour = "red")
xplot4

# Repetindo, a função "geom_density_2d()".
xplot4 + geom_density_2d()

# Gravando o resultado num pdf, A4, paisagem 
ggsave("phi_rho_phi_n_ARN120.pdf", width = 11.6925, height = 8.267)

# Repetindo o gráfico de dispersão ('cross plot') com Phi_rho e Phi_N, e profundidade na escala de cor
xplot5 <- ggplot(perfil, aes(x=Phi_rho, y=Phi_N))+  #Inicializa definindo aes() com as variáveis em x e y
  geom_point(shape = 21, size = 1)+           #Define o tipo de gráfico (dispersão com pontos, no caso),
  #forma e tamanho dos pontos
  geom_point(aes(colour = prof_m)) +          #Define a coluna que será usada para a cor do ponto
  scale_colour_gradient2(low = "darkblue", mid = "blue" , high = "red",
                         midpoint = mean(perfil$prof_m, na.rm = TRUE))+
  #Define um gradiente de cores
  theme_linedraw()  +                         #Define um gráfico simples sem cor de fundo
  geom_abline(intercept = 0, colour = "red")
xplot5

# Vamos retornar um pouco e examinar o 'painel4'
painel4
# Há três 'tracks': GR, GR com classe ARN (GR<120API) e RhoB/PhiN
# Como PhiN foi reescalado para mimetizar o modo convencional de representação,
# vamos gerar um 'plot' só com PhiN e remontar o painel para análise.

p11 <- ggplot(perfil, aes(x=prof_m, y=Phi_N)) + 
  # a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "DRDN" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$Phi_N), ylim = range(perfil$prof_m), expand = F) +
  # os limites do gráfico são explicitados para os limites dos dados
  geom_line(colour = "red", size = 0.4) +
  # define-se como gráfico de linha de cor verde ciano e espessura de linha ~1pt
  scale_x_reverse() +
  # a escala de "Depth" é revertida (menor no alto, maior na base)
  scale_y_reverse() +
  # a escala de "Phi_N" é revertida (menor à direita, maior à esquerda)
  coord_flip() +
  # gira-se o sistema de coordenadas ("Depth" agora está na ordenada)
    ggtitle("Phi_N")+
  #   adiciona um título no topo do gráfico
  theme(plot.title = element_text(hjust=0.5))
#   centraliza o título ao longo do topo
p11

# USANDO O PLOTLY
pp11 <- ggplotly(p11)
pp11

# Vamos montar um novo painel para analisar os resultados
painel5 <- subplot(pp8, pp7, pp11)
painel5


# Calcular Argilosidade a partir do perfil de porosidade-neutrão
# A fórmula base é: VSH_N = Phi_N_medido / Phi_N_Folhelho
# Determinar
# Criar nova coluna em "perfil" para armazenar "VSPhi_N"
perfil$VSH_N <- as.numeric("NA")

# Fazer a transformação
perfil$VSH_N <- ((perfil$Phi_N)/0.30)


# Gráficos de dispersão 3D interativo
# Usando sônico com cor (quarta dimensão)
p3d01 <- plot_ly(perfil, x = ~GR, y = ~DENSIDADE, z = ~NEUTRAO, color = ~SONICO, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gama'),
                      yaxis = list(title = 'Densidade'),
                      zaxis = list(title = 'Neutrao')))

p3d01

p3d01 <- plot_ly(perfil, x = ~GR, y = ~DENSIDADE, z = ~NEUTRAO, color = ~SONICO) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gama'),
                      yaxis = list(title = 'Densidade'),
                      zaxis = list(title = 'Neutrao')))

p3d01

p3d02 <- plot_ly(perfil, x = ~GR, y = ~DRDN, z = ~Phi_rho, color = ~SONICO) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gama'),
                      yaxis = list(title = 'DRDN'),
                      zaxis = list(title = 'Phi_rho')))

p3d02

# Usando resistividade profunda como cor
p3d03 <- plot_ly(perfil, x = ~GR, y = ~DRDN, z = ~Phi_rho, color = res_cores$RESPROF) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Gama'),
                      yaxis = list(title = 'DRDN'),
                      zaxis = list(title = 'Phi_rho')))

p3d03


save.image(file = "EX_2018_parte2.RData")















