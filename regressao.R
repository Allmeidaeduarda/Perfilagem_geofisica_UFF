# Adicionar reta de regressão num gráfico de dispersão
# Fonte: https://r-graph-gallery.com/50-51-52-scatter-plot-with-ggplot2.html
require(ggplot2)

gr_rhob_regressao <- ggplot(perfil, aes(x=GR, y=DENSIDADE)) + 
  #   a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$DENSIDADE), expand = F) +
  #   os limites do gráfico são explicitados para os limites dos dados
  geom_point(colour = "blue", size = 0.4)

gr_rhob_regressao

# Um modelo linear simples (regressão com mínimos quadrados) sem e com intervalo de confiança (se=FALSE ou se=TRUE).
# Com "method=lm", aplica-se um modelo generalizado de regressão
gr_rhob_regressao + geom_smooth(method="lm" , color="red", se=FALSE)
gr_rhob_regressao + geom_smooth(method="lm" , color="red", se=TRUE)

# Com "method=glm", aplica-se um modelo linear generalizado 
# (no caso, glm ou lm retornam a mesma curva)
gr_rhob_regressao + geom_smooth(method="glm" , color="red", se=TRUE)

# Com "method=gam", aplica-se um modelo aditivo generalizado
gr_rhob_regressao + geom_smooth(method="gam" , color="red", se=TRUE)

# Com "method=loes", aplica-se um modelo de regressão local suavizada
gr_rhob_regressao + geom_smooth(method="loess" , color="red", se=TRUE)

# Aplicando a regressão local suavizada, LOESS ou LOWESS, por trechos:

gr_rhob_regressao + geom_smooth(subset(perfil, GR <70), mapping = aes(method="loess" , color="red", se=TRUE))


gr_rhob_regressao + geom_smooth(subset(perfil, DRDN < -0.4), mapping = aes(method="loess" , color="red", se=TRUE))


gr_rhob_regressao + geom_smooth(subset(perfil, DRDN < -0.4), mapping = aes(method="loess" , color="red", se=TRUE)) +
            geom_smooth(subset(perfil, DRDN >= -0.4), mapping = aes(method="loess" , color="cyan", se=TRUE))

gr_rhob_regressao + geom_smooth(subset(perfil, GR <70), mapping = aes(method="loess" , color="red", se=TRUE)) +
  geom_smooth(subset(perfil, GR >= 70), mapping = aes(method="loess" , color="cyan", se=TRUE))

gr_rhob_regressao + geom_smooth(subset(perfil, DRDN < -0.4), mapping = aes(method="loess" , color="red", se=TRUE)) +
  geom_smooth(subset(perfil, (DRDN >= -0.4 & DRDN < 0)), mapping = aes(method="loess" , color="magenta", se=TRUE)) +
  geom_smooth(subset(perfil, DRDN >= 0), mapping = aes(method="loess" , color="cyan", se=TRUE))
