# Adicionar cor usando uma terceira variável num gráfico de dispersão
require(ggplot2)

gr_rhob_cor1 <- ggplot(perfil, aes(x=GR, y=DENSIDADE, color = NEUTRAO)) + 
  #   a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  #   agora, foi adicionada uma variável para controlar a cor de exibição dos pontos
  #   com "color = NEUTRAO"
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$DENSIDADE), expand = F) +
  #   os limites do gráfico são explicitados para os limites dos dados
  # geom_point(colour = "blue", size = 0.4)
  geom_point()

gr_rhob_cor1

# A escala de cores padrão não é adequada para o alcance da variável NEUTRAO
# como podemos verificar com summary()
summary(perfil$NEUTRAO)
# ou com a função range() que fornece apenas os valores extremos da variável.
range(na.omit(perfil$NEUTRAO))
# Observe que a função summary() automaticamente ignora os pontos não amostrados (NA)
# mas a função range() exige que o código imponha que NA seja ignorado com na.omit()

# Podemos criar escalas de com simplicidade, atribuindo, por exemplo, 3 cores que servirão para gerar um gradiente.
# Veja três exemplos e escolha um deles ou crie o seu próprio.
# Fonte: https://biostats.w.uib.no/color-scale-for-continuous-variables/

gr_rhob_cor1 + scale_color_gradient2(low = "yellow", mid = "darkblue", high = "red", midpoint = mean(na.omit(perfil$NEUTRAO)))

gr_rhob_cor1 + scale_color_gradient2(low = "darkblue", mid = "yellow", high = "red", midpoint = mean(na.omit(perfil$NEUTRAO)))
        
gr_rhob_cor1 + scale_color_gradient2(low = "green", mid = "darkblue", high = "red", midpoint = mean(na.omit(perfil$NEUTRAO)))
