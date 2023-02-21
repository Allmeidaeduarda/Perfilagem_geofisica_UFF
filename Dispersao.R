# Gráficos de Dispersão ou "cross plots"

gr_rhob <- ggplot(perfil, aes(x=GR, y=DENSIDADE)) + 
  #   a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$DENSIDADE), expand = F) +
  #   os limites do gráfico são explicitados para os limites dos dados
  geom_point(colour = "blue", size = 0.4)

gr_rhob

# Usando as classes com um valor de corte de GR de 70 API para Arenitos

corte_70 <- 70

gr_rhob_ARN70 <- ggplot(subset(perfil, GR < corte_70), aes(x=GR, y=DENSIDADE)) + 
  #   a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$DENSIDADE), expand = F) +
  #   os limites do gráfico são explicitados para os limites dos dados
  geom_point(colour = "blue", size = 0.4)

gr_rhob_ARN70

# Usando as classes com um valor de corte de GR de 115 API para Arenitos

corte_115 <- 115

gr_rhob_ARN115 <- ggplot(subset(perfil, GR < corte_115), aes(x=GR, y=DENSIDADE)) + 
  #   a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  theme_linedraw() +
  # aplica um tema para o gráfico sem plano de fundo, somente com linhas
  coord_cartesian(xlim = range (perfil$GR), ylim = range(perfil$DENSIDADE), expand = F) +
  #   os limites do gráfico são explicitados para os limites dos dados
  geom_point(colour = "blue", size = 0.4)

gr_rhob_ARN115

# Histogramas 2D

gr_rhob_histo2D_60 <- ggplot(perfil, aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 60)
gr_rhob_histo2D_60

gr_rhob_histo2D_40 <- ggplot(perfil, aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 40)
gr_rhob_histo2D_40

gr_rhob_histo2D_30 <- ggplot(perfil, aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 30)
gr_rhob_histo2D_30

# Refazer o histograma 2D para Folhelho (GR >=115)

FLH_gr_rhob_histo2D_30 <- ggplot(subset(perfil, perfil$GR >= 115), aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 30)
FLH_gr_rhob_histo2D_30

# Refazer o histograma 2D para ArenitoFolhelho (GR < 115)

ARN_gr_rhob_histo2D_30 <- ggplot(subset(perfil, perfil$GR < 115), aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 30)
ARN_gr_rhob_histo2D_30

# Refazer o histograma 2D para Arenito com DRDN < -0.4

ARN_gr_rhob_histo2D_30_DRDN <- ggplot(subset(perfil, perfil$DRDN < -0.4), aes(x=GR, y=DENSIDADE)) + 
  # 	a tabela "perfil" é a base de dados, "prof_m" é a abcissa e "GR" é a ordenada
  geom_hex(bins = 30)
ARN_gr_rhob_histo2D_30_DRDN
