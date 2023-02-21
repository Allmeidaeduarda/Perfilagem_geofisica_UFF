## Rho-Phi crossing
# Definir função para escalar o eixo y
### Gerar plot para matriz calcario
### Usualmente, para matriz calcario aplica-se 1.95 a 2.95 g/cm3 para densidade e 0.45 a −0.15 para neutrao
### No Brasil, e mais comum se aplicar 2 a 3 g/cm3 para densidade
### Para matriz arenito, aplica-se 1.65 a 2.65 g/cm3 e 60 a 0%, respectivamente.
### A função é uma simples transformação de escala linear
### (45 - NPHI)/60 + M
### M é dado pela matriz: calcário (2 ou 1,95) ou arenito (1,65)
### Usaremos a escala comum no Brasil.
### Por implementar M variável
 require(ggplot2)

Crossing_plot <- ggplot() +
  theme_linedraw() +
  # Aplica tema para o grafico
  geom_line(perfil, mapping = aes(x = prof_m, y = DENSIDADE), color = "blue") +
  # Gráfico de linha com Profundidada x Densidade na cor azul
  geom_line(perfil, mapping = aes(x = prof_m, y = (45 - NEUTRAO)/60 + 2), color = "red") +
  # Gráfico de linha com Profundidade x Neutrao transformado para o cruzamentona cor vermelha
  # expand_limits(y=c(2,3)) +
  # ylim(c(-15, 40)) +
  # ylim(c (2,3)) +
  # 	os limites do gráfico são explicitados para os limites dos dados
  scale_x_continuous(name = "Profundidade (m)") +
  # Define escala da profundidade e nomeia o eixo
  scale_y_continuous(name = "Densidade (g/cc)",breaks=seq(2, 3, 0.1), sec.axis = sec_axis(~.*-60 + 60*2.7, name = "Porosidade Neutrão (%)")) +
  # Nomes nos eixos com retransformação da escala de Neutrao no eixo secundario
  scale_x_reverse() +
  # a escala de "COTA" é revertida (menor no alto, maior na base)
  coord_flip()
# gira-se o sistema de coordenadas ("COTA" agora está na ordenada)
Crossing_plot
