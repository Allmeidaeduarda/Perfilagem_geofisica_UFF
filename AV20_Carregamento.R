# 	Importar dados do Excel
# Inicialmente, carregamos a biblioteca que pode abrir o formato binário do Excel e
# transformar numa matriz de dados do R

library(readxl)

Dados_para_Avaliacao <- read_excel("Dados_para_Avaliacao_2020.xlsx")
# View(Dados_para_Avaliacao)

# 	Renomear dataframe para tornar mais simples a digitação
perfil <- Dados_para_Avaliacao

#   Na verdade, os dados poderia ser carregados diretamente com
#   perfil <- read_excel("Dados_para_Avaliacao_2020.xlsx")
#   mas preferi deixar uma cópia do dados original para qualquer eventualidade...

#   Muitas das operações feitas com o R & RStudio podem ser replicadas no Excel
#   porém sem a flexibilidade e facilidade que temos aqui.

# Verificando o conjunto de dados carregado

# Visualizar como um planilha tabulada
View(perfil)

# As dimensões da matriz carregada
dim (perfil)
# Como se vê no console, é uma matriz com 776 linhas e 12 colunas

# A estrutura da matriz
str(perfil)

# A matriz de dados (denominada "tibble" pode conter valores numéricos, operadores lógicos
# e texto). É um formato próprio do R para agilizar as operações matriciais.
# Nas colunas, neste caso, temos as variáveis listadas.
# Nas operações que usam tais variáveis elas serão referidas pelo nome da matriz, um separador ($)
# e o nome da variável.
# A matriz de dados ("tibble") chama-se "perfil" e contém 12 variáveis com nomes abreviados
# autoexplicativos como "prof_m" e "prof_ft" (profundidade em metros e pés), "RESPROF", "RESMED" 
# e "RESRAS" (resistividade profunda ou longa, média e rasa ou curta).
# Todas as variáveis contidas em "perfil" serão referidas, por exemplo, como:
# perfil$GR (é a variável Raios Gama contida na matriz "perfil")


# Resumo estatístico dos dados
summary(perfil)

# Resumo estatístico de uma das variáveis
summary(perfil$RESPROF)

# 	Substituir os valores nulos (-999.25) pela constante lógica "NA" que indica 'dados ausentes'
perfil[perfil==-999.25] <-NA

# Resumo estatístico dos Dados

summary(perfil)
summary (perfil$RESPROF)

# Gravar o espaço de trabalho
# Se quiser dar uma pausa no trabalho, grave as variáveis num arquivo especial que poderá ser
# recarregado na sua próxima sessão ou "script".

save.image(file = "AV_2020_carregamento.RData")

# Todas as fases podem ser feitas também no MS-Excel (software proprietário) 
# ou também em softwares-livres como o LibreOffice ou o WPS
