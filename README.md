# impulsogov
## Case de Analytics para a vaga de Analista de Dados Sênior da [impulsogov.org](https://www.impulsogov.org/)

Esse case faz parte do processo seletivo da Impulso Gov para o cargo de Analista de Dados Sênior. Os códigos podem ser acessados aqui. Todos os códigos e visualizações aqui apresentadas foram feitas no Rstudio utilizando o [Tidyverse](https://www.tidyverse.org/). Essa escolha se dá pela facilidade que o R têm de integrar pacotes de análise de dados e roda-los localmente. Porém, reitero que é possível verificar em meu [portfolio](https://github.com/gustavoalcantara/data-portfolio) as outras linguagens (SQL, Python) que utilizo cotidianamente para realizar minhas atividades de analista de dados. 

### Passo 1 - Download e Compreensões das variáveis
Tendo feito o download dos arquivos em.csv localmente, foi possível obter algumas ideias de cada uma das variáveis. 
A sessao_id é cada login realizado a partir do id_usuario. Portanto, essa variável é única e identifica o login através do ‘usuario_id’. 
Existem 450 usuários na base ‘registro_de_eventos_ip.csv’ e 488 na base de ‘usuario_cadastrados_ip.csv’. Portanto, pode ocorrer, futuramente realizando um Inner Join entre as bases, de alguns usuários não serem contemplados em ambas as bases. Porém, para resolver isso, vou utilizar o Full Outter Join das bases. Assim, a análise contemplará duas bases: Uma com o inner join realizado (que chamei de x) e outra com o Full Outer Join que chamei de y).  
O formato da data está em YYYY-MM-DD-HH. 
Existem 6 páginas de acessps diferentes para o site que está na variável ‘pagina_url.  

Os códigos de tratamento para essas observações foram:
```r
#Registros de Eventos - IP
x <- read.csv('registro_de_eventos_ip.csv')
x|>
  dplyr::glimpse()
x|>
 dplyr::distinct(variaveis)
```
``` r
#Usuários Cadastrados
y <- read.csv('usuarios_cadastrados_ip.csv',
          	encoding = 'UTF-8')
y|>
  dplyr::glimpse()
y|>
 dplyr::distinct(variaveis)
```
### Passo 2 - A Análise do Sobrevoo 

Sempre gosto inicialmente de realizar uma prévia análise da dinâmica daquilo que estudo. Por serem bases que se contemplam, optei por realizar um INNER JOIN entre as bases realizar um pequeno processo de criação de novas variáveis que me subsidiarão futuramente:
```r
x|>
  dplyr::inner_join(y, #Join de ambas as bases
                	by =c('usuario_id' =
                  	'id_usuario'))|>
  dplyr::mutate(data_acesso = #criacao da variavel de data
              	substr(periodo_data_hora, 1, 8),
            	hora_acesso = #criacao da variavel de horas
              	substr(periodo_data_hora, 9, 10)) |>
  dplyr::mutate(uf = stringr::str_extract(municipio, "- [A-Z]{2}$")) |> #obtenção da UF
  dplyr::mutate(uf = stringr::str_replace(uf, "- ", "")) |>
  dplyr::mutate(data_acesso = #Atribuição da data de acesso como data
              	as.Date(data_acesso, format = '%Y%m%d')) ->base_inner
```
#### 2.1 - Qual o comportamento de acessos ao longo do tempo? 
Essa questão é básica para entender se a plataforma vêm sendo acessada e a dinâmica dos seus acessos. Para isso, elaborei o gráfico abaixo que demonstra que a partir da metade do mês de março a quantidade de acessos aumenta significativamente e segue essa tendência nos meses posteriores. Optei por fazer de forma acumulada porque existem padrões de acesso por dias da semana. 
![grafico_1](https://github.com/gustavoalcantara/impulsogov/assets/26544494/dccbe760-d3d8-4118-9699-c760e7697d32)

Código:
```r
base_inner |>
  dplyr::group_by(data_acesso)|>
  dplyr::summarise(acessos = dplyr::n())|>
  dplyr::mutate(casos_acumulados =
              	cumsum(acessos))|>
  ggplot(aes(x=data_acesso, y=casos_acumulados))+ #gráfico da quantidade de acesso por data
	geom_line()+
	labs(x = "Data de acesso",
     	y = "Quantidade de acessos",
     	title = "Acessos ao longo do tempo") +
	theme(axis.text.x = element_text(size = 14),
    	axis.text.y = element_text(size = 14),
    	title = element_text(size = 16),
    	axis.title.x = element_text(size = 16),
    	axis.title.y = element_text(size = 16))
```
Diante disso, qual o comportamento dos acessos para as áreas logadas? Quais páginas podem ter tido mais acesso?
