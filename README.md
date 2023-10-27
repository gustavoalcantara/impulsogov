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
Diante disso, qual o comportamento dos acessos para as áreas logadas? Quais páginas podem ter tido mais acesso ao longo do tempo? Será que elas são acessadas da mesma forma? E o tempo médio de duração nas páginas? Tentando responder isso, elaborei o gráfico a seguir com a média do tempo de acesso por data para cada página_url. Sendo assim é possível observar que a página `/lista_nominal_gestantes` é a que tinha mais acesso inicialmente e ao passar do tempo a `lista_nominal_hipertensos` teve maior participação. A `trilha_capacitacao` foi a que teve pico de participação em meados da metade do mês de março. Isso pode coincidir com o gráfico anteriormente apresentado no contexto dos acessos à plataforma. 
![grafico_2](https://github.com/gustavoalcantara/impulsogov/assets/26544494/e4de6f6f-ae04-474c-9a14-af991a2cc85e)
Código:
```r
base_inner|>
  dplyr::group_by(pagina_url,
              	data_acesso)|>
  dplyr::summarise(tempo_medio =
               	mean(sessao_duracao))|>
  ggplot(aes(x = data_acesso, y = tempo_medio, color = pagina_url)) +
  geom_line(size = 1.5) +
  labs(x = "Data de acesso", y = "Tempo de Acesso",
   	title = "Tempo por página ao longo do tempo") +
  theme(legend.text = element_text(size = 12),  
    	legend.title = element_text(size = 14),
    	axis.title.x = element_text(size = 14),
    	axis.title.y = element_text(size = 14),
    	plot.title = element_text(size = 16))
```
É perceptível, portanto, que algumas páginas são mais acessadas que outras. Diante disso, vale refleti se os usuários antigos ou novos podem ter diferenças nos acessos. Para isso, vamos para o tópico 2.2 que foi definido como A dualidade dos Acessos.

#### 2.2 A Dualidade dos Acessos
Para melhor categorizar os usuários como `antigo` ou `novo`, optei por realizar uma análise exploratória da quantidade de usuários criados ao longo do tempo. É possível observar que a partir da segunda quinzena de março, novos usuários são cadastrados na plataforma e o platô pretérito à este periodo começa um movimento de ascensão. Portanto, categorizei que usuários criados anteriormente à 3/3 são `antigo ` e posterior à isso `novo`. A linha tracejada apresenta essa categorização a partir da data. 
![grafico_3](https://github.com/gustavoalcantara/impulsogov/assets/26544494/020dd73e-49f1-4b7d-aadd-f608c5485758)

Assim, foi possível verificar quais usuários podem interagir mais com a plataforma através da variável `eventos`. Para isso, categorizei os usuários e somei as interações. Chama a atenção que usuários `novos` interagem mais com a `trilha de capacitação` do que usuários `antigos`.
![grafico_4](https://github.com/gustavoalcantara/impulsogov/assets/26544494/a4c654b9-a9f1-464e-a18d-bc405cf1e31c)
Código:
```r
base_inner |>
  dplyr::mutate(tipo_usuario =
                  ifelse(criacao_data < "2023-03-15", 
                         "antigo", "novo"))|>
  dplyr::group_by(tipo_usuario, pagina_url)|>
  dplyr::summarise(interacoes = 
                     sum(eventos))|>
  ggplot(aes(x = tipo_usuario, y = interacoes, fill = pagina_url)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  theme_minimal() +
  labs(x = "Tipo de Usuário", y = "Interações", fill = "URL da Página") +
  ggtitle("Interações por Página em Usuários Antigos e Novos")+
  theme(legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 16))
```
Feito isso, é possível verificar se o cargo das pessoas pode influenciar neste contexto. Para isso, fiz o agrupamento a partir da variavel `cargo` e  `tipo_usuario` e verifiquei o tempo médio de cada um destes paga cada `pagina_url`. Acontece que na categorização que fiz, existem os cargos de `Coordenação de Equipe` e `Coordenação APS` classificados como antigos. Os demais são todos usuários novos. Porém, cabe alguns insights.  Vejamos os resultados:
![grafico_5](https://github.com/gustavoalcantara/impulsogov/assets/26544494/a0ed94fc-c587-4c35-9714-fbf7aa39f451)
Código
```r
base_inner|>
  dplyr::mutate(cargo = stringr::str_trim(cargo))|>
  dplyr::mutate(cargo = ifelse(cargo %in% c("Técnica (o) de Enfermagem",
                                        	"Técnico(a) de Enfermagem"),
                           	"Técnico(a) de Enfermagem", cargo))|>
  dplyr::group_by(cargo, tipo_usuario, pagina_url)|>
  dplyr::summarise(tempo_sessao=
                 	mean(sessao_duracao))|>
  ggplot(aes(x = pagina_url, y = tempo_sessao, fill = tipo_usuario)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ cargo, scales = "free_y", ncol = 2) +
  labs(x = "Página URL", y = "Tempo Médio de Sessão", fill = "Tipo de Usuário") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Tempo Médio de Sessão por Página, Cargo e Tipo de Usuário")
```
É interessante verificar que enfermeiros, médicos e técnicos de enfermagem acessam mais a página de `início` e a `trilha de capacitação`. Os cargos de coordenação foram os que acessam as demais páginas e existem diferenças no tempo médio da sessão para os usuários antigos no cargo de `Coordenação de Equipe` praticamente para todas as páginas. 

## Considerações
Este case é fruto de um esforço de analisar as interações das pessoas em uma plataforma. É válido ressaltar que todas as análises aqui são exploratórias e podem indicar padrões de acesso e interação. Entretanto, para uma análise mais acurada é interessante analisar sob a ótica de séries temporais e modelos preditivos. É válido também levar em consideração a localidade das pessoas que acessam esta plataforma. Será que existe um padrão espacial de acesso? Será que existem UFS com maior dependência de outros fatores externos à plataforma? É válido ressaltar que os dados foram resultados de um join realizado no começo das análises. Para os demais usuários que não entraram nessa base, é interessante realizar uma análise especificamente deles. 
Deixo aqui minhas contribuições e futuros contatos caso haja interesse. 
Muito obrigado!












