#Registro de Eventos
getwd()
x <- read.csv('registro_de_eventos_ip.csv')

x|>
  dplyr::glimpse()

#Cada sessao_id é um login. 
#Existem 450 usuários realizando o login. É possível realizar um Join com a de Users?
#Existem 6 caminhos diferentes para o site. Entender e interpretar-los é válido. 
#Eventos são cliques nas páginas.
# o que é sessão_duracao?

x|>
  dplyr::distinct(variaveis)


#Usuários Cadastrados
y <- read.csv('usuarios_cadastrados_ip.csv',
              encoding = 'UTF-8')

y|>
  dplyr::distinct(id_usuario)

#Na base de Registros, há 38 usuários a menos. O que fazer?

x|>
  dplyr::glimpse()

y|>
  dplyr::glimpse()

#quantos acessos em cada uma das páginas?
#qual o tempo médio em cada uma das páginas?
#usuários mais velhos continuam acessando?
#Quais cargos que possuem mais acessos?
#Qual UF teve mais acesso? Qual teve menos?
#Qual o horário de acesso?

library(ggplot2)

windows()

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
                  as.Date(data_acesso, format = '%Y%m%d')) -> base_inner

#Quantidade total de acessos acumulados ao longo do tempo
base_inner |>
  dplyr::group_by(data_acesso)|>
  dplyr::summarise(acessos = dplyr::n())|>
  dplyr::mutate(casos_acumulados = 
                  cumsum(acessos))|>
  ggplot(aes(x=data_acesso, y=casos_acumulados))+ #gráfico da quantidade de acesso por data
    geom_line()+
    labs(x = "Data de acesso", 
         y = "Quantidade de acessos acumulado", 
         title = "Acessos ao longo do tempo") +
    theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
  
  
#Quantidade total de acessos acumulados ao longo do tempo por página

base_inner |>
  dplyr::group_by(data_acesso, pagina_url)|>
  dplyr::summarise(acessos = dplyr::n())|>
  dplyr::group_by(pagina_url)|>
  dplyr::mutate(casos_acumulados = cumsum(acessos))|>
  ggplot(aes(x = data_acesso, y = casos_acumulados, color = pagina_url)) +
  geom_line(size = 1.5) +
  labs(x = "Data de acesso", y = "Quantidade de acessos", 
       title = "Acessos por página ao longo do tempo") +
  theme(legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 16))   


#Tempo médio despendido por página:
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


#Quantidade de usuário por datas
base_inner|>
  dplyr::glimpse()

base_inner|>
  dplyr::mutate(criacao_data = 
                  as.Date(criacao_data, format ='%Y-%m-%d')) |>
  dplyr::group_by(criacao_data)|>
  dplyr::summarise(
    quantidade_usuarios = 
      dplyr::n_distinct(usuario_id))|>
  dplyr::mutate(users_acumulados = cumsum(quantidade_usuarios))|>
  ggplot(aes(x = criacao_data, y = users_acumulados)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-03-03"), 
             linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2023-03-03"), y = max_value, 
           label = "Entrada Novos Usuários", color = "black", 
           hjust = 0, vjust = -0.5, size = 4)+
  labs(x = "Data de acesso", y = "Quantidade de Usuário", 
     title = "Quantidade de Usuário ao Longo do tempo") +
  theme(legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 16))
      
#categorização dos usuários 
base_inner|>
  dplyr::glimpse()

base_inner |>
  dplyr::mutate(tipo_usuario =
                  ifelse(criacao_data < "2023-03-03", 
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

base_inner|>
  dplyr::glimpse()

####Tempo de sessão dos cargos. 
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




