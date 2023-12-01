cases_Brazil = select(mydata_Brazil,
                      c('date','state','newCases','totalCases','totalCases_per_100k_inhabitants'))

colnames(cases_Brazil) = c('date','state','Novos casos','Total de casos','Total de casos/100k hab.')

casos_colunas = select(cases_Brazil, 
                       c('Novos casos','Total de casos','Total de casos/100k hab.'))

deaths_Brazil = select(mydata_Brazil,
                       c('date','state','newDeaths','deaths','deaths_per_100k_inhabitants','deaths_by_totalCases'))

colnames(deaths_Brazil) = c('date','state','Novos registros de mortes','Registros totais de mortes','Total de mortes/100k hab.','Total de mortes/Total de casos')

obitos_colunas = select(deaths_Brazil,
                        c('Novos registros de mortes','Registros totais de mortes','Total de mortes/100k hab.','Total de mortes/Total de casos'))

estados_casos_obitos = unique(mydata_Brazil$state)

vaccination_Brazil = select(mydata_Brazil,
                            c('date','state','vaccinated','vaccinated_per_100_inhabitants',
                              'vaccinated_single','vaccinated_single_per_100_inhabitants',
                              'vaccinated_second','vaccinated_second_per_100_inhabitants',
                              'vaccinated_third','vaccinated_third_per_100_inhabitants'))

estados_vacinacao = unique(vaccination_Brazil$state)

# criação de coluna de full_vaccinated
vaccination_Brazil$full_vaccinated = vaccination_Brazil$vaccinated_single + vaccination_Brazil$vaccinated_second
# criação de séries diárias a partir das suas respectivas acumuladas
gb_vaccination_Brazil = vaccination_Brazil %>%
                          group_by(state) %>%
                          mutate(new_full_vaccinated = full_vaccinated - lag(full_vaccinated),
                                 new_vaccinated = vaccinated - lag(vaccinated),
                                 new_vaccinated_single = vaccinated_single - lag(vaccinated_single),
                                 new_vaccinated_second = vaccinated_second - lag(vaccinated_second),
                                 new_vaccinated_third = vaccinated_third - lag(vaccinated_third))

vaccination_Brazil = ungroup(gb_vaccination_Brazil)

colnames(vaccination_Brazil) = c('date','state','Vacinados com 1a dose','Vacinados com 1a dose/100k hab.',
                                 'Vacinados com dose unica','Vacinados com dose unica/100k hab.',
                                 'Vacinados com 2a dose','Vacinados com 2a dose/100k hab.',
                                 'Vacinados com 3a dose','Vacinados com 3a dose/100k hab.',
                                 'Totalmente vacinados', 'Novos totalmente vacinados',
                                 'Novos vacinados com 1a dose','Novos vacinados com dose unica',
                                 'Novos vacinados com 2a dose', 'Novos vacinados com 3a dose')

vacinacao_colunas = select(vaccination_Brazil, 
                           c('Vacinados com 1a dose','Vacinados com 1a dose/100k hab.',
                             'Vacinados com dose unica','Vacinados com dose unica/100k hab.',
                             'Vacinados com 2a dose','Vacinados com 2a dose/100k hab.',
                             'Vacinados com 3a dose','Vacinados com 3a dose/100k hab.',
                             'Totalmente vacinados', 'Novos totalmente vacinados',
                             'Novos vacinados com 1a dose','Novos vacinados com dose unica',
                             'Novos vacinados com 2a dose', 'Novos vacinados com 3a dose'))

mysociodata_Brazil = select(Dados_sociodemograficos,
                            c('COD7','NOME','UF','PIB_P_CAP','GINI','DENS_DEM','PERC60MAIS',
                              'MEDICOS_100MIL','LEITOS_100MIL','PERC_POP_EDUC_SUP'))

colnames(mysociodata_Brazil) = c('codigo','municipio','state','PIB per capita','Índice de Gini da renda domiciliar per capita dos municípios',
                                 'Densidade demográfica','Percentual da população com 60 anos ou mais',
                                 'Quantidade de Médicos/100k hab.','Quantidade de Leitos/100k hab.', 
                                 'Percentual da população com escolaridade de nível superior concluído')

sociodem_colunas = select(mysociodata_Brazil,
                          c('PIB per capita','Índice de Gini da renda domiciliar per capita dos municípios',
                            'Densidade demográfica','Percentual da população com 60 anos ou mais',
                            'Quantidade de Médicos/100k hab.','Quantidade de Leitos/100k hab.', 
                            'Percentual da população com escolaridade de nível superior concluído'))

estados_sociodem = unique(mysociodata_Brazil$state)


# construindo a UI do shiny que plotara o grafico
ui <- fluidPage(
  
  navbarPage('EFFECT-Brazil', 
             id = 'app', 
             theme = shinytheme('flatly'),
             
             tabPanel('Home',
                      useShinydashboard(),
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                introBox(  
                                  fluidRow(column(7,
                                                  h3('Bem vindo ao EFFECT-Brazil!', 
                                                     style='margin-top:0px;'),
                                                  h4('[Breve descrição do app]', 
                                                     style='margin-top:0px;'))
                                  )
                                  )
                                ),
                                
                                br(),
                                
                                fluidRow(
                                  box(title = 'Casos', 
                                      width = 3, 
                                      background = 'blue',
                                      'Explore dados sobre os casos confirmados de COVID-19',
                                      br(),
                                      actionBttn(inputId = 'switch_tab_cases',
                                                 label = 'Acessar página',
                                                 style = 'minimal',
                                                 size = 'sm')),
                                  
                                  box(title = 'Óbitos', 
                                      width = 3, 
                                      background = 'yellow',
                                      'Explore dados sobre os óbitos confirmados de COVID-19',
                                      br(),
                                      actionBttn(inputId = 'switch_tab_deaths',
                                                 label = 'Acessar página',
                                                 style = 'minimal',
                                                 size = 'sm')),
                                  
                                  box(title = 'Hospitalizações', 
                                      width = 3, 
                                      background = 'red',
                                      'Explore dados sobre as hospitalizações de COVID-19',
                                      br(),
                                      actionBttn(inputId = 'switch_tab_hospitalization',
                                                 label = 'Acessar página',
                                                 style = 'minimal',
                                                 size = 'sm')),
                                  
                                  box(title = 'Vacinação', 
                                      width = 3, 
                                      background = 'navy',
                                      'Explore dados sobre a vacinação contra a COVID-19',
                                      br(),
                                      actionBttn(inputId = 'switch_tab_vaccination',
                                                 label = 'Acessar página',
                                                 style = 'minimal',
                                                 size = 'sm')),
                                  
                                  img(src = "210812_Logo Lockup_IG2.jpg"
                                )
                      )
             ),
             
             
             tabPanel('Casos',
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Explore dados sobre os casos confirmados de COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                                
                      ),
                      
                      sidebarLayout(
                        # criando barra lateral para inputar dados
                        sidebarPanel(
                          useShinyjs(),
                          
                          # criando caixa de selecao de variavel plotada
                          varSelectInput(inputId = 'cases_metric',
                                         label = 'Escolha a métrica:',
                                         data = casos_colunas, 
                                         selected = 'Novos casos',
                                         multiple = TRUE),
                          
                          # criando caixa de intervalo temporal
                          dateRangeInput(inputId = 'cases_date',
                                         label = 'Intervalo de data:',
                                         start = min(cases_Brazil$date),
                                         end = max(cases_Brazil$date),
                                         format = "dd/mm/yy"),
                          
                          # criando botão de escolha de agrupamento
                          # radioGroupButtons(inputId = 'cases_groupby',
                          #                   label = 'Agrupar por:', 
                          #                   choices = list('Estado' = 'state',
                          #                                  'Cidade' = 'city'),
                          #                   status = 'primary'),
                          
                          # criando caixa de selecao de estados
                          selectInput(inputId = 'cases_state',
                                      label = 'Escolha o estado:',
                                      choices = estados_casos_obitos, # precisa ser um vetor com valores unicos
                                      selected = 'RJ'),
                          
                          # criando caixa de selecao de cidades
                          # selectInput(inputId = 'cases_city',
                          #             label = 'Escolha a cidade:',
                          #             choices = NULL),
                          
                          # criando caixa para cálculo com ou sem média móvel
                          selectInput(inputId = 'cases_mean',
                                      label = 'Plotar média móvel?',
                                      choices = list('Não' = 'withoutmean',
                                                     'Sim, apenas a média móvel' = 'withmean',
                                                     'Sim, média móvel + variável original' = 'both'),
                                      selected = 'withoutmean'),
                          
                          # criando caixa de selecao de periodos de media movel
                          # a caixa começa escondida quando o app é iniciado
                          hidden(sliderInput(inputId = 'cases_MA_size',
                                             label = 'Períodos de média móvel:',
                                             min = 1,
                                             max = 14,
                                             value = 7)),
                        ),
                        
                        # painel principal para apresentar outputs
                        mainPanel(dygraphOutput(outputId = 'cases_plot'))
                        
                      ),
                      
                      
                      # painel principal para apresentar outputs
                      mainPanel(br(),
                                
                                width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Visualização geográfica dos dados por estado/município sobre os casos confirmados de COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                      ),
                      
                      mainPanel(
                        fluidRow(
                          useShinyjs(),
                          
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      # criando filtro de data
                                      sliderInput(inputId = 'cases_date2',
                                                  "Arraste para selecionar a data:",
                                                  min = as.Date(min(cases_Brazil$date),"%Y-%m-%d"),
                                                  max = as.Date(max(cases_Brazil$date),"%Y-%m-%d"),
                                                  value=as.Date(max(cases_Brazil$date)),
                                                  timeFormat="%d-%m-%Y")),
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      # criando caixa de selecao de variavel plotada
                                      varSelectInput(inputId = 'cases_metric2',
                                                     label = 'Escolha a métrica (por Estado):',
                                                     data = casos_colunas, 
                                                     selected = 'Novos casos',
                                                     multiple = FALSE,
                                                     selectize = FALSE,
                                                     size = 3),
                                      
                                      # criando caixa de selecao de variavel plotada
                                      varSelectInput(inputId = 'demographic_metric1',
                                                     label = 'Escolha a métrica (por Município):',
                                                     data = sociodem_colunas, 
                                                     selected = 'PIB per capita',
                                                     multiple = FALSE,
                                                     selectize = FALSE,
                                                     size = 3,
                                                     width = '110%')),
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotOutput(outputId = 'cases_map'),
                                      
                                      plotOutput(outputId = 'demographic_map1'))
                        )
                      )
                      
             ),
             
             
             tabPanel('Óbitos',
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Explore dados sobre os óbitos confirmados de COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                                
                      ),
                      
                      sidebarLayout(
                        # criando barra lateral para inputar dados
                        sidebarPanel(
                          useShinyjs(),
                          
                          # criando caixa de selecao de variavel plotada
                          varSelectInput(inputId = 'deaths_metric',
                                         label = 'Escolha a métrica:',
                                         data = obitos_colunas, 
                                         selected = 'Novos registros de mortes',
                                         multiple = TRUE),
                          
                          # criando caixa de intervalo temporal
                          dateRangeInput(inputId = 'deaths_date',
                                         label = 'Intervalo de data:',
                                         start = min(deaths_Brazil$date),
                                         end = max(deaths_Brazil$date),
                                         format = "dd/mm/yy"),
                          
                          # criando botão de escolha de agrupamento
                          # radioGroupButtons(inputId = 'deaths_groupby',
                          #                   label = 'Agrupar por:', 
                          #                   choices = list('Estado' = 'state',
                          #                                  'Cidade' = 'city'),
                          #                   status = 'primary'),
                          
                          # criando caixa de selecao de estados
                          selectInput(inputId = 'deaths_state',
                                      label = 'Escolha o estado:',
                                      choices = estados_casos_obitos, # precisa ser um vetor com valores unicos
                                      selected = 'RJ'),
                          
                          # criando caixa de selecao de cidades
                          # selectInput(inputId = 'deaths_city',
                          #             label = 'Escolha a cidade:',
                          #             choices = NULL),
                          
                          # criando caixa para cálculo com ou sem média móvel
                          selectInput(inputId = 'deaths_mean',
                                      label = 'Plotar média móvel?',
                                      choices = list('Não' = 'withoutmean',
                                                     'Sim, apenas a média móvel' = 'withmean',
                                                     'Sim, média móvel + variável original' = 'both'),
                                      selected = 'withoutmean'),
                          
                          # criando caixa de selecao de periodos de media movel
                          # a caixa começa escondida quando o app é iniciado
                          hidden(sliderInput(inputId = 'deaths_MA_size',
                                             label = 'Períodos de média móvel:',
                                             min = 1,
                                             max = 14,
                                             value = 7)),
                        ),
                        
                        # painel principal para apresentar outputs
                        mainPanel(dygraphOutput(outputId = 'deaths_plot'))
                        
                      ),
                      
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                br(),
                                
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Visualização geográfica dos dados por estado/município sobre os óbitos confirmados de COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                                
                      ),
                      
                      mainPanel(
                        fluidRow(
                          useShinyjs(),
                          
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      # criando filtro de data
                                      sliderInput(inputId = 'deaths_date2',
                                                  "Arraste para selecionar a data:",
                                                  min = as.Date(min(deaths_Brazil$date),"%Y-%m-%d"),
                                                  max = as.Date(max(deaths_Brazil$date),"%Y-%m-%d"),
                                                  value=as.Date(max(deaths_Brazil$date)),
                                                  timeFormat="%d-%m-%Y")),
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      # criando caixa de selecao de variavel plotada
                                      varSelectInput(inputId = 'deaths_metric2',
                                                     label = 'Escolha a métrica (por Estado):',
                                                     data = obitos_colunas, 
                                                     selected = 'Novos registros de mortes',
                                                     multiple = FALSE,
                                                     selectize = FALSE,
                                                     size = 3),
                                      
                                      # criando caixa de selecao de variavel plotada
                                      varSelectInput(inputId = 'demographic_metric2',
                                                     label = 'Escolha a métrica (por Município):',
                                                     data = sociodem_colunas, 
                                                     selected = 'PIB per capita',
                                                     multiple = FALSE,
                                                     selectize = FALSE,
                                                     size = 3,
                                                     width = '110%')),
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      plotOutput(outputId = 'deaths_map'),
                                      
                                      plotOutput(outputId = 'demographic_map2'))
                        )
                      )
                      
             ),
             
             tabPanel('Hospitalizações',
                      menuItem("Source code", icon = icon("file-code-o"), 
                               href = "https://github.com/lslbastos/Bastos_Ranzani_etal_COVID19_ChangeWaves"),
                      fluidRow(
                        tags$iframe(
                          seamless = "seamless",
                          src = "https://lslbastos.shinyapps.io/sivep_covid_brazil/",
                          height = 800, width = 1400))
                      
                      
             ),
             
             tabPanel('Vacinação',
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Explore dados sobre a vacinação contra a COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                                
                      ),
                      
                      sidebarLayout(
                        # criando barra lateral para inputar dados
                        sidebarPanel(
                          useShinyjs(),
                          
                          # criando caixa de selecao de variavel plotada
                          varSelectInput(inputId = 'vaccination_metric',
                                         label = 'Escolha a métrica:',
                                         data = vacinacao_colunas, 
                                         selected = 'Vacinados com 1a dose',
                                         multiple = TRUE),
                          
                          # criando caixa de intervalo temporal
                          dateRangeInput(inputId = 'vaccination_date',
                                         label = 'Intervalo de data:',
                                         start = as.Date('2021-01-01'),
                                         end = max(vaccination_Brazil$date),
                                         format = "dd/mm/yy"),
                          
                          # criando caixa de selecao de estados
                          selectInput(inputId = 'vaccination_state',
                                      label = 'Escolha o estado:',
                                      choices = estados_vacinacao, # precisa ser um vetor com valores unicos
                                      selected = 'RJ')
                        ),
                        
                        # painel principal para apresentar outputs
                        mainPanel(dygraphOutput(outputId = 'vaccination_plot'))
                        
                      ),
                      
                      mainPanel(width = 11, 
                                style='margin-left:4%; margin-right:4%',
                                br(),
                                br(),
                                
                                introBox(  
                                  fluidRow(column(12,
                                                  h3('Visualização geográfica dos dados por estado/município sobre vacinação contra COVID-19', 
                                                     style='margin-top:0px;')))
                                ),
                                
                                br()
                                
                                
                                
                      ),
                        
                      mainPanel(
                        fluidRow(
                          useShinyjs(),
                          
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                      # criando filtro de data
                                      sliderInput(inputId = 'vaccination_date2',
                                                  "Arraste para selecionar a data:",
                                                  min = as.Date(min(vaccination_Brazil$date),"%Y-%m-%d"),
                                                  max = as.Date(max(vaccination_Brazil$date),"%Y-%m-%d"),
                                                  value=as.Date(max(vaccination_Brazil$date)),
                                                  timeFormat="%d-%m-%Y")),
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                                     # criando caixa de selecao de variavel plotada
                                     varSelectInput(inputId = 'vaccination_metric2',
                                                    label = 'Escolha a métrica (por Estado):',
                                                    data = vacinacao_colunas, 
                                                    selected = 'Vacinados com 1a dose',
                                                    multiple = FALSE,
                                                    selectize = FALSE,
                                                    size = 3),
                                     
                                     # criando caixa de selecao de variavel plotada
                                     varSelectInput(inputId = 'demographic_metric4',
                                                    label = 'Escolha a métrica (por Município):',
                                                    data = sociodem_colunas, 
                                                    selected = 'PIB per capita',
                                                    multiple = FALSE,
                                                    selectize = FALSE,
                                                    size = 3,
                                                    width = '110%')),
                         
                         splitLayout(cellWidths = c("50%", "50%"),
                                     plotOutput(outputId = 'vaccination_map'),
                                     
                                     plotOutput(outputId = 'demographic_map4'))
                       )
                    )
                      
                      
             )
             
             
  )
)