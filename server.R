# Definição de ausência de Notação Científica
options(scipen=10000)

# funcao auxiliar de teste para duas variaveis
up_two <- function(data, mean) {
  if (length(data) > 1 & mean == 'both'){
    'Erro: Escolha no máximo uma variável para plotar esse tipo de gráfico!'
  }
  else if (length(data) > 2) {
    'Erro: Escolha no máximo duas variáveis para plotar esse tipo de gráfico!'
  }
  else if (length(data) == 0) {
    'Erro: Escolha no mínimo uma variável para plotar esse tipo de gráfico!'
  } 
  else NULL
}


# função para criação de mapas
create_sociodem_map <- function(metric){
  
  
  sociodem_res = select(mysociodata_Brazil,
                        c('codigo',metric))
  
  all_muni <- read_municipality(year=2019)
  
  no_axis <- theme(axis.title=element_blank(),
                   axis.text=element_blank(),
                   axis.ticks=element_blank())
  
  
  all_muni <- dplyr::left_join(all_muni, sociodem_res, by = c("code_muni" = "codigo"))
  
  titulo_map <- metric
  
  plot_map <- ggplot() +
    geom_sf(data = all_muni, aes_string(fill = metric), color = NA) +
    labs(title=titulo_map, size=15) +
    scale_fill_distiller(palette = "YlOrBr",direction = 1) +
    theme_minimal() +
    theme(legend.title = element_blank(),legend.position = c(0.2, 0.2))+
    no_axis 
  
  return(plot_map)
  }



server  <- function(input, output, session)({
  # leva para a aba de Casos
  observeEvent(input$switch_tab_cases, {
    updateTabsetPanel(session, 
                      inputId = 'app',
                      selected = 'Casos')
  })
  # leva para a aba de Óbitos
  observeEvent(input$switch_tab_deaths, {
    updateTabsetPanel(session, 
                      inputId = 'app',
                      selected = 'Óbitos')
  })
  # leva para a aba de Hospitalizações
  observeEvent(input$switch_tab_hospitalization, {
    updateTabsetPanel(session, 
                      inputId = 'app',
                      selected = 'Hospitalizações')
  })
  # leva para a aba de Vacinação
  observeEvent(input$switch_tab_vaccination, {
    updateTabsetPanel(session, 
                      inputId = 'app',
                      selected = 'Vacinação')
  })
  
  
  ### Aba Casos ####  
  
  variaveis <- reactive({ get(input$cases_metric) })
  
  create_cases_dygraph <- reactive({validate(up_two(input$cases_metric, input$cases_mean))
    
    filtered <- filter(cases_Brazil,
                       state == input$cases_state,
                       date <= max(input$cases_date),
                       date >= min(input$cases_date))
    
    
    series <- xts(select(filtered,!!!input$cases_metric),
                  order.by = as.Date(filtered$date))
    
    if (input$cases_mean == 'withmean') {
      series <- rollmean(series, k = input$cases_MA_size, align  = 'right')
      colnames(series) = paste(colnames(series), 'media_movel', sep = '_')
      observe(show('cases_MA_size'))
    } else if (input$cases_mean == 'both') {
      series_mean <- rollmean(series, k = input$cases_MA_size, align  = 'right')
      colnames(series_mean) = paste(colnames(series_mean), 'media_movel', sep = '_')
      series <- cbind(series, series_mean)
      observe(show('cases_MA_size'))
    }
    
    
    dygraph(series) %>%
      {# fluxo de plotar duas variaveis
        if(length(input$cases_metric) > 1 )
          dySeries(.,str(input$cases_metric[2]), axis = 'y2')
        else
          .
      } %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% # mexendo na estrutura do grafico
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5) # config ao passar mouse em cima do grafico
    
    
  })
  
  
  output$cases_plot <- renderDygraph({create_cases_dygraph()})
  
  
  create_cases_map <- function(){
    
    corona_res = cases_Brazil %>% 
      filter(date == input$cases_date2,
             state != "TOTAL") %>% 
      select(state, input$cases_metric2)
    
    states <- read_state(year=2019)
    
    no_axis <- theme(axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank())
    
    
    states <- dplyr::left_join(states, corona_res, by = c("abbrev_state" = "state"))
    
    titulo_map <- input$cases_metric2
    
    plot_map <- ggplot() +
      geom_sf(data = states, aes_string(fill = input$cases_metric2) , color = NA) +
      labs(title=titulo_map, size=15) +
      scale_fill_distiller(palette = "BuPu",direction = 1) +
      theme_minimal() +
      theme(legend.title = element_blank(),legend.position = c(0.2, 0.2))+
      no_axis 
    
    return(plot_map)}
  
  # mapa de casos
  output$cases_map <- renderPlot({create_cases_map()})
  
  # mapa sociodemográfico
  output$demographic_map1<- renderPlot({create_sociodem_map(input$demographic_metric1)})
  
  
  
  ### Aba Óbitos ####  
  
  variaveis <- reactive({ get(input$deaths_metric) })
  
  create_deaths_dygraph <- reactive({validate(up_two(input$deaths_metric, input$deaths_mean))
    
    filtered <- filter(deaths_Brazil,
                       state == input$deaths_state,
                       date <= max(input$deaths_date),
                       date >= min(input$deaths_date))
    
    
    series <- xts(select(filtered,!!!input$deaths_metric),
                  order.by = as.Date(filtered$date))
    
    if (input$deaths_mean == 'withmean') {
      series <- rollmean(series, k = input$deaths_MA_size, align  = 'right')
      colnames(series) = paste(colnames(series), 'media_movel', sep = '_')
      observe(show('deaths_MA_size'))
    } else if (input$deaths_mean == 'both') {
      series_mean <- rollmean(series, k = input$deaths_MA_size, align  = 'right')
      colnames(series_mean) = paste(colnames(series_mean), 'media_movel', sep = '_')
      series <- cbind(series, series_mean)
      observe(show('deaths_MA_size'))
    }
    
    
    dygraph(series) %>%
      {# fluxo de plotar duas variaveis
        if(length(input$deaths_metric) > 1 )
          dySeries(.,str(input$deaths_metric[2]), axis = 'y2')
        else
          .
      } %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% # mexendo na estrutura do grafico
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5) # config ao passar mouse em cima do grafico
    
    
  })
  
  
  output$deaths_plot <- renderDygraph({create_deaths_dygraph()})
  
  
  create_deaths_map <- function(){
    
    corona_res = deaths_Brazil %>% 
      filter(date == input$deaths_date2,
             state != "TOTAL") %>% 
      select(state, input$deaths_metric2)
    
    states <- read_state(year=2019)
    
    no_axis <- theme(axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank())
    
    
    states <- dplyr::left_join(states, corona_res, by = c("abbrev_state" = "state"))
    
    titulo_map <- input$deaths_metric2
    
    plot_map <- ggplot() +
      geom_sf(data = states, aes_string(fill = input$deaths_metric2) , color = NA) +
      labs(title=titulo_map, size=15) +
      scale_fill_distiller(palette = "BuPu",direction = 1) +
      theme_minimal() +
      theme(legend.title = element_blank(),legend.position = c(0.2, 0.2))+
      no_axis 
    
    return(plot_map)}
  
  # mapa de óbitos
  output$deaths_map <- renderPlot({create_deaths_map()})
  
  # mapa sociodemográfico
  output$demographic_map2<- renderPlot({create_sociodem_map(input$demographic_metric2)})
  
  
  
  
  ### Aba Vacinação ####  
  
  variaveis <- reactive({ get(input$vaccination_metric) })
  
  create_vaccination_dygraph <- reactive({
    
    filtered <- filter(vaccination_Brazil,
                       state == input$vaccination_state,
                       date <= max(input$vaccination_date),
                       date >= min(input$vaccination_date))
    
    
    series <- xts(select(filtered,!!!input$vaccination_metric),
                  order.by = as.Date(filtered$date))
    
    
    dygraph(series) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% # mexendo na estrutura do grafico
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5) # config ao passar mouse em cima do grafico
    
    
  })
  
  
  output$vaccination_plot <- renderDygraph({create_vaccination_dygraph()})
  
  
  create_vaccination_map <- function(){
    
    corona_res = vaccination_Brazil %>% 
      filter(date == input$vaccination_date2,
             state != "TOTAL") %>% 
      select(state, input$vaccination_metric2)
    
    states <- read_state(year=2019)
    
    no_axis <- theme(axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank())
    
    
    states <- dplyr::left_join(states, corona_res, by = c("abbrev_state" = "state"))
    
    titulo_map <- input$vaccination_metric2
    
    plot_map <- ggplot() +
      geom_sf(data = states, aes_string(fill = input$vaccination_metric2) , color = NA) +
      labs(title=titulo_map, size=15) +
      scale_fill_distiller(palette = "BuPu",direction = 1) +
      theme_minimal() +
      theme(legend.title = element_blank(),legend.position = c(0.2, 0.2))+
      no_axis 
    
    return(plot_map)}
  
  # mapa de vacinação
  output$vaccination_map <- renderPlot({create_vaccination_map()})
  
  # mapa sociodemográfico
  output$demographic_map4<- renderPlot({create_sociodem_map(input$demographic_metric4)})
  
})
  



