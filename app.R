library(shiny)
library(EpiDynamics)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(deSolve)

# import dos dados
dados = read.csv("input_data/casos.csv")
dados_pr = read.csv('input_data/casos_pr.csv')

# beta para o brasil
dados$data = as.Date(dados$data)

casos <- dados$casos_acumulados[(nrow(dados)-9):nrow(dados)]
dias <- 1:length(casos)

modelo.exp <- lm(log(casos+1)~dias)

betta <- modelo.exp$coefficients[[2]] + 0.10

# beta para o parana
dados_pr$data = as.Date(dados_pr$data)

casos_pr = dados_pr$casos_acumulados[(nrow(dados_pr)-9):nrow(dados_pr)]
dias_pr <- 1:length(casos_pr)

modelo.exp_pr <- lm(log(casos_pr+1)~dias_pr)

betta_pr <- modelo.exp_pr$coefficients[[2]] + 0.10

###########
ui <- navbarPage(theme = shinytheme("spacelab"), collapsible = TRUE,
                 "COVID-19",
                 # aba de panorama para o brasil e parana
                 tabPanel("Panorama",
                          sidebarLayout(
                              sidebarPanel(
                                  br(),
                                  br(),
                                  h4(em(strong('Sobre os Gráficos'))),
                                  br(),
                                  p('O primeiro Gráfico apresenta a evolução dos casos acumulados e óbitos
                                     acumulados por dia, também é possível observar os casos ativos, que foram
                                     obtidos subtraindo os casos do dia menos os casos de 10 dias atrás: ',
                                     withMathJax('\\(C_{ativos} = C_{dia} - C_{dia-10}\\)')),

                                  p('O segundo e o terceiro gráficos mostram a evolução de novos casos e novos óbitos por
                                     dia junto com a média móvel de 3 dias.'),
                                  br(),
                                  br(),
                                  h4(em(strong(withMathJax('Obtençao de \\(\\beta\\)')))),
                                  br(),
                                  p(withMathJax('Para obter o valor de \\(\\beta\\) foi realizado uma regressão
                                             linear com o logaritimo dos casos acumulados dos últimos 10 dias.
                                             Ao valor do coeficiente angular obtido foi adicionado 0.1 (valor utilizado
                                             para \\(\\gamma\\) na construção do modelo '), em(strong('SEIR')),').')),
                                                
                                  mainPanel(
                                  tabsetPanel(
                                      # aba brasil
                                      tabPanel('Brasil',
                                               br(),
                                               h4(withMathJax(helpText('Valor de \\(\\beta\\)  Brasil - ' ,round(betta,4)))),
                                               br(),
                                               plotlyOutput("PANORAMA_acumulado"),
                                               br(),
                                               br(),
                                               br(),
                                               plotlyOutput("PANORAMA_novoscasos"),
                                               br(),
                                               br(),
                                               br(),
                                               plotlyOutput("PANORAMA_novosobitos")
                                      ),
                                      # aba parana
                                      tabPanel('Paraná',
                                               br(),
                                               h4(withMathJax(helpText('Valor de \\(\\beta\\) Paraná - ' ,round(betta_pr,4)))),
                                               br(),
                                               plotlyOutput("PANORAMA_acumulado_pr"),
                                               br(),
                                               br(),
                                               br(),
                                               plotlyOutput("PANORAMA_novoscasos_pr"),
                                               br(),
                                               br(),
                                               br(),
                                               plotlyOutput("PANORAMA_novosobitos_pr")
                                              
                              
                                      ))))),
                
                 
                 ####### AS PROJECOES AINDA NAO ESTAO BOAS
                 ####### PARA VISUALIZAR A ABA DE PROJECAO DESCOMENTE O CODIGO
                 
                 ## aba de projecoes utilizando 3 e 5 dias
                 # tabPanel("Projeções",
                 # 
                 #          sidebarLayout(
                 #              sidebarPanel(
                 #                 p(' ')
                 #              ),
                 # 
                 # 
                 #             mainPanel(
                 #                 tabsetPanel(
                 #                      tabPanel('3 Dias',
                 #                               br(),
                 #                               br(),
                 #                                plotlyOutput('PROJECAO_proj3')
                 #                      ),
                 #                      tabPanel('5 Dias',
                 #                               br(),
                 #                               br(),
                 #                                plotlyOutput('PROJECAO_proj5')
                 #                      )
                 #                      
                 #                  )
                 #              )
                 #          )),
                 
                 tabPanel("Modelos Epidemiológicos",
                          br(),
                          h2(em(strong('Modelo SEIR'))),
                          br(),
                          p('No modelo SEIR dividimos a população em quatro grupos ou compartimentos, sendo eles:'),
                          p(em(strong('- Suscetíveis (S):')), 'São todas as pessoas que podem, em algum mometo ser infectadas.'),
                          p(em(strong('- Expostos (E):')), 'Os expostos são as pessoas que já se contaminaram e ainda estão no período de incubação do vírus.'),
                          p(em(strong('- Infectados (I):')), 'Este grupo é formado pelas pessoas que estão infectadas com o vírus e já
                            passaram da fase de incubação, podendo infectar outras pessoas.'),
                          p(em(strong('- Recuperados (R):')), 'O grupo dos Recuperados é composto pelas pessoas que já passaram da fase de infecção,
                            se curando da doença ou as pessoas que vieram a falecer'),
                          img(src = "seir.png", height = 200, width = 900),
                          # equacoes SEIR
                          p('Agora com o diagrama do modelo ', em(strong('SEIR')), 'podemos obter o sistema de equações diferenciais:' ),
                          p(withMathJax('$$ dS = - \\beta \\cdot S \\cdot I $$')),
                          p(withMathJax('$$ dE = \\beta \\cdot I \\cdot S - \\sigma \\cdot E $$')),
                          p(withMathJax('$$ dI = \\sigma \\cdot E - \\gamma \\cdot I $$')),
                          p(withMathJax('$$ dR = \\gamma \\cdot I $$')),
                          #Taxas SEIR
                          p('Para o modelo ', em(strong('SEIR:'))),
                          p(withMathJax(em(strong('- \\(\\beta\\)')),' é a Taxa de propagação da infecção.')),
                          p(withMathJax(em(strong('- \\(\\sigma\\)')),' é a Taxa de incubação da infecção.')),
                          p(withMathJax(em(strong('- \\(\\gamma\\)')),' é a Taxa de recuperação da infecção.')),
                          
                          #modelo SEIRHM
                          br(),
                          h2(em(strong('Modelo SEIR+'))),
                          br(),
                          p('O modelo ', em(strong('SEIRHM')), 'é o modelo ', em(strong('SEIR')), 'modificado, utilizando o modelo', em(strong('SEIR')), 'adicionamos
                            dois compartimentos a mais, o compartimento dos Hospitalizados ',em(strong('H')), 'e Mortos',em(strong('M')), 'sendo assim, os grupos ficam do
                            seguinte modo:'),
                          p(em(strong('- Suscetíveis (S):')), 'São todas as pessoas que podem, em algum mometo ser infectadas.'),
                          p(em(strong('- Expostos (E):')), 'Os expostos são as pessoas que já se contaminaram e ainda estão no período de incubação do vírus.'),
                          p(em(strong('- Infectados (I):')), 'Este grupo é formado pelas pessoas que estão infectadas com o vírus e já
                            passaram da fase de incubação, podendo infectar outras pessoas.'),
                          p(em(strong('- Curados (R):')), 'O grupo dos Curados é composto pelas pessoas que já passaram da fase de infecção e foram curadas'),
                          p(em(strong('- Hospitalizados (H):')),'O grupo dos Hospitalizados é formados pelas pessoas que forão hospitalizadas por conta da infecção. '),
                          p(em(strong('- Mortos (M):')), 'O grupo dos Mortos é formado especificamente pelas pessoas que vieram a falecer por conta da infecção.'),
                          img(src = 'seirhmpng.png', height = 450, width = 1200),
                          p('Utilizando o diagrama podemos obter as equações diferenciais: '),
                          # equacoes modelo SEIRHM
                          p(withMathJax('$$ dS = - \\beta \\cdot S \\cdot I $$')),
                          p(withMathJax('$$ dE = \\beta \\cdot I \\cdot S - \\sigma \\cdot E $$')),
                          p(withMathJax('$$ dI = \\sigma \\cdot E - (1 - \\alpha) \\gamma \\cdot I - \\alpha\\cdot \\gamma \\cdot I$$')),
                          p(withMathJax('$$ dR = \\alpha \\cdot \\gamma \\cdot I +  (1 - \\lambda) \\rho \\cdot H $$')),
                          p(withMathJax('$$ dH = (1- \\alpha)  \\gamma \\cdot I - \\lambda \\cdot \\rho \\cdot H - (1-\\lambda) \\rho \\cdot H$$')),
                          p(withMathJax('$$ dM = \\lambda \\cdot \\rho \\cdot H$$')),
                          br(),
                          
                          # taxas modelo SEIRHM
                          p('Para o modelo', em(strong('SEIRHM:'))),
                          p(withMathJax(em(strong('- \\(\\beta\\)')),' é a Taxa de propagação da infecção.')),
                          p(withMathJax(em(strong('- \\(\\sigma\\)')),' é a Taxa de incubação da infecção.')),
                          p(withMathJax(em(strong('- \\(\\gamma\\)')),' é a Taxa de recuperação da infecção.')),
                          p(withMathJax(em(strong('- \\(\\alpha\\)')),' é a Taxa de hospitalização da infecção.')),
                          p(withMathJax(em(strong('- \\(\\lambda\\)')),' é a Taxa de')),
                          p(withMathJax(em(strong('- \\(\\rho\\)')),' é a Taxa de mortalidade da infecção.')),
                 ),
                 
                 # aba calculadora modelo SEIR e SEIRM
                 tabPanel("Calculadora de Modelos Epidemiológicos",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  tabsetPanel(
                                      tabPanel('Parâmetros',
                                        br(),
                                        
                                  # Populacao
                                  numericInput("N",
                                               "População:",
                                               min = 500,
                                               max = 13000000,
                                               value = 400000,
                                               step = 150),
                                  
                                  # Expostos
                                  numericInput("E",
                                               "Expostos:",
                                               min = 0,
                                               max = 13000000,
                                               value = 25,
                                               step = 1),
                                  
                                  # Infecciosos
                                  numericInput("I",
                                               "Infectados",
                                               min = 0,
                                               max = 13000000,
                                               value = 10,
                                               step = 1),
                                  
                                  # Beta
                                  numericInput('beta',
                                               withMathJax("\\(\\beta\\)"),
                                               min = 0.01,
                                               max = 1,
                                               value = 0.2,
                                               step = 0.01),
                                  
                                  # Sigma
                                  numericInput('sigma',
                                               withMathJax("\\(\\sigma\\)"),
                                               min = 0.01,
                                               max = 1,
                                               value = 0.2,
                                               step = 0.01),
                                  
                                  # gamma
                                  numericInput('gamma',
                                               withMathJax("\\(\\gamma\\)"),
                                               min = 0.01,
                                               max = 1,
                                               value = 0.1,
                                               step = 0.01),
                                  
                                  # tempo
                                  sliderInput('tempo',
                                              "Tempo:",
                                              min = 10,
                                              max = 365,
                                              value = 200,
                                              animate = animationOptions(interval = 100, loop = FALSE)),
                                  
                                  # alpha
                                  numericInput('alpha',
                                               withMathJax('\\(\\alpha\\) (SEIRHM)'),
                                               min = 0,
                                               max = 1,
                                               value = 0.1,
                                               step = 0.01),
                                  #lambda
                                  numericInput('lambda',
                                               withMathJax("\\(\\lambda\\) (SEIRHM)"),
                                               min = 0,
                                               max = 1,
                                               value = 0.1,
                                               step = 0.01),
                                  #rho
                                  numericInput('rho',
                                               withMathJax("\\(\\rho\\) (SEIRHM)"),
                                               min = 0,
                                               max = 1,
                                               value = 0.1,
                                               step = 0.01)),
                                  
                                  tabPanel('Sobre os Parâmetros',
                                           
                                          br(),
                                          h4('Parâmetros Utilizados na Calculadora do Modelo SEIR'),
                                          p(em(strong('População (N): ')), 'população total da cidade ou região a ser analisada.'),
                                          p(em(strong('Expostos (E): ')), 'os expostos são aqueles que já estão contaminados com
                                            o vírus, porém ainda na fase de incubação'),
                                          p(em(strong('Infectados (I): ')), 'os infectados são a parte da população que já está com o vírus e o vírus não
                                            está mais na fase de incubação, nos infectados estão os sintomáticos e assintomáticos.'),
                                          p(withMathJax('\\(\\beta\\): Taxa de propagação da infecção.')),
                                          p(withMathJax('\\(\\sigma\\): Taxa de incubação da infeção, é o tempo médio de icubação do vírus \\(\\sigma = \\dfrac{1}{t_e} \\),
                                                        sendo \\(t_e\\) o tempo médio de incubação do vírus.')),
                                          p(withMathJax('\\(\\gamma\\): Taxa de recuperação da infecção, e está relacionado com o tempo médio que uma pessoa infectada
                                                        é capaz de transmitir o vírus da forma que \\(\\gamma = \\dfrac{1}{t_r} \\).
                                                        sendo \\(t_r\\) o tempo médio de incubação do vírus.')),
                                          p(em('* Na construção da calculadora foi considerado que o número de Recuperados', em(strong('(R)')), 'é igual a 0')),
                                          
                                          h4('Parâmetros Utilizados na Calculadora do Modelo SEIRHM'),
                                          p(withMathJax('\\(\\alpha\\): ')),
                                          p(withMathJax('\\(\\lambda\\): ')),
                                          p(withMathJax('\\(\\rho\\): ')),
                                          p(em('** Na construção da calculadora foi considerado que o número inicial de Hospitalizados',em(strong('(H)')), ', o número de Mortos',
                                            em(strong('(M)')), 'e o número de Curados', em(strong('(R)')), 'é igual a 0.'))
                                        
                              ))),
                                  
                              mainPanel(
                                  
                                  tabsetPanel(
                                    tabPanel('Modelo SEIR',
                                        br(),
                                        br(),
                                        plotlyOutput('CALCULADORA_seir')),
                                  
                                  tabPanel('Modelo SEIRHM',
                                        br(),
                                        br(),
                                        plotlyOutput('CALCULADORA_seirhm'))
                                  )))),
                 
                 # aba contendo os dataframes do parana e do brasil
                 tabPanel("Dados",
                          numericInput("maxrows", "Colunas a ser mostrada:", 25),
                          tabsetPanel(
                              tabPanel('Dados Brasil',
                                       verbatimTextOutput("DADOS_tabela")),
                              tabPanel('Dados Paraná',
                                   verbatimTextOutput("DADOS_tabela_pr")))
                          ),
                 
                 # aba sobre o site
                 tabPanel('Sobre o Site',
                          tags$div(
                              br(),
                              h4('Última Atualização'),
                              h6('29/07/2020'), # Ultima Atualizacao
                              p("Este site foi feito como forma de atividade final para o curso de", em('Modelagem Matemática para COVID-19'),
                                'disponibilizado pela Universidade Estadual de Maringá (UEM).'),
                              
                              br(),
                              h4('Base de Dados'),
                              p('Os dados foram retirados do repositório fornecido pela Johns Hopkins University e pelo site Brasil.io:'),
                              tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series","JHU CSSE COVID-19 Dataset
"),tags$br(),
                              tags$a(href="https://brasil.io/dataset/covid19/caso_full/", "Brasil.io"),tags$br(),
                              
                              br(),
                              h4('Background'),
                              p('Em dezembro de 2019, casos de doenças respiratórias graves começaram a ser relatados em toda a cidade de Wuhan, na China. 
                                Estes foram causados por um novo tipo de coronavírus, e a doença agora é conhecida como COVID-19. O número de casos de COVID-19 
                                começou a aumentar mais rapidamente no inicio de janeiro e o vírus logo se espalhou para fora das fronteiras da China.
                                Os casos estão evoluindo rapidamente desde então, e a cada dia nos deparamos com notícias preocupantes sobre o estado atual do surto.'),
                              p('Isoladamente, essas manchetes podem ser difíceis de interpretar. Qual a velocidade da propagação do vírus? Os esforços para controlar
                                a doença estão funcionando? Como a situação se compara com epidemias anteriores? Este site é atualizado com base nos dados publicados pela
                                Johns Hopkins University e pelo site Brasil.io. Olhando além das manchetes, esperamos que seja possível obter uma compreensão mais profunda dessa
                                pandemia em desenvolvimento.'),
                              
                              br(),
                              h4('Repositório'),
                              p('Todo os dados, códigos em R e notebook em Python estão disponíveis no',
                                tags$a(href="https://github.com/henriquevedoveli/covid19", "Github"),tags$br())

                              ))
)

##### SERVER ######
server <- function(input, output) {
    
    output$PANORAMA_acumulado <- renderPlotly({
        
        fig_acumulados <- plot_ly(x = ~dados$data, y= ~dados$casos_acumulados, type = 'scatter', mode = 'lines',
                                  name = 'Casos Acumulados', line = list(color='green', width = 3))
        
        fig_acumulados <- fig_acumulados %>% add_trace(y= ~dados$casos_ativos, type = 'scatter', mode = 'lines',
                                                       name = 'Casos Ativos (10 dias)', line = list(color='blue'))
        
        fig_acumulados <- fig_acumulados %>% add_trace(y= ~dados$obitos_acumulados, type = 'scatter', mode = 'lines',
                                                       name = 'Óbitos Acumulados', line = list(color='red'))
        
        fig_acumulados <- fig_acumulados %>%  layout(title = list(text = 'Evolução Casos Acumulados por dia', 
                                                                  font = list(size = 15),
                                                                  x = 0),
                                                     hovermode = TRUE,
                                                     xaxis = list(title='Dias'),
                                                     yaxis = list(title='Casos'))
        
    })
    
    output$PANORAMA_novoscasos <- renderPlotly({
        fig_novos_casos <- plot_ly(x = ~dados$data, y= ~dados$rolling_mean_casos_novos, type = 'scatter', mode = 'lines',
                                   name = 'Média Móvel (3 dias)', line = list(color='rgb(0,255,255'), opacity = 0.7)
        
        fig_novos_casos <- fig_novos_casos %>% add_bars(y= ~dados$casos_novos, type = 'bar',
                                                        name = 'Casos Novos', marker = list(color = 'gray'), opacity = 1)
        
        
        fig_novos_casos <- fig_novos_casos %>% layout(title = list(text = 'Evolução de Novos Casos por dia', 
                                                                   font = list(size = 15),
                                                                   x = 0),
                                                      hovermode = TRUE,
                                                      xaxis = list(title = "Dias"),
                                                      yaxis = list(title = "Casos"))
    })
    
    output$PANORAMA_novosobitos <- renderPlotly({
        fig_novos_obitos <- plot_ly(x = ~dados$data, y= ~dados$rolling_mean_obitos_novos, type = 'scatter', mode = 'lines',
                                    name = 'Média Móvel (3 dias)', line = list(color='#F58C14'), opacity = 0.7)
        
        fig_novos_obitos <- fig_novos_obitos %>% add_trace(x = ~dados$data, y= ~dados$obitos_novos, type = 'bar',
                                                           name = 'Óbitos Novos', marker = list(color = 'gray'), opacity = 1)
        
        
        fig_novos_obitos <- fig_novos_obitos %>% layout(title = list(text = 'Evolução de Novos Óbitos por dia', 
                                                                     font = list(size = 15),
                                                                     x = 0),
                                                        hovermode = T,
                                                        xaxis = list(title = "Dias"),
                                                        yaxis = list(title = "Óbitos"))
    })
    
    output$PANORAMA_acumulado_pr <- renderPlotly({
        
        fig_acumulados_pr <- plot_ly(x = ~dados_pr$data, y= ~dados_pr$casos_acumulados, type = 'scatter', mode = 'lines',
                                  name = 'Casos Acumulados', line = list(color='green', width = 3))
        
        fig_acumulados_pr <- fig_acumulados_pr %>% add_trace(y= ~dados_pr$casos_ativos, type = 'scatter', mode = 'lines',
                                                       name = 'Casos Ativos (10 dias)', line = list(color='blue'))
        
        fig_acumulados_pr <- fig_acumulados_pr %>% add_trace(y= ~dados_pr$obitos_acumulados, type = 'scatter', mode = 'lines',
                                                       name = 'Óbitos Acumulados', line = list(color='red'))
        
        fig_acumulados_pr <- fig_acumulados_pr %>%  layout(title = list(text = 'Evolução Casos Acumulados por dia', 
                                                                  font = list(size = 15),
                                                                  x = 0),
                                                     hovermode = TRUE,
                                                     xaxis = list(title='Dias'),
                                                     yaxis = list(title='Casos'))
        
    })
    
    output$PANORAMA_novoscasos_pr <- renderPlotly({
        fig_novos_casos_pr <- plot_ly(x = ~dados_pr$data, y= ~dados_pr$rolling_mean_casos_novos, type = 'scatter', mode = 'lines',
                                   name = 'Média Móvel (3 dias)', line = list(color='rgb(0,255,255'), opacity = 0.7)
        
        fig_novos_casos_pr <- fig_novos_casos_pr %>% add_bars(y= ~dados_pr$casos_novos, type = 'bar',
                                                        name = 'Casos Novos', marker = list(color = 'gray'), opacity = 1)
        
        
        fig_novos_casos_pr <- fig_novos_casos_pr %>% layout(title = list(text = 'Evolução de Novos Casos por dia', 
                                                                   font = list(size = 15),
                                                                   x = 0),
                                                      hovermode = TRUE,
                                                      xaxis = list(title = "Dias"),
                                                      yaxis = list(title = "Casos"))
    })
    
    output$PANORAMA_novosobitos_pr <- renderPlotly({
        fig_novos_obitos_pr <- plot_ly(x = ~dados_pr$data, y= ~dados_pr$rolling_mean_obitos_novos, type = 'scatter', mode = 'lines',
                                    name = 'Média Móvel (3 dias)', line = list(color='#F58C14'), opacity = 0.7)
        
        fig_novos_obitos_pr <- fig_novos_obitos_pr %>% add_trace(x = ~dados_pr$data, y= ~dados_pr$obitos_novos, type = 'bar',
                                                           name = 'Óbitos Novos', marker = list(color = 'gray'), opacity = 1)
        
        
        fig_novos_obitos_pr <- fig_novos_obitos_pr %>% layout(title = list(text = 'Evolução de Novos Óbitos por dia', 
                                                                     font = list(size = 15),
                                                                     x = 0),
                                                        hovermode = T,
                                                        xaxis = list(title = "Dias"),
                                                        yaxis = list(title = "Óbitos"))
        
    })
    
    BETA = 0.16155
    output$PROJECAO_proj3 <- renderPlotly({
        
        N <- 200000000
        E <- 1000
        I <- 500
        R <- 0
        S <- N-E-I-R
        tempo_projecao = 3 + length(dados$casos_acumulados)
        
        
        ### criando dataframe auxiliar para o plot
        
        dias <- c(0:tempo_projecao)
        
        valores_nulos <- rep(NaN, (length(dias) - length(dados$casos_acumulados)))
        
        casos_ext = append(dados$casos_acumulados, valores_nulos)
        
        x <- data.frame("dias" = dias, "casos_acumulados" = casos_ext)
        x$dias = dados$data[[1]] + x$dias
        
        # Parameters and initial conditions.
        seir <- SEIR(pars = c(mu = 0, beta = BETA, sigma = 0.2, gamma = 0.1),
                     init = c(S= S/N, E = E/N, I= I/N, R = R/N),
                     time = 0:tempo_projecao)
        
        
        # casos reais
        fig_proj <- plot_ly(x = ~x$dias)
     
        fig_proj <- fig_proj %>% add_trace(y = ~x$casos_acumulados, name="Dados Reais", mode= 'markers',
                                           marker = list(size = 7,
                                                         color = 'rgba(255, 170, 170, .9)',
                                                         line = list(color = 'rgba(152, 0, 0, .8)',
                                                                     width = 2)))
        
        # acumulados projecao (recuperados + infectados)
        fig_proj <- fig_proj %>% add_trace(y=~N*(seir$results$R + seir$results$I), name="Recuperados + Infectados", mode= 'lines',
                                           line = list(color='gray', width = 4))
        
        # infecciosos
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$I, name="Infecciosos", mode= 'lines',
                                           line = list(color='red'), visible = 'legendonly')
        
        # recuperados
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$R, name="Recuperados", mode= 'lines',
                                           line = list(color='green'), visible = 'legendonly')
        
        # suscetiveis
        fig_proj <- fig_proj %>% add_trace(y = ~N*seir$results$S, type = 'scatter', mode = 'lines',
                                           line = list(color='blue') , name='Suscetíveis', visible = 'legendonly')
        
        # expostos
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$E, name="Expostos", mode= 'lines',
                                           line = list(color='orange'),  visible = 'legendonly')
        
        
        fig_proj <- fig_proj %>% layout(title='Projeção para 3 Dias', hovermode = TRUE, spikedistance = -1,
                                        xaxis = list(title='Dias'),
                                        yaxis = list(title='Casos'))
        
    })
    
    output$PROJECAO_proj5 <- renderPlotly({
        
        N <- 200000000
        E <- 1000
        I <- 500
        R <- 0
        S <- N-E-I-R
        tempo_projecao = 5 + length(dados$casos_acumulados)
        
        
        ### criando dataframe auxiliar para o plot
        
        dias <- c(0:tempo_projecao)
        
        valores_nulos <- rep(NaN, (length(dias) - length(dados$casos_acumulados)))
        
        casos_ext = append(dados$casos_acumulados, valores_nulos)
        
        x <- data.frame("dias" = dias, "casos_acumulados" = casos_ext)
        x$dias = dados$data[[1]] + x$dias
        
        # Parameters and initial conditions.
        seir <- SEIR(pars = c(mu = 0, beta = BETA, sigma = 0.2, gamma = 0.1),
                     init = c(S= S/N, E = E/N, I= I/N, R = R/N),
                     time = 0:tempo_projecao)
        
        
        # casos reais
        fig_proj <- plot_ly(x = ~x$dias)
        

        fig_proj <- fig_proj %>% add_trace(y = ~x$casos_acumulados, name="Dados Reais", mode= 'markers',
                                           marker = list(size = 7,
                                                         color = 'rgba(255, 170, 170, .9)',
                                                         line = list(color = 'rgba(152, 0, 0, .8)',
                                                                     width = 2)))
        
        # acumulados projecao (recuperados + infectados)
        fig_proj <- fig_proj %>% add_trace(y=~N*(seir$results$R + seir$results$I), name="Recuperados + Infectados", mode= 'lines',
                                           line = list(color='gray', width = 4))
        
        
        # infecciosos
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$I, name="Infecciosos", mode= 'lines',
                                           line = list(color='red'), visible = 'legendonly')
        
        # recuperados
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$R, name="Recuperados", mode= 'lines',
                                           line = list(color='green'), visible = 'legendonly')
        
        # suscetiveis
        fig_proj <- fig_proj %>% add_trace(y = ~N*seir$results$S, type = 'scatter', mode = 'lines',
                                           line = list(color='blue') , name='Suscetíveis', visible = 'legendonly')
        
        # expostos
        fig_proj <- fig_proj %>% add_trace(y=~N*seir$results$E, name="Expostos", mode= 'lines',
                                           line = list(color='orange'),  visible = 'legendonly')
        
        
        fig_proj <- fig_proj %>% layout(title='Projeção para 5 Dias', hovermode = TRUE, spikedistance = -1,
                                        xaxis = list(title='Dias'),
                                        yaxis = list(title='Casos'))
        
    })
    
    output$CALCULADORA_seir <- renderPlotly({
        
        N <- input$N
        E <- input$E
        I <- input$I
        R <- 0
        S <- N-E-I-R
        
        # Parameters and initial conditions.
        seir <- SEIR(pars = c(mu = 0, beta = input$beta, sigma = input$sigma, gamma = input$gamma),
                     init = c(S= S/N, E = E/N, I= I/N, R = R/N),
                     time = 0:input$tempo)
        
        # suscetiveis
        fig <- plot_ly(x = ~seir$results$time, y=~N*seir$results$S, type = 'scatter', mode = 'lines',
                       name = 'Suscetíveis', line = list(color='blue', width = 3))
        
        # expostos
        fig <- fig %>% add_trace(y=~N*seir$results$E, name="Expostos", mode= 'lines',
                                 line = list(color='orange'))
        
        # infecciosos
        fig <- fig %>% add_trace(y=~N*seir$results$I, name="Infecciosos", mode= 'lines',
                                 line = list(color='red'))
        
        # recuperados
        fig <- fig %>% add_trace(y=~N*seir$results$R, name="Recuperados", mode= 'lines',
                                 line = list(color='green'))
        
        fig <- fig %>% layout(title='Modelo SEIR', hovermode = TRUE, spikedistance = -1,
                              xaxis = list(title='Dias'),
                              yaxis = list(title='Casos'))
        
    })
    
    
    output$CALCULADORA_seirhm <- renderPlotly({
        N <- input$N
        I <- input$I
        R <- 0
        E <- input$E
        H <- 0
        M <- 0
        S <- N-E-I-R
    
        alpha = input$alpha
        lambda = input$lambda
        rho = input$rho
     
        
        sir_equations <- function(tempo, variaveis, parametros){
            with(as.list(c(variaveis, parametros)), {
                dS = -beta*S*I 
                dE = beta*S*I - sigma*E
                dI = sigma*E - (1-alpha)*gamma*I - alpha*gamma*I
                dR = alpha*gamma*I + (1-lambda)*rho*H
                dH = (1-alpha)*gamma*I - lambda*rho*H - (1-lambda)*rho*H
                dM = lambda*H*rho
                return(list(c(dS, dE, dI, dR,dH, dM)))
            })
        }
        
        sir_values_1 <- reactive({
            ode(y=c(S = S/N, E = E/N, I = I/N, R = R/N,H = H/N, M = M/N),
                times = seq(0, input$tempo),
                func = sir_equations,
                parms =  c(beta = input$beta, sigma = input$sigma, gamma = input$gamma))
            
        })
        
        seir <- as.data.frame(sir_values_1())
        
        fig <- plot_ly(x = ~seir$time, y=~N*seir$S, type = 'scatter', mode = 'lines',
                       name = 'Suscetíveis', line = list(color='blue', width = 3))
        
        fig <- fig %>% add_trace(y=~N*seir$R, name="Curados", mode= 'lines',
                                 line = list(color='green', width = 3))
    
        
        fig <- fig %>% add_trace(y=~N*seir$I, name="Infectados", mode= 'lines',
                                 line = list(color='red', width = 3))
        
        
        fig <- fig %>% add_trace( y=~N*seir$E, name="Expostos", mode= 'lines',
                                  line = list(color='orange', width = 3))
        
        
        fig <- fig %>% add_trace(y=~N*seir$M, name="Mortos", mode= 'lines',
                                 line = list(color='black'))
        
        fig <- fig %>% add_trace(y=~N*seir$H, name="Hospitalizados", mode= 'lines',
                                 line = list(color='gray'))
        
        
        fig <- fig %>% layout(title='Modelo SEIRHM', hovermode = TRUE, spikedistance = -1,
                              xaxis = list(title='Dias'),
                              yaxis = list(title='Casos'))
        
        
    })
    
    
    
    output$DADOS_tabela <- renderPrint({
        orig <- options(width = 500)
        print(tail(dados %>% select(c(data,casos_acumulados,casos_novos,obitos_acumulados,obitos_novos,rolling_mean_casos_novos
                                      ,rolling_mean_casos,rolling_mean_obitos_acumulados,rolling_mean_obitos_novos,casos_ativos)
        ), input$maxrows), row.names = FALSE)
        
    })
    
    output$DADOS_tabela_pr <- renderPrint({
        orig <- options(width = 500)
        print(tail(dados_pr %>% select(c(data,casos_acumulados,casos_novos,obitos_acumulados,obitos_novos,rolling_mean_casos_novos
                                         ,rolling_mean_casos,rolling_mean_obitos_acumulados,rolling_mean_obitos_novos,casos_ativos)
        ), input$maxrows), row.names = FALSE)
        
    })
}


shinyApp(ui = ui, server = server)
