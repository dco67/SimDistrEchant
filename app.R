library(shiny)
library(ggplot2)
library(confintr)
library(DT)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(plotrix)
library(latex2exp)
library(dplyr)


 source("global.R")

ui <- pageWithSidebar(
  headerPanel("Inférence Statistique: Un échantillon"),

  sidebarPanel(
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
    
    conditionalPanel(
      condition="input.tabselected==1",
      h4(" "),
      fluidRow(
        radioButtons("dist",
                     label = h4("Distribution $(Population)$"),
                     selected = "Normale",
                     choices = c("Uniforme", "Normale", "Exponentielle", "Chi-Carré"),
                     inline = TRUE
        ),
        conditionalPanel(
          condition = "input.dist == 'Uniforme'",
          fluidRow(
            column(
              6,
              numericInput("li",
                           "Limite Inférieure",
                           min = 0,
                           max = 100,
                           value = 0,
                           step = 1
              )
            ),
            column(
              6,
              numericInput("ls",
                           "Limite Supérieure",
                           min = 0,
                           max = 100,
                           value = 15,
                           step = 1
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.dist == 'Normale'",
        fluidRow(
          column(
            6,
            numericInput("mu",
                         "$\\mu$",
                         min = 0,
                         max = 100,
                         value = 100,
                         step = 1
            )
          ),
          column(
            6,
            numericInput("sd",
                         "$\\sigma$",
                         min = 1,
                         max = 100,
                         value = 15,
                         step = 0.1
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.dist == 'Exponentielle'",
        fluidRow(
          column(
            6,
            numericInput("lambda",
                         "$$\\lambda$$",
                         min = 0.1,
                         max = 25,
                         value = 1,
                         step = 0.1
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.dist == 'Chi-Carré'",
        fluidRow(
          column(
            6,
            numericInput("dl",
                         "Degrés de liberté",
                         min = 1,
                         max = 100,
                         value = 5,
                         step = 1
            )
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          selectInput("stat",
                      label = h4("Statistique étudiée"),
                      selected = "Moyenne",
                      choices = c(
                        "Moyenne" = "mean",
                        "Médiane" = "median",
                        "Variance" = "var",
                        "Écart-type" = "sd",
                        "Coefficient de variation" = "CV",
                        "Indice de symétrie" = "skewness",
                        "Indice de voussure" = "kurtosis",
                        "Premier Quartile" = "Q1",
                        "Troisième Quartile" = "Q3",
                        "Maximum" = "max",
                        "Minimum" = "min"
                      )
          )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.stat == 'var' || input.stat == 'sd'",
                 checkboxInput("csq",
                               "Transformation: $\\chi^2$",
                               value = FALSE)
               )
        )
      ),
      sliderInput("n",
                  label = h4("Taille de l'échantillon"),
                  value = 15,
                  min = 1,
                  max = 200,
                  step = 1
      ),
      sliderInput("ns",
                  label = h4("Nombre d'échantillons:"),
                  min = 1000,
                  max = 100000,
                  value = 10000,
                  step = 1000
      ),
      actionBttn("go",
                   "Échantillonner",
                   color = "success",
                   style = "stretch",
                   icon = icon("sliders"),
                   block = TRUE
      ),
      p(),
      
      wellPanel(
        helpText(
          h5("Cette application permet la simulation d'une distribution d'échantillonnage d'une statistique sélectionnée à partir du ",
          "du tableau de bord. On peut spécifier la forme de la distribution de la population ainsi que ses paramètres. ",
          "Sélectionnez la taille des échantillons ainsi que leur nombre, puis cliquez sur ", strong("Échantillonner"), " pour afficher ",
          "la distribution d'échantillonnage appropriée.  On peut alors obtenir la probabilité que la statistique étudiée se trouve à ",
          "l'intérieur d'un intervalle donné, spécifié par le curseur qui se trouve sous le graphique de la distribution d'échantillonnage. ",
          )
        )
      ),
      
      
      wellPanel(
        style = "background: lightblue",
        fluidRow(
          column(
            8,
            a(h4("Par Daniel Coulombe, Ph.D.")),
            p("Institut des Sciences, des Technologies et des Études Avancées d'Haiti"),
            p("2023")
          ),
          column(
            4,
            tags$a(
              href = "https://isteah.org",
              tags$img(
                src = "ISTEAH_LOGO.png",
                title = "ISTEAH",
                width = "160",
                height = "140"
              )
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition="input.tabselected==2",
      " ",
      wellPanel(
        radioButtons("param",
                     "Paramètre estimé:",
                     choices = c("Moyenne", "Proportion", "Variance"),
                     selected = "Moyenne",
                     inline = TRUE),
        fluidRow(
          column(6,
                 sliderInput("k",
                             "Nombre d'échantillons:",
                             min = 1,
                             max = 100,
                             value = 15,
                             step=1)
                 
          ),
          column(6,
                 sliderInput(inputId = "conf.level",
                             label = strong("Niveau de Confiance (%)"),
                             value = 95,
                             min = 80,
                             max = 99)
          )
          
        )
      ),
      wellPanel(
        strong("Paramêtre[s] de la population:"),

        conditionalPanel(
          condition = "input.param == 'Proportion'",
          fluidRow(
            column(6,
                   sliderInput("pr2",
                               "Probabilité d'un succès",
                               min = 0.01,
                               max = 0.99,
                               step = 0.01,
                               value = 0.7)
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.param != 'Proportion'",
          fluidRow(
            column(6,
                   sliderInput(inputId = "mu2",
                               label = strong("$\\mu$"),
                               value = 0,
                               min = 0,
                               max = 100)
            ),
            column(6,
                   sliderInput(inputId = "sig2",
                               label = strong("$\\sigma$"),
                               value = 1,
                               min = .01,
                               max = 30)
            )
          )
        ),
        
      fluidRow(
        column(6,
               sliderInput(inputId = "n2",
                           label = strong("Taille de l'échantillon"),
                           value = 25,
                           min = 5,
                           max = 200),
               
        ),
        column(6,
               conditionalPanel(
                 condition = "input.param == 'Moyenne'",
                 radioButtons("sigmaknown",
                              "Sigma...",
                              choices = c("connu", "inconnu"),
                              selected = "connu",
                              inline = TRUE)
               )
        )
      ),
      
      actionBttn(
        inputId = "simulate",
        label = "Simuler",
        color = "success",
        style = "stretch",
        icon = icon("sliders"),
        block = TRUE
      ),
      actionBttn(
        inputId = "reset",
        label = "Ré-initialiser",
        color = "danger",
        style = "stretch",
        icon = icon("gear"),
        block = TRUE
      ),
      p(),
      
      wellPanel(
        helpText(
          h5("Cette application tire k échantillons d'une population dont la distribution est soit normale avec une moyenne", 
             "$\\mu$ et un écart-type $\\sigma$, ou de Bernouilli, avec $\\pi$. On peut manipuler les paramètres de la population",
             "à l'aide des curseurs du tableau de bord.  ",
             "Pour chacun des échantillons tirés, on affiche l'intervalle de confiance du paramètre étudié [$\\mu$, $\\pi$ ou $\\sigma^2$].",
             "Les intervalles rouges ne capturent pas le paramètre de la population. La proportion de capture est également affichée.",
             "Cliquez sur ", strong("Simuler"), " pour lancer la simulation, et sur ", strong("Ré-initialiser"), " pour remettre les compteurs à leur état initial.")
        )
      ),
      
      p(),
      wellPanel(
        style = "background: lightblue",
        fluidRow(
          column(
            8,
            a(h4("Par Daniel Coulombe, Ph.D.")),
            p("Institut des Sciences, des Technologies et des Études Avancées d'Haiti"),
            p("2023")
          ),
          column(
            4,
            tags$a(
              href = "https://isteah.org",
              tags$img(
                src = "ISTEAH_LOGO.png",
                title = "ISTEAH",
                width = "160",
                height = "140"
              )
            )
          )
        )
      )
)
    
    ),
  
  conditionalPanel(
    condition="input.tabselected==3",
    " ",
    wellPanel(
      fluidRow(
        column(8,
      radioButtons("param3",
                   "Paramètre estimé:",
                   choices = c("Moyenne", "Proportion", "Variance"),
                   selected = "Moyenne",
                   inline = TRUE)
      ),
      column(4,
             conditionalPanel(
               condition = "input.param3 == 'Moyenne'",
               checkboxInput("knownSD",
                             "\u03C3 Connu:",
                             value = FALSE)
             )
             )

),

 
     
      fluidRow(
        column(6,
               sliderInput(inputId = "conf.level3",
                           label = strong("Niveau de Confiance (%)"),
                           value = 95,
                           min = 80,
                           max = 99)
        ),
        column(6,
        conditionalPanel(
          condition = "input.param3 == 'Moyenne'",

               checkboxGroupInput("showcurve",
                                  "Afficher:",
                                  choices = c("Distribution Empirique" = "empir", 
                                              "Distribution de la Population" = "theor",
                                              "Intervalle de Confiance" = "IC"),
                                  selected = NULL,
                                  inline = FALSE)
        
      )
    )
    )
),
    wellPanel(

      fluidRow(
        column(6,
               sliderInput(inputId = "n3",
                           label = strong("Taille de l'échantillon"),
                           value = 25,
                           min = 5,
                           max = 500,
                           step = 5),
        ),
        column(6,
               sliderInput(inputId = "k3",
                           label = strong("Nombre de Ré-Échantillonnages"),
                           value = 10000,
                           min = 1000,
                           max = 100000,
                           step = 1000)
        )
        
      ),
      
      actionBttn(
        inputId = "simulate3",
        label = "Simuler",
        color = "success",
        style = "stretch",
        icon = icon("sliders"),
        block = TRUE
      )
      ),
      
      wellPanel(
        withMathJax(
        helpText(
          h5("Cette application illustre l'estimation statistique utilisant le ", strong("ré-échantillonnage"), ". On tire un échantillon d'une certaine taille ",
          "d'une population normalement distribuée [estimation de $\\mu$ et de $\\sigma^2$], ou de Bernouilli [estimation de $\\pi$], ",
          "dont les paramètres sont inconnus.  On procède alors au ré-échantillonnage pour déterminer l'intervalle de confiance du paramètre estimé. ",
          "Pour fins de comparaison, on obtient également l'intervalle calculé de manière analytique.. ",
          "L'option d'affichage inclus la superposition de la courbe appropriée, calculée à partir des paramètres exacts de la population [distribution théorique], ",
          " ou à partir des valeurs estimées. Dans les deux cas, la mécanique de calcul est illustrée. Cliquez sur ", strong("Simuler"), "pour lancer ",
          "la simulation.")
        )
        )
      ),
      
      p(),
      wellPanel(
        style = "background: lightblue",
        fluidRow(
          column(
            8,
            a(h4("Par Daniel Coulombe, Ph.D.")),
            p("Institut des Sciences, des Technologies et des Études Avancées d'Haiti"),
            p("2023")
          ),
          column(
            4,
            tags$a(
              href = "https://isteah.org",
              tags$img(
                src = "ISTEAH_LOGO.png",
                title = "ISTEAH",
                width = "160",
                height = "140"
              )
            )
          )
        )
      )
      ),

conditionalPanel(
  condition="input.tabselected==4",
  " ",
  wellPanel(style = "background: lightyellow",

    fluidRow(
      column(4,
             radioButtons("param4",
                          "Paramètre étudié:",
                          choices = c("Moyenne", "Proportion", "Variance"),
                          selected = "Moyenne",
                          inline = FALSE)
             ),
             column(4,
                    radioButtons("direction",
                                 "Directionalité  $(H_1)$:",
                                 choices = c("<" = "1",
                                             ">" = "2",
                                             "\u2260" = "3"),
                                 selected = "3")
                    
             ),
      
      column(4,
             conditionalPanel(
               condition = "input.param4 != 'Proportion'",
               numericInput("hyppar",
                            "Paramètre proposé:",
                            min = 0,
                            value = 50,
                            step = 0.1)
             ),
             
             conditionalPanel(
               condition = "input.param4 == 'Proportion'",
               numericInput("hypPR",
                            "Paramètre proposé:",
                            min = 0.01,
                            max = 0.99,
                            step = 0.01,
                            value = 0.5)
               )
             )
    ),
    fluidRow(
      column(9,
             sliderInput(inputId = "sig.level4",
                         label = strong("Niveau de Signification"),
                         value = 0.05,
                         min = 0.001,
                         max = 0.2,
                         step = 0.001)
      ),
      
      column(3,
             conditionalPanel(
               condition = "input.param4 == 'Moyenne'",
               checkboxInput("knownSD4",
                             "\u03C3 Connu:",
                             value = FALSE)
             )
      )
      )
    ),
  
  wellPanel(style = "background: lightyellow",

    fluidRow(
      h5(strong("Afficher:")), p(),
      column(4,
             checkboxInput("probline",
                           "Intervalle de valeurs probables",
                           value = FALSE)),
      column(4,
             checkboxInput("icnf",
                           "Intervalle de Confiance",
                           value = FALSE)
                           ),
      column(4,
             conditionalPanel(
             condition = "input.param4 == 'Moyenne'",
             checkboxInput("tcurve",
                           "Distribution théorique",
                           value = FALSE)
      )
    )
    ),
    
    conditionalPanel(
      condition = "input.param4 != 'Variance'",
      fluidRow(
        column(12,
               radioButtons("showprob",
                            "Illustrer:",
                            choices = c("Zone de rejet de $H_0$" = "rjct",
                                        "Valeur de $p$" = "pval"),
                            selected = "rjct",
                            inline = TRUE)
             )
      )
      ),
    
#    conditionalPanel(
#      condition = "input.diagwhat == 'vars'",
#      fluidRow(
#        column(12,
#               radioButtons("diagwhat",
#                            "Tracer la distribution des:",
#                            choices = c("$s^2$" = "vars", 
#                                        "$\\chi^2$" = "chisq"),
#                            selected = "vars",
#                            inline = TRUE)
#        )
#        )
#      )
    ),

  wellPanel(style = "background: lightyellow",

    
    fluidRow(
      column(6,
             sliderInput(inputId = "n4",
                         label = strong("Taille de l'échantillon"),
                         value = 25,
                         min = 5,
                         max = 500,
                         step = 5),
      ),
      column(6,
             sliderInput(inputId = "k4",
                         label = strong("Nombre de Ré-Échantillonnages"),
                         value = 10000,
                         min = 1000,
                         max = 100000,
                         step = 1000)
      )
      
    ),
    
    actionBttn(
      inputId = "simulate4",
      label = "Échantillonner",
      color = "success",
      style = "stretch",
      icon = icon("sliders"),
      block = TRUE
    )
  ),
  
  wellPanel(
    withMathJax(
      helpText(
        h5("Cette application illustre un test d'hypothèse à partir d'un échantillon unique. Une procédure empirique, le ", strong("ré-échantillonnage"), ", et une ",
        "approche classique sont appliquées sur les données obtenues pour un échantillon tiré aléatoirement ",
           "d'une population normalement distribuée [hypothèse condernant $\\mu$ et de $\\sigma^2$], ou de Bernouilli [hypothèse concernant $\\pi$], ",
           "dont les paramètres sont inconnus.  On procède alors au ré-échantillonnage pour obtenir la distribution d'échantillonnage appropriée et conclure le test d'hypothèse. ",
           "Pour fins de comparaison, on effectue également le test de manière analytique. ",
           "L'option d'affichage inclus la superposition de la courbe appropriée, calculée à partir des paramètres exacts de la population [distribution théorique], ",
           " Cliquez sur ", strong("Échantillonner"), "pour lancer la simulation.")
      )
    )
  ),
  
  p(),
  wellPanel(
    style = "background: lightblue",
    fluidRow(
      column(
        8,
        a(h4("Par Daniel Coulombe, Ph.D.")),
        p("Institut des Sciences, des Technologies et des Études Avancées d'Haiti"),
        p("2023")
      ),
      column(
        4,
        tags$a(
          href = "https://isteah.org",
          tags$img(
            src = "ISTEAH_LOGO.png",
            title = "ISTEAH",
            width = "160",
            height = "140"
          )
        )
      )
    )
  )

)
),
 
  
  mainPanel(
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
    
    
    tabsetPanel(
      tabPanel("Distribution d'échantillonnage", 
      value=1, 
      
      plotOutput("parentPlot"),
      
      withSpinner(
      plotOutput("distPlot"),
      type = 4),
      
      fluidRow(
        column(
          10,
          offset = 1,
          uiOutput("probbtw2")
        )
      ),
      fluidRow(
        column(9,
               offset = 3,
               textOutput("calc32"),
               tags$head(tags$style("#calc32{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"))
        )
      )
      ),
  
      tabPanel("Intervalle de Confiance: Concept", 
               value=2,
               plotOutput("PopDist"),
               plotOutput("plot", width = "100%"),
               p(),
               p(),
               p(),
               dataTableOutput("resdat")
               ),
    
      
      tabPanel("Estimation Statistique", 
               
               value=3, 
               withSpinner(
               plotOutput("bootstr"),
               type = 4),
               
               conditionalPanel(
                 condition = "input.param3 == 'Moyenne'",
                 fluidRow(
                   column(6,
                          dataTableOutput("databootMoy")
                         ),
                   column(6,
                          uiOutput("calculMoy")
                          )
                   )
                 ),
               
               conditionalPanel(
                 condition = "input.param3 == 'Proportion'",
                 fluidRow(
                   column(6,
                          dataTableOutput("databootProp")
                   ),
                   column(6,
                          uiOutput("calculProp")
                   )
                 )
               ),
               
               conditionalPanel(
                 condition = "input.param3 == 'Variance'",
                 fluidRow(
                   column(6,
                          dataTableOutput("databootVar")
                   ),
                   column(6,
                          uiOutput("calculVar")
                   )
                 )
               ),
               
               conditionalPanel(
                 condition = "output.calculMoy || output.calculProp || output.calculVar",
                 fluidRow(
                   column(6, offset = 6,
                        wellPanel(
                          style = "background: lightblue",
                          uiOutput("interpret"))
               )
    )
    )
    ),

    tabPanel("Vérification d'hypothèse", 
             value=4, 
             
             conditionalPanel(
               condition = "input.param4 == 'Moyenne'",
               fluidRow(
                 column(6,
                        dataTableOutput("dataHTMoy")
                        ),
                 
                 column(6,
                        withSpinner(
                          plotOutput("meantstr4"),
                          type = 4)
                        )
                 ),

               fluidRow(
                 column(6,
                        uiOutput("calculMoy4"),
                        ),
                 column(6,
                        plotOutput("normDistrib")

                        )
                 ),
               uiOutput("interprMoy")
               ),
             
             conditionalPanel(
               condition = "input.param4 == 'Proportion'",
               fluidRow(
                 column(6,
                        dataTableOutput("dataHTProp")
                        ),
  
                 column(6,
                        withSpinner(
                          plotOutput("proptstr4"),
                          type = 4)
                 )
                 ),
               fluidRow(
                 column(6,
                        uiOutput("calculProp4")
                        ),
                 column(6,
                        plotOutput("normDistrib2")
                        )
                 ),
                        uiOutput("interprProp")

             ),
             
             conditionalPanel(
               condition = "input.param4 == 'Variance'",
               fluidRow(
                 column(6,
                        dataTableOutput("dataHTVar")
                 ),
                 column(6,
                        withSpinner(
                        plotOutput("vartstr4"),
                        type = 4)
                        )

                 ),
               
               fluidRow(
                 column(6,
                        uiOutput("calculVar4")
                 ),
                 column(6,
                        plotOutput("chi2Distrib")
                        )
               ),
                 
               uiOutput("interprVar")
               )
             ),


    id = "tabselected"
    )
  )
  )
  
server <- function(input, output, session) {
  
  vals <- reactiveValues()
  
  dat <- eventReactive(input$go, {
    if (input$dist == "Uniforme") {
      vals$par <- c(input$li, input$ls)
    }
    if (input$dist == "Normale") {
      vals$par <- c(input$mu, input$sd)
    }
    if (input$dist == "Exponentielle") {
      vals$par <- c(input$lambda, NA)
    }
    if (input$dist == "Chi-Carré") {
      vals$par <- c(input$dl, NA)
    }
    
    SimData(vals$par[1],
            vals$par[2],
            n = input$n,
            ng = as.integer(input$ng),
            dist = input$dist,
            ns = input$ns
    )
  })
  
  x <- reactive(apply(dat(), MARGIN = 2, input$stat))
  
  output$distPlot <- renderPlot({
    withMathJax()
    lbl <- data.frame(
      c("mean", "median", "var", "sd", "CV", "skewness", "kurtosis", "Q1", "Q3", "max", "min"),
      c("Moyennes", "Médianes", 
        ifelse(input$csq, "Variances Std (\u03C7 \u00B2 )", "Variances"), 
        ifelse(input$csq, "Écarts-types (sqrt[\u03C7 \u00B2])", "Écarts-types"), 
        "Coefficients de variation", "Symétries", "Voussures", "Premiers Quartiles", "Troisièmes Quartiles", "Maxima", "Minima")
    )
    colnames(lbl) <- c("fns", "label")
    lblf <- lbl[which(lbl[, 1] == input$stat), 2]
    ttl <- paste("Distribution d'échantillonnage \n des ", lblf)
    
    x <- x()
    if((input$stat == "var" || input$stat == "sd") & input$csq){
      v <- 
        if(input$dist == "Uniforme"){(input$max - input$min)^2 / 12}
      else if(input$dist == "Normale"){input$sd^2}
      else if(input$dist == "Exponentielle"){1}
      else {2 * input$dl}
      
      if(input$stat == "var"){x <- (input$n - 1) * x() / v}
      else {x <- sqrt((input$n - 1) * x() / v)}
    }
    
    hist_breaks <- hist(x, breaks = 50)$breaks
    color_list <- rep("darkgrey", length(hist_breaks))
    color_list[hist_breaks < input$ll2[2] & hist_breaks > input$ll2[1]] <- "lightblue"
      
    hist(x,
         breaks = 50,
         border = "darkgreen",
         freq = FALSE,
         xlab = lblf,
         ylab = "Densité",
         main = ttl,
         col = color_list,
         cex.main = 1.5,
         cex.lab = 1.5,
         cex.axis = 1.2
    )
    dx <- density(x)
    lines(dx,
          lwd = 2,
          col = "red"
    )
    legend("topright",
           legend = c(
             paste("\u03BC = ", round(mean(x), 2)),
             paste("Md = ", round(median(x), 2)),
             paste("\u03C3 = ", round(sd(x), 2)),
             paste("Sk = ", round(skewness(x), 2)),
             paste("Ku = ", round(kurtosis(x) - 3, 2))
           ),
           cex = 1.2,
           bg = "lightyellow",
           title = "Paramètres:"
    )
    box(col = "darkgreen",
        #        which = "figure",
        lwd = 2)
  })
  
  output$probbtw2 <- renderUI({
    #   x <- apply(dat(), MARGIN = 2, input$stat)
    #    mu <- mean(x)
    #    sigma <- sd(x)
    #    minim <- ifelse(input$dist == "Exponentielle" || input$dist == "Chi-Carré", 0, round(mu - 3 * sigma, 1))
    
    x <- x()
    if((input$stat == "var" || input$stat == "sd") & input$csq){
      v <- 
        if(input$dist == "Uniforme"){(input$max - input$min)^2 / 12}
      else if(input$dist == "Normale"){input$sd^2}
      else if(input$dist == "Exponentielle"){1}
      else {2 * input$dl}
      
      if(input$stat == "var"){x <- (input$n - 1) * x() / v}
      else {x <- sqrt((input$n - 1) * x() / v)}
    }
    
    sliderInput("ll2",
                label = " ",
                value = c(round(min(x), 1), round(max(x), 1)),
                min = round(min(x), 1),
                max = round(max(x), 1),
                width = "100%"
    )
  })
  
  output$calc32 <- renderText({
    lbl <- data.frame(
      c("mean", "median", "var", "sd", "CV", "skewness", "kurtosis", "Q1", "Q3", "max", "min"),
      c("Moyenne", "Médiane", "Variance", "Écart-type", "Coeff. variation", "Symétrie", "Voussure", "Q1", "Q3", "Max", "Min")
    )
    lblf <- lbl[which(lbl[, 1] == input$stat), 2]
    
    if (is.null(input$ll2)) {
      return()
    }
    x <- x()
    if((input$stat == "var" || input$stat == "sd") & input$csq){
      v <- 
        if(input$dist == "Uniforme"){(input$max - input$min)^2 / 12}
      else if(input$dist == "Normale"){input$sd^2}
      else if(input$dist == "Exponentielle"){1}
      else {2 * input$dl}
      
      if(input$stat == "var"){x <- (input$n - 1) * x() / v}
      else {x <- sqrt((input$n - 1) * x() / v)}
    }
    
    mu <- mean(x)
    sigma <- sd(x)
    paste(
      "P(", input$ll2[1], "\u2264 ", lblf, " \u2264", input$ll2[2], ") = ",
      round(mean(x <= input$ll2[2]) - mean(x <= input$ll2[1]), 4)
    )
  })
  
  output$parentPlot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    if (dist == "Uniforme") {
      min <- input$li
      max <- input$ls
      moy <- (min + max) / 2
      med <- (min + max) / 2
      sdx <- sqrt((max - min)^2 / 12)
      sk <- 0
      ku <- -6 / 5
      x <- seq(from = min, to = max, length = 2)
      plot(
        x = x,
        y = dunif(
          x = x,
          min = min,
          max = max
        ),
        main = paste("Distribution de la Population:", dist),
        xlab = "X",
        ylab = "Densité",
        type = "l",
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.2
      )
    } else if (dist == "Normale") {
      mean <- input$mu
      sd <- input$sd
      lambda <- input$lambda
      moy <- mean
      med <- mean
      sdx <- sd
      sk <- 0
      ku <- 0
      x <- seq(
        from = mean - 4 * sd,
        to = mean + 4 * sd,
        length = 1000
      )
      plot(
        x = x,
        y = dnorm(
          x = x,
          mean = mean,
          sd = sd
        ),
        main = paste("Distribution de la Population:", dist),
        xlab = "X",
        ylab = "Densité",
        type = "l",
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.2
      )
    } else if (dist == "Exponentielle") {
      lambda <- input$lambda
      moy <- 1 / lambda
      med <- log(2) / lambda
      sdx <- 1 / lambda
      sk <- 2
      ku <- 6
      x <- seq(
        from = 0,
        to = 5 / lambda,
        length = 1000
      )
      plot(
        x = x,
        y = dexp(
          x = x,
          rate = lambda
        ),
        main = paste("Distribution de la Population:", dist),
        xlab = "X",
        ylab = "Densité",
        type = "l",
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.2
      )
    } else if (dist == "Chi-Carré") {
      df <- input$dl
      moy <- df
      med <- df * (1 - 2 / (9 * df))^3
      sdx <- sqrt(2 * df)
      sk <- sqrt(8 / df)
      ku <- 12 / df
      x <- seq(
        from = 0,
        to = df + 4 * sqrt(2 * df),
        length = 1000
      )
      plot(
        x = x,
        y = dchisq(
          x = x,
          df = df
        ),
        main = paste("Distribution de la Population:", dist),
        xlab = "X",
        ylab = "Densité",
        type = "l",
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.2
      )
    }
    
    legend("topright",
           legend = c(
             paste("\u03BC = ", round(moy, 2)),
             paste("Md = ", round(med, 2)),
             paste("\u03C3 = ", round(sdx, 2)),
             paste("Sk = ", round(sk, 2)),
             paste("Ku = ", round(ku, 2))
           ),
           bg = "lightblue",
           title = "Paramètres:",
           cex = 1.2
    )
    abline(
      v = moy,
      lwd = 2,
      lty = 2,
      col = "darkgreen"
    )
    
    box(col = "darkgreen",
        #        which = "figure",
        lwd = 2)
  })
  
  ################## Estimation
  
  parsim <- reactiveValues()
  
#  simdat <- eventReactive(input$simulate | input$simulate == 0, {
  
  observeEvent(input$simulate, {
    
   tmp <-  gensim2(input$k,
                             input$param,
                             par1 = ifelse(input$param == "Proportion", input$pr2, input$mu2),
                             par2 = ifelse(input$param == "Proportion", 0, input$sig2),
                             input$n2)

   parsim$simdat <- tmp[[1]]
   parsim$parMU <- tmp[[2]]
   parsim$parSIG <- tmp[[3]]
   })

  

  
  tcov <- reactiveVal(0)
  tcovtot <- reactiveVal(0)
  
  observeEvent(input$reset, {
    tcov(0)
    tcovtot(0)
  })
    
  output$plot <- renderPlot({

    datsim <- matrix(NA, nrow = input$k, ncol = 4)
    cl <- input$conf.level / 100
    alpha <- (1 - cl) / 2
    probs <- c(alpha, cl + alpha )
    
  simdat <- parsim$simdat

    
    if(input$param == "Moyenne" & input$sigmaknown == "inconnu"){
      if(is.null(parsim$simdat)){return(NULL)}
      statdat <- apply(simdat, MARGIN = 2, ci_mean, probs = probs) } else
        if(input$param == "Moyenne" & input$sigmaknown == "connu"){
          if(is.null(parsim$simdat)){return(NULL)}
          statdat <- apply(simdat, MARGIN = 2, ci_mean, probs = probs, type = "Wald")
        }
    if(input$param == "Variance"){
      if(is.null(parsim$simdat)){return(NULL)}
      statdat <- apply(simdat, MARGIN = 2, ci_var, probs = probs) 
}
      
    if(input$param == "Proportion"){
      if(!all(simdat %in% 0:1)){return(NULL)}
      if(is.null(parsim$simdat)){return(NULL)}
      statdat <- apply(simdat, MARGIN = 2, ci_proportion, probs = probs) }
    
      for(i in 1:input$k){
        datsim[i, ] <- c(i, statdat[[i]]$estimate, statdat[[i]]$interval)
      }

    datsim <- as.data.frame(datsim)
    if(input$param == "Variance"){datsim <-  (input$n2 - 1) * datsim / input$sig2^2}
    colnames(datsim) <- c("Échant", "Estim", "LimInf", "LimSup")
    
    parsim$datsim <- datsim
    
    mu_stat <- if(input$param == "Proportion"){input$pr2} else {
      if(input$param == "Moyenne"){input$mu2} else {
        input$n2 -1}
    }

    se_stat <- if(input$param == "Proportion"){sqrt(input$pr2 * (1 - input$pr2) / input$n2)} else {
      if(input$param=="Moyenne"){input$sig2 / sqrt(input$n2)} else
      {sqrt(2 * (input$n2 - 1))}} 
    
    pp <- c("Moyenne", "Proportion", "Variance")
    pps <- c("\u03BC", "\u03C0", "\u03C3 \u00B2")
    ppp <- pps[which(input$param == pp)]
    
    plot(datsim[, 2], 
         1:nrow(datsim), 
         pch = 19,
         col = "darkblue",
         cex = 1.2,
         xlim = c(mu_stat - 3 * se_stat, mu_stat + 3 * se_stat),
         ylim = c(1, nrow(datsim)),
         xlab = input$param,
         ylab = "Échantillon",
         main = paste(input$k, "Intervalles de Confiance (", input$conf.level, "%) - n = ", input$n2),
         cex.lab = 1.2,
         cex.main = 1.5
    )
    cvr <- 0
    for (i in 1:nrow(datsim)){
      if(between(mu_stat, datsim[i, 3], datsim[i, 4])){
        segments(datsim[i, 3], i, datsim[i, 4], i, lwd = 2) 
        cvr <- cvr + 1
      } else {
        segments(datsim[i, 3], i, datsim[i, 4], i, col = "red", lwd = 2) 
      }
    }
    isolate({tcovtot(tcovtot() + input$k)})
    isolate({tcov(tcov() + cvr)})
    
    abline(v=mu_stat, col = "blue")
    legend("topleft", legend = c(paste("IC capture ", ppp(), ": ", round(cvr / input$k * 100, 2), "%"),
                                 paste("Taux Cumulatif = ", round(tcov() / tcovtot() * 100, 3), "%"),
                                 paste("# Échantillons = ", tcovtot())),
           cex = 1.2)
    
  }, height = 400)
  
  output$resdat <- renderDataTable({
    if(is.null(parsim$simdat)){return(NULL)}
    cl <- input$conf.level / 100
    alpha <- (1 - cl) / 2
    probs <- c(alpha, cl + alpha )

    datsim <-parsim$datsim

    datatable(datsim,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                  (paste(input$k, 
                              "échantillons tirés d'une distribution ", 
                              input$distr, 
                              ": Intervalles de confiance pour une ", 
                              input$param)
                      )
                  
                ),
              options = list(dom = "ptr", 
                             pageLength = 10, 
                             lengthMenu = list(c(5, 10, 15, 20)),
                             autoWidth = FALSE,
                             columnDefs = list(list(width = '10px', targets = "_all"))),
              rownames = FALSE,
              class = 'cell-border stripe',) %>%
      formatRound(c(2:4), 4) %>%
      formatStyle(columns = colnames(.), fontSize = '14px')

  })
  
  output$PopDist <- renderPlot({
    mu_stat <- if(input$param == "Proportion"){input$pr2} else {
      if(input$param == "Moyenne"){input$mu2} else {
        input$n2 -1
      }
    }
      {input$mu2} 
    se_stat <- if(input$param == "Proportion"){sqrt(input$pr2 * (1 - input$pr2) / input$n2)} else
      if(input$param=="Moyenne"){input$sig2 / sqrt(input$n2)} else
        {sqrt(2 * (input$n2 - 1))} 

      if(input$param == "Proportion" | input$param == "Moyenne"){
        x <- seq(from = -3 * se_stat + mu_stat, to = 3 * se_stat + mu_stat , by = diff(qnorm(c(0.001, 0.999), mean = mu_stat, sd = se_stat)) / 200)
        y <- dnorm(x, mean = mu_stat, sd = se_stat)} else {
        x <- seq(from = qchisq( 0.001, df = input$n2 - 1), to = qchisq(0.999, df = input$n2 - 1), by = diff(qchisq(c(0.001, 0.999), df=input$n2 - 1)) / 200)
        y <- dchisq(x, df = input$n2 -1)}
  
    cl <- input$conf.level / 100
    qtl <- c((1 - cl)/2, cl + 0.5 * (1 - cl))
    

#    lim <- ifelse(input$param != "Variance", qnorm(qtl, input$mu2, se_stat), qchisq(qtl, input$n2 - 1))
    if(input$param != "Variance"){
      lim <- qnorm(qtl, input$mu2, se_stat)
      dlim <- dnorm(lim, input$mu2, se_stat)
    } else {
      lim <- qchisq(qtl, input$n2 - 1)
      dlim <- dchisq(lim, input$n2 - 1)
    }
    
#    dlim <- ifelse(input$param != "Variance", dnorm(c(1, -1) * lim, input$mu2, se_stat), dchisq(lim, input$n2 - 1))
    
    maintxt <- paste0("Distribution d'échantillonnage des ", input$param, "s")
#    lgd <- ifelse(input$param != "Variance", paste("$\\mu_{\\bar{x}} = ", input$mu2, "$"), 
#                 paste("$\\mu_{\\mu_{\\chi^2} = ", input$n2 - 1, "$"))

    plot(x, 
         y,
         ylab = "Densité", lwd = 2, col = "red",
         xlab = paste0(input$param, "s"),
         type = "l",
         main = maintxt,
         cex.lab = 1.2,
         cex.main = 1.5
)
    abline(v = mu_stat,
           lwd = 2,
           col = "darkgreen",
           lty = 2)
    segments(lim[1], 0, lim[1], dlim[1],
                 col = "darkblue",
                 lwd = 2)
    segments(lim[2], 0, lim[2], dlim[2],
             col = "darkblue",
             lwd = 2)   
    legend("topleft", legend = c(paste("Moyenne = ", ifelse(input$param != "Variance", input$mu2, input$n2 - 1)), 
                                       paste("Err. Std = ", ifelse(input$param != "Variance", round(se_stat, 3), "NA"))))
    
  })
  
  ########################
  ## Tab Panel 3: Ré=échantillonnage
  ########################

    Data <- eventReactive(input$simulate3,{
      
        gensim3(k = input$k3,
                param = input$param3,
                par1 = ifelse(input$param3 != "Proportion", runif(1, 40, 100), runif(1, 0.1, 0.9)),
                par2 = ifelse(input$param3 != "Proportion", runif(1, 5, 30), NA),
                n = input$n3)
    })

  output$bootstr <- renderPlot({
#    if(is.null(Data4()[[1]])){return(NULL)}

    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    std.err <- ifelse(input$knownSD, parSIG, sd(dat)) / sqrt(input$n3) 
    
    pp <- c("Moyenne", "Proportion", "Variance")
    pps <- c("\u03BC", "\u03C0", "\u03C3 \u00B2")
    ppp <- pps[which(input$param3 == pp)]

    cl <- input$conf.level3 / 100
    qtl <- c((1 - cl)/2, cl + 0.5 * (1 - cl))
    
    cv <- ifelse(input$knownSD, qnorm(qtl), qt(qtl, input$n3 - 1))
    
    IClim <- c(-1, 1) * cv * std.err + mean(dat)

    r.stat <- if(input$param3 != "Variance"){sapply(resamples, mean)} else 
      {sapply(resamples, var)}
    intc <- quantile(r.stat, qtl)

    gr1 <- hist(r.stat, 
                breaks="FD", 
                plot=FALSE)
    
    lgd1 <- paste("C[", round(intc[1], 3), "\u2264 ", ppp(), " \u2264 ", round(intc[2], 3), "] = ", input$conf.level3, "%")
    
    cuts <- cut(gr1$breaks, 
                c(-Inf, 
                  intc[1], 
                  intc[2], 
                  Inf))
    
    cols <- c("red", "cadetblue1", "red")
    plot(gr1,
         xlab=paste0(input$param3, "s"),
         ylab="Densité",
         freq=FALSE,
         main = paste0("Distribution des ", input$param3, "s (Ré-échantillonnage, k = ", input$k3, ")"),
         col=cols[cuts])
    abline(v=mean(r.stat),
           col="red",
           lwd=2,
           lty=2)
    legend(ifelse(input$param3 == "Variance", "topright", "topleft"),
           legend = lgd1,
           cex = 1.2)

    x <- seq(from = min(r.stat), 
             to = max(r.stat),
             length = 500)
    
    y <- dnorm(x, 
               mean = parMU, 
               sd = parSIG / sqrt(input$n3))
    
    if(input$param3 == "Moyenne" & "theor" %in% input$showcurve){lines(x, y,
                                       col = "sienna4",
                                       lwd = 3,
                                       lty = 2)
      abline(v = parMU, lwd = 2, col = "darkgreen")}

    
    y1 <-  dnorm(x, 
                 mean = mean(r.stat), 
                 sd = sd(dat) / sqrt(input$n3))
    
    if(input$param3 == "Moyenne" & "empir" %in% input$showcurve){lines(x, y1,
                                      col = "slateblue3",
                                      lwd = 3,
                                      lty = 2)}
    
    
    
    if(input$param3 == "Moyenne" & "IC" %in% input$showcurve){
      yl <- par("usr")[3:4]
      yln <- diff(yl) / 4
      
      segments(IClim[1], yl[1] + yln, IClim[2], yl[1] + yln, lwd = 2)
      points(mean(dat), yl[1] + yln, pch=19, col="darkgreen", cex = 1.5)}

  })
  

  
  output$databootMoy <- renderDataTable({
    
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    statidxn <- c(
      "Moyenne",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(mean(dat), var(dat), sd(dat), skewness(dat), kurtosis(dat))
    
    param <- c(parMU, parSIG^2, parSIG, 0, 3)
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Paramètre (inconnu!)", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:20px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé"))
                
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              )
    ) %>%
      formatStyle(
        'Paramètre',
        backgroundColor = 'yellow')
    
    
  })
  

  output$calculMoy <- renderUI({
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    cl <- input$conf.level3 / 100
    qtl <- cl + (1 - cl) / 2
    
    cv <- ifelse(input$knownSD, qnorm(qtl), qt(qtl, input$n3 - 1))
    se3 <- ifelse(input$knownSD, parSIG / sqrt(input$n3), sd(dat) / sqrt(input$n3))
    
    frmtxt <- ifelse(input$knownSD, "$$\\sigma_{\\bar{x}}= \\frac{\\sigma_x}{\\sqrt{n}}= \\frac{", "$$s_{\\bar{x}}= \\frac{s_x}{\\sqrt{n}}= \\frac{")
    
    cvtxt <- ifelse(input$knownSD, 
                    paste("$$z_{(1-\\alpha)=", cl, "} = \\pm", round(cv, 3), "$$"), 
                    paste("$$t_{dl=", input$n3 - 1, "; (1-\\alpha)=", cl, "} = \\pm", round(cv, 3), "$$"))
      
      withMathJax( 
        
        helpText(
          strong("Calcul de l'intervalle de confiance:"), "$\\sigma $ ", ifelse(input$knownSD, " connu", " inconnu"),

          h5(paste(frmtxt, 
                   round(ifelse(input$knownSD, parSIG, sd(dat)), 3), 
                   "}{\\sqrt{", 
                   input$n3, 
                   "}}=", 
                   round(ifelse(input$knownSD, parSIG, sd(dat)) / sqrt(input$n3), 3), "$$")),
          h5(cvtxt),
             
          h5(paste("$$C[", 
                round(-cv, 3), 
                "\\times ", round(se3, 3), "+ ", 
                round(mean(dat), 3), 
                " \\le \\mu \\le ", 
                round(cv, 3), 
                "\\times ", 
                round(se3, 3), "+ ", 
                round(mean(dat), 3), "]=", 
                input$conf.level3, "\\% $$")),
         h3(paste("$$\\color{red}{C[", 
                round(-cv * se3 + mean(dat), 3), 
                " \\le \\mu \\le ", 
                round(cv * se3 + mean(dat), 3), "]=", 
                input$conf.level3, "\\% }$$")), p(),
          
          )
        )
      
  })
      
  output$databootProp <- renderDataTable({
    
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    p <- mean(dat)
    q <- 1 - p
    n <- input$n3
    
    statidxn <- c(
      "Proportion",
      "np",
      "n (1-p)",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(
      p,
      n * p,
      n * q,
      p * q,
      sqrt(p * q),
      (q - p) / sqrt(p * q),
      (1 - 6 * p * q) / (p * q)
    )
    param <- c(parMU, 
               NA, 
               NA, 
               parMU * (1 - parMU), 
               sqrt(parMU * (1 - parMU)),
               ((1 - parMU) - parMU)  / sqrt(parMU * (1 - parMU)),
               (1 - 6 *(parMU * (1 - parMU))) / (parMU * (1 - parMU))
               )
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Paramètre (inconnu!)", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé"))
                
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              )
    ) %>%
      formatStyle(
        'Paramètre',
        backgroundColor = 'yellow')
  })
  

  output$calculProp <- renderUI({
    
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    cl <- input$conf.level3 / 100
    qtl <- cl + (1 - cl) / 2
    cv <- qnorm(qtl)
    
    pr <- mean(dat)
    se3 <- sqrt((pr * (1-pr) / input$n3))
    
    withMathJax( 
      
      helpText(
        if(length(dat) * pr < 15 || length(dat) * (1 - pr) < 15){
 #         h4(strong("Solution inadmissible! Augmentez n!"))
          h3(paste("$$\\color{red}{Solution \\; inadmissible! \\; Augmentez \\; n!}$$"))
        },
        
        h4(strong("Calcul de l'intervalle de confiance:")),
        h4(paste("$$\\sigma_{p}= \\sqrt{\\frac{p(1-p)}{n}}= \\sqrt{\\frac{", round(pr, 3), "(1-", round(pr, 3), ")}{",input$n3, "}}=", round(se3, 3), "$$"),
        paste("$$z_{(1-\\alpha)=", cl, "} = \\pm", round(cv, 3), "$$"),
        paste("$$C[", 
              round(-cv, 3), 
              "\\times ", round(se3, 3), "+ ", 
              round(pr, 3), 
              " \\le \\pi \\le ", 
              round(cv, 3), 
              "\\times ", 
              round(se3, 3), "+ ", 
              round(pr, 3), "]=", 
              input$conf.level3, "\\% $$"))),
        h3(paste("$$\\color{red}{C[", 
              round(-cv * se3 + pr, 3), 
              " \\le \\pi \\le ", 
              round(cv * se3 + pr, 3), "]=", 
              input$conf.level3, "\\% }$$"))
        
    )
    })
  
  output$databootVar <- renderDataTable({
    
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    r.stat <- sapply(resamples, var)
    statidxn <- c(
      "Moyenne",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(mean(r.stat), var(r.stat), sd(r.stat), skewness(r.stat), kurtosis(r.stat))

    param <- c(parMU, 2 * (input$n3 - 1), sqrt(2 * (input$n3 - 1)), sqrt(8 / (input$n3 - 1)), 12 / (input$n3 - 1))
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Valeur Théorique", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé"))
                
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              )
    ) %>%
      formatStyle(
        'Paramètre',
        backgroundColor = 'yellow')
    
    
  })
  
  
  output$calculVar <- renderUI({
    
    resamples <- Data()[[1]]
    dat <- Data()[[2]]
    parMU <- Data()[[3]]
    parSIG <- Data()[[4]]
    
    cl <- input$conf.level3 / 100
    qtl <- c((1 - cl)/2, cl + 0.5 * (1 - cl))
    
    tc <- qchisq(qtl, input$n3 - 1)
    
    
    pr <- mean(dat)
    se3 <- sqrt(pr * (1-pr) / input$n3)
    
    withMathJax(         
      helpText(
        h4(strong("Calcul de l'intervalle de confiance:"),

        paste("$$\\frac{(n-1)s^2}{\\chi_{dl,1-\\alpha/2}^2} \\le \\sigma^2 \\le \\frac{(n-1)s^2}{\\chi_{dl,\\alpha/2}^2} $$"),
        paste("$$\\chi_{", input$n3 - 1, ", ", qtl[1], "}^2 = ", round(tc[1], 3), "$$"),
        paste("$$\\chi_{", input$n3 - 1, ", ", qtl[2], "}^2 = ", round(tc[2], 3), "$$"),  p(),
        
        paste("$$\\frac{(", input$n3, "-1)", round(var(dat), 3), "}{", round(tc[2], 3), "} \\le \\sigma^2 \\le \\frac{(", input$n3, "-1)", round(var(dat), 3), "}{",round(tc[1], 3), "}$$")),
        
        h3(paste("$$\\color{red}{", round((input$n3-1) * var(dat) / tc[2], 3), " \\le \\sigma^2 \\le ", round((input$n3-1) * var(dat) / tc[1], 3), "}$$")),
        
 ))

  })
  
  output$interpret <- renderUI({
    pp <- c("Moyenne", "Proportion", "Variance")
    pps <- c("\u03BC", "\u03C0", "\u03C3 \u00B2")
    ppp <- pps[which(input$param3 == pp)]
    helpText(
      withMathJax(
      h4(
        strong("Interprétation:"), p(),
      "En tirant un échantillon de ", input$n3, " observations d'une population normalement distribuée ",
      ifelse(input$param3 == "Moyenne" & input$knownSD, "dont $\\sigma$ est connu, ",  "dont $\\sigma$ est inconnu, "),
      "la probabilité que l'intervalle affiché ci-dessus capture la ", input$param3, " réelle de la population ",
      "est égale à ", input$conf.level3, "%.  En d'autres termes, dans les conditions décrites, il y a ",  input$conf.level3, "% des chances ",
      "que parmi les valeurs incluses dans cet intervalle, on trouve la valeur de ", ppp(), "...")
)
    )
    
  })
  
  
  ##############################################
  # Vérification d'hypothèse
  ##############################################
  
    Data4 <- eventReactive(input$simulate4,{
    
    gensim3(k = input$k4,
            param = input$param4,
            par1 = ifelse(input$param4 != "Proportion", runif(1, 40, 100), runif(1, 0.1, 0.9)),
            par2 = ifelse(input$param4 != "Proportion", runif(1, 5, 30), NA),
            n = input$n4)
  })
  

  ppp <- reactive(c("\u03BC", "\u03C0", "\u03C3 \u00B2")[which(input$param4 == c("Moyenne", "Proportion", "Variance"))])
  qtl1 <- reactive(c(input$sig.level4, 1 - input$sig.level4))
  qtl2 <- reactive(c(input$sig.level4 / 2, input$sig.level4 / 2 + (1 - input$sig.level4)))
 
  output$meantstr4 <- renderPlot({
    withMathJax()
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
#    observeEvent(input$simulate4, {
#      updateNumericInput(session, 
#                         "hyppar",
#                         value = round(mean(dat4), 3))
#    })
    
    r.stat4 <- sapply(resamples4, mean)
    std.err <- sd(r.stat4)
    
    r.stat4 <- r.stat4 + (input$hyppar - mean(r.stat4))
    
#    intc <- if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
#      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
#        quantile(r.stat4, qtl2())}
    
    intc <-  if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    merr <- abs(input$hyppar - mean(dat4))
    
    prt <- c(mean(r.stat4 < mean(dat4)), 
             mean(r.stat4 > mean(dat4)), 
             mean(r.stat4 < (input$hyppar - merr) | (r.stat4 > input$hyppar + merr)))[as.integer(input$direction)]
    
    gr4 <- hist(r.stat4, 
                breaks="FD", 
                plot=FALSE)
    
#    lgd1 <- if(input$direction == "1"){
#      paste("P[", round(intc[2], 3),  "\u2264 Moyenne \u2264 \u221E] = ", (1 - input$sig.level4) * 100, "%")} else
#        if(input$direction == "2"){
#          paste("P[-\u221E \u2264 Moyenne \u2264 ", round(intc[1], 3), "] = ", (1 - input$sig.level4) * 100, "%")} else
#            {paste("P[", round(intc[1], 3), " \u2264 Moyenne \u2264 ", round(intc[2], 3), "] = ", (1 - input$sig.level4) * 100, "%")}

    
    lgd2 <- paste("P = ", round(prt, 3))
    
      if(input$direction == "1"){cuts <- cut(gr4$breaks, 
                                             c(-Inf, ifelse(input$showprob == "rjct", intc[2], mean(dat4)), Inf))
                                         cols <- c("red", "lightyellow")} else 
        if(input$direction == "2"){cuts <- cut(gr4$breaks, 
                                             c(-Inf, ifelse(input$showprob == "rjct", intc[1], mean(dat4)), Inf))
                                         cols <- c("lightyellow", "red")} 
        else {cuts <- cut(gr4$breaks, 
                          c(-Inf, 
                            ifelse(input$showprob == "rjct", intc[1], input$hyppar - merr), 
                            ifelse(input$showprob == "rjct", intc[2], input$hyppar + merr), 
                            Inf))
                      cols <- c( "red", "lightyellow", "red")} 
      
    plot(gr4,
         xlab="", # paste0(input$param4, "s"),
         ylab="Densité",
         freq=FALSE,
         main = TeX(r'(Distribution des $\bar{X}s$, supposant $H_0$ vraie)'),
         cex.main = 1.5,
         col=cols[cuts])
    abline(v=mean(dat4),
           col="darkgreen",
           lwd=2,
           lty=2)
#    legend("topleft",
#           legend = lgd1,
#           cex = 1.2,
#           title = TeX(r'(Valeurs probables, sous $H_0$)'))
    legend("topright", 
           legend = lgd2,
           cex = 1.2,
           title = TeX(r'(Supposant $H_0$:)'))
    abline(v=input$hyppar,
           col="red",
           lty = 1,
           lwd = 2)
    
    yl <- par("usr")[3:4]
    yln <- diff(yl) / 4
    
    if(input$probline){
      segments(quantile(r.stat4, qtl2()[1]), yl[1] + yln, quantile(r.stat4, qtl2()[2]), yl[1] + yln, lwd = 2)
      points(input$hyppar, yl[1] + yln, pch=19, col="darkgreen", cex = 1.5)}
    
    if(input$icnf){
      segments(mean(dat4) + qnorm(qtl2()[1])*std.err, yl[1] + 1.2 * yln, mean(dat4)+ qnorm(qtl2()[2]) * std.err,  yl[1] + 1.2 * yln, lwd = 2)
      points(mean(dat4), yl[1] + 1.2 * yln, pch=19, col="darkgreen", cex = 1.5)}
    
    if(input$tcurve){
      curve(dnorm(x, mean = input$hyppar, sd = std.err),
            lwd = 2, 
            add = TRUE, 
            yaxt = "n", 
            col = "darkblue")
    }
    mtext(TeX(r'($\mu_{\bar{X}}$)'), side = 1, line = 2.5, at = input$hyppar, cex = 1.5, col = "red")
    mtext(TeX(r'($\bar{X}$)'), side = 1, line = 2.5, at = mean(dat4), cex = 1.5, col = "darkgreen")
    

    
  })
  
  output$dataHTMoy <- renderDataTable({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    statidxn <- c(
      "Moyenne",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(mean(dat4), var(dat4), sd(dat4), skewness(dat4), kurtosis(dat4))
    
    param <- c(parMU4, parSIG4^2, parSIG4, 0, 3)
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Paramètre (inconnu!)", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé")) 
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              ) 
    )%>%
      formatStyle(
        'Paramètre',
        backgroundColor = 'yellow')
  })
  
  output$calculMoy4 <- renderUI({
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, mean)
    std.err <- sd(r.stat4)
    intc <- if(input$direction == "1"){quantile(r.stat4, qtl1()[1])} else 
      if(input$direction == "2"){quantile(r.stat4, qtl1()[2])} else {
        quantile(r.stat4, qtl2())}
    
    cv <- ifelse(input$knownSD4, 
                 c(qnorm(qtl1()), qnorm(qtl2()[2]))[as.integer(input$direction)],
                 c(qt(qtl1(), input$n4 - 1), qt(qtl2()[2], input$n4 - 1))[as.integer(input$direction)])
    
    se4 <- ifelse(input$knownSD4, parSIG4 / sqrt(input$n4), sd(dat4) / sqrt(input$n4))
    
    tst <- (mean(dat4) - input$hyppar) / se4
    
    frmtxt <- ifelse(input$knownSD4, "$$\\sigma_{\\bar{x}}= \\frac{\\sigma_x}{\\sqrt{n}}= \\frac{", "$$s_{\\bar{x}}= \\frac{s_x}{\\sqrt{n}}= \\frac{")
    
    h1op <- c("<", ">", "\\ne ")[which(c("1", "2", "3") == input$direction)]
    
    cvtxt <- ifelse(input$knownSD4, 
                    paste("$$z_{", input$sig.level4, "} = ", round(cv, 3), "$$"), 
                    paste("$$t_{", input$n4 - 1, ";", input$sig.level4, "} = ", round(cv, 3), "$$"))
    

    withMathJax( 
      wellPanel(style = "background: lightyellow",
      
      helpText(
        
        wellPanel(
          style = "background: lightblue",
        h4(strong("Hypothèses:")), p(),
        "$$H_0:\\mu = ", input$hyppar, "$$",
        "$$H_1:\\mu", c("<", ">", "\u2260")[as.integer(input$direction)], input$hyppar, "$$",),
        
        h4(strong("Calcul de l'erreur-standard des moyennes:  "), ifelse(input$knownSD4, "$\\color{red}{\\sigma \\;connu:}$",  "$\\color{red}{\\sigma \\;inconnu:}$")),
        h5(paste(frmtxt, 
                 round(ifelse(input$knownSD4, parSIG4, sd(dat4)), 3), 
                 "}{\\sqrt{", 
                 input$n4, 
                 "}}=", 
                 round(ifelse(input$knownSD4, parSIG4, sd(dat4)) / sqrt(input$n4), 3), "$$")),
        h4(strong("Calcul de la statistique de vérification:")), 
        ifelse(input$knownSD4, 
               paste("$$z = \\frac{\\bar{X}-\\mu}{\\sigma_{\\bar{X}}}=\\frac{", round(mean(dat4), 3), " - ", input$hyppar, "}{", round(se4, 3), "} = ", round((mean(dat4) - input$hyppar) / se4, 3), "$$"),
               paste("$$t = \\frac{\\bar{X}-\\mu}{s_{\\bar{X}}}=\\frac{", round(mean(dat4), 3), " - ", input$hyppar, "}{", round(se4, 3), "} = ", round((mean(dat4) - input$hyppar) / se4 , 3), "$$")),
        h4(strong("Valeur Critique [test ", ifelse(input$direction != "3", "unilatéral]", "bilatéral]:"))),
        h4(cvtxt),
        h4(strong("Conclusion:"),
        ifelse((input$direction == "1" & tst < cv) || (input$direction == "2" & tst > cv) || (input$direction == "3" & abs(tst) > abs(cv)), 
               "p < 0.05,  Rejet de $H_0$ ", 
               "p > 0.05,  $H_0$ non rejetée"),
        h4("Les données ", ifelse((input$direction == "1" & tst < cv) || (input$direction == "2" & tst > cv) || (input$direction == "3" & abs(tst) > abs(cv)), 
                                  " ne sont pas", 
                                  " sont "),
        " compatibles avec l'idée que ", paste("$\\mu =", input$hyppar, "$"), "."),
        p(),
      
     h4(strong("Calcul de l'intervalle de confiance:")), 
        
        h5(paste("$$C[", 
                 round(-cv, 3), 
                 "\\times ", round(se4, 3), "+ ", 
                 round(mean(dat4), 3), 
                 " \\le \\mu \\le ", 
                 round(cv, 3), 
                 "\\times ", 
                 round(se4, 3), "+ ", 
                 round(mean(dat4), 3), "]=", 
                 (1 - input$sig.level4) * 100, "\\% $$")),
        h5(paste("$$\\color{red}{C[", 
                 round(-cv * se4 + mean(dat4), 3), 
                 " \\le \\mu \\le ", 
                 round(cv * se4 + mean(dat4), 3), "]=", 
                 (1 - input$sig.level4) * 100, "\\% }$$")), 
     p(),
     h4(strong("Calcul de l'intervalle des valeurs probables, sous $H_0$:")), 
     
     h5(paste("$$P[", 
              round(-cv, 3), 
              "\\times ", round(se4, 3), "+ ", 
              round(input$hyppar, 3), 
              " \\le \\bar{X} \\le ", 
              round(cv, 3), 
              "\\times ", 
              round(se4, 3), "+ ", 
              round(input$hyppar, 3), "]=", 
              (1 - input$sig.level4) * 100, "\\% $$")),
     h5(paste("$$\\color{red}{C[", 
              round(-cv * se4 + input$hyppar, 3), 
              " \\le \\mu \\le ", 
              round(cv * se4 + input$hyppar, 3), "]=", 
              (1 - input$sig.level4) * 100, "\\% }$$"))
        )
        
      )
)
    )
    })
    
  
  output$interprMoy <- renderUI({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, mean)
    r.stat4 <- r.stat4 + (input$hyppar - mean(r.stat4))
    std.err <- sd(r.stat4)
    
    smplMOY <- mean(dat4)
    
    intc <- if(input$direction == "1"){quantile(r.stat4, qtl1()[1])
                                      icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), " \\le \\mu \\le  \\infty $")} else 
      if(input$direction == "2"){quantile(r.stat4, qtl1()[2])
         icTxt <- paste("$-\\infty \\le \\mu \\le ", round(quantile(r.stat4, qtl1()[2]), 3),"$")} else {
           icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), "\\le \\mu \\le ", round(quantile(r.stat4, qtl1()[2]), 3), " $")}
           
    
    intclim <-  if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    merr <- abs(input$hyppar - smplMOY)
    
    prt <- c(mean(r.stat4 < smplMOY), 
             mean(r.stat4 > smplMOY), 
             mean(r.stat4 < (input$hyppar - merr) | (r.stat4 > input$hyppar + merr)))[as.integer(input$direction)]
    
    
    cv <- ifelse(input$knownSD4, 
                 c(qnorm(qtl1()), qnorm(qtl2()[2]))[as.integer(input$direction)],
                 c(qt(qtl1(), input$n4 - 1), qt(qtl2()[2], input$n4 - 1))[as.integer(input$direction)])
    
    cvsgn <- c("-", "+", "$\\pm$")[which(c("1", "2", "3") == input$direction)]

    
    se4 <- ifelse(input$knownSD4, parSIG4, sd(dat4)) / sqrt(input$n4)
    
    tst <- (smplMOY - input$hyppar) / se4
    
    p <- ifelse(input$knownSD4, 
                c(pnorm(tst, lower.tail = TRUE), 
                  pnorm(tst, lower.tail = FALSE), 
                  (2 * pnorm(abs(tst), lower.tail = FALSE)))[as.integer(input$direction)],
                c(pt(abs(tst), df = input$n4 - 1, lower.tail = TRUE), 
                  pt(tst, df = input$n4 - 1, lower.tail = FALSE), 
                  2 * pt(abs(tst), df = input$n4 - 1, lower.tail = FALSE))[as.integer(input$direction)])
    
    IC <- c(-1, 1) * cv * se4 + smplMOY
    
    dirtn <- c(paste("\\le"), 
               paste("\\ge"), 
               paste("\\le ", round(input$hyppar - merr, 3), "$ ou $\\bar{X} \\ge "))[which(input$direction == c("1", "2", "3"))]
    
    helpText(
      withMathJax(
        h3("Interprétation:"),
        h4(
          strong("Résultats de l'analyse par Ré-échantillonnage:"), p(),
          "L'analyse illustrée par le diagramme ci-dessus indique que si une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " définie par $H_0: \\mu=", input$hyppar, "$ est vraie, ",
          "la probabilité d'obtenir $\\bar{X}", dirtn, round(input$hyppar + merr, 3), "$, est $p=", round(prt, 3), "$.  Cette probabilité est jugée ",
          ifelse(prt < input$sig.level4, "faible", "élevée"), ", ce qui indique que la moyenne de la population d'où provient l'échantillon ", ifelse(prt < input$sig.level4, "pourrait ne pas ", "pourrait "), 
          "être égale à ", input$hyppar, ". On ", ifelse(prt < input$sig.level4, "peut ", "ne peut pas "), "rejeter $H_0$ sur la base des observations.", 
          "L'intervalle de confiance regroupant les valeurs probables des moyennes, pour un niveau de confiance $(1-\\alpha) = ", (1-(input$sig.level4)) * 100, "\\%$ et en supposant $H_0$ vraie, est le suivant:", p(),
          
          paste("$C[$", icTxt, "] = ", (1 - input$sig.level4) * 100, "$\\%$"), p(),
          
          "Les limites de cet intervalle sont simplement ",
          
          if(input$direction == "1"){paste("le percentile $P_{", 
                                           (1 - input$sig.level4 / 2) * 100, 
                                           "}$ et $\\infty$")} else
            if(input$direction == "2"){paste("$\\infty$ et le percentile $P_{", 
                                             (1 - input$sig.level4 / 2) * 100, 
                                             "}$")} else {
              paste("les percentiles $P_{", 
                    (input$sig.level4 / 2) * 100, 
                    "}$ et $P_{", 
                    (1 - input$sig.level4 / 2) * 100, 
                    "}$")}, 
          " de la distribution des moyennes produite par ré-échantillonnage. ", 
          "Cet intervalle est parfaitement compatible avec le résultat obtenu. En effet, puisque la valeur de $\\bar{X} = ", round(smplMOY, 3), 
          
          ifelse((input$direction == "1" & between(smplMOY, intclim[1], Inf)) 
                 || (input$direction == "2" & between(smplMOY, -Inf, intclim[1])) 
                 || (input$direction == "3" & between(smplMOY, intclim[1], intclim[2])),
                 "$ fait partie de cet intervalle, on ne peut pas ",
                 "$ ne fait pas partie de cet intervalle, on peut  "), 
               
          " rejeter $H_0$.", p(),

          strong("Test d'hypothèse: Approche paramétrique"), p(),
          
          "Si $H_0$, une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " selon laquelle $\\mu = ", input$hyppar,  "$, est vraie, alors la probabilité d'obtenir un ", 
          ifelse(input$knownSD4, "$z$", "$t$"), " égal ou plus extrême que ", round(tst, 3), ", la valeur obtenue avec ",
          "un échantillon aléatoire de $n = ", input$n4, "$ observations, est égale à ", paste("$p = ", round(p, 3)), 
          "$. Une telle probabilité est ", ifelse(p < input$sig.level4, "faible", "élevée"), ", de sorte que les données ",
          ifelse(p < input$sig.level4, "vont à l'encontre de $H_0$, que l'on peut rejeter.", "ne permettent pas de rejeter $H_0$."), p(),
                                                                                               
#          strong("Intervalle de confiance:"), p(),
#          "En tirant un échantillon de ", input$n4, " observations d'une population normalement distribuée ",
#          ifelse(input$param4 == "Moyenne" & input$knownSD4, "dont $\\sigma$ est connu, ",  "dont $\\sigma$ est inconnu, "),
#          "la probabilité que l'intervalle affiché ci-dessus capture la ", input$param4, " réelle de la population ",
#          "est égale à ", input$sig.level4, "%.  En d'autres termes, dans les conditions décrites, il y a ",  input$sig.level4, "% des chances ",
#          "que parmi les valeurs incluses dans cet intervalle, on trouve la valeur de ", ppp(), "..."
)
      )
    )
    
    
    
  })
  
  output$proptstr4 <- renderPlot({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    
#    observeEvent(input$simulate4, {
#      updateNumericInput(session, 
#                         "hypPR",
#                         value = round(mean(dat4), 3))
#    })
    
    r.stat4 <- sapply(resamples4, mean)
    std.err <- sd(r.stat4)
    
    r.stat4 <- r.stat4 + (input$hypPR - mean(r.stat4))
    
    intc <- if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    merr <- abs(input$hypPR - mean(dat4))
    
    gr4 <- hist(r.stat4, 
                breaks="FD", 
                plot=FALSE)
    
    lgd1 <- if(input$direction == "1"){
      paste("P[", round(intc[2], 3),  "\u2264 ", ppp(), " \u2264 \u221E] = ", (1 - input$sig.level4) * 100, "%")} else
        if(input$direction == "2"){
          paste("P[-\u221E \u2264 ", ppp(), " \u2264 ", round(intc[1], 3), "] = ", (1 - input$sig.level4) * 100, "%")} else
          {paste("P[", round(intc[1], 3), " \u2264 ", ppp(), " \u2264 ", round(intc[2], 3), "] = ", (1 - input$sig.level4) * 100, "%")}
    
 
    if(input$direction == "1"){cuts <- cut(gr4$breaks, 
                                           c(-Inf, ifelse(input$showprob == "rjct", intc[2], mean(dat4)), Inf))
    cols <- c("red", "lightyellow")} else 
      if(input$direction == "2"){cuts <- cut(gr4$breaks, 
                                             c(-Inf, ifelse(input$showprob == "rjct", intc[1], mean(dat4)), Inf))
      cols <- c("lightyellow", "red")} 
    else {cuts <- cut(gr4$breaks, 
                      c(-Inf, 
                        ifelse(input$showprob == "rjct", intc[1], input$hypPR - merr), 
                        ifelse(input$showprob == "rjct", intc[2], input$hypPR + merr), 
                        Inf))
    cols <- c( "red", "lightyellow", "red")} 
    
       
 #   if(input$direction == "1"){cuts <- cut(gr4$breaks, 
#                                           c(-Inf, intc[2], 
#                                             Inf))
#    cols <- c("red", "lightyellow")} else 
#      if(input$direction == "2"){cuts <- cut(gr4$breaks, 
#                                             c(-Inf, intc[1], 
#                                               Inf))
#      cols <- c("lightyellow", "red")} else {
#        cuts <- cut(gr4$breaks, 
#                    c(-Inf, intc[1], intc[2], 
#                      Inf))
#        cols <- c( "red", "lightyellow", "red")}
    
    
    plot(gr4,
         xlab=paste0(input$param4, "s"),
         ylab="Densité",
         freq=FALSE,
         main = paste0("Distribution des ", input$param4, "s, supposant H0 vraie"),
         col=cols[cuts])
    abline(v=mean(dat4),
           col="green",
           lwd=2,
           lty=2)
#    legend(ifelse(input$param4 == "Variance", "topright", "topleft"),
#           legend = lgd1,
#           cex = 1.2,
#           title = "valeurs probables, sous H0")
    abline(v=input$hypPR,
           col="red",
           lty = 1,
           lwd = 2)
    
    yl <- par("usr")[3:4]
    yln <- diff(yl) / 4

    if(input$probline){
      segments(quantile(r.stat4, qtl2()[1]), yl[1] + yln, quantile(r.stat4, qtl2()[2]), yl[1] + yln, 1, lwd = 2)
      points(input$hypPR, yl[1] + yln, pch=19, col="darkgreen", cex = 1.5)}
    
    if(input$icnf){
      segments(mean(dat4) + qnorm(qtl2()[1]) *std.err, yl[1] + 1.2 * yln, mean(dat4)+ qnorm(qtl2()[2]) *std.err, yl[1] + 1.2 * yln, lwd = 2)
      points(mean(dat4), yl[1] + 1.2 * yln, pch=19, col="darkgreen", cex = 1.5)}
      
  })
  
  output$dataHTProp <- renderDataTable({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    p <- mean(dat4)
    q <- 1 - p
    n <- input$n4
    
    statidxn <- c(
      "Proportion",
      "np",
      "n [1-p]",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(
      p,
      n * p,
      n * q,
      p * q,
      sqrt(p * q),
      (q - p) / sqrt(p * q),
      (1 - 6 * p * q) / (p * q)
    )
    param <- c(parMU4, 
               NA, 
               NA, 
               parMU4 * (1 - parMU4), 
               sqrt(parMU4 * (1 - parMU4)),
               ((1 - parMU4) - parMU4)  / sqrt(parMU4 * (1 - parMU4)),
               (1 - 6 *(parMU4 * (1 - parMU4))) / (parMU4 * (1 - parMU4))
    )
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Paramètre (inconnu!)", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé"))
                
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              )
    )  %>%
    formatStyle(
      'Paramètre',
      backgroundColor = 'yellow')
  })
  
  
  output$calculProp4 <- renderUI({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    cv <- c(qnorm(qtl1()), qnorm(qtl2()[2]))[as.integer(input$direction)]
    
    cv2 <- qnorm(1 - input$sig.level4 / 2)
    
    cvtxt <- paste("$$z_{", input$sig.level4, "} = ", round(cv, 3), "$$")
    
#    cvtxt <- paste("$$z_{(1-\\alpha)}=z_{", 1 - input$sig.level4, "} = \\pm ", round(qnorm(1 - input$sig.level4 / 2), 3), " $$")
    
    pr <- mean(dat4)
    se4 <- sqrt((pr * (1-pr) / input$n4))
    tst <- (pr - input$hypPR) / se4
    
    withMathJax( 
      
      helpText(
        if(length(dat4) * pr < 15 || length(dat4) * (1 - pr) < 15){
          h3(paste("$$\\color{red}{Solution \\; inadmissible! \\; Augmentez \\; n!}$$"))
        },
        wellPanel(
          style = "background: lightblue",
        strong("Hypothèses:"), p(),
        "$$H_0:\\pi = ", input$hypPR, "$$",
            "$$H_1:\\pi", c("<", ">", "\u2260")[as.integer(input$direction)], input$hypPR, "$$"),
            
            strong("Calcul de l'erreur-standard des proportions:  "), 
        
        h4(paste("$$\\sigma_{p}= \\sqrt{\\frac{p(1-p)}{n}}= \\sqrt{\\frac{", round(pr, 3), "(1-", round(pr, 3), ")}{",input$n4, "}}=", round(se4, 3), "$$")),
        
        strong("Calcul de la statistique de vérification:"), 
        paste("$$z = \\frac{p-\\pi}{\\sigma_{p}}=\\frac{", round(pr, 3), " - ", input$hypPR, "}{", round(se4, 3), "} = ", round((pr - input$hypPR) / se4, 3), "$$"),
        
        strong("Valeur Critique [test ", ifelse(input$direction != "3", "unilatéral]", "bilatéral]:")),
            h4(cvtxt),
        h4(strong("Conclusion:"),
            if((input$direction == "1" & tst < cv) || (input$direction == "2" & tst > cv) || (input$direction == "3" & abs(tst) > abs(cv))){"p < 0.05,  Rejet de $H_0$ "} else
            {"p > 0.05,  $H_0$ non rejetée"}),
            p(),
            
        h4(strong("Calcul de l'intervalle de confiance:")),

           paste("$$z_{(1-\\alpha)=", input$sig.level4, "} = \\pm", round(cv, 3), "$$"),
           paste("$$C[", 
                 round(-cv2, 3), 
                 "\\times ", round(se4, 3), "+ ", 
                 round(pr, 3), 
                 " \\le \\pi \\le ", 
                 round(cv2, 3), 
                 "\\times ", 
                 round(se4, 3), "+ ", 
                 round(pr, 3), "]=", 
                 input$sig.level4, "\\% $$"),
      h4(paste("$$\\color{red}{C[", 
               round(-cv * se4 + pr, 3), 
               " \\le \\pi \\le ", 
               round(cv2 * se4 + pr, 3), "]=", 
               (1 - input$sig.level4) * 100, "\\% }$$")),
      p(),
      h4(strong("Calcul de l'intervalle des valeurs probables de $p$, sous $H_0$:")), 
      
      h4(paste("$$P[", 
               round(-cv, 3), 
               "\\times ", round(se4, 3), "+ ", 
               round(input$hypPR, 3), 
               " \\le p \\le ", 
               round(cv, 3), 
               "\\times ", 
               round(se4, 3), "+ ", 
               round(input$hypPR, 3), "]=", 
               (1 - input$sig.level4) * 100, "\\% $$")),
      h4(paste("$$\\color{red}{C[", 
               round(-cv * se4 + input$hypPR, 3), 
               " \\le p \\le ", 
               round(cv * se4 + input$hypPR, 3), "]=", 
               (1 - input$sig.level4) * 100, "\\% }$$"))
      
      ) 
    )
  })
  
  output$interprProp <- renderUI({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, mean)
    r.stat4 <- r.stat4 + (input$hypPR - mean(r.stat4))
    std.err <- sd(r.stat4)
    smplPR <- mean(dat4)
    
    intc <- if(input$direction == "1"){quantile(r.stat4, qtl1()[1])
      icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), " \\le \\pi \\le  \\infty $")} else 
        if(input$direction == "2"){quantile(r.stat4, qtl1()[2])
          icTxt <- paste("$-\\infty \\le \\pi \\le ", round(quantile(r.stat4, qtl1()[2]), 3),"$")} else {
            icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), "\\le \\pi \\le ", round(quantile(r.stat4, qtl1()[2]), 3), " $")}
    
    
    intclim <-  if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    merr <- abs(input$hypPR - smplPR)
    
    prt <- c(mean(r.stat4 < smplPR), 
             mean(r.stat4 > smplPR), 
             mean(r.stat4 < (input$hypPR - merr) | (r.stat4 > input$hypPR + merr)))[as.integer(input$direction)]
    
    
    cv <- ifelse(input$knownSD4, 
                 c(qnorm(qtl1()), qnorm(qtl2()[2]))[as.integer(input$direction)],
                 c(qt(qtl1(), input$n4 - 1), qt(qtl2()[2], input$n4 - 1))[as.integer(input$direction)])
    
    cvsgn <- c("-", "+", "$\\pm$")[which(c("1", "2", "3") == input$direction)]
    
    
    se4 <- ifelse(input$knownSD4, parSIG4, sd(dat4)) / sqrt(input$n4)
    
    tst <- (smplPR - input$hypPR) / se4
    
    p <- c(pnorm(tst, lower.tail = TRUE), 
           pnorm(tst, lower.tail = FALSE), 
           (2 * pnorm(abs(tst), lower.tail = FALSE)))[as.integer(input$direction)]
    
    IC <- c(-1, 1) * cv * se4 + smplPR
    
    dirtn <- c(paste("\\le"), 
               paste("\\ge"), 
               paste("\\le ", round(input$hypPR - merr, 3), "$ ou $\\bar{X} \\ge "))[which(input$direction == c("1", "2", "3"))]
    
    helpText(
      withMathJax(
        h3("Interprétation:"),
        h4(
          strong("Résultats de l'analyse par Ré-échantillonnage:"), p(),
          "L'analyse illustrée par le diagramme ci-dessus indique que si une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " définie par $H_0: \\pi=", input$hypPR, "$ est vraie, ",
          "la probabilité d'obtenir $p", dirtn, round(input$hypPR + merr, 3), "$, est $p=", round(prt, 3), "$.  Cette probabilité est jugée ",
          ifelse(prt < input$sig.level4, "faible", "élevée"), ", ce qui indique que la proportion au niveau de la population d'où provient l'échantillon ", ifelse(prt < input$sig.level4, "pourrait ne pas ", "pourrait "), 
          "être égale à ", input$hypPR, ". On ", ifelse(prt < input$sig.level4, "peut ", "ne peut pas "), "rejeter $H_0$ sur la base des observations.", 
          "L'intervalle de confiance regroupant les valeurs probables des proportions, pour un niveau de confiance $(1-\\alpha) = ", (1-(input$sig.level4)) * 100, "\\%$ et en supposant $H_0$ vraie, est le suivant:", p(),
          
          paste("$C[$", icTxt, "] = ", (1 - input$sig.level4) * 100, "$\\%$"), p(),
          
          "Les limites de cet intervalle sont simplement ",
          
          if(input$direction == "1"){paste("le percentile $P_{", 
                                           (1 - input$sig.level4 / 2) * 100, 
                                           "}$ et $\\infty$")} else
                                             if(input$direction == "2"){paste("$\\infty$ et le percentile $P_{", 
                                                                              (1 - input$sig.level4 / 2) * 100, 
                                                                              "}$")} else {
                                                                                paste("les percentiles $P_{", 
                                                                                      (input$sig.level4 / 2) * 100, 
                                                                                      "}$ et $P_{", 
                                                                                      (1 - input$sig.level4 / 2) * 100, 
                                                                                      "}$")}, 
          " de la distribution des proportions produite par ré-échantillonnage. ", 
          "Cet intervalle est parfaitement compatible avec le résultat obtenu. En effet, puisque la valeur de $p = ", round(smplPR, 3), 
          ifelse((input$direction == "1" & between(smplPR, intclim[1], Inf)) 
                  || (input$direction == "2" & between(smplPR, -Inf, intclim[1])) 
                  || (input$direction == "3" & between(smplPR, intclim[1], intclim[2])),
                 "$ fait partie de cet intervalle, on ne peut pas ", 
                 "$ ne fait pas partie de cet intervalle, on peut "), 
          
          " rejeter $H_0$.", p(),
          
          strong("Test d'hypothèse: Approche paramétrique"), p(),
          
          "Si $H_0$, une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " selon laquelle $\\pi = ", input$hypPR,  "$, est vraie, alors la probabilité d'obtenir un ", 
          ifelse(input$knownSD4, "$z$", "$t$"), " égal ou plus extrême que ", round(tst, 3), ", la valeur obtenue avec ",
          "un échantillon aléatoire de $n = ", input$n4, "$ observations, est égale à ", paste("$p = ", round(p, 3)), 
          "$. Une telle probabilité est ", ifelse(p < input$sig.level4, "faible", "élevée"), ", de sorte que les données ",
          ifelse(p < input$sig.level4, "vont à l'encontre de $H_0$, que l'on peut rejeter.", "ne permettent pas de rejeter $H_0$."), p(),
          
          #          strong("Intervalle de confiance:"), p(),
          #          "En tirant un échantillon de ", input$n4, " observations d'une population normalement distribuée ",
          #          ifelse(input$param4 == "Moyenne" & input$knownSD4, "dont $\\sigma$ est connu, ",  "dont $\\sigma$ est inconnu, "),
          #          "la probabilité que l'intervalle affiché ci-dessus capture la ", input$param4, " réelle de la population ",
          #          "est égale à ", input$sig.level4, "%.  En d'autres termes, dans les conditions décrites, il y a ",  input$sig.level4, "% des chances ",
          #          "que parmi les valeurs incluses dans cet intervalle, on trouve la valeur de ", ppp(), "..."
        )
      )
    )
    
    
    
  })
  
  output$vartstr4 <- renderPlot({

    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
#    observeEvent(input$simulate4, {
#      updateNumericInput(session, 
#                         "hyppar",
#                         value = round(var(dat4), 3))
#    })
    
    IClim <- (input$n4 - 1) * var(dat4) / qchisq(qtl2(), input$n4 - 1)[2:1]
    
    r.stat4 <- sapply(resamples4, var)
#    r.stat4x2 <- (input$n4 - 1) * r.stat4.org / input$hyppar
    
#    if(input$diagwhat == "vars"){r.stat4 <- r.stat4.org + (input$hyppar - var(dat4))} else {
#                                 r.stat4 <- r.stat4x2 - (input$n4 - 1) * var(dat4) / input$hyppar}

    
    r.stat4 <- r.stat4 + (input$hyppar - var(dat4))
    
    intc <- if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    gr4 <- hist(r.stat4, 
                breaks="FD", 
                plot=FALSE)
    
#    lgd1 <- if(input$direction == "1"){
#      paste("P[", round(intc[2], 3),  "\u2264 ", ppp(), " \u2264 \u221E] = ", (1 - input$sig.level4) * 100, "%")} else
#        if(input$direction == "2"){
#          paste("P[-\u221E \u2264 ", ppp(), " \u2264 ", round(intc[1], 3), "] = ", (1 - input$sig.level4) * 100, "%")} else
#          {paste("P[", round(intc[1], 3), " \u2264 ", ppp(), " \u2264 ", round(intc[2], 3), "] = ", (1 - input$sig.level4) * 100, "%")}
    
    
    if(input$direction == "1"){cuts <- cut(gr4$breaks, 
                                           c(-Inf, intc[2], 
                                             Inf))
    cols <- c("red", "lightyellow")} else 
      if(input$direction == "2"){cuts <- cut(gr4$breaks, 
                                             c(-Inf, intc[1], 
                                               Inf))
      cols <- c("lightyellow", "red")} else {
        cuts <- cut(gr4$breaks, 
                    c(-Inf, intc[1], intc[2], 
                      Inf))
        cols <- c( "red", "lightyellow", "red")}
    
    
    plot(gr4,
         xlab=paste0(input$param4, "s"),
         ylab="Densité",
         freq=FALSE,
         main = paste0("Distribution des ", input$param4, "s, supposant H0 vraie"),
         col=cols[cuts])
    abline(v=var(dat4),
           col="green",
           lwd=2,
           lty=2)
#    legend("topright",
#           legend = lgd1,
#           cex = 1.2,
#           title = "valeurs probables, sous H0")
    abline(v=input$hyppar,
           col="red",
           lty = 1,
           lwd = 2)
    
    yl <- par("usr")[3:4]
    yln <- diff(yl) / 4
    
    if(input$probline){
      segments(quantile(r.stat4, input$sig.level4 / 2), yl[1] + yln, quantile(r.stat4, 1 - (input$sig.level4 / 2)), yl[1] + yln, lwd = 2)
      points(input$hyppar, yl[1] + yln, pch=19, col="darkgreen", cex = 1.5)}
    
    if(input$icnf){
      segments(IClim[1], yl[1] + 1.4 * yln, IClim[2], yl[1] + 1.4 * yln, lwd = 2)
      points(var(dat4), yl[1] + 1.4 * yln, pch=19, col="darkgreen", cex = 1.5)}
    
  })
  
  output$dataHTVar <- renderDataTable({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, var)
    statidxn <- c(
      "Moyenne",
      "Variance",
      "Écart-Type",
      "Symétrie",
      "Voussure"
    )
    statidx <- c(mean(dat4), var(dat4), sd(dat4), skewness(dat4), kurtosis(dat4))
    
    param <- c(parMU4, parSIG4^2, parSIG4, 0, 3)
    
    res <- data.frame(statidxn, round(param, 4), round(statidx, 4))
    colnames(res) <- c("Indice", "Paramètre", "Statistique")
    
    datatable(res,
              colnames = c("Indice descriptif", "Valeur Théorique", "Valeur Observée"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: Center; font-size:15px; font-weight: bold; ',
                (paste("Description de l'échantillon analysé"))
                
              ),
              rownames = FALSE,
              class = "cell-border stripe",
              options = list(
                info = FALSE,
                paging = FALSE,
                searching = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(width = "30%", targets = "_all"))
              )
    )  %>%
      formatStyle(
        'Paramètre',
        backgroundColor = 'yellow')
    
    
  })
  
  
  output$calculVar4 <- renderUI({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, var)
    std.err <- sd(r.stat4)
    
    r.stat4 <- r.stat4 + (input$hyppar - mean(r.stat4))
    
    mean.var <- mean(r.stat4)
    std.err <- sd(r.stat4)
    
    intc <- if(input$direction == "1"){quantile(r.stat4, qtl1()[1])} else 
      if(input$direction == "2"){quantile(r.stat4, qtl1()[2])} else {
        quantile(r.stat4, qtl2())}

    tc <- qchisq(qtl2(), input$n4 - 1)
    
    cv <- c(qchisq(qtl1(), input$n4 - 1), qchisq(qtl2(), input$n4 - 1))[as.integer(input$direction)]
    
    tst <- (input$n4 - 1) * var(dat4) / input$hyppar
    
    withMathJax( 
      
      helpText(
        
        wellPanel(
          style = "background: lightblue",
          strong("Hypothèses:"), p(),
          "$$H_0:\\sigma^2 = ", input$hyppar, "$$",
          "$$H_1:\\sigma^2", c("<", ">", "\u2260")[as.integer(input$direction)], input$hyppar, "$$",),
        
        h5(paste("$$\\chi_{dl}^2=\\frac{(n-1)s^2}{\\sigma^2}=\\frac{(", input$n4, "-1)", round(var(dat4), 3), "}{", input$hyppar, "}=", round(tst, 3), "$$")),

        strong(paste0("Valeur", ifelse(input$direction == "3", "s", ""), " Critique", ifelse(input$direction == "3", "s", "")), " [test ", ifelse(input$direction != "3", "unilatéral]", "bilatéral]:")),
        paste("$$\\chi_{", input$n4 - 1, ";", 1 - input$sig.level4, "}^2=", round(cv, 3), if(input$direction == "3"){paste(" / ", round(qchisq(qtl2()[2], input$n4 - 1), 3))}, "$$"), p(),

        h4(strong("Conclusion:"),
           if((input$direction == "1" & tst < cv) | (input$direction == "2" & tst > cv) | ((input$direction == "3") & (tst < cv | tst > qchisq(qtl2()[2], input$n4 - 1)))){paste("p < ", input$sig.level4,  " : Rejet de $H_0$ ")} else
           { paste("p > ", input$sig.level4,  " : Non Rejet de $H_0$ ")}
           ),
        p(),
    
    withMathJax(         
      helpText(
        h4(strong("Calcul de l'intervalle de confiance:"),
           
           paste("$$\\frac{(n-1)s^2}{\\chi_{dl,1-\\alpha/2}^2} \\le \\sigma^2 \\le \\frac{(n-1)s^2}{\\chi_{dl,\\alpha/2}^2} $$"),
           paste("$$\\chi_{", input$n4 - 1, ", ", round(qtl2()[1], 3), "}^2 = ", round(tc[1], 3), "$$"),
           paste("$$\\chi_{", input$n4 - 1, ", ", round(qtl2()[2], 3), "}^2 = ", round(tc[2], 3), "$$"),  p(),
           
           paste("$$\\frac{(", input$n4, "-1)", round(var(dat4), 3), "}{", round(tc[2], 3), "} \\le \\sigma^2 \\le \\frac{(", input$n4, "-1)", round(var(dat4), 3), "}{",round(tc[1], 3), "}$$")),
        
        h4(paste("$$\\color{red}{", round((input$n4-1) * var(dat4) / tc[2], 3), " \\le \\sigma^2 \\le ", round((input$n4-1) * var(dat4) / tc[1], 3), "}$$")),
        
        p(),
        
        h4(strong("Calcul de l'intervalle des valeurs probables de $s^2$, sous $H_0$:"),
           
           paste("$$\\frac{(n-1)s^2}{\\chi_{dl,1-\\alpha/2}^2} \\le s^2 \\le \\frac{(n-1)s^2}{\\chi_{dl,\\alpha/2}^2} $$"),
           p(),
           
           paste("$$\\frac{(", input$n4, "-1)", input$hyppar, "}{", round(tc[2], 3), "} \\le s^2 \\le \\frac{(", input$n4, "-1)",input$hyppar, "}{",round(tc[1], 3), "}$$")),
        
        h4(paste("$$\\color{red}{", round((input$n4-1) * input$hyppar / tc[2], 3), " \\le s^2 \\le ", round((input$n4-1) * input$hyppar / tc[1], 3), "}$$")),
        
        
      ))
      )
    )
    
  })
  
  output$interprVar <- renderUI({
    
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, var)
    r.stat4 <- r.stat4 + (input$hyppar - mean(r.stat4))
    std.err <- sd(r.stat4)
    
    smplVAR <- var(dat4)
    
    intc <- if(input$direction == "1"){quantile(r.stat4, qtl1()[1])
      icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), " \\le \\sigma^2 \\le  \\infty $")} else 
        if(input$direction == "2"){quantile(r.stat4, qtl1()[2])
          icTxt <- paste("$-\\infty \\le \\sigma^2 \\le ", round(quantile(r.stat4, qtl1()[2]), 3),"$")} else {
            icTxt <- paste("$", round(quantile(r.stat4, qtl1()[1]), 3), "\\le \\sigma^2 \\le ", round(quantile(r.stat4, qtl1()[2]), 3), " $")}
    
    
    intclim <-  if(input$direction == "1"){c(-Inf, quantile(r.stat4, qtl1()[1]))} else 
      if(input$direction == "2"){c(quantile(r.stat4, qtl1()[2]), Inf)} else {
        quantile(r.stat4, qtl2())}
    
    merr <- abs(input$hyppar - smplVAR)
    
    prt <- c(mean(r.stat4 < smplVAR), 
             mean(r.stat4 > smplVAR), 
             mean(r.stat4 < (input$hyppar - merr) | (r.stat4 > input$hyppar + merr)))[as.integer(input$direction)]
    
    
    cv <- c(qchisq(qtl1(), df = input$n4 - 1), qchisq(qtl2()[2], df = input$n4 - 1), qchisq(qtl2(), df = input$n4 - 1))[as.integer(input$direction)]
    
    cvsgn <- c("-", "+", "$\\pm$")[which(c("1", "2", "3") == input$direction)]
    
    tst <- (input$n4 - 1) * smplVAR / input$hyppar
    
    p <- c(pchisq(tst, df = input$n4 - 1, lower.tail = TRUE), 
                  pchisq(tst, df = input$n4 - 1, lower.tail = FALSE), 
                  pchisq(tst, df = input$n4 - 1, lower.tail = FALSE))[as.integer(input$direction)]
               
    
    IC <- c((input$n4 - 1) * smplVAR / qchisq(input$sig.level / 2, df = input$n4 - 1),
            (input$n4 - 1) * smplVAR / qchisq(1 - (input$sig.level / 2), df = input$n4 - 1))
    
    dirtn <- c(paste("\\le"), 
               paste("\\ge"), 
               paste("\\le ", round(input$hyppar - merr, 3), "$ ou $\\bar{X} \\ge "))[which(input$direction == c("1", "2", "3"))]
    
    helpText(
      withMathJax(
        h3("Interprétation:"),
        h4(
          strong("Résultats de l'analyse par Ré-échantillonnage:"), p(),
          "L'analyse illustrée par le diagramme ci-dessus indique que si une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " définie par $H_0: \\mu=", input$hyppar, "$ est vraie, ",
          "la probabilité d'obtenir $s^2", dirtn, round(input$hyppar + merr, 3), "$, est $p=", round(prt, 3), "$.  Cette probabilité est jugée ",
          ifelse(prt < input$sig.level4, "faible", "élevée"), ", ce qui indique que la variance de la population d'où provient l'échantillon ", ifelse(prt < input$sig.level4, "pourrait ne pas ", "pourrait "), 
          "être égale à ", input$hyppar, ". On ", ifelse(prt < input$sig.level4, "peut ", "ne peut pas "), "rejeter $H_0$ sur la base des observations.", 
          "L'intervalle de confiance regroupant les valeurs probables des variances, pour un niveau de confiance $(1-\\alpha) = ", (1-(input$sig.level4)) * 100, "\\%$ et en supposant $H_0$ vraie, est le suivant:", p(),
          
          paste("$C[$", icTxt, "] = ", (1 - input$sig.level4) * 100, "$\\%$"), p(),
          
          "Les limites de cet intervalle sont simplement ",
          
          if(input$direction == "1"){paste("le percentile $P_{", 
                                           (1 - input$sig.level4 / 2) * 100, 
                                           "}$ et $\\infty$")} else
                                             if(input$direction == "2"){paste("$\\infty$ et le percentile $P_{", 
                                                                              (1 - input$sig.level4 / 2) * 100, 
                                                                              "}$")} else {
                                                                                paste("les percentiles $P_{", 
                                                                                      (input$sig.level4 / 2) * 100, 
                                                                                      "}$ et $P_{", 
                                                                                      (1 - input$sig.level4 / 2) * 100, 
                                                                                      "}$")}, 
          " de la distribution des variances produite par ré-échantillonnage. ", 
          "Cet intervalle est parfaitement compatible avec le résultat obtenu. En effet, puisque la valeur de $s^2 = ", round(smplVAR, 3), 
          
          ifelse((input$direction == "1" & between(smplVAR, intclim[1], Inf)) 
                 || (input$direction == "2" & between(smplVAR, -Inf, intclim[1])) 
                 || (input$direction == "3" & between(smplVAR, intclim[1], intclim[2])),
                 "$ fait partie de cet intervalle, on ne peut pas ",
                 "$ ne fait pas partie de cet intervalle, on peut  "), 
          
          " rejeter $H_0$.", p(),
          
          strong("Test d'hypothèse: Approche paramétrique"), p(),
          
          "Si $H_0$, une hypothèse ", ifelse(input$direction != "3", "unilatérale", "bilatérale"), " selon laquelle $\\sigma^2 = ", input$hyppar,  
          "$ est vraie, alors la probabilité d'obtenir un ", 
          paste("$\\chi_{", input$n4 - 1, "}^2$"), " égal ou plus extrême que ", round(tst, 3), ", la valeur obtenue avec ",
          "un échantillon aléatoire de $n = ", input$n4, "$ observations, est égale à ", paste("$p = ", round(p, 3)), 
          "$. Une telle probabilité est ", ifelse(p < input$sig.level4, "faible", "élevée"), ", de sorte que les données ",
          ifelse(p < input$sig.level4, "vont à l'encontre de $H_0$, que l'on peut rejeter.", "ne permettent pas de rejeter $H_0$."), p(),
          
                    strong("Intervalle de confiance:"), p(),
                    "En tirant un échantillon de ", input$n4, " observations d'une population normalement distribuée ",
                    ifelse(input$param4 == "Moyenne" & input$knownSD4, "dont $\\sigma$ est connu, ",  "dont $\\sigma$ est inconnu, "),
                    "la probabilité que l'intervalle affiché ci-dessus capture la ", input$param4, " réelle de la population ",
                    "est égale à ", (1 - input$sig.level4) * 100, "%.  En d'autres termes, dans les conditions décrites, il y a ",  (1 - input$sig.level4) * 100, "% des chances ",
                    "que parmi les valeurs incluses dans cet intervalle, on trouve la valeur de ", ppp(), "..."
        )
      )
    )
    
    
    
    
  })
  
  
  output$interpret4 <- renderUI({
    
    helpText(
      withMathJax(
        h3("Interprétation:"),
        h4(
          strong("Test d'hypothèse:"), p(),
          "Si $H_0$ selon laquelle ", ppp() ," = ", ifelse(input$param4 == "Proportion", input$hypPR, input$hyppar), 
          " est vraie, alors la probabilité d'obtenir un ", ifelse(input$knownSD4, "$z$", "$t$"), " égal ou plus extrême que celui obtenu avec ",
          "un échantillon aléatoire de $n = ", input$n4, "$ observations est ...", p(),
          strong("Intervalle de confiance:"), p(),
          "En tirant un échantillon de ", input$n4, " observations d'une population normalement distribuée ",
          ifelse(input$param4 == "Moyenne" & input$knownSD4, "dont $\\sigma$ est connu, ",  "dont $\\sigma$ est inconnu, "),
          "la probabilité que l'intervalle affiché ci-dessus capture la ", input$param4, " réelle de la population ",
          "est égale à ", (1 - input$sig.level4) * 100, "%.  En d'autres termes, dans les conditions décrites, il y a ",  (1 - input$sig.level4) * 100, "% des chances ",
          "que parmi les valeurs incluses dans cet intervalle, on trouve la valeur de ", ppp(), "...")
      )
    )
    
  })
  
  output$hypot <- renderUI({
    
                 conditionalPanel(
                   condition = "input.param4 != 'Proportion'",
                   numericInput("hyppar",
                                "Paramètre proposé:",
                                min = 0,
                                value = 0,
                                step = 0.1)
                 )
                 
                 conditionalPanel(
                   condition = "input.param4 == 'Proportion'",
                   numericInput("hypPR",
                                "Paramètre proposé:",
                                min = 0.01,
                                max = 0.99,
                                step = 0.01,
                                value = 0.5)
                 )
    
    
  })
  
  output$normDistrib <- renderPlot({
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    m <- mean(dat4)
    
    merr <- abs(input$hyppar - mean(dat4))
    se4 <- ifelse(input$knownSD4, parSIG4 / sqrt(input$n4), sd(dat4) / sqrt(input$n4))
    
    direction <- c("less", "greater", "beyond")[as.integer(input$direction)]
    
    if(input$knownSD4){
      shaded <- if(input$direction == "1"){input$hyppar + se4 * qnorm(input$sig.level4)} else
      if(input$direction == "2"){input$hyppar + se4 * qnorm(1 - input$sig.level4)} else {
        c(input$hyppar + se4 * qnorm(input$sig.level4), 
          input$hyppar + se4 * qnorm(1 - input$sig.level4))}} 
    
    else {
            shaded <- if(input$direction == "1"){qt(input$sig.level4, df = input$n4 - 1)} else
            if(input$direction == "2"){qt(1 - input$sig.level4,  df = input$n4 - 1)} else {
              c( qt(input$sig.level4 / 2, df = input$n4 - 1), 
                 qt(1 - input$sig.level4 / 2, df = input$n4 - 1))}}
    
    if(input$knownSD4){
      plot_norm(mean = input$hyppar, 
                sd = round(se4, 3), 
                direction = direction, 
                shadeValues = shaded,
                col.shade = "red")} else {
                  plot_t(df = input$n4 - 1, 
                            direction = direction, 
                            shadeValues = shaded,
                            col.shade = "red")}
                  
    
  })
  
  output$normDistrib2 <- renderPlot({
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    m <- mean(dat4)
    
    se4 <- sqrt(input$hypPR * (1 - input$hypPR) / input$n4)
    
    merr <- abs(input$hypPR - mean(dat4))

    direction <- c("less", "greater", "beyond")[as.integer(input$direction)]

    shaded <- if(input$direction == "1"){input$hypPR + se4 * qnorm(input$sig.level4)} else
        if(input$direction == "2"){input$hypPR + se4 * qnorm(1 - input$sig.level4)} else {
          c(input$hypPR + se4 * qnorm(input$sig.level4), 
            input$hypPR + se4 * qnorm(1 - input$sig.level4))}
    
      plot_norm(mean = input$hypPR, 
                sd = round(se4, 3), 
                direction = direction, 
                shadeValues = shaded,
                col.shade = "red")
    
    
  })
  
  output$chi2Distrib <- renderPlot({
    resamples4 <- Data4()[[1]]
    dat4 <- Data4()[[2]]
    parMU4 <- Data4()[[3]]
    parSIG4 <- Data4()[[4]]
    
    r.stat4 <- sapply(resamples4, var)
    r.stat4 <- r.stat4 + (input$hyppar - mean(r.stat4))
    std.err <- sd(r.stat4)
    chi2 <- (input$n4 - 1) * sd(dat4) / input$hyppar
    
    smplVAR <- var(dat4)
    
    direction <- c("less", "greater", "beyond")[as.integer(input$direction)]
    
    shaded <- if(input$direction == "1"){input$hyppar + qchisq(input$sig.level4, input$n4 - 1)} else
      if(input$direction == "2"){input$hyppar + qchisq(1 - input$sig.level4, input$n4 -1)} else {
        c(input$hyppar + qchisq(input$sig.level4, input$n4 - 1), 
          input$hyppar + qchisq(1 - input$sig.level4, input$n4 - 1))}
    
    plot_chi2(df = input$n4 -1, 
              alpha = input$sig.level4,
              tail = c("left", "right", "two")[as.integer(input$direction)],
              vline = chi2, 
              col = "red", 
              col.alpha = 0.5)

  })
  
}

shinyApp(ui = ui, server = server)