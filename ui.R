#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library("shiny")
library("shinythemes")
library("coda")
library("rstan")
library("rstanarm")
library("ggmcmc")
library("bayesplot")
library("RColorBrewer")
library("gridExtra")
library("DT")
library("Rcpp")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("lumen"),
  tags$style(HTML("
    body, pre { font-size: 11pt; }
                  ")),
  
  #### Header ####
  fluidRow(
    class = 'headerrow',
    column(
      width = 9,
      
      h3(
        "The Importance of Prior Sensitivity Analysis in Bayesian Statistics:
        Demonstrations using an Interactive Shiny App",
        span(style = "font-weight: 150"),
        style = "color: #000; text-align: left;"
      )
      #p(
      #  "AUTHORS",
      #  style = "color: #000; text-align: left;"
      #)
    ),
    column(width = 1),
    
    column(
      width = 2,
      br()
      #img(
      #  src = 'LOGO',
      #  align = "right",
      #  height = 50
      #)
    )
  ),
  
  
  
  #### Main Page ####
  navbarPage(
    "",
    #### First Page ####
    tabPanel(
      "Start Here",
      includeMarkdown("hello.md"),
      br(),
      HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
      br(),
      fluidRow(
        column(6,
               includeMarkdown("senssteps.md"),
               ),
        column(6,
             includeMarkdown("navigate.md")
             )
        )
    ),
    #### Original Study ####
    tabPanel(
      "Original Study",
      h3("Overview of original study"),
      br(),
      HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
      br(),
      fluidRow(column(
        6,
        img(
          class = "img-responsive",
          width = "100%",
          src = "RegressionExample.png",
          align = "center"
        )
      ),
      column(6,
             includeMarkdown("originalstudy.md"))),
      HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
      fluidRow(column(
        6,
        h4("Motivation for Chosen Priors"),
        br(),
        includeMarkdown("originalpriors.md")
      ),
      column(6,
             plotOutput("ogpriorPlot"))),
      HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
      fluidRow(column(6,
                      plotOutput("ogtracePlot")),
               column(
                 6,
                 h4("Assessing Convergence"),
                 br(),
                 includeMarkdown("originalconvergence.md")
               )),
      HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
      fluidRow(
        column(6,
               h4("Results"),
               br(),
               includeMarkdown("originalresults.md")),
        column(6,
               DT::dataTableOutput("ogestTable"))
      ),
      br(),
      fluidRow(
        column(6,
               plotOutput("ogposteriorPlot")),
        column(6,
               plotOutput("ogposteriorHist"))
      ),
      br()
      
    ),
    #### Intercept ####
    tabPanel(
      "Sensitivity Analysis: Intercept",
      withMathJax(includeMarkdown("sensintercept.md")),
      sidebarLayout(
        sidebarPanel(
          h3("Specify Alternative Priors for the intercept of Cynicism"),
          wellPanel(
            h4("Alternative 1: Prior Hyperparameters"),
            numericInput("alt_mu5",
                         "Mean:",
                         value = 0),
            numericInput("alt_var5",
                         "Variance:",
                         value = 100,
                         min = .1)
          ),
          wellPanel(
            h4("Alternative 2: Prior Hyperparameters"),
            numericInput("alt_mu6",
                         "Mean:",
                         value = 20),
            numericInput("alt_var6",
                         "Variance:",
                         value = 10,
                         min = .1)
          ),
          actionButton(inputId = "runmodels_intercept", label = "Run Sensitivity Analysis!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "SensIntercept",
            tabPanel("Prior Densities",
                     br(),
                     includeMarkdown("senspriors.md"),
                     plotOutput("priorPlot_intercept")),
            tabPanel(
              "Convergence",
              br(),
              includeMarkdown("sensconvergence.md"),
              plotOutput("traceplotIntercept1"),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              plotOutput("traceplotIntercept2")
            ),
            tabPanel(
              "Posterior Densities",
              br(),
              includeMarkdown("sensdensities.md"),
              plotOutput("posteriorPlot30", height = "600px")
              
            ),
            tabPanel(
              "Posterior Estimates",
              br(),
              includeMarkdown("sensresults.md"),
              fluidRow(column(
                12,
                DT::dataTableOutput("interceptalt1estTable")
              )),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              fluidRow(column(
                12,
                DT::dataTableOutput("interceptalt2estTable")
              ))
            )
          )
        ) # End of mainPanel showing results for Lack of Trust
      ) # End of sidebarLayout for Lack of Trust
    ),
    #### Sex ####
    tabPanel(
      "Sensitivity Analysis: Sex",
      withMathJax(includeMarkdown("senssex.md")),
      sidebarLayout(
        sidebarPanel(
          h3("Specify Alternative Priors for Sex as a predictor of Cynicism"),
          wellPanel(
            h4("Alternative 1: Prior Hyperparameters"),
            numericInput("alt_mu3",
                         "Mean:",
                         value = 5),
            numericInput("alt_var3",
                         "Variance:",
                         value = 5,
                         min = .1)
          ),
          wellPanel(
            h4("Alternative 2: Prior Hyperparameters"),
            numericInput("alt_mu4",
                         "Mean:",
                         value = -10),
            numericInput("alt_var4",
                         "Variance:",
                         value = 5,
                         min = .1)
          ),
          actionButton(inputId = "runmodels_sex", label = "Run Sensitivity Analysis!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "SensSex",
            tabPanel("Prior Densities",
                     br(),
                     includeMarkdown("senspriors.md"),
                     plotOutput("priorPlot_sex")),
            tabPanel(
              "Convergence",
              br(),
              includeMarkdown("sensconvergence.md"),
              plotOutput("traceplotSex1"),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              plotOutput("traceplotSex2")
            ),
            tabPanel(
              "Posterior Densities",
              br(),
              includeMarkdown("sensdensities.md"),
              plotOutput("posteriorPlot20", height = "600px")
            ),
            #tabPanel(
            #  "Posterior Intervals",
            #  plotOutput("intervalSex", height = "600px")
            #  
            #),
            tabPanel(
              "Posterior Estimates",
              br(),
              includeMarkdown("sensresults.md"),
              fluidRow(column(12,
                              DT::dataTableOutput("sexalt1estTable"))),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              fluidRow(column(12,
                              DT::dataTableOutput("sexalt2estTable")))
            )
            
            
          )
        )
      )
    ),
    #### Trust ####
    tabPanel(
      "Sensitivity Analysis: Lack of Trust",
      withMathJax(includeMarkdown("senstrust.md")),
      sidebarLayout(
        sidebarPanel(
          h3("Specify Alternative Priors for Lack of Trust as predictor of Cynicism"),
          wellPanel(
            h4("Alternative 1: Prior Hyperparameters"),
            numericInput("alt_mu1",
                         "Mean:",
                         value = 0),
            numericInput("alt_var1",
                         "Variance:",
                         value = 100,
                         min = .1)
          ),
          wellPanel(
            h4("Alternative 2: Prior Hyperparameters"),
            numericInput("alt_mu2",
                         "Mean:",
                         value = 0),
            numericInput("alt_var2",
                         "Variance:",
                         value = 5,
                         min = .1)
          ),
          actionButton(inputId = "runmodels_trust", label = "Run Sensitivity Analysis!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "SensTrust",
            tabPanel("Prior Densities",
                     br(),
                     includeMarkdown("senspriors.md"),
                     plotOutput("priorPlot_trust")),
            tabPanel(
              "Convergence",
              br(),
              includeMarkdown("sensconvergence.md"),
              plotOutput("traceplotTrust1"),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              plotOutput("traceplotTrust2")
            ),
            tabPanel(
              "Posterior Densities",
              br(),
              includeMarkdown("sensdensities.md"),
              plotOutput("posteriorPlot10", height = "600px")
              
            ),
            #tabPanel(
            #  "Posterior Intervals",
            #  plotOutput("intervalTrust", height = "600px")
            #  
            #),
            tabPanel(
              "Posterior Estimates",
              br(),
              includeMarkdown("sensresults.md"),
              fluidRow(column(
                12,
                DT::dataTableOutput("trustalt1estTable")
              )),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              fluidRow(column(
                12,
                DT::dataTableOutput("trustalt2estTable")
              ))
            )
          )
        ) # End of mainPanel showing results for Lack of Trust
      ) # End of sidebarLayout for Lack of Trust
    ),
    #### Residual Variance ####
    tabPanel(
      "Sensitivity Analysis: Residual Variance",
      withMathJax(includeMarkdown("sensresidual.md")),
      sidebarLayout(
        sidebarPanel(
          h3("Specify Alternative Priors for the residual variance of Cynicism"),
          wellPanel(
            h4("Alternative 1: Prior Hyperparameters"),
            numericInput("alt_mu7",
                         "Shape:",
                         value = 1,
                         min = .000001),
            numericInput("alt_var7",
                         "Scale:",
                         value = 0.5,
                         min = .000001)
          ),
          wellPanel(
            h4("Alternative 2: Prior Hyperparameters"),
            numericInput("alt_mu8",
                         "Shape:",
                         value = .1,
                         min = .000001),
            numericInput("alt_var8",
                         "Scale:",
                         value = .1,
                         min = .000001)
          ),
          actionButton(inputId = "runmodels_resid", label = "Run Sensitivity Analysis!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "SensResid",
            tabPanel("Prior Densities",
                     br(),
                     includeMarkdown("senspriors.md"),
                     plotOutput("priorPlot_resid")),
            tabPanel(
              "Convergence",
              br(),
              includeMarkdown("sensconvergence.md"),
              plotOutput("traceplotResid1"),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              plotOutput("traceplotResid2")
            ),
            tabPanel(
              "Posterior Densities",
              br(),
              includeMarkdown("sensdensities.md"),
              plotOutput("posteriorPlot40", height = "600px")
              
            ),
            tabPanel(
              "Posterior Estimates",
              br(),
              includeMarkdown("sensresults.md"),
              fluidRow(column(
                12,
                DT::dataTableOutput("residalt1estTable")
              )),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              fluidRow(column(
                12,
                DT::dataTableOutput("residalt2estTable")
              ))
            )
          )
        ) # End of mainPanel showing results for Lack of Trust
      ) # End of sidebarLayout for Lack of Trust
    ),
    #### All Param ####
    tabPanel(
      "Sensitivity Analysis: All Parameters",
      withMathJax(includeMarkdown("senscombo.md")),
      sidebarLayout(
        sidebarPanel(
          h3("Specify Combination of Priors for all parameters in the model"),
          wellPanel(
            wellPanel(
              uiOutput("param3_opts") #,
              #textOutput("txt_param1")
            ),
            uiOutput("param1_opts") #,
            #textOutput("txt_param1")
          ),
          wellPanel(
            uiOutput("param2_opts")#,
            #textOutput("txt_param2")
          ),
          wellPanel(
            uiOutput("param4_opts") #,
            #textOutput("txt_param1")
          ),
          actionButton(inputId = "runmodels_both", label = "Run Sensitivity Analysis!")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "SensBoth",
            tabPanel("Prior Densities",
                     br(),
                     includeMarkdown("senspriors.md"),
                     span(textOutput("noboth"), style="color:red"),
                     plotOutput("priorPlot_both")),
            tabPanel(
              "Convergence",
              br(),
              includeMarkdown("sensconvergence.md"),
              plotOutput("ogtracePlot1"),
              HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">'),
              plotOutput("traceplotBoth2")
            ),
            tabPanel(
              "Posterior Densities",
              br(),
              includeMarkdown("sensdensities.md"),
              plotOutput("posteriorPlot12", height = "600px")
              
            ),
            #tabPanel(
            #  "Posterior Intervals",
            #  plotOutput("intervalBoth", height = "600px")
            #  
            #),
            tabPanel(
              "Posterior Estimates",
              br(),
              includeMarkdown("sensresults.md"),
              fluidRow(column(
                12,
                DT::dataTableOutput("bothalt1estTable")
              ))
            )
          )
        ) # End of mainPanel showing results for interaction of priors
      ) # End of sidebarLayout for interaction of priors
    ) # End of tabPanel for interaction of priors
  ), # End of navbarPage
  ## Footer of App:
  fluidRow(class = 'headerrow',
           column(12,
                  HTML('<hr style="color: #cccccc; border-top: 1.5px solid;">')
           )
           #column(12,
           #        p("This app is created by: AUTHOR. Contact ",
          #          a( "EMAIL", href="mailto:EMAIL"),
           #         " or ",
            #        a( "EMAIL", href="mailto:EMAIL"),
             #       "with any questions, bug reports, or feature requests.",
              #      style = "font-size:11px"
               #   )
          # )
  ) # End of footer
)) # End of fluidpage + shinyUI
