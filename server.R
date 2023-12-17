#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# packages <- c("coda", "shiny", "stan","stanarm","ggmcmc", 
#               "bayesplot", "cowplot", "RColorBrewer", "gridExtra",
#               "grid", "DT", "Rcpp", "shinythemes")
# 
# for(i in 1:length(packages)) {
#   install.packages(packages[i], dependencies = TRUE)
# }

library("shiny")
library("coda")
library("rstan")
library("rstanarm")
library("ggmcmc")
library("bayesplot")
library("cowplot")
library("RColorBrewer")
library("gridExtra")
library("grid")
library("DT")
library("Rcpp")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #############################################################################
  ## Original Study ####
  
  ## Static plot showing the original prior distribution specifications
  ## These plots (p1-p6) are created in global.R
  output$ogpriorPlot <- renderPlot({
    priors_og <- grid.arrange(p1, p2, p3, p4, nrow = 2, top = textGrob("Original Prior Distributions",
                                                                       gp=gpar(fontsize=18)))
    priors_og
  })
  
  ## Static table showing original posterior estimates
  ## The sum_og object is created in global.R
  output$ogestTable <- DT::renderDataTable({
    sum_og <- sum_og
    
    sum_og[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
    
    table_note <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%).'",
      "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
      "}"
    )
    
    DT::datatable(sum_og,
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(dom = 't',
                                 drawCallback = JS(table_note)),
                  colnames = c('Parameter' = 1,
                               'Mean' = 2,
                               'SD' = 3,
                               '5%' = 4,
                               '50%' = 5,
                               '95%' = 6),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                    'Original Priors: Posterior Estimates')) %>% formatRound(2:6, digits=3)
  })
  
  ## Static plot showing traceplot of original analysis
  ## post_all_og is created in global.R
  output$ogtracePlot <- renderPlot({
    iterations <-  iterations
    warm_up <- iterations/2
    
    
    
    mcmc_trace(post_all_og, n_warmup = warm_up,
               facet_args = list(nrow = 2, labeller = label_parsed)) +
      labs(title = "Original Priors: Trace Plots",
           caption = "*ESS: Effective Sample Size") +
      geom_blank(data = blank_data, aes(x = x, y=y))+
      geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
      theme_cowplot() +
      theme(text = element_text(family = "sans"),
            plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
            strip.text.x = element_text(size = 18),
            strip.background = element_rect(colour=NA, fill=NA))
  })
  
  ## Static plot showing posterior densities of original analysis
  ## post_half_og created in global.R (this object only includes post warm-up iterations)
  output$ogposteriorPlot <- renderPlot({
    mcmc_dens_overlay(post_half_og,
                      facet_args = list(nrow = 2, labeller = label_parsed)) +
      ggtitle("Original Priors: Posterior Density Plots") +
      theme_cowplot() +
      theme(text = element_text(family = "sans"),
            plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
            strip.text.x = element_text(size = 18, family = "sans"),
            strip.background = element_rect(colour=NA, fill=NA))
  })
  
  ## Static plot showing posterior histograms of original analysis
  ## post_half_og created in global.R (this object only includes post warm-up iterations)
  output$ogposteriorHist <- renderPlot({
    mcmc_hist(post_half_og,
                      facet_args = list(nrow = 2, labeller = label_parsed)) +
      ggtitle("Original Priors: Posterior Histograms") +
      theme_cowplot() +
      theme(text = element_text(family = "sans"),
            plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
            strip.text.x = element_text(size = 18, family = "sans"),
            strip.background = element_rect(colour=NA, fill=NA))
  })
  
  #############################################################################
  ## Extract selected values for alternative priors for the final page ####
  
  ## Reactive radiobutton menu for Cynicism ~ Sex
  output$param1_opts <- renderUI({
    option2 <- paste0("Alternative 1: N(", input$alt_mu3, ", ", input$alt_var3, ")")
    option3 <- paste0("Alternative 2: N(", input$alt_mu4, ", ", input$alt_var4, ")")
    
    options <- list("Original Prior: N(0, 10)" = "1",
                    option2 = "2",
                    option3 = "3")
    names(options)[2] <- option2
    names(options)[3] <- option3
    
    radioButtons("param1", h4("Cynicism ~ Sex: Prior Hyperparameters"), 
                 choices = options,
                 selected = "1")
    
  })
  
  ## Reactive radiobutton menu for Cynicism ~ Trust
  output$param2_opts <- renderUI({
    option2 <- paste0("Alternative 1: N(", input$alt_mu1, ", ", input$alt_var1, ")")
    option3 <- paste0("Alternative 2: N(", input$alt_mu2, ", ", input$alt_var2, ")")
    
    options <- list("Original Prior: N(6, 1)" = "1",
                    option2 = "2",
                    option3 = "3")
    names(options)[2] <- option2
    names(options)[3] <- option3
    
    radioButtons("param2", h4("Cynicism ~ Trust: Prior Hyperparameters"), 
                 choices = options,
                 selected = "1")
    
  })
  
  ## Reactive radiobutton menu for Intercept
  output$param3_opts <- renderUI({
    option2 <- paste0("Alternative 1: N(", input$alt_mu5, ", ", input$alt_var5, ")")
    option3 <- paste0("Alternative 2: N(", input$alt_mu6, ", ", input$alt_var6, ")")
    
    options <- list("Original Prior: N(41, 10)" = "1",
                    option2 = "2",
                    option3 = "3")
    names(options)[2] <- option2
    names(options)[3] <- option3
    
    radioButtons("param3", h4("Cynicism Intercept: Prior Hyperparameters"), 
                 choices = options,
                 selected = "1")
    
  })
  
  ## Reactive radiobutton menu for Residual Variance
  output$param4_opts <- renderUI({
    option2 <- paste0("Alternative 1: IG(", input$alt_mu7, ", ", input$alt_var7, ")")
    option3 <- paste0("Alternative 2: IG(", input$alt_mu8, ", ", input$alt_var8, ")")
    
    options <- list("Original Prior: IG(.5, .5)" = "1",
                    option2 = "2",
                    option3 = "3")
    names(options)[2] <- option2
    names(options)[3] <- option3
    
    radioButtons("param4", h4("Cynicism Residual: Prior Hyperparameters"), 
                 choices = options,
                 selected = "1")
    
  })
  
  #output$txt_param1 <- renderText({
  #  paste("You selected", input$param1)
  #})
  
  #output$txt_param2 <- renderText({
  #  paste("You selected", input$param2)
  #})
  
  ############################################################################# 
  #### Alternative Prior Plots ####
  ## Sex: 
  ## Plot that shows the alternative priors that are selected through the 
  ## numeric inputs on the screen 
  output$priorPlot_sex <- renderPlot({
    #colors <- brewer.pal(4, "Blues")
    ## minimum and maximum values are adjusted based on the actual prior values that are selected
    x_min <- min(6, input$alt_mu4, input$alt_mu3) - 3*sqrt(max(1, input$alt_var4, input$alt_var3))
    x_max <- max(6, input$alt_mu4, input$alt_mu3) + 3*sqrt(max(1, input$alt_var4, input$alt_var3))
    
    # Save the x-axis limits in a dataframe
    df <- data.frame(x = c(x_min, x_max))
    
    # Create an emply plot that has the x-axis in it
    p <- ggplot(df, aes(x=x)) +
      # First, add the prior distribution of the original prior. 
      # The first line creates an area (filled in with color), the second line creates a line graph
      stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10))) +
      # Repeat the above for each of the two alternative priors
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu3, sd = sqrt(input$alt_var3)), geom = "area", aes(fill = "Alternative 1"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu3, sd = sqrt(input$alt_var3))) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu4, sd = sqrt(input$alt_var4)), geom = "area", aes(fill = "Alternative 2"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu4, sd = sqrt(input$alt_var4))) +
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle("Comparison of Priors") +
      xlab(bquote(beta[sex])) +
      theme_classic() +  
      theme(legend.position="bottom", 
            axis.title.x=element_text(family = "sans", size = 18),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, family="sans", size = 18),
            legend.text=element_text(size=14))
    
    # Print the plot
    p
  })
  
  ## Trust: 
  ## Plot that shows the alternative priors that are selected through the 
  ## numeric inputs on the screen 
  output$priorPlot_trust <- renderPlot({
    #colors <- brewer.pal(4, "Blues")
    ## minimum and maximum values are adjusted based on the actual prior values that are selected
    x_min <- min(6, input$alt_mu1, input$alt_mu2) - 3*sqrt(max(1, input$alt_var1, input$alt_var2))
    x_max <- max(6, input$alt_mu1, input$alt_mu2) + 3*sqrt(max(1, input$alt_var1, input$alt_var2))
    
    # Save the x-axis limits in a dataframe
    df <- data.frame(x = c(x_min, x_max))
    
    # Create an emply plot that has the x-axis in it
    p <- ggplot(df, aes(x=x)) +
      # First, add the prior distribution of the original prior. 
      # The first line creates an area (filled in with color), the second line creates a line graph
      stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1))) +
      # Repeat the above for each of the two alternative priors
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu1, sd = sqrt(input$alt_var1)), geom = "area", aes(fill = "Alternative 1"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu1, sd = sqrt(input$alt_var1))) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu2, sd = sqrt(input$alt_var2)), geom = "area", aes(fill = "Alternative 2"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu2, sd = sqrt(input$alt_var2))) +
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle("Comparison of Priors") +
      xlab(bquote(beta[lackoftrust])) +
      theme_classic() +  
      theme(legend.position="bottom",
            axis.title.x = element_text(family="sans", size = 18),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, family="sans", size = 18),
            legend.text=element_text(size=14))
    
    # Print the plot
    p
  })
  
  ## Intercept: 
  ## Plot that shows the alternative priors that are selected through the 
  ## numeric inputs on the screen 
  output$priorPlot_intercept <- renderPlot({
    #colors <- brewer.pal(4, "Blues")
    ## minimum and maximum values are adjusted based on the actual prior values that are selected
    x_min <- min(41, input$alt_mu5, input$alt_mu6) - 3*sqrt(max(10, input$alt_var5, input$alt_var6))
    x_max <- max(41, input$alt_mu5, input$alt_mu6) + 3*sqrt(max(10, input$alt_var5, input$alt_var6))
    
    # Save the x-axis limits in a dataframe
    df <- data.frame(x = c(x_min, x_max))
    
    # Create an emply plot that has the x-axis in it
    p <- ggplot(df, aes(x=x)) +
      # First, add the prior distribution of the original prior. 
      # The first line creates an area (filled in with color), the second line creates a line graph
      stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10))) +
      # Repeat the above for each of the two alternative priors
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu5, sd = sqrt(input$alt_var5)), geom = "area", aes(fill = "Alternative 1"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu5, sd = sqrt(input$alt_var5))) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu6, sd = sqrt(input$alt_var6)), geom = "area", aes(fill = "Alternative 2"), alpha = .7) +
      stat_function(fun = dnorm, n = 1001, args = list(mean = input$alt_mu6, sd = sqrt(input$alt_var6))) +
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle("Comparison of Priors") +
      xlab(bquote(beta[intercept])) +
      theme_classic() +  
      theme(legend.position="bottom",
            axis.title.x = element_text(family="sans", size = 18),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, family="sans", size = 18),
            legend.text=element_text(size=14))
    
    # Print the plot
    p
  })
  
  ## Residual Variance: 
  ## Plot that shows the alternative priors that are selected through the 
  ## numeric inputs on the screen 
  output$priorPlot_resid <- renderPlot({
    #colors <- brewer.pal(4, "Blues")
    ## minimum and maximum values are adjusted based on the actual prior values that are selected
    x_min <- 0.000000000001
    x_max <- 30
    
    # Save the x-axis limits in a dataframe
    df <- data.frame(x = c(x_min, x_max))
    
    # Create an empty plot that has the x-axis in it
    p <- ggplot(df, aes(x=x)) +
      # First, add the prior distribution of the original prior. 
      # The first line creates an area (filled in with color), the second line creates a line graph
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = 0.5, scale = .5), geom = "area", aes(fill = "Original Prior"), alpha = .5) +
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = 0.5, scale = .5)) +
      # Repeat the above for each of the two alternative priors
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = input$alt_mu7, scale = input$alt_var7), geom = "area", aes(fill = "Alternative 1"), alpha = .5) +
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = input$alt_mu7, scale = input$alt_var7)) +
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = input$alt_mu8, scale = input$alt_var8), geom = "area", aes(fill = "Alternative 2"), alpha = .7) +
      stat_function(fun = dinvgamma, n = 1001, args = list(shape = input$alt_mu8, scale = input$alt_var8)) +
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle("Comparison of Priors") +
      xlab(bquote(epsilon^2)) +
      theme_classic() +  
      theme(legend.position="bottom",
            axis.title.x = element_text(family="sans", size = 18),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, family="sans", size = 18),
            legend.text=element_text(size=14))
    
    # Print the plot
    p
  })
  
  ## Both:
  ## Plot that shows the alternative priors that are selected through the numeric inputs on the screen 
   output$noboth <- renderText({
    input$runmodels_sex
    input$runmodels_trust
    input$runmodels_intercept
    input$runmodels_resid
    
    if (input$runmodels_sex != 0 | input$runmodels_trust != 0 | input$runmodels_intercept != 0 | input$runmodels_resid != 0) return()
    
    paste("Oops! Please take a moment to examine each parameter individually before looking at them all together.")
  })
  
  output$priorPlot_both <- renderPlot({
    input$runmodels_sex
    input$runmodels_trust
    input$runmodels_intercept
    input$runmodels_resid
    
    if(input$runmodels_sex == 0 | input$runmodels_trust == 0 | input$runmodels_intercept == 0 | input$runmodels_resid == 0) {
      return()
    } else {
      
      #colors <- brewer.pal(4, "Blues")
      choice1 <- as.numeric(input$param1)
      choice2 <- as.numeric(input$param2)
      choice3 <- as.numeric(input$param3)
      choice4 <- as.numeric(input$param4)
      
      ## Plot 3: Intercept
      if(choice3 == 1) {
        x_min <- 41 - 3*sqrt(10)
        x_max <- 41 + 3*sqrt(10)
        m_choice <- 41
        v_choice <- 10
        n_choice <- "Selected: Original Prior"
      } else if(choice3 == 2) {
        x_min <- min(41, input$alt_mu5) - 3*sqrt(max(10,input$alt_var5))
        x_max <- max(41, input$alt_mu5) + 3*sqrt(max(10,input$alt_var5))
        m_choice <- input$alt_mu5
        v_choice <- input$alt_var5
        n_choice <- "Selected: Alternative 1"
      } else if(choice3 == 3) {
        x_min <- min(41, input$alt_mu6) - 3*sqrt(max(10, input$alt_var6))
        x_max <- max(41, input$alt_mu6) + 3*sqrt(max(10, input$alt_var6))
        m_choice <- input$alt_mu6
        v_choice <- input$alt_var6
        n_choice <- "Selected: Alternative 2"
      }
      
      # Save the x-axis limits in a dataframe
      df <- data.frame(x = c(x_min, x_max))
      
      # Create an emply plot that has the x-axis in it
      pp3 <- ggplot(df, aes(x=x)) +
        # Repeat the above for each of the two alternative priors
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice)), geom = "area", aes(fill = n_choice), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice))) +
        # A3dd the prior distribution of the original prior. 
        # The first line creates an area (filled in with color), the second line creates a line graph
        stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10))) +
        scale_fill_brewer(palette = "Blues", name = "", direction = -1) +
        ggtitle("Comparison of Priors for the Intercept of Cynicism") +
        xlab(bquote(beta[intercept])) +
        theme_classic() +  
        theme(legend.position="bottom", 
              axis.title.x=element_text(family = "sans", size = 18),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 0.5, family="sans", size = 18),
              legend.text=element_text(size=14))
      
      ## Plot 1: Sex
      if(choice1 == 1) {
        x_min <- 0 - 3*sqrt(10)
        x_max <- 0 + 3*sqrt(10)
        m_choice <- 0
        v_choice <- 10
        n_choice <- "Selected: Original Prior"
      } else if(choice1 == 2) {
        x_min <- min(0, input$alt_mu3) - 3*sqrt(max(10,input$alt_var3))
        x_max <- max(0, input$alt_mu3) + 3*sqrt(max(10,input$alt_var3))
        m_choice <- input$alt_mu3
        v_choice <- input$alt_var3
        n_choice <- "Selected: Alternative 1"
      } else if(choice1 == 3) {
        x_min <- min(0, input$alt_mu4) - 3*sqrt(max(10, input$alt_var4))
        x_max <- max(0, input$alt_mu4) + 3*sqrt(max(10, input$alt_var4))
        m_choice <- input$alt_mu4
        v_choice <- input$alt_var4
        n_choice <- "Selected: Alternative 2"
      }
      
      # Save the x-axis limits in a dataframe
      df <- data.frame(x = c(x_min, x_max))
      
      # Create an emply plot that has the x-axis in it
      pp1 <- ggplot(df, aes(x=x)) +
        # Repeat the above for each of the two alternative priors
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice)), geom = "area", aes(fill = n_choice), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice))) +
        # Add the prior distribution of the original prior.
        # The first line creates an area (filled in with color), the second line creates a line graph
        stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10))) +
        scale_fill_brewer(palette = "Blues", name = "", direction = -1) +
        ggtitle("Comparison of Priors for Cynicism ~ Sex") +
        xlab(bquote(beta[sex])) +
        theme_classic() +  
        theme(legend.position="bottom", 
              axis.title.x=element_text(family = "sans", size = 18),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 0.5, family="sans", size = 18),
              legend.text=element_text(size=14))
      
      
      ## Plot 2: Trust
      if(choice2 == 1) {
        x_min <- 6 - 3*sqrt(1)
        x_max <- 6 + 3*sqrt(1)
        m_choice <- 6
        v_choice <- 1
        n_choice <- "Selected: Original Prior"
      } else if(choice2 == 2) {
        x_min <- min(6, input$alt_mu1) - 3*sqrt(max(1,input$alt_var1))
        x_max <- max(6, input$alt_mu1) + 3*sqrt(max(1,input$alt_var1))
        m_choice <- input$alt_mu1
        v_choice <- input$alt_var1
        n_choice <- "Selected: Alternative 1"
      } else if(choice2 == 3) {
        x_min <- min(6, input$alt_mu2) - 3*sqrt(max(1, input$alt_var2))
        x_max <- max(6, input$alt_mu2) + 3*sqrt(max(1, input$alt_var2))
        m_choice <- input$alt_mu2
        v_choice <- input$alt_var2
        n_choice <- "Selected: Alternative 2"
      }
      
      # Save the x-axis limits in a dataframe
      df <- data.frame(x = c(x_min, x_max))
      
      # Create an emply plot that has the x-axis in it
      pp2 <- ggplot(df, aes(x=x)) +
        # Repeat the above for each of the two alternative priors
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice)), geom = "area", aes(fill = n_choice), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = m_choice, sd = sqrt(v_choice))) +
        # A3dd the prior distribution of the original prior. 
        # The first line creates an area (filled in with color), the second line creates a line graph
        stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
        stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1))) +
        scale_fill_brewer(palette = "Blues", name = "", direction = -1) +
        ggtitle("Comparison of Priors for Cynicism ~ Lack of Trust") +
        xlab(bquote(beta[lackofchoice])) +
        theme_classic() +  
        theme(legend.position="bottom", 
              axis.title.x=element_text(family = "sans", size = 18),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 0.5, family="sans", size = 18),
              legend.text=element_text(size=14))
      
      
      
      ## Plot 4: Residual Variance
      if(choice4 == 1) {
        x_min <- 0
        x_max <- 30
        m_choice <- .5
        v_choice <- .5
        n_choice <- "Selected: Original Prior"
      } else if(choice4 == 2) {
        x_min <- 0
        x_max <- 30
        m_choice <- input$alt_mu7
        v_choice <- input$alt_var7
        n_choice <- "Selected: Alternative 1"
      } else if(choice4 == 3) {
        x_min <- 0
        x_max <- 30
        m_choice <- input$alt_mu8
        v_choice <- input$alt_var8
        n_choice <- "Selected: Alternative 2"
      }
      
      # Save the x-axis limits in a dataframe
      df <- data.frame(x = c(x_min, x_max))
      
      # Create an emply plot that has the x-axis in it
      pp4 <- ggplot(df, aes(x=x)) +
        # Repeat the above for each of the two alternative priors
        stat_function(fun = dinvgamma, n = 1001, args = list(shape = m_choice, scale = v_choice), geom = "area", aes(fill = n_choice), alpha = .7) +
        stat_function(fun = dinvgamma, n = 1001, args = list(shape = m_choice, scale = v_choice)) +
        # A3dd the prior distribution of the original prior. 
        # The first line creates an area (filled in with color), the second line creates a line graph
        stat_function(fun = dinvgamma, n = 1001, args = list(shape = .5, scale = .5), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
        stat_function(fun = dinvgamma, n = 1001, args = list(shape = .5, scale = .5)) +
        scale_fill_brewer(palette = "Blues", name = "", direction = -1) +
        ggtitle("Comparison of Priors for the Residual Variance of Cynicism") +
        xlab(bquote(epsilon^2)) +
        theme_classic() +  
        theme(legend.position="bottom", 
              axis.title.x=element_text(family = "sans", size = 18),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 0.5, family="sans", size = 18),
              legend.text=element_text(size=14))
      
      # Print the plot
      pblock <- plot_grid(pp3,pp1,
                          pp2,pp4,
                          nrow = 2, align = 'hv')
      pblock
    }
  })
  
  
  
  #############################################################################
  #### Run Models ####
  ## Trust: 
  ## Switch tabs to Convergence once button is clicked
  observeEvent(input$runmodels_trust, {
    updateTabsetPanel(session, "SensTrust",selected = "Convergence")
  })
  
  ## The event runs the alternative prior models once the "runmodels" button is clicked
  post_alts_trust <- eventReactive(input$runmodels_trust, {
    iterations <- iterations
    
    
    # First, we run each of the models, replacing the original prior values with user inputs
    withProgress(message = 'Running Models', 
                 detail = 'Lack of Trust: Alternative Prior 1', value = 0, {
    fit_a1 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                      y=reg_data$totcyn_T, 
                                      x=reg_data[,c(1,3)],
                                      mu0 = 41,
                                      s20 = 10,
                                      mu1 = 0,
                                      s21 = 10,
                                      mu2 = input$alt_mu1,
                                      s22 = input$alt_var1,
                                      ig1 = .5,
                                      ig2 = .5),
                   pars = c("alpha", "beta", "sigma2"),
                   chains = 2,
                   iter = iterations)
    
    incProgress(1/2, detail = 'Lack of Trust: Alternative prior 2')
    
    fit_a2 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                          y=reg_data$totcyn_T, 
                                          x=reg_data[,c(1,3)],
                                          mu0 = 41,
                                          s20 = 10,
                                          mu1 = 0,
                                          s21 = 10,
                                          mu2 = input$alt_mu2,
                                          s22 = input$alt_var2,
                                          ig1 = .5,
                                          ig2 = .5),
                       pars = c("alpha", "beta", "sigma2"),
                       chains = 2,
                       iter = iterations)
    
    
    incProgress(1/2, detail = 'Finishing up!')
    })
    
    
    # Then, we extract the posterior estimates (excludung the burnin phase)
    post_alt1 <- rstan::extract(fit_a1, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    post_alt2 <- rstan::extract(fit_a2, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    
    # Then, we extract the posterior estimates (INcludung the burnin phase for the traceplots)
    post_all_alt1 <- rstan::extract(fit_a1, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    post_all_alt2 <- rstan::extract(fit_a2, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    
    parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")
    
    dimnames(post_all_alt1)$parameters <- parnames
    dimnames(post_all_alt2)$parameters <- parnames
    
    # We transform the rstan objects into regular matrices for easier use in the future
    int_a1 <- matrix(unlist(post_alt1), nrow = nrow(post_alt1[[1]]), byrow = FALSE)
    colnames(int_a1) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    int_a2 <- matrix(unlist(post_alt2), nrow = nrow(post_alt2[[1]]), byrow = FALSE)
    colnames(int_a2) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    # Then we group the estimates together per parameter
    alphas <- data.frame(int_a2[,1], int_a1[,1], int_og[,1])
    b1 <- data.frame(int_a2[,2], int_a1[,2], int_og[,2])
    b2 <- data.frame(int_a2[,3], int_a1[,3], int_og[,3])
    s2 <- data.frame(int_a2[,4], int_a1[,4], int_og[,4])
    
    sum_alt1 <- data.frame("Parameter" = parnames, 
                         "Mean" = colMeans(int_a1),
                         "SD" = apply(int_a1, 2, sd),
                         "ll" = posterior_interval(int_a1)[,1],
                         "median" = apply(int_a1, 2, median),
                         "ul"  = posterior_interval(int_a1)[,2])
    
    sum_alt2 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a2),
                           "SD" = apply(int_a2, 2, sd),
                           "ll" = posterior_interval(int_a2)[,1],
                           "median" = apply(int_a2, 2, median),
                           "ul"  = posterior_interval(int_a2)[,2])
    
    #Extract Convergence Diagnostics for Trace plot
    rhats_1 <- summary(fit_a1)$summary[1:4,10]
    n_eff_1 <- summary(fit_a1)$summary[1:4,9]
    
    rhats_2 <- summary(fit_a2)$summary[1:4,10]
    n_eff_2 <- summary(fit_a2)$summary[1:4,9]
    
    # Set up plot labels
    plot_labels_1 <- paste0("R-hat: ", format(rhats_1, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_1, digits = 0, nsmall = 0))
    plot_labels_2 <- paste0("R-hat: ", format(rhats_2, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_2, digits = 0, nsmall = 0))
    
    rhats_1 <- data.frame("parameter" = parnames, "rhats" = plot_labels_1)
    rhats_1 <- bind_cols(rhats_1, "chain" = rep(1, nrow(rhats_1)))
    rhats_1$chain <- factor(rhats_1$chain)
    rhats_1$parameter <- factor(rhats_1$parameter)
    
    rhats_2 <- data.frame("parameter" = parnames, "rhats" = plot_labels_2)
    rhats_2 <- bind_cols(rhats_2, "chain" = rep(1, nrow(rhats_2)))
    rhats_2$chain <- factor(rhats_2$chain)
    rhats_2$parameter <- factor(rhats_2$parameter)
    
    # Finally, we combine all these dataframes in a list that is saved in post_alts() for further use
    post_alt <- list(post_alt1, post_alt2, alphas, b1, b2, s2, 
                     post_all_alt1, post_all_alt2,
                     sum_alt1, sum_alt2,
                     rhats_1, rhats_2)
    post_alt
  })
  
  ## Sex: 
  ## Switch tabs to Convergence once button is clicked
  
  observeEvent(input$runmodels_sex, {
    updateTabsetPanel(session, "SensSex",selected = "Convergence")
  })
  
  post_alts_sex <- eventReactive(input$runmodels_sex, {
    iterations <- iterations
    
    # First, we run each of the models, replacing the original prior values with user inputs
    withProgress(message = 'Running Models', 
                 detail = 'Sex: Alternative Prior 1', value = 0, {
                   fit_a3 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = 41,
                                                         s20 = 10,
                                                         mu1 = input$alt_mu3,
                                                         s21 = input$alt_var3,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Sex: Alternative prior 2')
                   
                   fit_a4 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = 41,
                                                         s20 = 10,
                                                         mu1 = input$alt_mu4,
                                                         s21 = input$alt_var4,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Finishing up!')
                 })
    
    
    # Then, we extract the posterior estimates (excludung the burnin phase)
    post_alt3 <- rstan::extract(fit_a3, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    post_alt4 <- rstan::extract(fit_a4, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    
    # Then, we extract the posterior estimates (INcludung the burnin phase for the traceplots)
    post_all_alt3 <- rstan::extract(fit_a3, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    post_all_alt4 <- rstan::extract(fit_a4, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    
    parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")
    
    dimnames(post_all_alt3)$parameters <- parnames
    dimnames(post_all_alt4)$parameters <- parnames
    
    # We transform the rstan objects into regular matrices for easier use in the future
    int_a3 <- matrix(unlist(post_alt3), nrow = nrow(post_alt3[[1]]), byrow = FALSE)
    colnames(int_a3) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    int_a4 <- matrix(unlist(post_alt4), nrow = nrow(post_alt4[[1]]), byrow = FALSE)
    colnames(int_a4) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    # Then we group the estimates together per parameter
    alphas <- data.frame(int_a4[,1], int_a3[,1], int_og[,1])
    b1 <- data.frame(int_a4[,2], int_a3[,2], int_og[,2])
    b2 <- data.frame(int_a4[,3], int_a3[,3], int_og[,3])
    s2 <- data.frame(int_a4[,4], int_a3[,4], int_og[,4])
    
    sum_alt3 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a3),
                           "SD" = apply(int_a3, 2, sd),
                           "ll" = posterior_interval(int_a3)[,1],
                           "median" = apply(int_a3, 2, median),
                           "ul"  = posterior_interval(int_a3)[,2])
    
    sum_alt4 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a4),
                           "SD" = apply(int_a4, 2, sd),
                           "ll" = posterior_interval(int_a4)[,1],
                           "median" = apply(int_a4, 2, median),
                           "ul"  = posterior_interval(int_a4)[,2])
    
    #Extract Convergence Diagnostics for Trace plot
    rhats_3 <- summary(fit_a3)$summary[1:4,10]
    n_eff_3 <- summary(fit_a3)$summary[1:4,9]
    
    rhats_4 <- summary(fit_a4)$summary[1:4,10]
    n_eff_4 <- summary(fit_a4)$summary[1:4,9]
    
    # Set up plot labels
    plot_labels_3 <- paste0("R-hat: ", format(rhats_3, digits = 2, nsmall = 2), 
                          "\n ESS: ", format(n_eff_3, digits = 0, nsmall = 0))
    plot_labels_4 <- paste0("R-hat: ", format(rhats_4, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_4, digits = 0, nsmall = 0))
    
    rhats_3 <- data.frame("parameter" = parnames, "rhats" = plot_labels_3)
    rhats_3 <- bind_cols(rhats_3, "chain" = rep(1, nrow(rhats_3)))
    rhats_3$chain <- factor(rhats_3$chain)
    rhats_3$parameter <- factor(rhats_3$parameter)
    
    rhats_4 <- data.frame("parameter" = parnames, "rhats" = plot_labels_4)
    rhats_4 <- bind_cols(rhats_4, "chain" = rep(1, nrow(rhats_4)))
    rhats_4$chain <- factor(rhats_4$chain)
    rhats_4$parameter <- factor(rhats_4$parameter)
    
    # Finally, we combine all these dataframes in a list that is saved in post_alts() for further use
    post_alt <- list(post_alt3, post_alt4, alphas, b1, b2, s2, 
                     post_all_alt3, post_all_alt4,
                     sum_alt3, sum_alt4,
                     rhats_3, rhats_4)
    post_alt
  })
  
  ## Intercept: 
  ## Switch tabs to Convergence once button is clicked
  
  observeEvent(input$runmodels_intercept, {
    updateTabsetPanel(session, "SensIntercept",selected = "Convergence")
  })
  
  post_alts_intercept <- eventReactive(input$runmodels_intercept, {
    iterations <- iterations
    
    # First, we run each of the models, replacing the original prior values with user inputs
    withProgress(message = 'Running Models', 
                 detail = 'Intercept: Alternative Prior 1', value = 0, {
                   fit_a5 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = input$alt_mu5,
                                                         s20 = input$alt_var5,
                                                         mu1 = 0,
                                                         s21 = 10,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Intercept: Alternative prior 2')
                   
                   fit_a6 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = input$alt_mu6,
                                                         s20 = input$alt_var6,
                                                         mu1 = 0,
                                                         s21 = 10,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Finishing up!')
                 })
    
    
    # Then, we extract the posterior estimates (excludung the burnin phase)
    post_alt5 <- rstan::extract(fit_a5, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    post_alt6 <- rstan::extract(fit_a6, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    
    # Then, we extract the posterior estimates (INcludung the burnin phase for the traceplots)
    post_all_alt5 <- rstan::extract(fit_a5, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    post_all_alt6 <- rstan::extract(fit_a6, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    
    parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")
    
    dimnames(post_all_alt5)$parameters <- parnames
    dimnames(post_all_alt6)$parameters <- parnames
    
    # We transform the rstan objects into regular matrices for easier use in the future
    int_a5 <- matrix(unlist(post_alt5), nrow = nrow(post_alt5[[1]]), byrow = FALSE)
    colnames(int_a5) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    int_a6 <- matrix(unlist(post_alt6), nrow = nrow(post_alt6[[1]]), byrow = FALSE)
    colnames(int_a6) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    # Then we group the estimates together per parameter
    alphas <- data.frame(int_a6[,1], int_a5[,1], int_og[,1])
    b1 <- data.frame(int_a6[,2], int_a5[,2], int_og[,2])
    b2 <- data.frame(int_a6[,3], int_a5[,3], int_og[,3])
    s2 <- data.frame(int_a6[,4], int_a5[,4], int_og[,4])
    
    sum_alt5 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a5),
                           "SD" = apply(int_a5, 2, sd),
                           "ll" = posterior_interval(int_a5)[,1],
                           "median" = apply(int_a5, 2, median),
                           "ul"  = posterior_interval(int_a5)[,2])
    
    sum_alt6 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a6),
                           "SD" = apply(int_a6, 2, sd),
                           "ll" = posterior_interval(int_a6)[,1],
                           "median" = apply(int_a6, 2, median),
                           "ul"  = posterior_interval(int_a6)[,2])
    
    #Extract Convergence Diagnostics for Trace plot
    rhats_5 <- summary(fit_a5)$summary[1:4,10]
    n_eff_5 <- summary(fit_a5)$summary[1:4,9]
    
    rhats_6 <- summary(fit_a6)$summary[1:4,10]
    n_eff_6 <- summary(fit_a6)$summary[1:4,9]
    
    # Set up plot labels
    plot_labels_5 <- paste0("R-hat: ", format(rhats_5, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_5, digits = 0, nsmall = 0))
    plot_labels_6 <- paste0("R-hat: ", format(rhats_6, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_6, digits = 0, nsmall = 0))
    
    rhats_5 <- data.frame("parameter" = parnames, "rhats" = plot_labels_5)
    rhats_5 <- bind_cols(rhats_5, "chain" = rep(1, nrow(rhats_5)))
    rhats_5$chain <- factor(rhats_5$chain)
    rhats_5$parameter <- factor(rhats_5$parameter)
    
    rhats_6 <- data.frame("parameter" = parnames, "rhats" = plot_labels_6)
    rhats_6 <- bind_cols(rhats_6, "chain" = rep(1, nrow(rhats_6)))
    rhats_6$chain <- factor(rhats_6$chain)
    rhats_6$parameter <- factor(rhats_6$parameter)
    
    # Finally, we combine all these dataframes in a list that is saved in post_alts() for further use
    post_alt <- list(post_alt5, post_alt6, alphas, b1, b2, s2, 
                     post_all_alt5, post_all_alt6,
                     sum_alt5, sum_alt6,
                     rhats_5, rhats_6)
    post_alt
  })
  
  ## Residual: 
  ## Switch tabs to Convergence once button is clicked
  
  observeEvent(input$runmodels_resid, {
    updateTabsetPanel(session, "SensResid",selected = "Convergence")
  })
  
  post_alts_resid <- eventReactive(input$runmodels_resid, {
    iterations <- iterations
    
    # First, we run each of the models, replacing the original prior values with user inputs
    withProgress(message = 'Running Models', 
                 detail = 'Residual Variance: Alternative Prior 1', value = 0, {
                   fit_a7 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = 41,
                                                         s20 = 10,
                                                         mu1 = 0,
                                                         s21 = 10,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Residual Variance: Alternative prior 2')
                   
                   fit_a8 <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = 41,
                                                         s20 = 10,
                                                         mu1 = 0,
                                                         s21 = 10,
                                                         mu2 = 6,
                                                         s22 = 1,
                                                         ig1 = .5,
                                                         ig2 = .5),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Finishing up!')
                 })
    
    
    # Then, we extract the posterior estimates (excludung the burnin phase)
    post_alt7 <- rstan::extract(fit_a7, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    post_alt8 <- rstan::extract(fit_a8, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    
    # Then, we extract the posterior estimates (INcludung the burnin phase for the traceplots)
    post_all_alt7 <- rstan::extract(fit_a7, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    post_all_alt8 <- rstan::extract(fit_a8, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    
    parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")
    
    dimnames(post_all_alt7)$parameters <- parnames
    dimnames(post_all_alt8)$parameters <- parnames
    
    # We transform the rstan objects into regular matrices for easier use in the future
    int_a7 <- matrix(unlist(post_alt7), nrow = nrow(post_alt7[[1]]), byrow = FALSE)
    colnames(int_a7) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    int_a8 <- matrix(unlist(post_alt8), nrow = nrow(post_alt8[[1]]), byrow = FALSE)
    colnames(int_a8) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    # Then we group the estimates together per parameter
    alphas <- data.frame(int_a8[,1], int_a7[,1], int_og[,1])
    b1 <- data.frame(int_a8[,2], int_a7[,2], int_og[,2])
    b2 <- data.frame(int_a8[,3], int_a7[,3], int_og[,3])
    s2 <- data.frame(int_a8[,4], int_a7[,4], int_og[,4])
    
    sum_alt7 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a7),
                           "SD" = apply(int_a7, 2, sd),
                           "ll" = posterior_interval(int_a7)[,1],
                           "median" = apply(int_a7, 2, median),
                           "ul"  = posterior_interval(int_a7)[,2])
    
    sum_alt8 <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_a8),
                           "SD" = apply(int_a8, 2, sd),
                           "ll" = posterior_interval(int_a8)[,1],
                           "median" = apply(int_a8, 2, median),
                           "ul"  = posterior_interval(int_a8)[,2])
    
    #Extract Convergence Diagnostics for Trace plot
    rhats_7 <- summary(fit_a7)$summary[1:4,10]
    n_eff_7 <- summary(fit_a7)$summary[1:4,9]
    
    rhats_8 <- summary(fit_a8)$summary[1:4,10]
    n_eff_8 <- summary(fit_a8)$summary[1:4,9]
    
    # Set up plot labels
    plot_labels_7 <- paste0("R-hat: ", format(rhats_7, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_7, digits = 0, nsmall = 0))
    plot_labels_8 <- paste0("R-hat: ", format(rhats_8, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff_8, digits = 0, nsmall = 0))
    
    rhats_7 <- data.frame("parameter" = parnames, "rhats" = plot_labels_7)
    rhats_7 <- bind_cols(rhats_7, "chain" = rep(1, nrow(rhats_7)))
    rhats_7$chain <- factor(rhats_7$chain)
    rhats_7$parameter <- factor(rhats_7$parameter)
    
    rhats_8 <- data.frame("parameter" = parnames, "rhats" = plot_labels_8)
    rhats_8 <- bind_cols(rhats_8, "chain" = rep(1, nrow(rhats_8)))
    rhats_8$chain <- factor(rhats_8$chain)
    rhats_8$parameter <- factor(rhats_8$parameter)
    
    # Finally, we combine all these dataframes in a list that is saved in post_alts() for further use
    post_alt <- list(post_alt7, post_alt8, alphas, b1, b2, s2, 
                     post_all_alt7, post_all_alt8,
                     sum_alt7, sum_alt8,
                     rhats_7, rhats_8)
    post_alt
  })
  
  ## Combination: 
  ## Switch tabs to Convergence once button is clicked
  observeEvent(input$runmodels_both, {
    updateTabsetPanel(session, "SensBoth", selected = "Convergence")
  })
  
  post_alts_both <- eventReactive(input$runmodels_both, {
    iterations <- iterations
    
    # First, we run each of the models, replacing the original prior values with user inputs
    withProgress(message = 'Running Model', 
                 detail = 'Both Parameters: Selected Priors', value = 0, {
                   
                   choice1 <- as.numeric(input$param1)
                   choice2 <- as.numeric(input$param2)
                   choice3 <- as.numeric(input$param3)
                   choice4 <- as.numeric(input$param4)
                   
                   if(choice1 == 1) {
                     mu_sex <- 0
                     s_sex <- 10
                   } else if(choice1 == 2) {
                     mu_sex <- input$alt_mu3
                     s_sex <- input$alt_var3
                   } else if(choice1 == 3) {
                     mu_sex <- input$alt_mu4
                     s_sex <- input$alt_var4
                   }
                   
                   if(choice2 == 1) {
                     mu_trust <- 6
                     s_trust <- 1
                   } else if(choice2 == 2) {
                     mu_trust <- input$alt_mu1
                     s_trust <- input$alt_var1
                   } else if(choice2 == 3) {
                     mu_trust <- input$alt_mu2
                     s_trust <- input$alt_var2
                   }
                   
                   if(choice3 == 1) {
                     mu_int <- 41
                     s_int <- 10
                   } else if(choice3 == 2) {
                     mu_int <- input$alt_mu5
                     s_int <- input$alt_var5
                   } else if(choice3 == 3) {
                     mu_int <- input$alt_mu6
                     s_int <- input$alt_var6
                   }
                   
                   if(choice4 == 1) {
                     mu_res <- .5
                     s_res <- .5
                   } else if(choice4 == 2) {
                     mu_res <- input$alt_mu7
                     s_res <- input$alt_var7
                   } else if(choice4 == 3) {
                     mu_res <- input$alt_mu8
                     s_res <- input$alt_var8
                   }
                   
                   fit_both <- sampling(model, data = list(N=nrow(reg_data), K=2, 
                                                         y=reg_data$totcyn_T, 
                                                         x=reg_data[,c(1,3)],
                                                         mu0 = mu_int,
                                                         s20 = s_int,
                                                         mu1 = mu_sex,
                                                         s21 = s_sex,
                                                         mu2 = mu_trust,
                                                         s22 = s_trust,
                                                         ig1 = mu_res,
                                                         ig2 = s_res),
                                      pars = c("alpha", "beta", "sigma2"),
                                      chains = 2,
                                      iter = iterations)
                   
                   incProgress(1/2, detail = 'Almost done!')
                   
                   incProgress(1/2, detail = 'Finishing up!')
                 })
    
    
    # Then, we extract the posterior estimates (excludung the burnin phase)
    post_altboth <- rstan::extract(fit_both, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)
    
    # Then, we extract the posterior estimates (INcludung the burnin phase for the traceplots)
    post_all_altboth <- rstan::extract(fit_both, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)
    
    parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")
    
    dimnames(post_all_altboth)$parameters <- parnames
    
    # We transform the rstan objects into regular matrices for easier use in the future
    int_both <- matrix(unlist(post_altboth), nrow = nrow(post_altboth[[1]]), byrow = FALSE)
    colnames(int_both) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")
    
    # Then we group the estimates together per parameter
    alphas <- data.frame(int_both[,1], int_og[,1])
    b1 <- data.frame(int_both[,2], int_og[,2])
    b2 <- data.frame(int_both[,3], int_og[,3])
    s2 <- data.frame(int_both[,4], int_og[,4])
    
    sum_altboth <- data.frame("Parameter" = parnames, 
                           "Mean" = colMeans(int_both),
                           "SD" = apply(int_both, 2, sd),
                           "ll" = posterior_interval(int_both)[,1],
                           "median" = apply(int_both, 2, median),
                           "ul"  = posterior_interval(int_both)[,2])
    
    #Extract Convergence Diagnostics for Trace plot
    rhats <- summary(fit_both)$summary[1:4,10]
    n_eff <- summary(fit_both)$summary[1:4,9]
    
    # Set up plot labels
    plot_labels <- paste0("R-hat: ", format(rhats, digits = 2, nsmall = 2), 
                            "\n ESS: ", format(n_eff, digits = 0, nsmall = 0))
    
    rhats <- data.frame("parameter" = parnames, "rhats" = plot_labels)
    rhats <- bind_cols(rhats, "chain" = rep(1, nrow(rhats)))
    rhats$chain <- factor(rhats$chain)
    rhats$parameter <- factor(rhats$parameter)
    
    # Finally, we combine all these dataframes in a list that is saved in post_alt() for further use
    post_alt <- list(post_altboth, alphas, b1, b2, s2, 
                     post_all_altboth,
                     sum_altboth, rhats)
    post_alt
  })
  
  #############################################################################
  #### Trace plots for alternative priors:####
  ## Sex:
  output$traceplotSex1 <- renderPlot({
    input$runmodels_sex
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_sex == 0) {
      return()
    } else {
      post_all_alt <- post_alts_sex()
      
      rhats <- post_all_alt[[11]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[7]][,1,1], post_all_alt[[7]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[7]][,1,2], post_all_alt[[7]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[7]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 1 for Sex as a predictor of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  output$traceplotSex2 <- renderPlot({
    input$runmodels_sex
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_sex == 0) {
      return()
    } else {
      post_all_alt <- post_alts_sex()
      
      rhats <- post_all_alt[[12]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[8]][,1,1], post_all_alt[[8]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[8]][,1,2], post_all_alt[[8]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[8]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 2 for Sex as a predictor of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  ## Trust:
  output$traceplotTrust1 <- renderPlot({
    input$runmodels_trust
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_trust == 0) {
      return()
    } else {
      post_all_alt <- post_alts_trust()
      rhats <- post_all_alt[[11]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[7]][,1,1], post_all_alt[[7]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[7]][,1,2], post_all_alt[[7]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[7]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 1 for Lack of Trust as a predictor of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  output$traceplotTrust2 <- renderPlot({
    input$runmodels_trust
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_trust == 0) {
      return()
    } else {
      post_all_alt <- post_alts_trust()
      rhats <- post_all_alt[[12]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[8]][,1,1], post_all_alt[[8]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[8]][,1,2], post_all_alt[[8]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[8]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 2 for Lack of Trust as a predictor of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  ## Intercept:
  output$traceplotIntercept1 <- renderPlot({
    input$runmodels_intercept
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_intercept == 0) {
      return()
    } else {
      post_all_alt <- post_alts_intercept()
      rhats <- post_all_alt[[11]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[7]][,1,1], post_all_alt[[7]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[7]][,1,2], post_all_alt[[7]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[7]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 1 for the Intercept of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  output$traceplotIntercept2 <- renderPlot({
    input$runmodels_intercept
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_intercept == 0) {
      return()
    } else {
      post_all_alt <- post_alts_intercept()
      rhats <- post_all_alt[[12]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[8]][,1,1], post_all_alt[[8]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[8]][,1,2], post_all_alt[[8]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[8]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 2 for the Intercept of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  ## Residual Variance:
  output$traceplotResid1 <- renderPlot({
    input$runmodels_resid
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_resid == 0) {
      return()
    } else {
      post_all_alt <- post_alts_resid()
      rhats <- post_all_alt[[11]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[7]][,1,1], post_all_alt[[7]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[7]][,1,2], post_all_alt[[7]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[7]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 1 for the Residual Variance of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  output$traceplotResid2 <- renderPlot({
    input$runmodels_resid
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_resid == 0) {
      return()
    } else {
      post_all_alt <- post_alts_intercept()
      rhats <- post_all_alt[[12]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[8]][,1,1], post_all_alt[[8]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[8]][,1,2], post_all_alt[[8]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[8]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Alternative Prior 2 for the Residual Variance of Cynicism",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  ## Both Param:
  
  ## Static plot showing traceplot of original analysis
  ## post_all_og is created in global.R
  output$ogtracePlot1 <- renderPlot({
    iterations <-  iterations
    warm_up <- iterations/2
    
    mcmc_trace(post_all_og, n_warmup = warm_up,
               facet_args = list(nrow = 2, labeller = label_parsed)) +
      labs(title = "Trace Plots with Original Priors",
           caption = "*ESS: Effective Sample Size") +
      geom_blank(data = blank_data, aes(x = x, y=y))+
      geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
      theme_cowplot() +
      theme(text = element_text(family = "sans"),
            plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
            strip.text.x = element_text(size = 18),
            strip.background = element_rect(colour=NA, fill=NA))
  })
  
  ## Traceplots of selection of priors made by users
  output$traceplotBoth2 <- renderPlot({
    input$runmodels_both
    
    iterations <-  iterations
    warm_up <- iterations/2
    
    if(input$runmodels_both == 0) {
      return()
    } else {
      post_all_alt <- post_alts_both()
      rhats <- post_all_alt[[8]]
      
      # Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
      # create white space for convergence diagnostics.
      blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
      blank_data$x <- 0
      blank_data$y <- c(0, 
                        max(post_all_alt[[6]][,1,1], post_all_alt[[6]][,2,1])*1.3, 
                        0, 
                        max(post_all_alt[[6]][,1,2], post_all_alt[[6]][,2,2])*2.2, 
                        0, NA, 0, NA)
      
      mcmc_trace(post_all_alt[[6]], n_warmup = warm_up,
                 facet_args = list(nrow = 2, labeller = label_parsed)) +
        labs(title = "Trace Plots with Selected Combination of Priors",
             caption = "*ESS: Effective Sample Size") +
        geom_blank(data = blank_data, aes(x = x, y=y))+
        geom_text(data = rhats, aes(x = Inf, y = Inf, label = rhats), hjust = 1, vjust = 1, show.legend = FALSE) +
        theme_cowplot() +
        theme(text = element_text(family = "sans"),
              plot.title = element_text(hjust = .5, family="sans", size = 18, face = "plain"),
              strip.text.x = element_text(size = 18, family = "sans"),
              strip.background = element_rect(colour=NA, fill=NA))
    }
  })
  
  #############################################################################
  #### Posterior Plots ####
  ## Trust:
  output$posteriorPlot10 <- renderPlot({
    input$runmodels_trust
    
    if(input$runmodels_trust == 0) {
      return()
    } else {
      post_alt <- post_alts_trust()
      
      post_plot <- data.frame(post_og, post_alt[[1]], post_alt[[2]])
      
      p1 <- post_plot_fun(post_plot, "alpha", senstype = 1)
      p2 <- post_plot_fun(post_plot, "beta.1", senstype = 1)
      p3 <- post_plot_fun(post_plot, "beta.2", senstype = 1)
      p4 <- post_plot_fun(post_plot, "sigma2", senstype = 1)
      
      pblock <- plot_grid(p1 + theme(legend.position= "none"), 
                          p2 + theme(legend.position= "none"), 
                          p3 + theme(legend.position= "none"), 
                          p4 + theme(legend.position= "none"),
                          nrow = 2, align = 'hv')
      
      legend_b <- get_legend(p1 + theme(legend.position="bottom"))
    
      postplots <- plot_grid(pblock, legend_b, ncol = 1, rel_heights = c(1, .2))
      
      postplots
      
    }
  })
  
  ## Sex:
  
  output$posteriorPlot20 <- renderPlot({
    input$runmodels_sex
    
    if(input$runmodels_sex == 0) {
      return()
    } else {
      post_alt <- post_alts_sex()
      
      post_plot <- data.frame(post_og, post_alt[[1]], post_alt[[2]])
      
      p1 <- post_plot_fun(post_plot, "alpha", senstype = 1)
      p2 <- post_plot_fun(post_plot, "beta.1", senstype = 1)
      p3 <- post_plot_fun(post_plot, "beta.2", senstype = 1)
      p4 <- post_plot_fun(post_plot, "sigma2", senstype = 1)
      
      pblock <- plot_grid(p1 + theme(legend.position= "none"), 
                          p2 + theme(legend.position= "none"), 
                          p3 + theme(legend.position= "none"), 
                          p4 + theme(legend.position= "none"),
                          nrow = 2, align = 'hv')
      
      legend_b <- get_legend(p1 + theme(legend.position="bottom"))
      
      postplots <- plot_grid(pblock, legend_b, ncol = 1, rel_heights = c(1, .2))
      
      postplots
      
    }
  })
  
  ## Intercept:
  
  output$posteriorPlot30 <- renderPlot({
    input$runmodels_intercept
    
    if(input$runmodels_intercept == 0) {
      return()
    } else {
      post_alt <- post_alts_intercept()
      
      post_plot <- data.frame(post_og, post_alt[[1]], post_alt[[2]])
      
      p1 <- post_plot_fun(post_plot, "alpha", senstype = 1)
      p2 <- post_plot_fun(post_plot, "beta.1", senstype = 1)
      p3 <- post_plot_fun(post_plot, "beta.2", senstype = 1)
      p4 <- post_plot_fun(post_plot, "sigma2", senstype = 1)
      
      pblock <- plot_grid(p1 + theme(legend.position= "none"), 
                          p2 + theme(legend.position= "none"), 
                          p3 + theme(legend.position= "none"), 
                          p4 + theme(legend.position= "none"),
                          nrow = 2, align = 'hv')
      
      legend_b <- get_legend(p1 + theme(legend.position="bottom"))
      
      postplots <- plot_grid(pblock, legend_b, ncol = 1, rel_heights = c(1, .2))
      
      postplots
      
    }
  })
  
  ## Residual Variance:
  
  output$posteriorPlot40 <- renderPlot({
    input$runmodels_resid
    
    if(input$runmodels_resid == 0) {
      return()
    } else {
      post_alt <- post_alts_resid()
      
      post_plot <- data.frame(post_og, post_alt[[1]], post_alt[[2]])
      
      p1 <- post_plot_fun(post_plot, "alpha", senstype = 1)
      p2 <- post_plot_fun(post_plot, "beta.1", senstype = 1)
      p3 <- post_plot_fun(post_plot, "beta.2", senstype = 1)
      p4 <- post_plot_fun(post_plot, "sigma2", senstype = 1)
      
      pblock <- plot_grid(p1 + theme(legend.position= "none"), 
                          p2 + theme(legend.position= "none"), 
                          p3 + theme(legend.position= "none"), 
                          p4 + theme(legend.position= "none"),
                          nrow = 2, align = 'hv')
      
      legend_b <- get_legend(p1 + theme(legend.position="bottom"))
      
      postplots <- plot_grid(pblock, legend_b, ncol = 1, rel_heights = c(1, .2))
      
      postplots
      
    }
  })
  
  ## Both Params:
  
  output$posteriorPlot12 <- renderPlot({
    input$runmodels_both
    
    if(input$runmodels_both == 0) {
      return()
    } else {
      post_alt <- post_alts_both()
      
      post_plot <- data.frame(post_og, post_alt[[1]])
      
      p1 <- post_plot_fun(post_plot, "alpha", senstype = 2)
      p2 <- post_plot_fun(post_plot, "beta.1", senstype = 2)
      p3 <- post_plot_fun(post_plot, "beta.2", senstype = 2)
      p4 <- post_plot_fun(post_plot, "sigma2", senstype = 2)
      
      pblock <- plot_grid(p1 + theme(legend.position= "none"), 
                          p2 + theme(legend.position= "none"), 
                          p3 + theme(legend.position= "none"), 
                          p4 + theme(legend.position= "none"),
                          nrow = 2, align = 'hv')
      
      legend_b <- get_legend(p1 + theme(legend.position="bottom"))
      
      postplots <- plot_grid(pblock, legend_b, ncol = 1, rel_heights = c(1, .2))
      
      postplots
      
    }
  })
  
  #############################################################################
  #### Posterior Estimates: #### 
  ## Trust:
  
  output$trustalt1estTable <- DT::renderDataTable({
    input$runmodels_trust
    
    if(input$runmodels_trust == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt1 <- post_alts_trust()
      sum_alt1 <- sum_alt1[[9]]
      
      sum_alt1 <- cbind(sum_alt1, "mean_og" = sum_og[,2], "bias" = (sum_alt1[,2] - sum_og[,2])/sum_og[,2]*100)
      
      sum_alt1[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt1,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 1: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  output$trustalt2estTable <- DT::renderDataTable({
    input$runmodels_trust
    
    if(input$runmodels_trust == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt2 <- post_alts_trust()
      sum_alt2 <- sum_alt2[[10]]
      
      sum_alt2 <- cbind(sum_alt2, "mean_og" = sum_og[,2], "bias" = (sum_alt2[,2] - sum_og[,2])/sum_og[,2]*100)
      
      sum_alt2[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt2,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 2: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  ## Sex:
  
  output$sexalt1estTable <- DT::renderDataTable({
    input$runmodels_sex
    
    if(input$runmodels_sex == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt3 <- post_alts_sex()
      sum_alt3 <- sum_alt3[[9]]
      
      sum_alt3 <- cbind(sum_alt3, "mean_og" = sum_og[,2], "bias" = (sum_alt3[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt3[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt3,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 1: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  output$sexalt2estTable <- DT::renderDataTable({
    input$runmodels_sex
    
    if(input$runmodels_sex == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt4 <- post_alts_sex()
      sum_alt4 <- sum_alt4[[10]]
      
      sum_alt4 <- cbind(sum_alt4, "mean_og" = sum_og[,2], "bias" = (sum_alt4[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt4[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt4,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 2: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  ## Intercept:
  
  output$interceptalt1estTable <- DT::renderDataTable({
    input$runmodels_intercept
    
    if(input$runmodels_intercept == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt3 <- post_alts_intercept()
      sum_alt3 <- sum_alt3[[9]]
      
      sum_alt3 <- cbind(sum_alt3, "mean_og" = sum_og[,2], "bias" = (sum_alt3[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt3[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt3,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 1: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  output$interceptalt2estTable <- DT::renderDataTable({
    input$runmodels_intercept
    
    if(input$runmodels_intercept == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt4 <- post_alts_intercept()
      sum_alt4 <- sum_alt4[[10]]
      
      sum_alt4 <- cbind(sum_alt4, "mean_og" = sum_og[,2], "bias" = (sum_alt4[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt4[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt4,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 2: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  ## Residual Variance:
  
  output$residalt1estTable <- DT::renderDataTable({
    input$runmodels_resid
    
    if(input$runmodels_resid == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt3 <- post_alts_resid()
      sum_alt3 <- sum_alt3[[9]]
      
      sum_alt3 <- cbind(sum_alt3, "mean_og" = sum_og[,2], "bias" = (sum_alt3[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt3[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt3,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 1: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  output$residalt2estTable <- DT::renderDataTable({
    input$runmodels_resid
    
    if(input$runmodels_resid == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_alt4 <- post_alts_resid()
      sum_alt4 <- sum_alt4[[10]]
      
      sum_alt4 <- cbind(sum_alt4, "mean_og" = sum_og[,2], "bias" = (sum_alt4[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_alt4[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_alt4,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Alternative Prior 2: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
  
  ## Both:
  
  output$bothalt1estTable <- DT::renderDataTable({
    input$runmodels_both
    
    if(input$runmodels_both == 0) {
      return()
    } else {
      sum_og <- sum_og
      sum_altboth <- post_alts_both()
      sum_altboth <- sum_altboth[[7]]
      
      sum_altboth <- cbind(sum_altboth, "mean_og" = sum_og[,2], "bias" = (sum_altboth[,2] - sum_og[,2])/sum_og[,2]*100)
      sum_altboth[,1] <- c("b<sub>intercept</sub>", "b<sub>sex</sub>", "b<sub>lackoftrust</sub>", "residual variance")
      
      table_note <- c(
        "function(settings){",
        "  var datatable = settings.oInstance.api();",
        "  var table = datatable.table().node();",
        "  var caption = 'Note: Mean = mean of posterior distribution; SD = standard deviation of posterior distribution; 5%-50%-95% = 90% highest posterior density interval (5% and 95%) and median of the posterior distribution (50%); Original Mean = posterior mean of analysis with original priors; Percentage Deviation = (Mean - Original Mean)/Original Mean * 100.'",
        "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
        "}"
      )
      
      DT::datatable(sum_altboth,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(dom = 't',
                                   drawCallback = JS(table_note)),
                    colnames = c('Parameter' = 1,
                                 'Mean' = 2,
                                 'SD' = 3,
                                 '5%' = 4,
                                 '50%' = 5,
                                 '95%' = 6,
                                 'Original Mean' = 7,
                                 'Percentage Deviation' = 8),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top;text-align: center;color:#000;font-size:14pt',
                      'Selected Priors: Posterior Estimates')) %>% formatRound(2:8, digits=3)
    }
  })
})


