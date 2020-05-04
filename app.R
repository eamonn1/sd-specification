#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()) 
set.seed(333) # reproducible
library(directlabels)
library(shiny) 
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(DT)
library(shinyalert)
library(Hmisc)
library(reshape)
library(rms)
library(ormPlot)
library(ordinal)
library(ggplot2)
library(tidyverse)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
options(max.print=1000000)    
 
fig.width7 <- 670
 
fig.height7 <- 500
 

## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=0)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p5 <- function(x) {formatC(x, format="f", digits=5)}
p6 <- function(x) {formatC(x, format="f", digits=6)}
p8 <- function(x) {formatC(x, format="f", digits=8)}
logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x) + 1)
inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
options(width=200)


# key function 
sd.spec <- function (input.sd, alph, n) {
  sd =       input.sd
  sign =     alph/1;    # note this is one sided, interested in upper limit only
  df <-      n-1
  crv_q_u  = qchisq(1-sign, df)     
  ci_cv_u =  sd*sqrt(crv_q_u /df)  
  return(ci_cv_u)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                  color = c( "#2171B5", "#F7FBFF"), 
                  gradient = "linear",
                  direction = "bottom"
                ),
                
                h2("Inference about a Population Variance"), 
                
                h4("  "), 
                
                h3("  "), 
                
                
                sidebarLayout(
                  
                  sidebarPanel( width=3 ,
                                
                                tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                
                                
                                actionButton(inputId='ab1', label="R Shiny ",   icon = icon("th"),   
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/sd-specification/master/app.R', '_blank')"), 
                                actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/sd-specification/master/sd%20specification.R', '_blank')"),  
                                actionButton("resample", "Simulate a new sample"),
                                br(),  
                                tags$style(".well {background-color:#b6aebd ;}"), 
                                
                                h4("Tab 1 presents the chi-squared sampling distribution and how to calculate a specification (false invalid risk) for a standard deviation (or variance).
                                   Tab 2 presents an example calcualtion based on the user inputs. Tab 3 allows a user to upload their own data and the final tab shows the theory."),
                                
                                h4("Instructions: The first input below is number of Monte Carlo simulations. 
                                The second is the true data generating population SD. The third input is the 
                                   number of replicates to be generated. The last input is the alpha level, 
                                   the default set at a one-sided 3 in a million level."),
                                div(
                                  
                                  tags$head(
                                    tags$style(HTML('#ab1{background-color:orange}'))
                                  ),
                                  
                                  tags$head(
                                    tags$style(HTML('#resample{background-color:orange}'))
                                  ),
                                  
                                  textInput('sims', 
                                            div(h5(tags$span(style="color:blue", "Number of Monte Carlo simulations"))), "1e4"),
                                  
                                  tags$hr(),
                                  textInput('popsd', 
                                            div(h5(tags$span(style="color:blue", "Population standard deviation"))), "3.7"),
                                  tags$hr(),
                                  textInput('reps', 
                                            div(h5(tags$span(style="color:blue", "Number of replicates to evaluate"))), "5"),
                                  tags$hr(), 
                                  textInput('alpha', 
                                            div(h5(tags$span(style="color:blue", "Alpha level"))), "0.000003"),
                         
                                
                                
                                tags$hr(),
                                div(h4("References:")),  
                                tags$a(href = "https://www.amazon.com/Statistical-Evaluation-Measurement-Errors-Reliability/dp/0340760702", tags$span(style="color:blue", "[1] Graham Dunn (2nd edition, Section 2.6.1, page 35)"),),   
                                div(p(" ")),
                                tags$a(href = "https://www.wiley.com/en-us/Biostatistics%3A+A+Methodology+For+the+Health+Sciences%2C+2nd+Edition-p-9780471031857",  tags$span(style="color:blue", "[2] Biostatisics A Methodology for the Health Sciences, 2nd edition, page 96"),),   
                                div(p(" ")),
                                tags$hr(),
                                
                                
                                
                                
                                )
                                
                                
                  ),
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                  mainPanel(width=9,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            navbarPage(       
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                              tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")),
                              
                              
                              tabPanel("1 Distribution of population variance", value=7, 
          
                                       h4(htmlOutput("textWithNumber",) ),
                                       h4(paste("Plugging in alpha, the population variance and the number of replicates (n) into equation [5] we calculate the specification.")), 


                                      withMathJax(
  
                                      helpText(
                                           tags$span(style="color:black",
              ' $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 5 \\right]    \\!$$  

              
                             
                            
              '))),

              h4(paste("Plugging in the population variance the number of replicates (n) and the simulated data into equation [4] we create the plot below left.")), 


                  withMathJax(
                      helpText(
                               tags$span(style="color:black",
            ' 
               $$\\frac{  {\\it{s}^2} {(n-1)} } {\\sigma^2} \\sim {\\chi^2}_{(n - 1)} \\qquad \\qquad  \\qquad \\qquad  \\qquad \\left[ 4 \\right]  \\!$$ 
                            
              '))),



 
                                      h4(htmlOutput("textWithNumber2",) ),

 

                                      fluidRow(
                                        column(width = 6, offset = 0, style='padding:1px;',
                                               
                                               div(plotOutput("his2",  width=fig.width7, height=fig.height7))
                                        ) ,
                                        
                                        
                                        fluidRow(
                                          column(width = 5, offset = 0, style='padding:1px;',
                                                 
                                                 div(plotOutput("his",  width=fig.width7, height=fig.height7)),
                                          )))



                              ) ,
                              
                              tabPanel("2 A single simulation ", value=3, 
                                         
                                       
                                       
                                       fluidRow(
                                         column(width = 12, offset = 0, style='padding:1px;',
                                                
                                                h4("(1) A single random sample of replicates (from the the population standard deviation, the data generating mechanism)"), 
                                                div( verbatimTextOutput("sample")),
                                                h4("(2) The standard deviation of the replicates in (1)"), 
                                                div( verbatimTextOutput("sd1")),
                                                
                                                
   # HTML(paste0("", tags$span(style="color:blue",  "(3) Upper one-sided specification for the standard deviation of the replicates (see alpha level); based on the population standard deviation and number of replicates, not dependent on actual replicate values in (1)" ))) ,
                                                
                                               h4("(3) Upper one-sided specification for the standard deviation of the replicates (see alpha level); based on the population standard deviation and number of replicates, not dependent on actual replicate values in (1)"), 
     div( verbatimTextOutput("datx")),
                                                
                                                
                                                
                                                h4("(4) The variance of the replicates in (1)"), 
                                                div( verbatimTextOutput("var1")),
   h4("(5) Upper one-sided specification for variance of the replicates (see alpha level); based on the population standard deviation and number of replicates, not dependent on actual replicate values in (1)" ) ,
   
                                             #  tags$span(style="color:blue",  "(5) Upper one-sided specification for variance of the replicates (see alpha level); based on the population standard deviation and number of replicates, not dependent on actual replicate values in (1)" ) ,
                                                 div( verbatimTextOutput("datxy")),
                                                
                                                h4("(6) Confidence interval for the standard deviation using the replicates in (1) (two-sided based on alpha level)"), 
                                                div( verbatimTextOutput("conf2")),
                                                
                                                h4("(7) Confidence interval for the variance using the replicates in (1) (two-sided based on alpha level)"), 
                                                div( verbatimTextOutput("conf")),
                                               
                                         ) ,
                                       
                                       
                                          
                                 
                                     
                                       fluidRow(
                                         column(width = 7, offset = 0, style='padding:1px;',
                                               # h4(paste("Figure 3. xxxxxxxxxxxxxxxxxxxxxxx.")), 
                                           
                                         )),
                                       
                                       
                              )),
               
   
   ######
   
   tabPanel("3 Upload your own data for analysis", fluid = TRUE,
            
            h4(("Upload your own data for analysis, the two radio button options are to help load.
                                  
                                  ")) ,
            
            h4(("We have an example data set to get things started (download the file and click 'Browse...' to locate and upload for analysis). 
            This is SIDS data, 11 cases of baby birth weights in gramms, the data can be found on page 97 of reference [2]. 
            For this data enter an alpha of 0.05 for 95% 
                CIs and 800 for the population SD (as stated in the reference). There is no need to enter the number of replicates, 
                this is automatically determined.
                We output the data, SD, one-sided SD specification and confidence intervals for SD and variance:")) ,
             
            
            div(p(" ")),
            tags$a(href = "https://raw.githubusercontent.com/eamonn2014/sd-specification/master/SIDS",  
                   tags$span(style="color:blue", "Link to a file of the example dataset"),),   
            div(p(" ")),
         
            sidebarLayout(
              
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                # Input: Select a file ----
                fileInput("file1", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", FALSE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t",
                                         Whitespace = ""),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = ''),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Select number of rows to display ----
              #   radioButtons("disp", "Display",
              #                choices = c(Head = "head",
              #                            All = "all"),
              #                selected = "head"),
              #   
              #   # Horizontal line ----
              #   tags$hr(),
              #   
              #   # Input: Select number of rows to display ----
              #   radioButtons("what", "Output",
              #                choices = c(Analysis = "Analysis",
              #                            Plot = "plot"),
              #                selected = "Analysis")
              #   
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                
                # Output: Data file ----
                
                
                 div(verbatimTextOutput("contents2")),
            #    div(verbatimTextOutput("contents3")),
                # plotOutput("plotx"),
                # tags$hr(),
                # 
                # tableOutput("contents") 
                
                
              ),
            )
   ) ,
   
   
   
   
   ######
   
                             
                             tabPanel("4 Explanation", value=3, 
                                      
                           
                                   tags$span(style="color:black",
                                   HTML(" <strong>We assume that the measurement errors are independently and identically normally distributed with a mean of zero and variance sigma squared")),

                                   br(),

                                   withMathJax(
                                    
                                     helpText(
                                       tags$span(style="color:black",
                                     'It is well known, with n being the number of replicate measurements, that 
                                     
                                     $$\\frac{{ {\\sum_i} (X_i - \\bar{X})^2}} {\\sigma^2}  \\sim {\\chi^2}_{(n - 1)}  \\qquad  \\qquad \\qquad  \\qquad \\left[ 1 \\right]  \\!$$ 
                                   
                                     recall
                                         
                                     $$\\frac{{ {\\sum_i} (X_i - \\bar{X})^2}} {(n-1)}  =  {\\it{s}^2}\\qquad \\qquad  \\qquad \\qquad  \\qquad \\left[ 2 \\right]  \\!$$
                                           
                                    we can see
                                     
                                     $${{ {\\sum_i} (X_i - \\bar{X})^2}} = {\\it{s}^2} {(n-1)} \\qquad \\qquad \\qquad \\qquad \\left[3 \\right]  \\!$$ 
                                     hence
                                     
                                     $$\\frac{  {\\it{s}^2} {(n-1)} } {\\sigma^2} \\sim {\\chi^2}_{(n - 1)} \\qquad \\qquad  \\qquad \\qquad  \\qquad \\left[ 4 \\right]  \\!$$ 
                                     
                                   
                                     
                                     now we can calculate any quantile of the true population variance (for any sample size n)...
                                     
                                     $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha/2)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 5 \\right]    \\!$$  
                                     
                                    So if we have a reliable estimate of the population variance, 
                                    we can calculate a specification say 95% confidence (50000 in a million) or 99.9997% (3 in a million) to see if the sample replicate variation
                                    is consistent with the population variance. A specification such as 3 in a million is useful for example in the diagnostic industry,
                                    where huge numbers of replicates are run in the field and during analytical studies for example.
                                     

                                      $$ $$
                                     Now if we want to calculate a confidence interval for the population variance, based on the standard deviation from a sample  
                                     
                                     $$ {  P\\left[ {(\\chi^2}_{(n - 1), (\\alpha/2)})   \\le {     {\\chi^2}_{(n - 1)} \\le \ {(\\chi^2}_{(n - 1), (1-\\alpha/2)}) }     \\right] =   1-\\alpha}  \\qquad \\qquad  \\qquad \\qquad  \\qquad \\left[ 6 \\right]  \\!$$  
                                     
                                     the quantity chi-square is now replaced by its equivalent 

                                      $$ {  P\\left[ {(\\chi^2}_{(n - 1), (\\alpha/2)})   \\le { \\frac{  {\\it{s}^2} {(n-1)} } {\\sigma^2}  \\le  {(\\chi^2}_{(n - 1), (1-\\alpha/2)}) }     \\right]   =   1-\\alpha}  \\qquad \\qquad  \\qquad \\qquad    \\left[ 7 \\right]  \\!$$ 
                                     
                                     this can be rearranged to 
                                     
                                      $$ {  P\\left[   { \\frac{  {\\it{s}^2} {(n-1)} } {(\\chi^2_{(n - 1), (1-\\alpha/2)} )}}     \\le   {\\sigma^2}  \\le  { \\frac{  {\\it{s}^2} {(n-1)} } {(\\chi^2_{(n - 1), (\\alpha/2)}) }}     \\right]    =   1-\\alpha    }  \\qquad \\qquad  \\qquad\\qquad \\qquad  \\qquad \\left[ 8 \\right]  \\!$$ 

                                     '))),

                                   
                                   br(),
                                   br(),
                                   br(),
                                   br()
                               
                             )
                              
                           
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   END NEW   
                            )
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  )
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
  
  shinyalert("Welcome! \nInference concerning a population variance!",
             "...the standard deviaiton of a standard deviation...", 
             type = "info")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated, no alpha here
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  random.sample <- reactive({
    
    foo <- input$resample
    
    popsd <- as.numeric(unlist(strsplit(input$popsd,",")))
    sims <- as.numeric(unlist(strsplit(input$sims,",")))
    reps <- as.numeric(unlist(strsplit(input$reps,",")))

    return(list(  
      sims=sims[1],   # Number of simulation
      reps=reps[1],   # replicates
      popsd=popsd[1]  # sd
     
    ))
    
  })
  
  
  # not haveing alpha here, but in next step allows it not to resample
  step <- reactive({
    
    sample <- random.sample()
 
    
    popsd. <- as.numeric(unlist(strsplit(input$popsd,",")))
    sims.  <- as.numeric(unlist(strsplit(input$sims,",")))
    reps.  <- as.numeric(unlist(strsplit(input$reps,",")))
    
    return(list(   popsd.=popsd., sims.=sims.,reps.=reps.)) 
    
    
  })
  
  
  
  
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tab 1 simulate po model data and analyse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NOT OK
  spec <- reactive({
    
  #sample <- random.sample()  code this out
    
  popsd.  <- step()$popsd.
  sims. <- step()$sims.
  reps.   <- step()$reps. 
 
 
    alpha.  <-    isolate(as.numeric(unlist(strsplit(input$alpha,","))) )  ### isolate this so not update with clicking resample button
   
    spec. <- sd.spec(input.sd=popsd., alph=alpha., n=reps.)    ### 
    
    x1 <- replicate(sims., sd(rnorm( reps., 0, popsd.)) )
    
    sim. <- quantile(x1, c(1-alpha.))   # simulation spec one sided
    
    onesample <-  rnorm( reps., 0, popsd.)
    
    return(list( sims.=sims., reps.=reps., alpha.=alpha., popsd.=popsd. , spec.=spec., x1=x1, sim.=sim., onesample=onesample)) 
    
  })
   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ci0 <- reactive({
    
    d <- spec()$onesample  # simulated sample
    return(list( d=d )) 
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~OK
  # ci of pop from sample
  ci <- reactive({
 
    dat <- ci0()$d
    # alpha selected here
    alpha.  <-    (as.numeric(unlist(strsplit(input$alpha,","))) )
   
    df <- length(dat)-1
    est <- var(dat)
   
    lower <- df* est/qchisq(df=df, p=1-alpha./2)
    upper <- df* est/qchisq(df=df, p=alpha./2)
    
    cis <- c(lower, upper)
    sdcis <- c(lower^.5, upper^.5)
    return(list(cis=cis ,d=dat, sdcis=sdcis ,var1=est , sd1=est^.5)) 
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$var1 <- renderPrint({
    
    d <- ci()$var1
    
    return(print(d, digits=4))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$sd1 <- renderPrint({
    
    d <- ci()$sd1
    
    return(print(d, digits=4))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$conf <- renderPrint({
    
    d <- ci()$cis
    
    return(print(d, digits=4))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$conf2 <- renderPrint({
    
    d <- ci()$sdcis
    
    return(print(d, digits=4))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$sample <- renderPrint({
    
    d <- ci()$d
    
    return(print(d, digits=4))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$dat <- renderPrint({
    
    d <- spec()$spec.
    
    return(print(d, digits=4))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$datx <- renderPrint({
    
    d <- spec()$spec.
    
    return(print(d, digits=4))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$datxy <- renderPrint({
    
    d <- spec()$spec.^2
    
    return(print(d, digits=4))
  })
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$dat1 <- renderPrint({
    
    sim <- spec()$sim.
    
    return(print(sim, digits=4))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plot 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
  
  output$his <- renderPlot({
    
    sample <- random.sample()
    
    dx <-  spec()$x1
    sp <- spec()$spec.
    
    reps. <-   sample$reps
    popsd.<-   sample$popsd
    sims. <-   sample$sims

    h <-  hist(dx,  breaks="FD", xlab="Standard deviation", xlim=c(0, sp*1.1),
    main=paste("Distribution of",sims., "SDs each of sample size", reps.,"drawn from a \npopulation SD of",popsd.,"black line indicates the upper specification of", p4(sp) ) , col="violet", border='blue')
    abline(v = sp, col="black",   lwd = 2, lty=2)

    })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plot 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
  
  output$his2 <- renderPlot({
    
    sample <- random.sample()
     
    dx <-  spec()$x1
    sp <- spec()$spec.
    
    sims.   <-   sample$sims
    reps.   <-   sample$reps
    alpha.  <-   sample$alpha
    popsd.  <-   sample$popsd
    df      <-   reps. -1
 
     z <- (dx^2)*(df)/(popsd.^2) ## this is chi square distributed.  

    h <-  hist(z,   breaks="FD", xlab="Chi-square distribution", prob=TRUE,
               main=paste0("Blue dashed line is the chi-square distribution with d.f =", df, ", histogram created 
               using equation 4 pluging in the simulated squared SDs (right histogram), \ndegrees of freedom and population standard variance ", p4(popsd.^2) ) , col="violet", border='blue')

    curve( dchisq(x, df=df) , col='blue', add=TRUE, lty=2, lwd=3)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$textWithNumber <- renderText({ 
    
    dis <- as.numeric(unlist(strsplit(input$popsd,",")))
   # n <- as.numeric(unlist(strsplit(input$sims,",")))
    ctr <- as.numeric(unlist(strsplit(input$reps,",")))
    or1<- as.numeric(unlist(strsplit(input$alpha,",")))
    
   # d1 <- spec()$sim.
  #  d <- spec()$spec.
    
    
    HTML(paste0( "With a population standard deviation of "
                 , tags$span(style="color:purple",  p4( dis) ) ,
                 " from a reliable source we will evaluate  "
                 , tags$span(style="color:purple",  p0(ctr) ) ," replicates.",

                 " We have set the one sided alpha level to "
                 , tags$span(style="color:purple",  p8(or1) ) ,
                ". This is equivalent to a upper confidence limit of ",
                tags$span(style="color:purple",  p5(100*(1-or1)) ) , 
                tags$span(style="color:purple",   "%. " ) , 
                
                "This means we are prepared to accept  "
                 , tags$span(style="color:purple",  p0(or1*1e6) ) ,
                 " out of specification results if we were to perform 1 million evaluations 
                using "
                 , tags$span(style="color:purple",  p0(ctr) ) ," replicates",
                 ", when in truth every sample of " 
                , tags$span(style="color:purple",  p0(ctr) ) ," replicates are from the stated population. "
                
          
    ))    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$textWithNumber2 <- renderText({ 
    
    # sims <- as.numeric(unlist(strsplit(input$sims,",")))
   # reps <- as.numeric(unlist(strsplit(input$reps,",")))
     
 
   # d1 <- spec()$sim.   # simulated spec
  #  d <- spec()$spec.   # analytical spec
    
    popsd.  <- step()$popsd.
    sims. <-   step()$sims.
    reps.   <- step()$reps. 
    
    
    alpha.  <-    (as.numeric(unlist(strsplit(input$alpha,","))) )  ### isolate this so not update with clicking resample button
    
    d <- sd.spec(input.sd=popsd., alph=alpha., n=reps.)    ### 
    
       
    x1 <- replicate(sims., sd(rnorm( reps., 0, popsd.)) )
     d1 <- quantile(x1, c(1-alpha.))   # simulation spec one sided
    
    
    HTML(paste0( 
                 "We can therefore state if the standard deviation of the "
                 , tags$span(style="color:purple",  p0(reps.) ) ,
                 " replicated measurements is less than or equal to the calculated specification of "
                 
                 , tags$span(style="color:purple",  p4(d) ) ,
                 
                 " the error is considered consistent with the established test method error. As an aside we can also check the analytic derived specification of "
                 , tags$span(style="color:purple",  p4( d) ) ,
                 " using simulation. With "
                 , tags$span(style="color:purple",  (sims.) ) ," Monte Carlo simulations",

                 
                 " we estimate the specification as "
                 , tags$span(style="color:purple",  p4(d1) ) ,". Remember to estimate a quantile in the extreme tails requires a very large number of Monte Carlo simulations."
                 
                 
    ))    
    
  })

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # analyse on user data, variance components for plot title
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  user <-  reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep =   input$sep,
                   quote = input$quote)
    
    df <-  (as.vector(df))
    
 return(list(df=df))
    
    
    
  })     
  
  
  
  
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # PBE analysis of user uploaded data 
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   output$contents2 <- renderPrint({
     
       u <- user()$df
       
       sd.user <- sd(u)
       
       popsd.  <- step()$popsd.
       sims. <-   step()$sims.
       reps.   <- step()$reps. 
       
       
       alpha.  <-    (as.numeric(unlist(strsplit(input$alpha,","))) )  ### isolate this so not update with clicking resample button
       
       d <- sd.spec(input.sd=popsd., alph=alpha., n=length(u))    ### 
       
       
       df <- length(u)-1
       est <- sd.user^2
       
       lower <- df* est/qchisq(df=df, p=1-alpha./2)
       upper <- df* est/qchisq(df=df, p=alpha./2)
       
       cis <- c(lower, upper)
       sdcis <- c(lower^.5, upper^.5)
       

 
       return(list(data=u , SD.user.data=sd.user, Variance.user.data=est , SD.spec.user.data=d,  SD.confidence.interval.user.data=sdcis, 
                   Variance.confidence.interval.user.data=cis))
        
     })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)