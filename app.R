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
fig.width <- 400
fig.height <- 300
fig.width1 <- 1380
fig.height1 <- 700
fig.width2 <- 1400
fig.height2 <- 300
fig.width3 <- 1400  
fig.height3 <- 600
fig.width4 <- 1380
fig.height4 <- 450
fig.width5 <- 1380
fig.height5 <- 225
fig.width6 <- 400
fig.height6 <- 550
fig.width7 <- 600
fig.widthx <- 593
fig.heightx <- 268
fig.height7 <- 600
fig.width9 <- 1380
fig.height9 <- 500

## convenience functions
p0 <- function(x) {formatC(x, format="f", digits=0)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p5 <- function(x) {formatC(x, format="f", digits=5)}
p6 <- function(x) {formatC(x, format="f", digits=6)}
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
                
                h2("A specification for repeatability"), 
                
                h4("  "), 
                
                h3("  "), 
                
                
                sidebarLayout(
                  
                  sidebarPanel( width=3 ,
                                
                                tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                
                                
                                actionButton(inputId='ab1', label="R Shiny ",   icon = icon("th"),   
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app.R', '_blank')"), 
                                actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app%20stripped%20code.R', '_blank')"),  
                                actionButton("resample", "Simulate a new sample"),
                                br(),  
                                tags$style(".well {background-color:#b6aebd ;}"), 
                                
                                h4("Instructions: The first input below is xxxxxxxxxxxxxx."),
                                div(
                                  
                                  tags$head(
                                    tags$style(HTML('#ab1{background-color:orange}'))
                                  ),
                                  
                                  tags$head(
                                    tags$style(HTML('#resample{background-color:orange}'))
                                  ),
                                  
                                  textInput('n', 
                                            div(h5(tags$span(style="color:blue", "Number of simulations"))), "1e5"),
                                  
                                  tags$hr(),
                                  textInput('dist', 
                                            div(h5(tags$span(style="color:blue", "Standard deviation"))), "3.7"),
                                  
                                  textInput('levels', 
                                            div(h5(tags$span(style="color:blue", "number of replicates"))), "5"),
                                  tags$hr(), 
                                  textInput('or1', 
                                            div(h5(tags$span(style="color:blue", "alpha level"))), ".000003"),
                                  
                                  
                                #  textInput('or2', 
                                      #      div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxxxxxxxxxx"))), "1"),
                                  
                                  #  textInput('n2y2', 
                                  # #      div(h5("Enter the true correlation (tab 2)")), ".8"),
                                  # div(h5(tags$span(style="color:blue", "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))), "0.8"),
                                  # tags$hr(),
                                  
                                  # div(h5("References:")),  
                                  # tags$a(href = "https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29", tags$span(style="color:blue", "[1] PRO"),),   
                                  # div(p(" ")),
                                  # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176345338",  tags$span(style="color:blue", "[2] PO"),),   
                                  # div(p(" ")),
                                  # tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                  # div(p(" ")),
                                  # tags$a(href = "https://blogs.sas.com/content/iml/2017/09/20/fishers-transformation-correlation.html", tags$span(style="color:blue", "[4] xxxxxx"),),  
                                  # div(p(" ")),
                                  # tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "prediction of model mean"),),  
                                  # div(p(" ")),
                                  # tags$hr()
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
                              
                              
                              tabPanel("1 xxxxxx", value=7, 
                                       h4("If the standard deviation of the two replicated measurements is less than or equal to the stated specification, the error is considered consistent with the established test method error. 
The baseline input required is a repeatability sd from a reliable source i.e. a precision study"),
                                      
              
                                       fluidRow(
                                         column(width = 6, offset = 0, style='padding:1px;',
                                                
                                            #    div(plotOutput("beta",  width=fig.width7, height=fig.height7)),
                                            div( verbatimTextOutput("dat") ),
                                         ) ,
                                        
                                         
                                         fluidRow(
                                           column(width = 5, offset = 0, style='padding:1px;',
                                                  div( verbatimTextOutput("dat1") ),
                                                 # div(plotOutput("reg.plotx",  width=fig.width7, height=fig.height7)) 
                                                  
                                           ))),
                                       h4(paste("xxxxxxxxxxxxxxxxxxxxxxx")), 
                                       h4(htmlOutput("textWithNumber",) ),
                              ) ,
                              
                              tabPanel("2 xxxxxxx", value=3, 
                                            
                                      # div(plotOutput("reg.plot99", width=fig.width1, height=fig.height1)),
                                       
                                       fluidRow(
                                         column(width = 7, offset = 0, style='padding:1px;',
                                                h4(paste("Figure 3. xxxxxxxxxxxxxxxxxxxxxxx.")), 
                                           
                                         )),
                                       
                                       
                              ),
                              
                              tabPanel("3 xxxxx", value=7, 
                               
                                       fluidRow(
                                         column(width = 6, offset = 0, style='padding:1px;',
                                                h4("Table 1 Proportional odds model"), 
                                              
                                         ) ,
                                         
                                 
                                         
                                         h4("Table 2 xxxxx"),
                                         fluidRow(
                                           column(width = 6, offset = 0, style='padding:1px;',

                                                 splitLayout(
                                                  textInput("bas1", div(h5("Enter a baseline low effect")), value="1", width=100),
                                                   textInput("bas2", div(h5("Enter a baseline high effect")),value="2", width=100)
                                                 ),

                                                 
                                                #  div( verbatimTextOutput("reg.summary3")),

                                              
                                           ))),
                                        
                              ) ,
                              
                             
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             
                              tabPanel("4 xxxx", value=3, 
                                       
                                       h5(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")), 
                                       textInput('rcat2', 
                                                 div(h5(tags$span(style="color:blue",
                                                                  ))), "999"),
                                       
                                       
                                      # div(plotOutput("preds2", width=fig.width1, height=fig.height3)),
                                       
                                       
                                       
                                       fluidRow(
                                         column(width = 7, offset = 0, style='padding:1px;',
                                                h4(paste("Figure 4. xxxxxxxxxxxxxxxxxxxxxx")), 
                                                 
                                         )),
                              ),
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("5 xxxx", 
                                       h4(paste("Figure 5 & 6. xxxxxxxxxxxxxxxxxxx")),
                                        
                                       h4("xxxxxxxxxxxxxxxxxxxxx
"),
                                       fluidRow(
                                         column(width = 6, offset = 0, style='padding:1px;',
                                                
                                           #     div(plotOutput("preds", width=fig.width7, height=fig.height3)),
                                                
                                                fluidRow(
                                                  
                                                  textInput('base', 
                                                            div(h5(tags$span(style="color:blue", 
                                                                             "xxxxxxxxxxxxxxxxx"))), "1")
                                                  
                                                  
                                                ),
                                         ) ,
                                         
                                         fluidRow(
                                           
                                          
                                           column(width = 5, offset = 0, style='padding:1px;',
                                                 
                                                 # div(plotOutput("predicts", width=fig.width7, height=fig.height3)),
                                                  
                                                  fluidRow(
                                                    
                                                    textInput('group', 
                                                              div(h5(tags$span(style="color:blue", 
                                                                               "xxxxxxxxxxxxxxxxxxxxxx"))), "1"),
                                                    
                                                    textInput('rcat', 
                                                              div(h5(tags$span(style="color:blue", 
                                                                               "xxxxxxxxxxxxxxxxxxxxxxx"))), "999"),

                                                  ),
                                               
                                           ))),
                                       
                                        
                                       width = 30 )     ,
                               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              tabPanel("6 xxxx",
                                       h4(paste("Table 3 xxxxxxxxxxxxxxxxxxxxxxxxx")),
                                       fluidRow(
                                         column(width = 12, offset = 0, style='padding:1px;',
                                       
                                            # div( verbatimTextOutput("reg.summaryp") ),
                                             h4(paste("Table xxxxxxxxxxxxxxxxxxxx")),
                                           #  div( verbatimTextOutput("reg.summaryc") ),
                                         ) ,
                                    
                                         ),

                              ),
                              
                             tabPanel("7 xxxx", value=3, 
                                       h4(" xxxxxxxxxxxxxxxxxxxxxxxx"),
                                      
                                       
                                       fluidRow(
                                         column(width = 6, offset = 0, style='padding:1px;',
                                                
                                             #   div( verbatimTextOutput("reg.summary4") )
                                         ) ,
                                        
                                         fluidRow(
                                           column(width = 5, offset = 0, style='padding:1px;',
                                                  
                                                 # div( verbatimTextOutput("reg.summary5")),
                                                  #div(plotOutput("predictl", width=fig.widthx, height=fig.heightx)),
                                              
                                           ))),
                                      h4("xxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                             ),
                                      
                                      tabPanel("8 xxxx", value=3, 
                                            
                                               fluidRow(
                                                 column(width = 6, offset = 0, style='padding:1px;',
                                                        h4("xxxxxxxxxxxxxxxxxxxx"),
                                                        textInput('kints',
                                                                  div(h5(tags$span(style="color:blue",
                                                                                   ""))), ""), 
                                                        
                                                       # div(plotOutput("PP.plot", width=fig.width7, height=fig.height6)),
                                                        h4("Figure 8 xxxxxxxxxxxxxxxxxxxxxxxxx"),
                                                        br() , 
                                                        
                                                        h4(""),
                                                        
                                                        h4("Table xxxxxxxxxxxxxxxxxxx"),
                                                       # div( verbatimTextOutput("predz"), width = 2), # 
                                  ),
                                                 
                                                 fluidRow(
                                                   
                                                  
                                                   h4("xxxxxxxxxxxxxxxxxx"),
                                                   h4("xxxxxxxxxxxxxxxxxxx"),
                                                   br(), br(), br() ,  
                                                   
                                                   
                                                   column(width = 5, offset = 0, style='padding:0px;',

                                                       #   div(plotOutput("PP.plot2", width=fig.width7, height=fig.height6)),
                                                          h4("Figure 9 xxxxxxxxxxxxxxxxxxxx"),
                                                           
                                                   )))
                                        
                              ) ,
   

                             tabPanel("9 xxxx", value=3, 
                                      
                                  #h5(paste("Checking assumptions")), 
                                  #div(plotOutput("assumption", width=fig.width1, height=fig.height3)),
                                  h4("Figure 10 xxxxxxxxxxxxxxxxxxxxxxx"),
                                  h4( "xxxxxxxxxxxxxxxxxxxxxxx" ),
                                  h4("Table 8 xxxxxxxxxxxxxxxxxxxxx"),
                                  div( verbatimTextOutput("assump")),  
                                
                             ),
                             
                             
                             tabPanel("10 xxxx", value=3, 
                                      
                                      #div(plotOutput("ecdfs", width=fig.width1, height=fig.height3)),
                                      h4("Figure 11 xxxxxxxxxxxxxxxxxxxxxxxxx"), 
                                      h4("xxxxxxxxxxxxxxxxxxxxxxxx"), 
                                   # div(plotOutput("logitseries", width=fig.width1, height=fig.height3)),
                                     
                                      
                                   h4("Figure 12 xxxxxxxxxxxxxxxxxxxxxxxxxx"),  
                                   
                                   h4("xxxxxxxxxxxxxxxxxx.")
                               
                             ),
                              
                             
                             tabPanel("11 xxxx", 
                                      
                                      fluidRow(
                                        column(width = 3, offset = 0, style='padding:1px;',
                                               h4("Table 9 Data listing"),
                                               #div( verbatimTextOutput("dat")),
                                        ),
                                        
                                        column(width = 9, offset = 0, style='padding:1px;',
                                               h4("xxxxxxxxxxx"),
                                               h4("xxxxxxxxxxxxxxxxx 
                                                  \n"),
                                               
                                               tags$hr(),
                                               div(h4("References:")),  
                                               tags$a(href = "https://stats.stackexchange.com/search?q=proportional+odds+model", tags$span(style="color:blue", "[1] Proportional odds model"),),   
                                               div(p(" ")),
                                               tags$a(href = "hhttps://en.wikipedia.org/wiki/Ordered_logit",  tags$span(style="color:blue", "[2] Proportional odds wiki"),),   
                                               div(p(" ")),
                                             #  tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                             #  div(p(" ")),
                                                tags$a(href = "http://hbiostat.org/doc/rms.pdf", tags$span(style="color:blue", "[3] Regression modelling strategies"),),  
                                               div(p(" ")),
                                               tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "[4] Prediction of model mean"),),  
                                               div(p(" ")),
                                             tags$a(href = "https://psyarxiv.com/x8swp/", tags$span(style="color:blue", "[5] Ordinal Regression Models in Psychology: A Tutorial"),),  
                                             div(p(" ")),
                                             tags$a(href = "
https://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression#89485
", tags$span(style="color:blue", "[6] Stackexchange"),),  
                                             div(p(" ")),
                                               tags$hr()
                                               
                                        )
                                        
                                        
                                      )
                             )##end
                             
                             
                             
                             
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   END NEW   
                            )
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  )
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
  
  shinyalert("Welcome! \nxxxxxxxxxxxxx!",
             "xxxxxxxxxxxxxxxxx", 
             type = "info")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  random.sample <- reactive({
    
    foo <- input$resample
    
    dis <- as.numeric(unlist(strsplit(input$dist,",")))
    
    n <- as.numeric(unlist(strsplit(input$n,",")))
    
    ctr <- as.numeric(unlist(strsplit(input$levels,",")))

    # n1y1 <- log(as.numeric(unlist(strsplit(input$or1,","))))   # user enter odds , need log for the maths
    # 
    # n2y2 <- log(as.numeric(unlist(strsplit(input$or2,","))))    # user enter odds , need log for the maths
    # 
    
     or1<- as.numeric(unlist(strsplit(input$or1,",")))
     
    
    return(list(  
      n=n[1],       # Number of simulation
      lev=ctr[1],   # replicates
      or1=or1[1],   # alpha
      shape1=dis[1] # sd
  
   
      
    ))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tab 1 simulate po model data and analyse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spec <- reactive({
    
    sample <- random.sample()
    
    n.    <-   sample$n
    reps. <-   sample$lev
    alpha.  <- sample$or1
    sd.  <-    sample$shape1
 
    spec. <- sd.spec(input.sd=sd., alph=alpha., n=reps.)  
    
    return(list( n.=n., reps.=reps., alpha.=alpha., sd.=sd. , spec.=spec.)) 
  })
   
  
  
  output$dat <- renderPrint({
    
    d <- spec()$spec.
    
    return(print(d, digits=4))
  })
  
  
  sim <- reactive({
    
    sample <- random.sample()
    
    n.    <-   sample$n
    reps. <-   sample$lev
    alpha.  <- sample$or1
    sd.  <-    sample$shape1
    
    x<-replicate(n., sd(rnorm( reps.,0, sd.)) )
    sim. <- quantile(x, c(1-alpha.))
    #hist(x, breaks=1e6/10, xlab="SD", main="Distribution of SD")
    
    return(list( x=x, sim.=sim.)) 
  })
  
  output$dat1 <- renderPrint({
    
    d <- sim()$sim.
    
    return(print(d, digits=4))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # beta dist plot 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$beta <- renderPlot({        
    
    sample <- random.sample()
    
    
        
         
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  end ggplot barplot of beta distribution
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
  output$reg.plotx <- renderPlot({         
    
  
    
     
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$reg.plot99 <- renderPlot({         
    
  
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # non cummulative predicted probabilities plot run the analysis again
  # not efficient I know
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$preds2 <- renderPlot({
    
    sample <- random.sample()
 
    
    
    print(gp)
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # non cummulative predicted probabilities plot run the analysis again
  # not efficient I know
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$preds <- renderPlot({
    
    
   

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tables of predictions
  
  predictz <- reactive({  
    
 
    
    
 
#return(list(probs=probs, cprobs=cprobs, p.with.ci=p.with.ci , plotci=plotci$data))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plots of predictions
  
  output$predicts <- renderPlot({   
    
    # sample <- random.sample()
    # # 
    # f    <- analysis()$f2
    # # 
    # levz <- sample$lev
    
    

    print(gpp)
    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
  output$PP.plot <- renderPlot({   
    
   
    
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  output$PP.plot2 <- renderPlot({   
    
    # K <- analysis()$K
    # 
    # txt <- paste0("Ordinal intercept ", K)
    # 
    # levz <- input$levels
    # 
    # P <- analysis()$P
    
    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a plot of coef from series of logistic regression models. checking assumptions

  output$logitseries <- renderPlot({   
    
    # sample <- random.sample()
    # 
    # #levz <- sample$lev
    # dat <- mcmc()$dat
    
    
    
    
    
  })
  
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~assumption plot~~~~~~~~~~~~~~~~~~~~~~~~    
  # on the fly plot harrell's PO assumption plot...
  
  output$assumption <- renderPlot({   
    
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
  }) 
  
  
  assump <- reactive({
    
    # dat <- mcmc()$dat
    # levz <- input$levels
    # l2 <- as.numeric(levz)-1
    # y <- as.integer(dat$y)  
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # here turn text into a function to allow flexibilty in changing levels
    # 
    # return(list( s=s  )) 
    
  })  
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~baseline predictions~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  predz <- reactive({
    
    
    return(list( p=p )) 
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  
  output$textWithNumber <- renderText({ 
    
    
                
  })

  
  output$assump <- renderPrint({
   
  #  return(print(assump()$s, digits=3))
  
  }) 
  

  output$textWithNumber1 <- renderText({ 
    
    A <- analysis()$f2     
    
    
  })
  

  
  
  output$predz <- renderPrint({
    
    return(print(predz()$p, digits=4))
  })
  
  output$predt <- renderPrint({
    
    return(print(predt()$pt, digits=4))
  })
  
  
  output$reg.summary1 <- renderPrint({
    
    return( (analysis()$f2 ))
    
  })
  
  output$reg.summary3 <- renderPrint({
    
    return(print(analysis()$sf1, digits=4))
    
  })
  
  output$reg.summary4 <- renderPrint({
    
    return(print(lmx()$linear, digits=4))
    
  })

  output$reg.summary5 <- renderPrint({
    
    return(print(lmx()$an, digits=4))
    
  })
  
  output$reg.summaryp <- renderPrint({
    
    #return(print(predictz()$prob, digits=4))
    
  })
  
  output$reg.summaryc <- renderPrint({
    
    
    
  })
  
  output$reg.summaryci <- renderPrint({
    
  
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  lmx <- reactive({
    
  
     
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$predictl <- renderPlot({   
    
  
    
   
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$textWithNumber <- renderText({ 
    
    
    dis <- as.numeric(unlist(strsplit(input$dist,",")))
    n <- as.numeric(unlist(strsplit(input$n,",")))
    ctr <- as.numeric(unlist(strsplit(input$levels,",")))
    or1<- as.numeric(unlist(strsplit(input$or1,",")))
    
   d1 <- sim()$sim.
    d <- spec()$spec.
    
    HTML(paste0( "We have entered a population standard deviation of "
                 , tags$span(style="color:red",  p4( dis) ) ,
                 " from a reliable source and we are planning to evaluate  "
                 , tags$span(style="color:red",  p0(ctr) ) ," replicates.",
                 
                 br(), br(),  
                  
                
                #  tags$span(style="color:red",  p4(ctr) ) , 
            
                 " We have set the one sided alpha level to "
                 , tags$span(style="color:red",  p6(or1) ) ,".",
                 
                 " This means we are prepared to accept  "
                 , tags$span(style="color:red",  p0(or1*1e6) ) ,
                 " out of specification results in 1 million evaluations. ",
                 
                 br(), br(),
                 "If the standard deviation of the "
               , tags$span(style="color:red",  p0(ctr) ) ,
               " replicated measurements is less than or equal to the calcualted specification of "
                 
                 , tags$span(style="color:red",  p4(d) ) ,
                 
                 " the error is considered consistent with the established test method error.",
               
               
               br(), br(),  
               
               "We can also check the analytic solution using simulation "
               , tags$span(style="color:red",  p4( dis) ) ,
               " with "
               , tags$span(style="color:red",  (n) ) ," Monte Carlo simulations",
               
               # br(), br(),  
               
               
               #  tags$span(style="color:red",  p4(ctr) ) , 
               
               " we estimate the specification as "
               , tags$span(style="color:red",  p4(d1) ) ,"."
               
              
               
               
    ))    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)