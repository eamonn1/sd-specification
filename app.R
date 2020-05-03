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
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app.R', '_blank')"), 
                                actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                             onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/proportional-odds-model2/master/app%20stripped%20code.R', '_blank')"),  
                                actionButton("resample", "Simulate a new sample"),
                                br(),  
                                tags$style(".well {background-color:#b6aebd ;}"), 
                                
                                h4("Instructions: The first input below is number of Monte Carlo simulations. The second is the true data generating population SD. The third input is the 
                                   number of replicates to be generated. The last input is the alpha level, the default set at a one-sided 3 in a million level."),
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
                                            div(h5(tags$span(style="color:blue", "alpha level"))), "0.000003"),
                                  
                                  
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
                                
                                
                                tags$hr(),
                                div(h4("References:")),  
                                tags$a(href = "https://www.amazon.com/Statistical-Evaluation-Measurement-Errors-Reliability/dp/0340760702", tags$span(style="color:blue", "[1] Graham Dunn (2nd edition section 2.6.1 page 35"),),   
                                div(p(" ")),
                                tags$a(href = "https://www.wiley.com/en-us/Biostatistics%3A+A+Methodology+For+the+Health+Sciences%2C+2nd+Edition-p-9780471031857",  tags$span(style="color:blue", "[2] Biostatisics A Methodology for the Health Sciences, 2nd Ed. page 96"),),   
                                div(p(" ")),
                                #  tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", tags$span(style="color:blue", "[3] Krushke"),),
                                #  div(p(" ")),
                                # tags$a(href = "http://hbiostat.org/doc/rms.pdf", tags$span(style="color:blue", "[3] Regression modelling strategies"),),  
                                # div(p(" ")),
                                # tags$a(href = "https://rdrr.io/cran/rms/man/predict.lrm.html", tags$span(style="color:blue", "[4] Prediction of model mean"),),  
                                # div(p(" ")),
                                # tags$a(href = "https://psyarxiv.com/x8swp/", tags$span(style="color:blue", "[5] Ordinal Regression Models in Psychology: A Tutorial"),),  
                                # div(p(" ")),
#                                 tags$a(href = "
# https://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression#89485
# ", tags$span(style="color:blue", "[6] Stackexchange"),),  
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
                                   #    h4("If the standard deviation of the two replicated measurements is less than or equal to the stated specification, the error is considered consistent with the established test method error. 
#The baseline input required is a repeatability sd from a reliable source i.e. a precision study"),
                                      
              

                                       h4(htmlOutput("textWithNumber",) ),
                                       h4(paste("Plugging in alpha, the population variance and the number of replicates (n) into equation [5] we calculate the specification.")), 


                                      withMathJax(
  
                                      helpText(
                                           tags$span(style="color:black",
              ' 
              
                            $$ {  {\\it{s}^2} =  \\frac{     {(\\sigma^2)}   {(\\chi^2}_{(n - 1), (1-\\alpha)}) }  {(n-1)}          } \\qquad  \\qquad \\qquad  \\qquad \\left[ 5 \\right]    \\!$$  

              
                             
                            
              '))),

h4(paste("Plugging in the population variance the number of replicates (n) and the simulated data into equation [4] we create the plot below right.")), 


withMathJax(
helpText(
  tags$span(style="color:black",
            ' 
              
                         
              
                            $$\\frac{  {\\it{s}^2} {(n-1)} } {\\sigma^2} \\sim {\\chi^2}_{(n - 1)} \\qquad \\qquad  \\qquad \\qquad  \\qquad \\left[ 4 \\right]  \\!$$ 
                            
              '))),



















 
                                      h4(htmlOutput("textWithNumber2",) ),

                                          #  div(plotOutput("his2",  width=fig.width7, height=fig.height7)),



fluidRow(
  column(width = 6, offset = 0, style='padding:1px;',
         
         div(plotOutput("his",  width=fig.width7, height=fig.height7))
  ) ,
  
  
  fluidRow(
    column(width = 5, offset = 0, style='padding:1px;',
           
           div(plotOutput("his2",  width=fig.width7, height=fig.height7)),
    )))













 
                              ) ,
                              
                              tabPanel("2 A single simulation ", value=3, 
                                         
                                       
                                       
                                       fluidRow(
                                         column(width = 6, offset = 0, style='padding:1px;',
                                                
                                                # div(plotOutput("reg.plot99", width=fig.width1, height=fig.height1)),
                                                h4("A single sample of replicates (from the the population SD data generating mechanism)"), 
                                                div( verbatimTextOutput("sample")),
                                                h4("The standard deviation of the replicates"), 
                                                div( verbatimTextOutput("sd1")),
                                                h4("Upper one-sided specification for the standard deviation of the replicates (see alpha level)"), 
                                                div( verbatimTextOutput("datx")),
                                                
                                                
                                                
                                                h4("The variance of the replicates"), 
                                                div( verbatimTextOutput("var1")),
                                                
                                                h4("Upper one-sided specification for variance of the replicates (see alpha level)"), 
                                                div( verbatimTextOutput("datxy")),
                                                
                                                h4("Confidence interval for the standard deviation (two-sided based on alpha level)"), 
                                                div( verbatimTextOutput("conf2")),
                                                
                                                h4("Confidence interval for the variance (two-sided based on alpha level)"), 
                                                div( verbatimTextOutput("conf")),
                                               
                                         ) ,
                                       
                                       
                                          
                                 
                                     
                                       fluidRow(
                                         column(width = 7, offset = 0, style='padding:1px;',
                                               # h4(paste("Figure 3. xxxxxxxxxxxxxxxxxxxxxxx.")), 
                                           
                                         )),
                                       
                                       
                              )),


 









                              
                              # tabPanel("3 xxxxx", value=7, 
                              #  
                              #          fluidRow(
                              #            column(width = 6, offset = 0, style='padding:1px;',
                              #                   h4("Table 1 Proportional odds model"), 
                              #                 
                              #            ) ,
                              #            
                              #    
                              #            
                              #            h4("Table 2 xxxxx"),
                              #            fluidRow(
                              #              column(width = 6, offset = 0, style='padding:1px;',
                              # 
                              #                    splitLayout(
                              #                     textInput("bas1", div(h5("Enter a baseline low effect")), value="1", width=100),
                              #                      textInput("bas2", div(h5("Enter a baseline high effect")),value="2", width=100)
                              #                    ),
                              # 
                              #                    
                              #                   #  div( verbatimTextOutput("reg.summary3")),
                              # 
                              #                 
                              #              ))),
                              #           
                            #  ) ,
                              
                             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



























                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             
#                               tabPanel("4 xxxx", value=3, 
#                                        
#                                        h5(paste("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")), 
#                                        textInput('rcat2', 
#                                                  div(h5(tags$span(style="color:blue",
#                                                                   ))), "999"),
#                                        
#                                        
#                                       # div(plotOutput("preds2", width=fig.width1, height=fig.height3)),
#                                        
#                                        
#                                        
#                                        fluidRow(
#                                          column(width = 7, offset = 0, style='padding:1px;',
#                                                 h4(paste("Figure 4. xxxxxxxxxxxxxxxxxxxxxx")), 
#                                                  
#                                          )),
#                               ),
#                               #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               tabPanel("5 xxxx", 
#                                        h4(paste("Figure 5 & 6. xxxxxxxxxxxxxxxxxxx")),
#                                         
#                                        h4("xxxxxxxxxxxxxxxxxxxxx
# "),
#                                        fluidRow(
#                                          column(width = 6, offset = 0, style='padding:1px;',
#                                                 
#                                            #     div(plotOutput("preds", width=fig.width7, height=fig.height3)),
#                                                 
#                                                 fluidRow(
#                                                   
#                                                   textInput('base', 
#                                                             div(h5(tags$span(style="color:blue", 
#                                                                              "xxxxxxxxxxxxxxxxx"))), "1")
#                                                   
#                                                   
#                                                 ),
#                                          ) ,
#                                          
#                                          fluidRow(
#                                            
#                                           
#                                            column(width = 5, offset = 0, style='padding:1px;',
#                                                  
#                                                  # div(plotOutput("predicts", width=fig.width7, height=fig.height3)),
#                                                   
#                                                   fluidRow(
#                                                     
#                                                     textInput('group', 
#                                                               div(h5(tags$span(style="color:blue", 
#                                                                                "xxxxxxxxxxxxxxxxxxxxxx"))), "1"),
#                                                     
#                                                     textInput('rcat', 
#                                                               div(h5(tags$span(style="color:blue", 
#                                                                                "xxxxxxxxxxxxxxxxxxxxxxx"))), "999"),
# 
#                                                   ),
#                                                
#                                            ))),
#                                        
#                                         
#                                        width = 30 )     ,
#                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               tabPanel("6 xxxx",
#                                        h4(paste("Table 3 xxxxxxxxxxxxxxxxxxxxxxxxx")),
#                                        fluidRow(
#                                          column(width = 12, offset = 0, style='padding:1px;',
#                                        
#                                             # div( verbatimTextOutput("reg.summaryp") ),
#                                              h4(paste("Table xxxxxxxxxxxxxxxxxxxx")),
#                                            #  div( verbatimTextOutput("reg.summaryc") ),
#                                          ) ,
#                                     
#                                          ),
# 
#                               ),
#                               
#                              tabPanel("7 xxxx", value=3, 
#                                        h4(" xxxxxxxxxxxxxxxxxxxxxxxx"),
#                                       
#                                        
#                                        fluidRow(
#                                          column(width = 6, offset = 0, style='padding:1px;',
#                                                 
#                                              #   div( verbatimTextOutput("reg.summary4") )
#                                          ) ,
#                                         
#                                          fluidRow(
#                                            column(width = 5, offset = 0, style='padding:1px;',
#                                                   
#                                                  # div( verbatimTextOutput("reg.summary5")),
#                                                   #div(plotOutput("predictl", width=fig.widthx, height=fig.heightx)),
#                                               
#                                            ))),
#                                       h4("xxxxxxxxxxxxxxxxxxxxxxxxxxx"),
#                              ),
#                                       
#                                       tabPanel("8 xxxx", value=3, 
#                                             
#                                                fluidRow(
#                                                  column(width = 6, offset = 0, style='padding:1px;',
#                                                         h4("xxxxxxxxxxxxxxxxxxxx"),
#                                                         textInput('kints',
#                                                                   div(h5(tags$span(style="color:blue",
#                                                                                    ""))), ""), 
#                                                         
#                                                        # div(plotOutput("PP.plot", width=fig.width7, height=fig.height6)),
#                                                         h4("Figure 8 xxxxxxxxxxxxxxxxxxxxxxxxx"),
#                                                         br() , 
#                                                         
#                                                         h4(""),
#                                                         
#                                                         h4("Table xxxxxxxxxxxxxxxxxxx"),
#                                                        # div( verbatimTextOutput("predz"), width = 2), # 
#                                   ),
#                                                  
#                                                  fluidRow(
#                                                    
#                                                   
#                                                    h4("xxxxxxxxxxxxxxxxxx"),
#                                                    h4("xxxxxxxxxxxxxxxxxxx"),
#                                                    br(), br(), br() ,  
#                                                    
#                                                    
#                                                    column(width = 5, offset = 0, style='padding:0px;',
# 
#                                                        #   div(plotOutput("PP.plot2", width=fig.width7, height=fig.height6)),
#                                                           h4("Figure 9 xxxxxxxxxxxxxxxxxxxx"),
#                                                            
#                                                    )))
#                                         
#                               ) ,
#    
# 
#                              tabPanel("9 xxxx", value=3, 
#                                       
#                                   #h5(paste("Checking assumptions")), 
#                                   #div(plotOutput("assumption", width=fig.width1, height=fig.height3)),
#                                   h4("Figure 10 xxxxxxxxxxxxxxxxxxxxxxx"),
#                                   h4( "xxxxxxxxxxxxxxxxxxxxxxx" ),
#                                   h4("Table 8 xxxxxxxxxxxxxxxxxxxxx"),
#                                   div( verbatimTextOutput("assump")),  
#                                 
#                              ),
#                              
                             
                             tabPanel("3 Explanation", value=3, 
                                      
                                      #div(plotOutput("ecdfs", width=fig.width1, height=fig.height3)),
                                   #    h4("Figure 11 xxxxxxxxxxxxxxxxxxxxxxxxx"), 
                                   #    h4("xxxxxxxxxxxxxxxxxxxxxxxx"), 
                                   # # div(plotOutput("logitseries", width=fig.width1, height=fig.height3)),
                                   #   
                                   #    
                                   # h4("Figure 12 xxxxxxxxxxxxxxxxxxxxxxxxxx"),  
                                   # 
                                   
                                   
                                   
                                   
                                  # withMathJax(),
                                   # section below allows in-line LaTeX via $ in mathjax.
                                   #tags$div(HTML("")),
                                   #helpText('An irrational number $\sqrt{2}$ and a fraction $1-\frac{1}{2}$'),
                                   
                                   
                                  #$$\\frac{{ {\\chi^2}_{(n - 1)}}} {\\sigma^2}  \\!$$ degrees of freedom (n being the number of replicate measurements)
                                   
                                   # h4("xxxxxxxxxxxxxxxxxx.")
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
                               
                             )#,
                              
                             
                             # tabPanel("11 xxxx", 
                             #          
                             #          fluidRow(
                             #            column(width = 3, offset = 0, style='padding:1px;',
                             #                   h4("Table 9 Data listing"),
                             #                   #div( verbatimTextOutput("dat")),
                             #            ),
                             #            
                             #            column(width = 9, offset = 0, style='padding:1px;',
                             #                   h4("xxxxxxxxxxx"),
                             #                   h4("xxxxxxxxxxxxxxxxx 
                             #                      \n"),
                             #                   
                             #            )
                             #            
                             #            
                             #          )
                             # )##end
                             
                             
                             
                             
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
             "...the standard deviaiton of a standard deviation...what?", 
             type = "info")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated , no alpha here
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tab 1 simulate po model data and analyse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spec <- reactive({
    
  sample <- random.sample()
    
        popsd. <- as.numeric(unlist(strsplit(input$popsd,",")))
        sims.  <- as.numeric(unlist(strsplit(input$sims,",")))
        reps.  <- as.numeric(unlist(strsplit(input$reps,",")))

        
    spec. <- sd.spec(input.sd=popsd., alph=alpha., n=reps.)  
    
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  
  output$datx <- renderPrint({
    
    d <- spec()$spec.
    
    return(print(d, digits=4))
  })
  
  
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
  # beta dist plot 
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
               using equation 4 pluging in the simulated squared SDs (seen left), \ndegrees of freedom and population standard variance ", p4(popsd.^2) ) , col="violet", border='blue')
    #abline(v = sp, col = "blue", lwd = 2, lty=2)
   
    curve( dchisq(x, df=df) , col='blue', add=TRUE, lty=2, lwd=3)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$textWithNumber <- renderText({ 
    
    dis <- as.numeric(unlist(strsplit(input$popsd,",")))
    n <- as.numeric(unlist(strsplit(input$sims,",")))
    ctr <- as.numeric(unlist(strsplit(input$reps,",")))
    or1<- as.numeric(unlist(strsplit(input$alpha,",")))
    
    d1 <- spec()$sim.
    d <- spec()$spec.
    
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
                 , tags$span(style="color:purple",  p0(ctr) ) ," replicates ",
                 " when in truth every sample of " 
                , tags$span(style="color:purple",  p0(ctr) ) ," replicates are from the stated population. "
                
          
    ))    
    
  })
  
  
  output$textWithNumber2 <- renderText({ 
    
    dis <- as.numeric(unlist(strsplit(input$popsd,",")))
    n <- as.numeric(unlist(strsplit(input$sims,",")))
    ctr <- as.numeric(unlist(strsplit(input$reps,",")))
    or1<- as.numeric(unlist(strsplit(input$alpha,",")))
    
 
    d1 <- spec()$sim.
    d <- spec()$spec.
    
    HTML(paste0( 
                 "We can therefore state if the standard deviation of the "
                 , tags$span(style="color:purple",  p0(ctr) ) ,
                 " replicated measurements is less than or equal to the calculated specification of "
                 
                 , tags$span(style="color:purple",  p4(d) ) ,
                 
                 " the error is considered consistent with the established test method error. As an aside we can also check the analytic derived specification of "
                 , tags$span(style="color:purple",  p4( d) ) ,
                 " using simulation. With "
                 , tags$span(style="color:purple",  (n) ) ," Monte Carlo simulations",

                 
                 " we estimate the specification as "
                 , tags$span(style="color:purple",  p4(d1) ) ,". Remember to estimate a quantile in the extreme tails requires a very large number of Monte Carlo simulations."
                 
                 
    ))    
    
  })

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})

# Run the application 
shinyApp(ui = ui, server = server)