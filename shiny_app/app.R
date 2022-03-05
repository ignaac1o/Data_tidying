#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)
library(forecast)
library(bslib)


#Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     h1(titlePanel("Stock Viewer")),
# 
#     fluidRow(
#         column(2, offset = 3,
#                textInput("tkt","Introduce stock acronym",value = "AAPL"),
#                ),
# 
#         column(1,
#                #Start Date
#                dateInput(inputId = 'StartDate',label="Start Date",value='2020-01-01',autoclose = TRUE),
#                ),
# 
#         column(1,
#                #End Date
#                dateInput(inputId = 'EndDate',label="End Date",autoclose = TRUE,max = Sys.Date()),
#                #datesdisabled = seq.Date(from = as.Date("1000-01-01"),to=as.Date('StartDate'),by=1)),
#                ),
# 
#         column(1,
#                numericInput(inputId = "breaksDays", label = "Number of breaks", value = 5,min = 1,max = 50),
# 
#         ),
# 
#         column(1,
#                selectInput(inputId = "typeBreaks", label = "Day/Month/Year", choices = c("days","weeks","months","years"),selected = "months"),
#         ),
# 
#     ),
# 
#     fluidRow(
#         column(1, offset = 3,
#             checkboxInput("MAbox","Show Moving Average",value = FALSE),
#         ),
#         column(1,
#             numericInput(inputId = 'MAdays',label = "Days of window",value = 10,min = 1,max = 150),
#         ),
# 
#     ),
# 
#     # fluidRow(
#     #     column(2, offset=5,
#     #            submitButton('Set Changes'),
#     #     )
#     # ),
# 
#     hr(),
# 
#     plotOutput("stockPlot",width = "100%"),
# 
# 
#     hr(),
# 
#     verbatimTextOutput("verb"),
# )


ui<-navbarPage(title = "Stock-Tool", theme = bs_theme(bootswatch = "lux"),
           tabPanel(title = "Home", 
                    "content 1"),
           tabPanel(title = "Price Overview",
                    "content 2",
                    fluidPage(
                        
                        h1(titlePanel("Stock Viewer")),
                        
                        fluidRow(
                            column(2, offset = 3,
                                   textInput("tkt","Introduce stock acronym",value = "AAPL"),
                            ),
                            
                            column(1,
                                   #Start Date
                                   dateInput(inputId = 'StartDate',label="Start Date",value='2020-01-01',autoclose = TRUE),
                            ),
                            
                            column(1,
                                   #End Date
                                   dateInput(inputId = 'EndDate',label="End Date",autoclose = TRUE,max = Sys.Date()),
                                   #datesdisabled = seq.Date(from = as.Date("1000-01-01"),to=as.Date('StartDate'),by=1)),
                            ),
                            
                            column(1,
                                   numericInput(inputId = "breaksDays", label = "Nb of breaks", value = 5,min = 1,max = 50),
                                   
                            ),
                            
                            column(1,
                                   selectInput(inputId = "typeBreaks", label = "Day/Month/Year", choices = c("days","weeks","months","years"),selected = "months"),
                            ),
                            
                        ),
                        
                        hr(),
                        
                        plotOutput("stockPlot",width = "100%"),
                        
                        hr(),
                        
                        fluidRow(
                            column(2, offset = 4,
                                   checkboxInput("MAbox","Show Moving Average",value = FALSE,width = "100%"),
                            ),
                            column(2,
                                   numericInput(inputId = 'MAdays',label = "Days of window",value = 10,min = 1,max = 150),
                            ),
                            
                        ),
                        
                        # fluidRow(
                        #     column(2, offset=5,
                        #            submitButton('Set Changes'),
                        #     )
                        # ),
                        
                        
                        
                        
   
                        
                        verbatimTextOutput("verb"),
                    )
                    
                    
            ),
           
           
           tabPanel(title = "Predictions",
                fluidPage(
                    h1(titlePanel("Predictions")),
                    
                    hr(),
                    
                    
                    plotOutput("predictionPlot",),
                    
                    verbatimTextOutput("test"),
                    
                    
                )    
            ),
           
           inverse=T
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    start <- reactive({
        as.character(input$StartDate)
    })

    end <- reactive({
       as.character(input$EndDate)
    })
    
    
    output$verb<-renderPrint({
         
         paste("La fecha",breaksday())
    })
    
    mabox=reactive({
        input$MAbox
    })
    
    madays=reactive({
        input$MAdays
    })
    
    breaksday=reactive({
        input$breaksDays
    })
    
    typebreaks=reactive({
       input$typeBreaks
    })
    
    output$test<-renderPrint({
        
        paste("La fecha",breaksday())
    })
    
    
     
    
        
    output$stockPlot=renderPlot({
        
        breaks=paste(as.character(breaksday()),sep=" ",typebreaks())
        getSymbols(input$tkt,src="yahoo",from=start(),to=end())
        stock=xts(eval(parse(text = input$tkt)))
        
        if(mabox()==TRUE){
            stock_ma <- subset(stock, index(stock) >= start())
            stock_maDays <- rollmean(stock_ma[,6], madays(), fill = list(NA, NULL, NA), align = "right")
            stock_ma$maDays <- coredata(stock_maDays)
        
        ggplot(stock, aes(x = index(stock_ma))) +
            geom_line(aes(y = stock_ma[,6], color = "darkblue")) + ggtitle("Petrobras prices series") +
            geom_line(aes(y = stock_ma$maDays, color = "MM10")) + xlab("Date") + ylab("Price") +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
            scale_x_date(date_labels = "%b %y", date_breaks = breaks) +
            scale_colour_manual("Series", values=c("PBR"="gray40", "MM10"="firebrick4"))

    
    }else{
        ggplot(stock, aes(x = index(stock), y = stock[,6])) +
            geom_line(color = "darkblue")+
            ggtitle(input$tkt) +
            xlab("Date") + ylab("Price") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_date(date_labels = "%b %y", date_breaks = breaks)
        }
    })
    
    
    output$predictionPlot=renderPlot({
        getSymbols(input$tkt,src="yahoo",from=start(),to=end())
        stock=xts(eval(parse(text = input$tkt)))
        breaks=paste(as.character(breaksday()),sep=" ",typebreaks())
        number=100

        trainset=head(Cl(stock),length(Cl(stock))-number)
        testset=tail(Cl(stock),number)

        # Forecast the data
        fc_na <- naive(trainset, h=number)

        # Plot the result
        autoplot(fc_na) +
            autolayer(ts(testset, start=length(trainset)), series = "Test Data")
        
    })
    
    
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
