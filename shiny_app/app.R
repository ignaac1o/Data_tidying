
library(shiny)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)
library(forecast)
library(bslib)


ui<-navbarPage(title = "Stock Viewer", theme = bs_theme(bootswatch = "lux"),
           tabPanel(title = "Home", 
                    h5("HOME"),
                    hr(),
                    fluidPage(
                        column(1,offset = 4,
                               imageOutput("logo")
                               
                        ),
                        br(),
                        br(),
                        
                        #column(6,offset = 3,
                        h2(style="text-align: center","Shiny-App Description"),
                        #),
                        
                        br(),
                        br(),
                            
                        p(style="text-align: justify; font-size = 25px",
                              "Stock-Viewer is a Shiny application made with just with the purpose of learning how to build this type of applications. 
                              This is an assignment for the elective subject of", em("Data Tying and Reporting"), "from the masters program in", em("Statistics for Data Science"), "at the Universidad Carlos III de Madrid. 
                              To build this application we have basically coded some of the most used functions and methods from the library",em("Quantmod,"), "which is one of the most used ones for stock market analysis using R."
                              ),
                        
                        br(),
                        
                        p(style="text-align: justify; font-size = 25px",
                          h6("*DISCLAIMER*")),
                        p(style="text-align: justify; font-size = 25px",
                          "This is just a learning tool. Therefore, do not consider the information provided as an invesment guide. We will not be responsible for your losses."
                        ),
                        
                        br(),
                        br(),
                        
                        p(style="text-align: center; font-size = 20px",
                              em("Authors:"),"Alejandra Estrada & Ignacio Almodóvar")
                        
                        
                    )

            ),
           

            tabPanel(title = "Price Overview",
                    fluidPage(
                        
                        h5("PRICE OVERVIEW"),
                        
                        hr(),
                        
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
                                   numericInput(inputId = "breaksDays", label = "Nb breaks", value = 5,min = 1,max = 50),
                                   
                            ),
                            
                            column(1,
                                   selectInput(inputId = "typeBreaks", label = "Day/Month/Year", choices = c("days","weeks","months","years"),selected = "months"),
                            ),

                            
                        ),
                        
                        fluidRow(
                            column(2, offset = 5,
                                   checkboxInput("candle_chart",label="Show Candle Chart",value = FALSE,width = "100%"),
                            ),
                            
                        ),
                        
                        p(style="text-align: center; font-size = 20px","*Data is downloaded from Yahoo Finance. Therefore, for some tickers you might need to specify also the stock exchange. e.g. CLNX.MC"),
                        
                        hr(),
                        
                        plotOutput("stockPlot",width = "100%"),
                        
                        hr(),
                        
                        
                        fluidRow(
                            column(2, offset = 5,
                                   checkboxInput("MAbox","Show Moving Average",value = FALSE,width = "100%"),
                            )
                        ),

                        fluidRow(
                            column(2,offset=5,
                               numericInput(inputId = 'MAdays',label = "Days of window",value = 10,min = 1,max = 150),
                            )
                        ),
                        
                    )
            
            ),
           
           
           tabPanel(title = "Returns",
                fluidPage(
                    h5("Returns"),
                    hr(),
                    
                    sidebarLayout(
                    
                        sidebarPanel(

                            radioButtons(inputId = "return",label = "Select return calculation period",
                                 choices = c("Daily","Weekly","Monthly","Quarterly","Yearly"),selected = "Weekly"),
                            
                            numericInput(inputId = "breaksDays_return", label = "Nb of breaks", value = 5,min = 1,max = 50),
                            
                            selectInput(inputId = "typeBreaks_return", label = "Day/Month/Year", 
                                        choices = c("days","weeks","months","years"),selected = "months"),
                            
              
                        ),
                    mainPanel(
                        plotOutput("return_plot")
                    )
                )
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
    
    candle=reactive({
        input$candle_chart
    })
    
    
    output$logo <- renderImage({
        
            list(src = "chart2.png",
             height = 400,width=390)
        
    }, deleteFile = F)
    
    return_period=reactive({
        input$return
    })
    
    breaksday_return=reactive({
        input$breaksDays_return
    })
    
    typebreaks_return=reactive({
        input$typeBreaks_return
    })
        
    output$stockPlot=renderPlot({
        
        breaks=paste(as.character(breaksday()),sep=" ",typebreaks())
        getSymbols(input$tkt,src="yahoo",from=start(),to=end())
        stock=xts(eval(parse(text = input$tkt)))
        
        if(mabox()==TRUE & candle()==FALSE){
            stock_ma <- subset(stock, index(stock) >= start())
            stock_maDays <- rollmean(stock_ma[,6], madays(), fill = list(NA, NULL, NA), align = "right")
            stock_ma$maDays <- coredata(stock_maDays)
        
        ggplot(stock, aes(x = index(stock_ma))) +
            geom_line(aes(y = stock_ma[,6], color = "deepskyblue4")) + ggtitle(input$tkt) +
            geom_line(aes(y = stock_ma$maDays, color = "MM10")) + xlab("Date") + ylab("Price") +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
            scale_x_date(date_labels = "%b %y", date_breaks = breaks) +
            scale_colour_manual("Series", values=c("PBR"="deepskyblue4", "MM10"="firebrick4"))

    
        }else if(candle()==TRUE){
            chartSeries(stock,TA=NULL,theme = "white",time.scale = breaks,name=input$tkt)
            if(mabox()==TRUE){
                addSMA(n = madays(), on = 1, with.col = Cl, overlay = TRUE, col = "brown")
            }
        }
        else{
            ggplot(stock, aes(x = index(stock), y = stock[,6])) +
            geom_line(color = "deepskyblue4")+
            ggtitle(input$tkt) +
            xlab("Date") + ylab("Price") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_date(date_labels = "%b %y", date_breaks = breaks)
        }
    })
    
    
    output$return_plot=renderPlot({
        
        breaks_return=paste(as.character(breaksday_return()),sep=" ",typebreaks_return())
        getSymbols(input$tkt,src="yahoo",from=start(),to=end())
        stock=xts(eval(parse(text = input$tkt)))
        
        result = switch(return_period(),  
            "Daily"= dailyReturn(stock),  
            "Weekly"= weeklyReturn(stock),  
            "Monthly"= monthlyReturn(stock),  
            "Quarterly"= quarterlyReturn(stock),
            "Yearly"= yearlyReturn(stock),
        )
        
        ggplot(result, aes(x = index(result), y = result)) +
            geom_line(color = "deepskyblue4") +
            ggtitle(input$tkt) +
            xlab("Date") + ylab("Return") +
            theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = breaks_return)
        
        
    })
    
    
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
