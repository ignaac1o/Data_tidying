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



# Define UI for application that draws a histogram
ui <- fluidPage(
    
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
               numericInput(inputId = "breaksDays", label = "Number of breaks", value = 5,min = 1,max = 50),
            
        ),
        
        column(1,
               selectInput(inputId = "typeBreaks", label = "Day/Month/Year", choices = c("days","weeks","months","years"),selected = "months"),
        ),
        
    ),
    
    fluidRow(
        column(1, offset = 3,
            checkboxInput("MAbox","Show Moving Average",value = FALSE),
        ),
        column(1,
            numericInput(inputId = 'MAdays',label = "Days of window",value = 10,min = 1,max = 150),
        ),

    ),
    
    # fluidRow(
    #     column(2, offset=5,
    #            submitButton('Set Changes'),
    #     )
    # ),
    
    hr(),
    
    plotOutput("stockPlot",width = "100%"), 

    
    hr(),
    
    verbatimTextOutput("verb"),
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
         
         paste("La fecha",breaks)
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
    
    
    
    
        
    output$stockPlot=renderPlot({
        
        breaks=paste(as.character(breaksday()),sep=" ",typebreaks())  
        
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
        getSymbols(input$tkt,src="yahoo",from=start(),to=end())
        stock=xts(eval(parse(text = input$tkt)))
        ggplot(stock, aes(x = index(stock), y = stock[,6])) +
            geom_line(color = "darkblue")+
            ggtitle(input$tkt) +
            xlab("Date") + ylab("Price") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_date(date_labels = "%b %y", date_breaks = breaks)
        }
    })
    
    
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
