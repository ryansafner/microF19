#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cobb-Douglas Cost Functions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Cost Function Parameters"),
            p("Set the parameters below for the production function"),
            helpText("$$q=l^{\alpha}k^{\beta}$$"),
            # Input: Slider for f ----
            sliderInput(inputId = "alpha",
                        label = "Fixed Costs, alpha",
                        min = 0,
                        max = 2,
                        value = 0.5, step=0.25),
            
            # Input: Slider for a ----
            sliderInput(inputId = "beta",
                        label = "Scale Parameter, beta",
                        min = 0,
                        max = 2,
                        value = 0.5, step=0.25),
            
            # Input: Slider for b ----
            sliderInput(inputId = "w",
                        label = "Price of labor, w",
                        min = 0.0,
                        max = 2.5,
                        value = 1.5, step=0.5),
            
            # Input: Slider for b ----
            sliderInput(inputId = "r",
                        label = "Price of capital, r",
                        min = 0.0,
                        max = 2.5,
                        value = 1.5, step=0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"),
           p("Total Costs", uiOutput("wage"), align="center"),
           p("Total Costs", uiOutput("TC"), align="center"),
           p("This model is coded by",
             a("Ryan Safner",
               href="http://ryansafner.com"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        
        #create functions from parameters
        w<-input$w
        r<-input$r
        a<-input$alpha
        b<-input$beta
        total<-a+b
        
        #create functions from parameters 
        library("mosaic")
        #total cost function
        C=function(q){((a/b)^{b/total}+(a/b)^{-a/total})*w^{a/total}*r^{b/total}*q^{1/total}}
        
        #average fixed costs function
        #AFC=function(x){f/x}
        
        #average variable costs function
        #AVC=function(x){(a*x^b)/x}
        
        #average costs function
        #AC=function(x){(f+a*x^b)/x}
        
        #marginal costs function
        #MC=function(x){a*b*x^(b-1)}
        
        #total costs function is sum of C and D functions
        #Total=function(x){C(x)+D(x)}
        
        #find minimum of total costs
        #minimum<-optimize(Total, interval=c(0, 100), maximum=FALSE)
        
        #round minimum (for printing in caption)
        #mino<-round(minimum$minimum, digits=0)
        
        #plot functions 
        library("ggplot2")
        ggplot(data.frame(x=c(0,10)), aes(x=x))+
            stat_function(fun=C, geom="line",color="purple")+
            #stat_function(fun=AVC, geom="line",color="green")+
            #stat_function(fun=MC, geom="line",color="red")+
            #stat_function(fun=AC, geom="line", color="orange")+
            #geom_vline(xintercept=minimum$minimum, color="green",linetype="dashed", size=1.5)+
            #geom_vline(xintercept=50, color="black", linetype="dotted", size=1)+
            xlab("Output (q)")+
            ylab("Costs")+
            theme_bw()    
    })
    
    output$wage<-renderUI({
        w <- reactive(input$w)
        FC <- "$$wage=%.0f$$"
        text <- sprintf(FC, w())
        withMathJax(  
            tags$p(text)
        )
    })
    
    output$TC<-renderUI({
        #create functions from parameters
        w<-reactive(input$w)
        r<-reactive(input$r)
        a<-reactive(input$alpha)
        b<-reactive(input$beta)
        total<-a+b
        
        #total cost function
        C_coef<-(((a/b)^{b/total}+(a/b)^{-a/total})*w^{a/total}*r^{b/total})
        C_exp<-(1/total)
        
        TC <- "$$C(q)=%0.fq^{%0.2f}$$"
        text <- sprintf(TC, C_coef(),C_exp())
        withMathJax(
            tags$p(text)
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
