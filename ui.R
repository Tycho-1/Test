shinyUI(pageWithSidebar(
    headerPanel("Example Option Plot"),
    sidebarPanel(
        numericInput('St','Current price of underlying', value = 50, min = 0, max = Inf, step = 5),
        numericInput('K','Strike price', value = 50, min = 0,max = Inf, step = 5),
        hr(),
        sliderInput('r','Interest Rate', value = .05, min = 0, max = 1,step = .01),
        sliderInput('sigma', 'Volatility', value = .2, min = 0, max = 1, step = 0.01),
        sliderInput('t','Time in years', value = 1, min = 0, max = 20, step = .25),
        submitButton("Go!")
        ),
    mainPanel(
        tabsetPanel(
            tabPanel("Documentation",includeHTML("documentation.html")),
            tabPanel("Prices", mainPanel(
                h3('Prices of the Options'), verbatimTextOutput("BS_price"), 
                h3('Put-Call Parity'), 
                p('Call + Discounted Strike Price = C + K*exp(-r*T)'), verbatimTextOutput("Call_price"),
                                                                       verbatimTextOutput("K_discounted"),
                                                                       verbatimTextOutput("Call_plus_K"),
                p('Put + Stock Price = P + St'), verbatimTextOutput("Put_price"),
                                                 verbatimTextOutput("Stock_price"),
                                                 verbatimTextOutput("Put_plus_St"))
                ),
            tabPanel("ProfitPlot",  plotOutput('myProfitPlot')), 
            tabPanel("Sensitivity I", h3("Sensitivity of the Stock Price and the Strike Price"), plotOutput('sensitivitySt_K')),
            tabPanel("Sensitivity II", h3("Sensitivity of Volatility, Interest rates, and Time"), plotOutput('sensitivitySigma_r_t')),
            tabPanel("Greeks I", plotOutput("GreeksI")),
            tabPanel("Greeks II", plotOutput("GreeksII"))
        )
       
    )
))