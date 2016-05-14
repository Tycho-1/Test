BS_call <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    St*pnorm(d1) - K*exp(-r*(t))*pnorm(d2)
}

BS_put <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    K*exp(-r*(t))*pnorm(-d2) - St*pnorm(-d1)
}

call_opt <-function(x){
    if (x<K) {y<-(-CallPrice)}
    else {y=-CallPrice +x-K}
    y
}

put_opt <- function(x){
    if (x>K) {y<-(-P)}
    else {y= -P+K-x}
    y
}

delta_call <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    pnorm(d1) 
}

delta_put <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    pnorm(d1)-1 
}

gamma_option <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    (exp(-(d1^2)/2)/(sqrt(2*pi)))/(St*sigma*sqrt(t))
}


vega_option <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    (exp(-(d1^2)/2)/(sqrt(2*pi)))*(St*sqrt(t))
}

rho_call <- function(St,K,r,sigma,t){
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    K*t*exp(-r*t)*pnorm(d2)
}

rho_put <- function(St,K,r,sigma,t){
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    -K*t*exp(-r*t)*pnorm(-d2)
}

theta_call <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    (-St*(exp(-(d1^2)/2)/(sqrt(2*pi)))*sigma)/(2*sqrt(t)) - r*K*exp(-r*t)*pnorm(d2)
}

theta_put <- function(St,K,r,sigma,t){
    d1=(1/(sigma*sqrt(t))*(log(St/K) + (r + (sigma^2)/2)*(t)))
    d2=(1/(sigma*sqrt(t))*(log(St/K) + (r - (sigma^2)/2)*(t)))
    (-St*(exp(-(d1^2)/2)/(sqrt(2*pi)))*sigma)/(2*sqrt(t)) + r*K*exp(-r*t)*pnorm(-d2)
}

library(ggplot2)
library(gridExtra)

shinyServer(
    function(input, output) {
            output$myProfitPlot <- renderPlot({
            K <<- input$K
            CallPrice <<- BS_call(input$St,input$K,input$r,input$sigma,input$t)
            P <<- BS_put(input$St,input$K,input$r,input$sigma,input$t)
            g1 <- qplot(2*((1:1e3*K)/1e3),as.numeric(lapply(2*((1:1e3*K)/1e3),call_opt)),col=I("red"),xlab = "Price of unerlying",ylab="Profit of Long Call")+geom_hline(yintercept=0,col="blue")+theme_bw()+annotate("Text",x=.5*K,y=CallPrice*.5,label="Long Call",size=5)
            g2 <- qplot(2*((1:1e3*K)/1e3),-as.numeric(lapply(2*((1:1e3*K)/1e3),call_opt)),col=I("red"),xlab = "Price of unerlying",ylab="Profit of Short Call")+geom_hline(yintercept=0,col="blue")+theme_bw()
            g3 <- qplot(2*((1:1e3*K)/1e3),as.numeric(lapply(2*((1:1e3*K)/1e3),put_opt)),col=I("red"),xlab = "Price of unerlying",ylab="Profit of Long Put")+geom_hline(yintercept=0,col="blue")+theme_bw()
            g4 <- qplot(2*((1:1e3*K)/1e3),-as.numeric(lapply(2*((1:1e3*K)/1e3),put_opt)),col=I("red"),xlab = "Price of unerlying",ylab="Profit of Short Put")+geom_hline(yintercept=0,col="blue")+theme_bw()
            grid.arrange(g1,g2,g3,g4,ncol=2,nrow=2)
            }) 
            output$BS_price <- renderPrint({
                 K <- input$K
                 t <- input$t
                 r <- input$r
                 St <- input$St
                 C <- BS_call(input$St,input$K,input$r,input$sigma,input$t)
                 P <- BS_put(input$St,input$K,input$r,input$sigma,input$t)
                 cbind(C,P)
                })
            output$Call_price <- renderPrint({
                K <- input$K
                t <- input$t
                r <- input$r
                St <- input$St
                C <- BS_call(input$St,input$K,input$r,input$sigma,input$t)
                P <- BS_put(input$St,input$K,input$r,input$sigma,input$t)
                C   
            })
            output$K_discounted <- renderPrint({
                 K <- input$K;  t <- input$t;  r <- input$r
                 K*exp(-r*t)
            })
            output$Put_price <- renderPrint({
                K <- input$K
                t <- input$t
                r <- input$r
                St <- input$St
                BS_put(input$St,input$K,input$r,input$sigma,input$t)
             })
            output$Stock_price <- renderPrint({
                St <- input$St
                St
            })
            output$Call_plus_K <- renderPrint({
                K <- input$K
                t <- input$t
                r <- input$r
                St <- input$St
                C <- BS_call(input$St,input$K,input$r,input$sigma,input$t)
                P <- BS_put(input$St,input$K,input$r,input$sigma,input$t)
                C + K*exp(-r*t)
            })
            output$Put_plus_St <- renderPrint({
                K <- input$K
                t <- input$t
                r <- input$r
                St <- input$St
                C <- BS_call(input$St,input$K,input$r,input$sigma,input$t)
                P <- BS_put(input$St,input$K,input$r,input$sigma,input$t)
                P + St
            })
            
            output$sensitivitySt_K <- renderPlot({
                K <- input$K
                r <- input$r
                St <- input$St
                sigma <- input$sigma
                t <- input$t
             
            g7 <- qplot(2*((1:1e3*St)/1e3),as.numeric(BS_call((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Price of the underlying stock", ylab = "Call option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
            g8 <- qplot(2*((1:1e3*St)/1e3),as.numeric(BS_put((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Price of the underlying stock", ylab = "Put option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
            g9 <- qplot(2*((1:1e3*K)/1e3),as.numeric(BS_call(St,(2*((1:1e3*K)/1e3)),r,sigma,t)),col=I("red"),xlab = "Strike price", ylab = "Call option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
            g10 <- qplot(2*((1:1e3*K)/1e3),as.numeric(BS_put(St,(2*((1:1e3*K)/1e3)),r,sigma,t)),col=I("red"),xlab = "Strike price", ylab = "Put option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
            grid.arrange(g7,g8,g9,g10,ncol=2,nrow=2)
            })
            output$sensitivitySigma_r_t <- renderPlot({
                K <- input$K
                r <- input$r
                St <- input$St
                sigma <- input$sigma
                t <- input$t
                C <- BS_call(input$St,input$K,input$r,input$sigma,input$t)
                P <- BS_put(input$St,input$K,input$r,input$sigma,input$t)
                g1 <- qplot(2*((1:1e3*sigma)/1e3),as.numeric(BS_call(St,K,r,(2*((1:1e3*sigma)/1e3)),t)),col=I("red"),xlab = "Volatility", ylab = "Call option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g2 <- qplot(2*((1:1e3*sigma)/1e3),as.numeric(BS_put(St,K,r,(2*((1:1e3*sigma)/1e3)),t)),col=I("red"),xlab = "Volatility", ylab = "Put option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g3 <- qplot(2*((1:1e3*r)/1e3),as.numeric(BS_call(St,K,(2*((1:1e3*r)/1e3)),sigma,t)),col=I("red"),xlab = "Interest rate", ylab = "Call option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g4 <- qplot(2*((1:1e3*r)/1e3),as.numeric(BS_put(St,K,(2*((1:1e3*r)/1e3)),sigma,t)),col=I("red"),xlab = "Interest rate", ylab = "Put option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g5 <- qplot(2*((1:1e3*t)/1e3),as.numeric(BS_call(St,K,sigma,(2*((1:1e3*t)/1e3)),t)),col=I("red"),xlab = "Time to expiration", ylab = "Call option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g6 <- qplot(2*((1:1e3*t)/1e3),as.numeric(BS_put(St,K,sigma,(2*((1:1e3*t)/1e3)),t)),col=I("red"),xlab = "Time to expiration", ylab = "Put option price" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                grid.arrange(g1,g2,g3,g4,g5,g6,ncol=2,nrow=3)
            })
            output$GreeksI <- renderPlot({
                K <- input$K
                r <- input$r
                St <- input$St
                sigma <- input$sigma
                t <- input$t

                g11 <- qplot(2*((1:1e3*St)/1e3),as.numeric(delta_call((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Delta of a call option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g12 <- qplot(2*((1:1e3*St)/1e3),as.numeric(delta_put((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Delta of a put option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g13 <- qplot(2*((1:1e3*St)/1e3),as.numeric(gamma_option((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Gamma of an option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g14 <- qplot(2*((1:1e3*St)/1e3),as.numeric(vega_option((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Vega of an option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                grid.arrange(g11,g12,g13,g14,ncol=2,nrow=2)
                 })
            output$GreeksII <- renderPlot({
                K <- input$K
                r <- input$r
                St <- input$St
                sigma <- input$sigma
                t <- input$t
                
                g15 <- qplot(2*((1:1e3*St)/1e3),as.numeric(rho_call((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Rho of a call option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g16 <- qplot(2*((1:1e3*St)/1e3),as.numeric(rho_put((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Rho of a put option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g17 <- qplot(2*((1:1e3*St)/1e3),as.numeric(theta_call((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Theta of an call option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                g18 <- qplot(2*((1:1e3*St)/1e3),as.numeric(theta_put((2*((1:1e3*St)/1e3)),K,r,sigma,t)),col=I("red"),xlab = "Stock price", ylab = "Theta of an put option" )+geom_hline(yintercept=0,col="blue")+theme_bw()
                grid.arrange(g15,g16,g17,g18,ncol=2,nrow=4)
                
            })
            
            }
)