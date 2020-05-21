

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyUI(
        navbarPage("",
                   tabPanel(
                       "European option",
                       sidebarPanel(
                           
                           radioButtons("typ_e",
                                        "Type of option:",
                                        choices = list("Call" = 0, "Put" = 1),
                                        selected = 0
                           ),
                           sliderInput("S0_e",
                                       "Current stock price:",
                                       min = 1,
                                       max = 200,
                                       value = 50,
                                       animate = T
                           ),
                           sliderInput("K_e",
                                       "Strike price:",
                                       min = 1, 
                                       max = 400,
                                       value = 48,
                                       animate = T
                           ),
                           sliderInput("T_e",
                                       "Time to expiration:",
                                       min = 0.5,
                                       max = 5,
                                       value = 2,
                                       step = 0.25,
                                       animate = T
                           ),
                           sliderInput("sigma_e",
                                       "Volatility:",
                                       min = 0.01,
                                       max = 0.6,
                                       value = 0.3,
                                       animate = T
                           ),
                           sliderInput("r_e",
                                       "Risk-free interest rate:",
                                       min = 0,
                                       max = 0.5,
                                       value = 0.02,
                                       animate = T
                           ),
                           sliderInput("zakres_e",
                                       "Share price range:",
                                       min = 0, max = 1000,
                                       value = c(0, 500)
                           )
                           
                       ),
                       mainPanel(
                           plotOutput("plot_e" ,  width = "100%", height = "200%"),
                           h3(htmlOutput("text_e"))
                       )
                       
                   ),
                   
                   tabPanel("American option",
                            
                            sidebarPanel(
                                radioButtons("typ_a",
                                             "Type of option",
                                             choices = list("Call" = 0, "Put" = 1),
                                             selected = 0
                                ),
                                
                                sliderInput("S0_a",
                                            "Current stock price:",
                                            min = 1,
                                            max = 200,
                                            value = 50,
                                            animate = T
                                ),
                                sliderInput("K_a",
                                            "Strike price:",
                                            min = 1, 
                                            max = 400,
                                            value = 48,
                                            animate = T
                                ),
                                sliderInput("T_a",
                                            "Time to expiration:",
                                            min = 0.5,
                                            max = 5,
                                            value = 2,
                                            step = 0.25,
                                            animate = T
                                ),
                                sliderInput("sigma_a",
                                            "Volatility:",
                                            min = 0.01,
                                            max = 0.6,
                                            value = 0.3,
                                            animate = T
                                ),
                                sliderInput("r_a",
                                            "Riske-free interest rate",
                                            min = 0,
                                            max = 0.5,
                                            value = 0.02,
                                            animate = T
                                ),
                                sliderInput("zakres_a",
                                            "Share price range:",
                                            min = 0, max = 1000,
                                            value = c(0, 500)
                                )
                                
                            ),
                            mainPanel(
                                plotOutput("plot_a",  width = "100%", height = "200%"),
                                h3(htmlOutput("text_a"))
                            )
                   ),
                   
                   tabPanel("Comparsion",
                            sidebarPanel(
                                radioButtons("typ_po",
                                             "Rodzaj opcji",
                                             choices = list("Call" = 0, "Put" = 1),
                                             selected = 0
                                ),
                                
                                sliderInput("S0_po",
                                            "Current stock price:",
                                            min = 1,
                                            max = 200,
                                            value = 50,
                                            animate = T
                                ),
                                sliderInput("K_po",
                                            "Strike price:",
                                            min = 1, 
                                            max = 400,
                                            value = 48,
                                            animate = T
                                ),
                                sliderInput("T_po",
                                            "Time to expiration:",
                                            min = 0.5,
                                            max = 5,
                                            value = 2,
                                            step = 0.25,
                                            animate = T
                                ),
                                sliderInput("sigma_po",
                                            "Volatility:",
                                            min = 0.01,
                                            max = 0.6,
                                            value = 0.3,
                                            animate = T
                                ),
                                sliderInput("r_po",
                                            "Riske-free interest rate",
                                            min = 0,
                                            max = 0.5,
                                            value = 0.02,
                                            animate = T
                                ),
                                sliderInput("zakres_po",
                                            "Share price range:",
                                            min = 0, max = 1000,
                                            value = c(0, 500)
                                )
                                
                            ),
                            mainPanel(
                                plotOutput("plot_po",  width = "100%", height = "200%"),
                                h4(htmlOutput("text_po_e")),
                                h4(htmlOutput("text_po_a"))
                            )
                            
                   ),
                   tabPanel("Early execution",
                            sidebarPanel(
                                
                                sliderInput("S0_wc",
                                            "Current stock price:",
                                            min = 1,
                                            max = 200,
                                            value = 50,
                                            animate = T
                                ),
                                sliderInput("K_wc",
                                            "Strike price:",
                                            min = 1, 
                                            max = 400,
                                            value = 48,
                                            animate = T
                                ),
                                sliderInput("T_wc",
                                            "Time to expiration:",
                                            min = 0.5,
                                            max = 5,
                                            value = 2,
                                            step = 0.25,
                                            animate = T
                                ),
                                sliderInput("sigma_wc",
                                            "Volatility:",
                                            min = 0.01,
                                            max = 0.6,
                                            value = 0.3,
                                            animate = T
                                ),
                                sliderInput("r_wc",
                                            "Riske-free interest rate",
                                            min = 0,
                                            max = 0.5,
                                            value = 0.02,
                                            animate = T
                                )
                                
                            ),
                            mainPanel(
                                plotOutput("plot_wc")
                            )
                            
                   )
        )
    )
    
    
)

server <- function(input, output) {
    
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    
    
    cenaakcji<-function(So,sigma,t,dt){
        kroki=t/dt
        u=exp(sigma*sqrt(dt))
        d=1/u
        wynik=c(So)
        rzad=wynik
        for (i in 1:kroki){
            rzad=c(rzad,tail(rzad,1))*c(rep(u,i),d)
            wynik=c(wynik,rzad)
        }
        wynik
    }
    
    europejskawycena = function(So, K, r, sigma, t, dt, typ) {
        kroki = t / dt
        u = exp(sigma * sqrt(dt))
        d = 1 / u
        p = (exp(r * dt) - d) / (u - d)
        akcja = cenaakcji(So, sigma, t, dt)
        ST = tail(akcja, (kroki + 1))
        if (typ == 0) {
            ft=pmax(0, ST - K)
        }
        if (typ == 1) {
            ft = pmax(0, K - ST)
        }
        rzad = ft
        for (i in 1 : kroki) {
            rzad = exp(-r * dt) * (p * rzad[1 : (length(rzad) - 1)] + (1 - p) * rzad[2 : length(rzad)])
            ft = c(rzad, ft)
        }
        czas = rep(seq(0, t, by = dt), seq(1, (kroki + 1)))
        wynik = data.frame(Czas = round(czas, 4), Cena_akcji = round(akcja, 4), Cena_opcji = round(ft, 4))
        
        wynik
    }
    
    
    amerykanskawycena<-function(So, K, r, sigma, t, dt, typ) {
        kroki = t / dt
        u = exp(sigma * sqrt(dt))
        d = 1 / u
        p = (exp(r * dt) - d) / (u - d)
        akcja = cenaakcji(So, sigma, t, dt)
        #ST = tail(akcja, (kroki + 1))
        
        if (typ == 0) {
            ft = pmax(0, akcja - K)
        }
        if (typ == 1) {
            ft = pmax(0, K - akcja)
        }
        rzad = tail(ft, kroki + 1)
        wycena = rzad
        for (i in 1 : kroki) {
            licznik = length(ft) - length(wycena)
            rzad = pmax(ft[(licznik - length(rzad) + 2) : licznik], exp(-r * dt) * (p * rzad[1 : (length(rzad) - 1)] + (1 - p) * rzad[2 : length(rzad)]))
            wycena = c(rzad, wycena)
        }
        czas = rep(seq(0, t, by = dt), seq(1, (kroki + 1)))
        wynik = data.frame(Czas = round(czas, 4),Cena_akcji = round(akcja, 4), Cena_opcji = round(wycena, 4))
        wynik
    }
    
    kiedywykonacwykres<-function(So,K,r,sigma,t,dt,typ){
        wyceny=amerykanskawycena(So,K,r,sigma,t,dt,typ)
        if (typ==0){
            warunek=wyceny$`Cena_opcji`==round(wyceny$`Cena_akcji`-K,4)
        }
        if (typ==1){
            warunek=wyceny$`Cena_opcji`==round(K-wyceny$`Cena_akcji`,4)
        }
        wynik=data.frame(Czas = wyceny$Czas, Cena_akcji = wyceny$Cena_akcji, czy_warto = warunek)
        wynik
    }
    
    
    output$plot_e = renderPlot({
        # generate bins based on input$bins from ui.R
        
        europejskawycena(input$S0_e, input$K_e, input$r_e, input$sigma_e, input$T_e, 1 / 12, input$typ_e) %>%
            filter(between(Cena_akcji, input$zakres_e[1], input$zakres_e[2])) %>%
            ggplot(aes(x = Czas, y = Cena_akcji))+
            geom_point(aes(color = Cena_opcji))+
            scale_color_gradientn(colours = c("black","green","yellow","orange","darkorange","brown","darkred"),
                                  values = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
            labs(title = "European option price ", y = "Share price", color = "Option price", x = "Time" )+
            theme(plot.title = element_text(hjust = 0.5))
        # # draw the histogram with the specified number of bins
        
    }, width = 1024, height = 720)
    
    output$text_e = renderPrint({
        HTML(paste0("Current option price: ", "<b>", round(europejskawycena(input$S0_e, input$K_e, input$r_e, input$sigma_e, input$T_e, 1 / 12, input$typ_e)[1, 3], 2) , "</b>"))
    })
    
    output$plot_a = renderPlot({
        
        amerykanskawycena(input$S0_a, input$K_a, input$r_a, input$sigma_a, input$T_a, 1 / 12, input$typ_a) %>%
            filter(between(Cena_akcji, input$zakres_a[1], input$zakres_a[2])) %>%
            ggplot( aes(x = Czas, y =Cena_akcji))+
            geom_point(aes(color = Cena_opcji))+
            scale_color_gradientn(colours = c("black","green","yellow","orange","darkorange","brown","darkred"),
                                  values = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
            labs(title = "American option price ", y = "Share price", color = "Option price", x = "Time" )+
            theme(plot.title = element_text(hjust = 0.5))
    }, width = 1024, height = 720)
    
    output$text_a = renderPrint({
        HTML(paste0("Current option price: ", "<b>", round(amerykanskawycena(input$S0_a, input$K_a, input$r_a, input$sigma_a, input$T_a, 1 / 12, input$typ_a)[1, 3], 2) , "</b>")) 
    })
    
    output$plot_po = renderPlot({
        
        am = amerykanskawycena(input$S0_po, input$K_po, input$r_po, input$sigma_po, input$T_po, 1 / 12, input$typ_po) %>%
            filter(between(Cena_akcji, input$zakres_po[1], input$zakres_po[2])) 
        eu = europejskawycena(input$S0_po, input$K_po, input$r_po, input$sigma_po, input$T_po, 1 / 12, input$typ_po) %>%
            filter(between(Cena_akcji, input$zakres_po[1], input$zakres_po[2])) 
        
        data.frame(Czas = am$Czas, Cena_akcji = am$Cena_akcji, European = eu$Cena_opcji, American = am$Cena_opcji) %>%
            gather("key", "value", -c(Czas, Cena_akcji)) %>%
            ggplot(aes(x = Czas, y = Cena_akcji))+
            geom_point(aes(color = value))+
            scale_color_gradientn(colours = c("black","green","yellow","orange","darkorange","brown","darkred"),
                                  values = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))+
            facet_wrap(~key)+
            labs(y = "Share price", color = "Option price", x = "Time")    
    }, width = 1024, height = 720)
    
    output$text_po_e = renderPrint({
        HTML(paste0("Current european option price: ", "<b>", round(europejskawycena(input$S0_po, input$K_po, input$r_po, input$sigma_po, input$T_po, 1 / 12, input$typ_po)[1, 3], 2) , "</b>")) 
        
    })
    
    output$text_po_a = renderPrint({
        HTML(paste0("Current american option price: ", "<b>", round(amerykanskawycena(input$S0_po, input$K_po, input$r_po, input$sigma_po, input$T_po, 1 / 12, input$typ_po)[1, 3], 2) , "</b>")) 
        
    })
    
    output$plot_wc = renderPlot({
        colors1 = c("green", "red")
        names(colors1) = c(T, F)
        kiedywykonacwykres(input$S0_wc, input$K_wc, input$r_wc, input$sigma_wc, input$T_wc, 1 / 12, 1) %>%
            ggplot( aes(x = Czas, y =Cena_akcji))+
            geom_point(aes(color = czy_warto))+
            scale_color_manual(values = colors1 )+
            scale_size_area()+
            labs(title = "Profitability of early execution of the American put options", y = "Share price", color = "Is it worth it", x = "Time" )+
            theme(plot.title = element_text(hjust = 0.5))
    }, width = 1024, height = 720)
}

# Run the application 
shinyApp(ui = ui, server = server)
