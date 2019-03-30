library(shiny)
library(shinythemes)
library(xts)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)


# From AIR to EIR calculator
cal_irr <- function(cf0, cf1, duration) {
  
  cashflow.data <- c(cf0, rep(cf1, duration))
  
  NPV <- function(C, r) {
    sum(C / (1 + r) ^ (seq(along = C) - 1))
  }
  
  IRR <- function(C) {
    uniroot(NPV, c(0, 1), tol = .0000001, C = C)$root
  }
  
  out <- IRR(cashflow.data)
  
  return(out)
  
} 

loan.calculation <- function(loan.amount, loan.duration.yr, interest.rate.yr,
                             loan.start.date, enable.simulation.mode, range, seq.yr.gap,
                             method = c("AIR", "EIR")) {
  
  # Function to compute monthly loan payment
  monthly.loan.payment <- function(loan.amount, interest.rate.yr, loan.duration.yr, method = method){
    
    
    switch(method, AIR = {
      
      total.interest <- (interest.rate.yr / 100) * loan.amount * loan.duration.yr
      temp <- (loan.amount + total.interest) / (loan.duration.yr * 12)
      
    }, EIR = {
      
      interest.rate.mth <- (interest.rate.yr / 100) / 12
      loan.duration.mth <- loan.duration.yr * 12
      
      temp <- (((1 + interest.rate.mth) ^ loan.duration.mth) - 1) / 
        (interest.rate.mth*(1 + interest.rate.mth) ^ loan.duration.mth)
      
      temp <- loan.amount / temp
      
    })
    
    return(temp)
    
  }
  
  # Function to create amortization table
  amortization.table <- function(loan.amount, loan.duration.yr, monthly.loan.payment, interest.rate.yr,
                                 loan.start.date, method = method) {
    
    interest.rate.mth <- (interest.rate.yr / 100) / 12
    loan.duration.mth <- loan.duration.yr * 12
    loan.date.seq <- seq.Date(from = as.Date(loan.start.date), by = "month", length.out = (loan.duration.mth))
    
    # Set up matrix with first row
    dummy.mat <- matrix(double(), (loan.duration.mth), 8)
    colnames(dummy.mat) <- c("LoanBalance", "Interest", "Principal", "Payment",
                             "CumSum", "IntCumSum", "Perc.Loan.Balance", "Perc.Loan.Paid")
    
    dummy.mat[,"Payment"] <- monthly.loan.payment(loan.amount, interest.rate.yr, loan.duration.yr, method = method)
    dummy.mat[1,"LoanBalance"] <- loan.amount
    
    if (method == "AIR") {
      
      irr.mth <- cal_irr(-loan.amount, dummy.mat[1,"Payment"], loan.duration.mth)
      dummy.mat[1,"Interest"] <-  dummy.mat[1,"LoanBalance"] * irr.mth
      
    } else {
      
      dummy.mat[1,"Interest"] <-  interest.rate.mth * loan.amount
      
    }
    
    dummy.mat[1,"Principal"] <- dummy.mat[1,"Payment"] - dummy.mat[1,"Interest"]
    
    
    # Fill up the matrix
    for (i in 2:nrow(dummy.mat)) {
      
      dummy.mat[i,"LoanBalance"] <- dummy.mat[i - 1,1] - dummy.mat[i - 1, "Principal"]
      
      if (method == "AIR") {
        
        dummy.mat[i,"Interest"] <-  dummy.mat[i,"LoanBalance"] * irr.mth
        dummy.mat[i,"Principal"] <- dummy.mat[i,"Payment"] - dummy.mat[i,"Interest"]
        
        
      } else {
        
        dummy.mat[i,"Interest"] <- interest.rate.mth * dummy.mat[i,"LoanBalance"]
        dummy.mat[i,"Principal"] <- dummy.mat[i,"Payment"] - dummy.mat[i,"Interest"]
        
      }
      
    }
    
    dummy.mat[,"CumSum"] <- cumsum(dummy.mat[,"Payment"]) 
    dummy.mat[,"Perc.Loan.Balance"] <- (dummy.mat[,"LoanBalance"] / loan.amount) * 100
    dummy.mat[,"Perc.Loan.Paid"] <- (dummy.mat[,"CumSum"] / loan.amount) * 100
    dummy.mat[,"IntCumSum"] <- (cumsum(dummy.mat[,"Interest"]) / loan.amount) * 100
    
    dummy.mat[,1:6] <- round(dummy.mat[,1:6], 2)
    dummy.mat[,7:8] <- round(dummy.mat[,7:8], 2)
    colnames(dummy.mat) <- c("Loan Balance", "Interest", "Principal", "Payment", "Sum.Cumulative", "Perc.Loan.Int.",  
                             "Perc.Loan.Left", "Perc.Loan.Paid")
    
    amort.table <- as.xts(dummy.mat, order.by = loan.date.seq)
    return(amort.table)
    
  }
  
  # Variable loan duration for simulation
  if (enable.simulation.mode) {
    
    loan.duration.yr <- seq(from = range[1], to = range[2], by = seq.yr.gap)
    
  } 
  
  # Variable CPF contributions
  loan.date.seq <- list()
  mortgage.amort.table <- list()
  mortage.mth.payment <- first.downpay.interest <- second.downpay.interest <- vector("numeric")
  for (i in 1:length(loan.duration.yr)) {
    
    # Calculate monthly payment for each duration period
    mortage.mth.payment <- monthly.loan.payment(loan.amount, interest.rate.yr, loan.duration.yr[i], method = method)
    
    # Create mortage amortization table for each duration period
    mortgage.amort.table[[i]] <- amortization.table(loan.amount, loan.duration.yr[i], mortage.mth.payment, 
                                                    interest.rate.yr, loan.start.date, method = method)
    
  }
  
  return(mortgage.amort.table)
  
}

summary.output <- function(loan.data, method = c("AIR", "EIR")) {
  
  mth.payment <- last(loan.data[,"Payment"])
  total.paid <- last(loan.data[1:nrow(loan.data),"Sum.Cumulative"])
  tota.paid.perc <- last(loan.data[,"Perc.Loan.Paid"])
  total.interest <- sum(loan.data[,"Interest"])
  
  switch(method, AIR = {
    
    irr.mth <- cal_irr(-loan.data[1,"Loan.Balance"], loan.data[1,"Payment"], nrow(loan.data)) * 12 * 100
    irr.mth <- round(irr.mth, 2)
    out <- as.data.frame(cbind(mth.payment, total.paid, total.interest, tota.paid.perc, irr.mth))
    colnames(out) <- c("Monthly Payment", "Total Loan Cost", "Total Interest", 
                       "Total Loan Cost (%)", "Equivalent EIR (%)")
    
  }, EIR = {
    
    out <- as.data.frame(cbind(mth.payment, total.paid, total.interest, tota.paid.perc))
    colnames(out) <- c("Monthly Payment", "Total Loan Cost", "Total Interest", "Total Loan Cost (%)")
    
  })
  
  return(out)
  
}

summary.output.simu <- function(loan.data, range, seq.yr.gap) {
  
  loan.duration.yr <- seq(from = range[1], to = range[2], by = seq.yr.gap)
  
  dummy.mat <- matrix(double(), length(loan.duration.yr), 5)
  for (i in 1:nrow(dummy.mat)) {
    
    dummy.mat[i, 1] <- loan.duration.yr[i]
    dummy.mat[i, 2] <- loan.data[[i]][1,"Payment"]
    dummy.mat[i, 3] <- last(loan.data[[i]][1:nrow(loan.data[[i]]),"Sum.Cumulative"])
    dummy.mat[i, 4] <- round(sum(loan.data[[i]][,"Interest"]))
    dummy.mat[i, 5] <- loan.data[[i]][nrow(loan.data[[i]]),"Perc.Loan.Paid"]
    
  }
  
  out <- round(as.data.frame(dummy.mat))
  
  colnames(out) <- c("Loan Duration", "Loan Mth Payment", "Total Loan Amount", "Total Interest", 
                     "Total Loan Cost (%)")
  
  return(out)
  
}

# Define UI for application that draws a histogram
shinyApp(
  
  ui <- tagList(
    
    #shinythemes::themeSelector(),
    navbarPage(
      theme = shinytheme("simplex"),
      "Loan Calculator & Simulator",
      tabPanel("Loan Calculation",
               shinyjs::useShinyjs(),
               sidebarPanel(
                 numericInput("loan.amount",
                              h3("Loan Amount"),
                              value = 100000),
                 
                 selectInput("interest.method", 
                             h3("Interest Type"), 
                             choices = list("Effective Interest Rate (EIR)" = 1,
                                            "Assumed Interest Rate (AIR)" = 2), 
                             selected = 1),
                 
                 numericInput("interest.rate.yr",
                              h3("Interest Rate (%)"),
                              value = 2.5),
                 
                 dateInput("loan.start.date",
                           h3("Loan Start Date"),
                           value = "2019-01-01"),
                 
                 selectInput("loan.duration", 
                             h3("Loan Duration Type"), 
                             choices = list("Year" = 1, "Month" = 2), 
                             selected = 1),
                 
                 sliderInput("loan.duration.yr",
                             h3("Loan Duration (Year)"),
                             min = 1, max = 40, value = 20),
                 
                 sliderInput("loan.duration.mth",
                             h3("Loan Duration (Month)"),
                             min = 2, max = 120, value = 20)
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Loan Plot",
                            DT::dataTableOutput("summary"), plotOutput("plot.curves"), plotOutput("plot.bar")),
                   tabPanel("Amortization Table",
                            DT::dataTableOutput("loan.calculation"))
                   
                 )
               )
      ),
      tabPanel("Loan Simulation",
               shinyjs::useShinyjs(),
               sidebarPanel(
                 numericInput("loan.amount.simu",
                              h3("Loan Amount"),
                              value = 100000),
                 
                 selectInput("interest.method.simu", 
                             h3("Interest Type"), 
                             choices = list("Effective Interest Rate (EIR)" = 1,
                                            "Assumed Interest Rate (AIR)" = 2), 
                             selected = 1),
                 
                 numericInput("interest.rate.yr.simu",
                              h3("Interest Rate (%)"),
                              value = 2.5, max = 100),
                 
                 dateInput("loan.start.date.simu",
                           h3("Loan Start Date"),
                           value = "2019-01-01"),
                 
                 sliderInput("loan.duration.yr.simu",
                             h3("Loan Simulation Duration (Year)"),
                             min = 1, max = 40, value = c(1, 20)),
                 
                 sliderInput("seq.yr.gap",
                             h3("Simulation Gap"),
                             min = 1, max = 10, value = 1)
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Loan Plot",
                            plotOutput("plot.curves.simu", height = "600")),
                   tabPanel("Summary Simulation", 
                            DT::dataTableOutput("summary.simu"))
                 )
               ))
    )
  ),
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    
    observeEvent(input$loan.duration, {
      
      if (input$loan.duration == 1) {
        
        shinyjs::enable("loan.duration.yr")
        shinyjs::disable("loan.duration.mth")
        
      } else {
        
        shinyjs::disable("loan.duration.yr")
        shinyjs::enable("loan.duration.mth")
        
      }
    })
    
    observeEvent(input$loan.duration.simu, {
      
      if (input$loan.duration.simu == 1) {
        
        shinyjs::enable("loan.duration.yr.simu")
        shinyjs::disable("loan.duration.mth.simu")
        
      } else {
        
        shinyjs::disable("loan.duration.yr.simu")
        shinyjs::enable("loan.duration.mth.simu")
        
      }
    })
    
    loan.data <- reactive({
      
      
      loan.amount <- input$loan.amount
      interest.rate.yr <- input$interest.rate.yr
      loan.start.date <- input$loan.start.date
      
      if (input$loan.duration == 1) {
        
        loan.duration.yr <- input$loan.duration.yr
        
      } else {
        
        loan.duration.yr <- (input$loan.duration.mth / 12)
        
      }
      
      if (input$interest.method == 1) {
        
        method <- "EIR"
        
      } else {
        
        method <- "AIR"
        
      }
      
      as.data.frame(loan.calculation(loan.amount, loan.duration.yr, interest.rate.yr,
                                     as.Date(loan.start.date), enable.simulation.mode = F, range = 0, seq.yr.gap = 1,
                                     method = method))
      
    })
    
    output$loan.calculation <- DT::renderDataTable({
      
      datatable(loan.data())
      
    })
    
    output$summary <- DT::renderDataTable({
      
      
      if (input$interest.method == 1) {
        
        method <- "EIR"
        
      } else {
        
        method <- "AIR"
        
      }
      
      datatable(summary.output(loan.data(), method = method), options = list(dom = "t"), rownames = FALSE)
      
    })
    
    output$plot.curves <- renderPlot({
      
      
      loan.df <- loan.data()
      table.dates <- as.Date(rownames(loan.df))
      loan.df <- cbind(table.dates, loan.df)
      
      breaks.range <- as.numeric(format.Date(range(loan.df[,"table.dates"]), "%Y"))
      
      loan.df <- loan.df %>%
        select(table.dates, contains("Perc.Loan.Left"), contains("Perc.Loan.Paid"), contains("Perc.Loan.Int")) %>%
        gather(key = "variable", value = "value", -table.dates)
      
      p <- ggplot(loan.df, aes(x = table.dates, y = value)) +
        geom_line(aes(color = variable), size = 1) + 
        labs(title = "% Loan Balance Vs % Total Loan Paid", x = element_blank(), y = "% Loan") +
        theme_gray(base_size = 15) +
        theme(legend.title = element_blank(), legend.position = "bottom") + 
        scale_color_manual(labels = c("% Loan Interest", "% Loan Left", "% Loan Paid"),
                           values = c("maroon", "skyblue", "khaki3"))
      
      if (input$loan.duration == 1) {
        
        breaks.range <- (breaks.range[2] - breaks.range[1]) / 10
        breaks.range <- ifelse(ceiling(breaks.range) > 0, ceiling(breaks.range), 1)
        breaks.range <- paste0(breaks.range, " years")
        p + scale_x_date(date_breaks = breaks.range, date_labels = "%Y")
        
      } else {
        
        breaks.range <- (breaks.range[2] - breaks.range[1]) / 0.4
        breaks.range <- ifelse(ceiling(breaks.range) > 0, ceiling(breaks.range), 1)
        breaks.range <- paste0(breaks.range, " months")
        p + scale_x_date(date_breaks = breaks.range, date_labels = "%Y-%m")
        
      }
      
    })
    
    output$plot.bar <- renderPlot({
      
      loan.df <- as.xts(loan.data())
      
      if (input$loan.duration == 1) {
        
        loan.df <- to.yearly(loan.df, OHLC = F)
        
      }
      
      loan.df <- as.data.frame(loan.df)
      table.dates <- as.Date(rownames(loan.df))
      loan.df <- cbind(table.dates, loan.df)
      
      breaks.range <- as.numeric(format.Date(range(loan.df[,"table.dates"]), "%Y"))
      
      loan.df <- loan.df %>%
        select(table.dates, contains("Interest"), contains("Principal")) %>%
        gather(key = "variable", value = "value", -table.dates)
      
      p <- ggplot(loan.df, aes(x = table.dates, y = value, fill = factor(variable))) +
        labs(title = "Yearly Interest & Principal", x = element_blank(), y = "$") +
        geom_bar(stat = "identity", position = "dodge") + 
        theme_gray(base_size = 15) +
        theme(legend.title = element_blank(), legend.position = "bottom")
      
      
      if (input$loan.duration == 1) {
        
        breaks.range <- (breaks.range[2] - breaks.range[1]) / 10
        breaks.range <- ifelse(ceiling(breaks.range) > 0, ceiling(breaks.range), 1)
        breaks.range <- paste0(breaks.range, " years")
        p + scale_x_date(date_breaks = breaks.range, date_labels = "%Y")
        
      } else {
        
        breaks.range <- (breaks.range[2] - breaks.range[1]) / 0.4
        breaks.range <- ifelse(ceiling(breaks.range) > 0, ceiling(breaks.range), 1)
        breaks.range <- paste0(breaks.range, " months")
        p + scale_x_date(date_breaks = breaks.range, date_labels = "%Y-%m")
        
      }
      
    })
    
    loan.data.simu <- reactive({
      
      
      loan.amount <- input$loan.amount.simu
      interest.rate.yr <- input$interest.rate.yr.simu
      range <- input$loan.duration.yr.simu
      loan.start.date <- input$loan.start.date.simu
      seq.yr.gap <- input$seq.yr.gap
      
      if (input$interest.method.simu == 1) {
        
        method <- "EIR"
        
      } else {
        
        method <- "AIR"
        
      }
      
      loan.calculation(loan.amount, loan.duration.yr = 0, interest.rate.yr,
                       as.Date(loan.start.date), enable.simulation.mode = T,
                       range = range, seq.yr.gap = seq.yr.gap, method = method)
      
      
    })
    
    output$summary.simu <- DT::renderDataTable({
      
      datatable(summary.output.simu(loan.data.simu(), input$loan.duration.yr.simu, input$seq.yr.gap),
                rownames = FALSE)
      
    })
    
    output$plot.curves.simu <- renderPlot({
      
      seq.yr.gap <- input$seq.yr.gap
      range <- input$loan.duration.yr.simu
      
      loan.data.list <- loan.data.simu()
      simulation.table.df <- as.data.frame(do.call(merge.xts, loan.data.list))
      table.dates <- index(loan.data.list[[(length(loan.data.list))]])
      simulation.df <- cbind(table.dates, simulation.table.df)
      
      labels.loan.balance <- paste(as.character(seq(range[1], range[2], seq.yr.gap)), "yr %Left", sep = "")
      labels.loan.paid <- paste(as.character(seq(range[1], range[2], seq.yr.gap)), "yr %Paid", sep = "")
      
      breaks.range <- as.numeric(format.Date(range(table.dates), "%Y"))
      
      simulation.df <- simulation.df %>%
        select(table.dates, contains("Perc.Loan.Left"), contains("Perc.Loan.Paid")) %>%
        gather(key = "variable", value = "value", -table.dates)
      
      p <- ggplot(simulation.df, aes(x = table.dates, y = value)) +
        geom_line(aes(color = variable), size = 1) + 
        theme_gray(base_size = 15) +
        theme(legend.title = element_blank(), legend.position = "bottom") + 
        labs(title = "% Loan Balance Vs % Total Loan Paid", x = "Dates", y = "% Loan") +
        scale_y_continuous(breaks = seq(0, 200, 10))
      
      p + scale_colour_discrete(labels = c(labels.loan.balance, labels.loan.paid))
      
    })
    
  }
  
)