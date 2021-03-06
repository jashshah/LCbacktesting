library(shiny)
source('functions.R')
mydata <- read_csv('mydata.csv')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  md_b <- reactive({dataframe_process(mydata = mydata, grade_ = 'B', cutoff = input$cutoff_b, recovery = input$recovery)})
  
  md_c <- reactive({dataframe_process(mydata = mydata, grade_ = 'C', cutoff = input$cutoff_c, recovery = input$recovery)})
  
  md_d <- reactive({dataframe_process(mydata = mydata, grade_ = 'D', cutoff = input$cutoff_d, recovery = input$recovery)})
  
  md_df <- reactive({rbind(md_b(), md_c(), md_d())})
  
  qtr_choices <- reactive({unique(md_df()$quarter_year)})

  # rv <- reactiveValues()
  # rv$qtr_choices <- qtr_choices()

  output$quarter <- renderUI({
    
    selectInput(inputId = 'quarter', 
                label = 'Quarter', 
                choices = qtr_choices())
    })
  
  output$confusion_matrix <- renderPrint({qtr_conf_mat(md_df(), quarter_year_ = input$quarter)})
  
  output$acc <- renderPrint({cat(qtr_imp_metrics(md_df(), quarter_year_ = input$quarter)$accuracy)})
  
  output$prec <- renderPrint({cat(qtr_imp_metrics(md_df(), quarter_year_ = input$quarter)$precision)})
  
  output$rec <- renderPrint({cat(qtr_imp_metrics(md_df(), quarter_year_ = input$quarter)$recall)})
  
  output$grade_mix <- renderPrint({qtr_grade_mix(md_df(), quarter_year_ = input$quarter)})
  
  output$prop_purch_loans <- renderPrint({qtr_grade_props(md_df(), quarter_year_ = input$quarter)})
  
  output$return_all <- renderPrint({cat(round(qtr_alpha(md_df(), quarter_year_ = input$quarter)$all_loans, 3))})
  
  output$return_purch <- renderPrint({cat(round(qtr_alpha(md_df(), quarter_year_ = input$quarter)$purchased_loans, 3))})
  
  alpha <- reactive(qtr_alpha(md_df(), quarter_year_ = input$quarter)$purchased_loans - qtr_alpha(md_df(), quarter_year_ = input$quarter)$all_loans)
  
  output$alpha <- renderPrint({cat(round(alpha(), 3))})
  
})
