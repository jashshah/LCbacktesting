library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Backtesting"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       numericInput(inputId = 'cutoff_b',
                    label = 'Grade B Percentile Cutoff',
                    min = 0,
                    max = 1,
                    value = 0.2, 
                    step = 0.05),
       numericInput(inputId = 'cutoff_c',
                    label = 'Grade C Percentile Cutoff',
                    min = 0,
                    max = 1,
                    value = 0.2, 
                    step = 0.05),
       numericInput(inputId = 'cutoff_d',
                    label = 'Grade D Percentile Cutoff',
                    min = 0,
                    max = 1,
                    value = 0.2, 
                    step = 0.05),
       numericInput(inputId = 'recovery',
                    label = 'Recovery Ratio',
                    min = 0,
                    max =  1,
                    value = 0.65, 
                    step = 0.05),
       uiOutput('quarter'),
       submitButton('Submit')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4('Confusion Matrix'),
      verbatimTextOutput("confusion_matrix"),
      h4('Accuracy'),
      verbatimTextOutput('acc'),
      h4('Precision'),
      verbatimTextOutput('prec'),
      h4('Recall'),
      verbatimTextOutput('rec'),
      h4('Grade Mix'),
      verbatimTextOutput('grade_mix'),
      h4('Purchased Loans Proportions (in %)'),
      verbatimTextOutput('prop_purch_loans'),
      h4('Returns: All Loans'),
      verbatimTextOutput('return_all'),
      h4('Returns: Purchased Loans'),
      verbatimTextOutput('return_purch'),
      h4('Alpha (Purchased Loans - All Loans)'),
      verbatimTextOutput('alpha')
    )
  )
))
