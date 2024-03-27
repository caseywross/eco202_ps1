library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)

# Define UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Scarf and Glove Production Calculator"),

  # Load shinyjs and add custom JavaScript to restrict input to digits 0-9 ----
  useShinyjs(),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input boxes for variables A, B, C, and D ----
      numericInput("input_A", "A", value = 0, min = 0, max = 9),
      numericInput("input_B", "B", value = 0, min = 0, max = 9),
      numericInput("input_C", "C", value = 0, min = 0, max = 9),
      numericInput("input_D", "D", value = 0, min = 0, max = 9)
    ),

    # Main panel for displaying output ----
    mainPanel(

      # Output: Production table ----
      tableOutput("production_table"),
      hr(),
      # Output: Production plot ----
      plotlyOutput("production_plot"),
      hr(),
      br(),

      h4("Opportunity Costs"),

      # Output: Text  ----
      tableOutput("answer_key")
    )
  ),

  # Custom JavaScript to restrict input to digits 0-9 ----
  tags$script("
    shinyjs.limitDigits = function() {
      $('.numericInput').keypress(function(e) {
        if (e.which != 8 && e.which != 0 && (e.which < 48 || e.which > 57)) {
          return false;
        }
      });
    }
    shinyjs.limitDigits();"
  )
)

# Define server logic ----
server <- function(input, output, session) {

  # Modify inputs: If input is 0, change it to 10 ----
  observeEvent(input$input_A, {
    req(input$input_A)

    if (input$input_A == 0) {
      updateNumericInput(session, "input_A", value = 10)
    }
  })

  observeEvent(input$input_B, {
    req(input$input_A)

    if (input$input_B == 0) {
      updateNumericInput(session, "input_B", value = 10)
    }
  })

  observeEvent(input$input_C, {
    req(input$input_C)

    if (input$input_C == 0) {
      updateNumericInput(session, "input_C", value = 10)
    }
  })

  observeEvent(input$input_D, {
    req(input$input_D)

    if (input$input_D == 0) {
      updateNumericInput(session, "input_D", value = 10)
    }
  })

  # Reactive Table that can be accessed by the other reactive functions ----

  data_tbl_reactive <- reactive({
    Amy_Scarves <- input$input_A
    Amy_Gloves <- input$input_B
    Bob_Scarves <- input$input_C
    Bob_Gloves <- input$input_D

    production_data <- data.frame(
      Worker = c("Amy", "Amy", "Bob", "Bob"),
      Product = c("Scarves", "Gloves", "Scarves", "Gloves"),
      Production = c(Amy_Scarves, Amy_Gloves, Bob_Scarves, Bob_Gloves)
    )
  })

  # Calculate production based on input values ----
  output$production_table <- renderTable({

    data_tbl_reactive() %>% pivot_wider(names_from = Product, values_from = Production)

  })

  # Create production plot based on input values ----
  output$production_plot <- renderPlotly({

    plot_data <- data_tbl_reactive() %>%
      mutate(scarves_prod = if_else(Product == "Scarves", Production, 0),
             gloves_prod = if_else(Product == "Gloves", Production, 0)) %>%
      select(-Product, -Production)

    # Create plotly line plot
    plot_ly(plot_data, x = ~scarves_prod, y = ~gloves_prod, color = ~Worker, type = 'scatter', mode = 'lines',
            line = list(shape = "linear")) %>%
      layout(title = "Production Curve",
             xaxis = list(title = "Scarves"),
             yaxis = list(title = "Gloves"))
  })


  output$answer_key <- renderTable({
    data_tbl_reactive() %>%
      pivot_wider(names_from = Product, values_from = Production) %>%
      mutate(`Scarves (in terms of Gloves)` = str_glue("{Gloves}/{Scarves} = {round(Gloves/Scarves, 3)}"),
             `Gloves (in terms of Scarves)` = str_glue("{Scarves}/{Gloves} = {round(Scarves/Gloves, 3)}")) %>%
      select(-Scarves, -Gloves)
  })
}

# Create Shiny app object ----
shinyApp(ui = ui, server = server)
