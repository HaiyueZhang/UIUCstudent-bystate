# load package
library(shiny)
library(tidyverse)


# load data
student_bystate = read_csv(file = "data/student_bystate.csv")
student_bystate_degree = read_csv(file = "data/student_bystate_degree.csv")

# define the order of category 
degree_order = c("Undergraduate", "Professional", "Graduate")

ui = navbarPage(
  title = "Where does UIUC student come from?",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "UIUC student's number of such degree from one State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "state", 
          label = "State:",
          choices = sort(unique(student_bystate_degree$State)),
          selected = "Illinois"),
        selectInput(
          inputId = "degree", 
          label = "Degree:",
          choices = sort(unique(student_bystate_degree$Degree)),
          selected = "Undergraduate")
      ),
      mainPanel(plotOutput("plot1"))
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "state1", 
          label = "State:",
          choices = sort(unique(student_bystate$State)),
          selected = "Illinois"),
        selectInput(
          inputId = "year", 
          label = "Year:",
          choices = sort(unique(student_bystate$Year)),
          selected = 2017),
          checkboxInput(inputId = "stateid", label = "Filter Table to State Name", value = FALSE)
        ),
        mainPanel(plotOutput("plot2"))
      )
    ),
  tabPanel(title = "Table1", dataTableOutput("table1")),
  tabPanel(title = "Table2", dataTableOutput("table2")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
)

server = function(input, output) {
  # Line chart shows difference on number of student in a state
    student_bystate_State_Degree = reactive({
      student_bystate_degree |>
        filter(State == input$state) |>
        filter(Degree == input$degree)
    })
    
    # Draw plot1
    output$plot1 = renderPlot({
      
      data = student_bystate_degree |>
        filter(State == input$state) |>
        filter(Degree == input$degree) |>
        select(Year, Count) |>
        arrange(Year) 
      
      ggplot(data, aes(x = Year, y = Count)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(color = "red", size = 3) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Plot1: Students with Specific Degree from one State by Years",
             x = "Year",
             y = "Number of Students")
    })
    
    # Table of plot1
    output$table1 = renderDataTable({
      student_bystate_State_Degree()
    })
    
    # Pie chart of distribution on different degree in each state, year
    student_bystate_state = reactive({
      student_bystate |>
        filter(State == input$state1) 
    })
    
    observeEvent(
      eventExpr = input$state1,
      handlerExpr = {
        updateSelectInput(inputId = "year",
                          choices = sort(unique(student_bystate_state()$Year)),
                          selected = max(unique(student_bystate_state()$Year)))
      }
    )
    
    student_bystate_year = reactive({
      student_bystate |>
        filter(Year == input$year)
    })
    
    # Draw plot2
    output$plot2 = renderPlot({
      
      degree_data = student_bystate |>
        filter(State == input$state1) |>
        filter(Year == input$year) |>
        pivot_longer(Undergraduate:Graduate, names_to = "Degree", values_to = "Count") |>
        select(Degree, Count) |>
        mutate(Degree = factor(Degree, levels = degree_order))
      
      ggplot(degree_data) +
        aes(x = "", y = Count, fill = Degree) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0("n = ", Count)),
                  position = position_stack(vjust = 0.5),
                  color = "white",
                  fontface = "bold") +
        theme_void() +
        theme(legend.position = "right",
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Plot2: Students' Degree Distribution in one State within specific year",
             fill = "Degree")
    })
    
    # Table of plot2
    output$table2 = renderDataTable({
      tab = student_bystate_year() |>
        calc_ratio()
      
      if (input$stateid) {
        tab = tab |>
          filter(State == input$state1)
      }
      tab
    })
    
    
}

shinyApp(ui = ui, server = server)
