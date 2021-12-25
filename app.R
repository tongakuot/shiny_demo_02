# Analysis of the Data Science Skills ----

# Load Libraries
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(bslib)

# Load the dataset
ds_tbl <- vroom::vroom('00_data/ds_tbl.csv', show_col_types = FALSE)
majors_tbl <- vroom::vroom('00_data/majors_tbl.csv', show_col_types = FALSE)

# USER INTERFACE ----
ui <- navbarPage(
  title    = 'Jonglei Institute of Technology' %>% strong(),
  
  theme    = bslib::bs_theme(version = 4, bootswatch = 'darkly'),
  selected = 'Analysis of Data Science Skills' %>% strong(),
  
  
  tabPanel(
    title = 'Analysis of Data Science Skills' %>% strong(),
    
      fluidPage(
        
        tabsetPanel(
          type = c('pills'),
          
          tabPanel(
            title = 'Data Science Skills by Title',
            
            div(
              class = 'container',
              style = 'padding-top: 75px;',
              id    = 'tab_1',
              
              # User inputs ----
              column(
                width = 4,
                
                wellPanel(
                div(
                  id = 'top_skills',
                  
                  selectInput(
                    inputId   = 'title', 
                    label     = 'Select a job title' %>% str_to_upper(), 
                    choices   = unique(ds_tbl$title),
                    multiple  = FALSE,
                    selected  = 'Data Scientist' %>% str_to_title(),
                    selectize = TRUE,
                    size      = NULL
                  )
                )
              )
            ),
            
            br(), 
            br(),
            
            # PLOT ----
            column(
              width = 10,
              
              plotlyOutput(
                'top_skills_g', height = '600px'
              )
            )
          )
        ),
        
        tabPanel(
          title = 'Data Science Majors by Title',
          
          div(
            class = 'container',
            style = 'padding-top: 75px;',
            id    = 'tab_1',
            
            # User inputs ----
            column(
              width = 4,
              
              wellPanel(
                div(
                  id = 'top_ds_majors',
                  
                  selectInput(
                    inputId   = 'major_title', 
                    label     = 'Select a job title' %>% str_to_upper(), 
                    choices   = unique(majors_tbl$title),
                    multiple  = FALSE,
                    selected  = 'data scientist' %>% str_to_title(),
                    selectize = TRUE,
                    size      = NULL
                  )
                )
              )
            ),
            
            br(), 
            br(),
            
            # PLOT ----
            column(
              width = 10,
              
              plotOutput(
                'top_majors_g', height = '540px'
              )
            )
          )
        )
      )
    )
  ),
  
  # About ----
  tabPanel(
    title = "About" %>% strong(),
    div(
      class = "container",
      column(
        width = 12,
        div(
          class = "panel-header",
          h2("Jonglei Institute of Technology") %>% strong(),
          div(
            p("Jonglei Institute of Technology(JIT) is a faith-based nonprofit organization established by a group of Lost Boys and Lost Girls of Sudan to empower South Sudanese diaspora families by providing them free online education, tutoring, mentoring services; Bible studies and spiritual support, counseling, and guidance; as well as free financial literacy training. JITeam strongly believes that these free services will help South Sudanese families succeed. And when these families succeed, they will be able to raise successful kids, for successful families raise successful kids.")
          ),
          
          br(),
          
          div(
            p(
              class = "lead",
              "Alier Reng,",  tags$small("President")),
            p("Alier is a data science professional, statistician, and educator with over seven years of university teaching experience and more than five years of experience in healthcare analytics. He is passionate about delivering quality data-driven results with efficiency and high accuracy. As a former refugee, his life experiences have taught him the importance of patience, perseverance, pressure, and stress management.
              He aspires to help others learn data science, statistics, mathematics, and data visualization with R Shiny.")
          ),
          
          br(),
          
          div(
            p(class = "lead",
              "Credits"
            ),
            p("Thanks to", tags$a(href = "https://khuyentran1401.github.io/Efficient_Python_tricks_and_tools_for_data_scientists/Chapter8/insights.html", "Khuyen Tran"), "for always sharing new data science and python tricks and techniques with us on LinkedIn for free! Please find time to thank her, follow, and share her work with your connections." ),
          )
        )
      )
    )
  )
  
)

# SERVER ----
server <- function(input, output, session) {
  
  # Data prep ----
  # Data Science Skills ----
  skills <- reactive({
    # Data
    ds_tbl %>% 
      
      # Filter data by title column
      filter(
        title == input$title
      ) %>% 
      
      # Sort the data by count
      arrange(count) %>% 
      
      # Reorder the data
      mutate(skill = skill %>% fct_reorder(count))
    
  })
  
  # Data science majors ----
  majors <- reactive({
    
    # Data
    majors_tbl %>% 
      
      # Filter data by title column
      filter(
        title == input$major_title
      ) %>% 
      
      # Modify major names to wrap around
      mutate(
        major = str_wrap(major, width = 25)
      ) %>% 
      
      # Sort the data by count
      arrange(freq) %>% 
      
      # Reorder the data
      mutate(major = fct_reorder(major, freq))
      
     
    
  })
  
  
  # Data science top skills plot ----
  output$top_skills_g <- renderPlotly({
    
    # Initialize the canvas
   g <- ggplot(skills(), aes(count, skill)) +
        
        # Add the data points
        geom_point(alpha = 0.5) +
        
        # Add the title and subtitle
        labs(
          title = str_glue("{input$title}'s Top Skills"),
          x     = NULL
        ) +
        
        # Center-align the plot title
        theme(
          plot.title = element_text(hjust = 0.5)
        )
    
    # Convert the plot to plotly
    ggplotly(g)
    
  }) 
  
  # Data science top majors plot ----
  output$top_majors_g <- renderPlot({
    
    # Initialize the canvas
    ggplot(majors(), aes(freq, major)) +
      
      # Add the data points
      geom_col() +
      
      # Add data labels
      geom_label(aes(label = freq)) +
      
      # Add theme styling
      bbplot::bbc_style() +
      
      # Add the title and subtitle
      labs(
        title = str_glue("{input$major_title}'s Top Majors")
      ) +
      
      # Center-align the plot title
      theme(
        plot.title  = element_text(hjust = 0.5, 
                                  face  = 'plain',
                                  size  = 18),
        axis.text   = element_text(size  = 12)
      )
    
  })
    
}

# RUN THE APP
shinyApp(ui, server)