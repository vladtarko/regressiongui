library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(ggstance)
library(jtools)
library(GGally)
library(broom)
library(broom.mixed)
library(plm)
 
# UI layout -----------------------------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("sandstone"),

  #  titlePanel("Simple regression"),
    p(),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose data file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".dta",
                                 ".rds")),
            fluidRow(
                column(6, uiOutput("select1")),
                column(6, uiOutput("select2"))
            ),
                          uiOutput("select3"),
            fluidRow(
                column(6, uiOutput("select4")),
                column(6, uiOutput("select5"))
            ),
        ),

        uiOutput("select6")
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # load data ------------------------------------------------------------------------------------
    
    v <- reactiveValues(df = NULL)             # reactiveValues is needed so we can modify the data
    
    data <- reactive({                         # defines reactive function data()
        req(input$file1)                       # only call rio::import if a file was selected
        rio::import(input$file1$datapath)      # imports the data from file
    })
    
    observe({ v$df <- data() })                # populates the reactiveValues dataframe
    
    # show data ---------------------------------------------------------------------------------------
    
    output$show_data <- renderDataTable({

        req(input$file1)
        
        v$df %>% DT::datatable(
            rownames = FALSE,
            extensions = c('Scroller', 'Buttons'),        # makes scrolling of large data fast
            class = "display",
            options = list(
                buttons = c('csv', 'excel'),
                class = "compact",           # makes the table more compact
                dom = 'ftBi',                # shows only search box, table, Buttons, and info
                pageLength = nrow(.),        # shows all rows in a single page
                columnDefs = list(
                    list(className = 'dt-left', targets = c(1,2))   # aligns columns to left
                ),

                # for Scroller extension
                deferRender = TRUE,
                scrollY     = 650,          # height of the table
                scroller    = TRUE
            ))
        
    })
    
    # UI elements that appear after loading the data ----------------------------------------------
    
    output$select1 <- renderUI({
        req(input$file1)
        radioButtons(
            inputId = "y",
            label = "Dependent variable",
            choices = names(v$df),
            selected = NA)
    })
    
    output$select2 <- renderUI({
        req(input$file1)
        checkboxGroupInput(
            inputId = "x",
            label = "Independent variables",
            choices = names(v$df))
    })
    
    output$select3 <- renderUI({
        req(input$file1)
        selectInput("reg_type", label = "Type of fixed effects analysis", choices = c("Pooled", "Within", "Between"))
    })
    
    output$select4 <- renderUI({
        req(input$file1)
        radioButtons(
            inputId = "unit",
            label = "Unit variable",
            choices = names(v$df),
            selected = NA)
    })
    
    output$select5 <- renderUI({
        req(input$file1)
        radioButtons(
            inputId = "time",
            label = "Time variables",
            choices = names(v$df),
            selected = NA)
    })
    
    output$select6 <- renderUI({
        req(input$file1)
        
        mainPanel(
            tabsetPanel(
                tabPanel("Data",           p(), dataTableOutput("show_data"), height = 800),
                tabPanel("Add/Edit variables", 
                         p(),
                         fluidRow(
                             column(4, 
                                                textInput("var_new",   label = "Variable name"),
                                                textInput("var_def",   label = "Variable definition"),
                                                actionButton("submit", label = "Apply")
                                    ),
                             column(6,          includeMarkdown("explain.md")),
                             column(2)
                         )
                                               
                ),
                tabPanel("Summary stats", p(), tableOutput("show_summary")),
                tabPanel("Correlations",       plotOutput("show_cor",          height = 800)),
                tabPanel("Regression results", plotOutput("show_results_plot", height = 600),
                                               tableOutput("show_results_table")
                ),
                tabPanel("About", p(),         includeMarkdown("about.md"))
            )
            
        )
    })
    
    # add/change variables -----------------------------------------------------------------
    
    observeEvent(input$submit, {
        req(input$var_new, input$var_def)

        try(
            v$df <- mutate(v$df, !!input$var_new := eval(parse(text = input$var_def)))
        )

    })

    
    # create summaries ----------------------------------------------------------------------
    
    output$show_summary <- renderTable({
        req(input$file1)
        #req(!is.na(input$y) || !is.na(input$x))
        
        df_ <- v$df %>% select(input$unit, input$time, input$y, input$x)
        
        tibble(
            Variable    = names(df_),
            Obs.        = map_dbl(df_, ~sum(!is.na(.x))),
            Unique      = map_dbl(df_, ~length(unique(.x))),
            Missing     = map_dbl(df_, ~sum( is.na(.x))),
            Mean        = map_dbl(df_, ~ifelse(is.numeric(.x), mean(.x, na.rm = TRUE), NA)),
            Median      = map_dbl(df_, ~ifelse(is.numeric(.x), median(.x, na.rm = TRUE), NA)),
            Std.Dev.    = map_dbl(df_, ~ifelse(is.numeric(.x), sd(.x, na.rm = TRUE), NA)),
            Min         = map_chr(df_, ~ifelse(is.numeric(.x), min(.x, na.rm = TRUE), NA)),
            Max         = map_chr(df_, ~ifelse(is.numeric(.x), max(.x, na.rm = TRUE), NA))
        )
         
    })
    
    # create correlations plots ------------------------------------------------------------
    
    output$show_cor <- renderPlot({
        req(input$file1)
        req(!is.na(input$y))
        req(!is.na(input$x))
        
        v$df %>% 
            select(input$y, input$x) %>% 
            select_if(is.numeric) %>% 
            GGally::ggpairs(upper = list(continuous = wrap("cor", size=10))) +
                scale_x_continuous(labels = scales::comma) +
                scale_y_continuous(labels = scales::comma) +
                theme_light(base_size = 20)
        
    })
    
    # create the model based on selected variables -------------------------------------------
    
    model <- reactive({
        
        f <- paste(input$y, "~", paste(input$x, collapse = "+")) %>% formula() 
        
        if (input$reg_type == "Within") {
            
          m <- plm(f,
                  model = "within",
                  index = c(input$unit, input$time),
                  effect = "twoway",
                  data = v$df)
          
        } else if (input$reg_type == "Between"){
            
          m <- plm(f,
                   model = "between",
                   index = c(input$unit, input$time),
                   data = v$df)
            
        } else {
            
          m <- lm(f, data = v$df)
          
        }
        
        return(m)
        
    })
    
    # show the regression results, plot + table ---------------------------------------------------------
    
    output$show_results_plot <- renderPlot({
        req(input$file1)
        req(!is.na(input$y))
        req(!is.na(input$x))
        
        jtools::plot_summs(model(), inner_ci_level = .9, plot.distributions = TRUE, rescale.distributions = TRUE, legend.title = "") +      
            labs(title = paste("Effect on ", input$y), 
                 subtitle = paste("Fixed effects:", stringr::str_to_lower(input$reg_type), "model"),
                 x = input$y) +
            scale_x_continuous(labels = scales::comma) +
            theme_light(base_size = 20)

    })    
    
    output$show_results_table <- renderTable({
       req(input$file1)
       req(!is.na(input$y))
       req(!is.na(input$x))
        
       broom::tidy(model(), conf.int = TRUE) %>% 
            select(-statistic)
    })   
}

# Run the application 
shinyApp(ui = ui, server = server)
