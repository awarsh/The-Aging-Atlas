library(shiny) 
library(shinythemes)
library(readxl) 
library(ggplot2)
library(plotly)
library(ggpattern)
library(readr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(janitor)

# Now that we have our tools ready, lets bring something in to work on.
data <- read_csv("mousea_aging_transformed_data_with_colon.csv")

# Crafting the user interface for our app.
ui = navbarPage(
  
  # Naming our app. Make sure to pick a cool name, or else it won't work.
  title = "The Aging Atlas",
  
  theme = shinytheme("superhero"),
  
  #project description tab
  tabPanel("Project Description",
           mainPanel(
             h3("Project Description"),
             p("This interactive application is a tool developed
                 to explore the changes in protein expression that occur 
                 with aging. The application facilitates a comparative study between 
                 young (12 weeks old) and aged (84 weeks old) mice, aiming to
                 elucidate the alterations in protein profiles and gain comprehensive 
                 insights into the biological mechanisms underlying aging.")
           )
  ),
  
  # Laying out the main structure: a sidebar for inputs and a main panel for outputs.
  tabPanel("Data Analysis",
           sidebarLayout(
             
             # The sidebar: where users tell us what they want. 
             sidebarPanel(
               # Add a radio button for the user to select gene or protein
               radioButtons("selection", "Select an option:", 
                            choices = list("Gene" = "gene", "Protein" = "protein")),
               numericInput("num_compare", "How many would you like to compare?", value = 2, min = 1, max = 10),
               uiOutput("dynamic_ui"),
               radioButtons("na_action", "NA Action:",
                            choices = c("None" = "none", "Remove" = "remove", "Replace with 0" = "replace")),
               checkboxGroupInput("week_choice", "Select age of mice:",
                                  choices = c("12 weeks" = "12 Week", "84 weeks" = "84 Week"),
                                  selected = c("12 Week", "84 Week")),
               checkboxGroupInput("organ_choice", "Select organs of interest:",
                                  choices = c("Colon", "Liver", "Small Intestine", "Cecum", "Stomach", 
                                              "Kidney", "Heart", "Lung", "Brain", "Spleen", 
                                              "Smooth Muscle", "Serum", "Blood", "Tongue", 
                                              "Eye", "Skin"),
                                  selected = c("Liver", "Brain")),
             ),
             
             # The main panel: our space to deliver results. Don't mess this up.
             mainPanel(
               # Organizing our outputs with tabs for clarity.
               tabsetPanel(
                 tabPanel("Bar Plot", 
                          radioButtons("log2_choice_bar", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          plotlyOutput("bar_plot"),
                          checkboxInput("show_points", "Show individual points"),
                          
                 )
               )
             )
             
           )
  ),
  
  tabPanel("User Guide",
           mainPanel(
             h3("How to use the app"),
             tags$ul(
               tags$li("Selecting Gene or Protein:", tags$p("Start by specifying whether you would like to analyze genes or proteins using the radio button provided.")),
               tags$li("Determining Quantity for Comparison:", tags$p("Decide the number of genes or proteins you wish to compare, utilizing the relevant input option.")),
               tags$li("Specifying Genes/Proteins:", tags$p("After selecting the quantity, you will need to specify exactly which genes or proteins you intend to analyze.")),
               tags$li("NA Action Selection:", tags$p("You are provided with three options for handling NA values:"),
                       tags$ul(
                         tags$li(tags$b("None:"), "When you select 'None', the NA values are not altered, so the mean value for the organ in both age groups are 
                                 computed considering the NA values. In R, any arithmetic operation involving NA returns NA, so if there is at least one NA in the value 
                                 column for the 12-week organ, the mean will be NA, which is represented as 0 in the bar plot."),
                         tags$li(tags$b("Remove:"), "NA values are omitted before computation, providing a mean value based solely on non-NA values."),
                         tags$li(tags$b("Replace with 0:"), "NA values are equated to 0 before computation, affecting the average accordingly.")
                       )
               ),
               tags$li("Selecting Age of Mice:", tags$p("You can select the age of the mice you want to observe.")),
               tags$li("Choosing Organs:", tags$p("After determining the age, select the specific organs you are interested in examining.")),
               tags$li("Navigating the Graph Tab:",
                       tags$ul(
                         tags$li(tags$b("Data Transformation (Optional):"), "If desired, you have the option to view the data set in a Log2 transformed scale."),
                         tags$li(tags$b("Viewing Original Points (Optional):"), "You can choose to view the original data points on the graph by selecting the provided checkbox."),
                         tags$li(tags$b("Interaction with the Graph:"), "The graph is interactive, allowing you to use additional functions provided on the plot for deeper insights 
                                 and analysis. The interactive features include Downloading the Plot, Zooming In on Plots, Panning, Selecting Data Points with Box or Lasso, Auto-scaling,
                                 Hover Tooltips, and Legend Interaction.")
                       )
               )
             )
           )
  )
  
)
server <- function(input, output, session) {
  
  # Render UI for dynamic selection of gene or protein
  output$dynamic_ui <- renderUI({
    tagList(
      lapply(1:input$num_compare, function(i) {
        if (input$selection == "gene") {
          selectInput(paste0("gene_choice_", i), paste0("Gene ", i), choices = unique(data$pg_genes))
        } else {
          selectInput(paste0("protein_choice_", i), paste0("Protein ", i), choices = unique(data$pg_protein_groups))
        }
      })
    )
  })
  
  observe({
    lapply(1:input$num_compare, function(i) {
      if (input$selection == "gene") {
        print(input[[paste0("gene_choice_", i)]])
      } else {
        print(input[[paste0("protein_choice_", i)]])
      }
    })
  })
  
  filtered_data <- reactive({
    #req(input$organ_choice, input$week_choice, input$selection, input$gene_choice, input$protein_choice, input$na_action)
    
    selected_organ <- input$organ_choice
    selected_week <- input$week_choice
    selected_gene <- unlist(lapply(1:input$num_compare, function(i) input[[paste0("gene_choice_", i)]]))
    selected_protein <- unlist(lapply(1:input$num_compare, function(i) input[[paste0("protein_choice_", i)]]))
    
    filtered_data <- data %>% 
      filter(organ %in% selected_organ, age %in% selected_week)
    
    if(input$selection == "gene") {
      filtered_data <- filtered_data %>% filter(pg_genes %in% selected_gene)
    } else {
      filtered_data <- filtered_data %>% filter(pg_protein_groups %in% selected_protein)
    }
    
    if(input$na_action == "remove"){
      filtered_data <- na.omit(filtered_data)
    } else if(input$na_action == "replace"){
      filtered_data$value[is.na(filtered_data$value)] <- 0
    }
    
    filtered_data
  })
  
  observe({
    # Print first few rows of filtered_data to the console
    print(filtered_data(), n = 18)
    
    # Print out the dimensions of the filtered data
    print(dim(filtered_data()))
    
    # Print out the unique values in certain columns for further insights
    print(unique(filtered_data()$organ))
    print(unique(filtered_data()$age))
    if(input$selection == "gene") {
      print(unique(filtered_data()$pg_genes))
    } else {
      print(unique(filtered_data()$pg_protein_groups))
    }
  })
  
  summary_data <- reactive({
    selected_column <- if (input$selection == "gene") "pg_genes" else "pg_protein_groups"
    
    filtered_data() %>%
      mutate(selected_col_value = .data[[selected_column]]) %>%  # create a new column with a simple name
      group_by(organ, age, selected_col_value) %>%               # group by this simple name
      mutate(mean_value = mean(value), sd_value = sd(value), n = n()) %>%
      mutate(se = sd_value / sqrt(n)) %>%
      ungroup()
  })
  
  observe({
    # Print first few rows of summary_data to the console
    print(head(summary_data()))
    
    # Print out the dimensions of the summary data
    print(dim(summary_data()))
    
    # Print out the unique values in certain columns for further insights
    print(unique(summary_data()$organ))
    print(unique(summary_data()$age))
    if(input$selection == "gene") {
      print(unique(summary_data()$pg_genes))
    } else {
      print(unique(summary_data()$pg_protein_groups))
    }
  })
  
  output$bar_plot <- renderPlotly({
    
    legend_title <- if (input$selection == "gene") "Gene" else "Protein"
    
    # If log2 transformation is selected, transform the individual data points first.
    if (input$log2_choice_bar == "log2") {
      plot_data <- summary_data() %>%
        mutate(value = log2(value + 1)) # Transform individual data points
    } else {
      plot_data <- summary_data()
    }
    
    # After transformation, recalculate mean_value, se, and error bars on the transformed scale.
    plot_data <- plot_data %>%
      group_by(organ, age, selected_col_value) %>%
      mutate(
        mean_value = mean(value), # Recalculate mean_value on the transformed scale
        se = sd(value) / sqrt(n())  # Recalculate se on the transformed scale
      ) %>%
      ungroup() %>%
      mutate(
        lower = pmax(mean_value - se, 0), # Ensure lower bound is non-negative
        upper = mean_value + se
      )
    
    p <- ggplot(plot_data, aes(x = organ, fill = age, y = mean_value)) + 
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = lower, ymax = upper),  # Updated aesthetics
                    position = position_dodge(0.9),
                    width = 0.25) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(name = "Age", palette = "Pastel1") +
      labs(y = "Mean Value", x = "Organ") +
      facet_wrap(~selected_col_value)
    
    if (input$show_points) {
      p <- p + geom_point(data = plot_data, aes(x = organ, y = value), 
                          position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9))
    }
    
    plotly::ggplotly(p)
  })
  
  
}
# Run the app
shinyApp(ui = ui, server = server)

