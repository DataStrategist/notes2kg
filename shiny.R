library(shiny)

ui <- fluidPage(
  titlePanel("Excel File Uploader"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose an Excel file", accept = ".xlsx")
    ),
    mainPanel(
      actionButton("analyze_button", "Go!"),
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {
  
  # Store uploaded file
  observeEvent(input$file, {
    file.copy(input$file$datapath, "data.xlsx", overwrite = TRUE)
  })
  
  # Run analyzer.R script when "Go!" button is clicked
  analyze_data <- eventReactive(input$analyze_button, {
    library(tidyverse)
    library(visNetwork)
    
    ## read data in
    df_nodes <- readxl::read_excel("data.xlsx", sheet = "nodes") %>% 
      mutate(label = node %>% stringr::str_wrap(width = 10), shape = "box") %>% rename(title = extra)
    
    df_edges <- readxl::read_excel("data.xlsx", sheet = "edges") %>% 
      mutate(label = what, group = what, arrows = "to",
             color = case_when(
               status == "present" ~ "black",
               status == "in progress" ~ "black",
               status == "to do" ~ "lightgreen",
               status == "should be" ~ "red"),
             dashes = case_when(
               status == "present" ~ FALSE,
               status == "in progress" ~ TRUE,
               status == "to do" ~ TRUE,
               status == "should be" ~ TRUE))
    
    ## add in node sizes, and bold the names
    df_nodes <- df_nodes %>% 
      left_join(c(df_edges$entity1, df_edges$entity2) %>% table %>% 
                  as_tibble %>% rename(node = 1, value = n)) %>% 
      mutate(node = paste0("<b>", node, "</b>"))
    
    ##plot
    visNetwork(nodes = df_nodes, edges = df_edges) %>% 
      visEdges(font = list(color = "lightgrey")) %>% 
      visOptions(nodesIdSelection = TRUE) 
    return(result)
  })
  
  # Display results
  output$results <- renderPrint({
    browser()
    analyze_data()
  })
  
}

shinyApp(ui, server)
