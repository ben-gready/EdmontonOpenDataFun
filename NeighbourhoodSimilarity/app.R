#### Load necessary packages and data ####
library(data.table)
library(shiny)
library(shinydashboard)
library(memoise)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(RSocrata)
library(reshape2)
library(tidyr)
library(DT)
library(devtools)
#dev_mode()
library(networkD3)
#dev_mode()
#library(rJava)


source('./WranglingFunctions.R')

load_features <- function(source){
  if(source == "occupation"){
    data <- PrepareOccupationData()
  }
  else if(source == "ownership_residence_time"){
    data <- PrepareLengthOfResidenceData()
  }else if(source == "age_gender"){
    data <- PrepareGenderAgeData()
  }
  else{
    stop("not defined")
  }
  return(data)
}

mem_load_features <- memoise(load_features)

generate_cluster_tree <- function(data){
  data <- data.frame(data)
  row.names(data) <- data$neighbourhood_name
  tree <- hclust(dist(data), method = "ward.D2")
  return(tree)
}

#### UI ####

sidebar <- dashboardSidebar(
  sidebarMenu(
    sliderInput("target_links_max", "Max Targets", value = 2, min = 1,
                max = 5, step = 1),
    selectizeInput(inputId = "distance_type",
                   choices = c("occupation", "ownership_residence_time", "age_gender"),
                   multiple = FALSE,
                   label = "Similarity Based Upon",
                   selected = c("occupation")),
    selectizeInput(inputId = "wards",
                   choices = paste("WARD", 1:12),
                   multiple = TRUE,
                   label = "Add/Remove Wards",
                   selected = paste("WARD", c(1,6,8,12))),
    sliderInput("n_clusters", "Number of Clusters", value = 4, min = 1,
                max = 8, step = 1)
  )
)

box_force <- box(width = NULL,
                 title = "Force Directed Graph",
                 forceNetworkOutput("force")#, height = 1200, width = 1200
)

box_tree <- box(width = NULL,
                title = "Clustering Tree Diagram",
                dendroNetworkOutput("tree_diagram", height = 2000)#, height = 1200, width = 1200)
)
box_intro <- box(
  title = "Introduction", width = 7, solidHeader = TRUE,
  "Explore the similarity between Edmonton neighbourhoods! Below is a 'force directed graph' containing neighbourhoods in the wards selected in the sidebar. The graph connects each neighbourhood to its most similar neighbourhoods. You can control the maximum number of connections for each neighbourhood in the sidebar. The Dendrograph tab shows the hierarchy of businesses, determined by a hierarchical clustering algorithm. We can use this tree to create groups of business types that have similar properties, by choosing to 'cut the tree' at a certain point. You can control where to cut the tree by choosing the number of groups in the sidebar. Colours in the visualizations correspond to the groupings based upon the selected number of clusters. In the sidebar you may control the 'distance metric source'."
)

box_dt <- box(
  width = NULL,
  title = "The Data",
  div(style = 'overflow-x: scroll',DT::dataTableOutput('table'))
)

visuals_box <- tabBox(
  #title = "Vertical Similarity Visuals",
  width = NULL,
  #height = 3000,
  # The id lets us use input$tabset1 on the server to find the current tab
  id = "tabset1",# height = "250px",
  tabPanel("Network Graph", box_force),
  tabPanel("Dendrograph", box_tree),
  tabPanel("The Data", box_dt)
)

ui <- dashboardPage(
  dashboardHeader(title = "Neighbourhood /nSimilarities"),
  sidebar,
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box_intro
    ),
    fluidRow(
      #box_force
      visuals_box
    )
  )
)

#### Server ####
server <- function(input, output) {
  
  get_data <- reactive({
    return(mem_load_features(input$distance_type))
  })
  
  get_max_links <- reactive(return(input$target_links_max))
  get_n_clusters <- reactive(return(input$n_clusters))
  get_wards <- reactive(return(input$wards))
  
  output$tree_diagram <- renderDendroNetwork({
    data <- get_data()
    
    # subset wards:
    wards <- get_wards()
    data_subset <- data[ward %in% wards]
# 
#     data_list <- get_data()
#     data <- data_list$data
    tree <- generate_cluster_tree(data_subset)
    n_clusters <- get_n_clusters()
    
    dendroNetwork(tree, width = 500, zoom = TRUE, textColour = brewer.pal(n = 8, name = 'Set1')[cutree(tree, n_clusters)])#, height = 3000
  })
  
  output$table = renderDataTable({
    data <- get_data()
    wards <- get_wards()
    data_subset <- data[ward %in% wards]
    data_subset
  },filter = 'top', options = list(lengthChange = TRUE, pageLength = 25))
  
  output$force <- renderForceNetwork({
    withProgress(message = 'Loading Data', {
      data <- get_data()
      
      # subset wards:
      wards <- get_wards()
      data_subset <- data[ward %in% wards]
      links_nodes <- GetNodesAndLinks(data_subset)
      
      links <- links_nodes$links
      nodes <- links_nodes$nodes
      max_links <- get_max_links()
      n_clusters <- get_n_clusters()
      tree <- generate_cluster_tree(data_subset)
      
      tree_dt <- data.table(name = names(cutree(tree, n_clusters)), group=cutree(tree, n_clusters))
      tree_dt <- tree_dt[name %in% nodes$name]
      
      nodes <- merge(nodes,tree_dt, by = "name", all.x = TRUE)
      setkey(nodes, NUMBER)
      
      links_cropped <- data.frame(links[!is.na(TARGET) & ORDER <= max_links, list(SOURCE, TARGET, DISTANCE)])

      nodes <- data.frame(nodes)
    })
    
    ColourScale <- paste0("d3.scale.category10().range(['",paste(brewer.pal(n = 8, name = 'Set1'), collapse = "','"),"'])")
    
    # d3.scaleOrdinal(d3.schemeCategory10);
    forceNetwork(Links =  links_cropped, Nodes = nodes, colourScale = ColourScale,
                 Source = "SOURCE", Target = "TARGET", 
                 Value = "DISTANCE", NodeID = "name", Group = "group", 
                 zoom = TRUE, opacity = 0.8, opacityNoHover = 0.6, bounded = FALSE,
                 linkDistance = networkD3::JS("function(d) { return 10*d.value; }")#, # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                 #linkWidth = networkD3::JS("function(d) { return 25 * d.value; }")# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                 )
  })
  
}

#### Run ####
shinyApp(ui = ui, server = server)
