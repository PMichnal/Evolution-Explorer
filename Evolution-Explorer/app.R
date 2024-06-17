library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)


evo_data <- read.csv("../Evolution_DataSets.csv")
colnames(evo_data)[colnames(evo_data) == "Genus_._Specie"] <- "Specie"

evo_data <- evo_data %>%
    mutate(Time = -Time)

numeric_cols <- names(evo_data)[sapply(evo_data, is.numeric)]
cat_cols <- names(evo_data)[!names(evo_data) %in% numeric_cols]
cat_cols <- cat_cols[cat_cols != "Specie"]

###########################
# UI                      #
###########################
ui <- fluidPage(
    navbarPage("Human Evolution Explorer",
        navbarMenu("Explore",
            tabPanel(
                "Time",
                titlePanel("Species on a Time Scale"),
                sidebarLayout(
                    sidebarPanel(
                    selectInput("speciesTime", "Select Species:", 
                        choices = unique(evo_data$Specie), 
                        selected = 'Homo Sapiens', 
                        multiple = TRUE),
                    downloadButton("downloadTimePlot", "Download Plot")
                    ),
                    mainPanel(
                        plotOutput("timePlot")
                    )
                )
            ), # Time tabPanel
            tabPanel(
                "Cranial Capacity",
                titlePanel("Carnial Capacity of selected species over time"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("speciesCranial", "Select Species:", 
                            choices = unique(evo_data$Specie), 
                            selected = 'Homo Sapiens', 
                            multiple = TRUE),
                        downloadButton("downloadCranialPlot", "Download Plot")
                    ),
                    mainPanel(
                        plotOutput("cranialPlot")
                    )
                )
            ), # Cranial Capacity tabPanel
            tabPanel(
                "Height",
                titlePanel("Height of Selected Species Over Time"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("speciesHeight", "Select Species:", 
                                    choices = unique(evo_data$Specie), 
                                    selected = 'Homo Sapiens', 
                                    multiple = TRUE),
                        downloadButton("downloadHeightPlot", "Download Plot")
                    ),
                    mainPanel(
                        plotOutput("heightTimePlot")
                    )
                )
            ), # Height tabPanel
            
            tabPanel(
                "Versus",
                titlePanel("Plot "),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("Var1", "X axis:", choices = numeric_cols, selected = colnames(evo_data)[1]),
                        selectInput("Var2", "Y axis:", choices = numeric_cols, selected = colnames(evo_data)[2]),
                        selectInput("groupVar", "Group By (Optional):", choices = c("None", cat_cols, "Specie"), selected = "None"),
                        downloadButton("downloadVersusPlot", "Download Plot")
                    ),
                    mainPanel(
                        plotOutput("versusPlot")
                    )
                )
            ), # Versus tabPanel
            
            tabPanel(
                "Bar Plot",
                titlePanel("Bar Plot of Categorical Data"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("catVar", "Category Variable:", choices = cat_cols, selected = cat_cols[1]),
                        selectInput("groupVarBar", "Group By (Optional):", choices = c("None", cat_cols, "Specie"), selected = "None"),
                        downloadButton("downloadBarPlot", "Download Plot")
                    ),
                    mainPanel(
                        plotOutput("barPlot")
                    )
                )
            ) # Bar Plot tabPanel
            
        ), # Explore
               
            tabPanel(
                title="Credits",
                titlePanel("Data"),
                div(includeMarkdown("../data.md"),
                    align="justify")
            ) # Credits tabPanel()
               
    ) # navbarPage()
) # fluidPage()

###########################
# Server logic            #
###########################
server <- function(input, output, session){

    timePlot <- reactive({
        if(length(input$speciesTime) == 0){
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "No species selected", 
                         size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                labs(title = "Time Scale", x = "Time (Millions of Years Ago)", y = "Density") +
                theme_minimal() +
                theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            selected_values <- evo_data %>%
                filter(Specie %in% input$speciesTime)

            ggplot(selected_values, aes(x = Time, color = Specie, fill = Specie, group = Specie)) +
                geom_density(alpha = 0.5) + 
                labs(title = "Time Scale for Selected Species", x = "Time (Millions of Years Ago)", y = "Density") +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                scale_fill_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    cranialPlot <- reactive({
        if(length(input$speciesCranial) == 0){
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "No species selected", 
                         size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                labs(title = "Cranial Capacity Over Time", x = "Time (Millions of Years Ago)", y = "Cranial Capacity (cc)") +
                theme_minimal() +
                theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            selected_values <- evo_data %>%
                filter(Specie %in% input$speciesCranial)
            
            ggplot(selected_values, aes(x = Time, y = Cranial_Capacity, color = Specie)) +
                geom_point() +
                geom_smooth(method = "lm", se = FALSE) +
                labs(title = "Cranial Capacity Over Time", x = "Time (Millions of Years Ago)", y = "Cranial Capacity (cc)") +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    heightPlot <- reactive({
        if(length(input$speciesHeight) == 0){
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "No species selected", 
                         size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
                labs(title = "Height Over Time", x = "Time (Millions of Years Ago)", y = "Height (cm)") +
                theme_minimal() +
                theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            selected_values <- evo_data %>%
                filter(Specie %in% input$speciesHeight)
            
            ggplot(selected_values, aes(x = Time, y = Height, color = Specie)) +
                geom_point() +
                geom_smooth(method = "lm", se = FALSE) +
                labs(title = "Height Over Time", x = "Time (Millions of Years Ago)", y = "Height (cm)") +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    versusPlot <- reactive({
        var1 <- input$Var1
        var2 <- input$Var2
        group_var <- input$groupVar
        
        if(group_var == "None"){
            p <- ggplot(evo_data, aes_string(x = var1, y = var2)) +
                geom_point() +
                labs(title = paste("Scatter plot of", var1, "vs", var2),
                     x = var1, y = var2) +
                theme_minimal() +
                theme(
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            p <- ggplot(evo_data, aes_string(x = var1, y = var2, color = group_var)) +
                geom_point() +
                labs(title = paste("Scatter plot of", var1, "vs", var2, "\nGrouped by", group_var),
                     x = var1, y = var2) +
                theme_minimal() +
                theme(
                    legend.title = element_text(size = 16, face = "bold"),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        
        print(p)
    })
    
    barPlot <- reactive({
        cat_var <- input$catVar
        group_var <- input$groupVarBar
        
        if(group_var == "None"){
            p <- ggplot(evo_data, aes_string(x = cat_var, fill = cat_var)) +
                geom_bar() +
                labs(title = paste("Bar plot of", cat_var),
                     x = cat_var, y = "Count") +
                theme_minimal() +
                theme(
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            p <- ggplot(evo_data, aes_string(x = cat_var, fill = group_var)) +
                geom_bar(position = "stack") +
                labs(title = paste("Bar plot of", cat_var, "\nGrouped by", group_var),
                     x = cat_var, y = "Count") +
                theme_minimal() +
                theme(
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        
        print(p)
    })
    
    output$timePlot <- renderPlot({
        timePlot()
    })
    
    output$cranialPlot <- renderPlot({
        cranialPlot()
    })
    
    output$heightTimePlot <- renderPlot({
        heightPlot()
    })
    
    output$versusPlot <- renderPlot({
        versusPlot()
    })
    
    output$barPlot <- renderPlot({
        barPlot()
    })
    
    output$downloadTimePlot <- downloadHandler(
        filename = function(){
            paste("time_scale_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = timePlot(), device = "png", width = 10, height = 6)
        }
    )
    
    output$downloadCranialPlot <- downloadHandler(
        filename = function(){
            paste("cranial_capacity_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = cranialPlot(), device = "png", width = 10, height = 6)
        }
    )
    
    output$downloadHeightPlot <- downloadHandler(
        filename = function(){
            paste("height_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = heightPlot(), device = "png", width = 10, height = 6)
        }
    )
    
    output$downloadVersusPlot <- downloadHandler(
        filename = function(){
            paste("versus_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = versusPlot(), device = "png", width = 10, height = 6)
        }
    )
    
    output$downloadBarPlot <- downloadHandler(
        filename = function(){
            paste("bar_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = barPlot(), device = "png", width = 10, height = 6)
        }
    )
    
}

#running the app
shinyApp(ui = ui, server = server)
