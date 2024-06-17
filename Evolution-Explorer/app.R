library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)
library(shinythemes)
library(markdown)


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
ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage("Human Evolution Explorer",
        navbarMenu("Explore",
            tabPanel(
                "Time",
                titlePanel("Species Occurance on a Time Scale"),
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
                titlePanel("Versus Plot for Numeric Data"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("Var1", "X axis:", choices = numeric_cols, selected = colnames(evo_data)[1]),
                        selectInput("Var2", "Y axis:", choices = numeric_cols, selected = colnames(evo_data)[2]),
                        selectInput("groupVar", "Group By (Optional):", choices = c("None", cat_cols, "Specie", numeric_cols), selected = "None"),
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
                title="About Data",
                div(includeMarkdown("../data.md"),
                    align = "justify")
            ), # Data tabPanel()
        
            tabPanel(
                title="Credits",
                div(includeMarkdown("../credits.md"),
                    align="justify")
            ) # Credits tabPanel()
    ) # navbarPage()
) # fluidPage()

###########################
# Server logic            #
###########################
server <- function(input, output, session){

    ##
    # Plot for Time tabPanel
    ##
    
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
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    ##
    # Plot for Cranial tabPanel
    ##
    
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
                geom_smooth(method = "lm", se = FALSE) + # adding a trend line
                labs(title = "Cranial Capacity Over Time", x = "Time (Millions of Years Ago)", y = "Cranial Capacity (cc)") +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_blank(),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    ##
    # Plot for Height tabPanel
    ##
    
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
                geom_smooth(method = "lm", se = FALSE) + # adding a trend line
                labs(title = "Height Over Time", x = "Time (Millions of Years Ago)", y = "Height (cm)") +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
    })
    
    ##
    # Plot for Versus tabPanel
    ##
    
    versusPlot <- reactive({
        var1 <- input$Var1
        var2 <- input$Var2
        group_var <- input$groupVar
        
        x_label <- if (var1 != "Time") var1 else "Time (Millions of Years Ago)"
        y_label <- if (var2 != "Time") var2 else "Time (Millions of Years Ago)"
        x_label <- if (var1 != "Height") x_label else "Height (cm)"
        y_label <- if (var2 != "Height") y_label else "Height (cm)"
        x_label <- if (var1 != "Cranial_Capacity") x_label else "Cranial Capacity (cm)"
        y_label <- if (var2 != "Cranial_Capacity") y_label else "Cranial Capacity (cc)"
        
        if(group_var == "None"){
            p <- ggplot(evo_data, aes_string(x = var1, y = var2)) +
                geom_point() +
                labs(title = paste("Scatter plot of", var1, "vs", var2),
                     x = x_label, y = y_label) +
                theme_minimal() +
                theme(
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            p <- ggplot(evo_data, aes_string(x = var1, y = var2, color = group_var)) +
                geom_point() +
                labs(title = paste("Scatter plot of", var1, "vs", var2, "\nGrouped by", group_var),
                     x = x_label, y = y_label) +
                theme_minimal() +
                scale_color_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_text(size = 16, face = "bold"),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        
        print(p)
    })
    
    ##
    # Plot for Bar Plot tabPanel
    ##
    
    barPlot <- reactive({
        cat_var <- input$catVar
        group_var <- input$groupVarBar
        
        # If no grouping variable was chosen
        if(group_var == "None"){
            p <- ggplot(evo_data, aes_string(x = cat_var)) +
                geom_bar() +
                labs(title = paste("Bar plot of", cat_var),
                     x = cat_var, y = "Count") +
                theme_minimal() +
                theme(
                    legend.title = element_text(size = 16, face = "bold"),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        else{
            titleOfPlot <- paste("Bar plot of", cat_var)
            
            # if category and grouping variable is not the same than we should rename the title to inform about it
            if(cat_var != group_var){
                titleOfPlot <-  paste(titleOfPlot, "\nGrouped by", group_var)
            }
            p <- ggplot(evo_data, aes_string(x = cat_var, fill = group_var)) +
                geom_bar(position = "stack") +
                labs(title = titleOfPlot,
                     x = cat_var, y = "Count") +
                theme_minimal() +
                scale_fill_viridis_d(option = "plasma") +
                theme(
                    legend.title = element_text(size = 16, face = "bold"),
                    legend.text = element_text(size = 14, face = "bold"),
                    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
                )
        }
        
        print(p)
    })
    
    ##
    # Outputing Plots
    ##
    
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
    
    ##
    # Handling downloads
    ##
    output$downloadTimePlot <- downloadHandler(
        filename = function(){
            paste("time_scale_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = timePlot(), device = "png", width = 10, height = 6, bg = "white")
        }
    )
    
    output$downloadCranialPlot <- downloadHandler(
        filename = function(){
            paste("cranial_capacity_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = cranialPlot(), device = "png", width = 10, height = 6, bg = "white")
        }
    )
    
    output$downloadHeightPlot <- downloadHandler(
        filename = function(){
            paste("height_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = heightPlot(), device = "png", width = 10, height = 6, bg = "white")
        }
    )
    
    output$downloadVersusPlot <- downloadHandler(
        filename = function(){
            paste("versus_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = versusPlot(), device = "png", width = 10, height = 6, bg = "white")
        }
    )
    
    output$downloadBarPlot <- downloadHandler(
        filename = function(){
            paste("bar_plot", Sys.Date(), ".png", sep = "")
        },
        content = function(file){
            ggsave(file, plot = barPlot(), device = "png", width = 10, height = 6, bg = "white")
        }
    )
    
    ##
    # End of server logic
    ##
}

#running the app
shinyApp(ui = ui, server = server)
