options(scipen=99)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(readr)
library(magrittr)
library(tidyverse)
library(glue)
library(plotly)
library(scales)
library(DT)

imdb <- read_csv("imdb_top_1000.csv", locale = locale(encoding = "latin1"))

imdb <- imdb |> mutate(
  Released_Year = as.integer(Released_Year),
  Certificate = as.factor(Certificate),
  Genre = as.factor(Genre)
)
imdb <- na.exclude(imdb)


if (interactive()) {
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  dashboardHeader(title = "IMDB MOVIES LIST"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Certificate/Meta Score", tabName = "tabtop", icon = icon("signal")),
      menuItem("Tweak Your Table", tabName = "tabmid", icon = icon("wrench")),
      menuItem("Sort by Certificate", tabName = "tabbot", icon = icon("table"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "tabtop",
              fluidRow(
                infoBox("TOTAL MOVIES",
                        length(unique(imdb$Series_Title)),
                        icon = icon("play"),
                        color = "yellow"),
              ),
              fluidRow(
                box(width = 12,
                    plotlyOutput(outputId = "graph1")
                )
              )
      ),
      tabItem(tabName = "tabbot",
              fluidRow(
                box(width = 12,
                    title = 'From the "Top Certificate" Menu, You can sort all columns based on the total meta score that suits you best. 
                    To locate the movie faster, you can use search feature.', 
                    background = "red",
                    textOutput("status"))),
              
                fluidRow(
                  box(width = 12,
                      DT::dataTableOutput("table1")),
                )
              ),
      tabItem(tabName = "tabmid",
              fluidRow(
                box(
                checkboxGroupInput(
                   "variable", "Variables to show:",
                                 c("Genre" = "Genre",
                                   "Rating" = "IMDB_Rating",
                                   "Total Votes" = "No_of_Votes",
                                   "Certificate" = "Certificate"))),
              ),
              fluidRow(tableOutput("data"))
      )

    )
  )
)


server <- function(input, output) {

  output$graph1 <- renderPlotly({

    imdb_genre <-
      imdb |>
      group_by(Certificate) |>
      summarise(mscore = sum(Meta_score)) |>
      ungroup() |>
      arrange(desc(mscore)) |>
      top_n(30)

    imdb_genre <-
      imdb_genre |>
      mutate(label = glue("Certificate : {Certificate}
                      Rating : {mscore}"))

    plot1 <- ggplot(imdb_genre,
                    aes(x = mscore,
                        y = reorder(Certificate,mscore),
                        text = label)) +
      geom_col(aes(fill=mscore))+
      scale_fill_gradient(low = "blue", high = "red")+
      labs(
        title = "Top Certificate on IMDB by Meta Score",
        x = "Meta Score",
        y = "Certificate")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5))

    ggplotly(plot1, tooltip = "text")
  })
  
  output$table1 <- DT::renderDataTable({
  
    imdb_certificate <-
      imdb |> dplyr::select("Series_Title", "Genre", "Certificate")
    
    
    datatable(imdb_certificate, filter="top", selection="multiple", escape=FALSE, options = list(dom = 'ltipr'))
    })
  
  output$data <- renderTable({
    
    imdb_vote <- 
      imdb |> 
      dplyr::select("Series_Title", "Genre", "IMDB_Rating", "No_of_Votes", "Certificate")
    
    imdb_vote[, c("Movie Title" = "Series_Title", input$variable), drop = FALSE]
    })
}


shinyApp(ui = ui, server = server)
}