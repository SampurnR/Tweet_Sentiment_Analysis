ui <- fluidPage(
	br(),
	textInput("search_term", "Keyword", NULL),
	br(),
	submitButton("Plot Chart"),
	br(),
	hr(),
	plotOutput("chart")
)

server <- function(input, output) {
	library(twitteR)
	library(syuzhet)
	library(ggplot2)
	source("./twitterStuff.R")

	output$chart <- renderPlot({
		print(input$search_term)
		if(is.null(input$search_term) | nchar(input$search_term) < 4) return()
		renderChart(getSentiment(getTweetText(input$search_term)), input$search_term)
	})
}

shinyApp(ui, server)