# Cross-linguistic voice onset time interactive app
# Written by Eleanor Chodroff
# Created 12 May 2018
# Last updated 1 Feb 2019

library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)

# The data presented here accompany the following article:
# Chodroff, E., Golden, A., and Wilson, C. (2019). Evidence for a universal constraint on phonetic realization: Covariation of stop voice onset time across languages. The Journal of the Acoustical Society of America 145(1), EL109-EL115.

means <- read.csv("crosslinguisticVOTmeans.csv", header=T)
means$language <- ifelse(!is.na(means$dialect), paste(means$language, means$dialect, sep = ', '), as.character(means$language))
langfamily <- sort(unique(means$family))
languages <- sort(unique(means$language))


# Define UI----
ui <- dashboardPage(

	# App title ----
	dashboardHeader(title = "Cross-linguistic voice onset time", titleWidth = 325),

	# Sidebar layout with input and output definitions ----
	dashboardSidebar(disable=T),

	# Main panel for displaying outputs ----
	dashboardBody(
	
 		fluidRow(
 		 
 		# Output: Plot
		box(width=6, plotOutput("stopPlot", hover=hoverOpts("plot_hover", delay=20, clip=T), height=400), verbatimTextOutput("hover_info", placeholder=T)),
		
 		# Input: Selector for variable 1
		box(selectInput("place1", "Place of Articulation 1:",
		c("labial VOT" = "labial", "coronal VOT" = "coronal", "dorsal VOT" = "dorsal")),
		
		# Input: Selector for variable 2
		selectInput("place2", "Place of Articulation 2:",
		c("labial VOT" = "labial", "coronal VOT" = "coronal", "dorsal VOT" = "dorsal"), selected="dorsal"),
		
		radioButtons("votcati", "VOT Category:",
		c("all" = "all", "long-lag VOT" = "long.lag", "short-lag VOT" = "short.lag", "lead VOT" = "lead"), selected="all"),
				
		# Input: Selector for language family
		selectInput("myfamilies", "Language Family:", as.character(langfamily), multiple=T),
		
		# Input: Selector for language
		selectInput("mylanguages", "Language:", as.character(languages), multiple=T),
				
		# Input: Checkbox for contrast
		checkboxInput("showContrasts", "Mark languages without a laryngeal contrast", value=T))),
		
		fluidRow(
		box(width=8, dataTableOutput("votTable")),
		
		box(width=4, uiOutput("link2data"), br(), textOutput("citation"))
		)
		)
	)


# Define server logic to plot various variables ----
server <- function(input, output) {

	voice_subset <- reactive({
		if (input$votcati == "all") {
			mydat <- means
			axismin <- -180
			axismax <- 160
			textx <- 130
			texty <- -170
			}
		else if (input$votcati == 'long.lag') {
			mydat <- subset(means, vot.category==input$votcati)
			axismin <- 25
			axismax <- 160
			textx <- 147
			texty <- 30
			}
		else if (input$votcati == 'short.lag') {
			mydat <- subset(means, vot.category==input$votcati)
			axismin <- -10
			axismax <- 45
			textx <- 40
			texty <- -8.5
			}
		else {			
			mydat <- subset(means, vot.category==input$votcati)
			axismin <- -180
			axismax <- 10
			textx <- -10
			texty <- -175
			}

		mydat$selected <- ifelse(!(mydat$language %in% input$mylanguages | mydat$family %in% input$myfamilies), 'not selected', 'selected')
		mydat$selected <- factor(mydat$selected, levels=c('selected', 'not selected'))
		
		
		if ('selected' %in% mydat$selected) {
			mycolors = c('firebrick1', 'gray')
			mysizes = c(2.5,2)
			myalphas = c(1, 0.5)
			} else {
			mycolors = c('black')
			mysizes = c(2.5)
			myalphas = c(1)
			}
		
		if (input$showContrasts == TRUE) {
			mydat$tmpContrast <- mydat$contrast
			mydat$tmpContrast <- factor(mydat$tmpContrast, levels=c('yes', 'no'))
			myshapes = c(16, 4)
			} else {
			mydat$tmpContrast <- 'NA'
			myshapes = c(16)
			}
		
		mydat[,colnames(mydat)%in%c('labial','coronal','dorsal')] <- round(mydat[,colnames(mydat)%in%c('labial','coronal','dorsal')], 1)
		
		mycor <- cor.test(~mydat[,colnames(mydat)==input$place1] +  mydat[,colnames(mydat)==input$place2])$estimate
  		mycor <- paste0('r = ', round(mycor, 2))
  		
		subsetTerms <- list(mydat = mydat, axismin = axismin, axismax = axismax, mycolors = mycolors, mysizes = mysizes, myalphas = myalphas, mycor = mycor, textx = textx, texty = texty, myshapes = myshapes)
		return(subsetTerms)
		})		
		
    	
  # Generate a plot of the requested variables ----
  	output$stopPlot <- renderPlot({
  		voiceSubset <- voice_subset()
  		mydat <- voiceSubset$mydat
  		axismin <- voiceSubset$axismin
  		axismax <- voiceSubset$axismax
  		mycolors <- voiceSubset$mycolors
  		mysizes <- voiceSubset$mysizes
  		myalphas <- voiceSubset$myalphas
  		myshapes <- voiceSubset$myshapes
  		mycor <- voiceSubset$mycor
  		textx <- voiceSubset$textx
  		texty <- voiceSubset$texty
  		
  		ggplot(mydat) + 
  		geom_point(aes(x = mydat[,colnames(mydat)==input$place1], y = mydat[,colnames(mydat)==input$place2], color=selected, size=selected, alpha=selected, shape=tmpContrast)) + 
  		xlab(input$place1) + 
  		ylab(input$place2) + geom_smooth(aes(mydat[,colnames(mydat)==input$place1], mydat[,colnames(mydat)==input$place2]), method=lm, color = "black", size = 0.75, se=TRUE) + 
  		theme_bw(20) +
  		scale_colour_manual(values = mycolors) +
  		scale_size_manual(values=mysizes) + 
  		scale_alpha_manual(values=myalphas) + 
  		scale_shape_manual(values=myshapes) +
  		guides(color=F, size=F, alpha=F, shape=F) + 
  		scale_x_continuous(limits=c(axismin, axismax)) + 
  		scale_y_continuous(limits=c(axismin, axismax)) + 
  		annotate("text", x = textx, y = texty, size = 6, label = mycor, fontface = "italic")
  })
  
		
	output$hover_info <- renderText({
  		voiceSubset <- voice_subset()
  		mydat <- voiceSubset$mydat
		
		if(!is.null(input$plot_hover)) {
		hover=input$plot_hover
		hoverpoint = which.min( (mydat[,colnames(mydat)==input$place1]-hover$x)^2 + (mydat[,colnames(mydat)==input$place2]-hover$y)^2 )
		
		lgName = as.character(mydat$language[hoverpoint])
		
		place1vot = paste0(input$place1, ': ', mydat[,colnames(mydat)==input$place1][hoverpoint], ' ms')
		
		place2vot = paste0(input$place2, ': ', mydat[,colnames(mydat)==input$place2][hoverpoint], ' ms')
		
		contrast = paste0('contrast: ', mydat$contrast[hoverpoint])
		paste(lgName, place1vot, place2vot, contrast, sep='\n')
		} else {
		paste('Hover for more info', ' ', ' ', ' ', sep='\n') }
    })

	output$votTable <- renderDataTable(datatable({
		voiceSubset <- voice_subset()
  		mydat <- voiceSubset$mydat
  		mydat$dialect <- NULL
  		colnames(mydat) <- c('family', 'language', 'category', 'contrast', 'labial', 'coronal','dorsal')
		mydat[,1:7]}, options=list(scrollX=T, list(width='50%', targets=c(4))), rownames=F))
		
	
		url <- a("OSF", href="https://osf.io/ry3en/?view_only=39da1fe1f24c4c8191007d60f3364af0", target="_blank")
		output$link2data <- renderUI({
		tagList("Download the data here: ", url)})
		
		output$citation <- renderText({
		paste("Please cite the following for use of this data:", "Chodroff, E., Golden, A., and Wilson, C. (2019). Evidence for a universal constraint on phonetic realization: Covariation of stop voice onset time across languages. The Journal of the Acoustical Society of America 145(1), EL109-EL115.", sep="\n")
		})
  }
  
  
  # Create Shiny app ----
shinyApp(ui, server)