
## app.R ##
library(shinydashboard)
library(plotly)
library(plyr)
library(dplyr)
library(curl)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      
      column(3, textInput("college", label = h3("college"), value = "Enter text...")),
      column(3, textInput("branch", label = h3("branch"), value = "Enter text...")),
      column(3, textInput("year", label = h3("year"), value = "2012")),
      column(3, textInput("company", label = h3("company"), value = "Enter text...")),
      column(3, textInput("ctc", label = h3("ctc"), value = "Enter text...")),
      column(3, textInput("designation", label = h3("designation"), value = "Enter text...")),
      column(3, textInput("skill", label = h3("skill"), value = "Enter text...")),
      column(3, textInput("degree", label = h3("degree"), value = "B.Tech/B.E.")),
      
      h3(textOutput("caption", container = span)),
      h3(tableOutput("plot6")),
      box(plotlyOutput("avgctcminmax", height = 450)),
      box(plotlyOutput("plot2", height = 250)),
      box(plotlyOutput("plot3", height = 250)),
      box(plotlyOutput("plot4", height = 250)),
      box(plotlyOutput("topPaidSkills", height = 250))
      
    )
    # )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  data <- read.csv(file = '/home/aman/mappedData.csv', header = TRUE, fill = TRUE)
  #data = as.data.frame(data)
  
  output$topPaidSkills <- renderPlotly({
    date = as.double(input$year)
    data = data[data$UG_COURSE == input$degree, ]
    data = data[data$UG_YOC == date, ]
    data = head(arrange(data,desc(ctc)), n = 10)
    skills = c('php','javascript','ReactJs','Java','html','AngularJs','C','C++')
    browser()
    count = numeric();
    for ( i in 1:length(skills)){
      count = c(count,sum(str_count(data$skills, skills[i])))
    }
    
    p <- plot_ly(x = count, y = skills, type = 'bar', orientation = 'h', name = 'Top Paid Skills', 
                 marker = list(color = 'rgba(246, 78, 139, 0.6)',
                               line = list(color = 'rgba(240, 78, 139, 1.0)',
                                           width = 2))) %>%
      layout(barmode = 'stack',
             xaxis = list(title = "Most acquired skills"),
             yaxis = list(title ="Skills"))
    
  })
    
  output$caption <- renderText({
    h <- new_handle()
    handle_setform(h,
                   college = "118",
                   branch = "12",
                   batch = "2012"
    )
    req <- curl_fetch_memory("http://192.168.173.56:8092/hackathon/getAvgCtc", handle = h)
    json = rawToChar(req$content)
  })
  
  output$avgctcminmax <- renderPlotly({
    h <- new_handle()
    
    #date1 = paste0(as.character(as.double(input$year)-2), ".0")
    #date2 = paste0(as.character(as.double(input$year)-1), ".0")
    #date3 = paste0(as.character(as.double(input$year)), ".0")
    #date4 = paste0(as.character(as.double(input$year)+1), ".0")
    #date5 = paste0(as.character(as.double(input$year)+2), ".0")
    date1 = as.double(input$year)
    #browser()
    data = data[data$UG_YOC %in% c((date1-2):(date1+2)), ]
    browser()
    cars <- data %>%
      select(UG_YOC, ctc) %>%
      group_by(UG_YOC) %>%
      summarise(avg = mean(ctc), max = max(ctc), min = min(ctc))
    browser()
    
    #x <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
    #roW <- c(219, 146, 112, 127, 124, 180, 236, 207, 236, 263, 350, 430, 474, 526, 488, 537, 500, 439)
    #China <- c(16, 13, 10, 11, 28, 37, 43, 55, 56, 88, 105, 156, 270, 299, 340, 403, 549, 499)
    data <- data.frame(cars$UG_YOC, cars$avg, cars$max, cars$min)
    browser()
    p <- plot_ly(data, x = ~cars.UG_YOC, y = ~cars.avg, type = 'bar', name = 'Average',
                 marker = list(color = 'rgb(55, 83, 109)')) %>%
      add_trace(y = ~cars.max, name = 'Max', marker = list(color = 'rgb(26, 118, 255)')) %>%
      add_trace(y = ~cars.min, name = 'Min', marker = list(color = 'rgb(26, 218, 255)')) %>%
      layout(title = 'Minimum, Average, Maximum CTC Versus Year of Passing',
             xaxis = list(
               title = "",
               tickfont = list(
                 size = 14,
                 color = 'rgb(107, 107, 107)')),
             yaxis = list(
               title = 'USD (millions)',
               titlefont = list(
                 size = 16,
                 color = 'rgb(107, 107, 107)'),
               tickfont = list(
                 size = 14,
                 color = 'rgb(107, 107, 107)')),
             legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
             barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
  })
  output$plot6 <- renderTable({
    h <- new_handle()
    handle_setform(h,
                   college = "118",
                   branch = "12",
                   startBatch = "2000",
                   endBatch = "2013"
    )
    req <- curl_fetch_memory("http://192.168.173.56:8092/hackathon/getAvgCtcOFGivenRange", handle = h)
    json = rawToChar(req$content)
    library("rjson")
    read = fromJSON(json)
    json_data_frame <- as.data.frame(read)
    
  })
  
  output$plot5 <- renderPlotly({
    h <- new_handle()
    handle_setform(h,
                   college = "118",
                   branch = "12",
                   startBatch = "2000",
                   endBatch = "2013"
    )
    req <- curl_fetch_memory("http://192.168.173.56:8092/hackathon/getCompaniesListOfGivenRange", handle = h)
    
    json = rawToChar(req$content)
    library("rjson")
    read = fromJSON(json)
    json_data_frame <- as.data.frame(read)
    x = colnames(json_data_frame)
    y = as.list(json_data_frame[1,])
    #browser()
    print(x)
    print(as.numeric(y))
    
    #browser()
    p <- plot_ly(labels = x, values = as.numeric(y)) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Donut charts using Plotly",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot4 <- renderPlotly({
    h <- new_handle()
    handle_setform(h,
                   college = "118",
                   branch = "12",
                   batch = "2012"
    )
    req <- curl_fetch_memory("http://192.168.173.56:8092/hackathon/getCompaniesListByBatch", handle = h)
    
    json = rawToChar(req$content)
    library("rjson")
    read = fromJSON(json)
    json_data_frame <- as.data.frame(read)
    x = colnames(json_data_frame)
    y = as.list(json_data_frame[1,])
    #browser()
    print(x)
    print(y)
    
    p <- plot_ly(labels = x, values = as.numeric(y)) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Donut charts using Plotly",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot2 <- renderPlotly({
    
    x <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
    roW <- c(219, 146, 112, 127, 124, 180, 236, 207, 236, 263, 350, 430, 474, 526, 488, 537, 500, 439)
    China <- c(16, 13, 10, 11, 28, 37, 43, 55, 56, 88, 105, 156, 270, 299, 340, 403, 549, 499)
    data <- data.frame(x, roW, China)
    
    p <- plot_ly(data, x = ~x, y = ~roW, type = 'bar', name = 'Rest of the World',
                 marker = list(color = 'rgb(55, 83, 109)')) %>%
      add_trace(y = ~China, name = 'China', marker = list(color = 'rgb(26, 118, 255)')) %>%
      layout(title = 'US Export of Plastic Scrap',
             xaxis = list(
               title = "",
               tickfont = list(
                 size = 14,
                 color = 'rgb(107, 107, 107)')),
             yaxis = list(
               title = 'USD (millions)',
               titlefont = list(
                 size = 16,
                 color = 'rgb(107, 107, 107)'),
               tickfont = list(
                 size = 14,
                 color = 'rgb(107, 107, 107)')),
             legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
             barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
  })
  
  
}

shinyApp(ui, server)
