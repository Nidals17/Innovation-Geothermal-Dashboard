# short script
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)  
library(shinycssloaders)
library(igraph)

#setwd("C:/Users/nidal/OneDrive/Documents/Dash")
df <- read.csv("df.csv",sep=";") %>% select(-"Column1",-"authors",-"keywords",-"conference")
df$beta <- as.numeric(gsub(",",".",df$beta))

comp_hist = read.csv("comp_hist.csv",sep=",")


ui = dashboardPage(
  
  dashboardHeader(title="Co-evolution of technologies and science on Geothermal patent", titleWidth = 650
                  
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the type" , choices = c("patent","paper"))),
                menuItem("Network", tabName = "net",icon = icon("network-wired"))
                
                
    )
  ),
  
  
  dashboardBody(
    
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 8,imageOutput("picture", height = "500",width="1000"),
                                       tags$br() , 
                                       tags$a(""), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p(style = "font-size: 12pt;","This dashboard presents an analysis of the scientific and technological dynamics related to geothermal energy. We aim to understand and quantify the impact of a paper or patent in the field of science and technology.",align="center"),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$b(style = "font-size: 12pt;","Nidal Souk"),
                                       tags$br(),
                                       tags$b(style = "font-size: 12pt;","Farhan Acalasow"),
                                       tags$br(),
                                       tags$b(style = "font-size: 12pt;","Paul Cantin"),
                                       tags$br(),
                                       tags$b(style = "font-size: 12pt;","Verlaine Koubikani")
                                       
                                )
                              )
                              
                              
                     ), 
                     tabPanel("Data",value = "DATAs", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("Summary Stats", icon=icon("chart-pie"),
                              verbatimTextOutput("summary"),width=350,
                              fluidRow(
                                column(width = 3, plotOutput("PieChart")),
                                column(width = 9, plotlyOutput("Countrybar"))))
              )),  
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Papers and Patents", value="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              withSpinner(plotlyOutput("bar"))
                     ),
                     tabPanel("Ratio", 
                              radioButtons(inputId ="fit" , label = "Select Between Sum or Cumsum Ratio" , choices = c("Sum", "Cumsum"), selected = "Sum" , inline = TRUE), 
                              withSpinner(plotlyOutput("scatter")), value="relation",tags$br(),tags$br(),tags$p(style = "font-size: 14pt;","The paper-to-patent ratio refers to the number of publications compared to the number of patents that have been filed or granted in the field of geothermal energy. This ratio gives an idea of the level of research and development activity in this industry, as well as the emphasis placed on publishing research results versus protecting intellectual property through patents.")),
                     tabPanel("Innovation", withSpinner(plotOutput("CompInno")),tags$br(),tags$br(),tags$p(style = "font-size: 14pt;","This is a graph that focuses on the variable compo_inno, meaning whether a term is considered innovative. It can be seen that only 11 terms are considered innovative.In the dashboard, you can select the term you wish to view.")),
                     tabPanel("Innovation/term",selectInput("termInno", "Select an option:",choices = unique(comp_hist %>%filter(comp_inno == 1)%>%select(term))$term),withSpinner(plotOutput("CompInnoTerm")))
                     
              )),
      tabItem(tabName = "net",
              tabBox(id="t3",width = 12,
                     tabPanel("Technology Network",withSpinner(plotOutput("TechNet",width = "800px", height = "600px")),tags$br(),tags$br(),tags$p(style = "font-size: 14pt;","Graphical representation of the relationships or connections between different terms or concepts related to the field of geothermal energy.The nodes of the network represent technological terms and the edges represent the relationships between them.These relationships are based on similarity, co-occurrence,or causal relationships. These networks can be used to understand the structure of knowledge, identify key concepts, and explore association patterns.")),
                     tabPanel("Scientific Network",withSpinner(plotOutput("ScientNet",width = "800px", height = "600px")),tags$br(),tags$br(),tags$p(style = "font-size: 14pt;","Graphical representation of the connections between different terms related to the field of geothermal energy.The nodes of the network represent Scientifical terms and the edges represent the relationships between them.These relationships are based on similarity, co-occurrence,or causal relationships"))
                     
              ))
      
      
    )
    
  ))

server = function(input, output){
  
  # Data table Output
  
  data_for_chart <- comp_hist %>%
    filter(comp_inno == 1) %>% 
    select(year, first_year_mentioned, type,term)
  
  df4 <- data_for_chart %>% 
    group_by(first_year_mentioned, type) %>%
    summarize(count = n()) 
  
  df5 <- data_for_chart %>% 
    group_by(year, type) %>% 
    summarize(count1 = n()) 
  
  
  output$dataT <- renderDataTable(df)
  
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 years with the most ", input$var2, "Done")
  )
  
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 years with the least ", input$var2, "Done")
  )
  
  
  output$top5 <- renderTable({
    
    df %>% 
      select(year, type) %>% group_by(year,type) %>% filter(type == input$var2) %>% 
      summarise(count=n()) %>% arrange(desc(count)) %>% head(5)
  })
  
  output$low5 <- renderTable({
    
    df %>% 
      select(year, type) %>% group_by(year,type) %>% filter(type == input$var2) %>% 
      summarise(count=n()) %>% arrange(count) %>% head(5)
    
    
  })
  
  
  # For Structure output
  output$structure <- renderPrint({
    df %>% 
      str()
  })
  
  output$PieChart = renderPlot({
    ggplot(data.frame(type = c("Patent", "Paper"), count = c(sum(df$type=="patent"), sum(df$type=="paper"))), aes(x = "", y = count, fill = type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      ggtitle("Number of Patents and Papers") +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
      theme_void()+
      scale_fill_discrete(name = "Type", guide = guide_legend(title.position = "left",title.theme=element_text(size = 10)))
  })
  
  
  # For Summary Output
  output$summary <- renderPrint({
    df %>% 
      summary()
  })
  
  
  output$Countrybar <- renderPlotly({
    df %>%select(country,type) %>% group_by(country,type)  %>% na.omit() %>%as.data.frame() %>% ggplot(aes(x = country,fill=factor(type))) + geom_bar() + 
      scale_fill_discrete(name = "Type", guide = guide_legend(title.position = "left",title.theme=element_text(size = 10))) +
      labs(x = "Country",y="") +
      ggtitle("Number of patents by Country") + 
      theme(legend.position = "none")
    
  })
  
  output$picture <- renderImage({
    return(list(src = "geo.PNG",contentType = "image/png",alt = "Alignment"))
  }, deleteFile = FALSE) #where the src is wherever you have the picture
  
  
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlotly({
    df %>% group_by(year,type) %>% ggplot(aes(x = year,fill=factor(type))) + geom_bar() + 
      scale_fill_discrete(name = "Type", guide = guide_legend(title.position = "left",title.theme=element_text(size = 10))) +
      labs(x = "Year",y="Number") +
      ggtitle("Number of patents and papers per year") +
      guides(fill=guide_legend(override.aes = list(x = 0.5, y = -0.1))) +
      theme(legend.position = "bottom", 
            legend.justification = 0.5, # align center
            legend.box = "horizontal")
    
  })
  
  data_for_chart <- comp_hist %>%
    filter(comp_inno == 1) %>% 
    select(term, n, year, first_year_mentioned, type)
  df4 <- data_for_chart %>% 
    group_by(first_year_mentioned, type) %>%
    summarize(count = n()) 
  df5 <- data_for_chart %>% 
    group_by(year, type) %>% 
    summarize(count1 = n()) 
  
  output$CompInno = renderPlot({
    ggplot() +
      geom_segment(data = df4 %>% filter(type=="paper"), aes(x=first_year_mentioned,y=0,xend = first_year_mentioned, yend = count,color = "red"),size=1) +
      geom_segment(data =df4%>%filter(type=="patent"), aes(x=first_year_mentioned+0.5,y=0,xend = first_year_mentioned+0.5, yend = count,color = "blue"),size=1) +
      geom_segment(data =df5%>% filter(type=="paper"), aes(x=year+0.5,y=0,xend = year+0.5, yend = count1,color = "blue"),linetype="dashed",size=1) +
      geom_segment(data =df5%>% filter(type=="patent"), aes(x=year+0.8,y=0,xend = year+0.8, yend = count1,color = "red"),linetype="dashed",size=1) +
      scale_linetype_manual(name="typee",values = c("solid","Dashed" )) +
      scale_color_discrete(name = "type", labels = c("paper","patent")) +
      labs(x= "Year", y="",title = "Component Innovation: Appearance and Diffusion")
  })
  
  output$CompInnoTerm = renderPlot({
    ggplot() +
      geom_vline(data = data_for_chart%>%filter(term == input$termInno), aes(xintercept=first_year_mentioned,color=ifelse(type=="paper","blue","red")),size=1) +
      geom_vline(data = data_for_chart%>%filter(term == input$termInno), aes(xintercept=year,color=ifelse(type=="paper","red","blue")),size=1,linetype = "dashed") +
      labs(x= "Year", y="",title = "Component Innovation: Appearance and Diffusion") +
      xlim(1975,2020) +
      scale_color_discrete(name = "type", labels = c("paper","patent")) 
  })
  
  library(igraph)
  
  
  
  
  
  output$TechNet = renderPlot({
    dff2 = df %>% filter(beta>0.87)%>%filter(type == "patent") %>% select(document,topic)
    recomb2=merge(dff2, dff2, by = "document", all.x = TRUE)
    recomb2=distinct(recomb2)
    recomb2=recomb2 %>% select(topic.x,topic.y) %>%filter(topic.x != topic.y) %>% as.data.frame()
    
    gg2 <- graph_from_data_frame(recomb2,
                                 directed=FALSE)
    
    V(gg2)$size=degree(gg2)*3
    V(gg2)$color="LemonChiffon3"
    V(gg2)$label.color <- "black"
    V(gg2)$label.cex <- 1.2
    V(gg2)$shape = "circle"
    plot(gg2,layout=layout.fruchterman.reingold,main="Network between Technology Terms",edge.curved=FALSE,edge.color="lightblue", vertex.size=12, label.size=0.4)
  })
  
  
  output$ScientNet = renderPlot({
    dff1 = df %>% filter(beta>0.87)%>%filter(type == "paper") %>% select(document,topic)
    recomb1=merge(dff1, dff1, by = "document", all.x = TRUE)
    recomb1=distinct(recomb1)
    recomb1=recomb1 %>% select(topic.x,topic.y) %>%filter(topic.x != topic.y) %>% as.data.frame()
    
    gg1 <- graph_from_data_frame(recomb1,
                                 directed=FALSE)
    
    V(gg1)$size=degree(gg1)*3
    V(gg1)$color="lightblue"
    V(gg1)$label.color <- "black"
    V(gg1)$label.cex <- 1.2
    V(gg1)$shape = "circle"
    plot(gg1,layout=layout.random,main="Network between Scientific Terms",edge.curved=FALSE,edge.color="LemonChiffon3", vertex.size=12, label.size=0.4)
  })
  
  
  ### Scatter Charts 
  data_ratio <- df %>%
    group_by(year) %>%
    summarize(num_patents = sum(type == "patent"), num_papers = sum(type == "paper")) %>%
    mutate(ratio = num_papers / num_patents)
  data_ratio=as.data.frame(data_ratio)
  
  
  p = df %>%
    group_by(year) %>%
    summarize(num_patents = sum(type == "patent"), num_papers = sum(type == "paper")) %>%
    mutate(ratio = num_papers / num_patents) %>% 
    ggplot(aes(x=year,y=ratio)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(title = paste("Ratio of paper over patents"),
         x = "Year",
         y = "Ratio") 
  g = df %>%
    group_by(year) %>%
    summarize(num_patents = sum(type == "patent"), num_papers = sum(type == "paper")) %>%
    mutate(ratio = num_papers / num_patents) %>% mutate(cumsum_patent=cumsum(num_patents))%>%
    mutate(cumsum_paper=cumsum(num_papers))%>%mutate(ratio_cumsum=cumsum_paper/cumsum_patent)%>%ggplot(aes(x=year,y=ratio_cumsum)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(title = paste("Ratio of cumsum paper over patents"),
         x = "Year",
         y = "Ratio") 
  
  
  output$scatter <- renderPlotly({
    if (input$fit == "Sum") {
      ggplotly(p)
    } else if (input$fit == "Cumsum") {
      ggplotly(g)
    }
    
    
  })
  
  
  
}

shinyApp(ui,server)
