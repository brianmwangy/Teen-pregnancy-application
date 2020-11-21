
###############loading libraries################
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(RColorBrewer)
source("global.R")

################Overview UI module#########
uioverview<-function(id){
  ns<-NS(id)
  tabPanel("Overview",
           fluidRow(
             column(6,
                    img(src ="preg.jpg", width = "80%"),
                    includeMarkdown("about.md")
             ),
             column(6,
                    h4(tags$b("Data")),
                    h5(tags$p("The teen pregnancy data is obtained from the DHIS2 July 2020 courtesy of Women in GIS Kenya.
                                  It is from January 2016 to July 2020. ")),
                    h5(tags$p("The plot belows shows the total teen pregnancy trends quarterly from 2016Q1 to 2020Q2 at the national level.")),
                    plotlyOutput(ns("nation"),width = 600,height = 400),
                    h5(tags$p("The pregnancy rates for teens between 15 to 19 years is higher compared to those between 10 to 14 years.")),
                    h5(tags$p("The third quarter of 2018 has the highest teen pregnancies over the years.")),
                   h5(tags$p("The teen pregnancies decreases drastically from Jan to June 2020."))
                    
             )
           )
           
  )
}#end module


##############map UI module###########
mapui<-function(id){
  ns<-NS(id)
  tabPanel("Map",
           
           fluidRow(
             column(6,
                    h5(tags$p("The map shows the total teen pregnancy (10 to 19 years) for 2016 to 2020Q2 per county.")),
                    selectizeInput(ns("year"),label = h5(tags$b("Year")),choices =c("2016","2017","2018","2019","2020")) 
             )
           ),
          fluidRow(
            leafletOutput(ns("imap"),height = 450)
          ) 
           )
}

#####################chart UI module##########
chartui<-function(id){
  ns<-NS(id)
    tabPanel("Charts",
             fluidRow(
               column(3,
                      selectizeInput(ns("year"),label = h5(tags$b("Year quarter")),choices =yr) 
               ),
               column(3,
                      selectizeInput(ns("pregnancy"),label = h5(tags$b("Teen Pregnancy")),
                                     choices = preg)
               ),
               column(3,
                      selectizeInput(ns("county"),label = h5(tags$b("County")),choices = cnty)
               ),
               column(3,
                      selectizeInput(ns("subcnty"),label = h5(tags$b("SubCounty")),choices =NULL)
               )
             ),
             
             fluidRow(
               tabBox(
                 width = 12,
                 tabPanel(h5("Trends by County"),
                          fluidRow(
                            column(6,
                                   plotlyOutput(ns("countie"),height = 580)),
                            column(6,
                                   plotlyOutput(ns("tpcountie"),height = 580)
                                   )
                                   )
                           
                          ),
                 tabPanel(h5("Trends by Subcounty"),
                          column(8,
                                 fluidRow(plotlyOutput(ns("subcountie"),height = 600))  
                                 ),
                          column(4,
                                 fluidRow(plotlyOutput(ns("tpsubcountie"),height = 600))
                                 )
                          
                          ),
                 tabPanel(h5("Trends by Ward"),
                          column(8,
                                 plotlyOutput(ns("tmwrd"),height = 600)
                                 ),
                          column(4,
                                 plotlyOutput(ns("wrd"),height = 600)     
                          ) 
                          
                          ),
                 tabPanel(h5("Data"),dataTableOutput(ns("table")))
               )
               
             )
            
    )#end chart tab
  
}


################About UI#############

aboutui<-function(id){
  tabPanel("About me",
           #fluidRow(
             
             column(4,
                    h3(strong("Brian Mwangi"),style="text-align:center"),
                    img(src ="profile.jpg", width = "60%",style = "display: block; margin-left: auto; margin-right: auto;"),
            
                    ),
             
             column(8,
                    
                    h3(strong("Description"),style="text-align:center"),
                    h4(tags$p("Currently, I'm a Research Assistant at the Aquaya Institute but open to new opportunities in Data science.
                              My key tasks include data wrangling, visualization and presentation to key stakeholders for informed decision making.")
                       
                    ),
                    h4(tags$p("I love gathering, interpreting and transforming data into actionable insights. Data is the oil of the 21st century
                    and analytics is the combustion engine.")
                       ),
                    h4(tags$p("Interests:"),tags$li("Data wrangling."),tags$li("Data visualization."),tags$li("Machine Learning."),
                       tags$li("Computer Vision.")),
                    h4(tags$p("Social Contacts:")),
                    h4(
                      tags$a(
                        href = 'https://www.linkedin.com/in/brianmwangi/',
                        img(
                          src = 'LinkedIn.png',
                          height = "50px"
                        )
                      ),
                      HTML('&nbsp;'),
                      tags$a(
                        href = 'mailto:brynmwangy@gmail.com',
                        img(
                          src = 'mail2.png',
                          height = "50px"
                        )
                      ),
                      HTML('&nbsp;'),
                      tags$a(href = 'https://github.com/brynmwangy', img(
                        src = 'github.jpg',
                        height = "50px"
                      )),
                      HTML('&nbsp;'),
                      tags$a(href = 'https://medium.com/@itsbrianmwangi', img(
                        src = 'medium.png',
                        height = "50px"
                      ))
                      
                    )
                    )
             
          # )
           
           )
  
}

###############map server module########
mapserver<-function(id){
  moduleServer(id,function(input,output,session){
    pal<-colorNumeric(palette = "Reds",domain = NULL)
    
    selectedYear<-reactive({
      switch(input$year,
             "2016"=bsmap$Preg16,
             "2017"=bsmap$Preg17,
             "2018"=bsmap$Preg18,
             "2019"=bsmap$Preg19,
             "2020"=bsmap$Preg20
             
             )
    })
    
    output$imap<-renderLeaflet({
      leaflet(
        options = leafletOptions(minZoom = 6, maxZoom = 10)
      ) %>% 
      #setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      fitBounds(lng1 = 33.908859, lat1 = -4.678047, lng2 = 41.899078, lat2 = 5.019938) %>% 
      #addTiles()%>%
      #addProviderTiles(providers$Stamen.TonerLite) %>% 
      #addMapPane("polygons",zIndex = 410)%>%
      #addMapPane("borders",zIndex = 420)%>%
        addPolygons(
          data = bsmap,
          weight = 1,
          fillColor =~pal(selectedYear()),
          opacity = 1,
          fillOpacity = 1,
          label = ~ADM1_EN,
          popup = ~paste("County:", ADM1_EN,
                         "<br>",
                         "Teen Pregnancy:",selectedYear())
        ) %>% 
        addLegend(title = "Teen Pregnancy",pal = pal,values=selectedYear(),position = "bottomright")
       
    })# end of rendering the basemap
    
    
    # observeEvent(input$year,
    #           leafletProxy("imap")%>%
    #             addPolygons(
    #               data = selectedYear(),
    #               fillColor = ~pal(selectedYear())
    #             )
    # 
    #              )
  })
}

##############chart server module###########
chartserver<-function(id){
  moduleServer(id,function(input,output,session){
    
    #########ward filter############
    ward_filter<-reactive({
      wardf %>% filter(subcounty==input$subcnty & Type==input$pregnancy)
    })
    
    ##########subcounty filter##########
    subcounty_filter<-reactive({
      subcounty  %>% filter(county==input$county & type==input$pregnancy)
      
    })
    
    ##############top subcounty filter#########
    yrpregsb<-reactive({
      subcounty_filter() %>% filter(period==input$year)
    })
    
    #############county filter########
    county_filter<-reactive({
        county %>% filter(county==input$county & type==input$pregnancy)
    })
    #######reactive function for year and pregnancy#######
    yrpreg<-reactive({
      wardf %>% filter(period==input$year & Type==input$pregnancy)
    })
    
    ##############reactive function for top counties quarterly###########
    yrpreg2<-reactive({
      county %>% filter(period==input$year & type==input$pregnancy)
    })
    ##########reactive function for county######
    cntyreact<-reactive({
      yrpreg() %>% filter(county==input$county)
    })
    
    
    ############reactive function for subcounty##########
    sbcntyreact<-reactive({
      cntyreact() %>% filter(subcounty==input$subcnty)
    })
    ###########update select input for subcounty#######
    observeEvent(input$county,{
      updateSelectizeInput(session,"subcnty", choices=unique(cntyreact()$subcounty[cntyreact()$county==input$county]))
    })
    
    ##########update select input for ward###########
    # observeEvent(input$subcnty,{
    #   updateSelectizeInput(session,"ward", choices = unique(sbcntyreact()$ward[sbcntyreact()$subcounty==input$subcnty]))
    # })
    
    #######rendering the table outpu#############
    output$table<-renderDataTable({
      sbcntyreact()
    })
    
    ##################trends by ward time series plot output#######
    output$tmwrd<-renderPlotly({
      ggplotly(
        ggplot(data = ward_filter(),aes(x=period,y=Pregnancy))+
          geom_line(aes(color=Type))+
          labs(
            title = "Teen pregnancy trends per ward from 2016Q1 to 2020Q2",
            x="Yearly quarters",
            y="Teen pregnancies"
          )+
          facet_wrap(~ward)+
          scale_x_yearqtr(n=15)+
          theme(
            axis.text.x = element_text(angle = 45),
            plot.title = element_text(size = 10)
          )+
          guides(
            color=FALSE
          )
      ) %>% layout(legend = list(
        orientation = "h",x=0.3,y=-0.1
      )) 
      
    }) ###end
    
    
    ##################trends by ward bar plot output##############
    output$wrd<-renderPlotly({
      ggplotly(
        ggplot(data = sbcntyreact(),aes(x=reorder(ward,Pregnancy),y=Pregnancy))+
          geom_col(fill="red")+
          labs(
            title = "Total teen pregnancies quarterly per ward",
            x="ward",
            y="Teen pregnancy"
          )+
          facet_wrap(~subcounty)+
          coord_flip()+
          theme(
            axis.title.y= element_blank(),
            plot.title = element_text(size = 10)
          )
      )
    })
    
    #####################trends by county timeseries plot output###########
    output$countie<-renderPlotly({
      ggplotly(
        ggplot(data = county_filter(),aes(x=period,y=pregnancy))+
          geom_line(aes(color=type))+
          labs(
            title = "Teen pregnancy trends per county from 2016Q1 to 2020Q2",
            x="Yearly quarters",
            y="Pregnancies"
          )+
          facet_wrap(~county)+
          scale_x_yearqtr(n=15)+
          theme(
            axis.text.x = element_text(angle = 45),
            plot.title = element_text(hjust = 0.5,size = 10),
            axis.text.y= element_text(face = "bold")
            
          )+
          guides(
            color=FALSE
          )
      ) %>% 
        layout(legend = list(
          orientation = "h",x=0.3,y=-0.2
        )) 
      
    }) ###end
    
    
    ###################trends by county top counties bar plot output############
    output$tpcountie<-renderPlotly({
      ggplotly(
        yrpreg2() %>% arrange(desc(pregnancy)) %>% top_n(20) %>%
        ggplot(aes(x=reorder(county,pregnancy),y=pregnancy))+
          geom_col(fill="red")+
          labs(
            title = "Top 20 counties with the highest teen pregnancies quarterly",
            x="county",
            y="Teen pregnancy"
          )+
          coord_flip()+
        theme(
          plot.title = element_text(size = 10)
        )
      )
    })
    
    
    
    #################trends by subcounty time series plot output##########
    output$subcountie<-renderPlotly({
      ggplotly(
        ggplot(data = subcounty_filter(),aes(x=period,y=pregnancy))+
          geom_line(aes(color=type))+
          labs(
            title = "Teen pregnancy trends per subcounty from 2016Q1 to 2020Q2",
            x="Yearly quarters",
            y="Teen pregnancies"
          )+
          facet_wrap(~subcounty)+
          scale_x_yearqtr(n=15)+
          theme(
            axis.text.x = element_text(angle = 45),
            plot.title = element_text(size = 10)
          )+
          guides(
            color=FALSE
          )
      ) %>% layout(legend = list(
        orientation = "h",x=0.3,y=-0.1
      )) 
      
    }) ###end
    
    ######################trends by subcounty top subcounties bar plot output########
    output$tpsubcountie<-renderPlotly({
      ggplotly(
        yrpregsb() %>% arrange(desc(pregnancy)) %>%
          ggplot(aes(x=reorder(subcounty,pregnancy),y=pregnancy))+
          geom_col(fill="red")+
          labs(
            title = "Total teen pregnancies quarterly per subcounty",
            x="subcounty",
            y="Teen pregnancy"
          )+
          coord_flip()+
          theme(
            plot.title = element_text(size = 10)
          )
      )
    })
    
    
  })
}

##############overview server module##############
serveroverview<-function(id){
  moduleServer(id, function(input,output,session){
    
    ############national trend teen pregnancy#########
    output$nation<-renderPlotly({
      ggplotly(
        ggplot(data = national,aes(x=period, y=pregnancy))+
          geom_line(aes(color=type),size=0.5)+
          scale_color_manual(values=c("red","blue","green"),
                             labels=c("Teen pregnancy (10-19years)","Teen pregnancy (15-19years)","Teen pregnancy (10-14years)"))+
          labs(
            title = "Teen pregnancy trends from 2016Q1 to 2020Q2",
            x="Yearly quarters",
            y="Teen pregnancies"
            
          )+
          scale_x_yearqtr(n=10)+
          theme_gray()+
          theme(
            plot.title = element_text(size = 10),
            legend.title = element_blank(),
            legend.text = element_text(size = 6),
            axis.title = element_text(face = "bold",size = 9),
            axis.text = element_text(face = "bold",size = 7)
            
          )
      ) %>%
        layout(legend = list(
          orientation = "h",x=0.3,y=-0.2
        )) 
    })
  }
    )
}

aboutserver<-function(id){
  moduleServer(id,function(input,output,session){
    
  })
}
# Define UI for application 
ui <- navbarPage(
    title = "Teen Pregnancy in Kenya",
    theme = shinytheme("flatly"),
    uioverview("intro"),
    mapui("map"),
    chartui("chart"),
    aboutui("about")
   
    
    
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 serveroverview("intro")
  mapserver("map")
  chartserver("chart")
  aboutserver("about")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
