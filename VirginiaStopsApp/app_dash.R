#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readr)
library(sf)
library(plotly)
library(tidycensus)
library(tidyverse)


setwd("C:\\Users\\fwunschel\\OneDrive - Vera Institute of Justice\\Local Projects\\Charlottesville\\VA_App\\VirginiaStopsApp")
VA<-read_csv('VA_Stops_Set.csv')
VA<-VA[-1]
VA$Value<-1
timeOrder<-c(paste(month.name,'2020'),paste(month.name,'2021'))
VA$RaceEthnicity[grep('black',VA$race,ignore.case = T)]<-'BLACK'
`%notin%`<-Negate(`%in%`)


VA_SC<-read_sf('Virginia.shp')
VA_SC$fillValue<-'Off'

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = 'Virginia Stops Data'),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Statewide", tabName = "statewide", icon = icon("vuejs")),
            menuItem("City/County Specific", tabName = "widget", icon = icon("dashboard"))
        )
        
    ),
    dashboardBody(
                     tabItems(
                         tabItem(tabName = 'statewide',
                                 plotlyOutput('IntPlot')),
                         tabItem(tabName = 'widget',
                                 tags$head(tags$style(HTML(".fa{font-size: 50%;}"))),
                                 fluidRow(
                                     column(6,selectInput(inputId = 'LocationList',label = 'Pick a County or City',choices = c('All',sort(unique(VA$Location))))),
                                     column(6,dateRangeInput("daterange", "Date range:",start= range(VA$incident_date)[1],end= range(VA$incident_date)[2],min=range(VA$incident_date)[1],max=range(VA$incident_date)[2]))),
                                 fluidRow(column(12,plotOutput('mapPlot'))),
                                 fluidRow(valueBoxOutput('StopsBox',width = 3),valueBoxOutput('SearchBox',width = 3),valueBoxOutput('ArrestBox',width = 3),valueBoxOutput('CitationBox',width = 3)),
                                 tabBox(width = 12,
                                        tabPanel('Volume',plotOutput('StopsPlot1'),plotOutput('ActionPlot'),plotOutput('ReasonPlot')),
                                        tabPanel('Race',plotOutput('REStopsPlot'),plotOutput('REReasonPlot'),plotOutput('REActionPlot'),tableOutput('RNTab')),
                                        tabPanel('Gender/Age',plotOutput('GStopsPlot'),plotOutput('GAge'),plotOutput('GReasonPlot'),plotOutput('GActionPlot'))))
                         
                         
                     )
                     
            )
        )
    
            
        
        
    
    
    


        # Show a plot of the generated distribution
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data<-reactive({ 
        if(input$LocationList=='All'){
        a<-VA
        }else{
         a<-VA[which(VA$Location==input$LocationList),]   
        }
        d<-which(a$incident_date>=input$daterange[1]&a$incident_date<=input$daterange[2])
        ad<-a[d,]
        ad
    })
    
    spatialdata<-reactive({ 
        a<-VA_SC
        
        if(input$LocationList!='All'){
            a$fillValue[which(VA_SC$Locatin==input$LocationList)]<-'On' 
        }
        a
    })
    
    output$StopsBox <- renderValueBox({
        
        valueBox(value = tags$p(ifelse(input$LocationList=='All',nrow(VA),nrow(data())), style = "font-size: 50%;"),#quick fix because nrow wasn't working for some reason
                 subtitle=tags$p('Stops', style = "font-size: 70%;"),
                 icon = icon("siren")
                 
                 
                 )
    })
    output$SearchBox <- renderValueBox({
        valueBox(
            value = tags$p(sum(data()$Search=='YES',na.rm = T), style = "font-size: 60%;"),
            subtitle = tags$p('Person or Vehicle Searches', style = "font-size: 70%;"),
            icon = icon("car fa-lg"),
            color = 'blue'
        )
    })
    output$ArrestBox <- renderValueBox({
        valueBox(value = tags$p(sum(data()$action_taken=='ARREST',na.rm = T), style = "font-size: 60%;"), 
                 subtitle=tags$p('Arrests from Stop', style = "font-size: 70%;"),
                                       color = 'red')
    })
    output$CitationBox <- renderValueBox({
        valueBox(value = tags$p(sum(data()$action_taken=='CITATION/SUMMONS',na.rm = T), style = "font-size: 60%;"),
                 subtitle=tags$p('Citations or Summons from Stop', style = "font-size: 70%;"),
                 color = 'yellow')
    })
    output$mapPlot<-renderPlot({
        set<-spatialdata()
        
        p<-ggplot(spatialdata(),aes(fill=fillValue))+geom_sf()+lims(x=c(-84,-75),y=c(36.5,39.6))+scale_fill_manual(guide='none',values=c('orange','darkblue'))
        p1<-p+theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),panel.background = element_rect("transparent",color=NA), # bg of the panel
                plot.background = element_rect("transparent", color = NA),axis.line = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())
        atitle<-ifelse(input$LocationList=='All',' The State of Virginia Stops Data',paste(input$LocationList,', Virginia Stops Data'))
        p1+labs(title = atitle,subtitle = paste(input$daterange[1],'-',input$daterange[2]))
    })

    ##Volume Tab
    output$StopsPlot1<-renderPlot({
        base<-ggplot(data=data(),aes(x=MonthYear))+geom_bar()+scale_x_discrete(limits=timeOrder[timeOrder%in%data()$MonthYear])
        base+labs(y='Number of Stops')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 30,hjust=1))
    
        })
    output$ActionPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=MonthYear,fill=action_taken))+geom_bar()+scale_x_discrete(limits=timeOrder[timeOrder%in%data()$MonthYear])
        base+labs(y='Number of Stops',fill='Stop Outcome')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 30,hjust=1))
    })
    output$ReasonPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=MonthYear,fill=reason_for_stop))+geom_bar()+scale_x_discrete(limits=timeOrder[timeOrder%in%data()$MonthYear])
        base+labs(y='Number of Stops',fill='Stop Source')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 30,hjust=1))
    })
    
    ##Race Based tab
    output$REStopsPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=MonthYear, fill=RaceEthnicity))+geom_bar()+scale_x_discrete(limits=timeOrder[timeOrder%in%data()$MonthYear])
        base+labs(y='Number of Stops',fill='Race/Ethnicity')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 30,hjust=1))
    
        })
    output$REActionPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=RaceEthnicity, fill=action_taken))+geom_bar()
        base+labs(y='Number of Stops',fill='Stops Outcome')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 15,hjust=1))
        
    })
    output$REReasonPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=RaceEthnicity, fill=reason_for_stop))+geom_bar()
        base+labs(y='Number of Stops',fill='Stop Source')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 15,hjust=1))
    })
    
    ##Gender
    output$GStopsPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=MonthYear, fill=gender))+geom_bar()+scale_x_discrete(limits=timeOrder[timeOrder%in%data()$MonthYear])
        base+labs(y='Number of Stops',fill='Gender')+theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 30,hjust=1))
        
    })
    output$GActionPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=gender, fill=action_taken))+geom_bar()
        base+labs(y='Number of Stops',fill='Stops Outcome')+theme(axis.title.x = element_blank())
    })
    output$GReasonPlot<-renderPlot({
        base<-ggplot(data=data(),aes(x=gender, fill=reason_for_stop))+geom_bar()
        base+labs(y='Number of Stops',fill='Stop Source')+theme(axis.title.x = element_blank())
    })
    
    output$GAge<-renderPlot({
        base<-ggplot(data=data(),aes(x=age))+geom_histogram()
        base+labs(y='Number of Stops',x='Age')
    })
    ###render plotly
    output$IntPlot<-renderPlotly({
        set<-spatialdata()
        
        p<-ggplot(spatialdata(),aes(text=paste0(
            Location,', Virginia','\n',
            'Number of Stops: ',NStops,'\n',
            'Number of Searches: ',NSearches,'\n',
            'Percent White: ',100*round(PropWht,3),'%')))+geom_sf(fill='orange')+lims(x=c(-84,-75),y=c(36.5,39.6))+scale_fill_manual(guide='none',values=c('orange','darkblue'))
        p1<-p+theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),panel.background = element_rect("transparent",color=NA), # bg of the panel
                    plot.background = element_rect("transparent", color = NA),axis.line = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())
        p2<-p1+labs(title = 'The State of Virginia Stops Data')
        pl<-ggplotly(p2,tooltip = 'text')
        pl
    })
    
    output$RNTab<-renderTable({
        a<-xtabs(~RaceEthnicity+MaxDays,VA)
        b<-a[rownames(a)%in%c('BLACK','HISPANIC','WHITE'),colnames(a)%notin%c(60,365,1095)]
        cn<-c('Fine',10,'6 Months','~1 Year','~5 Years','10 Years','20 Years','30 Years','40 Years','100 Years/Life')
        cnum<-c(-1,10,180,360,1825,3650,7300,10950,14600,36500)
        c1<-cbind(rownames(b),as.matrix.data.frame(b,rowname.force=T))
        colnames(c1)<-c('Race',cnum[cnum%in%colnames(b)])
        c1
    })
    output$RPTab<-renderTable({
        a<-xtabs(~RaceEthnicity+MaxDays,data())
        b<-a[rownames(a)%in%c('BLACK','HISPANIC','WHITE'),colnames(a)%notin%c(60,365,1095)]
        cn<-c('Fine',10,'6 Months','1 Year','5 Years','10 Years','20 Years','30 Years','40 Years','100 Years/Life')
        cnum<-c(-1,10,180,360,1825,3650,7300,10950,14600,36500)
        colnames(b)<-cn[which(cnum%in%colnames(b))]
        as.matrix(round(prop.table(b,1),3),rowname=T) 
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)


