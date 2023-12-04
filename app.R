library(rsconnect)
library(shiny)
library(ggplot2)
library(tidyverse, warn.conflicts = FALSE)
library(shinyjs)
library(ggtext)


climate1<-read.csv("data/Daily_climate_1988_2022.csv", sep = ",", stringsAsFactors=FALSE)

###User Interface
ui<-fluidPage(
  useShinyjs(), 
  
  titlePanel("Climate Summaries"),
  sidebarLayout(
    sidebarPanel(
      id="sidebar",
      helpText ("Create climate summaries by selecting a date range of interest"),
      sliderInput("range", "Year", sep="",
                  min=1988, max=2022, value = c(1988,2022)),
      br(),
      br()
    ),
    mainPanel(
      verbatimTextOutput("range"),
      tabsetPanel(type = "tabs",
                  tabPanel("Hythergraph", plotOutput("plot"), htmlOutput("text4")),
                  tabPanel("Climograph", plotOutput("plot2"), htmlOutput("text5")),
                  tabPanel("Monthly Summaries", tableOutput("table"), htmlOutput("text1")),
                  tabPanel("Seasonal Summaries", tableOutput("table2"),htmlOutput("text2")),
                  tabPanel("Yearly Summaries", tableOutput("table3"),htmlOutput("text3")),
                  tabPanel("Comparisons",
                           br(),
                           fluidRow(
                             splitLayout(cellWidths=c("50%", "50%"), 
                                         wellPanel(sliderInput("range2", "Time Series 1", sep="", min=1988, max=2022, value = c(1988,2022))),
                                         wellPanel(sliderInput("range3", "Time Series 2", sep="", min=1988, max=2022, value = c(1988,2022))))),
                           p("Using the slider bars select the time intervals for the two periods of time you are interested in comparing. The climographs and hythergraph will update automatically."),
                           br(),
                           h3(p("Climographs")),
                           fluidRow(
                             splitLayout(cellWidths=c("50%", "50%"), 
                                         plotOutput("plot3"), plotOutput("plot4"))),
                           br(),
                           h3(p("Hythergraphs")),
                           plotOutput("plot5"),
                           br(),
                           htmlOutput("text6")),
                  tabPanel("About", 
                           br(),
                           h4(p("About the Climate Summaries")),
                           br(),
                           h5(p("Climate Summaries are calculated from daily average temperatures and precipitation sums at Toolik Field Station from 1988 to 2022.")),
                           h5(p("The raw data for the analysis can be downloaded from the",a("Toolik Field Station website", href ="https://www.uaf.edu/toolik"),">", a("Environmental Data Center", href = "https://www.uaf.edu/toolik/edc/index.php")," > ",a("Meterological Data Query", href = "https://www.uaf.edu/toolik/edc/monitoring/abiotic/met-data-query.php"),". You will need to fill out our", a("Conditions of Use", href ="https://www.uaf.edu/toolik/edc/about/conditions-of-use.php")," prior to downloading the data.")),
                           h5(p("We hope you find these Climate Summaries interesting and/or useful.  Let Amanda Young at ayoung55@alaska.edu know if you have any comments or questions about the summaries")),
                           p("The source code for this Shiny app is available on", a("GitHub", href = "https://github.com/amandabeayoung/Toolik-Climate-Summaries"),"."),
                  ),
                  id = "tabset"
      ),
      id="main"
    )
  )
)


### Server functions for each of the four tabs

### Monthly Table Prep

server <- function(input, output) {
  
  #### removal of sidepanel for just the Comparison tab, adapted from the stackoverflow response by Stephane Laurent
  ####https://stackoverflow.com/questions/61120731/how-to-hide-sidebarpanel-of-a-shiny-app-for-a-particular-tab
  
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == "Comparisons"){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    }else{
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
    }
  })
  
  sliderValues<-reactive({
    test1<-climate1 %>% 
      filter(Year >=input$range[1], 
             Year <=input$range[2]) %>% 
      group_by(Month,Year) %>%
      summarize(mean_air = mean(air_temp_5m, na.rm=TRUE),
                sum_precip = sum(rain, na.rm=TRUE),
                non_na_temp = sum(!is.na(air_temp_5m)),
                non_na_precip = sum(!is.na(rain))) %>% 
      mutate(Per = ifelse(Month == '1' | Month == "3" | Month == '5' | Month == '7' | Month == '8'| Month == '10' |Month == '12', 31, 
                          ifelse(Month == '4' | Month == '6' | Month == '9' | Month == '11', 30,
                                 ifelse(Month == '2' & 
                                          Year == "1988" | Year == "1992" | Year == "1996" | Year == "2000"|
                                          Year == "2004" | Year == "2008" | Year == "2012" | Year == "2016"|
                                          Year == "2020", 29,
                                        ifelse(Month == '2' & 
                                                 Year == "1989" | Year == "1990" | Year == "1991" | 
                                                 Year == "1993" | Year == "1994" | Year == "1995" |
                                                 Year == "1997" | Year == "1998" | Year == "1999" |
                                                 Year == "2001" | Year == "2002" | Year == "2003" |
                                                 Year == "2005" | Year == "2006" | Year == "2007" | 
                                                 Year == "2009" | Year == "2010" | Year == "2011" |
                                                 Year == "2013" | Year == "2014" | Year == "2015" |
                                                 Year == "2017" | Year == "2018" | Year == "2019" |
                                                 Year == "2021" | Year == "2022", 28,NA))))) %>%  
      group_by(Month) %>% 
      summarize(mean_air = mean(mean_air, na.rm=TRUE),
                sum_precip = mean(sum_precip, na.rm=TRUE),  
                per_temp = (sum(non_na_temp)/sum(Per))*100,
                per_precip = (sum(non_na_precip)/sum(Per))*100) %>% 
      add_column(month_name = c("January", "February", "March", "April", "May", "June", "July", "August",
                                "September","October", "November", "December")) %>% 
      select(month_name, mean_air, sum_precip, per_temp, per_precip) %>% 
      rename("Month" = month_name, "Temp (Mean)" = mean_air, "Precip (Sum)"= sum_precip,
             "% Temp Data" = per_temp, "% Precip data" = per_precip)
  })
  
  output$table<-renderTable({sliderValues()
  })
  
  output$text1<-renderUI({
    HTML(paste("",
               "NOTES:",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Monthly Air temperature summaries are averages over the timestep. Monthly precipitation summaries are average total precipitation by month for the timestep.",
               "",
               "There might be missing data within the selected date range. Along with the data summary, the output contains the percentage of data points per month included in the temperature and precipitation summaries.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Monthly climate summaries from Toolik, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  ###Seasonal Table prep  
  
  sliderValues2<-reactive({
    test2<-climate1 %>% 
      filter(Year >=input$range[1], 
             Year <=input$range[2]) %>% 
      group_by(Month,Year) %>%
      summarize(mean_air = mean(air_temp_5m, na.rm=TRUE),
                sum_precip = sum(rain, na.rm=TRUE),
                non_na_temp = sum(!is.na(air_temp_5m)),
                non_na_precip = sum(!is.na(rain))) %>% 
      mutate(Per = ifelse(Month == '1' | Month == "3" | Month == '5' | Month == '7' | Month == '8'| Month == '10' |Month == '12', 31, 
                          ifelse(Month == '4' | Month == '6' | Month == '9' | Month == '11', 30,
                                 ifelse(Month == '2' & 
                                          Year == "1988" | Year == "1992" | Year == "1996" | Year == "2000"|
                                          Year == "2004" | Year == "2008" | Year == "2012" | Year == "2016"|
                                          Year == "2020", 29,
                                        ifelse(Month == '2' & 
                                                 Year == "1989" | Year == "1990" | Year == "1991" | 
                                                 Year == "1993" | Year == "1994" | Year == "1995" |
                                                 Year == "1997" | Year == "1998" | Year == "1999" |
                                                 Year == "2001" | Year == "2002" | Year == "2003" |
                                                 Year == "2005" | Year == "2006" | Year == "2007" | 
                                                 Year == "2009" | Year == "2010" | Year == "2011" |
                                                 Year == "2013" | Year == "2014" | Year == "2015" |
                                                 Year == "2017" | Year == "2018" | Year == "2019" |
                                                 Year == "2021" | Year == "2022", 28,NA))))) %>%  
      group_by(Month) %>% 
      summarize(mean_air = mean(mean_air, na.rm=TRUE),
                sum_precip = mean(sum_precip, na.rm=TRUE),  
                non_na_temp1 = sum(non_na_temp),
                non_na_precip1 = sum(non_na_precip),
                Per1 = sum(Per))%>% 
      #arrange(factor(Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))) %>% 
      add_column(month_name = c("J", "F", "M", "A", "M", "J", "J", "A", "S","O", "N", "D")) %>% 
      add_column(season = c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", 
                            "SON", "SON", "SON", "DJF")) %>% 
      group_by(season) %>% 
      
      summarize(mean_air = mean(mean_air),
                sum_precip = sum(sum_precip),
                non_na_temp2 = (sum(non_na_temp1)/sum(Per1)*100),
                non_na_precip2 = (sum(non_na_precip1)/sum(Per1)*100)) %>% 
      #arrange(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))) %>% 
      select(season, mean_air, sum_precip, non_na_temp2, non_na_precip2) %>% 
      rename("Season" = season, "Temp (Mean)" = mean_air, "Precip (Sum)"= sum_precip,
             "% Temp Data" = non_na_temp2, "% Precip data" = non_na_precip2)
    
  })
  output$table2<-renderTable({sliderValues2()
  })
  output$text2<-renderUI({
    HTML(paste("",
               "NOTES:",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Air temperature summaries are averages over the timestep. Precipitation summaries are total precipitation for the timestep.",
               "",
               "Seasons are defined as summer (June to August) and winter (September to May). Winter summaries are only given when both years are in the selected range. For example, to get winter 2010-2011, both 2010 and 2011 must be included in your selected year range.",
               "",
               "There might be missing data within the selected date range. Along with the data summary, the output contains the percentage of data points per season included in the temperature and precipitation summaries.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Seasonal climate summaries from Toolik, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  
  ### Yearly Table prep  
  sliderValues3<-reactive({
    test3<- climate1 %>% 
      filter(Year >=input$range[1], 
             Year <=input$range[2]) %>% 
      group_by(Year) %>% 
      summarize(mean_air = mean(air_temp_5m,na.rm=TRUE),
                sum_precip = sum(rain,na.rm=TRUE),
                non_na_temp = sum(!is.na(air_temp_5m)),
                non_na_precip = sum(!is.na(rain))) %>% 
      mutate(Per = ifelse(Year == "1988" | Year == "1992" | Year == "1996" | Year == "2000"|
                            Year == "2004" | Year == "2008" | Year == "2012" | Year == "2016"| Year == "2020", 366,
                          ifelse(Year == "1989" | Year == "1990" | Year == "1991" | 
                                   Year == "1993" | Year == "1994" | Year == "1995" |
                                   Year == "1997" | Year == "1998" | Year == "1999" |
                                   Year == "2001" | Year == "2002" | Year == "2003" |
                                   Year == "2005" | Year == "2006" | Year == "2007" | 
                                   Year == "2009" | Year == "2010" | Year == "2011" |
                                   Year == "2013" | Year == "2014" | Year == "2015" |
                                   Year == "2017" | Year == "2018" | Year == "2019" |
                                   Year == "2021" | Year == "2022", 365,NA)),
             non_na_temp2 = ((non_na_temp)/(Per))*100,
             non_na_precip2 = ((non_na_precip)/(Per))*100) %>% 
      select(Year, mean_air, sum_precip, non_na_temp2, non_na_precip2) %>% 
      rename("Year" = Year, "Temp (Mean)" = mean_air, "Precip (Sum)"= sum_precip,
             "% Temp Data" = non_na_temp2, "% Precip data" = non_na_precip2)
    
  })
  output$table3<-renderTable({sliderValues3()
  })
  output$text3<-renderUI({
    HTML(paste("",
               "NOTES:",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Air temperature summaries are averages over the timestep. Precipitation summaries are total precipitation for the timestep.",
               "",
               "There might be missing data within the selected date range. Along with the data summary, the output contains the percentage of data points per year included in the temperature and precipitation summaries.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Annual climate summaries from Toolik, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  ###Climograph data prep
  
  sliderValues4<-reactive({
    climate4<-climate1 %>% 
      filter(Year >=input$range[1], 
             Year <=input$range[2]) %>% 
      group_by(Month,Year) %>%
      summarize(mean_air = mean(air_temp_5m, na.rm=TRUE),
                sum_precip = sum(rain, na.rm=TRUE)) %>% 
      #group_by(Month) %>%
      summarize(mean_air = mean(mean_air, na.rm=TRUE),
                sum_precip = mean(sum_precip, na.rm=TRUE)) %>%       
      mutate(Month = factor(Month, levels = c('1','2','3','4','5','6','7','8','9','10','11','12'))) %>% 
      add_column(month_name = c("J", "F", "M", "A", "M", "J", "J", "A", "S","O", "N", "D")) 
    
    rbind (climate4, climate4 %>% 
             filter(Month == "1"))
    
  })
  
  output$plot2<-renderPlot({
    ggplot(data=sliderValues4(), mapping=aes(x=sliderValues4()$Month, y=sliderValues4()$mean_air, group=1))+
      geom_hline(yintercept=0, linetype="solid", colour="black", size=1)+
      geom_bar(mapping=aes(y=sliderValues4()$sum_precip),  stat = "identity", fill="blue")+
      geom_line(colour="red", size=2)+
      scale_y_continuous("Temperature, C", limits=c(-30,90),breaks = c(-30, -15, 0, 15, 30), minor_breaks = c(-30,-15,0,15,30,45,60,75,90),sec.axis=sec_axis(~., name = "Precipitation, mm", breaks = c(0,15, 30,45, 60, 75, 90)))+
      scale_x_discrete(labels = c('1'="Jan",'2'="Feb",'3'="Mar",'4'="Apr",'5'="May",'6'="Jun",'7'="Jul",
                                  '8'="Aug",'9'="Sep",'10'="Oct",'11'="Nov",'12'="Dec"), name="Month")+
      ggtitle(paste("Climate Summaries", min(sliderValues3()$Year), "to", max(sliderValues3()$Year)))+
      theme(axis.title.y.right = element_text(colour = "blue", size=15, face="bold", hjust = 0.25, vjust = 1), 
            axis.title.y.left = element_text(colour = "red", size=15, face="bold", hjust = 0.2),
            axis.text.y.right = element_text(size=12, colour = "blue", face = "bold"), 
            axis.text.y.left = element_text(size=12, colour = "red", face="bold"),
            axis.text.x = element_text(size=12), axis.title.x=element_text(size=15, face="bold"),
            plot.title = element_text(size=20),
            panel.background=element_rect(fill=NA),
            panel.grid.major = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.grid.minor = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.border = element_rect(colour="black", fill=NA, size=1))
    
  })
  output$text5<-renderUI({
    HTML(paste("",
               "NOTES:",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Air temperature summaries are averages over the timestep. Precipitation summaries are total precipitation for the timestep. Caution with the older precipitation data for during the winter there are many missing days of data.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Climograph of monthly climate summaries from Toolik Field Station, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  
  ### Hythergraph data prep
  
  output$plot<-renderPlot({
    ggplot(data= sliderValues4(),aes(x=sliderValues4()$sum_precip, y=sliderValues4()$mean_air))+
      geom_path(size=1, col="#000099")  +
      geom_point(size = 7, col="#000099")+
      geom_text(aes(label=sliderValues4()$month_name), size = 4, fontface="bold", col="white")+
      scale_colour_manual(values = c("white", "white","white", "white","white", "white","white", "white","white", "white","white", "white"), guide="none") +
      labs(x="Precipitation (mm)", y="Temperature (C)")+
      ggtitle(paste("Climate Summaries", min(sliderValues3()$Year), "to", max(sliderValues3()$Year)))+
      theme(axis.title.y = element_text(colour = "red", size=15, face="bold"), 
            axis.title.x = element_text(colour = "blue", size=15, face="bold"),
            axis.text.y = element_text(size=12), 
            axis.text.x = element_text(size=12),
            plot.title = element_text(size=20),
            panel.background=element_rect(fill=NA),
            panel.grid.major = element_line(colour="grey50", linetype="dotted"),
            panel.border = element_rect(colour="black", fill=NA, size=1))
  })
  output$text4<-renderUI({
    HTML(paste("How to read a hythergraph: each point is the mean temperature and mean total precipitation for the months selected in a timestep. Months are labeled based on the first letter of the month and are followed by the next month. For instance. S (September) and on either side of it is O (October) and A (August)",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Air temperature summaries are averages over the timestep. Precipitation summaries are total precipitation for the timestep.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Hythergraph of monthly climate summaries from Toolik, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  ##########################################################
  ############ COMPARISONS ##############################
  
  sliderValues5<-reactive({
    climate4<-climate1 %>% 
      filter(Year >=input$range2[1], 
             Year <=input$range2[2]) %>% 
      group_by(Month,Year) %>%
      summarize(mean_air = mean(air_temp_5m, na.rm=TRUE),
                sum_precip = sum(rain, na.rm=TRUE)) %>% 
      #group_by(Month) %>%
      summarize(mean_air = mean(mean_air, na.rm=TRUE),
                sum_precip = mean(sum_precip, na.rm=TRUE)) %>%       
      mutate(Month = factor(Month, levels = c('1','2','3','4','5','6','7','8','9','10','11','12'))) %>% 
      add_column(month_name = c("J", "F", "M", "A", "M", "J", "J", "A", "S","O", "N", "D")) 
    
    rbind (climate4, climate4 %>% 
             filter(Month == "1"))
    
  })
  
  output$plot3<-renderPlot({
    ggplot(data=sliderValues5(), mapping=aes(x=sliderValues5()$Month, y=sliderValues5()$mean_air, group=1))+
      geom_hline(yintercept=0, linetype="solid", colour="black", size=1)+
      geom_bar(mapping=aes(y=sliderValues5()$sum_precip),  stat = "identity", fill="blue")+
      geom_line(colour="red", size=2)+
      scale_y_continuous("Temperature, C", limits=c(-30,90),breaks = c(-30, -15, 0, 15, 30), minor_breaks = c(-30,-15,0,15,30,45,60,75,90),sec.axis=sec_axis(~., name = "Precipitation, mm", breaks = c(0,15, 30,45, 60, 75, 90)))+
      scale_x_discrete(labels = c('1'="Jan",'2'="Feb",'3'="Mar",'4'="Apr",'5'="May",'6'="Jun",'7'="Jul",
                                  '8'="Aug",'9'="Sep",'10'="Oct",'11'="Nov",'12'="Dec"), name="Month")+
      ggtitle(paste("Climate Summaries", min(sliderValues7()$Year), "to", max(sliderValues7()$Year)))+
      theme(axis.title.y.right = element_text(colour = "blue", size=15, face="bold", hjust = 0.25, vjust = 1), 
            axis.title.y.left = element_text(colour = "red", size=15, face="bold", hjust = 0.2),
            axis.text.y.right = element_text(size=12, colour = "blue", face = "bold"), 
            axis.text.y.left = element_text(size=12, colour = "red", face="bold"),
            axis.text.x = element_text(size=12), axis.title.x=element_text(size=15, face="bold"),
            plot.title = element_text(size=20),
            panel.background=element_rect(fill=NA),
            panel.grid.major = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.grid.minor = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.border = element_rect(colour="black", fill=NA, size=1))
    
  })
  
  sliderValues6<-reactive({
    climate4<-climate1 %>% 
      filter(Year >=input$range3[1], 
             Year <=input$range3[2]) %>% 
      group_by(Month,Year) %>%
      summarize(mean_air = mean(air_temp_5m, na.rm=TRUE),
                sum_precip = sum(rain, na.rm=TRUE)) %>% 
      #group_by(Month) %>%
      summarize(mean_air = mean(mean_air, na.rm=TRUE),
                sum_precip = mean(sum_precip, na.rm=TRUE)) %>%       
      mutate(Month = factor(Month, levels = c('1','2','3','4','5','6','7','8','9','10','11','12'))) %>% 
      add_column(month_name = c("J", "F", "M", "A", "M", "J", "J", "A", "S","O", "N", "D")) 
    
    rbind (climate4, climate4 %>% 
             filter(Month == "1"))
    
  })
  
  output$plot4<-renderPlot({
    ggplot(data=sliderValues6(), mapping=aes(x=sliderValues6()$Month, y=sliderValues6()$mean_air, group=1))+
      geom_hline(yintercept=0, linetype="solid", colour="black", size=1)+
      geom_bar(mapping=aes(y=sliderValues6()$sum_precip),  stat = "identity", fill="blue")+
      geom_line(colour="red", size=2)+
      scale_y_continuous("Temperature, C", limits=c(-30,90),breaks = c(-30, -15, 0, 15, 30), minor_breaks = c(-30,-15,0,15,30,45,60,75,90),sec.axis=sec_axis(~., name = "Precipitation, mm", breaks = c(0,15, 30,45, 60, 75, 90)))+
      scale_x_discrete(labels = c('1'="Jan",'2'="Feb",'3'="Mar",'4'="Apr",'5'="May",'6'="Jun",'7'="Jul",
                                  '8'="Aug",'9'="Sep",'10'="Oct",'11'="Nov",'12'="Dec"), name="Month")+
      ggtitle(paste("Climate Summaries", min(sliderValues8()$Year), "to", max(sliderValues8()$Year)))+
      theme(axis.title.y.right = element_text(colour = "blue", size=15, face="bold", hjust = 0.25, vjust = 1), 
            axis.title.y.left = element_text(colour = "red", size=15, face="bold", hjust = 0.2),
            axis.text.y.right = element_text(size=12, colour = "blue", face = "bold"), 
            axis.text.y.left = element_text(size=12, colour = "red", face="bold"),
            axis.text.x = element_text(size=12), axis.title.x=element_text(size=15, face="bold"),
            plot.title = element_text(size=20),
            panel.background=element_rect(fill=NA),
            panel.grid.major = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.grid.minor = element_line(colour="grey50", linetype="dotted", size = 0.3),
            panel.border = element_rect(colour="black", fill=NA, size=1))
  })  
  
  sliderValues7<-reactive({
    test3<- climate1 %>% 
      filter(Year >=input$range2[1], 
             Year <=input$range2[2]) %>% 
      group_by(Year) %>% 
      summarize(mean_air = mean(air_temp_5m,na.rm=TRUE),
                sum_precip = sum(rain,na.rm=TRUE),
                non_na_temp = sum(!is.na(air_temp_5m)),
                non_na_precip = sum(!is.na(rain))) %>% 
      mutate(Per = ifelse(Year == "1988" | Year == "1992" | Year == "1996" | Year == "2000"|
                            Year == "2004" | Year == "2008" | Year == "2012" | Year == "2016"| Year == "2020", 366,
                          ifelse(Year == "1989" | Year == "1990" | Year == "1991" | 
                                   Year == "1993" | Year == "1994" | Year == "1995" |
                                   Year == "1997" | Year == "1998" | Year == "1999" |
                                   Year == "2001" | Year == "2002" | Year == "2003" |
                                   Year == "2005" | Year == "2006" | Year == "2007" | 
                                   Year == "2009" | Year == "2010" | Year == "2011" |
                                   Year == "2013" | Year == "2014" | Year == "2015" |
                                   Year == "2017" | Year == "2018" | Year == "2019" |
                                   Year == "2021" | Year == "2022", 365,NA)),
             non_na_temp2 = ((non_na_temp)/(Per))*100,
             non_na_precip2 = ((non_na_precip)/(Per))*100) %>% 
      select(Year, mean_air, sum_precip, non_na_temp2, non_na_precip2) %>% 
      rename("Year" = Year, "Temp (Mean)" = mean_air, "Precip (Sum)"= sum_precip,
             "% Temp Data" = non_na_temp2, "% Precip data" = non_na_precip2)
    
  })
  
  sliderValues8<-reactive({
    test3<- climate1 %>% 
      filter(Year >=input$range3[1], 
             Year <=input$range3[2]) %>% 
      group_by(Year) %>% 
      summarize(mean_air = mean(air_temp_5m,na.rm=TRUE),
                sum_precip = sum(rain,na.rm=TRUE),
                non_na_temp = sum(!is.na(air_temp_5m)),
                non_na_precip = sum(!is.na(rain))) %>% 
      mutate(Per = ifelse(Year == "1988" | Year == "1992" | Year == "1996" | Year == "2000"|
                            Year == "2004" | Year == "2008" | Year == "2012" | Year == "2016"| Year == "2020", 366,
                          ifelse(Year == "1989" | Year == "1990" | Year == "1991" | 
                                   Year == "1993" | Year == "1994" | Year == "1995" |
                                   Year == "1997" | Year == "1998" | Year == "1999" |
                                   Year == "2001" | Year == "2002" | Year == "2003" |
                                   Year == "2005" | Year == "2006" | Year == "2007" | 
                                   Year == "2009" | Year == "2010" | Year == "2011" |
                                   Year == "2013" | Year == "2014" | Year == "2015" |
                                   Year == "2017" | Year == "2018" | Year == "2019" |
                                   Year == "2021" | Year == "2022", 365,NA)),
             non_na_temp2 = ((non_na_temp)/(Per))*100,
             non_na_precip2 = ((non_na_precip)/(Per))*100) %>% 
      select(Year, mean_air, sum_precip, non_na_temp2, non_na_precip2) %>% 
      rename("Year" = Year, "Temp (Mean)" = mean_air, "Precip (Sum)"= sum_precip,
             "% Temp Data" = non_na_temp2, "% Precip data" = non_na_precip2)
    
  })
  output$text6<-renderUI({
    HTML(paste("",
               "NOTES:",
               "",
               "Error checked data will be used when possible (up to 2022-12-31) and supplemented after that with uncorrected data",
               "",
               "Air temperature summaries are averages over the timestep. Precipitation summaries are total precipitation for the timestep. Caution with the older precipitation data for during the winter there are many missing days of data.",
               "",
               "Environmental Data Center Team. Year of data retrieval. Climate summary comparisons of monthly climate summaries from Toolik Field Station, Alaska (Date Range). Toolik Field Station, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK 99775. https://www.uaf.edu/toolik/edc/monitoring/abiotic/climate_summaries.php",
               sep="<br/>"))
  })
  
  output$plot5<-renderPlot({
    ggplot(data= sliderValues5(),aes(x=sum_precip, y=mean_air))+
      geom_path(size=1, col="#000099")  +
      geom_point(size = 7, col="#000099")+
      geom_text(aes(label=sliderValues5()$month_name), size = 4, fontface="bold", col="white")+
      geom_path(data= sliderValues6(),aes(x=sum_precip, y=mean_air),size=1, col="red")+
      geom_point(data= sliderValues6(),aes(x=sum_precip, y=mean_air), size=7, col="red")+
      geom_text(data = sliderValues6(),aes(label=sliderValues6()$month_name), size = 4, fontface="bold", col="white")+
      scale_colour_manual(values = c("white", "white","white", "white","white", "white","white", "white","white", "white","white", "white","white", "white"), guide="none") +
      labs(x="Precipitation (mm)", y="Temperature (C)")+
      ggtitle(paste("<span style = 'color: #000099;'>Time Series 1 </span>",min(sliderValues7()$Year), "to", max(sliderValues7()$Year), "vs","<span style = 'color: red;'>Time Series 2 </span>", min(sliderValues8()$Year), "to", max(sliderValues8()$Year)))+
      theme(axis.title.y = element_text(colour = "red", size=15, face="bold"), 
            axis.title.x = element_text(colour = "blue", size=15, face="bold"),
            axis.text.y = element_text(size=12), 
            axis.text.x = element_text(size=12),
            plot.title = element_markdown(size=20),
            panel.background=element_rect(fill=NA),
            panel.grid.major = element_line(colour="grey50", linetype="dotted"),
            panel.border = element_rect(colour="black", fill=NA, size=1))
  })
  
}


shinyApp(ui = ui, server = server)
