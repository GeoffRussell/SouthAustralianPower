#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinythemes)
library(tidyverse)
library(markdown)
library(plotly)

comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  t<-read_file(filename)
  markdown(t)
}
options(scipen=999)

dfpower<-read_csv("facilities.csv")
dfdata<-read_csv("opennem-29-11-2023sa5.csv") %>% 
  rename_with(~sub('date','Time',.x)) %>% 
  rename_with(~sub('  ',' ',.x))
dfout<-dfdata %>% mutate(demand=select(.,`Battery (Charging) - MW`:`Solar (Rooftop) - MW`) %>% apply(1,sum)) 
dfoutshort<-dfout %>% mutate(short=demand-(`Solar (Utility) - MW`+`Solar (Rooftop) - MW`+`Wind - MW`),evreq=short*1e6/50e3) 
dfcumshort<-dfout %>% mutate(short=demand-(`Solar (Utility) - MW`+`Solar (Rooftop) - MW`+`Wind - MW`),cumshortMWh=cumsum(short*5/60)) %>%
  select(Time,demand,short,cumshortMWh)
dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","short"),names_to="Level",values_to="MW") 
coef<-3/28


ui <- fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("A week in the electricity supply of South Australia"),
  sidebarLayout(
    sidebarPanel(
      
      
    ),
    mainPanel(
      markdownFile("intro.txt"),
      imageOutput("weekpng",height=400),
      markdownFile("ob1.txt"),
      plotOutput("shortfall"),
      plotOutput("facilities")
    )
  )
)
server<-function(input,output) {
  output$weekpng<-renderImage(list(src="WeekEnding29-11-2023.png",height=400),deleteFile=FALSE)
  output$shortfall<-renderPlot({
      dfcs %>% ggplot() + geom_line(aes(x=Time,y=MW,color=Level)) + 
        geom_line(aes(x=Time,y=cumshortMWh*coef))+
        labs(color="MW",title="Demand and non-renewable shortfall, Week ending November 28 2023")+
        scale_y_continuous(
          name="MW",
          sec.axis = sec_axis(~./coef, name="Cumulative shortfall in MWh")
        )
  })
  output$facilities=renderPlot({
    dfpower %>% group_by(Technology) %>% summarise(MW=sum(`Generator Capacity (MW)`)) %>%
      ggplot()+geom_col(aes(x=Technology,y=MW),width=0.6,fill="blue")+coord_flip()
  })
  
}
shinyApp(ui=ui,server=server)



                  


