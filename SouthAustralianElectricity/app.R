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

dfpop<-read_csv("populationRenamed2022.csv")
dfkwPerCapSolar<-read_csv("kwPerCap2022.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
dfkwPerCapWind<-read_csv("kwPerCap2022Wind.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
write_csv(dfkwPerCapWind,"xxx.csv")
dfsa<-tribble(
  ~Country,~"2022",~Population,~MW,~kwPerCap,
  "South Australia",3443,1.7e6,3442,(3442000/1.7e6)
)
dfkwPerCapWind<-bind_rows(dfkwPerCapWind,dfsa)
dfkwPerCapWind <- dfkwPerCapWind %>% mutate(Group=case_when(Country=="South Australia" ~ "XX",.default = "YY"))
write_csv(dfkwPerCapWind,"xxx.csv")
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
      plotOutput("kwpercapsolar"),
      markdownFile("obw.txt"),
      plotOutput("kwpercapwind"),
      markdownFile("ob0.txt"),
      fluidRow(align="center",imageOutput("weekpng",height=400)),
      markdownFile("ob1.txt"),
      plotOutput("shortfall"),
      plotOutput("facilities")
    )
  )
)
server<-function(input,output) {
  ptheme<-theme(plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica"))
  output$weekpng<-renderImage(list(src="WeekEnding29-11-2023.png",height=400),deleteFile=FALSE)
  output$kwpercapsolar<-renderPlot({
      dfkwPerCapSolar %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap),width=0.6,fill="steelblue1") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ptheme +
      labs(x="",y="kilowatts per person",title="Solar: Top 20 countries by kilowatts per person")
  })
  output$kwpercapwind<-renderPlot({
      dfkwPerCapWind %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap,fill=Group),width=0.6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ptheme +
      scale_fill_manual(values=c("red","steelblue1"))+guides(fill="none")+
      labs(x="",y="kilowatts per person",title="Wind: Top 20 countries (plus SA) by kilowatts per person")
  })
  output$shortfall<-renderPlot({
      dfcs %>% ggplot() + geom_line(aes(x=Time,y=MW,color=Level)) +  ptheme +
        geom_line(aes(x=Time,y=cumshortMWh*coef))+
        labs(color="MW",title="Demand and non-renewable shortfall, Week ending November 28 2023")+
        scale_y_continuous(
          name="MW",
          sec.axis = sec_axis(~./coef, name="Cumulative shortfall in MWh")
        )
  })
  output$facilities=renderPlot({
    dfpower %>% group_by(Technology) %>% summarise(MW=sum(`Generator Capacity (MW)`)) %>%
      ggplot()+geom_col(aes(x=Technology,y=MW),width=0.6,fill="blue")+coord_flip() + ptheme 
  })
  
}
shinyApp(ui=ui,server=server)



                  


