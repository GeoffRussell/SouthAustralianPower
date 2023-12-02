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
cols<-c(
  "Biomass"="brown",
  "Solar"="yellow",
  "Wind"="forestgreen",
  "Nuclear"="purple",
  "Hydro"="blue"
  )

dfpop<-read_csv("populationRenamed2022.csv")
dfkwPerCapSolar<-read_csv("kwPerCap2022.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
dfkwPerCapWind<-read_csv("kwPerCap2022Wind.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
countries<-dfkwPerCapWind %>% select(Country) 
countries<-bind_rows(countries,tribble(~Country,"South Australia"))

dfwsbh<-read_csv("wsbh.csv") %>% inner_join(countries)
dfel<-bind_rows(tribble(~Country,~TWh,~Population,"South Australia",14.2,1.8e6),(read_csv("el.csv") %>% inner_join(countries)))
print(dfel)

dfwsbh<-dfwsbh %>% group_by(Country) %>% summarise(sum=sum(kWhPerCap)) %>% ungroup() %>% inner_join(dfwsbh)

dfsa<-tribble(
  ~Country,~"2022",~Population,~MW,~kwPerCap,
  "South Australia",3443,1.7e6,3442,(3442000/1.7e6)
)
dfkwPerCapWind<-bind_rows(dfkwPerCapWind,dfsa)
dfkwPerCapWind <- dfkwPerCapWind %>% mutate(Group=case_when(Country=="South Australia" ~ "XX",.default = "YY"))
dfpower<-read_csv("facilities.csv")
dfdata<-read_csv("opennem-30-11-2023sa5.csv") %>% 
  rename_with(~sub('date','Time',.x)) %>% 
  rename_with(~sub('  ',' ',.x))
dfout<-dfdata %>% mutate(demand=select(.,`Battery (Charging) - MW`:`Solar (Rooftop) - MW`) %>% apply(1,sum)) 
dfcumshort<-dfout %>% mutate(short=demand-(`Solar (Utility) - MW`+`Solar (Rooftop) - MW`+`Wind - MW`),cumshortMWh=cumsum(short*5/60)) %>%
  select(Time,demand,short,cumshortMWh)
dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","short"),names_to="Level",values_to="MW") 
coef<-3/28


ui <- fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("South Australia's excellent renewable adventure"),
  verticalLayout(
    mainPanel(
      markdownFile("intro.txt"),
      markdownFile("obw.txt"),
      plotOutput("kwpercapwind"),
      markdownFile("obs.txt"),
      plotOutput("kwpercapsolar"),
      markdownFile("obwsbh.txt"),
      plotOutput("wsbh"),
      markdownFile("obwsbh2.txt"),
      markdownFile("ob0.txt"),
      fluidRow(align="center",imageOutput("weekpng",height=400)),
      markdownFile("ob1.txt"),
      plotOutput("shortfall"),
      plotOutput("facilities")
    )
  )
)
server<-function(input,output) {
  ptheme<-theme(plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica"),
                axis.text=element_text(face="bold",size=12)
                )
  output$weekpng<-renderImage(list(src="WeekEnding30-11-2023.png",height=400),deleteFile=FALSE)
  output$kwpercapsolar<-renderPlot({
      dfkwPerCapSolar %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap),width=0.6,fill="yellow") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",18),"red","black"))) + 
      ptheme +
      labs(x="",y="kilowatts per person",title="Solar: Top 20 countries by kilowatts per person")
  })
  output$wsbh<-renderPlot({
    dfwsbh %>% ggplot() + 
      geom_col(aes(x=reorder(Country,desc(sum)),y=kWhPerCap,fill=Type)) +
      #geom_point(aes(x=Country,y=TWh*1e9/Population),shape=5,data=dfel) +
      geom_col(aes(x=Country,y=(TWh*1e9/Population)*2),data=dfel,alpha=0.1,fill="red") +
      scale_fill_manual(name="Technology",values=cols)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",4),"red",rep("black",8),"red",rep("black",7)))) + 
      ptheme +
      labs(x="",y="Annual low carabon kilowatt-hours per person",
           title="Wind/Solar/Biomass/Hydro/Nuclear: Top 20 \ncountries by wind capacity")
  })
  output$kwpercapwind<-renderPlot({
      dfkwPerCapWind %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap,fill=Group),width=0.6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",7),"red",rep("black",12),"red"))) + ptheme + 
      scale_fill_manual(values=c("forestgreen","forestgreen"))+guides(fill="none")+
      labs(x="",y="kilowatts per person",title="Wind: Top 20 countries (plus SA) by kilowatts per person")
  })
  output$shortfall<-renderPlot({
      dfcs %>% ggplot() + geom_line(aes(x=Time,y=MW,color=Level)) +  ptheme +
        geom_line(aes(x=Time,y=cumshortMWh*coef))+
        labs(color="MW",title="Demand and non-renewable shortfall, Week ending November 29 2023")+
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



                  


