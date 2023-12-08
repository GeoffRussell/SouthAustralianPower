#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinyWidgets)
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
#-------------------------------------------------------------------------------------------
# Set up plotly
#-------------------------------------------------------------------------------------------
ggplconfig <- function(.data) {
  config(.data,displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("autoScale2d", 
                                       "hoverClosestCartesian", "hoverCompareCartesian", 
                                       "select2d", "zoomIn2d", "zoomOut2d","lasso2d","toggleSpikelines"))
}

dfpop<-read_csv("populationRenamed2022.csv")
dfkwPerCapSolar<-read_csv("kwPerCap2022.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
dfkwPerCapWind<-read_csv("kwPerCap2022Wind.csv") %>% arrange(desc(kwPerCap)) %>% slice_head(n=20)
countries<-dfkwPerCapWind %>% select(Country) 
countries<-bind_rows(countries,tribble(~Country,"South Australia"))

dfwsbh<-read_csv("wsbh.csv") %>% inner_join(countries)
dfel<-bind_rows(tribble(~Country,~TWh,~Population,"South Australia",14.2,1.8e6),(read_csv("el.csv") %>% inner_join(countries)))
write_csv(dfel,"electricityByCountry.csv")

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
      markdownFile("intro1a.txt"),
      fluidRow(align="center",imageOutput("scaleissues2",height=400)),
      markdownFile("intro1ba.txt"),
      fluidRow(align="center",imageOutput("blacksummer",height=300)),
      markdownFile("intro1bb.txt"),
      tableOutput("coal"),
      markdownFile("intro2.txt"),
      tableOutput("interconnectors"),
      markdownFile("intro3.txt"),
      fluidRow(align="center",imageOutput("scaleissues1",height=300)),
      markdownFile("intro3b.txt"),
      fluidRow(align="center",imageOutput("scaleissues2b",height=400)),
      markdownFile("intro3c.txt"),
      markdownFile("obw.txt"),
      plotOutput("kwpercapwind"),
      markdownFile("obs.txt"),
      plotOutput("kwpercapsolar"),
      markdownFile("obwsbh.txt"),
      fluidRow(
        column(width=3,
            pickerInput("countrypick",choices=sort(countries$Country),selected=sort(c("South Australia","Australia","Germany")),multiple=TRUE,
                          label = 'Top 20 Wind power countries',
                        options = pickerOptions(
                          actionsBox = TRUE,
                          selectedTextFormat = 'static',
                          noneSelectedText = 'Select',
                        )
            ) 
        ),
        column(width=9,
            plotlyOutput("wsbh")
        )
      ),
      markdownFile("obwsbh2.txt"),
      markdownFile("ob0.txt"),
      fluidRow(align="center",imageOutput("weekpng",height=400)),
      markdownFile("ob1.txt"),
      plotOutput("shortfall"),
      plotOutput("facilities")
    )
  )
)
server<-function(input,output,session) {
  mtheme<-theme(plot.margin=unit(c(1,0,0,0),"cm"))
  ptheme<-theme(plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica"),
                axis.text=element_text(face="bold",size=12))+mtheme
    
  output$interconnectors<-renderTable(
    tribble(
      ~Interconnector, ~Capacity, ~Status, ~Type,
      "Heywood", "650MW", "Operating","AC",
      "Murraylink", "220MW", "Operating","HVDC",
      "Energy Connect", "800MW", "InProgress","AC"
    )
    
  )
  output$coal<-renderTable(
    tibble("Retired Coal Plants"=c("Playford A","Playford B","Northern"),"Capacity"=c("90MW","240MW","520MW"),"Closed"=c("1985","2016","2016"))
  )
  output$blacksummer<-renderImage(list(src="black-summer-2019.png",height=300),deleteFile=FALSE)
  output$scaleissues1<-renderImage(list(src="renewable-scaleissues.jpg",height=300),deleteFile=FALSE)
  output$scaleissues2<-renderImage(list(src="renewable-scaleissues-mod.png",height=400),deleteFile=FALSE)
  output$scaleissues2b<-renderImage(list(src="renewable-scaleissues-mod.png",height=400),deleteFile=FALSE)
  output$weekpng<-renderImage(list(src="WeekEnding30-11-2023.png",height=400),deleteFile=FALSE)
  output$kwpercapsolar<-renderPlot({
      dfkwPerCapSolar %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap),width=0.6,fill="yellow") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",18),"red","black"))) + 
      ptheme +
      labs(x="",y="kilowatts per person",title="Solar: Top 20 countries by kilowatts per person")
  })
  output$wsbh<-renderPlotly({
    c<-input$countrypick
    print(c)
    p<-dfwsbh %>% filter(Country %in% c) %>% ggplot() + 
      geom_col(aes(x=reorder(Country,desc(sum)),y=kWhPerCap,fill=Type)) +
      #geom_point(aes(x=Country,y=TWh*1e9/Population),shape=5,data=dfel) +
      geom_col(aes(x=Country,y=kWhPerCapTimes2),
               data=dfel %>% filter(Country %in% c) %>% mutate(kWhPerCapTimes2=(TWh*1e9/Population)*2),
               alpha=0.1,fill="red") +
      scale_fill_manual(name="Technology",values=cols)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(plot.margin=unit(c(1,0,0,0),"cm"))+
      ptheme +
      labs(x="",y="Annual low carabon\nkilowatt-hours per person",
           title="Low carbon electricity+targets")
     ggplotly(p) %>% ggplconfig %>% layout(plot_bgcolor = "#e5ecf6") 
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



                  


