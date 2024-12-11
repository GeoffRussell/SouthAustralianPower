#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(markdown)
library(plotly)
library(RcppRoll)

comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  t<-read_file(pipe(paste0("cat m4defsnull.txt ",filename," | m4 ")))
  #t<-read_file(pipe(paste0("cat m4defs.txt ",filename," | m4 ")))
  markdown(t)
}
options(scipen=999)
colswind<-c(
  "demand"="brown",
  "wind"="forestgreen"
)
labswind<-c(
  "Demand",
  "Wind"
)
colsshort<-c(
  "renew"="purple",
  "dblrenew"="cyan",
  "demand"="brown"
)
labsshort<-c(
  "Overbuild",
  "Demand",
  "Wind+Solar"
)
colsfacilities<-c(
  "Wind"="forestgreen",
  "Solar (Utility),Wind"="cyan",
  "Solar (Utility)"="yellow",
  "Gas (Steam)"="bisque4",
  "Gas (Reciprocating)"="bisque3",
  "Gas (OCGT)"="bisque2",
  "Gas (CCGT)"="bisque1",
  "Distillate"="grey42",
  "Bioenergy (Biogas)"="brown",
  "Battery (Discharging),Solar(Utility)"="blue",
  "Battery (Discharging)"="blue"
)
cols<-c(
  "Biomass"="brown",
  "Solar"="yellow",
  "Wind"="forestgreen",
  "Nuclear"="purple",
  "Hydro"="blue",
  "kWh/Person"="bisque1",
  "IEA2050Target"="grey70"
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
write_csv(dfwsbh,"tmpwsbh.csv")

dataSets<-c(
  "(VIC) WE 25 January 2024"="openNem-VIC-25-01-24-7D.csv",
  "WE 16 May 2024"="openNem-SA-16-05-24-7D.csv",
  "WE 14 May 2024"="openNem-SA-14-05-24-7D.csv",
  "(VIC) WE 16 May 2024"="openNem-VIC-16-05-24-7D.csv",
  "(NEM) WE 16 May 2024"="openNem-NEM-16-05-24-7D.csv",
  "June 2024"="openNEMMerge-June-2024.csv",
  "WE 30 January 2024"="openNem-SA-30-01-24-7D.csv",
  "WE 25 January 2024"="openNem-SA-25-01-24-7D.csv",
  "WE 30 November 2023"="opennem-30-11-2023sa5.csv",
  "First heatwave, Dec 2019"="openNem-SA-21-12-19-7D.csv",
  "Second heatwave, Dec 2019"="openNem-SA-28-12-19-7D.csv",
  "March heatwave, 2024"="openNem-SA-12-03-24-7D.csv"
)
dataSetTitles<-c(
  "(VIC) WE 25 January 2024"="Electricity renewable/demand/curtailment/shortfall\n(Victoria) Week ending 25 Jan 2024",
  "WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 16 May 2024",
  "WE 14 May 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 14 May 2024",
  "(VIC) WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nVIC Week ending 16 May 2024",
  "(NEM) WE 16 May 2024"="Electricity renewable/demand/curtailment/shortfall\nNEM Week ending 16 May 2024",
  "June 2024"="Electricity renewable/demand/shortfall\nJune 2024",
  "WE 30 January 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 30 Jan 2024",
  "WE 25 January 2024"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 25 Jan 2024",
  "WE 30 November 2023"="Electricity renewable/demand/curtailment/shortfall\nWeek ending 30 November 2023",
  "First heatwave, Dec 2019"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 21 December 2019",
  "Second heatwave, Dec 2019"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 28 December 2019",
  "March heatwave, 2024"="Electricity renewable/demand/curtailment/shortfall\nHeatwave, WE 12 March 2024"
)

dfsa<-tribble(
  ~Country,~"2022",~Population,~MW,~kwPerCap,
  "South Australia",3443,1.7e6,3442,(3442000/1.7e6)
)
dfkwPerCapWind<-bind_rows(dfkwPerCapWind,dfsa)
dfkwPerCapWind <- dfkwPerCapWind %>% mutate(Group=case_when(Country=="South Australia" ~ "XX",.default = "YY"))
dfpower<-read_csv("facilities.csv")
fields<-c("Battery (Charging) - MW","Imports - MW","Distillate - MW","Gas (Steam) - MW","Gas (CCGT) - MW", "Gas (OCGT) - MW","Gas (Reciprocating) - MW","Battery (Discharging) - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
fieldsVIC<-c("Battery (Charging) - MW","Imports - MW","Coal (Brown) - MW","Gas (OCGT) - MW",
             "Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
fieldsNEM<-c("Battery (Charging) - MW","Coal (Brown) - MW","Gas (OCGT) - MW",
             "Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")

renewfields<-c("Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
renewfieldsVIC<-c("Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
renewfieldsNEM<-c("Hydro - MW","Wind - MW","Solar (Utility) - MW","Solar (Rooftop) - MW")
annual<-p("SA - annual avg power 1.58GW (2023/4)")

isnem<-function(f) {
  str_detect(f,"NEM")
}
isvic<-function(f) {
  str_detect(f,"VIC")
}
readDataSet<-function(n) {
  print(n)
  flds<-fields
  annual<<-p("SA - annual avg power 1.58GW (2023/4)")
  avgpower<<-1.58
  if (isnem(n)) {
    annual<<-p("NEM- annual avg power 23.9GW (2023/4)")
    avgpower<<-23.9
    flds<-fieldsNEM
  }
  if (isvic(n)) {
    annual<<-p("VIC - annual avg power 5.4GW (2023/4)")
    avgpower<<-5.4
    flds<-fieldsVIC
  }
  dfdata<-read_csv(dataSets[n]) %>% 
  rename_with(~sub('date','Time',.x)) %>% 
  rename_with(~sub('  ',' ',.x))
  dfdata %>% mutate(demand=select(.,all_of(flds)) %>% apply(1,sum)) 
}
dfout<-readDataSet("WE 30 November 2023")
#--------------------------------------------------------------------------
# coef is arbitrary ... just scales the second axis on the graph 
#--------------------------------------------------------------------------
coef<-500*(3/28)
#---------------------------------------------------------------------------------------
# Find night time bands 
#---------------------------------------------------------------------------------------
findBands<-function(df) {
  sunbreak=0.05*max(df$`Solar (Rooftop) - MW`)  # 5 percent of max is start of day
  dark<-(df$`Solar (Rooftop) - MW`<sunbreak)    # which periods are dark
  ldld<-which(dark[-1] != dark[-length(dark)])  # where are the changes of state
  darkdf<-tribble(~t1,~t2)
  st<-2                                         
  if (dark[1]) {                                # are we starting in the night or the day?
     tmp<-tibble(t1=df$Time[1],t2=df$Time[ldld[1]])
     darkdf<-bind_rows(darkdf,tmp)
     st<-3
  }
  for (i in seq(st,length(ldld),2)) {
    tmp<-tibble(t1=df$Time[ldld[i-1]],t2=df$Time[ldld[i]])
    darkdf<-bind_rows(darkdf,tmp)
  }
  darkdf
}

#---------------------------------------------------------------------------------------
# Battery handling
#---------------------------------------------------------------------------------------
lastdfsum<-dfout
bcalcObsolete<-function(bmax,ofac,icsize=0,dspick,baseloadsize=0) {
  print(dataSets[dspick])
  dfout<-readDataSet(dspick)
  batteryMaxCapacity<-bmax
  dfsum <- dfout %>% mutate(
    battuse=`Battery (Discharging) - MW`,
    #imports=`Imports - MW`,
    #diesel=`Distillate - MW`,
    wind=`Wind - MW`,
    solar=`Solar (Rooftop) - MW`+`Solar (Utility) - MW`) 
  
  if (isnem(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else if (isvic(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else { # in SA most exports are of excess wind/solar
    dfsum<-dfsum %>% mutate(renew=wind+solar-`Exports - MW`,dblrenew=ofac*renew) 
  }
  dfsum<-dfsum %>% mutate(noBattShortfall=dblrenew-demand,cumNoBattShortfall=cumsum(dblrenew-demand))  
  if (baseloadsize>0) {
    dfsum<-dfsum %>% mutate(savedemand=demand,demand=ifelse(demand>baseloadsize,demand-baseloadsize,0),baseloadsize=baseloadsize)
  }
  #-------------------
  # start with battery full 
  #-------------------
  batteryStatus<-bmax
  nperiods<-length(dfsum$demand)
  dfsum$shortFall=rep(0,nperiods)
  dfsum$icExpMWh=rep(0,nperiods) # in MWh
  dfsum$batteryStatus=rep(0,nperiods)
  dfsum$batterySupplied=rep(0,nperiods) # MWh
  dfsum$throwOutMWh=rep(0,nperiods)
  dfsum$addedToBattery=rep(0,nperiods)
  lastdfsum<<-dfsum
  totalBattuse<<-sum(lastdfsum$battuse/12)
  maxNeed<-0
  dfsum$minroll20<-roll_sum(dfsum$dblrenew,n=20,fill=0)
  write_csv(dfsum,"xxx.csv")
  for(i in 1:nperiods) {
    dfsum$shortFall[i]=0
    # spareE is in MWh
    spareE=(dfsum$dblrenew[i]-dfsum$demand[i])/12 
    if (spareE>0) {  # Electricity exceeds demand ... add spare to battery if there is any capacity
      if (batteryStatus<batteryMaxCapacity) { # battery isn't full
        spareB=batteryMaxCapacity-batteryStatus
        addE=min(spareE,spareB)
        batteryStatus=batteryStatus+addE
        dfsum$addedToBattery[i]=addE
        leftOver=spareE-spareB
        if (leftOver>0) {
             # send out interconnector
             spareW=12*leftOver # convert to MW 
             if (spareW>icsize) {
               dfsum$icExpMWh[i]=icsize/12
               spareW=spareW-icsize
               leftOver=spareW/12
             } else {
               dfsum$icExpMWh[i]=spareW/12
               spareW=0
               leftOver=0
             }
             dfsum$throwOutMWh[i]=leftOver
        }
      }
      else {   # battery is full, discard energy
               # or send out interconnector 
        spareW=12*spareE # convert to MW 
        if (spareW>icsize) {
          dfsum$icExpMWh[i]=icsize/12
          spareW=spareW-icsize
          spareE=spareW/12
        }
        else {
           dfsum$icExpMWh[i]=spareW/12
           spareE=0
        }
        dfsum$throwOutMWh[i]=spareE
      }
    }
    if (spareE<0) { # demand exceeds generation, get from battery if any available
      needE=-spareE
      if (needE>maxNeed) {
        maxNeed<-needE
      }
      if (batteryStatus>0) {
        if (batteryStatus>=needE) { # extract from battery
          batteryStatus=batteryStatus-needE
          dfsum$batteryStatus[i]=batteryStatus
          dfsum$batterySupplied[i]=needE
        }
        else { # we have some in battery, but not enough
          dfsum$shortFall[i]=needE-batteryStatus
          dfsum$batterySupplied[i]=batteryStatus
          batteryStatus=0
        }
      }
      else { # battery empty
          dfsum$shortFall[i]=needE
      }
    }
    dfsum$batteryStatus[i]=batteryStatus
  }
  if (baseloadsize>0) {
    dfsum<-dfsum %>% mutate(demand=ifelse(demand>baseloadsize,demand-baseloadsize,0),baseloadsize=baseloadsize)
  }
  if (baseloadsize>0) {
    dfsum<-dfsum %>% mutate(demand=savedemand)
  }
  dfsum %>% mutate(cumShortMWh=cumsum(shortFall),
                   cumThrowOutMWh=cumsum(throwOutMWh),
                   cumIcExpMWh=cumsum(icExpMWh),
                   maxShortMW=max(shortFall)*12
                   )
}
bcalc<-function(bmax,ofac,icsize=0,dspick,baseloadsize=0) {
  print(dataSets[dspick])
  dfout<-readDataSet(dspick)
  batteryMaxCapacity<-bmax
  dfsum <- dfout %>% mutate(
    battuse=`Battery (Discharging) - MW`,
    #imports=`Imports - MW`,
    #diesel=`Distillate - MW`,
    wind=`Wind - MW`,
    solar=`Solar (Rooftop) - MW`+`Solar (Utility) - MW`) 
  
  if (isnem(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else if (isvic(dspick)) {
    dfsum<-dfsum %>% mutate(renew=wind+solar,dblrenew=ofac*renew)
  }
  else { # in SA most exports are of excess wind/solar
    dfsum<-dfsum %>% mutate(renew=wind+solar-`Exports - MW`,dblrenew=ofac*renew) 
  }
  dfsum<-dfsum %>% mutate(noBattShortfall=dblrenew-demand,cumNoBattShortfall=cumsum(dblrenew-demand))  
  if (baseloadsize>0) {
    dfsum<-dfsum %>% mutate(supply=dblrenew+baseloadsize,baseloadsize=baseloadsize)
  }
  else {
    dfsum<-dfsum %>% mutate(supply=dblrenew,baseloadsize=0)
  }
  #-------------------
  # start with battery full 
  #-------------------
  batteryStatus<-bmax
  nperiods<-length(dfsum$demand)
  dfsum$shortFall=rep(0,nperiods)
  dfsum$icExpMWh=rep(0,nperiods) # in MWh
  dfsum$batteryStatus=rep(0,nperiods)
  dfsum$batterySupplied=rep(0,nperiods) # MWh
  dfsum$throwOutMWh=rep(0,nperiods)
  dfsum$addedToBattery=rep(0,nperiods)
  lastdfsum<<-dfsum
  totalBattuse<<-sum(lastdfsum$battuse/12)
  maxNeed<-0
  dfsum$minroll20<-roll_sum(dfsum$supply,n=20,fill=0)
  write_csv(dfsum,"xxx.csv")
  for(i in 1:nperiods) {
    dfsum$shortFall[i]=0
    # spareE is in MWh
    spareE=(dfsum$supply[i]-dfsum$demand[i])/12 
    if (spareE>0) {  # Electricity exceeds demand ... add spare to battery if there is any capacity
      if (batteryStatus<batteryMaxCapacity) { # battery isn't full
        spareB=batteryMaxCapacity-batteryStatus
        addE=min(spareE,spareB)
        batteryStatus=batteryStatus+addE
        dfsum$addedToBattery[i]=addE
        leftOver=spareE-spareB
        if (leftOver>0) {
             # send out interconnector
             spareW=12*leftOver # convert to MW 
             if (spareW>icsize) {
               dfsum$icExpMWh[i]=icsize/12
               spareW=spareW-icsize
               leftOver=spareW/12
             } else {
               dfsum$icExpMWh[i]=spareW/12
               spareW=0
               leftOver=0
             }
             dfsum$throwOutMWh[i]=leftOver
        }
      }
      else {   # battery is full, discard energy
               # or send out interconnector 
        spareW=12*spareE # convert to MW 
        if (spareW>icsize) {
          dfsum$icExpMWh[i]=icsize/12
          spareW=spareW-icsize
          spareE=spareW/12
        }
        else {
           dfsum$icExpMWh[i]=spareW/12
           spareE=0
        }
        dfsum$throwOutMWh[i]=spareE
      }
    }
    if (spareE<0) { # demand exceeds generation, get from battery if any available
      needE=-spareE
      if (needE>maxNeed) {
        maxNeed<-needE
      }
      if (batteryStatus>0) {
        if (batteryStatus>=needE) { # extract from battery
          batteryStatus=batteryStatus-needE
          dfsum$batteryStatus[i]=batteryStatus
          dfsum$batterySupplied[i]=needE
        }
        else { # we have some in battery, but not enough
          dfsum$shortFall[i]=needE-batteryStatus
          dfsum$batterySupplied[i]=batteryStatus
          batteryStatus=0
        }
      }
      else { # battery empty
          dfsum$shortFall[i]=needE
      }
    }
    dfsum$batteryStatus[i]=batteryStatus
  }
  dfsum %>% mutate(cumShortMWh=cumsum(shortFall),
                   cumThrowOutMWh=cumsum(throwOutMWh),
                   cumIcExpMWh=cumsum(icExpMWh),
                   maxShortMW=max(shortFall)*12
                   )
}

#------------------------------------------------------------
# Constants
#------------------------------------------------------------
avgpower<-1.58 # giga watts


ui <- fluidPage(theme = shinytheme("cerulean"),
  tags$head(tags$style(".standout-container {margin: 20px 0; padding: 20px; font-weight: bold; background-color: Teal; color: white;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                       }")), 
                
  # Application title
  titlePanel("South Australia's excellent renewable adventure"),
  verticalLayout(
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Intro",
      markdownFile("intro1aa.txt"),
      fluidRow(
        column(width=12,
            checkboxInput("kwhpercap",label="Show total 2022 kWh/person",value=FALSE),
            checkboxInput("ieatarget",label="Show IEA Targets for 2050",value=FALSE),
            checkboxInput("cmpnr",label="Compare nuclear/solar+wind",value=FALSE),
            pickerInput("countrypick",choices=sort(countries$Country),selected=sort(c("South Australia","Australia","Germany")),multiple=TRUE,
                          label = 'Top 20 countries by wind power output',
                        options = pickerOptions(
                          actionsBox = TRUE,
                          selectedTextFormat = 'static',
                          noneSelectedText = 'Select',
                        )
            ) 
        ),
      ),
      fluidRow(
        column(width=12,
            plotOutput("wsbh",height="500px",width="100%")
        )
      ),
      markdownFile("intro1ab.txt"),
      fluidRow(align="center",imageOutput("scaleissues2",height=400)),
      markdownFile("intro1ba.txt"),
      fluidRow(align="center",imageOutput("blacksummer",height=300)),
      markdownFile("intro1bba.txt"),
      fluidRow(align="center",imageOutput("batheat",height=400))
                  ),
                  tabPanel("SA's grid",
      markdownFile("intro1bbb.txt"),
      tableOutput("coal"),
      markdownFile("intro2.txt"),
      markdownFile("obw.txt"),
      plotOutput("kwpercapwind"),
      markdownFile("obs.txt"),
      plotOutput("kwpercapsolar"),
      markdownFile("intro3aa.txt"),
      tableOutput("interconnectors"),
      markdownFile("intro3ab.txt")
      #markdownFile("intro3b.txt"),
      #markdownFile("intro3c.txt")
                  ),
                  tabPanel("Inertia",
      markdownFile("inertia.txt")
      #markdownFile("obwsbh.txt"),
      #markdownFile("obwsbh2.txt")
                  ),
                  tabPanel("Gridlock",
      markdownFile("connecta.txt"),
      fluidRow(align="center",imageOutput("connect",height=400)),
      markdownFile("connectb.txt"),
      fluidRow(align="center",imageOutput("connectgit",height=400)),
      markdownFile("connectc.txt")
                  ),
                  tabPanel("SA Data",
      markdownFile("ob0.txt"),
      fluidRow(align="center",imageOutput("weekpng",height=400)),
      markdownFile("ob1a.txt"),
      fluidRow(
        column(width=6,
            sliderInput("ofac",label="Overbuild factor for wind+solar", min=1,max=3,step=0.1,value=1),
            sliderInput("bsize",label="Battery size in MWh", min=500,max=30000,step=500,value=500),
            sliderInput("icsize",label="Interconnector size (MW)", min=0,max=2000,step=100,value=0),
            sliderInput("baseloadsize",label="Baseload size (MW)", min=0,max=1800,step=600,value=0),
#            sliderInput("dfac",label="Electricity expansion factor", min=1,max=2,step=0.2,value=1),
            pickerInput("datasetpick",choices=sort(names(dataSets)),selected=c("WE 30 November 2023"),multiple=FALSE,
                          label = 'Alternative datasets',
                        options = pickerOptions(
                          actionsBox = TRUE,
                          selectedTextFormat = 'static',
                          noneSelectedText = 'Select',
                        )
            ) 
        ),
        column(width=6,
            checkboxInput("showShort",label="Show shortfall (GWh)",value=FALSE),
            checkboxInput("showCurtailed",label="Show dumped energy (GWh)",value=FALSE),
            checkboxInput("showBatteryStatus",label="Show batteryStatus (%)",value=FALSE),
            checkboxInput("showInterconnector",label="Show interconnector flow (GWh)",value=FALSE),
            checkboxInput("showWindDemand",label="Show wind vs demand",value=FALSE)
        )
      ),
      fluidRow(
        column(width=12,
            plotOutput("shortfall")
        ),
        column(width=12,
               uiOutput("bcalcresult")
        )
      ),
      markdownFile("ob1b.txt")
#      plotOutput("facilities")
      ),
      tabPanel("About",
          markdownFile("about.txt")
         )
      )
    )
  )
)
server<-function(input,output,session) {
  mtheme<-theme(plot.margin=unit(c(5,0,0,0),"mm"))
  ptheme<-theme(plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica"),
                axis.text=element_text(face="bold",size=12))+mtheme
  gendfsum<-reactive({
       print(input$datasetpick)
       bstatus<-bcalc(input$bsize,input$ofac,input$icsize,input$datasetpick,input$baseloadsize)
       dfile<-bstatus %>%  mutate(diffE=(dblrenew-demand)/12) %>% select(Time,dblrenew,demand,diffE,batteryStatus,batterySupplied,shortFall,addedToBattery) 
       write_csv(dfile,"bcalc-output.csv")
       bstatus
  })
    
  output$bcalcresult<-renderUI({
    dfsum<-gendfsum()
    totdemand<-dfsum %>% summarise(totdemand=sum(demand/12))
    sh<-dfsum %>% summarise(max(cumShortMWh))
    curt<-dfsum %>% summarise(max(cumThrowOutMWh))
    bsup<-dfsum %>% summarise(sum(batterySupplied))
    bmax<-dfsum %>% summarise(max(batterySupplied*12))
    dmand<-dfsum %>% summarise(sum(dblrenew/12))
    shortMW<-dfsum %>% summarise(max(maxShortMW))
    
    hrs<-8
    dfsum$diff<-roll_sum((dfsum$dblrenew-dfsum$demand)/12,n=12*hrs,align="right",fill=0)
    #write_csv(dfsum,"tmp-dfsum.csv")
    dfsum$sumdblrenew<-roll_sum(dfsum$dblrenew/12,n=12*hrs,align="right",fill=0)
    dfsum$sumdemand<-roll_sum(dfsum$demand/12,n=12*hrs,align="right",fill=0)
    r<-dfsum %>% select(Time,sumdblrenew,sumdemand,diff) %>% slice_min(diff)
    rnights<-dfsum %>% select(Time,sumdblrenew,sumdemand,diff) %>% filter(hour(Time)*60+minute(Time)==9*60) 
    #write_csv(rnights,"tmp-rnights.csv")
    maxwind<-max(dfsum$wind)
    minwind<-min(dfsum$wind)
    
  dfsum$windhr<-roll_sum(dfsum$wind/12,n=12,fill=0)
  dfsum$wind8hr<-roll_sum(dfsum$wind/12,n=12*8,fill=0)
  minwindhr<-min(dfsum$windhr[dfsum$windhr>0])
  maxwindhr<-max(dfsum$windhr)
  minwind8hr<-min(dfsum$wind8hr[dfsum$wind8hr>0])
  maxwind8hr<-max(dfsum$wind8hr)
  print(paste0("MinWindHr ",minwindhr))
  print(paste0("MaxWindHr ",maxwindhr))
  print(paste0("MinWind8Hr ",minwind8hr))
  print(paste0("MaxWind8Hr ",maxwind8hr))
  write_csv(dfsum,"xxx1.csv")
  
#      p("NSW - annual avg power 8.5GW (2023/4)"),
#      p("QLD - annual avg power 7.1GW (2023/4)"),
    onshortages<-""
    onbattmult<-""
    for(i in 1:nrow(rnights)) {
      d<-rnights$diff[i]
      t<-day(rnights$Time[i])
      if (d<0) {
        onshortages<-paste0(onshortages," ",t,":",comma(-d)," MWh ")
        onbattmult<-paste0(onbattmult," ",t,":",comma(-d/input$bsize),"x ")
      }
    }
    
    
    nperiods<-length(dfsum$Time)
    tags$div(class="standout-container",
      annual,
      p("Total Period Demand: ",comma(totdemand/1000)," GWh"),
      p("Period length: ",comma((nperiods/12)/24)," days"),
      p("Overbuild factor: ",comma(input$ofac),""),
#     p("Electrification expansion factor: ",comma(input$dfac),""),
      p("Interconnector export size: ",comma(input$icsize)," MWh"),
      p("Baseload size: ",comma(input$baseloadsize)," MW"),
      p("Shortfall over period: ",comma(sh/1000)," GWh (wind+solar+batteries=",comma((totdemand-sh)/totdemand*100),"%)"),
      p("Curtailment: ",comma(curt/1000)," GWh (",comma(100*curt/dmand),"percent)"),
      p("Max MW shortage: ",comma(shortMW)," dispatchable MW"),
      p("Battery energy storage size: ",comma(input$bsize)," MWh (",comma((input$bsize*1e6)/(avgpower*1e9))," hrs)" ),
      p("Battery energy supplied: ",comma(bsup/1000)," GWh"),
      p("Max battery power: ",comma(bmax)," MW"),
      p("Max wind power(5 min): ",comma(maxwind)," MW (Min ",comma(minwind),"MW",comma(minwind/maxwind*100),"%)"),
      p("Max wind energy(60 min): ",comma(maxwindhr)," MWh (Min ",comma(minwindhr),"MWh",comma(minwindhr/maxwindhr*100),"%)"),
      p("Max wind energy(8 hrs): ",comma(maxwind8hr)," MWh (Min ",comma(minwind8hr),"MWh",comma(minwind8hr/maxwind8hr*100),"%)"),
      p("Battery capacity factor: ",comma(100*bsup/((bmax/12)*nperiods)),"%"),
      {if (length(r$diff)==1) {
      p("Max ",comma(hrs),"hr shortage (endpoint) ",r$Time,": ",comma(-r$diff),"MWh")
      }},
      p("Overnight Shortages: ",onshortages),
      p("Battery shortfallss: ",onbattmult),
      p("(Battery shortfalls ... the multiple of configured batterysizes to supply the shortfall)"),
      p("")
    )
  })
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
  output$batheat<-renderImage(list(src="suzanne-bat.jpg",height=400),deleteFile=FALSE)
  output$connectgit<-renderImage(list(src="aemo-kuhlmann-git.png",height=300),deleteFile=FALSE)
  output$connect<-renderImage(list(src="modelling-rms-vs-emt.png",height=300),deleteFile=FALSE)
  output$blacksummer<-renderImage(list(src="wind-vs-demand-heatwave-2019.png",height=300),deleteFile=FALSE)
  output$scaleissues1<-renderImage(list(src="renewable-scaleissues.jpg",height=300),deleteFile=FALSE)
  output$scaleissues2<-renderImage(list(src="renewable-scaleissues-mod.png",height=400),deleteFile=FALSE)
#  output$scaleissues2b<-renderImage(list(src="renewable-scaleissues-mod.png",height=400),deleteFile=FALSE)
  output$weekpng<-renderImage(list(src="WeekEnding30-11-2023.png",height=400),deleteFile=FALSE)
  output$kwpercapsolar<-renderPlot({
      dfkwPerCapSolar %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap),width=0.6,fill="yellow") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",18),"red","black"))) + 
      ptheme +
      labs(x="",y="kilowatts per person",title="Solar: Top 20 countries by kilowatts per person")
  })
  output$wsbh<-renderPlot({
    c<-input$countrypick
    print(c)
    p<-dfwsbh %>% filter(Country %in% c) %>% 
      { if (input$cmpnr==TRUE) filter(.,Type %in% c("Nuclear","Wind","Solar")) else  . } %>%
      mutate(Country=reorder(Country,desc(Population))) %>% 
      ggplot() + 
      #geom_point(aes(x=Country,y=TWh*1e9/Population),shape=5,data=dfel) +
      {if (input$ieatarget) geom_col(aes(x=Country,y=kWhPerCapTimes2,fill="IEA2050Target"),width=0.6,
               data=dfel %>% filter(Country %in% c) %>% mutate(Country=reorder(Country,desc(Population))) %>% mutate(kWhPerCapTimes2=(TWh*1e9/Population)*2),
               alpha=1) 
        }+
      {if (input$kwhpercap) geom_col(aes(x=Country,y=kWhPerCap,fill="kWh/Person"),width=0.55,
               data=dfel %>% filter(Country %in% c) %>% mutate(Country=reorder(Country,desc(Population))) %>% mutate(kWhPerCap=(TWh*1e9/Population)),
               alpha=1) 
      }+
      geom_col(aes(x=Country,y=kWhPerCap,fill=Type),width=0.5) + 
      scale_fill_manual(name="Technology",values=cols) +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.6)) +
      theme(plot.margin=unit(c(1,0,0,0),"cm")) +
      theme(legend.key.size = unit(7, 'mm')) +
      ptheme +
      labs(x="",y="Annual low carbon\nkilowatt-hours per person",
           title="Per person low carbon electricity 2022")
      p
      #ggplotly(p) %>% ggplconfig %>% layout(plot_bgcolor = "#e5ecf6")
  })
  output$kwpercapwind<-renderPlot({
      dfkwPerCapWind %>% ggplot() + geom_col(aes(x=reorder(Country,kwPerCap),y=kwPerCap,fill=Group),width=0.6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
        color=c(rep("black",7),"red",rep("black",12),"red"))) + ptheme + 
      scale_fill_manual(values=c("forestgreen","forestgreen"))+guides(fill="none")+
      labs(x="",y="kilowatts per person",title="Wind: Top 20 countries (plus SA) by kilowatts per person")
  })
  output$shortfall<-renderPlot({
      dfsum<-gendfsum()
      #write_csv(dfsum,"xxx1.csv")
      dfcumshort<-dfsum %>% select(Time,batteryStatus,wind,demand,cumShortMWh,maxShortMW,renew,dblrenew,cumThrowOutMWh)
      bl<-ifelse(input$baseloadsize>0,paste0("BL",input$baseloadsize,"MW"),"nobl")
      bsz<-ifelse(input$bsize>0,paste0("BATT",comma(input$bsize),"MW"),"nobatt")
      ovfac<-ifelse(input$ofac>0,paste0("FAC",comma(input$ofac),""),"nooverbuild")
      ff<-gsub(" ","",input$datasetpick)
      fname=paste0("dfsum-",bl,"-",bsz,"-",ovfac,"-",ff,".csv")
      
      write_csv(dfsum,fname)
      thecols=colsshort
      thelabs=labsshort
      dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","renew","dblrenew"),names_to="Level",values_to="MW") 
      thetitle=dataSetTitles[input$datasetpick]
      if (input$showWindDemand) {
          dfcs<-dfcumshort %>% pivot_longer(cols=c("demand","wind"),names_to="Level",values_to="MW") 
          x<-str_split_1(thetitle,"\n")
          print(x)
          thetitle<-paste0("Electricity demand vs wind\n",x[2])
          thecols=colswind
          thelabs=labswind
      }
      nperiods<-length(dfsum$Time)
      lab<-c()
      val<-c()
      if (input$showShort) {
        lab=c("Shortfall")
        val=c("dashed")
      }
      if (input$showCurtailed) {
        lab=c(lab,"Curtailment")
        val=c(val,"dotted")
      }
      if (input$showInterconnector) {
        lab=c(lab,"Interconnector Export")
        val=c(val,"twodash")
      }
      nightbands<-findBands(dfsum)
      for(i in 1:nrow(nightbands)) {
        cat(paste0())
      }
      bfac=input$bsize
      if (bfac==0) bfac=1000
      p<-dfcs %>% ggplot() + 
        geom_line(aes(x=Time,y=MW,color=Level)) +  
        ptheme +
        {if (input$showShort)
            geom_line(aes(x=Time,y=cumShortMWh*coef/1000,linetype="dashed"),data=dfsum)
        }+
        {if (input$showCurtailed)
        geom_line(aes(x=Time,y=cumThrowOutMWh*coef/1000,linetype="dotted"),data=dfsum)
        }+
        {if (input$showInterconnector)
        geom_line(aes(x=Time,y=cumIcExpMWh*coef/1000,linetype="twodash"),data=dfsum)
        }+
        {if (input$showBatteryStatus)  
             geom_line(aes(x=Time,y=(batteryStatus/input$bsize)*bfac),color="red",data=dfsum)
        }+
        {if (input$baseloadsize)  
             geom_hline(aes(yintercept=baseloadsize),color="purple",data=dfsum)
        }+
        {if (input$showBatteryStatus)  
            annotate('text',x=dfcs$Time[nperiods/2],y=input$bsize,label="100% full",color="red",vjust=-0.2,hjust=0)
        }+
        {if (input$showBatteryStatus)  
            geom_col(aes(x=Time,y=shortFall*12,alpha=0.2),color="orange",data=dfsum,show.legend=FALSE)
        }+
        geom_rect(aes(xmin=t1,xmax=t2,ymin=0,ymax=Inf),data=nightbands,alpha=0.2)+
        labs(color="Megawatts",title=thetitle)+
        scale_color_manual(labels=thelabs,values=thecols)+
        scale_linetype_manual(name="Gigawatt-hours",labels=lab,values=val)+
        scale_y_continuous(
          name="Megawatts",
          sec.axis = sec_axis(~./coef, name="Cumulative shortfall/curtailment in GWh")
        )+theme(legend.direction="vertical",legend.box="vertical")
        p
#        baseloadsize
  })
  output$facilities=renderPlot({
    dfpower %>% group_by(Technology) %>% summarise(MW=sum(`Generator Capacity (MW)`)) %>%
      ggplot()+geom_col(aes(x=Technology,y=MW,fill=Technology),width=0.6)+
      ptheme  + geom_text(aes(x=Technology,y=MW,label=MW),hjust=-0.1)+ylim(0,3500)+
      coord_flip()+labs(title="Power ratings of available \ngenerators in SA",x="",y="Megawatts")+ 
      scale_fill_manual(name="Technology",values=colsfacilities)+guides(fill="none")
  })
  
}
shinyApp(ui=ui,server=server)



                  


