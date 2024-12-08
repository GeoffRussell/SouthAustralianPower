#-----------------------------------------------------------------------------------
# This is an app to illustrate scaling difficulties in producing enough
# batteries to decarbonise transport ... but it has grown!
#-----------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(extrafont)
library(tippy)
library(MASS)
library(markdown)
library(googleway)
library(geosphere)
library(ggmap)
library(leaflet)
library(leaflet.providers)
library(plotly)
library(readxl)
library(lubridate)
library(shinyjs)
library(RColorBrewer)

#library(reactlog)
#options(shiny.reactlog=TRUE)

dfc<-read_csv("unfccc-summarydata2020.csv",na=c("n/a","NA")) %>% filter(Country %in% c("AUS","DEU","FRA")) %>% mutate(pce=1000*Electricity/Population)
dflu<-read_csv("landuse1.csv")


comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
commac<-function(x) formatC(x,digits=0,big.mark=",",format="f")
ncomma<-function(x) toString(signif(x,digits=4))
comma2<-function(x) prettyNum(signif(x,digits=2),big.mark=",")
comma3<-function(x) prettyNum(signif(x,digits=3),big.mark=",")

bpfile<-"bp-stats-review-2021-all-data.xlsx"
#------------------------------------------------------------------------------
# Various constants ... mostly IEA Future of Hydrogen
#------------------------------------------------------------------------------

currProdH2<-69e6         # Current production of hydrogen
ElCurrProdH2<-3600e12    # Electricity (watt-hours) to make current production of hydrogen with electrolysis
ElPerTonneH2<-ElCurrProdH2/currProdH2 # this is about 52 MWh/tonne, but the embodied energy is about 39.4 MWh/tonne
ElSynOnePercOil<-1000e12 # Electricity to make synthetic hydrocarbons to replace one percent of oil
ElSynOnePercGas<-700e12  # Electricity to make synthetic hydrocarbons to replace one percent of gas
ElGlobal2020<-27000e12   # Global electricity 

# Following data from ... 
# https://energypolicy.columbia.edu/sites/default/files/pictures/CGEP_Electric%20Vehicles%20and%20Their%20Impact%20on%20Oil%20Demand-Why%20Forecasts%20Differ.pdf
#
globaloil<-94e6          # Global oil barrels per day 
caroil<-25e6             # Global oil barrels per day 
# Wikipedia
jperbarrel<-6.118e9      # Joules in barrel of oil
jperkgh2<-120e6          # joules in kg of h2
globaloiljoules<-jperbarrel*globaloil*365 # joules in global oil
caroiljoules<-jperbarrel*caroil*365 # joules in global car oil
repoilh2<-globaloiljoules/jperkgh2 # kg of h2 to replace oil
repoilh2car<-caroiljoules/jperkgh2 # kg of h2 to replace car oil
# Assume that fuelcell is 2.5 times more efficient https://afdc.energy.gov/fuels/hydrogen_basics.html#:~:text=Hydrogen%20as%20an%20Alternative%20Fuel&text=In%20fact%2C%20a%20fuel%20cell,combustion%20engine%20running%20on%20gasoline.&text=The%20energy%20in%202.2%20pounds,%2C%202.8%20kilograms)%20of%20gasoline.
repoilh2car<-repoilh2car*(1/2.5)
globalgasexajoules<-140                     # 2019 figure (BPstats 2021)
gigasize<-35 # in GW
#-------------------------------------------------
# sectoral breakdown of gas usage
# https://www.iea.org/data-and-statistics/charts/world-natural-gas-final-consumption-by-sector-2018
residentialgasexa<-0.30*globalgasexajoules               # what proportion of res gas is heating?
represgash2mt<-(residentialgasexa*1e18/jperkgh2)/1e9  # mt of h2 to replace residential gas heating




# check
globaloilexajoules<-globaloiljoules/1e18    # this is 209 ... BP says 191 in 2019 (BPStats 2021)

#-------------------------------------------------------------------------
# Energy (watt-hours) per tonne of Uranium (in LWR) ... figures from Australian energy audit
EPerTonneU<-924e12/7679  # this is about 120 GWh/tonne (Thermal) ... or about 40 GWh/tonne (Electrical)
JPerTonneU<-EPerTonneU*3600     # 3600 joules in a watt-hour
#----------------------------------------------------------------
# Asian Renewable Energy Hubo
# https://www.epa.wa.gov.au/sites/default/files/Referral_Documentation/Supporting%20Document_20.pdf
# and https://asianrehub.com/about/
# AREH website says 26GW of wind and solar => implies each turbine is 8.5mw (290m high)
#----------------------------------------------------------------
AREHel<-100e12               # annual output
AREHsol<-10.8e9              # 18*600MW
AREHwind<-1743*8.5e6         #
AREHcapf<-0.44               # implied by 100TWh from 26GW wind+solar
AREHha<-668100
AREHclearing<-20810
AREHnh3<-27106*365           # tonnes of nh3 per year
EintonneNH3<-5.17e6         # embodied watt-hours per tonne      
EintonneH2<-36.4e6          # embodied watt-hours per tonne (half way between LHV and HHV)      
#---------------------------------------------------------------------
# Efficiency of making H2 is the electricity in each tonne over electricity used to make it
#---------------------------------------------------------------------
EffofH2<-EintonneH2/ElPerTonneH2

print(paste0("EffofH2:",EffofH2*100,"%    ElPerTonneH2 (MW):",ElPerTonneH2/1e6))



#----------------------------------------------------------------
# Areas
#----------------------------------------------------------------
locations<-tribble(
  ~name,~lat,~lon,
  "Leibstadt Nuclear Plant, Switzerland",47.59938,8.1790203,
  "Adelaide",-34.92841799629078, 138.5973867125219,
  "Sydney",-33.82511309691236, 151.18461943498028,
  "London",51.510362826811175, -0.13046736570532447,
  "New York",40.6971494,-74.2598693,
  "Paris",48.8588377,2.2770201,
  "Sizewell C",52.21651324,1.61928223,
  "Hinkley Point C",51.20654984,-3.12990999,
  "Olympic Dam (Copper mine)",-30.4405142,136.8289900,
  "Fukushima",37.422458,141.026594
)
# https://en.wikipedia.org/wiki/Dau_Tieng_Solar_Power_Project
solarfarms<-tribble(
   ~name,~area,~lat,~lon,~npanels,~pweight,~capacity,~el,
   "Nyngan",250,-31.555360, 147.086372,1.36e6,12,102e6,233e9,
   "Dau Tieng",500,11.448979, 106.233591,1.4e6,NA,600e6,694000e6,
   "Copper Mountain",16200, 35.783333,-114.991667,9e6,12,802,1348e9
)
ieagoals<-tribble(
  ~Technology,~GW2020,~GW2050,~TWh2050,~TWh2020,
  "Solar",737,14458,23469,821,
  "Wind",737,8265,24785,1592,
  "Hydro",1327,2599,8461,4418,
  "BioEnergy",171,640,3267,718,
  "CSP",6,426,1386,14,
  "Nuclear",415,812,5497,2698,
  "Hydrogen",0,1867,1713,0,
  "FossilCCUS",1,394,1332,4,
  "UnabatedFossil",4368,677,259,16382,
  "Battery",18,3097,0,0
)
areas<-tribble(
    ~id,~name,~ha,~lat,~lon,
    1,"Tasmania",6840100,-42.068540,146.642073,
    2,"West Virginia",6275600,38.955297,-80.453396,
    3,"France",64384100,46.917466,2.5926823,
    4,"South Korea",10021000,36.798385,127.930143,
    5,"Thailand",51312000,16.073111,101.891854,
    6,"Mutanda Cobalt Mine, Congo",2000,-10.785456, 25.809515,
    7,"Macarthur River Lead/Silver Mine, Australia",2000,-16.421591,136.083620,
    8,"Macarthur River Uranium Mine, Canada",71,57.761111,-105.051667,
    9,"Olympic Dam Mine, South Australia",2000,-30.4405142,136.8289900
)
#------------------------------------------------------------
# Various initial constants and closures
#------------------------------------------------------------
mtbfpv<-30
bkuphours<-4
years<-30
pvProdGWnow<-107      
windProdGWnow<-93  #       https://reneweconomy.com.au/explainer-what-happens-to-old-wind-turbines/
batteryGWhPerYearnow<-396      
batteryGrowthPercent<-4.3
prodgrowthpercent<-4.3
prodgrowthpercent<-12
tonnesperGWPV<-75000
pvRecycleGrowthPercent<-20
batteryRecycleGrowthPercent<-20
pvRecycleGWnow<-100
windRecycleGWnow<-10
batteryRecycleGWhnow<-20
pvStartingGW<-737
windStartingGW<-737
caskweight<-126

makeWeightFunction<-function(tonnePerGW,years) {
  function(year,GW) {
    tonnePerGW*GW;
  }
}
makeProduction<-function(peryear,years) {
  function(year) { peryear; }
}
makeExpProduction<-function(percent,peryear,years) {
  function(year) { peryear*((1+percent/100)^year); }
}
makeRecycle<-function(percent,GWcapnow) {
  function(failed,year) {
    capacity<-GWcapnow*((1+percent/100)^year)
    ifelse(failed<capacity,failed,capacity)
  }
}
makeFail<-function(mtbf) {
  lambda<-1/mtbf
  function(n,p) {
    (1-exp(-lambda*(n-1)))*p
    #cat(paste0("n,p,f:",n,",",p,",",r,"\n"))
  }
}
pvProductionFun<-makeExpProduction(prodgrowthpercent,pvProdGWnow,years)
pvRecycleFun<-makeRecycle(pvRecycleGrowthPercent,pvRecycleGWnow)
batteryProductionFun<-makeExpProduction(batteryGrowthPercent,batteryGWhPerYearnow,years)
batteryRecycleFun<-makeRecycle(batteryRecycleGrowthPercent,batteryRecycleGWhnow)

calculateBatteryFuture<-function(years,batteryProductionFun,batteryRecycleFun,
                                 batteryenergydensity,batterylifespan,evreserved,batterysize) {
  lambda<-1/batterylifespan
  tonnePerGWh<-(1e9/batteryenergydensity)/1000
  print(paste0("calculateBatteryFuture gfsize ",gigasize))
  batteryFailFun<-makeFail(batterylifespan)
  batteryWeightFun<-makeWeightFunction(tonnePerGWh,years)
  y1<-ymd('2021-01-01')
  y2<-ymd('2021-01-01') %m+% years(years-1)
  print(y2)
  # first we lay out the production schedule
  df<-tibble(
    produced=(1:years %>% map_dbl(batteryProductionFun)),
    gigafactories=produced/gigasize,
    evbatteries=produced*(evreserved/100)*1e9/(batterysize*1e3),
    otherbatteries=produced*((100-evreserved)/100)*1e9/(batterysize*1e3),
    year=seq(y1,y2,by = '1 year')
  )
  cat(paste0("Battery production:",toString(df$produced)))
  cat(paste0("Battery growth:",toString(batteryGrowthPercent)))
  cat(paste0("Battery prod/yr now:",toString(batteryGWhPerYearnow)))
  cat(paste0("Gigafactories:",toString(df$gigafactories)))
  
  batteryFailed<-rep(0,years)
  batterypvRecycled<-rep(0,years)
  batteryRecycledGWh<-rep(0,years)
  # we run a simple exponential failure model with time in years, nothing fails in year 0. 
  # first we set up failures for the currently created batterys (assumed to be 1 years worth)  
  startingGWh<-batteryGWhPerYearnow
#  for(n in 1:(years-1)) {
#    batteryFailed[n]<-batteryFailed[n]+batteryFailFun(n+1,startingGWh)
#    #cat(paste0("n,batteryFailed[n]:",n,",",batteryFailed[n],"\n"))
#  } 
  print("BT1")
  for(i in 1:years) {
    if (i<years) {
      for(n in (i+1):years) {
        batteryFailed[n]<-batteryFailed[n]+batteryFailFun(n-i,df$produced[i])
        #cat(paste0("i,n,batteryFailed[n]:",i,",",n,",",batteryFailed[n],"\n"))
      }
    }
  }
  print("BT2")
  df$batteryFailed<-batteryFailed 
  df$batteryRecycled<-map2_dbl(batteryFailed,1:years,batteryRecycleFun) 
  df<-df %>% mutate(batteryWaste=batteryFailed-batteryRecycled,batteryProduced=cumsum(produced),
                    batteryOperational=batteryProduced-batteryFailed+batteryRecycled+startingGWh)
  # Now calculate weights of everything 
  df$batteryWasteTonnes<-map2_dbl(1:years,df$batteryWaste,batteryWeightFun) 
  df$batteryFailedTonnes<-map2_dbl(1:years,df$batteryFailed,batteryWeightFun) 
  df$batteryOperationalTonnes<-map2_dbl(1:years,df$batteryOperational,batteryWeightFun) 
  df$batteryRecycledTonnes<-map2_dbl(1:years,df$batteryRecycled,batteryWeightFun) 
  df$batteryProducedTonnes<-map2_dbl(1:years,df$batteryProduced,batteryWeightFun) 
  df
}
calculatePVfuture<-function(years,prodfun,pvRecycleFun,batteryRecycleFun,mtbfpv=25,bkuphours=4,mtbfbatt=15,
                            startingGW=pvStartingGW,tonnePerGW=66000,batteryenergydensity=150,pvcf=14) {
  print("calculatePVfuture")
  print(paste0("years: ",years))
  print(paste0("mtbfpv: ",mtbfpv))
  print(paste0("bkuphours: ",bkuphours))
  print(paste0("mtbfbatt: ",mtbfbatt))
  
  pvFailFun<-makeFail(mtbfpv)
  batteryFailFun<-makeFail(mtbfbatt)
  panelWeightFun<-makeWeightFunction(tonnePerGW,years)
  batteryWeightFun<-makeWeightFunction((1e9/batteryenergydensity/1000),years) # tonnes per GWh
  y1<-ymd('2021-01-01')
  y2<-ymd('2021-01-01') %m+% years(years-1)
  print(y2)
  # first we lay out the production schedule
  df<-tibble(
    produced=(1:years %>% map_dbl(prodfun)),
    year=seq(y1,y2,by = '1 year')
  )
  
  pvFailed<-rep(0,years)
  pvRecycled<-rep(0,years)
  batteryRecycledGWh<-rep(0,years)
  # we run a simple exponential failure model with time in years, nothing fails in year 0. 
  # first we set up failures for the currently installed panels 
  for(n in 1:(years-1)) {
    pvFailed[n]<-pvFailed[n]+pvFailFun(n+1,startingGW)
    #cat(paste0("n,pvFailed[n]:",n,",",pvFailed[n],"\n"))
  } 
  for(i in 1:years) {
    if (i<years) {
      for(n in (i+1):years) {
        pvFailed[n]<-pvFailed[n]+pvFailFun(n-i,df$produced[i])
        #cat(paste0("i,n,pvFailed[n]:",i,",",n,",",pvFailed[n],"\n"))
      }
    }
  }
  # Now we factor in recycling  
  df$pvFailed=pvFailed 
  df$pvRecycled<-map2_dbl(pvFailed,1:years,pvRecycleFun) 
  df<-df %>% mutate(pvWaste=pvFailed-pvRecycled,total_produced=cumsum(produced),
                    pvOperational=total_produced-pvFailed+pvRecycled+startingGW)
  # And add batteries to support operational GW 
  df<-df %>% mutate(batteryGWh=pvOperational*bkuphours) 
  # Now calculate weights of everything 
  df$pvWasteTonnes<-map2_dbl(1:years,df$pvWaste,panelWeightFun) 
  df$pvFailedTonnes<-map2_dbl(1:years,df$pvFailed,panelWeightFun) 
  df$pvOperationalTonnes<-map2_dbl(1:years,df$pvOperational,panelWeightFun) 
  df$pvRecycledTonnes<-map2_dbl(1:years,df$pvRecycled,panelWeightFun) 
  df$total_producedTonnes<-map2_dbl(1:years,df$total_produced,panelWeightFun) 
  df$batteryGWhTonnes<-map2_dbl(1:years,df$batteryGWh,batteryWeightFun) 
  df<-df %>% mutate(addedBatteryGWh=batteryGWh-lag(batteryGWh))
  df$addedBatteryGWh[1]=0
  # Now we add in the battery failures
  failedBatteryGWh<-rep(0,years)
  for(i in 1:years) {
    if (i<years) {
      for(n in (i+1):years) {
        failedBatteryGWh[n]<-failedBatteryGWh[n]+batteryFailFun(n,df$addedBatteryGWh[i])
        #cat(paste0("i,n,failed[n]:",i,",",n,",",failed[n],"\n"))
      }
    }
  }
  df$failedBatteryGWh<-failedBatteryGWh
  df$recycledBatteryGWh<-map2_dbl(failedBatteryGWh,1:years,batteryRecycleFun) 
  df$failedBatteryGWhTonnes<-map2_dbl(1:years,df$failedBatteryGWh,batteryWeightFun) 
  df$recycledBatteryGWhTonnes<-map2_dbl(1:years,df$recycledBatteryGWh,batteryWeightFun) 
  df <- df %>% mutate(wasteBatteryGWhTonnes=failedBatteryGWhTonnes-recycledBatteryGWhTonnes)
  #---------------------------------------------------------------
  # now we calculate equivalent nuclear plants and waste output
  # we only consider the cask weight, not the actual waste weight
  #---------------------------------------------------------------
  thorconEquivalent<-rep(0,years+5)
  wasteperyear<-30
  thorconEquivalent[1]<-df$pvOperational[1]*(pvcf/90)
  for(i in 2:years) {
      p<-df$pvOperational[i]-df$pvOperational[i-1]
      thorconEquivalent[i]<-p*(pvcf/90)
  }
  df$thorconEquivalent<-thorconEquivalent[1:years]
  df<-df %>% mutate(nwaste=cumsum(thorconEquivalent)*wasteperyear)
  #-----------------------------------------------------------------
  print(names(df))
  df
}
splitter<- function(s) {
  gsub('(.{1,40})(\\s|$)', '\\1\n', s)
}
getAfricaList<-function() {
  dfa<-read_csv("africa.csv") %>% select(Country,Population,InBP) %>% filter(InBP=="no"&Population>5e6)
  dfb<-read_csv("rowCountries.csv") %>% select(Country,Population,InBP) %>% filter(InBP=="no")
  bind_rows(dfa,dfb)
}
makeString <-function(.data) {
  (.data %>% mutate(str=splitter(paste(paste0(Country," ",comma(Population/1e6),"m"),sep=", ",collapse=", "))))$str[1]
}
calculateWindfuture<-function(years,prodfun,windRecycleFun,batteryRecycleFun,mtbfwind=25,bkuphours=4,mtbfbatt=15,
                            startingGW=windStartingGWwind,tonnePerGW=180000,batteryenergydensity=150) {
  print("calculateWindFuture")
  print(paste0("years: ",years))
  print(paste0("mtbfwind: ",mtbfwind))
  print(paste0("bkuphours: ",bkuphours))
  print(paste0("mtbfbatt: ",mtbfbatt))
  
  windFailFun<-makeFail(mtbfwind)
  batteryFailFun<-makeFail(mtbfbatt)
  windWeightFun<-makeWeightFunction(tonnePerGW,years)
  batteryWeightFun<-makeWeightFunction((1e9/batteryenergydensity/1000),years) # tonnes per GWh
  y1<-ymd('2021-01-01')
  y2<-ymd('2021-01-01') %m+% years(years-1)
  # first we lay out the production schedule
  df<-tibble(
    produced=(1:years %>% map_dbl(prodfun)),
    year=seq(y1,y2,by = '1 year')
  )
  windFailed<-rep(0,years)
  windRecycled<-rep(0,years)
  batteryRecycledGWh<-rep(0,years)
  # we run a simple exponential failure model with time in years, nothing fails in year 0. 
  # first we set up failures for the currently installed winds 
  for(n in 1:(years-1)) {
    windFailed[n]<-windFailed[n]+windFailFun(n+1,startingGW)
    #cat(paste0("n,windFailed[n]:",n,",",windFailed[n],"\n"))
  } 
  for(i in 1:years) {
    if (i<years) {
      for(n in (i+1):years) {
        windFailed[n]<-windFailed[n]+windFailFun(n-i,df$produced[i])
        #cat(paste0("i,n,windFailed[n]:",i,",",n,",",windFailed[n],"\n"))
      }
    }
  }
  # Now we factor in recycling  
  df$windFailed=windFailed 
  df$windRecycled<-map2_dbl(windFailed,1:years,windRecycleFun) 
  df<-df %>% mutate(windWaste=windFailed-windRecycled,total_produced=cumsum(produced),
                    windOperational=total_produced-windFailed+windRecycled+startingGW)
  # And add batteries to support operational GW 
  df<-df %>% mutate(batteryGWh=windOperational*bkuphours) 
  # Now calculate weights of everything 
  df$windWasteTonnes<-map2_dbl(1:years,df$windWaste,windWeightFun) 
  df$windFailedTonnes<-map2_dbl(1:years,df$windFailed,windWeightFun) 
  df$windOperationalTonnes<-map2_dbl(1:years,df$windOperational,windWeightFun) 
  df$windRecycledTonnes<-map2_dbl(1:years,df$windRecycled,windWeightFun) 
  df$total_producedTonnes<-map2_dbl(1:years,df$total_produced,windWeightFun) 
  df$batteryGWhTonnes<-map2_dbl(1:years,df$batteryGWh,batteryWeightFun) 
  df<-df %>% mutate(addedBatteryGWh=batteryGWh-lag(batteryGWh))
  df$addedBatteryGWh[1]=0
  # Now we add in the battery failures
  failedBatteryGWh<-rep(0,years)
  for(i in 1:years) {
    if (i<years) {
      for(n in (i+1):years) {
        failedBatteryGWh[n]<-failedBatteryGWh[n]+batteryFailFun(n-i,df$addedBatteryGWh[i])
        #cat(paste0("i,n,failed[n]:",i,",",n,",",failed[n],"\n"))
      }
    }
  }
  df$failedBatteryGWh<-failedBatteryGWh
  df$recycledBatteryGWh<-map2_dbl(failedBatteryGWh,1:years,batteryRecycleFun) 
  df$failedBatteryGWhTonnes<-map2_dbl(1:years,df$failedBatteryGWh,batteryWeightFun) 
  df$recycledBatteryGWhTonnes<-map2_dbl(1:years,df$recycledBatteryGWh,batteryWeightFun) 
  df <- df %>% mutate(wasteBatteryGWhTonnes=failedBatteryGWhTonnes-recycledBatteryGWhTonnes)
  df
  print(names(df))
  df
}

extractMT<-function(.data) {
  .data %>% pivot_longer(ends_with("Tonnes"),names_to="Materials",values_to="Tonnes") %>% select(year,Materials,Tonnes) %>% mutate(MegaTonnes=Tonnes/1e6) %>% select(year,Materials,MegaTonnes)
}
extractMaxMT<-function(.data) {
  .data %>% select(ends_with("Tonnes")) %>% map_dfr(function(x) max(x)/1e6)
}
extractStates<-function(.data) {
  .data %>% pivot_longer(c("pvOperational","pvFailed","total_produced","pvWaste","pvRecycled"),
                         names_to="State",values_to="GW") %>%  select(year,State,GW)
}
extractMaxStates<-function(.data,key) {
  .data %>% select(starts_with(key)) %>% map_dfr(function(x) max(x))
}
#------------------------------------------------------------
# Various functions
#------------------------------------------------------------
calculateWaterBoiledByWattHours<-function(wh) {
      # 1gm water by 1 deg C = 1 calorie = 4.184 joules = 4.184 = 4.184 Wseconds * 1/3600 = 0.001162222 Wh
      # 1gm watery by 80 dev C = 0.09297778  Wh
      gm80<-0.09297778
      grams<-wh/gm80
}
calculateSarea<-function(pvperyear) {
      haPerGW=nynArea/(nynPow/1e9)
      years<-30
      area<-rep(0,years)
      area[1]=pvperyear*haPerGW
      for(n in 2:30) {
        area[n]=area[n-1]+area[1]
      }
      df<-tibble(year=seq(ymd('2021-01-01'),ymd('2050-01-01'), by = '1 year'),
                 area=area)
      df
}
#calculateNwaste<-function(pvperyear) {
#  pvcapacityfactor<-0.14
#  nukesperyear<-pvperyear*0.14/0.9  # 1GW Thorcon
#  years<-30
#  caskweight<-126    # weight of one cask ... 2 needed every 4 years per GW
#  inst<-rep(0,years+5)
#  casktonnes<-rep(0,years+5)
#  for(n in 2:years) {
#    inst[n]=inst[n-1]+nukesperyear
#    if ((n+3)<=years) {
#      for(m in seq(n+3,years,by=4)) {
#        casktonnes[m]=casktonnes[m]+nukesperyear*caskweight
#      }
#    }
#  }
#  df<-tibble(year=seq(ymd('2021-01-01'),ymd('2050-01-01'), by = '1 year'),
#                 installed=inst[1:years],casktonnes=casktonnes[1:years])
#  df
#         
#}
calculateNukeSteel<-function(nuketwh2050) {
  # we ignore current nuclear reactors
  # except to set goal
  currentnuketwh<-2700
  nuke2020GW<-currentnuketwh/7.88
  years<-30
  UperGW<-64.58*2
  currentUperGW<-180
  twhpernuke<-1000e6*24*365*0.9/1e12
  nuketwhperyear<-(1000*nuketwh2050-currentnuketwh)/years
  nukesperyear<-nuketwhperyear/twhpernuke
  
  print(paste0("nuketwh2050: ",nuketwh2050))
  print(paste0("nuketwhperyear: ",nuketwhperyear))
  print(paste0("nukesperyear: ",nukesperyear))
  
  totalsteelIsle<-rep(0,years)
  totalsteelLand<-rep(0,years)
  totalNukeGW<-rep(0,years)
  extraNukeGW<-rep(0,years)
  totalNukeTWh<-rep(0,years)
  extraU<-rep(0,30)
  
  totalsteelIsle[1]=nukesperyear*steelPerGWThIsle
  totalsteelLand[1]=nukesperyear*steelPerGWThLand
  totalNukeGW[1]=nukesperyear+nuke2020GW
  extraNukeGW[1]=nukesperyear
  totalNukeTWh[1]=currentnuketwh+nukesperyear*twhpernuke
  extraU[1]=nukesperyear*UperGW
  
  for(n in 2:years) {
    totalsteelIsle[n]=totalsteelIsle[n-1]+(nukesperyear*steelPerGWThIsle)
    totalsteelLand[n]=totalsteelLand[n-1]+(nukesperyear*steelPerGWThLand)
    totalNukeGW[n]=totalNukeGW[n-1]+nukesperyear
    extraNukeGW[n]=extraNukeGW[n-1]+nukesperyear
    extraU[n]=extraU[n-1]+UperGW*nukesperyear
  }
  df<-tibble(year=seq(ymd('2021-01-01'),ymd('2050-01-01'), by = '1 year'),
                 totalsteelIsle=totalsteelIsle,
                 totalsteelLand=totalsteelLand,
             extraNukes=extraNukeGW,uranium=extraU)
  df
}
ggplconfig <- function(.data) {
    config(.data,displayModeBar = "static", displaylogo = FALSE, 
              modeBarButtonsToRemove = list("autoScale2d", 
                "hoverClosestCartesian", "hoverCompareCartesian", 
                "select2d", "zoomIn2d", "zoomOut2d","lasso2d","toggleSpikelines"))
}

getLNGdata<-function() {
# The lng exports series starts at 2000, unlike gas production figures which go back to 1970
	lngexportsbcm<-read_excel(bpfile,
        	sheet="Gas - LNG exports",na=c("n/a"),skip=2,col_types=c('text',rep('numeric',24))) %>% 
		rename(Country="Billion cubic metres") %>% filter(!is.na(Country)) %>% slice_head(n=28)

# Key factors ... BCM*36=PJ   PJ/1000=EJ (exajoules ... 10^18)
# 120MJ/kg of H2
		H2ProcessEfficiency=1/0.7
		ElPerTonneH2<-52e6     # electricity to make a tonne of H2
lngtotals<-lngexportsbcm %>% filter(grepl("Total",Country)) %>% 
		rename(`2020`=`2020...22`) %>% 
		select(`Country`,`2000`:`2020`) %>% 
		arrange(desc(`2020`)) %>% slice_head(n=12) %>%
		pivot_longer("2000":"2020",names_to="Year",values_to="BCM") %>%
		mutate(EJ=36*BCM/1000,
		       H2MT=(EJ*1e18)/120e6/1000/1e6,Uranium=EJ*277.78*1e12/EPerTonneU,
		       ElToMakeH2TWh=H2MT*H2ProcessEfficiency*1e6*ElPerTonneH2/1e12)

# 277.78 TWh per exajoule
	lngrest<-lngexportsbcm %>% filter(!grepl("Total",Country)) %>%
		rename(`2020`=`2020...22`) %>% 
		select(`Country`,`2000`:`2020`) %>%
		arrange(desc(`2020`)) %>% slice_head(n=12) %>%
		pivot_longer("2000":"2020",names_to="Year",values_to="BCM") %>% 
		mutate(EJ=36*BCM/1000,H2MT=(EJ*1e18)/120e6/1000/1e6,Uranium=EJ*277.78*1e12/EPerTonneU,
		       ElToMakeH2TWh=H2MT*H2ProcessEfficiency*1e6*ElPerTonneH2/1e12)
	res<-list(lngrest,lngtotals)
	res
}
makePlot<-function(thename,wh) {
      loc<-getAreaLoc(thename)
      #print(loc)
      if (loc[1]==0) { return; }
      lat<-loc[1]
      lon<-loc[2]
      n1<-(wh/arehEl)
      n2<-(wh/nynEl)
      n3<-(wh/thorconEl)
      rloc1<-makeRect(c(lon,lat),(wh/arehEl)*arehArea)
      rloc2<-makeRect(c(lon,lat),(wh/nynEl)*nynArea)
      rloc3<-makeRect(c(lon,lat),(wh/thorconEl)*thorconArea)
      leaflet() %>% 
        addTiles(options=tileOptions(opacity=1),group="Outline") %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        setView(lon,lat,zoom=7) %>% 
        addRectangles(rloc1[1],rloc1[2],rloc1[3],rloc1[4],fillOpacity=0.1,color="red",label=paste0(comma(n1)," Wind&Solar (e.g.,Asian Renewable Energy Hubs)")) %>%
        addRectangles(rloc2[1],rloc2[2],rloc2[3],rloc2[4],color="#008080",label=paste0(comma(n2)," Solar Farms (e.g., Nyngan)")) %>%
        addRectangles(rloc3[1],rloc3[2],rloc3[3],rloc3[4],color="yellow",label=paste0(comma(n3)," Thorcon nuclear isles")) %>%
        addMarkers(lng=lon,lat=lat,label=thename) %>%
        addLayersControl(
          baseGroups = c("Outline", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit="metre",primaryAreaUnit = "hectare") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
}
makeComparisonDescription<-function(wh,name,v) {
        markdown(
        paste0(startCard(),"1. If this ",comma(wh/1e12), " TWh of electricity came from a project like the Asian Renewable Energy Hub, you'd need ",
        comma(wh/arehEl), " of these, with a site area of ", 
        comma((wh/arehEl)*arehArea), " hectares (see red square below);  including ",
        comma((wh/arehEl)*arehCleared), " hectares cleared for ",comma((wh/arehEl)*(arehSPow/1e9)*v$tpGW),  " tonnes of panels. ",
        getArea((wh/arehEl)*arehArea,name),
        "</p>",pp(),"2. If it was supplied purely from solar power, you'd need ",
        comma(wh/nynEl), 
        " solar farms, like that in Nyngan in Australia, covering some ", 
        comma((wh/nynEl)*nynArea), " hectares (see blue square below). That's about ", comma(((wh/nynEl)*nynPow)/1e9), 
        " GW of solar panels. ",
        "\n</p>",pp(),"3. If the electricity came from several 1GW Thorcon nuclear isles, you'd need ",
        comma(wh/thorconEl), " of these, with a site area of ", 
        comma((wh/thorconEl)*thorconArea), " hectares (yellow square below; see Advanced Heat panel for details).",endCard()
        ))
}
makelink<-function(target,txt) {
    paste0('<a onclick="customHref(\'',target,'\')">',txt,'</a>')
}
endCard<-function() {
  '</p></div>'
}
startCard<-function() {
  "<div class=\"standout-container\"><p>"
}
pp<-function() {
  "<p class=\"standout-p\">"
}
makespace<-function(mm=6) {
  HTML(paste0('<div style="padding: ',comma(mm),'mm 0 0 0;"></div>'))

}
markdownFile<-function(filename) {
  #t<-read_file(pipe(paste0("date >>.m4.log && cat m4defs.txt ",filename," | m4 2>>.m4.log")))
  t<-read_file(pipe(paste0("cat m4defs.txt ",filename," | m4 ")))
  s<-str_replace_all(t,'\\[1\\] "(.*)"','\\1')
  markdown(s)
}
makeMapWithZoom<-function(lat,lon,z) {
      leaflet(options=leafletOptions(minZoom=z-2,maxZoom=z+1)) %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        addTiles(options=tileOptions(scrollWheelZoom=FALSE,opacity=1),group="Outline") %>% 
        setView(lon,lat,zoom=z) %>% 
        addLayersControl(
          baseGroups = c("Satellite", "Outline"),
          options = layersControlOptions( collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit="metres",primaryAreaUnit = "hectares") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
  
}
makeMapWithCircles<-function(thename,area,colr,initz,rfactor=1,offsetbearing=270) {
      lat=(locations %>% filter(name==thename))$lat
      lon=(locations %>% filter(name==thename))$lon
      r=sqrt(10000*area/pi)
      if (length(r)>1) {
          lastr=r[length(r)]
      }
      else {lastr=r}
      dp<-destPoint(c(lon,lat),d=rfactor*lastr,b=offsetbearing)
      z<-initz
      leaflet(options=leafletOptions(minZoom=z-4,maxZoom=z+4)) %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        addTiles(options=tileOptions(opacity=1),group="Outline") %>% 
        setView(dp[1],dp[2],zoom=z) %>% 
        addMarkers(lng=lon,lat=lat,label=thename) %>%
        addCircles(lng=dp[1],lat=dp[2],fillOpacity=0.1,color=colr,radius=r) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Outline"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit="metres",primaryAreaUnit = "hectares") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
}
makeMapWithOffsetCircle<-function(lat,lon,area,rfactor,offsetbearing,zoom=11) {
      r=sqrt(10000*area/pi)
      dp<-destPoint(c(lon,lat),d=rfactor*r,b=offsetbearing)
      z<-zoom
      leaflet(options=leafletOptions(minZoom=z-4,maxZoom=z+2)) %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        addTiles(options=tileOptions(opacity=1),group="Outline") %>% 
        setView(lon,lat,zoom=z) %>% 
        addCircles(lng=dp[1],lat=dp[2],fillOpacity=0.1,radius=r) %>%
        #addMarkers(lng=lon,lat=lat,label=thename) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Outline"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMeasure(primaryLengthUnit="metres",primaryAreaUnit = "hectares") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
}
makeRect<-function(loc,A) {
    # find the lon/lat of rect which has area A and height h with width 1.5h
    f<-1
    h<-sqrt(A/f)
    e<-destPoint(loc,b=90,d=(f/2.0)*100*h)
    ne<-destPoint(e,b=0,d=0.5*100*h)
    se<-destPoint(ne,b=180,d=100*h)
    sw<-destPoint(se,b=270,d=f*100*h)
    c(ne,sw)
}
getAreaLoc<-function(n) {
    if (n=="hectares") {
      return(c(0,0))
    }
    #print(n)
    lat<-(areas %>% filter(name==n))$lat
    lon<-(areas %>% filter(name==n))$lon
    c(lat,lon)
}
getArea<-function(thisha,n) {
    if (n=="hectares") {
      return("")
    }
    #print(thisha)
    #print(n)
    p<-"The site area is about "
    ha<-(areas %>% filter(name==n))$ha
    name<-(areas %>% filter(name==n))$name
    #print(ha)
    
    if (thisha>ha) {
            return(paste0(p,comma(thisha/ha)," times the area of ",name))
    }
    x<-as.numeric(comma2(thisha/ha))
    if ((thisha/ha)>=0.95) {
        return(paste0(p," the area of ",name, " (within 5%)"))
    }
    if ((thisha/ha)>=0.90) {
        return(paste0(p," the area of ",name, " (within 10%)"))
    }
    return(paste0(p,fractions(x)," times the area of ",name))
}



#----------------------------------------------------------------
# Comparison solar/wind+solar farms 
#----------------------------------------------------------------
nynEl<-233e9             # Nyngan, annual electricity
nynPow<-102e6             # Nyngan, annual electricity
nynArea<-250             # Nyngan, area 

arehSPow<-10.8e9           # AREH, 2021 figure for annual solar electricity (AREH=Asian Renewable Energy Hub Proposal)
#arehEl<-100e12           # AREH, 2021 figure for annual electricity (AREH=Asian Renewable Energy Hub Proposal)
arehEl<-90e12           # AREH, new BP figure 
arehElTWh<-arehEl/1e12   # AREH, 2021 figure for annual electricity (AREH=Asian Renewable Energy Hub Proposal)
arehArea<-662400         # AREH, site area ha
arehCleared<-20810       # AREH, area cleared ha

#-------------------------------------------------
#tonnesperGWPV<-(1e9/nynPow) * (1.36e6*12/1000)

#------------------------------------------------------------------
# Thorcon 1GW figures (area comes from https://aris.iaea.org/PDF/ThorCon_2020.pdf)
#------------------------------------------------------------------
thorconEl<-1000e6*24*365*0.9
thorconArea<-50          
globalSteel2019<-1869e6
steelPerGWThIsle<-128000
steelPerGWThLand<-35000

l<-getLNGdata()
#print(l)
lngex<-l[[1]]
lngtot<-l[[2]]

totalAREHs<-lngtot %>% filter(Country=="Total LNG exports"&Year=="2020") %>% summarise(narehs=sum(ElToMakeH2TWh/arehElTWh))
print(paste0("Total AREHS: ",totalAREHs))
totalUranium<-lngtot %>% filter(Country=="Total LNG exports"&Year=="2020") %>% summarise(tU=sum(Uranium))
print(paste0("Total U tonnes: ",totalUranium))

ui <- function(request) {
  fluidPage(
    useShinyjs(),
    tags$script(HTML('(function() { document.body.style.backgroundColor = "SmokeWhite"; })(); ')),
    chooseSliderSkin("Round",color="#008080"),
#  fluidRow(
#    column(10),
#    column(2,bookmarkButton())
#  ),
    theme=shinytheme("spacelab"),
    tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1600px; }")),
    tags$head(tags$script(src="js/index.js")),
    tags$head(tags$style(".teal {background-color: Teal ;}")),
    tags$head(tags$style(".cRow {text-align: center;}")),
    tags$head(tags$style("#splash {display: block; margin: auto; padding: 20px 0px 0px 0px; background-color: #008080;  }")),
    tags$head(tags$style("#splash p {padding: 0px 0px 20px 20px; font-weight: bold; color: white; font-size: 150%; }")),

#  tags$head(tags$style(".js-irs-22 .irs-handle {background: OrangeRed ;}")), # maxgigafactories
#  tags$head(tags$style(".js-irs-2 .irs-handle {background: OrangeRed ;}")), 
  tags$head(tags$style(".standout {font-size: 120%; font-weight: bold; background-color: Teal; color: white; }")), 
  tags$head(tags$style(".row-flex {font-size: 100%; display: flex; justify-content: space-between;}")), 
  tags$head(tags$style(".cc1 {width: 55%; color: Black; }")), 
  tags$head(tags$style(".cc2 {padding-right: 5px; width: 10%; text-align: right; color: white; }")), 
  tags$head(tags$style(".cc3 {width: 35%; color: LightSkyBlue; }")), 
  tags$head(tags$style(".standout-c {text-align: center; font-size: 130%; font-weight: bold; background-color: Teal; color: white; ;}")), 
  tags$head(tags$style(".standout-f {text-align: center; font-size: 70%; background-color: Teal; color: white; ;}")), 
  tags$head(tags$style(".standout-p {font-size: 110%; font-weight: bold; background-color: Teal; color: white; ; }")), 
  tags$head(tags$style(".plotly { margin-bottom: 30px; margin-top: 30px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); }")), 
  tags$head(tags$style(".leaflet { margin-bottom: 30px; margin-bottom: 30px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); }")), 

  tags$head(tags$style(".container-flex {display: flex;}")),
  tags$head(tags$style(".sp-flex {display: inline-flex;}")),
  tags$head(tags$style(".spacer {margin: 20px 0; height=30px}")),
  tags$head(tags$style(".bShadow {box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); }")), 
  tags$head(tags$style(".standout-container {margin: 20px 0; padding: 20px; font-weight: bold; background-color: Teal; color: white;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                       }")), 
  tags$head(tags$style("img.it {vertical-align: text-bot; padding-left: 10px; padding-bottom: 10px;}")), 
#  tags$head(tags$style("img {margin: 0px; border: 5px solid red !important; }")), 
  tags$head(tags$style(".col-sm-12 {padding: 0px !important; margin: 0px; }")), 
  tags$head(tags$style(".row {padding: 0px !important; margin: 0px; }")), 
  tags$head(tags$style(".cAlign {display: block; margin: 0 auto; background-color: Teal;}")),
  tags$head(tags$style(".cntr {display: block; margin: auto; border: 50 solid white; }")),
  #tags$style(type="text/css","body {padding-top: 70px; background-color: WhiteSmoke;}"),
  tags$style(type="text/css",".btn { padding: 5px ; }"),
  #shinythemes::themeSelector(),
  navbarPage(bookmarkButton(label=""),id="navpage",#header=tags$div("This is a footer"),
             #position="fixed-top",
             selected="intro",windowTitle="Stuffthatcounts",
    tags$head(tags$style("#totalelectricity2050 { text-align: center; color: white; font-weight: bold; background-color: Teal; }")),
    tags$head(tags$style("#steelfornuclear, #setIEAvalues, #lngtotals, #Hgasheat, #Hgaspeak, #lrownukemines, 
    #lrowlng,    #lrowsolarland2, #lrowsolarland, #lrowsolarmines, #lrowsolarmines2, 
    #lrowhydrogen, #lrowh, #lrowbattmines, #lrowb, #lrowsyn, #pvpaneltonnage, 
    #pvpaneltonnage2, #gigafactories, #ESynOil, #hydrogen,   #lrowfc, #lrowbw, #lrowsolarwaste,
    #gtotliioncap, #Ehydrogen  {color: Teal; font-size: 14px; } ")),
    tags$head(tags$style("#lrowhydrogen, #lrowbattmines, #lrowlng, #lrowsolarland2, #lrowsolarland, 
                          #lrowgpb2, #lrowgpb, #lrownukemines, #lrowb, #lrowwindvth, #lrowwindmine,#lrowwindland2,
                          #lrowsolarmines, #lrowsolarmines2, #lrowsyn, #lrowh {background-color: Gainsboro; } ")),
    tags$head(tags$style("#lrowwindvth,#lrowfc,#lrowbw,#lrowsolarwaste {background-color: Gainsboro; } ")),
    tags$head(tags$style("#gaspeak, #EHaHydrogenFleet, #EHaHydrogen, #HaSynOil {color: black; font-size: 14px; background-color: Gainsboro; } ")),
    
     navbarMenu("About",icon=icon("info-circle"),
      tabPanel("Introduction",value="intro",
          fluidRow(
            column(12,
            imageOutput("supplychains",height=200)
            )
          ),
          fluidRow(
            column(1),
            column(10,
#              tags$a("Got to howtodrive page", onclick="customHref('howtodrive')"),
              markdownFile("notthetech.txt"),
              leafletOutput("kamoto",height="300"),
              markdownFile("graphite.txt"),
              leafletOutput("graphite",height="300"),
              markdownFile("batteriesorh2.txt")
            )
          )
        ),
        tabPanel("IEA Targets",value="ieatargets",
               column(1),
#               sliderInput("ieanucleartwh",
#                    tippy("Nuclear ('000 TWh):",
#                      "IEA's 2050 target is to double nuclear from 2.7 to 5.4",
#                        placement="bottom"),
#                      min = 2,
#                      max = 71,
#                      step = 0.1,
#                      value = 2.7),
#               sliderInput("ieasolartwh",
#                    tippy("Solar ('000 TWh):",
#                      "IEA's 2050 target is to take solar from 0.8 to 24",
#                        placement="bottom"),
#                      min = 0,
#                      max = 35,
#                      value = 0.8),
#               sliderInput("ieawindtwh",
#                    tippy("Wind ('000 TWh):",
#                      "IEA's 2050 target is to take wind from 1.5 to 25",
#                        placement="bottom"),
#                      min = 1,
#                      max = 25,
#                      value = 1.5),
#               sliderInput("ieahydrotwh",
#                    tippy("Hydro ('000 TWh):",
#                      "IEA's 2050 target is to take hydro from 4.4 to 8.4",
#                        placement="bottom"),
#                      min = 4,
#                      max = 10,
#                      value = 4.4),
#               sliderInput("ieafftwh",
#                    tippy("Fossil fuels ('000 TWh):",
#                      "IEA's 2050 target is to take fossil fuels from 16.4 to 0.2 (with CCS)",
#                        placement="bottom"),
#                      min = 0,
#                      max = 20,
#                      value = 16.4),
#               fluidRow(
#                 div(style="display: block; text-align: center; margin-left: 0 auto; margin-right: 0 auto; ",
#                 actionButton("setcurrent",
#                                 tippy("Reset",
#                                       ""),
#                                 class="btn-success"),
#                 actionButton("setieagoals",
#                                 tippy("IEA goals",
#                                       "Set sliders to IEA 2050 targets"),
#                                 class="btn-success")
#                 )
#               )
#               ),
               column(10,
                      markdownFile("ieatargets.txt"),
                      plotlyOutput("ieatwhgoals"),
                      plotlyOutput("ieagwgoals")
                      #uiOutput("totalelectricity2050")
               )
        ),
        tabPanel("Growth rates",value="growthgeneral",
                 column(10, markdownFile("growth.txt"), ),
                 column(10, plotlyOutput("hydroplot2") ),
                 column(10, markdownFile("growth1.txt") ),
                 column(10, plotlyOutput("growthplot") ),
                 column(10, markdownFile("growth2.txt") ),
                 column(10, plotlyOutput("growthplot2") ),
                 column(10, markdownFile("growth3.txt") ),
                 column(10, imageOutput("growthtable",height=500) ),
                 column(10, markdownFile("growth4.txt") ),
                 column(10, imageOutput("growthtable2",height=500) ),
                 column(10, markdownFile("growth5.txt") )
                 #column(10, plotlyOutput("growthtable3",height=500) )
        ),
        tabPanel("Settings and model parameters",value="settings",
                 column(10,
                  markdownFile("settings.txt")
                 ),
                 column(10,
                        markdownFile("areaunits.txt")
                 ),
                 column(5, selectInput("masterunits","Area units:",areas$name)  ),
                 column(10,
                        markdown("
#### Tonnes of panels per GW of solar PV

PV panels are constructed of valuable mined materials of various kinds; including
quartz, aluminium, silver, tin and copper, to name just a few. The breakdown of
materials varies between different technologies and even within technologies. Tonnes
per GW of commonly used panels can vary between 70,000 and 160,000. Newer technologies
tend to be lighter than older technologies. 

Here is a plot of several current and a couple of older model panels. The **First Solar** panels in the plot are 
commonly used in
solar farms where robust construction takes precedence over price. The **Longi** panels are included because this
Chinese company is currently the largest producer of panels. You can see the weight reductions over a decade in the 
three First Solar models. Will this continue, or will it be like phones? Phones got **very** small and then 
got bigger again. The trade off is in robustness and longevity vs price and weight. 

                                 "),
                        plotlyOutput("pvtonnagedata")
                 ),
                 column(5, 
                    sliderInput("tonnesperGWPV1",
                    tippy("Tonnes of PV panels per gigawatt (GW):",
                      "See text for details",
                        placement="bottom"),
                      min = 30000,
                      step=1000,
                      max = 120000,
                      value = tonnesperGWPV),
                 ),
#---------------------------- BLOCK
                 column(10,
                        markdown("
                        
#### Hectares per gigawatt of solar 

On Sep 2nd 2021 I downloaded and processed a table of solar farm
data from [Wikipedia](https://en.wikipedia.org/wiki/List_of_photovoltaic_power_stations). 
Of the 34 solar farms who provided data on both area and power, the median hectares per gigawatt (GW) was 2562.
                                 ")
                 ),
                 column(5, 
                    sliderInput("haPerGW",
                    tippy("Hectares per gigawatt (GW):",
                      "See text for details",
                        placement="bottom"),
                      min = 1000,
                      max = 10000,
                      value = 2562),
                 ),
#---------------------------- BLOCK
                 column(10,
                        markdown("
                        
#### PV capacity factor 

This isn't used in many places in the tool. But it is used in waste calculations to estimate
the number of nuclear reactors required to produce the same amount of electricity annually as
a given amount of PV.


                                 ")
                 ),
                 column(5, 
                    sliderInput("pvCapacityFactor",
                    tippy("PV capacity factor:",
                      "See text for details",
                        placement="bottom"),
                      min = 5,
                      max = 30,
                      value = 20),
                 ),
#------------------------------------
                 column(10,
                        markdown("
                        
#### Tonnes of copper per GW of PV

Copper per gigawatt of PV comes from study by [Navigent Research](https://www.copper.org/publications/pub_list/pdf/a6198-na-wind-energy-analysis.pdf)
Copper for offshore wind farms may be about double this; because of the cabling.

                                 ")
                 ),
                 column(5, 
                    sliderInput("cuPerGW",
                    tippy("Tonnes of copper per gigawatt PV:",
                      "See text for details",
                        placement="bottom"),
                      min = 1000,
                      max = 20000,
                      value = 5000),
                 ),
                 column(10,
                        markdown("
                        
#### Tonnes of copper per GW of Wind

Copper per gigawatt of Wind comes from study by [Navigent Research](https://www.copper.org/publications/pub_list/pdf/a6198-na-wind-energy-analysis.pdf)
Copper for offshore wind farms may be about double this; because of the cabling.

                                 ")
                 ),
                 column(5, 
                    sliderInput("cuPerGWWind",
                    tippy("Tonnes of copper per gigawatt of Wind:",
                      "See text for details",
                        placement="bottom"),
                      min = 1000,
                      max = 12000,
                      value = 4300),
                 ),
#----------------------------------------------
                 column(10,
                        markdownFile("referencesolar.txt")
                 ),
                 column(5, selectInput("mastersolarfarm","Solar farm reference:",solarfarms$name)  ),
                 fluidRow(
                    column(1),
                    column(10,
                         markdown("
#### Parameters                        
 
Almost all the parameters are adjustable; by you. But some data just uses
known values. For example we know the value of solar PV installed currently installed as of 2020, so 
we use it. The recycling capacity of panels and batteries doesn't seem to be reliably known, so I've made
a conservative guess (meaning quite large). If I'd have guessed small, then it would take many years, even
at compound growth rates, so get to a reasonable size.

                                  "),
                         uiOutput("modelassumptions"),
                    )
                 )
        ),
        tabPanel("How to drive this tool",value="howtodrive",
                 fluidRow(
                    column(1),
                    column(10,
                      markdownFile("howto.txt"),
                      sliderInput("sample1","Electric vehicle percentage",min=0,max=100,value=50),
                      markdownFile("howto2.txt"),
                    )
                 )
        ),
        tabPanel("References",value="refs",
                 fluidRow(
                    column(1),
                    column(10,
                      markdownFile("references.txt")
                    )
                 )
        ),
        tabPanel("Credits",value="Credits",
             tags$div(id="iconattr",p("Most icons are Font-Awesome, additional icons made by",
                  tags$span(id="creditspan",
                            tags$img(class="it",src="nuclear-plant.png",width="30px"),
                            tags$a(href="https://www.flaticon.com/authors/good-ware","Good Ware"), 
                            tags$a(href="https://www.flaticon.com/authors/good-ware","www.flaticon.com")
                            ))),
              markdownFile("credits.txt")
        ),
        tabPanel("The Author",value="theauthor",
                 fluidRow(
                    column(1),
                    column(10,
                      markdownFile("theauthor.txt")
                    )
                 )
        )
    ),
    tabPanel("Transport",value="transport",icon=icon("bus"),
      tabsetPanel(id="BEVtabset",
      tabPanel("Electric Vehicles",value="BEVS",
      fluidRow(
        column(12,imageOutput("gfimage",height=200))
      ),
      fluidRow(
        column(3),
        column(9,
            markdownFile("gigafactorycaption.txt"),
            markdownFile("transport.txt"),
            imageOutput("icctimage",height=300),
            markdownFile("transport2.txt")
        )
      ),
      fluidRow(
        column(3,id="lrowb",
        sliderInput("globalvsale",
            tippy("Global annual light vehicle sales (in millions):",
                  "In 2018 global light vehicle sales hit 97 million vehicles, but were down to 78 million in 2020. Will they rebound and will the market grow as more Chinese and others in developing countries have more buying power?"),
            min = 20,
            max = 150,
            value = 70),
        sliderInput("evreserved",
            tippy("Percentage of Battery capacity reserved for EVs:",
                  "If batteries are used for grid storage, there will be less battery capacity available for EV use. How should capacity be allocated?",
                  placement="bottom"),
            min = 1,
            max = 100,
            value = 85),
        sliderInput("evpercent1",
            tippy("EV percentage, assume rest are Hydrogen",
                  "Currently we have only two clean vehicle technologies available, hydrogen fuel cells and EVs, how will battery capacity requirements change with the fraction of EVs in the mix?"),
            min = 0,
            max = 100,
            value = 60),
        sliderInput("batterysize1",
            tippy("Average size of EV battery:",
                  "A small EV might run with a 50 kwh battery, while most Teslas are coming with something like 100 kwh. How do choices influence capacity requirements?"
            ),
            min = 40,
            max = 110,
            value = 86)
        ),
        column(9,
                markdownFile("gigafactories.txt"),
                uiOutput("gigafactories"),
                plotlyOutput("ngf",height=130),
                
                markdown("
                
If you adjust the sliders to request 100m EVs each year (no hydrogen) with 70kwh batteries with claims to 
100% of factory output, you'll see we need 200 dedicated gigafactories to produce them. This is what the IEA
[Net Zero by 2050](https://www.iea.org/reports/net-zero-by-2050) report called for. Who's going to 
build these? And who's going to build the additional gigafactories for the batteries people want on
grids to backup renewables? And in their homes?
"),
               markdown("
               
#### Assume remaining vehicles EVs running on hydrogen (fuel cells)

You can set a slider to choose the percentage of EVs and just assume the rest of
the vehicles will be something else; hydrogen being the only other technology which
seems plausible. How much hydrogen will be required if half of all new vehicles 
are to be hydrogen? With so few vehicles on the market, that's a hard question
to answer. But the IEA has calculated how much hydrogen is required in fuel cells
to replace petrol in current internal combustion engines. You can find this
information on the Fuels -> Hydrogen page.

               "),
               markdownFile("lithium2023.txt"),
               plotlyOutput("liioncap",height=180),
               uiOutput("gtotliioncap"),
               markdownFile("globalcarsales2020.txt"),
               plotlyOutput("sales2020",height=230)
          )
       )
      ),
      tabPanel("Hydrogen Vehicles",value="fuelcells",
        fluidRow(
          column(12,imageOutput("fctruckimage",height=200))
        ),
        fluidRow(
            column(3),
            column(9,
                 markdownFile("fuelcell.txt")
            )
        ),
        fluidRow(
            column(3,id="lrowfc",
              sliderInput("fcpercent",
            tippy("Hydrogen fuel cell percentage, assume rest are EV",
                  "Currently we have only two clean vehicle technologies available, hydrogen fuel cells and EVs, how will battery capacity requirements change with the fraction of EVs in the mix?"),
            min = 0,
            max = 100,
            value = 40)
            ),
            column(9, # XXX
                 uiOutput("hydrogen"),
                 plotlyOutput("hydrogenfleet"),
                 uiOutput("EHaHydrogenFleet"),
                 makespace(),
                 leafletOutput("LHaHydrogenFleet",height="600")
            )
        )
      )
      )
    ),
    tabPanel("Fuels",value="fuelspanel",icon=icon("burn"),
        tabsetPanel(id="fuels",
        tabPanel("Hydrogen",value="hydrogen",icon=icon("burn"),
                fluidRow(
                    column(12,imageOutput("watchimage",height=200))
                ),
                fluidRow(
                     column(3,id="lrowhydrogen"),
                     column(9,
                        markdown("Watch this space; beautiful images abound, but, as of 2021, all renewable powered 
hydrogen production facilities are tiny and there are no large shipping super tankers for transporting hydrogen. 
                                 "),
                        markdownFile("hydrogennow.txt")
                     )
                 ),
                 fluidRow(
                     column(3,id="lrowh",
                        sliderInput("hydrogenReq",
                        "In 2019, global hydrogen production was 70 million tonnes",
                        min = 1, max = 2000,value = 35),
                        selectInput("haunits","Select a unit to compare with:",
                                    areas$name) 
                     ),
                     column(9,
                        markdownFile("hydrogennow2.txt"),
                        uiOutput("Ehydrogen"),
                        plotlyOutput("PlEhydrogenCmp"),
                        markdownFile("habitatmaterials.txt"),
                        uiOutput("EHaHydrogen"),
                        makespace(),
                        leafletOutput("LHaHydrogen",height="600")
                     )
                 )
        ),
        tabPanel("Synthetic fuel",value="syntheticfuel",icon=icon("gas-pump"),
                 fluidRow(
                     column(3,id="lrowsyn",
                        sliderInput("synOil",
                        "Percentage of current oil to be replaced by synthetic hydrocarbons; made with electricity:",
                        min = 0, max = 100,value = 1),
                        selectInput("synunits","Select a unit to compare with:",
                                    areas$name) 
                     ),
                     column(9,
                        markdownFile("synfuel.txt"),
                        uiOutput("ESynOil"),
                        markdownFile("synfuelland.txt"),
                        uiOutput("HaSynOil"),
                        makespace(),
                        leafletOutput("LHaSynOil",height="600")
                     )
                 )
        ),
        tabPanel("Gas",value="gaspeaking",icon=icon("burn"),
                 fluidRow(
                   column(12,
                        imageOutput("gaspipe",height=200)
                   ),
                   column(3,id="lrowgpb"),
                   column(9,
                        markdownFile("naturalgas.txt")
                   )
                 ),
                 fluidRow(
                     column(3,id="lrowgpb2",
                        sliderInput("gaspeaking",
                        "Percentage of current gas PEAKING power replaced by hydrogen; made with electrolysis:",
                        min = 0, step=1, max = 100,value = 1),
                        sliderInput("gasheating",
                        "Percentage of current gas HEATING to be replaced by hydrogen; made with electrolysis:",
                        min = 0, step=1, max = 50,value = 1),
                        selectInput("gasunits","Select a unit to compare with:",
                                    areas$name) 
                     ),
                     column(9,
                        markdown("
Use the slider to specify how much of our natural gas peaking you want to replace with hydrogen. Ideally, this would be
100 percent. Alternative, you can add carbon capture to the plants where feasible. Or again, dump gas and use a reliable
energy source instead of wind and solar; either hydro, nuclear, or biofuels.
                                 "),    
                        uiOutput("Hgaspeak"),
                        markdownFile("naturalgasheating.txt"),
                        uiOutput("Hgasheat"),
                        plotlyOutput("Plgaspeak"),
                        markdown("
#### Land use 
                                 
How much land are we talking about?                                 
                                 
                                 "),
                        uiOutput("gaspeak"),
                        makespace(),
                        leafletOutput("Lgaspeakheat")
                     )
                 )
        ),
        tabPanel("LNG (Case study)",value="caselng",icon=icon("burn"),
               fluidRow(
                   column(12,
                        imageOutput("gastanker",height=200)
                   ),
                  column(3,id="lrowlng"),
                  column(9,
                     markdownFile("casestudylng.txt"),
#                     plotlyOutput("Pllng"),
                     plotlyOutput("Pllng2"),
                     markdownFile("casestudylng2.txt"),
                     plotlyOutput("PllngU"),
                     uiOutput("lngtotals"),
                     markdownFile("casestudylng3.txt")
                  )
               )
        ),
        tabPanel("Steel",value="steel",icon=icon("industry"),
               fluidRow(
                   column(12,
                        imageOutput("steelimage",height=200)
                   )
               ),
               fluidRow(
                   column(3),
                   column(9,
                          markdownFile("steelmaking.txt")
                   )
               )
        )
        )
        ),
        tabPanel("Heat",value="heatandhyd",icon=icon("fire"),
            fluidRow(
                    column(1,id="adheat"
                    ),
                    column(10,
                           markdownFile("heatandh2.txt")
                    )
                 )
        ),
        tabPanel("Electricity",value="electricity",icon=icon("plug"),
            tabsetPanel(id="etabs",selected=NULL,
              tabPanel("Land",value="landuse",icon=icon("map"),
                tabsetPanel(id="landusetabs",selected=NULL,
                  tabPanel("General",value="landgeneral",icon=icon("info"),
                           fluidRow(
                             column(1),
                             column(10,
                                    markdownFile("landgeneral.txt"),
                                    plotlyOutput("landuseaus"),
                                    markdownFile("landgeneral2.txt"),
                                    plotlyOutput("landuseaus2"),
                                    markdownFile("landgeneral3.txt")
                             )
                           )
                  ),
                  tabPanel("Utility scale solar",value="landsolar",icon=icon("solar-panel"),
                    fluidRow(
                      column(12, imageOutput("solarhills",height=220) )
                    ),
                    fluidRow(
                      column(3),
                      column(9,
                          markdownFile("landforsolar.txt")
                      )
                    ),
                    fluidRow(
                        column(3,id="lrowsolarland", selectInput("lmapname1","Select a familar place:", locations$name)),
                        column(9,
                          leafletOutput("leafletmap1",height="500")
                        )
                    ),
                    fluidRow(
                     column(3),
                     column(9,
                       markdown("
#### Land use for solar PV in 2050?
                                ")
                       )
                    ),
                    fluidRow(
                       column(3,id="lrowsolarland2",
                        sliderInput("pvRateIncrease1",
                        tippy("PV production growth rate to 2050:",
                          "There were 737 GW of solar PV installed in 2020",
                            placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 20,
                              value = 10),
                        sliderInput("tonnesperGWPV2",
                            tippy("Tonnes of PV panels per gigawatt (GW):",
                          "See text for details",
                              placement="bottom"),
                              min = 30000,
                              step=1000,
                              max = 120000,
                              value = tonnesperGWPV),
                        sliderInput("mtbfPV2",
                            tippy("PV panel lifespan:",
                              "See text for details",
                                placement="bottom"),
                              min = 20,
                              max = 40,
                              value = 25),
                        sliderInput("pvrecyclegrowthrate2",
                            tippy("PV recycling capacity growth rate:",
                              "Percentage GW/yr",
                                placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 25,
                              value = 5),
                        sliderInput("bkuphours2",
                            tippy("Backup battery time (hours):",
                              "See text for details",
                              placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 15,
                              value = bkuphours)
                        ),
                        column(9,
                             markdownFile("landforsolar2.txt"),
                             uiOutput("pvpanelland"),
                             makespace(),
                             markdown("
#### Getting a feel for big areas

The **Implications** panel had a number for the amount of land covered by panels in 2050. Do you think
in hectares? Did it mean anything. Here is a map of the area relative to a convenient land mass so that
you can understand it better. Go to the top of the page and select an area that is familiar.
                                      "),
                             leafletOutput("leafletmapSolarArea",height="500")
                        )
                    )
                ), # Land and mining for renewables
                tabPanel("Wind",value="landwind",icon=icon("wind"),
                    fluidRow(
#                       column(3,id="lrowwindland2",
#                        sliderInput("windRateIncrease1",
#                        tippy("Wind power production growth rate to 2050:",
#                          "There was 737 GW of wind power installed in 2020",
#                            placement="bottom"),
#                              min = 0,
#                              step=0.1,
#                              max = 25,
#                              value = 10),
#                        sliderInput("tonnesPerGWWind1",
#                            tippy("Wind turbines tonnes per GW:",
#                          "See text for details",
#                              placement="bottom"),
#                              min = 30000,
#                              max = 1e6,
#                              value = 180000),
#                        sliderInput("mtbfwind1",
#                            tippy("Wind turbine lifespan:",
#                              "See text for details",
#                                placement="bottom"),
#                              min = 20,
#                              max = 40,
#                              value = 25),
#                        sliderInput("windrecyclegrowthrate1",
#                            tippy("Wind turbine recycling capacity growth rate:",
#                              "Percentage GW/yr",
#                                placement="bottom"),
#                              min = 0,
#                              step=0.1,
#                              max = 100,
#                              value = 5),
#                        sliderInput("windbkuphours1",
#                            tippy("Backup battery time (hours):",
#                              "See text for details",
#                              placement="bottom"),
#                              min = 0,
#                              step=0.1,
#                              max = 15,
#                              value = 2)
#                        ),
                        column(1),
                        column(10,
                             markdownFile("landforwind.txt")
                        )
                    )
                ),
                tabPanel("Hydro",value="landhydro",icon=icon("water"),
                    fluidRow(
                      column(12,
                             imageOutput("threegorges",height=200),
                             markdown("China's Three Gorges Dam ")                      
                      )
                     ),
                     fluidRow(
                        column(3),
                        column(9,
                              markdownFile("landforhydro.txt"),
                              plotlyOutput("hydroplot"),
                              markdownFile("landforhydro1.txt")
                        )
                     )
                ),
                tabPanel("Nuclear",value="landnuclear",icon=icon("atom"),
                    fluidRow(
                      column(12, imageOutput("leibstadt",height=200) ),
                      column(12, markdown("The Leibstadt nuclear plant in Switzerland"))
                    ),
                    fluidRow(
                      column(3,id="lrownukeland",
                       selectInput("lmapname2","Select a familar place:", locations$name) 
                      ),
                      column(9,
# https://www.firstsolar.com/-/media/First-Solar/Project-Documents/PD-5-401-03_Series3Black-4_NA.ashx
# Nyngan uses Series 3 Black modules 12 kg each 
                        markdownFile("landfornuclear.txt"),
                        leafletOutput("leafletmap2",height="600"),
                        markdownFile("fukushima.txt"),
                        leafletOutput("fukumap",height="600")
                      )
                   )
                ),
                tabPanel("Biomass",value="landbiomass",icon=icon("tree"),
                   column(1),
                   column(10,
                    markdownFile("landforbiomass.txt"),
                    leafletOutput('biomassmap',height="600")
                    )
                )
              )
            ), # Land Menu
            tabPanel("Mining",value="miningpanel",icon=icon("fill"),
              tabsetPanel(id="miningtabset",type="tabs",
                tabPanel("General",value="mininggeneral",icon=icon("info"),
                         fluidRow(column(1),column(10,markdownFile("mininggeneral.txt")))),
                tabPanel("Solar",value="miningrenewables",icon=icon("solar-panel"),
                fluidRow(
                    column(12,imageOutput("polyimage",height=200))
                ),
                fluidRow(
                    column(3,id="lrowsolarmines"),
                    column(9,
                         markdownFile("mineralsforsolar.txt")
                    )
                ),
                fluidRow(
                    column(3,id="lrowsolarmines2",
                        sliderInput("pvRateIncrease2",
                        tippy("PV production growth rate to 2050:",
                          "There were 737 GW of solar PV installed in 2020",
                            placement="bottom"),
                          min = 0,
                          step=0.1,
                          max = 20,
                          value = 10),
                    sliderInput("tonnesperGWPV3",
                        tippy("Tonnes of PV panels per gigawatt (GW):",
                          "See text for details",
                          placement="bottom"),
                          min = 30000,
                          step=1000,
                          max = 120000,
                          value = tonnesperGWPV),
                    sliderInput("mtbfPV1",
                        tippy("Average PV panel lifespan:",
                          "See text for details",
                            placement="bottom"),
                          min = 20,
                          max = 40,
                          step=1,
                          value = 25),
                    sliderInput("pvrecyclegrowthrate1",
                        tippy("Average PV recycle growth rate:",
                          "Percentage annual growth in recycling capacity (GW)",
                            placement="bottom"),
                          min = 0,
                          step=0.1,
                          max = 25,
                          value = 5),
                    sliderInput("bkuphours1",
                        tippy("Backup battery time (hours):",
                          "See text for details",
                          placement="bottom"),
                          min = 0,
                          step=0.1,
                          max = 15,
                          value = bkuphours)
                    ),
                    column(9,
                         markdownFile("mineralsforsolar1.txt"),
                         uiOutput("pvpaneltonnage"),
                         markdown("
#### Detailed breakdown

The graph below shows the detail. You can zoom in to see details of the situation in 2050.  
(Click and drag over a rectangle containing the years between 2045 and 2050, for example)

"),
                         plotlyOutput("pvTonnes"),
                         plotlyOutput("pvGW"),
                         markdownFile("mineralsforsolar2.txt")
                    )
                )
            ), 
            tabPanel("Batteries",value="miningbatteries",icon=icon("battery-full"),
              fluidRow(
                column(12,
                       imageOutput("graphiteminer",height=250),
                       markdown("
Chinese graphite miner, 2017 (Alamy)                       
                                ")
                )
              ),
              fluidRow(
                column(3),
                column(9,
                     markdownFile("miningforbatteries.txt"),
                     uiOutput("boiledwater"),
                     makespace(),
                     markdownFile("miningforbatteries-a.txt")
                )
              ),
              fluidRow(
                column(3,id="lrowbattmines",
                    sliderInput("batterygrowthpercent1",
                    tippy("Battery production growth rate",
                    "Annual percentage increase in battery production capacity",
                        placement="bottom"),
                      step = 0.1,
                      min = 0,
                      max = 25,
                      value = batteryGrowthPercent),
                    sliderInput("batteryenergydensity1",
                    tippy("Energy Density (Wh/kg):",
                      "Current Tesla Model 3 batteries have a density of about 150 Wh/kg",
                        placement="bottom"),
                      min = 1,
                      max = 500,
                      value = 150),
                    sliderInput("batterylifespan1",
                    tippy("Average battery lifespan (years):",
                      "Very contentious number",
                        placement="bottom"),
                      min = 1,
                      max = 50,
                      value = 15),
                    sliderInput("batteryrecyclegrowthrate1",
                    tippy("Recycling growth rate:",
                      "specify the percentage growth rate of recycling",
                        placement="bottom"),
                      min = 0,
                      step = 0.1,
                      max = 25,
                      value = 5),
#                    actionButton("setIEAvalues",
#                                 tippy("Set IEA assumptions",
#                                       "IEA target 200 gigafactories, but I've had to guess at their assumed level of EVs"),
#                                 class="btn-success cAlign")
                    sliderInput("batterysize2",
                        tippy("Average size of EV battery:",
                              "A small EV might run with a 50 kwh battery, while most Teslas are coming with something like 100 kwh. How do choices influence capacity requirements?"
                        ),
                        min = 40,
                        max = 110,
                        value = 86)
                  ),
                  column(9,  
                     markdownFile("miningforbatteries1.txt"),
                     uiOutput("batterymining"),
                     makespace(),
                     markdownFile("miningforbatteries2.txt"),
                     plotlyOutput("plbatteriestonnage"),
                     markdownFile("miningforbatteries1a.txt"),
                     plotlyOutput("plbatteries"),
                     markdownFile("mineralsforsolar0.txt"),
                     imageOutput("wbminerals",height=300),
                     markdownFile("miningforbatteries3.txt")
                   )
              )
          ),
          tabPanel("Wind",value="miningwind",icon=icon("wind"),
              fluidRow(
                column(12,imageOutput("windimage",height=200))
              ),
              fluidRow(
                  column(3,id="lrowwindmine",
                        sliderInput("windRateIncrease2",
                        tippy("Wind turbine production growth rate to 2050:",
                          "There was 737 GW of wind power installed by 2020",
                            placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 20,
                              value = 10),
                        sliderInput("tonnesPerGWWind2",
                            tippy("Wind turbines tonnes per GW:",
                          "See text for details",
                              placement="bottom"),
                              min = 30000,
                              max = 360000,
                              value = 180000),
                        sliderInput("mtbfwind2",
                            tippy("Wind turbine lifespan:",
                              "See text for details",
                                placement="bottom"),
                              min = 20,
                              max = 40,
                              value = 25),
                        sliderInput("windrecyclegrowthrate2",
                            tippy("Wind turbine recycling capacity growth rate:",
                              "Percentage GW/yr",
                                placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 100,
                              value = 5),
                        sliderInput("windbkuphours2",
                            tippy("Backup battery time (hours):",
                              "See text for details",
                              placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 15,
                              value = bkuphours)
                  ),
                  column(9,
                             markdownFile("miningforwind3.txt"),
                             uiOutput("windmines")
                  )
              )
          ),
          tabPanel("Hydro",value="mininghydro",icon=icon("water"),
                    fluidRow(
                      column(12,
                             imageOutput("threegorges2",height=200),
                             markdown("China's Three Gorges Dam ")                      
                      )
                     ),
                   fluidRow(
                     column(2),
                     column(9,
                            markdownFile("mininghydro.txt")
                     )
                   )
          ),
          tabPanel("Nuclear",value="miningnuclear",icon=icon("atom"),
            fluidRow(
              column(12,imageOutput("rosatomimage",height=200)),
              column(12,markdown("
Connect the 600 tonne nuclear reactor pressure vessel in this image to a suitable turbine and generator and fuel it with 200 tonnes per year of
uranium fuel and you'll get more electricity per year than 600 x 5 megawatt (900 tonnes of steel each, 540,000 tonnes of steel total) wind turbines.
                                 "))
            ),
            fluidRow(
              column(3),
              column(9,
                markdownFile("miningfornuclear.txt")
              )
            ),
            fluidRow(
              column(3,id="lrownukemines",
               sliderInput("nucleartwh",
                    tippy("Nuclear ('000 TWh):",
                      "IEA's 2050 target is to double nuclear from 2.7 to 5.4",
                        placement="bottom"),
                      min = 2,
                      max = 71,
                      step=0.1,
                      value = 2.7),
              ),
              column(9,
# https://www.firstsolar.com/-/media/First-Solar/Project-Documents/PD-5-401-03_Series3Black-4_NA.ashx
# Nyngan uses Series 3 Black modules 12 kg each 
                markdownFile("miningfornuclear1.txt"),
                uiOutput("steelfornuclear"),
                makespace(),
                uiOutput("steelanduraniumfornuclear"),
#                plotlyOutput("plsteelfornuclear"),
#                plotlyOutput("pluraniumfornuclear")
              )
           )
         ),
         tabPanel("Wind V Nuclear",value="comparethwind",icon=icon("trophy"),
              fluidRow(
                column(12,imageOutput("steelimage2",height=200))
              ),
              fluidRow(
                column(3,id="lrowwindvth",
                sliderInput("twhwindvthorcon",
                    tippy("Thousand terawatt hours per year:",
                      "IEA's 2050 target 25,000 TWh (currently it's about 1500)",
                        placement="bottom"),
                      min = 1,
                      max = 25,
                      value = 2),
                sliderInput("windcapfactor",
                    tippy("Wind capacity factor:",
                      "Typically somewhere between 0.3 and 0.5",
                        placement="bottom"),
                      min = 0.2,
                      step=0.01,
                      max = 0.7,
                      value = 0.33)
                ),
                column(9,
                  markdownFile("miningforwind.txt"),
                  uiOutput("steelwindvthorcon"),
                  makespace(),
                  markdownFile("miningforwind2.txt")
                )
              )
          )
        )
        ),
        tabPanel("Waste",value="wastetab",icon=icon("trash"),
          tabsetPanel(id="wastetabs",
            tabPanel("General",value="wastestreamsgeneral",icon=icon("info"),
                 fluidRow(
                    column(1), 
                    column(10, 
                       markdownFile("wastestreams.txt")
                     )
                  )
            ),
            tabPanel("Solar",value="wastestreamssolar",icon=icon("solar-panel"),
                 column(12,imageOutput("solarwasteimage",height=200)),
                 column(12,"   Smashed panels in the Virgin Islands (Alamy). Damage like this is over and above the failure model used by this website,"),
                 
                 fluidRow(
                    column(3), 
                    column(9, 
                        markdownFile("wastestreamssolar.txt")
                    )
                 ),
                 fluidRow(
                 column(3,id="lrowsolarwaste", # WWWWWWWW
                    sliderInput("pvRateIncrease3",
                      tippy("PV production growth rate to 2050:",
                      "There were 737 GW of solar PV installed in 2020",
                        placement="bottom"),
                          min = 0,
                          step = 0.1,
                          max = 20,
                          value = 10),
                     sliderInput("tonnesperGWPV4",
                        tippy("Tonnes of PV panels per gigawatt (GW):",
                        "See text for details",
                        placement="bottom"),
                        min = 30000,
                        step=1000,
                        max = 120000,
                        value = tonnesperGWPV),
                    sliderInput("mtbfPV3",
                        tippy("Average PV panel lifespan:",
                          "See text for details",
                            placement="bottom"),
                          min = 20,
                          max = 40,
                          step=1,
                          value = 25),
                    sliderInput("pvRecycleGWnow", # XXX
                         tippy("PV recycling capacity (GW) 2021:",
                         "GW/yr",
                         placement="bottom"),
                         min = 0,
                         step=20,
                         max = 500,
                         value = 10),
                    sliderInput("pvrecyclegrowthrate3",
                         tippy("PV recycling capacity growth rate:",
                         "Percentage GW/yr",
                         placement="bottom"),
                         min = 0,
                         step=0.1,
                         max = 25,
                         value = 5)
                 ),
                 column(9, 
                    markdown("#### Accumulating waste 
                    
The tonnage of material to be recycled or disposed of grows rapidly. With a solar farm, the material is relatively
concentrated compared to rooftop PV where collection costs will be much higher. [As of 2020](https://www.iea.org/reports/renewables-2020/solar-pv), household PV is only about
20 percent of the global PV market. 

                    "),
                    uiOutput("pvwastetonnesspec"),
                    plotlyOutput("pvwastetonnes"),
                    markdownFile("wastestreamssolar2.txt")
                 )
              )
           ),
           tabPanel("Battery",value="wastestreamsbatteries",icon=icon("battery-full"),
                     column(12,imageOutput("bwasteimage",height=200)),
                     fluidRow(column(12,"Battery recycling has always been intrinsically hard to regulate at global scale, lead poisoning of children continues to be a catastophe in developing countries")),
                     fluidRow(
                       column(3),
                       column(9,
                          markdownFile("batterywaste0.txt")
                       )
                     ),
                     fluidRow(
                        column(3,id="lrowbw", # BBB2
                          sliderInput("batterygrowthpercent2",
                          tippy("Battery production growth rate",
                          "Annual percentage increase in battery production capacity",
                              placement="bottom"),
                            step = 0.1,
                            min = 0,
                            max = 25,
                            value = batteryGrowthPercent),
                          sliderInput("batteryenergydensity2",
                          tippy("Energy Density (Wh/kg):",
                            "Current Tesla Model 3 batteries have a density of about 150 Wh/kg",
                              placement="bottom"),
                            min = 1,
                            max = 500,
                            value = 150),
                          sliderInput("batterylifespan2",
                          tippy("Average battery lifespan (years):",
                            "Very contentious number",
                              placement="bottom"),
                            min = 1,
                            max = 50,
                            value = 15),
                          sliderInput("batteryrecyclegrowthrate2",
                          tippy("Recycling growth rate:",
                            "specify the percentage growth rate of recycling",
                              placement="bottom"),
                            min = 0,
                            step = 0.1,
                            max = 25,
                            value = 5)
                        ),
                        column(9,
                             plotlyOutput("plbatteriestonnage2")
                        )
                     )
           ),
           tabPanel("Wind power",value="wastestreamswind",icon=icon("wind"),
                 column(12,imageOutput("windfarmlightning",height=200)),
                 fluidRow(
                   column(1),
                   column(10, markdownFile("wastestreamswind.txt") ) 
                 )
           ),
           tabPanel("Nuclear",value="wastestreamsnuke",icon=icon("radiation"),
              column(12,imageOutput("drycasks",height=200)),
              fluidRow(
                   column(3),
                   column(9, 
                    markdownFile("wastestreamsnuclear.txt"),
                    markdownFile("wastestreamex.txt")
                  )
              ),
              fluidRow(
                   column(3,id="lrownukewaste",
                        sliderInput("pvRateIncrease4",
                        tippy("PV production growth rate to 2050:",
                          "There were 737 GW of solar PV installed in 2020",
                            placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 20,
                              value = 10),
                        sliderInput("bkuphours3",
                            tippy("Backup battery time (hours):",
                              "See text for details",
                              placement="bottom"),
                              min = 0,
                              step=0.1,
                              max = 15,
                              value = bkuphours)
                   ),
                   column(9,
                    uiOutput("nGWexp"),
                    plotlyOutput("nGW"),
                    markdownFile("wastestreamsex2.txt"),
                    plotlyOutput("wnGW"),
                    markdownFile("wastestreamsnuclear2.txt")
                   )
              )
           ),
           tabPanel("Recycling",value="recycling",icon=icon("trash"),
              fluidRow(
                column(1),
                column(10,
                       markdown("
#### Recycling 

Wouldn't it be terrific to recycle stuff instead of dumping it? It's an irresistable idea and
has sometimes been spectacularly successful.

                                ")
                       ),
              ),
              fluidRow( column(2), column(9,  imageOutput("recyclingimages1",height="300px"))  ),
              fluidRow(
                  column(1),column(10,
                      markdown("
#### Recycling with oil 

Most recycling aims to reduce mining of one kind by using mining of another kind, namely oil
extraction. Oil is burned in making and using vehicles to collect and transform used 
material. 

When the energy required for collection and reprocessing is lower than for mining
new material, then everybody loves this. Aluminium recycling has been spectacularly
successful because recycled aluminium is identical to new aluminium but requires much less energy
to make. **Nevertheless, aluminium production has still risen from 15 million tonnes in 1984 to over
65 million tonnes in 2020**.
                               ")
                  )
                
              ),
              fluidRow( column(2), column(9, imageOutput("recyclingimages2",height="300px")) ),
              fluidRow(
                  column(1),column(10,
                      markdown("
#### Recycling with renewables 

If the energy is renewable, so the theory goes, then recycling is all part of an infinitely
repeatable loop. Again the issue is quantitative,  wind and solar harvesting require considerable mining to
build the plant and considerable land. Far more land is required than is required for mining. If for
example we need 5,000 hectares of solar farm to avoid a 300 hectare mine, then is this a winning strategy? Not all
hectares are equal, but wind and solar farms both degrade habitat; with wind farms killing bats and birds directly. 

In general, each case has to be assessed on its merits. In the real world, the best solution, even when
this means the least worst, may not conform to some simple slogan about a circular economy.

                               ")
                  )
                
              ),
              fluidRow( column(3), column(9, imageOutput("recyclingimages3",height="300px")) )
           )
        ) 
      ),   
      tabPanel("Distribution",value="distrib",icon=icon("network-wired"),
            column(12, imageOutput("guilioverne",height=230) ),
            column(12, "The cable laying vessel Guilio Verne." ),
            fluidRow(
              column(2),
              column(10,
                markdownFile("distribution.txt"),
                imageOutput("hvdc",height=450),
                markdownFile("distribution1.txt"),
                imageOutput("hvdcland",height=450),
                markdownFile("distribution2.txt")
              )
            )
      )
      )
      ),  # Electricity 
        tabPanel("Nuclear",value="thorcontab",icon=icon("atom"),
          tabsetPanel(id="tabsetnuclear",
              tabPanel("History",value="history",icon=icon("door-open"),
                  column(12, imageOutput("nukehistory",height=230) ),
                  column(12, "The Gösgen nuclear plant in Switzerland, producing clean electricity since 1979" ),
                  column(12, tags$div(class="spacer")),
                  fluidRow(
                       column(1,id="none"),
                       column(10,
                        markdownFile("historynuclear.txt"),
                        plotlyOutput("ausfrdeu"),
                        markdownFile("historynuclear1.txt")
                      )
                  )
              ),
              tabPanel("Radiation",value="radiation",icon=icon("dna"),
                  column(12, imageOutput("radimage",height=230) ),
                  column(12, "Radiation was the first carcinogen discovered, but it turned out to be causally 
                  weak compared to lifestyle factors."),
                  column(12, tags$div(class="spacer")),
                  fluidRow(
                     column(1),
                     column(10, 
                      markdownFile("radiation.txt")
                     )
                  )
              ),
              tabPanel("Why Thorcon?",value="thorcon",icon=icon("atom"),
#                  fluidRow(
#                     column(1),
#                    column(10, imageOutput("globalenergyshortfall",height=400) )
#                  ),
                  fluidRow(
                     column(1),
                    column(10, plotlyOutput("newglobalenergyshortfall",height=400) )
                  ),
                  fluidRow(
                     column(1),
                     column(10, 
                      markdownFile("thorcon.txt")
                     )
                  )
              )
           )
        )
    )
#--------------------------------------------------------------------------------------------------
# This is a kind of splash page ... not using it for now. Instead we just go to the "intro" page on 
# startup
#--------------------------------------------------------------------------------------------------
#     ,
#    tabPanel("Splash",
#             tags$div(id="splash",
#                  tags$p("How many container ships of solar panels need to be produced during the next 30 years?"),
#                  tags$img(src="container-ship-s.jpg",width="100%"),
#             )
#    )
#--------------------------------------------------------------------------------------------------------
    )
}

# Define server logic required to draw a histogram
server <- function(ui,input,output,session) {
#------------------------------------------------------------------------
#    timer <-reactiveValues(val=0,timer=reactiveTimer(10000))
#    observe({
#      timer$timer()
#      isolate(timer$val<-timer$val+1)
#      if (timer$val==2) {
#        print("hide")
#        hide("splash")
#        timer$timer<-reactiveTimer(Inf)
#      }
#    })
#------------------------------------------------------------------------
    v <-reactiveValues(u=NULL)
    
    observeEvent(input$batterysize1,{  v$batterysize=input$batterysize1  })
    observeEvent(input$batterysize2,{  v$batterysize=input$batterysize2  })
    observeEvent(v$batterysize,{
      if (input$batterysize1!=v$batterysize) { isolate(updateSliderInput(session,"batterysize1", value = v$batterysize)) }
      if (input$batterysize2!=v$batterysize) { isolate(updateSliderInput(session,"batterysize2", value = v$batterysize)) }
    })
    
    observeEvent(input$batterylifespan1,{  v$batterylifespan=input$batterylifespan1  })
    observeEvent(input$batterylifespan2,{  v$batterylifespan=input$batterylifespan2  })
    observeEvent(v$batterylifespan,{
      if (input$batterylifespan1!=v$batterylifespan) { isolate(updateSliderInput(session,"batterylifespan1", value = v$batterylifespan)) }
      if (input$batterylifespan2!=v$batterylifespan) { isolate(updateSliderInput(session,"batterylifespan2", value = v$batterylifespan)) }
    })
    
    observeEvent(input$batteryrecyclegrowthrate1,{  v$batteryrecyclegrowthrate=input$batteryrecyclegrowthrate1  })
    observeEvent(input$batteryrecyclegrowthrate2,{  v$batteryrecyclegrowthrate=input$batteryrecyclegrowthrate2  })
    observeEvent(v$batteryrecyclegrowthrate,{
      if (input$batteryrecyclegrowthrate1!=v$batteryrecyclegrowthrate) { isolate(updateSliderInput(session,"batteryrecyclegrowthrate1", value = v$batteryrecyclegrowthrate)) }
      if (input$batteryrecyclegrowthrate2!=v$batteryrecyclegrowthrate) { isolate(updateSliderInput(session,"batteryrecyclegrowthrate2", value = v$batteryrecyclegrowthrate)) }
    })
    
    observeEvent(input$batteryenergydensity1,{  v$batteryenergydensity=input$batteryenergydensity1  })
    observeEvent(input$batteryenergydensity2,{  v$batteryenergydensity=input$batteryenergydensity2  })
    observeEvent(v$batteryenergydensity,{
      gigasize<-35*(v$batteryenergydensity/150)
      print(paste0("Gigafactorysize: ",gigasize))
      if (input$batteryenergydensity1!=v$batteryenergydensity) { isolate(updateSliderInput(session,"batteryenergydensity1", value = v$batteryenergydensity)) }
      if (input$batteryenergydensity2!=v$batteryenergydensity) { isolate(updateSliderInput(session,"batteryenergydensity2", value = v$batteryenergydensity)) }
    })
    
    observeEvent(input$batterygrowthpercent1,{  v$batterygrowthpercent=input$batterygrowthpercent1  })
    observeEvent(input$batterygrowthpercent2,{  v$batterygrowthpercent=input$batterygrowthpercent2  })
    observeEvent(v$batterygrowthpercent,{
      if (input$batterygrowthpercent1!=v$batterygrowthpercent) { isolate(updateSliderInput(session,"batterygrowthpercent1", value = v$batterygrowthpercent)) }
      if (input$batterygrowthpercent2!=v$batterygrowthpercent) { isolate(updateSliderInput(session,"batterygrowthpercent2", value = v$batterygrowthpercent)) }
    })
    
    observeEvent(input$evpercent1,{  v$evpercent=input$evpercent1  })
    observeEvent(input$fcpercent,{  v$evpercent=100-input$fcpercent  })
    observeEvent(v$evpercent,{
      if (input$evpercent1!=v$evpercent) { isolate(updateSliderInput(session,"evpercent1", value = v$evpercent)) }
      if (input$fcpercent!=100-v$evpercent) { isolate(updateSliderInput(session,"fcpercent", value = 100-v$evpercent)) }
    })
    
    observeEvent(input$lmapname1,{  v$mapname=input$lmapname1  })
    observeEvent(input$lmapname2,{  v$mapname=input$lmapname2  })
    observeEvent(v$mapname,{
      if (input$lmapname1!=v$mapname) { isolate(updateSliderInput(session,"lmapname1", value = v$mapname)) }
      if (input$lmapname2!=v$mapname) { isolate(updateSliderInput(session,"lmapname2", value = v$mapname)) }
    })
    
    observeEvent(input$bkuphours1,{ v$bkuphours=input$bkuphours1  })
    observeEvent(input$bkuphours2,{ v$bkuphours=input$bkuphours2 })
    observeEvent(input$bkuphours3,{ v$bkuphours=input$bkuphours3  })
    observeEvent(v$bkuphours,{ 
      if (input$bkuphours1!=v$bkuphours) { updateSliderInput(session,"bkuphours1", value = v$bkuphours) }   
      if (input$bkuphours2!=v$bkuphours) { updateSliderInput(session,"bkuphours2", value = v$bkuphours) }   
      if (input$bkuphours3!=v$bkuphours) { updateSliderInput(session,"bkuphours3", value = v$bkuphours) }   
   })
    
    observeEvent(input$tonnesperGWPV1,{ v$tpGW=input$tonnesperGWPV1  })
    observeEvent(input$tonnesperGWPV2,{ v$tpGW=input$tonnesperGWPV2  })
    observeEvent(input$tonnesperGWPV3,{ v$tpGW=input$tonnesperGWPV3  })
    observeEvent(input$tonnesperGWPV4,{ v$tpGW=input$tonnesperGWPV4  })
    
    observeEvent(v$tpGW,{ 
      if (input$tonnesperGWPV1!=v$tpGW) { updateSliderInput(session,"tonnesperGWPV1", value = v$tpGW) }   
      if (input$tonnesperGWPV2!=v$tpGW) { updateSliderInput(session,"tonnesperGWPV2", value = v$tpGW) }  
      if (input$tonnesperGWPV3!=v$tpGW) { updateSliderInput(session,"tonnesperGWPV3", value = v$tpGW) } 
      if (input$tonnesperGWPV4!=v$tpGW) { updateSliderInput(session,"tonnesperGWPV4", value = v$tpGW) } 
   })
    
    observeEvent(input$pvrecyclegrowthrate1,{  v$pvrecyclegrowthrate=input$pvrecyclegrowthrate1   })
    observeEvent(input$pvrecyclegrowthrate2,{  v$pvrecyclegrowthrate=input$pvrecyclegrowthrate2   })
    observeEvent(input$pvrecyclegrowthrate3,{  v$pvrecyclegrowthrate=input$pvrecyclegrowthrate3   })
    observeEvent(input$pvRecycleGWnow,{  
      v$pvRecycleGWnow=input$pvRecycleGWnow   
    })
    
    observeEvent(v$pvRecycleGWnow,{
      if (input$pvRecycleGWnow!=v$pvRecycleGWnow) { updateSliderInput(session,"pvRecycleGWnow", value = v$pvRecycleGWnow) }
    })
    observeEvent(v$pvrecyclegrowthrate,{
      if (input$pvrecyclegrowthrate1!=v$pvrecyclegrowthrate) { updateSliderInput(session,"pvrecyclegrowthrate1", value = v$pvrecyclegrowthrate) }
      if (input$pvrecyclegrowthrate2!=v$pvrecyclegrowthrate) { updateSliderInput(session,"pvrecyclegrowthrate2", value = v$pvrecyclegrowthrate) }
      if (input$pvrecyclegrowthrate3!=v$pvrecyclegrowthrate) { updateSliderInput(session,"pvrecyclegrowthrate3", value = v$pvrecyclegrowthrate) }
    })
    
    observeEvent(input$mtbfPV1,{  v$mtbfpv=input$mtbfPV1   })
    observeEvent(input$mtbfPV2,{  v$mtbfpv=input$mtbfPV2   })
    observeEvent(input$mtbfPV3,{  v$mtbfpv=input$mtbfPV3   })
    observeEvent(v$mtbfpv,{
      if (input$mtbfPV1!=v$mtbfpv) { updateSliderInput(session,"mtbfPV1", value = v$mtbfpv) }
      if (input$mtbfPV2!=v$mtbfpv) { updateSliderInput(session,"mtbfPV2", value = v$mtbfpv) }
      if (input$mtbfPV3!=v$mtbfpv) { updateSliderInput(session,"mtbfPV3", value = v$mtbfpv) }
    })
    
#    observeEvent(input$mtbfwind1,{  v$mtbfwind=input$mtbfwind1   })
    observeEvent(input$mtbfwind2,{  v$mtbfwind=input$mtbfwind2   })
    observeEvent(v$mtbfwind,{
#      if (input$mtbfwind1!=v$mtbfwind) { updateSliderInput(session,"mtbfwind1", value = v$mtbfwind) }
      if (input$mtbfwind2!=v$mtbfwind) { updateSliderInput(session,"mtbfwind2", value = v$mtbfwind) }
    })
    
#    observeEvent(input$windbkuphours1,{  v$windbkuphours=input$windbkuphours1   })
    observeEvent(input$windbkuphours2,{  v$windbkuphours=input$windbkuphours2   })
    observeEvent(v$windbkuphours,{
#      if (input$windbkuphours1!=v$windbkuphours) { updateSliderInput(session,"windbkuphours1", value = v$windbkuphours) }
      if (input$windbkuphours2!=v$windbkuphours) { updateSliderInput(session,"windbkuphours2", value = v$windbkuphours) }
    })
    
#    observeEvent(input$windrecyclegrowthrate1,{  v$windrecyclegrowthrate=input$windrecyclegrowthrate1   })
    observeEvent(input$windrecyclegrowthrate2,{  v$windrecyclegrowthrate=input$windrecyclegrowthrate2   })
    observeEvent(v$windrecyclegrowthrate,{
#      if (input$windrecyclegrowthrate1!=v$windrecyclegrowthrate) { updateSliderInput(session,"windrecyclegrowthrate1", value = v$windrecyclegrowthrate) }
      if (input$windrecyclegrowthrate2!=v$windrecyclegrowthrate) { updateSliderInput(session,"windrecyclegrowthrate2", value = v$windrecyclegrowthrate) }
    })
    
#    observeEvent(input$windRateIncrease1,{  v$windRateIncrease=input$windRateIncrease1   })
    observeEvent(input$windRateIncrease2,{  v$windRateIncrease=input$windRateIncrease2   })
    observeEvent(v$windRateIncrease,{
#      if (input$windRateIncrease1!=v$windRateIncrease) { updateSliderInput(session,"windRateIncrease1", value = v$windRateIncrease) }
      if (input$windRateIncrease2!=v$windRateIncrease) { updateSliderInput(session,"windRateIncrease2", value = v$windRateIncrease) }
    })
    
    # XXX
#    observeEvent(input$tonnesPerGWWind1,{  v$tonnesPerGWWind=input$tonnesPerGWWind1   })
    observeEvent(input$tonnesPerGWWind2,{  v$tonnesPerGWWind=input$tonnesPerGWWind2   })
    observeEvent(v$tonnesPerGWWind,{
#      if (input$tonnesPerGWWind1!=v$tonnesPerGWWind) { updateSliderInput(session,"tonnesPerGWWind1", value = v$tonnesPerGWWind) }
      if (input$tonnesPerGWWind2!=v$tonnesPerGWWind) { updateSliderInput(session,"tonnesPerGWWind2", value = v$tonnesPerGWWind) }
    })
    
    observeEvent(input$pvRateIncrease1,{  v$pvRateIncrease=input$pvRateIncrease1   })
    observeEvent(input$pvRateIncrease2,{  v$pvRateIncrease=input$pvRateIncrease2   })
    observeEvent(input$pvRateIncrease3,{  v$pvRateIncrease=input$pvRateIncrease3   })
    observeEvent(input$pvRateIncrease4,{  v$pvRateIncrease=input$pvRateIncrease4   })
#    observeEvent(input$setcurrent,{
#        updateSliderInput(session,"ieasolartwh", value = 0.8)
#        updateSliderInput(session,"ieawindtwh", value = 1.5)
#        updateSliderInput(session,"ieanucleartwh", value = 2.7)
#        updateSliderInput(session,"ieahydrotwh", value = 4.4)
#        updateSliderInput(session,"ieafftwh", value = 16)
#    })
#    observeEvent(input$setieagoals,{
#        updateSliderInput(session,"ieasolartwh", value = 23.4)
#        updateSliderInput(session,"ieawindtwh", value = 24.7)
#        updateSliderInput(session,"ieanucleartwh", value = 5.4)
#        updateSliderInput(session,"ieahydrotwh", value = 8.4)
#        updateSliderInput(session,"ieafftwh", value = 2)
#    })
    observeEvent(input$setIEAvalues,{
        print("set IEA values")
        isolate(updateSliderInput(session,"globalvsale", value = 94))
        isolate(updateSliderInput(session,"evpercent1", value = 62))
    })
    
    observeEvent(input$masterunits,{
        v$u=input$masterunits 
    })
    observeEvent(v$pvRateIncrease,{
      print("pvperyear----------------------------------------------------")
      print(paste0("pvRecycleGWnow: ",pvRecycleGWnow))
      if (input$pvRateIncrease1!=v$pvRateIncrease) { isolate(updateSliderInput(session,"pvRateIncrease1", value = v$pvRateIncrease)) }
      if (input$pvRateIncrease2!=v$pvRateIncrease) { isolate(updateSliderInput(session,"pvRateIncrease2", value = v$pvRateIncrease)) }
      if (input$pvRateIncrease3!=v$pvRateIncrease) { isolate(updateSliderInput(session,"pvRateIncrease3", value = v$pvRateIncrease)) }
      if (input$pvRateIncrease4!=v$pvRateIncrease) { isolate(updateSliderInput(session,"pvRateIncrease4", value = v$pvRateIncrease)) }
      print(v$pvRateIncrease)
      pvProductionFun<-makeExpProduction(v$pvRateIncrease,pvProdGWnow,years)
      #pvRecycleFun<-makeRecycle(v$pvrecyclegrowthrate,pvRecycleGWnow)
      pvRecycleFun<-makeRecycle(v$pvrecyclegrowthrate,v$pvRecycleGWnow)
      gwa <- calculatePVfuture(years,pvProductionFun,pvRecycleFun,batteryRecycleFun,
                            mtbfpv=input$mtbfPV1,
                            bkuphours=v$bkuphours,
                            mtbfbatt=input$batterylifespan1,
                            startingGW=pvStartingGW,
                            tonnePerGW=input$tonnesperGWPV1,
                            pvcf=input$pvCapacityFactor,
                            batteryenergydensity=input$batteryenergydensity1) 
      
      #isolate(updateSliderInput(session,"ieasolartwh", value = gwa$tot/1000))
      #print(paste0("gwavailable: ",gwa$tot/1000))
      print("pvperyearEnd")
    })
    observeEvent(v$u,{
      if (input$haunits!=v$u) {
        updateSliderInput(session,"haunits", value = v$u)
      }  
      if (input$synunits!=v$u) {
        updateSliderInput(session,"synunits", value = v$u)
      }  
      if (input$gasunits!=v$u) {
        updateSliderInput(session,"gasunits", value = v$u)
      }  
    })
    theme_update(axis.text=element_text(size=15,color="black",family="Helvetica")) 
    dfebyregion<-read_csv("el-by-region.csv")
    
    dfb23<-read_csv("li-b-tier.csv") %>% pivot_longer(cols=c('Tier1','Tier2','Tier3'),names_to='Tier') %>% 
        filter(Year=='2023' & (Tier=="Tier1"|Tier=="Tier2"))
    
    c2023<-dfb23 %>% summarise(total=sum(value))
    print(c2023)
    prod2023<-reactive({ 
      print(input$evreserved)
      print(input$batterysize1)
      (((input$evreserved/100)*c2023*1e9)/(input$batterysize1*1e3))/1e6 
    })
    sales<-tribble(
        ~type, ~count,
        "Electric",3.1e6,
        "Internal Combustion",75e6,
        "Hydrogen",7500
    )
    whoil<-reactive({input$synOil*ElSynOnePercOil})
    ngf<-reactive({
      ((1/(input$evreserved/100))*input$globalvsale*1e6*(input$evpercent1/100)*input$batterysize1*1e3)/(gigasize*1e9)
    })
    
    eh2<-reactive({(input$hydrogenReq/70)*3600e12})
    eh2fleet<-reactive({
         ((repoilh2car/1e9)*(1-(v$evpercent/100))*1e6*ElPerTonneH2)
    })
    dfnukesteel<-reactive({
      calculateNukeSteel(input$nucleartwh)
    })
    dfbatteries<-reactive({
      batteryProductionFun<-makeExpProduction(input$batterygrowthpercent1,batteryGWhPerYearnow,years)
      batteryRecycleFun<-makeRecycle(input$batteryrecyclegrowthrate1,batteryRecycleGWhnow)
      calculateBatteryFuture(years,batteryProductionFun,batteryRecycleFun,
                             batteryenergydensity=v$batteryenergydensity,
                             batterylifespan=input$batterylifespan1,
                             evreserved=input$evreserved,
                             batterysize=input$batterysize1
                             )
    })
    dfpvspecs<-reactive({
      print("dfpvspecs")
      pvProductionFun<-makeExpProduction(v$pvRateIncrease,pvProdGWnow,years)
      pvRecycleFun<-makeRecycle(v$pvrecyclegrowthrate,v$pvRecycleGWnow)
      calculatePVfuture(years,pvProductionFun,pvRecycleFun,batteryRecycleFun,
                            mtbfpv=input$mtbfPV1,
                            bkuphours=v$bkuphours,
                            mtbfbatt=input$batterylifespan1,
                            startingGW=pvStartingGW,
                            pvcf=input$pvCapacityFactor,
                            tonnePerGW=input$tonnesperGWPV1,
                            batteryenergydensity=v$batteryenergydensity) 
    }) 
    dfwindspecs<-reactive({
      print("dfwindspecs")
      windProductionFun<-makeExpProduction(v$windRateIncrease,windProdGWnow,years)
      windRecycleFun<-makeRecycle(v$windrecyclegrowthrate,windRecycleGWnow)
      calculateWindfuture(years,windProductionFun,windRecycleFun,batteryRecycleFun,
                            mtbfwind=v$mtbfwind,
                            bkuphours=v$windbkuphours,
                            mtbfbatt=input$batterylifespan1,
                            startingGW=windStartingGW,
                            tonnePerGW=v$tonnesPerGWWind,
                            batteryenergydensity=v$batteryenergydensity) 
    }) 
    batttonnes<-reactive({
      dfb<-dfbatteries() %>% extractMaxMT()
      print(names(dfb))
      
      avail<-dfb$batteryOperationalTonnes 
      fbtonnes<-dfb$batteryFailedTonnes 
      wbtonnes<-dfb$batteryWasteTonnes 
      totproduced<-avail+fbtonnes-dfb$batteryRecycledTonnes
      recycled<-dfb$batteryRecycledTonnes
      gf2050<-dfbatteries() %>% summarise(m=max(gigafactories))
      evb2050<-dfbatteries() %>% summarise(m=max(evbatteries))
      otherb2050<-dfbatteries() %>% summarise(m=(max(otherbatteries)*v$batterysize*1e3)/1e9) 
      
      tags$div(class="standout-container",
           p(class="standout-c","Implications (to 2050) "),
           p(class="standout",tags$span(img(class="it",src="factory-w.png",width="50px"),paste0("Battery gigafactories in 2050: ",comma(ceiling(as.numeric(gf2050))),"\n\n"))),
           p(class="standout",tags$span(img(class="it",src="electric-vehicle-w.png",width="50px"),
                                        paste0("EV batteries/yr in 2050: ",
                                        comma(ceiling(as.numeric(evb2050))),
                                        " (",comma(v$batterysize)," kwh)",
                                        "\n\n"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),
                                        paste0("Other batteries/yr in 2050: ",
                                        comma(otherb2050),
                                        " GWh \n\n"))),
         #  p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),paste0("(assuming ",
         #                      comma(input$evreserved), "% capacity reserved and ",
         #                      comma(input$batterysize), " kwh batteries)\n\n"))),
           p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),paste0("Batteries manufactured: ",comma(totproduced)," million tonnes \n\n"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("Batteries operational: ",comma(avail)," million tonnes \n\n"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("(including recycled: ",comma(recycled)," million tonnes) \n\n"))),
           p(class="standout",tags$span(img(class="it",src="battery-broken-w.png",width="50px"),paste0("Batteries failed: ",comma(fbtonnes)," million tonnes \n\n"))),
           p(class="standout",tags$span(img(class="it",src="dump-truck-w.png",width="50px"),paste0("Batteries waste: ",comma(wbtonnes)," million tonnes \n\n"))),
           p(class="standout-f",tags$span(footnotespecsBatteries()))
      )
    })
    nwastedesc<-reactive({
      df<-dfpvspecs() 
      dfms<- df %>% extractMaxStates("pv")
      dfmt<- df %>% extractMaxMT() 
      mtbfbatt<-input$batterylifespan1
      print(names(dfmt))
      totw<-df %>%summarise(sum(nwaste))
      tags$div(class="standout-container",
           p(class="standout-c",paste0("Waste comparison to 2050 of ",comma(dfms$pvOperational)," GW of PV or " ,
                                       comma(dfms$pvOperational*(input$pvCapacityFactor/90))," GW of Nuclear ")),
           p(class="standout",tags$span(img(class="it",src="poisonous-w.png",width="50px"),
                                        paste0("Panel waste: ",comma(dfmt$pvWasteTonnes)," million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="poisonous-w.png",width="50px"),
                                        paste0("Battery waste: ",comma(dfmt$wasteBatteryGWhTonnes)," million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="radiation-w.png",width="50px"),
                                        paste0("Spent fuel waste: ",comma2(totw/1e6)," million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="radiation-w.png",width="50px"),
                                        paste0("Spent fuel waste (with recycling): ",comma2(totw/20/1e6)," million tonnes"))),
           p(class="standout-f",tags$span(footnotespecsPV()))
      )
    })
    pvptnew<-reactive({
      print("pvptnew...")
      df<-dfpvspecs() 
      dfms<- df %>% extractMaxStates("pv")
      dfmt<- df %>% extractMaxMT() 
      print(names(dfmt))
      haop<-as.numeric(dfms$pvOperational)*input$haPerGW
      tags$div(class="standout-container",
           p(class="standout-c","Implications (by 2050)"),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w.png",width="50px"),
                                        paste0("Operational panels: ",comma(dfmt$pvOperationalTonnes)," million tonnes (",
                                               comma(dfms$pvOperational)," GW )" ))),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w.png",width="50px"),paste0("(including recycled: ",comma(dfmt$pvRecycledTonnes)," million tonnes) "))),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w-broke.png",width="50px"),paste0("Failed panels: ",comma(dfmt$pvFailedTonnes)," million tonnes"))),
#          p(class="standout",tags$span(img(class="it",src="dump-truck-w.png",width="50px"),paste0("Panel waste: ",comma(dfmt$pvWasteTonnes)," million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("Battery tonnage: ",
                                                                                                comma(dfmt$batteryGWhTonnes),
                                                                                                " million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("Battery waste: ",
                                                                                                comma(dfmt$wasteBatteryGWhTonnes),
                                                                                                " million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),paste0("Copper: ",comma(dfms$pvOperational*input$cuPerGW/1e6)," million tonnes"))),
           p(class="standout-f",tags$span(footnotespecsPV()))
      )
    })
    boiledWater<-
      tags$div(class="standout-container",
           p(class="standout-c","How much water could you boil with?"),
           p(class="standout",tags$span(img(class="it",src="battery-phone-w.png",width="50px"),paste0("A high end phone battery: ",comma(calculateWaterBoiledByWattHours(18.5))," grams \n\n"))),
           p(class="standout",tags$span(img(class="it",src="bonfire-w.png",width="40px"),paste0("A 60g piece of wood: ",comma(calculateWaterBoiledByWattHours(60))," grams\n\n"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("An 18kg car battery: ",comma(calculateWaterBoiledByWattHours(700))," grams\n\n"))),
           p(class="standout",tags$span(img(class="it",src="bonfire-w.png",width="50px"),paste0("An 18kg log of wood: ",comma(calculateWaterBoiledByWattHours(90000))," grams\n\n"))),
           p(class="standout",tags$span(img(class="it",src="pills-w.png",width="50px"),paste0("A 10g nuclear fuel pellet: ",comma(calculateWaterBoiledByWattHours(4.8e6)/1e6)," tonnes\n\n")))
      )
#p("",
#tags$span(id="xx",
#                  tags$img(src="nuclear-plant.png",width="30px"),
#tags$a(href="https://www.flaticon.com/authors/good-ware","www.flaticon.com")
    
    output$nGWexp <- renderUI({
        nwastedesc()
    })
    output$steelwindvthorcon <- renderUI({
                 twh<-input$twhwindvthorcon
                 gwthorcon<-1000*twh/7.884
                 gwwind<-((1e12*1000*twh)/(24*365*input$windcapfactor))/1e9
                 print(paste0("gwthorcon:",gwthorcon))
                 print(paste0("gwwind:",gwwind))
                 steelwind<-gwwind*v$tonnesPerGWWind
                 cuwind<-gwwind*input$cuPerGWWind
                 steelthorcon<-gwthorcon*steelPerGWThIsle
                 steelthorconLand<-gwthorcon*steelPerGWThLand
                 print(paste0("steelthorcon:",steelthorcon/1e6))
                 print(paste0("steelwind:",steelwind/1e6))
      tags$div(class="standout-container",
           p(class="standout-c","Material use for ",comma(1000*twh)," terawatt-hours per year of electricity"),
           p(class="standout",tags$span(img(class="it",src="wind-energy-w.png",width="50px"),
                                        paste0("Wind Turbines: ",commac(steelwind/1e6)," million tonnes (steel)\n\n"))),
           p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),
                                        paste0("Copper for turbines: ",commac(cuwind)," tonnes \n\n"))),
           p(class="standout",tags$span(img(class="it",src="nuclear-plant-w.png",width="50px"),
                                        paste0(comma(gwthorcon)," floating Thorcon reactors: ",commac(steelthorcon/1e6)," million tonnes (steel) \n\n"))),
           p(class="standout",tags$span(img(class="it",src="nuclear-plant-w.png",width="50px"),
                                        paste0("OR ... Land based: ",commac(steelthorconLand/1e6)," million tonnes (steel) \n\n")))
      )
    })
    output$ieagwgoals <- renderPlotly({
      p<-ieagoals %>% pivot_longer(c("GW2050","GW2020"),names_to="Year",values_to="value") %>% 
        mutate(ThousandGW=value/1000) %>%
        ggplot()+geom_col(aes(x=fct_reorder2(Technology,Year,ThousandGW,.desc=FALSE),y=ThousandGW,fill=Year),position="dodge") + coord_flip()+
        labs(y="'000 Gigawatts",x="",title="IEA Power Targets")+
        theme(axis.text.y=element_text(size=12,hjust=0,vjust=0))
              ggplotly(p) %>% ggplconfig
    })
    output$ieatwhgoals <- renderPlotly({
      p<-ieagoals %>% pivot_longer(c("TWh2050","TWh2020"),names_to="Year",values_to="value") %>% mutate(ThousandTWh=value/1000) %>%
        ggplot()+geom_col(aes(x=fct_reorder2(Technology,Year,ThousandTWh,.desc=FALSE),y=ThousandTWh,fill=Year),position="dodge") + coord_flip()+
        labs(y="'000 Terawatt-hours per year",x="",title="IEA Energy Targets")+
        theme(axis.text.y=element_text(size=12,hjust=0,vjust=0))
              ggplotly(p) %>% ggplconfig
      
    })
#    output$totalelectricity2050 <- renderUI({
#           sum<-input$ieasolartwh+input$ieawindtwh+input$ieahydrotwh+input$ieafftwh+input$ieanucleartwh
#           markdown(paste0("Total electricity from slider settings: ",comma(sum*1000),"TWh\n\n (IEA target by 2050 is 71,000 TWh) "))
#    })
    output$steelfornuclear <- renderUI({
      df<-dfnukesteel()
      peryear<-df$extraNukes[length(df$extraNukes)]-df$extraNukes[length(df$extraNukes)-1]
      tot<-df$extraNukes[length(df$extraNukes)]
      if (peryear>0) {
        markdown(paste0("We currently get some 2700 TWh from nuclear power, to get ",ncomma(1000*input$nucleartwh)," TWh by 2050, ",
"assuming no closures, we need to build ",comma(peryear)," Thorcon 1GW reactors each year for a total of ",
ceiling(tot)," reactors by 2050. Thorcon estimate that one shipyard can build 100x1GW reactors a year.",
"Thorcon have both a floating and land based design. The build time for a floating reactor will be significantly long."
                      ))
      } else markdown("")
      
    })
    output$steelanduraniumfornuclear <- renderUI({
          df<-dfnukesteel()
          print(names(df))
          steelLand<-df %>% summarize(smt=max(totalsteelLand)/1e6)
          steelIsle<-df %>% summarize(smt=max(totalsteelIsle)/1e6)
          uranium<-df %>% summarize(smt=max(uranium))
          reactors<-df %>% summarize(smt=ceiling(max(extraNukes)))
          if (as.numeric(reactors)>0) {
            
          tags$div(class="standout-container",
               p(class="standout-c","Thorcon reactor steel and uranium requirements (by 2050)"),
               p(class="standout",tags$span(img(class="it",src="nuclear-plant-w.png",width="50px"),
                                            paste0("Additional reactors by 2050: ",comma(as.numeric(reactors))," \n\n"))),
               p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),
                                            paste0("Annual uranium use by 2050: ",comma(as.numeric(uranium))," tonnes\n\n"))),
               p(class="standout",tags$span(img(class="it",src="nuclear-plant-w.png",width="50px"),
                                            paste0("Shipyard-years: ",comma(as.numeric(reactors/100))," \n\n"))),
               p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),
                                            paste0("Total steel by 2050 (Land): ",comma(as.numeric(steelLand))," million tonnes\n\n"))),
               p(class="standout",tags$span(img(class="it",src="mining-w.png",width="50px"),
                                            paste0("OR (Floating): ",comma(as.numeric(steelIsle))," million tonnes\n\n"))),
               p(class="standout-f",tags$span(footnotespecsNuke()))
           )
          } else markdown("")
          
    })
#    output$plsteelfornuclear <- renderPlotly({
#          df<-dfnukesteel()
#          p<-df %>% ggplot()+geom_col(aes(x=year,y=totalsteelIsle/1e6))+
#                labs(x="Year",y="Million tonnes of steel",title="Steel for extra nuclear plants")
#          ggplotly(p) %>% ggplconfig
#    })
#    output$pluraniumfornuclear <- renderPlotly({
#          df<-dfnukesteel()
#          p<-df %>% ggplot()+geom_col(aes(x=year,y=uranium/1000))+
#                labs(x="Year",y="Thousand tonnes of uranium annually",title="Additional uranium required")
#          ggplotly(p) %>% ggplconfig
#    })
    output$plbatteriestonnage2 <- renderPlotly({
          df<-dfbatteries() 
          print(names(df))
          df <-df %>% pivot_longer(ends_with("Tonnes"),names_to="State",values_to="Tonnes") 
          df$State <- sub("Tonnes","",df$State)
          df$State <- sub("battery","",df$State)
          p<-df %>% ggplot()+geom_col(aes(x=year,y=Tonnes/1e6,fill=State),position="dodge")+
                labs(x="Year",y="Million tonnes",title="Battery and battery waste growth")
              ggplotly(p) %>% ggplconfig
    })
    output$plbatteriestonnage <- renderPlotly({
          df<-dfbatteries() 
          print(names(df))
          print(paste0("Total Produced: ",toString(df$batteryProduced)))
          df <-df %>% pivot_longer(ends_with("Tonnes"),names_to="State",values_to="Tonnes") 
          df$State <- sub("Tonnes","",df$State)
          df$State <- sub("battery","",df$State)
          p<-df %>% ggplot()+geom_col(aes(x=year,y=Tonnes/1e6,fill=State),position="dodge")+
                labs(x="Year",y="Million tonnes",title="Material flows, global batteries")
              ggplotly(p) %>% ggplconfig
    })
    output$plbatteries <- renderPlotly({
          df<-dfbatteries() 
          print(names(df))
          df <-df %>% pivot_longer(starts_with("battery")&!ends_with("Tonnes"),names_to="State",values_to="GWh") 
          df$State <- sub("battery","",df$State)
          p<-df %>% ggplot()+geom_col(aes(x=year,y=GWh/1000,fill=State),position="dodge")+
                labs(x="Year",y="'000 GWh",title="Global battery energy capacity")
              ggplotly(p) %>% ggplconfig
    })
    output$growthtable3 <- renderPlotly({
          df<-readRDS("growthtables2021-10.rds")
          rmTypes<-c("SolarPlusWindPlusBio","Clean")
          ntop<-20
          top<-df %>% filter(!Type %in% rmTypes) %>% 
            arrange(desc(Max)) %>% 
            select(Country,Type,MaxYear,Max,MaxPop,cleanMWhPerCap,totalMWhPerCap) %>% ungroup() %>% 
            slice_head(n=ntop) %>% 
            mutate(Rank=rank(desc(Max))) %>%
            mutate(Key=paste0(Rank,". ",Country,", ",MaxYear,", ",comma2(as.numeric(MaxPop)/1e6),"m"))
          write_csv(top,file="xxx.csv")
          colorVals<-vector(length=length(top$Type))
          for(i in 1:length(top$Type)) {
              if (top$Type[i]=="Solar") { colorVals[i]="#ffff99"}
              if (top$Type[i]=="SolarPlusWind") { colorVals[i]="#ffff00"}
              if (top$Type[i]=="Wind") { colorVals[i]="red"}
              if (top$Type[i]=="Hydro") { colorVals[i]="#39f"}
              if (top$Type[i]=="Nuclear") { colorVals[i]="green"}
              if (top$Type[i]=="Oil") { colorVals[i]="brown"}
              if (top$Type[i]=="Gas") { colorVals[i]="grey"}
              if (top$Type[i]=="Coal") { colorVals[i]="black"}
          }
          
          plot_ly() %>%
              add_bars(
                type="bar",orientation="h",x=top$Max,y=reorder(top$Key,top$Max),text=comma(top$Max),
                marker=list(color=colorVals),
                       textposition='outside',
                       texttemplate="%{x:,.3s}",
                       name="Max over 10 years") %>% 
              layout(xaxis=list(title="Max over 10 years [STILL UNDER CONSTRUCTION]"),yaxis=list(title="")) %>% ggplconfig
    })
    output$PlEhydrogenCmp <- renderPlotly({
        df1<-dfebyregion %>% mutate(type="Highlight")
        t=tibble("Region"="Hydrogen by Electrolysis","TWh"=eh2()/1e12,type="Normal")
        df<-bind_rows(df1,t)
        bnd<-15000
        if ((eh2()/1e12)>bnd) {
            bnd=as.numeric(eh2()/1e12)+10000
        }
        locs<-'outside'
        mxtwh <- t %>% summarise(m=max(TWh))
        if (mxtwh>10000) {
          locs<-'inside'
          
        }
        plot_ly() %>%
              add_bars(
                type="bar",y=df1$Region,x=df1$TWh,text=df1$TWh,marker=list(color="#008080"),
                       textposition='outside',
                       texttemplate="%{x:,.3s}",
                       name="Current electricity") %>% 
              add_bars(
                type="bar",y=t$Region,x=t$TWh,text=t$TWh,marker=list(color="#ff0000"),
                       textposition=locs,
                       texttemplate="%{x:,.3s}",
                       name="Additional electricity") %>% 
              layout(xaxis=list(title="TWh (terawatt-hours)"),yaxis=list(title="")) %>% ggplconfig
    })
    
    gph2<-reactive({ElPerTonneH2*input$gaspeaking*4.5e6})                                      
    ghh2<-reactive({ElPerTonneH2*represgash2mt*1e6*(input$gasheating/100)})
    
    output$Hgasheat<- renderUI(markdown(paste0(startCard(),"To replace ",
                                        input$gasheating,
                                        "% of gas currently used residentially, we would need ",
                                        represgash2mt*(input$gasheating/100),
                                        " million tonnes of hydrogen, which, if produced by electrolysis, would need ",
                                        comma(ghh2()/1e12)," TWh of electricity. ",endCard()
                                        )))
    output$Hgaspeak <- renderUI(markdown(paste0(startCard(),"To run ",
                                         comma(input$gaspeaking*25),
                                         " GW of gas peaking power plants at 40% capacity with hydrogen would require ", 
                                         comma(input$gaspeaking*4.5), " million tonnes of hydrogen annually, which if generated by hydrolysis, would need ",
                                         comma(gph2()/1e12), " TWh. You can compare this with other numbers in the graph below. The entire electrical",
                                         " output of the EU is 3.89k (3,890) TWh. If you have the slider at 100, you can see that we'd need ",
                                         "about 4 times the entire current EU electrical output to make this peaking hydrogen. " ,
                                         endCard()
                                         )))
    output$Plgaspeak <- renderPlotly({
        df1<-dfebyregion %>% mutate(type="Old")
        t=tibble("Region"="Gas Peaking H2","TWh"=gph2()/1e12,type="New")
        t2=tibble("Region"="Gas Heating H2","TWh"=ghh2()/1e12,type="New")
        t3<-bind_rows(t,t2)
        bnd<-15000
        if ((gph2()/1e12)>bnd) {
            bnd=as.numeric(gph2()/1e12)+10000
        }
        locs<-'outside'
        mxtwh <- t3 %>% summarise(m=max(TWh))
        if (mxtwh>10000) {
          locs<-'inside'
          
        }
        plot_ly() %>%
              add_bars(
                type="bar",y=df1$Region,x=df1$TWh,text=df1$TWh,marker=list(color="#008080"),
                       textposition='outside',
                       texttemplate="%{x:,.3s}",
                       name="Current electricity") %>% 
              add_bars(
                type="bar",y=t3$Region,x=t3$TWh,text=t3$TWh,marker=list(color="#ff0000"),
                       textposition=locs,
                       texttemplate="%{x:,.3s}",
                       name="Additional electricity") %>% 
              layout(xaxis=list(title="TWh (terawatt-hours)"),yaxis=list(title="")) %>% ggplconfig
    })
    
    output$gaspeak <- renderUI({
        makeComparisonDescription(gph2()+ghh2(),input$gasunits,v)
    })
    output$Lgaspeakheat <- renderLeaflet({
      thename<-input$gasunits
      makePlot(thename,gph2()+ghh2())
    })
    
    output$Ehydrogen <- renderUI(markdown(paste0(startCard(),"To produce ",
                                          input$hydrogenReq,
                                          " million tonnes of hydrogen (H2) with electrolysis will require ",
                                          comma(eh2()/1e12)," TWh of electricity (",comma(eh2()/ElGlobal2020*100),
                                          "% of current global electricity production). ",
                                          "Use the graph below to compare the amount with the electricity ",
                                          "currently generated in various regions.",endCard()
                                          )))
    output$EHaHydrogen <- renderUI({
          makeComparisonDescription(eh2(),input$haunits,v)
    })
    output$EHaHydrogenFleet <- renderUI({
          makeComparisonDescription(eh2fleet(),input$haunits,v)
    })
    output$LHaHydrogen <- renderLeaflet({
      thename<-input$haunits
      makePlot(thename,eh2())
    })
    output$LHaHydrogenFleet <- renderLeaflet({
      thename<-input$haunits
      makePlot(thename,eh2fleet())
    })
    
    output$HaSynOil <- renderUI({
      makeComparisonDescription(whoil(),input$synunits,v)
    })

    output$LHaSynOil <- renderLeaflet({
      thename<-input$synunits
      makePlot(thename,whoil())
    })
    output$ESynOil <- renderUI(markdown(paste0(startCard(),"IEA estimate electricity requirements to replace ",comma(input$synOil),
                                        "% of oil with synthetic hydrocarbons: ",
                                        comma(whoil()/1e12)," TWh (",
                                        comma(((input$synOil*ElSynOnePercOil)/ElGlobal2020)*100),
                                        "% of current global electricity production)",endCard())))

    output$hydrogen <- renderUI(markdown(paste0(startCard(),"To replace the gas/petrol by hydrogen in ",comma((1-(v$evpercent/100))*100),
                                  "% of the global fleet of light vehicles (mostly cars), we will need ",
                                  comma((repoilh2car/1e9)*(1-(v$evpercent/100))),
                                  " megatonnes of hydrogen annually. If this is generated by ",
                                  "hydrolysis, it will need ",
                                  comma(((repoilh2car/1e9)*(1-(v$evpercent/100))*1e6*ElPerTonneH2)/1e12), 
                                  " TWh of electricity (we assume that a fuel cell is 2.5 times more energy efficient than an internal combustion engine).  ",
                                  endCard()
                                  )))
    
    output$hydrogenfleet <- renderPlotly({
        t=tibble("Region"="Hydrogen by Electrolysis","TWh"=((repoilh2car/1e9)*(1-(v$evpercent/100))*1e6*ElPerTonneH2)/1e12,
                 type="Normal")
        df1<-dfebyregion %>% mutate(type="Highlight")
#        t=tibble("Region"="Hydrogen by Electrolysis","TWh"=eh2()/1e12,type="Normal")
        df<-bind_rows(df1,t)
        bnd<-15000
        if ((eh2()/1e12)>bnd) {
            bnd=as.numeric(eh2()/1e12)+10000
        }
        locs<-'outside'
        mxtwh <- t %>% summarise(m=max(TWh))
        if (mxtwh>10000) {
          locs<-'inside'
          
        }
        plot_ly() %>%
              add_bars(
                type="bar",y=df1$Region,x=df1$TWh,text=df1$TWh,marker=list(color="#008080"),
                       textposition='outside',
                       texttemplate="%{x:,.3s}",
                       name="Current electricity") %>% 
              add_bars(
                type="bar",y=t$Region,x=t$TWh,text=t$TWh,marker=list(color="#ff0000"),
                       textposition=locs,
                       texttemplate="%{x:,.3s}",
                       name="Additional electricity") %>% 
              layout(xaxis=list(title="TWh (terawatt-hours)"),yaxis=list(title="")) %>% ggplconfig
    })
    output$gigafactories <- renderUI(markdown(paste0(startCard(),"How many gigafactories (35GWh/yr) are needed to make batteries for ",
        input$globalvsale," million vehicles in a year? Assuming ",comma(v$evpercent), 
        " percent are electric and with ",input$evreserved,
        " percent of battery capacity reserved for EVs and with an average battery size of ",input$batterysize1, " kwh? ",endCard()
        )))
    output$ngf <- renderPlotly({
        csvt<-paste0("Type,Number\n",
        'Gigafactories,',comma(ngf()),"\n")
        bnd<-500
        if (ngf()>500) {
            bnd=as.numeric(ngf())+20
            }
        df<-read_csv(csvt)
        p<-df %>% ggplot() + 
            geom_bar(aes(x=Type,y=Number),fill="#008080",stat="identity",width=0.4)+ coord_flip()+
            geom_text(aes(x=Type,y=Number+30,label=Number,hjust=0))+
            ylim(c(0,bnd))+
            labs(title="",x="",y="")+
            theme(
                text=element_text(color="#008080",size=12,face="bold",family="Helvetica"),
                axis.text=element_text(color="#008080",size=12,face="bold",family="Helvetica"),
                plot.title=element_text(color="#008080",size=15,face="bold",family="Helvetica")
           )
        ggplotly(p) %>% ggplconfig
    })
    output$sales2020 <- renderPlotly({
        p<-sales %>% ggplot() + 
            geom_bar(aes(x=type,y=count/1e6),width=0.3,fill="grey45",stat="identity")+ coord_flip()+
            geom_text(aes(x=type,y=count/1e6+2,label=comma(count/1e6),hjust=0))+
            labs(title="Light vehicle sales 2020",x="",y="Million vehicles")
        ggplotly(p) %>% ggplconfig
    })
    output$liioncap <- renderPlotly({
        p<-dfb23 %>% ggplot()+
            geom_bar(aes(x=Tier,y=value),width=0.3,fill="grey45",stat='identity')+
            coord_flip()+
            labs(y="GWh",x="",title="Estimated Li-Ion battery production capacity 2023")
        ggplotly(p) %>% ggplconfig
    })
    targ<-tribble(
      ~year,~value,~label,~yl,
      ymd("2050-01-01"),15000,"IEA Solar Target",ymd("2044-01-01")
    )
    btarg<-tribble(
      ~year,~value,~label,~yl,
      ymd("2050-01-01"),3100,"IEA grid battery target",ymd("2040-01-01")
    )
    output$wnGW <- renderPlotly({
              dfn<-dfpvspecs()
              tot<-dfn %>%summarise(sum(nwaste))
              p<-dfn %>% ggplot()+geom_col(aes(x=year,y=nwaste))+
                #geom_point(data=targ,aes(x=year,y=value),color="red")+
                #geom_text(data=targ,aes(x=yl,y=15000,label=label))+
                annotate('text',x=ymd("2035-01-10"),y=50000,label=paste0("Cumulative waste weight total by 2050: ",comma(tot)," tonnes"))+
                labs(x="Year",y="Tonnes",title="High level nuclear waste ")
              ggplotly(p) %>% ggplconfig
    })
    output$nGW <- renderPlotly({
              dfn<-dfpvspecs() %>% mutate(thorcon=cumsum(thorconEquivalent))
              p<-dfn %>% ggplot()+geom_col(aes(x=year,y=thorcon))+
              labs(x="Year",y="Gigawatt (GW)",title="Total Thorcon GW Installed")
              ggplotly(p) %>% ggplconfig
    })
    setparam<-function(h1,v,h2) {
           div(class="standout row-flex",
               tags$span(class="cc1",h1),
               tags$span(class="cc2",v),
               tags$span(class="cc3",h2)
           )
    }
    footnotespecsNuke<-reactive({
        s<-paste0("Terawatt-hours annually ",comma(input$nucleartwh*1000))
        s
    })
    footnotespecsBatteries<-reactive({
         s<-paste0("Batt Production growth rate ",comma(input$batterygrowthpercent1),"%,",
         "Recycling capacity growth rate ",comma(input$batteryrecyclegrowthrate1),"%,",
         "Batt Lifespan: ",comma(input$batterylifespan1),"yrs,",
         "Batt size: ",comma(input$batterysize1),"kwh,",
         "Wh/kg: ",comma(v$batteryenergydensity)," ",
         "(IEA target,200 gigafactories by 2050)"
         )
         s
    })
    footnotespecsPV<-reactive({
         s<-paste0("PV growth rate: ",comma(v$pvRateIncrease),"%, ",
         "PV recycling capacity(now): ",comma(v$pvRecycleGWnow),"GW/yr, ",
         "PV recycling growth: ",comma(v$pvrecyclegrowthrate),"%, ",
         "PV per GW: ",comma(v$tpGW)," tonnes, ",
         "Battery backup time: ",comma(v$bkuphours)," hours, ",
         "Battery lifespan: ",comma(input$batterylifespan1)," years, ",
         "PV lifespan: ",comma(v$mtbfpv)," years")
         s
    })
    footnotespecsLand<-reactive({
          s<-paste0("PV growth rate: ",comma(v$pvRateIncrease),"%, ",
          "PV recycling capacity(now): ",comma(v$pvRecycleGWnow),"GW/yr, ",
          "PV recycling growth: ",comma(v$pvrecyclegrowthrate),"%, ",
          "PV per GW: ",comma(v$tpGW)," tonnes, ",
          "Hectares per GW: ",comma(input$haPerGW),", ",
          "PV lifespan: ",comma(v$mtbfpv))
          s
    })
    footnotespecsWind<-reactive({
          s<-paste0("Wind growth rate: ",comma(v$windRateIncrease),"%, ",
          "Wind recycling capacity(now): ",comma(windRecycleGWnow),"GW/yr, ",
          "Wind recycling growth: ",comma(v$windrecyclegrowthrate),"%, ",
          "Wind Turbines per GW: ",commac(v$tonnesPerGWWind)," tonnes, ",
          "Wind Turbine lifespan: ",comma(v$mtbfwind)," years")
          s
    })
    modelassumptions<-reactive({
           tags$div(class="standout-container ",
                tags$div(class="standout-c","Parameter values"),
                setparam("PV production growth rate: ",comma(v$pvRateIncrease)," percent (adjustable)"),
                setparam("Current PV recycling capacity: ",comma(v$pvRecycleGWnow)," GW/yr (guesstimate)"),
                setparam("PV recycling capacity growth rate: ",comma(v$pvrecyclegrowthrate)," percent (adjustable)"),
                setparam("Battery production growth rate: ",comma(input$batterygrowthpercent1)," percent (adjustable)"),
                setparam("Current Battery recycling capacity: ",comma(batteryRecycleGWhnow)," GW/yr (guesstimate)"),
                setparam("Battery recycling capacity growth rate: ",comma(input$batteryrecyclegrowthrate1)," percent (adjustable)"),
                setparam("PV lifespan: ",comma(v$mtbfpv)," years (adjustable)"),
                setparam("Battery lifespan: ",comma(input$batterylifespan1)," years (adjustable)"),
                setparam("Battery backup: ",comma(v$bkuphours)," hours (adjustable)"),
                setparam("Battery energy density: ",comma(v$batteryenergydensity)," watt-hours/kg (adjustable)"),
                setparam("PV per GW: ",comma(v$tpGW)," tonnes (adjustable)")
           )
    })
    output$modelassumptions <- renderUI({
      modelassumptions()
    })
    output$growthplot2 <- renderPlotly({
        twh<-read_csv("bp2021-wind-solar-hydro.csv")
        ltwh<-read_csv("bp2021-wind-solar-hydro-logistic.csv")
        print(names(twh))
        p<-twh %>% ggplot(aes(x=Year,y=TWh,color=Type,group=Type))+
          geom_line()+labs(title="Possible growth of Hydro, Wind and Solar power")+
          geom_line(aes(x=Year,y=Model,color=Type,group=Type),linetype="dotdash",data=ltwh)+
          annotate('text',x=2035,y=400,label="Data: BP World Energy Statistics 2021")
        ggplotly(p) %>% ggplconfig
    })
    output$growthplot <- renderPlotly({
        twh<-read_csv("bp2021-wind-solar-hydro.csv")
        ltwh<-read_csv("bp2021-wind-solar-hydro-logistic.csv")
        print(names(twh))
        p<-twh %>% mutate(PercentageIncrease=D1*100) %>% ggplot(aes(x=Year,y=PercentageIncrease,color=Type,group=Type))+
          geom_line()+labs(title="Growth rates of Hydro, Wind and Solar power")+
          annotate('text',x=2005,y=10,label="Data: BP World Energy Statistics 2021")
        ggplotly(p) %>% ggplconfig
    })
    output$hydroplot2 <- renderPlotly({
        twh<-read_csv("bp2021-wind-solar-hydro.csv")
        print(names(twh))
        p<-twh %>% ggplot(aes(x=Year,y=TWh,color=Type,group=Type))+
          geom_line()+
          annotate('text',x=2008,y=300,label="Data: BP World Energy Statistics 2021")
        ggplotly(p) %>% ggplconfig
    })
    output$hydroplot3 <- renderPlotly({
        twh<-read_csv("bp2021-wind-solar-hydro.csv")
        print(names(twh))
        p<-twh %>% ggplot(aes(x=Year,y=TWh,color=Type,group=Type))+
          geom_line()+xlim(2000,2050)+
          #stat_smooth(method = "lm",formula=y~x+I(x^2),fullrange=T)+
          stat_smooth(method = "gam",formula=y~s(x),fullrange=T)+
          annotate('text',x=2008,y=300,label="Data: BP World Energy Statistics 2021")
        ggplotly(p) %>% ggplconfig
    })
    output$hydroplot <- renderPlotly({
        twh<-read_csv("bp2021-wind-solar-hydro.csv")
        print(names(twh))
        p<-twh %>% ggplot()+
          geom_line(aes(x=Year,y=TWh,color=Type,group=Type))+
          annotate('text',x=2008,y=300,label="Data: BP World Energy Statistics 2021")
        ggplotly(p) %>% ggplconfig
    })
    output$pvTonnes <- renderPlotly({
              print("pvTonnes")
            dfmt<-dfpvspecs() %>% extractMT()
            p<-dfmt %>% ggplot()+geom_col(aes(x=year,y=MegaTonnes,fill=Materials),position="dodge")+
              labs(y="Millions of tonnes",title="PV requirements to 2050")
              ggplotly(p) %>% ggplconfig
    })
    output$pvGW <- renderPlotly({
              print("pvGW")
              dfl<-dfpvspecs() %>% extractStates() 
              p<-dfl %>% ggplot()+geom_col(aes(x=year,y=GW,fill=State),position="dodge")+
                geom_point(data=targ,aes(x=year,y=value),color="red")+
                geom_text(data=targ,aes(x=yl,y=15000,label=label))+
                labs(title="PV rollout to 2050")
              ggplotly(p) %>% ggplconfig
    })
    
    output$pvwastetonnesspec <- renderUI({
          df <- dfpvspecs()
          dfms<- df %>% extractMaxStates("pv")
          markdown(paste0(startCard(),"The graph below assumes a ",
                 comma(v$pvRateIncrease)," percent increase in PV production annually, with a panel lifespan of ",
                 comma(v$mtbfpv)," years,  for a total of ",comma(dfms$pvOperational)," GW in 2050. ",
                 "It also assumes magic recycling where all of a ",
                 " panel is returned to service, and with a growth in recycling capacity of ",
                 comma(v$pvrecyclegrowthrate)," percent per year. ",
                 endCard()
                 ))
    })
    output$pvwastetonnes <- renderPlotly({
              print("pvwastetonnes")
              dfpv<-dfpvspecs() %>% extractMT() %>% filter(Materials %in% c("pvWasteTonnes","pvRecycledTonnes","pvOperationalTonnes"))
              p<-dfpv %>% ggplot()+geom_col(aes(x=year,y=MegaTonnes,fill=Materials),position="dodge")+
                labs(x="Year",y="Million Tonnes",title="Growing tonnage of PV panel waste")
              ggplotly(p) %>% ggplconfig
    })
    output$boiledwater <- renderUI({ boiledWater })
    output$batterymining <- renderUI({ 
      batttonnes() 
    })
    output$windmines <- renderUI({ 
      print("windmines...")
      df<-dfwindspecs() 
      dfms<- df %>% extractMaxStates("wind")
      dfmt<- df %>% extractMaxMT() 
      print(names(dfmt))
      tags$div(class="standout-container",
           p(class="standout-c","Implications (by 2050)"),
           p(class="standout",tags$span(img(class="it",src="wind-energy-w.png",width="50px"),
                                        paste0("Operational turbines: ",
                                               comma(dfmt$windOperationalTonnes)," million tonnes (",
                                               comma(dfms$windOperational)," GW) "
                                               ))),
           p(class="standout",tags$span(img(class="it",src="wind-energy-w.png",width="50px"),
                                        paste0("(including recycled: ",comma(dfmt$windRecycledTonnes)," million tonnes) "))),
           p(class="standout",tags$span(img(class="it",src="wind-energy-w-broken.png",width="50px"),paste0("Failed turbines: ",
                                 comma(dfmt$windFailedTonnes), " million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="dump-truck-w.png",width="50px"),paste0("Turbine waste: ",
                                 comma(dfmt$windWasteTonnes), " million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("Operational batteries: ",
                                 comma(dfmt$batteryGWhTonnes), " million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="dump-truck-w.png",width="50px"),paste0("Battery waste: ",
                                 comma(dfmt$wasteBatteryGWhTonnes), " million tonnes"))),
           p(class="standout-f",tags$span(footnotespecsWind()))
      )
    })
    output$pvpanelland <- renderUI({ 
      print("pvpanelland...")
      df<-dfpvspecs() 
      dfms<- df %>% extractMaxStates("pv")
      dfmt<- df %>% extractMaxMT() 
      print(names(dfmt))
      haop<-as.numeric(dfms$pvOperational)*input$haPerGW
      tags$div(class="standout-container",
           p(class="standout-c","Implications (by 2050)"),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w.png",width="50px"),
                                        paste0("Solar farm area: ",comma(haop)," ha (", comma(dfms$pvOperational)," GW )" ))),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w.png",width="50px"),paste0("Operational panels: ",comma(dfmt$pvOperationalTonnes)," million tonnes"))),
           p(class="standout",tags$span(img(class="it",src="solar-panels-w.png",width="50px"),paste0("(including recycled: ",comma(dfmt$pvRecycledTonnes)," million tonnes) "))),
           p(class="standout",tags$span(img(class="it",src="battery-w.png",width="50px"),paste0("Battery waste: ",
                                 comma(dfmt$wasteBatteryGWhTonnes), " million tonnes"))),
           p(class="standout-f",tags$span(footnotespecsLand()))
      )
    })
    output$pvpaneltonnage <- renderUI({ pvptnew() })
                                               
    output$gtotliioncap <- renderUI({
              markdown(paste0(startCard(),"Global EV battery capacity 2023 (",input$batterysize1,
                                             "kwh batteries using ",input$evreserved,"% of production) : ",
                                             comma(prod2023()),
                                             " million  from a total battery production capacity of ",comma(c2023)," GWh ",endCard()))
    })
    output$newglobalenergyshortfall<-renderPlotly({
      getPalette = colorRampPalette(brewer.pal(9, "OrRd"))
      worldPop20<-7.795e9
      dfdata<-read_csv("PerCapPowerByCountry.csv")
      ndiv<-7
      restOfWorld<-read_csv("RestOfWorld.csv")
      dgroups<-dfdata %>% mutate(Quantile=paste0("E",ntile(-PerCapPower,ndiv)))
      groups<-dgroups %>% select(Country,Quantile,PerCapPower)
      qdfg<-dgroups %>% group_by(Quantile) %>%
        summarise(Population=sum(Popn)/1e6,PerCapPower=sum(TWh*1e12)/sum(Popn)/24/365) %>%
        arrange(desc(PerCapPower)) %>% rename(Region=Quantile)
      qdfg<-bind_rows(qdfg,restOfWorld)
      dfgroups <- qdfg %>% mutate(xmin=cumsum(Population)-Population,xmax=xmin+Population,ymin=0,ymax=PerCapPower) 
      totalgw<-dfgroups %>% summarise(sum(PerCapPower*Population/1000))
      totalpop<-dfgroups %>% summarise(sum(Population))
      groupCountries<-groups %>% group_by(Quantile) %>% 
        mutate(str=splitter(paste(paste0(Country," ",comma(PerCapPower),"w"),sep=", ",collapse=", "))) %>% 
        ungroup() %>% select(Quantile,str) %>% rename(Region=Quantile) %>% unique() 
      pdfgroups<-dfgroups %>% left_join(groupCountries) %>% mutate(colr=rev(getPalette(nrow(dfgroups)))) %>% 
        mutate(x=xmin+(xmax-xmin)/2,y=ymax,width=xmax-xmin,ymid=y/2)
      africaString<-getAfricaList() %>% makeString
      pdfgroups$str<-ifelse(is.na(pdfgroups$str),paste0("The following countries have more\nthan 5 million people but no\nindividual data in the BP\nWorld Energy Statistics\n",africaString),
                            pdfgroups$str)
      pdfgroups<-pdfgroups %>% 
        mutate(str=paste0("Popn: ",comma(Population),"m\n",str),GW=comma(Population*PerCapPower/1000))
      gfig<-plot_ly()
      fb <- list(size = 13, color = "black")
      fw <- list(size = 18, color = "grey")
      fwhite <- list(color = "white")
      fsmall <- list(color = "black",size=10)
      fbig <- list(size = 18)
      etarget=1200
      popm<-worldPop20/1e6
      pop50<-9600
      gfig<-layout(gfig,title=list(
                text="<b>Global per person distribution of electricity 2020</b>",
                y=0.90,x=0.2, xanchor='left',yanchor='top',font=list(size=20)
          ),showlegend=FALSE,
            shapes=list(x0=0,y0=0,x1=pop50,y1=etarget,layer="below",
                        fillcolor="Khaki",type="rect",line=list(width=0)),
            xaxis=list(title="Global Population (millions)"),
            yaxis=list(title="Watts per person (continuous power)")) %>%
            layout(margin=list(t=80)) %>%
            layout(annotations=list(
              text=paste0("Possible scenario... ",etarget," watts per person"),
              hovertext=paste0("With target average global continuous\npower ",
                                etarget," watts per person ...\n",
          "global power required in 2050 with\n",
              comma(pop50/1000)," billion people: ",
              comma(etarget*pop50/1000)," GW"
                                ),align='left',showarrow=TRUE,x=2800,y=etarget,font=fb)) %>%
            layout(annotations=list(
              text=paste0("Data: BP World Energy Stats (Population data from World Bank)"),xanchor='left',x=100,y=60,font=list(color='white'),showarrow=FALSE)) %>%
            layout(annotations=list(
              hovertext=paste("IEA 2050 target is 2.6 times\nthis present electricity output\n",
                              "(",comma(2.6*totalgw)," GW) ",
                              "implying \n",comma(2.6*totalgw/(pop50/1000))," watts/person "
              ),
              text=paste0("Current global power with\n",
              comma(popm/1000)," billion people: ",comma(totalgw)," GW"),font=list(size="12"),
              x=5500,y=300,xanchor='left',showarrow=FALSE))
            
      gfig<-gfig %>% add_trace(data=pdfgroups,type="bar",x=~x,y=~y,width=~width,text=~Region,hovertext=~str,
                       textfont=fbig,
                       hovertemplate="Avg w/Cap: %{y}\n%{hovertext}<extra></extra>",
                       marker=list(color=~colr)) 
      for(i in 1:nrow(pdfgroups)) {
          row<-pdfgroups[i,]
          a<-0
          f<-fsmall
          if (i<6) { 
            a <- -90 
            f<-fwhite
          }
          gfig<-gfig %>% layout(annotations=list(x=row$x,y=row$ymid,font=f,text=paste(row$GW,"GW"),textangle=a,showarrow=FALSE))
      }
      #htmlwidgets::saveWidget(as_widget(gfig), "thorconplot-bygroup.html")
      gfig %>% ggplconfig
    })
    
#    output$globalenergyshortfall<-renderImage(list(src="www/thorcon-pop-energy-new.png",height="400px"),deleteFile=FALSE)
#    output$newglobalenergyshortfall<-renderImage(list(src="www/thorcon-pop-energy-new.png",height="400px"),deleteFile=FALSE)
    output$radimage<-renderImage(list(src="www/processed-meat-tray-packs-and-warnings.jpg",height="230px"),deleteFile=FALSE)
    output$nukehistory<-renderImage(list(src="www/iStock-471776178-swiss-nuke-s.jpg",height="230px"),deleteFile=FALSE)
    output$guilioverne<-renderImage(list(src="www/M88N61-Guilio-Verne-ss.jpg",height="230px"),deleteFile=FALSE)
    output$hvdc<-renderImage(list(src="www/HVDC_Europe_annotated.png",height="400px",class="cntr bShadow"),deleteFile=FALSE)
    output$hvdcland<-renderImage(list(src="www/hitachi-review-vschvdc.jpg",height="400px",class="cntr bShadow"),deleteFile=FALSE)
    output$icctimage<-renderImage(list(src="www/Global-LCA-passenger-cars-jul2021_0.jpg",height="300px"),deleteFile=FALSE)
    output$recyclingimages1<-renderImage(list(src="www/recycling-b.png",height="300px"),deleteFile=FALSE)
    output$recyclingimages2<-renderImage(list(src="www/recycling-with-oil-b.png",height="300px"),deleteFile=FALSE)
    output$recyclingimages3<-renderImage(list(src="www/recycling-with-renewables-b.png",height="300px"),deleteFile=FALSE)
    output$drycasks<-renderImage(list(src="dry-cask-storage.png",height=200),deleteFile=FALSE)
    output$leibstadt<-renderImage(list(src="www/2AD746X-leibstadt-ss.jpg",height=200),deleteFile=FALSE)
    output$solarhills<-renderImage(list(src="www/2GXDW3F-solar-farm-ss.jpg",height=200),deleteFile=FALSE)
    output$windfarmlightning<-renderImage(list(src="2C922X4-windfarm-lightning-s.jpg",height=200),deleteFile=FALSE)
    output$solarwasteimage<-renderImage(list(src="solar-smashed-virgin-islands-s.jpg",height=200),deleteFile=FALSE)
    output$bwasteimage<-renderImage(list(src="E4F1K3-battery-recycling-ss.jpg",height=200),deleteFile=FALSE)
    output$gastanker<-renderImage(list(src="iStock-1282774986-lngtanker.png",height=200),deleteFile=FALSE)
    output$threegorges<-renderImage(list(src="www/TDRN75-three-gorges-s.jpg",height=200),deleteFile=FALSE)
    output$threegorges2<-renderImage(list(src="www/TDRN75-three-gorges-s.jpg",height=200),deleteFile=FALSE)
    output$steelimage<-renderImage(list(src="www/2AWJKDD-steel-making-ss.jpg",height=200),deleteFile=FALSE)
    output$rosatomimage<-renderImage(list(src="www/2CC974N-rosatomreactor-s.jpg",height=200),deleteFile=FALSE)
    output$steelimage2<-renderImage(list(src="www/2AWJKDD-steel-making-ss.jpg",height=200),deleteFile=FALSE)
    output$gaspipe<-renderImage(list(src="iStock-968906216-gas.png",height=200),deleteFile=FALSE)
    output$graphiteminer<-renderImage(list(src="graphite-miner-china.jpg",height=250),deleteFile=FALSE)
    
    output$fctruckimage<-renderImage(list(src="iStock-1341873414-fctruck.jpg",height=200),deleteFile=FALSE)
    output$gfimage<-renderImage(list(src="iStock-1214783080-s.jpg",height=200),deleteFile=FALSE)
    output$supplychains<-renderImage(list(src="supply-chain.jpg",height=200),deleteFile=FALSE)
    output$polyimage<-renderImage(list(src="polysilicon.jpg",height=200),deleteFile=FALSE)
    output$windimage<-renderImage(list(src="wind-turbine-ss.jpg",height=200),deleteFile=FALSE)
    output$kamoto<-renderLeaflet({
         makeMapWithZoom(-10.754798, 25.385542,13)
    })
    output$graphite<-renderLeaflet({
         makeMapWithZoom(-13.306863, 38.655888,15)
    })
    output$watchimage<-renderImage(list(src="h2image.jpg",height=200),deleteFile=FALSE)
    output$wbminerals<-renderImage(list(src="world-bank-minerals-to-2050.jpg",height=300),deleteFile=FALSE)
    output$fukutext2<-renderUI({
      
    })
    output$leafletmapSolarArea<-renderLeaflet({
      df<-dfpvspecs() 
      dfms<- df %>% extractMaxStates("pv")
      haop<-as.numeric(dfms$pvOperational)*input$haPerGW
      makeMapWithCircles(input$lmapname1,as.numeric(haop),c("blue"),6,rfactor=0)
    })
    output$biomassmap<-renderLeaflet({
      #Fukushima ... 5306MW at 80% cf
      # 330000 Smil factor for 1GW at 70% cf 
      makeMapWithCircles("Fukushima",c(39897,330000*(0.8/0.7)*5.3),c("blue","red"),8) 
    })
    output$fukumap<-renderLeaflet({
      # Fukushima ... 5306MW at 80% cf
      makeMapWithOffsetCircle(37.422458,141.026594,39897,0.95,270,zoom=13)
    })
    output$leafletmap1<-renderLeaflet({
      thename<-input$lmapname1
      print(thename)
      lat=(locations %>% filter(name==thename))$lat
      lon=(locations %>% filter(name==thename))$lon
      print(lat)
      #dp<-destPoint(c(lon,lat),d=5641-892,b=90)
      r1=11445   # just one circle of 41000 ha
      r2=r1
      dp<-destPoint(c(lon,lat),d=r1,b=-90)
      lon1=lon; lat1=lat; lon2=lon; lat2=lat;
      if (thename=="Sizewell C" || thename=="Hinkley Point C") {
          dp2<-destPoint(c(lon,lat),d=11445,b=-90)
          lon1=dp2[1]; lat1=dp2[2]; lon2=dp2[1]; lat2=dp2[2];
          r2=11445    # one circle of 41000 ha 
          r1=r2
      }
      if (thename=="Hinkley Point C") {
          r2=11445    # one circle of 41000 ha 
          r1=r2
          dp2<-destPoint(c(lon,lat),d=r1,b=-180)
          lon1=dp2[1]; lat1=dp2[2]; lon2=dp2[1]; lat2=dp2[2];
      }
      print(dp)
      print(str(dp))
      z<-12
      leaflet(options=leafletOptions(minZoom=z-3,maxZoom=z+3)) %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        addTiles(options=tileOptions(opacity=1),group="Outline") %>% 
        setView(lon,lat,zoom=z) %>% 
        addCircles(lng=lon1,lat=lat1,fillOpacity=0.1,radius=r1) %>%
        addCircles(lng=lon2,lat=lat2,fillOpacity=0.1,radius=r2) %>%
        # next adds a single Nyngan sized circle
        # addCircles(lng=dp[1],lat=dp[2],fillOpacity=0.2,color="red",radius=892) %>%
        addMarkers(lng=lon,lat=lat,label=thename) %>%
        addLayersControl(
          baseGroups = c("Satellite","Outline"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addMeasure(primaryLengthUnit="metres",primaryAreaUnit = "hectares") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
    })
    output$landuseaus<-renderPlotly({
        df<-dflu %>% mutate(MillionHa=Area*100/1e6)
        plu<-ggplot(df)+geom_bar(aes(x=Category,y=MillionHa,fill=Color),stat="identity")+coord_flip()+
          ylab("Area (million hectares)")+
        xlab("")+scale_fill_manual(values=c("red","black","lightgreen","purple","darkgreen","grey"))+
          guides(fill="none")+labs(title="Land use in Australia I")+
        theme(
          plot.title=element_text(size=11,hjust=0,vjust=0,face='bold'),
          axis.text.x=element_text(size=11,hjust=0,vjust=0,face='bold'),
          axis.text.y=element_text(size=11,hjust=0,vjust=0,face='bold')
              )
          ggplotly(plu) %>% ggplconfig
    })
    output$landuseaus2<-renderPlotly({
        df<-dflu %>% mutate(MillionHa=Area*100/1e6) %>% filter(Area<3000000)
        plu<-ggplot(df)+geom_bar(aes(x=Category,y=MillionHa,fill=Color),stat="identity")+coord_flip()+ylab("Area (million hectares)")+
        xlab("")+scale_fill_manual(values=c("red","black","lightgreen","purple","darkgreen","grey"))+
          guides(fill="none")+labs(title="Land use in Australia II")+
        theme(
          plot.title=element_text(size=11,hjust=0,vjust=0,face='bold'),
          axis.text.x=element_text(size=11,hjust=0,vjust=0,face='bold'),
          axis.text.y=element_text(size=11,hjust=0,vjust=0,face='bold')
              )
          ggplotly(plu) %>% ggplconfig
    })
    output$Pllng2<-renderPlotly({
      df<-lngex %>% mutate(year=as.numeric(Year),mtlng=BCM/1.379,AREHs=ElToMakeH2TWh/arehElTWh,str=paste0(Country,"\n",comma(BCM)," BCM\n",comma(mtlng)," mt LNG\n",comma(AREHs)," AREH")) 
      print(df) 
      fig<-plot_ly()
      fig<-layout(fig,title="Top dozen LNG exporters",yaxis=list(title="Number of AREHs required"))
      fig<-fig %>% add_trace(data=df,type="scatter",mode="lines",x=~Year,y=~AREHs,
                             color=~Country,hovertext=~str,hovertemplate="%{hovertext}")
      fig %>% ggplconfig
    })
#    output$Pllng<-renderPlotly({
#     p<-lngex %>% mutate(year=as.numeric(Year),AREHs=ElToMakeH2TWh/arehElTWh) %>% ggplot() + geom_line(aes(x=year,y=AREHs,color=Country,group=Country),size=0.7)+
#       labs(x="",y="Number of AREHs required",title="Top dozen LNG exporters")
#     ggplotly(p) %>% ggplconfig
#   })
    output$lngtotals<-renderUI({
          markdown(paste0(startCard(),"The total number of AREHs that need to be built to generate green hydrogen to replace the current global LNG export totals is ",
                 comma(as.numeric(totalAREHs)), ". This is for all LNG exports, not just the top dozen we featured in the graphs. ",
                 " In comparison the total amount of uranium required for thermal equivalence would be ",
                 comma(as.numeric(totalUranium))," tonnes. ",endCard()
                 ))
      
    })
    output$pvtonnagedata<-renderPlotly({
      df<-read_csv("solarspecs.csv") %>% mutate(name=paste0(Company,"-",Model),watts=`NCOT Watts`,tPerGW=(1e9/watts*Kg)/1000)
      p<-df %>% ggplot()+geom_col(aes(x=name,y=tPerGW),fill="red")+
        labs(title="Various models of panels",x="",y="Tonnes per GW")+coord_flip()+
                theme(plot.title=element_text(hjust=0),
                      axis.text.y=element_text(size=10))
      ggplotly(p) %>% ggplconfig
      
      
    })
    output$PllngU<-renderPlotly({
      p<-lngex %>% mutate(year=as.numeric(Year)) %>% ggplot() + geom_line(aes(x=year,y=Uranium,color=Country,group=Country),size=0.7)+
        labs(x="",y="Replacement tonnes of Uranium ",title="Top dozen LNG exporters")
      ggplotly(p) %>% ggplconfig
    })
    output$growthtable2<-renderImage(list(src="www/rollout-top30-over-10-years-SWHNBS.png",height="500px",class="bShadow cntr"),deleteFile=FALSE)
    output$growthtable<-renderImage(list(src="www/rollout-top30-over-10-years-GCOSWHNBS.png",height="500px",class="bShadow cntr"),deleteFile=FALSE)
    output$ausfrdeu<-renderPlotly({
        dfc %>% plot_ly(x=~Year,y=~pce) %>% group_by(Country) %>% 
        add_lines(color=~ordered(Country)) %>% 
          layout(title="Electricity generation emissions", yaxis=list(title="Tonnes of carbon dioxide per person per year")) %>%
          add_annotations(x=1995,y=1.5,text="Data: UNFCCC National Submissions 2020",showarrow=F) %>% ggplconfig
      
    })
    output$leafletmap2<-renderLeaflet({
      thename<-input$lmapname2
      print(thename)
      lat=(locations %>% filter(name==thename))$lat
      lon=(locations %>% filter(name==thename))$lon
      #print(lat)
      dp<-destPoint(c(lon,lat),d=5641-892,b=90)
      #print(dp)
      z<-12
      leaflet(options=leafletOptions(minZoom=z-2,maxZoom=z+1)) %>% 
        addProviderTiles(options=providerTileOptions(opacity=1),providers$Esri.WorldImagery,group="Satellite") %>% 
        addTiles(options=tileOptions(opacity=1),group="Outline") %>% 
        setView(lon,lat,zoom=z) %>% 
        addCircles(lng=lon,lat=lat,fillOpacity=0.1,radius=5641) %>%
        # next adds a single Nyngan sized circle
        # addCircles(lng=dp[1],lat=dp[2],fillOpacity=0.2,color="red",radius=892) %>%
        addMarkers(lng=lon,lat=lat,label=thename) %>%
        addLayersControl(
          baseGroups = c("Satellite","Outline"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addMeasure(primaryLengthUnit="metres",primaryAreaUnit = "hectares") %>% addScaleBar(position="bottomright",options=scaleBarOptions(metric=TRUE,imperial=FALSE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
