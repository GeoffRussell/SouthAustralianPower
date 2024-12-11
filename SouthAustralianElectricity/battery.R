
  batteryMaxCapacity<-bmax
  #-------------------
  # start the battery full 
  #-------------------
  batteryStatus<-bmax
  nperiods<-length(dfsum$demand)
  dfsum$shortFall=rep(0,nperiods)
  dfsum$netflow=sum(dfsum$flow/12)
  dfsum$batteryStatus=rep(0,nperiods)
  dfsum$throwOut=rep(0,nperiods)
  lastdfsum<<-dfsum
  totalBattuse<<-sum(lastdfsum$battuse/12)
  maxNeed<-0
  for(i in 1:nperiods) {
    dfsum$shortFall[i]=0
    # spareE is in MWh
    spareE=(dfsum$dblrenew[i]-dfsum$demand[i])/12 
    if (spareE>0) {  # Electricity exceeds demand ... add spare to battery if there is any capacity
      if (batteryStatus<batteryMaxCapacity) { # battery isn't full
        spareB=batteryMaxCapacity-batteryStatus
        addE=min(spareE,spareB)
        batteryStatus=batteryStatus+addE
        leftOver=spareE-spareB
        if (leftOver>0) {
          dfsum$throwOut[i]=leftOver
        }
      }
      else {   # battery is full, discard energy
        dfsum$throwOut[i]=spareE
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
        }
        else { # we have some in battery, but not enough
          dfsum$shortFall[i]=needE-batteryStatus
          batteryStatus=0
        }
      }
      else { # battery empty
          dfsum$shortFall[i]=needE
      }
    }
    dfsum$batteryStatus[i]=batteryStatus
  }
  #---------------------------------------------------------
  # End of battery stuff
  #---------------------------------------------------------
