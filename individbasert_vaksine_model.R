# Individbasert vaksinemodell

  rm(list=ls())
  
  library(ggplot2)
  library(reshape)
  library(dplyr)
  library(scales)
  library(R0)
  source("h:/data/R/Rt_funcs.R")
  library(openxlsx)
  
for(vaks in 1:1)
{ 
  seed.var <- 1234
  set.seed(seed.var)
  
  beregn.R.R0.package <- TRUE
  start.etter.vaks.eldre <- FALSE
  start.etter.vaks.risiko <- TRUE


  vaksine.strategi <- vaks
  
  # Paramtere
  tid.inkubasjon <- 2
  tid.smittsom <- 5
  tid.til.vaksine <- 14
  
  n.sim <- 100
  n.dager <- 200
  daglige.vaksiner <- 750
  n.vaksiner <- 110000
  start.dato <- as.Date("2020/05/01")
  datoer <- seq(start.dato, (start.dato + n.dager), by  = "day")
  
  
  andel.ukjent.friske <- 0.3
  tid.symp.innl <- 9
  liggetid <- 6
  
  vaksine.dekning.yngre <- 0.7
  vaksine.dekning.eldre <- 0.8
  vaksine.dekning.eldre50t59 <- 0.7
  vaksine.dekning.eldre40t49 <- 0.66
  vaksine.dekning.eldre30t39 <- 0.57
  vaksine.dekning.eldre18t29 <- 0.7


  
  n0t9 <- 48909
  n10t19 <- 48199
  n20t29 <- 48296
  n30t39 <- 54740
  n40t49 <- 51135
  n50t59 <- 46595
  n60t69 <- 35913
  n70t79 <- 23849
  n80p <- 12485
  
  n.r0t9 <- 3228
  n.r10t19 <- 3470
  n.r20t29 <- 3204
  n.r30t39 <- 4270
  n.r40t49 <- 6136
  n.r50t59 <- 9319
  n.r60t69 <- 11492
  n.r70t79 <- 11448
  n.r80p <- 7616
  
  k.imp <- 0.7
  
  sim.data <- c()
  for(sim in 1:n.sim)
  {
    
   
    #Import
    n.snitt.daglig.import <- 0
    Import.verdier <- rnbinom(n.dager+1, s=k.imp, m=n.snitt.daglig.import)
    #Import.sannsynlighet <- c(0.2,0.4,1,1,1,1,0.8,0.4,0.2)
    
    # Velg vaksineringsstrategi:
    if(vaksine.strategi == 1)
    {
      vaksineringsrekkefølge <- c('60-69år',
                                  '50-59år',
                                  '40-49år',
                                  '30-39år',
                                  '20-29år',
                                  '10-19år')
      # '80+',
      # '70-79år',
      # '60-69år')  #'0-9år',
      vaksinegruppe.alder <- c(n60t69,n50t59,n40t49,n30t39,n20t29,n10t19)
      vaksine.dekning.strategi <- c(vaksine.dekning.eldre, vaksine.dekning.eldre50t59,vaksine.dekning.eldre40t49,
                                    vaksine.dekning.eldre30t39,vaksine.dekning.eldre18t29,vaksine.dekning.eldre18t29*0.2 )
    }
    
  
    if(vaksine.strategi >= 2)
    {
      vaksineringsrekkefølge <- c('20-29år',
                                  '10-19år',
                                  '30-39år',
                                  '40-49år',
                                  '50-59år',
                                  '60-69år')
      vaksinegruppe.alder <- c(n20t29,n10t19,n30t39,n40t49,n50t59,n60t69)
      vaksine.dekning.strategi <- c(vaksine.dekning.eldre18t29,vaksine.dekning.eldre18t29*0.2,
                                    vaksine.dekning.eldre30t39 ,vaksine.dekning.eldre40t49,vaksine.dekning.eldre50t59,vaksine.dekning.eldre)
      if(vaksine.strategi == 3)
      {
        vaksine.dekning.strategi <- c(vaksine.dekning.eldre18t29,vaksine.dekning.eldre18t29*0.4,
                                      vaksine.dekning.eldre30t39 ,vaksine.dekning.eldre40t49,vaksine.dekning.eldre50t59,vaksine.dekning.eldre)
      }
      else if(vaksine.strategi == 4)
      {
        vaksine.dekning.strategi <- c(vaksine.dekning.eldre18t29,vaksine.dekning.eldre18t29*0.8,
                                      vaksine.dekning.eldre30t39 ,vaksine.dekning.eldre40t49,vaksine.dekning.eldre50t59,vaksine.dekning.eldre)
      }
    }

    
    #Initier modellen
    n.smittede <- 10
    n.smittsomme <- 15
    n.friske <- 5000
    n.friske.ukjent <- round(n.friske*andel.ukjent.friske) #Lagt inn som en test. Ikke relevant.
    
    k <- 0.43
    #k.hi <- 2.5 #dispersjonsfaktoren Skal være lik?
    k.imp <- 0.7
    
    # definer og generer populasjon
    alders.grupper <- c('0-9år','10-19år','20-29år','30-39år','40-49år','50-59år','60-69år','70-79år','80+')
    R.verdier.snitt <- c(0.496,1.281,1.640, 1.301, 1.181, 0.948, 0.585, 0.366, 0.446)*1.5 #Basert på MSIS tall okt. + nov.
    R.Verdier <- c(rnbinom(n0t9, s=k, m=R.verdier.snitt[1]), 
                   rnbinom(n10t19, s=k, m=R.verdier.snitt[2]),
                   rnbinom(n20t29, s=k, m=R.verdier.snitt[3]), 
                   rnbinom(n30t39, s=k, m=R.verdier.snitt[4]), 
                   rnbinom(n40t49, s=k, m=R.verdier.snitt[5]), 
                   rnbinom(n50t59, s=k, m=R.verdier.snitt[6]), 
                   rnbinom(n60t69, s=k, m=R.verdier.snitt[7]), 
                   rnbinom(n70t79, s=k, m=R.verdier.snitt[8]), 
                   rnbinom(n80p, s=k, m=R.verdier.snitt[9]))
    
    Innleggelses.prosenter.snitt <- c(0.001,0.001,0.005,0.011,0.014,0.029,0.058,0.093,0.223)#*1000
    Innleggelses.prosenter.risk <- c(0.002,0.002,0.012,0.026,0.031,0.058,0.1,0.14,0.30)
    Innleggelses.prosenter.ikke.risk <- c(0.0009,0.0009,0.0045,0.0097,0.0117,0.0218,0.0384,0.0492,0.1029)
  
    Befolkning.per.alder <- c(n0t9,n10t19,n20t29,n30t39,n40t49,n50t59,n60t69,n70t79,n80p)
    Befolkning.risiko.per.alder<- c(n.r0t9,n.r10t19,n.r20t29,n.r30t39,n.r40t49,n.r50t59,n.r60t69,n.r70t79,n.r80p)
    Befolkning.ikke.risiko.per.alder <- Befolkning.per.alder - Befolkning.risiko.per.alder
    
    total.befolkning <- sum(Befolkning.per.alder)
    n.ikke.S <- n.smittede + n.smittsomme + n.friske.ukjent + n.friske
    
    
    
      initier.befolkning <- function()
      {
        befolkning.alder <- c(rep(alders.grupper, times = Befolkning.per.alder))
        befolkning.R <- R.Verdier #c(rep(R.verdier, times = Befolkning.per.alder))
        befolkning.R.snitt <- c(rep(R.verdier.snitt, times = Befolkning.per.alder))
        
        befolkning.innleggelses.prosent <- c(rep(Innleggelses.prosenter.risk[1], times = Befolkning.risiko.per.alder[1]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[1], times = Befolkning.ikke.risiko.per.alder[1]),
                                             rep(Innleggelses.prosenter.risk[2], times = Befolkning.risiko.per.alder[2]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[2], times = Befolkning.ikke.risiko.per.alder[2]),
                                             rep(Innleggelses.prosenter.risk[3], times = Befolkning.risiko.per.alder[3]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[3], times = Befolkning.ikke.risiko.per.alder[3]),
                                             rep(Innleggelses.prosenter.risk[4], times = Befolkning.risiko.per.alder[4]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[4], times = Befolkning.ikke.risiko.per.alder[4]),
                                             rep(Innleggelses.prosenter.risk[5], times = Befolkning.risiko.per.alder[5]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[5], times = Befolkning.ikke.risiko.per.alder[5]),
                                             rep(Innleggelses.prosenter.risk[6], times = Befolkning.risiko.per.alder[6]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[6], times = Befolkning.ikke.risiko.per.alder[6]),
                                             rep(Innleggelses.prosenter.risk[7], times = Befolkning.risiko.per.alder[7]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[7], times = Befolkning.ikke.risiko.per.alder[7]),
                                             rep(Innleggelses.prosenter.risk[8], times = Befolkning.risiko.per.alder[8]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[8], times = Befolkning.ikke.risiko.per.alder[8]),
                                             rep(Innleggelses.prosenter.risk[9], times = Befolkning.risiko.per.alder[9]) , 
                                             rep(Innleggelses.prosenter.ikke.risk[9], times = Befolkning.ikke.risiko.per.alder[9])
                                             )
        
        boks <- c(rep("S", total.befolkning))
        dager.i.tilstand <- c(rep(0, total.befolkning))
        risiko.gruppe <-  c(rep(1, times = Befolkning.risiko.per.alder[1]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[1]),
                            rep(1, times = Befolkning.risiko.per.alder[2]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[2]),
                            rep(1, times = Befolkning.risiko.per.alder[3]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[3]),
                            rep(1, times = Befolkning.risiko.per.alder[4]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[4]),
                            rep(1, times = Befolkning.risiko.per.alder[5]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[5]),
                            rep(1, times = Befolkning.risiko.per.alder[6]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[6]),
                            rep(1, times = Befolkning.risiko.per.alder[7]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[7]),
                            rep(1, times = Befolkning.risiko.per.alder[8]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[8]),
                            rep(1, times = Befolkning.risiko.per.alder[9]) , 
                            rep(0, times = Befolkning.ikke.risiko.per.alder[9])
        )
        
        
        df.befolkning <- data.frame(aldersgruppe = befolkning.alder,
                                    R = befolkning.R, 
                                    R.snitt = befolkning.R.snitt,
                                    innleggelsesprosent = befolkning.innleggelses.prosent,
                                    boks = boks,
                                    'dag i tilstand' = dager.i.tilstand,
                                    risikogruppe = risiko.gruppe,
                                    stringsAsFactors=FALSE)
        
        ikke.S <- sample(total.befolkning, n.ikke.S, replace = FALSE ,prob = df.befolkning$R.snitt)
        
        df.befolkning$boks[ikke.S[1:n.smittede]] <- "E"
        df.befolkning$dag.i.tilstand[ikke.S[1:n.smittede]] <- sample(1:tid.inkubasjon,n.smittede, replace = TRUE)
        
        df.befolkning$boks[ikke.S[(n.smittede + n.smittsomme +1):(n.smittede + n.smittsomme + n.friske)]] <- "R"
        df.befolkning$boks[ikke.S[(n.smittede + n.smittsomme + 1 + n.friske):n.ikke.S]] <- "RU"
        
        df.befolkning$boks[ikke.S[(n.smittede + 1):(n.smittede + n.smittsomme)]] <- "I"
        df.befolkning$dag.i.tilstand[ikke.S[(n.smittede + 1):(n.smittede + n.smittsomme)]] <- sample(1:tid.smittsom,n.smittsomme, replace = TRUE)
        
        if(start.etter.vaks.eldre)
        {
          
          n.vaksiners80 <- round(Befolkning.per.alder[9]*vaksine.dekning.eldre)
          n.vaksiners70 <- round(Befolkning.per.alder[8]*vaksine.dekning.eldre)
          n.vaksiners60 <- round(Befolkning.per.alder[7]*vaksine.dekning.eldre)
          
          index.vaks.80 <- sample(which(df.befolkning$aldersgruppe == "80+"), n.vaksiners80)
          index.vaks.70 <- sample(which(df.befolkning$aldersgruppe == "70-79år"), n.vaksiners70)
          index.vaks.60 <- sample(which(df.befolkning$aldersgruppe == "60-69år"), n.vaksiners60)
          
          df.befolkning$boks[index.vaks.80] <- "RV"
          df.befolkning$boks[index.vaks.70] <- "RV"
          df.befolkning$boks[index.vaks.60] <- "RV"
          
          
        }
        
        if(start.etter.vaks.risiko)
        {
          #Estimer antall vaksiner som skal settes
          #Risikogruppen
          n.r.vaksiners80 <- round(Befolkning.risiko.per.alder[9]*vaksine.dekning.eldre)
          n.r.vaksiners70 <- round(Befolkning.risiko.per.alder[8]*vaksine.dekning.eldre)
          n.r.vaksiners60 <- round(Befolkning.risiko.per.alder[7]*vaksine.dekning.eldre)
          n.r.vaksiners50 <- round(Befolkning.risiko.per.alder[6]*vaksine.dekning.eldre50t59)
          n.r.vaksiners40 <- round(Befolkning.risiko.per.alder[5]*vaksine.dekning.eldre40t49)
          n.r.vaksiners30 <- round(Befolkning.risiko.per.alder[4]*vaksine.dekning.eldre30t39)
          n.r.vaksiners20 <- round(Befolkning.risiko.per.alder[3]*vaksine.dekning.eldre18t29)
          if (vaksine.strategi <= 2)
          {
            n.r.vaksiners10 <- round(Befolkning.risiko.per.alder[2]*vaksine.dekning.eldre18t29*0.2)
          }
          else if (vaksine.strategi == 3)
          {
            n.r.vaksiners10 <- round(Befolkning.risiko.per.alder[2]*vaksine.dekning.eldre18t29*0.4) #vaksiner t.o.m. 16 år
          }
          else if (vaksine.strategi == 4)
          {
            n.r.vaksiners10 <- round(Befolkning.risiko.per.alder[2]*vaksine.dekning.eldre18t29*0.8) #vaksiner t.o.m. 12 år
          }
          
          #Ikke risikogruppen
          n.ir.vaksiners80 <- round(Befolkning.ikke.risiko.per.alder[9]*vaksine.dekning.eldre)
          n.ir.vaksiners70 <- round(Befolkning.ikke.risiko.per.alder[8]*vaksine.dekning.eldre)
          n.ir.vaksiners60 <- round(Befolkning.ikke.risiko.per.alder[7]*0.4)
          n.ir.vaksiners50 <- round(Befolkning.ikke.risiko.per.alder[6]*0.01)
          n.ir.vaksiners40 <- round(Befolkning.ikke.risiko.per.alder[5]*0.01)
          n.ir.vaksiners30 <- round(Befolkning.ikke.risiko.per.alder[4]*0.01)
          n.ir.vaksiners20 <- round(Befolkning.ikke.risiko.per.alder[3]*0.01)
          n.ir.vaksiners10 <- round(Befolkning.ikke.risiko.per.alder[2]*0)
          
          #Hent indekser
          index.r.vaks.80 <- sample(which(df.befolkning$aldersgruppe == "80+" & df.befolkning$risikogruppe == 1), n.r.vaksiners80)
          index.r.vaks.70 <- sample(which(df.befolkning$aldersgruppe == "70-79år"& df.befolkning$risikogruppe == 1), n.r.vaksiners70)
          index.r.vaks.60 <- sample(which(df.befolkning$aldersgruppe == "60-69år"& df.befolkning$risikogruppe == 1), n.r.vaksiners60)
          index.r.vaks.50 <- sample(which(df.befolkning$aldersgruppe == "50-59år"& df.befolkning$risikogruppe == 1), n.r.vaksiners50)
          index.r.vaks.40 <- sample(which(df.befolkning$aldersgruppe == "40-49år"& df.befolkning$risikogruppe == 1), n.r.vaksiners40)
          index.r.vaks.30 <- sample(which(df.befolkning$aldersgruppe == "30-39år"& df.befolkning$risikogruppe == 1), n.r.vaksiners30)
          index.r.vaks.20 <- sample(which(df.befolkning$aldersgruppe == "20-29år"& df.befolkning$risikogruppe == 1), n.r.vaksiners20)
          index.r.vaks.10 <- sample(which(df.befolkning$aldersgruppe == "10-19år"& df.befolkning$risikogruppe == 1), n.r.vaksiners10)
          #index.r.vaks.10 <- sample(which(df.befolkning$aldersgruppe == "60-69år"& df.befolkning$risikogruppe == 1), n.r.vaksiners60)
          
          #Kan vaksinere de som er i R, RU, E og I
          index.ir.vaks.80 <- sample(which(df.befolkning$aldersgruppe == "80+" & df.befolkning$risikogruppe == 0), n.ir.vaksiners80)
          index.ir.vaks.70 <- sample(which(df.befolkning$aldersgruppe == "70-79år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners70)
          index.ir.vaks.60 <- sample(which(df.befolkning$aldersgruppe == "60-69år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners60)
          index.ir.vaks.50 <- sample(which(df.befolkning$aldersgruppe == "50-59år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners50)
          index.ir.vaks.40 <- sample(which(df.befolkning$aldersgruppe == "40-49år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners40)
          index.ir.vaks.30 <- sample(which(df.befolkning$aldersgruppe == "30-39år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners30)
          index.ir.vaks.20 <- sample(which(df.befolkning$aldersgruppe == "20-29år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners20)
          index.ir.vaks.10 <- sample(which(df.befolkning$aldersgruppe == "10-19år"& df.befolkning$risikogruppe == 0), n.ir.vaksiners10)
          
          #Vaksiner:
          df.befolkning$boks[index.r.vaks.80] <- "RV"
          df.befolkning$boks[index.r.vaks.70] <- "RV"
          df.befolkning$boks[index.r.vaks.60] <- "RV"
          df.befolkning$boks[index.r.vaks.50] <- "RV"
          df.befolkning$boks[index.r.vaks.40] <- "RV"
          df.befolkning$boks[index.r.vaks.30] <- "RV"
          df.befolkning$boks[index.r.vaks.20] <- "RV"
          df.befolkning$boks[index.r.vaks.10] <- "RV"
          df.befolkning$boks[index.ir.vaks.80] <- "RV"
          df.befolkning$boks[index.ir.vaks.70] <- "RV"
          df.befolkning$boks[index.ir.vaks.60] <- "RV"
          df.befolkning$boks[index.ir.vaks.50] <- "RV"
          df.befolkning$boks[index.ir.vaks.40] <- "RV"
          df.befolkning$boks[index.ir.vaks.30] <- "RV"
          df.befolkning$boks[index.ir.vaks.20] <- "RV"
          df.befolkning$boks[index.ir.vaks.10] <- "RV"

          
        }
        
        return(df.befolkning)
      }
    
      df.befolkning <- initier.befolkning()
    
      initier.simulering <- function(df.befolkning, total.befolkning, n.smittede, n.smittsomme, n.friske,n.friske.ukjent)
      {
      
      S0 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '0-9år'))
      S10 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '10-19år'))
      S20 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '20-29år'))
      S30 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '30-39år'))
      S40 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '40-49år'))
      S50 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '50-59år'))
      S60 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '60-69år'))
      S70 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '70-79år'))
      S80 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '80+'))
      S = S0+S10+S20+S30+S40+S50+S60+S70+S80
      
      
      E0 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '0-9år'))
      E10 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '10-19år'))
      E20 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '20-29år'))
      E30 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '30-39år'))
      E40 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '40-49år'))
      E50 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '50-59år'))
      E60 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '60-69år'))
      E70 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '70-79år'))
      E80 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '80+'))
      E = E0 + E10+ E20+ E30+ E40+ E50+ E60+ E70+ E80
      
      
      
      I0 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '0-9år'))
      I10 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '10-19år'))
      I20 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '20-29år'))
      I30 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '30-39år'))
      I40 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '40-49år'))
      I50 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '50-59år'))
      I60 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '60-69år'))
      I70 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '70-79år'))
      I80 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '80+'))
      I = I0 + I10+ I20+ I30+ I40+ I50+ I60+ I70+ I80
      
      R0 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '0-9år'))
      R10 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '10-19år'))
      R20 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '20-29år'))
      R30 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '30-39år'))
      R40 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '40-49år'))
      R50 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '50-59år'))
      R60 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '60-69år'))
      R70 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '70-79år'))
      R80 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '80+'))
      R = R0 + R10 + R20 + R30+R40+R50+R60+R70+R80
      
      RV = 0
      RV0 = 0
      RV10 = 0
      RV20 = 0
      RV30 = 0
      RV40 = 0
      RV50 = 0
      RV60 = 0
      RV70 = 0
      RV80 = 0
      
     
      RU0 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '0-9år'))
      RU10 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '10-19år'))
      RU20 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '20-29år'))
      RU30 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '30-39år'))
      RU40 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '40-49år'))
      RU50 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '50-59år'))
      RU60 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '60-69år'))
      RU70 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '70-79år'))
      RU80 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '80+'))
      RU = RU0+RU10+RU20+RU30+RU40+RU50+RU60+RU70+RU80
      
      simulering <- matrix(c(S,S0,S10,S20,S30,S40,S50,S60,S70,S80,
                             0,0,0,0,0,0,0,0,0,0,
                             E,E0,E10,E20,E30,E40,E50,E60,E70,E80,
                             0,0,0,0,0,0,0,0,0,0,
                             I,I0,I10,I20,I30,I40,I50,I60,I70,I80,
                             0,0,0,0,0,0,0,0,0,0,
                             R,R0,R10,R20,R30,R40,R50,R60,R70,R80,
                             0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,
                             RU,RU0,RU10,RU20,RU30,RU40,RU50,RU60,RU70,RU80,
                             0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0), nrow = 1)
      
      
      #colnames(simulering) <- c("S","dS", "E", "dE", "I", "dI", "R", "dR")
      colnames(simulering) <- c("S","S 0-9år","S 10-19år","S 20-29år","S 30-39år","S 40-49år","S 50-59år","S 60-69år","S 70-79år","S 80år +",
                                "dS","dS 0-9år","dS 10-19år","dS 20-29år","dS 30-39år","dS 40-49år","dS 50-59år","dS 60-69år","dS 70-79år","dS 80år +",
                                "E","E 0-9år","E 10-19år","E 20-29år","E 30-39år","E 40-49år","E 50-59år","E 60-69år","E 70-79år","E 80år +",
                                "dE","dE 0-9år","dE 10-19år","dE 20-29år","dE 30-39år","dE 40-49år","dE 50-59år","dE 60-69år","dE 70-79år","dE 80år +",
                                "I","I 0-9år","I 10-19år","I 20-29år","I 30-39år","I 40-49år","I 50-59år","I 60-69år","I 70-79år","I 80år +",
                                "dI", "dI 0-9år","dI 10-19år","dI 20-29år","dI 30-39år","dI 40-49år","dI 50-59år","dI 60-69år","dI 70-79år","dI 80år +",
                                "R","R 0-9år","R 10-19år","R 20-29år","R 30-39år","R 40-49år","R 50-59år","R 60-69år","R 70-79år","R 80år +",
                                "dR","dR 0-9år","dR 10-19år","dR 20-29år","dR 30-39år","dR 40-49år","dR 50-59år","dR 60-69år","dR 70-79år","dR 80år +",
                                "RV","RV 0-9år","RV 10-19år","RV 20-29år","RV 30-39år","RV 40-49år","RV 50-59år","RV 60-69år","RV 70-79år","RV 80år +",
                                "dRV","dRV 0-9år","dRV 10-19år","dRV 20-29år","dRV 30-39år","dRV 40-49år","dRV 50-59år","dRV 60-69år","dRV 70-79år","dRV 80år +",
                                "RU","RU 0-9år","RU 10-19år","RU 20-29år","RU 30-39år","RU 40-49år","RU 50-59år","RU 60-69år","RU 70-79år","RU 80år +",
                                "dRU","dRU 0-9år","dRU 10-19år","dRU 20-29år","dRU 30-39år","dRU 40-49år","dRU 50-59år","dRU 60-69år","dRU 70-79år","dRU 80år +",
                                "Hi","Hi 0-9år","Hi 10-19år","Hi 20-29år","Hi 30-39år","Hi 40-49år","Hi 50-59år","Hi 60-69år","Hi 70-79år","Hi 80år +",
                                "dHi","dHi 0-9år","dHi 10-19år","dHi 20-29år","dHi 30-39år","dHi 40-49år","dHi 50-59år","dHi 60-69år","dHi 70-79år","dHi 80år +")
      
      
      return(simulering)
      }
    
      oppdater.simulering <- function(df.befolkning, de.som.smittes,index.blir.syke, index.som.vaksineres, index.blir.friske,  index.blir.friske.ukjent, index.blir.friske.kjent,index.blir.lagtinn, index.blir.utskrevet, total.befolkning)
      {
        
        dS <- - length(de.som.smittes)
        dS0 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '0-9år'))
        dS10 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '10-19år'))
        dS20 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '20-29år'))
        dS30 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '30-39år'))
        dS40 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '40-49år'))
        dS50 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '50-59år'))
        dS60 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '60-69år'))
        dS70 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '70-79år'))
        dS80 <- - length(which(df.befolkning$aldersgruppe[de.som.smittes] == '80+'))
        
        dE <-  length(de.som.smittes)
        dE0 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '0-9år'))
        dE10 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '10-19år'))
        dE20 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '20-29år'))
        dE30 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '30-39år'))
        dE40 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '40-49år'))
        dE50 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '50-59år'))
        dE60 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '60-69år'))
        dE70 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '70-79år'))
        dE80 <-  length(which(df.befolkning$aldersgruppe[de.som.smittes] == '80+'))
        
        dI <-  length(index.blir.syke)
        dI0 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '0-9år'))
        dI10 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '10-19år'))
        dI20 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '20-29år'))
        dI30 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '30-39år'))
        dI40 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '40-49år'))
        dI50 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '50-59år'))
        dI60 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '60-69år'))
        dI70 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '70-79år'))
        dI80 <-  length(which(df.befolkning$aldersgruppe[index.blir.syke] == '80+'))
        
        dHI <-  length(index.blir.lagtinn)
        dHI0 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '0-9år'))
        dHI10 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '10-19år'))
        dHI20 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '20-29år'))
        dHI30 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '30-39år'))
        dHI40 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '40-49år'))
        dHI50 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '50-59år'))
        dHI60 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '60-69år'))
        dHI70 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '70-79år'))
        dHI80 <-  length(which(df.befolkning$aldersgruppe[index.blir.lagtinn] == '80+'))
        
        dR <-  length(c(index.blir.friske.kjent,index.blir.utskrevet))
        dR0 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '0-9år'))
        dR10 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '10-19år'))
        dR20 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '20-29år'))
        dR30 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '30-39år'))
        dR40 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '40-49år'))
        dR50 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '50-59år'))
        dR60 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '60-69år'))
        dR70 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '70-79år'))
        dR80 <-  length(which(df.befolkning$aldersgruppe[c(index.blir.friske.kjent,index.blir.utskrevet)] == '80+'))
        
        dRV <-  length(index.blir.friske)
        dRV0 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '0-9år'))
        dRV10 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '10-19år'))
        dRV20 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '20-29år'))
        dRV30 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '30-39år'))
        dRV40 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '40-49år'))
        dRV50 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '50-59år'))
        dRV60 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '60-69år'))
        dRV70 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '70-79år'))
        dRV80 <-  length(which(df.befolkning$aldersgruppe[index.som.vaksineres] == '80+'))
        
        dRU <-  length(index.blir.friske.ukjent)
        dRU0 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '0-9år'))
        dRU10 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '10-19år'))
        dRU20 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '20-29år'))
        dRU30 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '30-39år'))
        dRU40 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '40-49år'))
        dRU50 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '50-59år'))
        dRU60 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '60-69år'))
        dRU70 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '70-79år'))
        dRU80 <-  length(which(df.befolkning$aldersgruppe[index.blir.friske.ukjent] == '80+'))
        
        S = length(which(df.befolkning$boks == "S"))
        S0 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '0-9år'))
        S10 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '10-19år'))
        S20 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '20-29år'))
        S30 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '30-39år'))
        S40 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '40-49år'))
        S50 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '50-59år'))
        S60 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '60-69år'))
        S70 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '70-79år'))
        S80 = length(which(df.befolkning$boks == "S" & df.befolkning$aldersgruppe == '80+'))
        
        E = length(which(df.befolkning$boks == "E"))
        E0 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '0-9år'))
        E10 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '10-19år'))
        E20 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '20-29år'))
        E30 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '30-39år'))
        E40 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '40-49år'))
        E50 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '50-59år'))
        E60 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '60-69år'))
        E70 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '70-79år'))
        E80 = length(which(df.befolkning$boks == "E" & df.befolkning$aldersgruppe == '80+'))
        
        I = length(which(df.befolkning$boks == "I"))
        I0 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '0-9år'))
        I10 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '10-19år'))
        I20 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '20-29år'))
        I30 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '30-39år'))
        I40 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '40-49år'))
        I50 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '50-59år'))
        I60 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '60-69år'))
        I70 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '70-79år'))
        I80 = length(which(df.befolkning$boks == "I" & df.befolkning$aldersgruppe == '80+'))
        
        HI = length(which(df.befolkning$boks == "Hi"))
        HI0 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '0-9år'))
        HI10 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '10-19år'))
        HI20 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '20-29år'))
        HI30 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '30-39år'))
        HI40 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '40-49år'))
        HI50 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '50-59år'))
        HI60 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '60-69år'))
        HI70 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '70-79år'))
        HI80 = length(which(df.befolkning$boks == "Hi" & df.befolkning$aldersgruppe == '80+'))
        
        R = length(which(df.befolkning$boks == "R"))
        R0 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '0-9år'))
        R10 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '10-19år'))
        R20 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '20-29år'))
        R30 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '30-39år'))
        R40 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '40-49år'))
        R50 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '50-59år'))
        R60 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '60-69år'))
        R70 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '70-79år'))
        R80 = length(which(df.befolkning$boks == "R" & df.befolkning$aldersgruppe == '80+'))
        
        RU = length(which(df.befolkning$boks == "RU"))
        RU0 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '0-9år'))
        RU10 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '10-19år'))
        RU20 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '20-29år'))
        RU30 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '30-39år'))
        RU40 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '40-49år'))
        RU50 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '50-59år'))
        RU60 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '60-69år'))
        RU70 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '70-79år'))
        RU80 = length(which(df.befolkning$boks == "RU" & df.befolkning$aldersgruppe == '80+'))
        
        RV = length(which(df.befolkning$boks == "RV"))
        RV0 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '0-9år'))
        RV10 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '10-19år'))
        RV20 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '20-29år'))
        RV30 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '30-39år'))
        RV40 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '40-49år'))
        RV50 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '50-59år'))
        RV60 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '60-69år'))
        RV70 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '70-79år'))
        RV80 = length(which(df.befolkning$boks == "RV" & df.befolkning$aldersgruppe == '80+'))
        
        
        
        simulering <- rbind(simulering, c(S,S0,S10,S20,S30,S40,S50,S60,S70,S80,
                               dS,dS0,dS10,dS20,dS30,dS40,dS50,dS60,dS70,dS80,
                               E,E0,E10,E20,E30,E40,E50,E60,E70,E80,
                               dE,dE0,dE10,dE20,dE30,dE40,dE50,dE60,dE70,dE80,
                               I,I0,I10,I20,I30,I40,I50,I60,I70,I80,
                               dI,dI0,dI10,dI20,dI30,dI40,dI50,dI60,dI70,dI80,
                               R,R0,R10,R20,R30,R40,R50,R60,R70,R80,
                               dR,dR0,dR10,dR20,dR30,dR40,dR50,dR60,dR70,dR80,
                               RV,RV0,RV10,RV20,RV30,RV40,RV50,RV60,RV70,RV80,
                               dRV,dRV0,dRV10,dRV20,dRV30,dRV40,dRV50,dRV60,dRV70,dRV80,
                               RU,RU0,RU10,RU20,RU30,RU40,RU50,RU60,RU70,RU80,
                               dRU,dRU0,dRU10,dRU20,dRU30,dRU40,dRU50,dRU60,dRU70,dRU80,
                               HI,HI0,HI10,HI20,HI30,HI40,HI50,HI60,HI70,HI80,
                               dHI,dHI0,dHI10,dHI20,dHI30,dHI40,dHI50,dHI60,dHI70,dHI80))
        
        return(simulering)
    }
    
      simulering <- initier.simulering(df.befolkning, total.befolkning, n.smittede, n.smittsomme, n.friske, n.friske.ukjent)
      
    
    vaksinering.R.RU <- matrix(0, nrow = tid.til.vaksine, ncol = 2)
    colnames(vaksinering.R.RU) <- c("Antall friske vaksineres", "Antall mottagelige vaksineres")
    R.eff.daglig <- c(0)
    #Kjør
    j = 1 # gruppe som skal vaksineres
    
    for(i in 1:n.dager)
    {
      # Spre smitten:
      de.smittsomme <- which(df.befolkning$boks == "I")
      n.forsøkes.smittes <- round(length(de.smittsomme)*mean(df.befolkning$R[de.smittsomme])*(1/tid.smittsom))
      
      if(length(de.smittsomme) != 0)
      {
        index.forsøkes.smittes <- sample(total.befolkning,n.forsøkes.smittes, prob = df.befolkning$R.snitt, replace = FALSE)
      }
      else
      {
          index.forsøkes.smittes <- c()
      }
      
      de.forsøkes.smittes <- df.befolkning[index.forsøkes.smittes,]
      de.som.smittes <- as.integer(rownames(de.forsøkes.smittes[(de.forsøkes.smittes$boks == "S") ,]))
      
      if(length(de.smittsomme) == 0)
      {
        R.eff <- 0
      }
      else
      {
        R.eff <- (length(de.som.smittes)/length(de.smittsomme))*tid.smittsom # Vurder R0-package
      }
     
      R.eff.daglig[i+1] <- R.eff
      #print(R.eff)
      #de.som.smittes <- as.integer(rownames(df.befolkning[index.forsøkes.smittes,] =="S"))
    
      df.befolkning$boks[de.som.smittes] <- "E"
      df.befolkning$dag.i.tilstand[de.som.smittes] <- 0
      
    
      
      
      index.som.vaksineres <- c()
      vaksinegruppe <- c()
      vaksinegruppe2 <- c()
      #Vaksiner:
      if(i >= tid.til.vaksine) 
        {
        
        if(j < length(vaksineringsrekkefølge))
        {
          
          vaksinegruppe <- which(df.befolkning$aldersgruppe == vaksineringsrekkefølge[j] & (df.befolkning$boks == "S" | df.befolkning$boks == "RU") & df.befolkning$risikogruppe == 0 )  # kan det hende at man vaksinerer folk som har gjennomgått sykdomsforløp 
          
          andel.vaksinert <- (1 - length(vaksinegruppe)/vaksinegruppe.alder[j])
          #print(andel.vaksinert)
          if(daglige.vaksiner <= length(vaksinegruppe)  & daglige.vaksiner > 0 & andel.vaksinert < vaksine.dekning.strategi[j])
          { 
            index.som.vaksineres <- sample(vaksinegruppe, daglige.vaksiner ,replace = FALSE)
            n.friske.vaksineres <- length(which(df.befolkning$boks[index.som.vaksineres] == "RU"))
            n.mottagelige.vaksineres <- length(which(df.befolkning$boks[index.som.vaksineres] == "S"))
          }
          else if(andel.vaksinert >= vaksine.dekning.strategi[j])
          {
            j<-j+1
            vaksinegruppe <- which(df.befolkning$aldersgruppe == vaksineringsrekkefølge[j] & (df.befolkning$boks == "S" | df.befolkning$boks == "RU")& df.befolkning$risikogruppe == 0) 
            index.som.vaksineres <- sample(vaksinegruppe, daglige.vaksiner ,replace = FALSE)
            n.friske.vaksineres <- length(which(df.befolkning$boks[index.som.vaksineres] == "RU"))
            n.mottagelige.vaksineres <- length(which(df.befolkning$boks[index.som.vaksineres] == "S"))
            print(paste("nå vaksineres de i aldersgruppen",vaksineringsrekkefølge[j]))
          }
          else
          {
            index.som.vaksineres <- c()
            n.friske.vaksineres <- 0
            n.mottagelige.vaksineres <- 0
          }
        }
        df.befolkning$boks[index.som.vaksineres] <- "RV"
        df.befolkning$dag.i.tilstand[index.som.vaksineres] <- 0
        vaksinering.R.RU <- rbind(vaksinering.R.RU, c(n.friske.vaksineres, n.mottagelige.vaksineres))
      }
      if(vaks == 0)
      {
        vaksinering.R.RU <- rbind(c(0,0))
      }
      
      
      #Oppdater befolkningstabell:
      df.befolkning$dag.i.tilstand <- df.befolkning$dag.i.tilstand + 1 
      
      index.blir.friske <- which(df.befolkning$boks == "I" & df.befolkning$dag.i.tilstand == tid.smittsom) 
      index.blir.lagtinn <- which(df.befolkning$boks == "I" & df.befolkning$dag.i.tilstand == tid.symp.innl) 
      index.blir.utskrevet <- which(df.befolkning$boks == "Hi" & df.befolkning$dag.i.tilstand == liggetid) 
      index.blir.syke <- which(df.befolkning$boks == "E" & df.befolkning$dag.i.tilstand > tid.inkubasjon)
      index.blir.friske.ukjent <- sample(index.blir.friske,(length(index.blir.friske)*andel.ukjent.friske) ,replace = FALSE ) 
      index.blir.friske.kjent <- setdiff(index.blir.friske,index.blir.friske.ukjent)
      
      
      df.befolkning$boks[index.blir.friske.kjent] <- "R"
      df.befolkning$boks[index.blir.utskrevet] <- "R"
      df.befolkning$boks[index.blir.friske.ukjent] <- "RU" #En andel oppdages ikke.
      df.befolkning$dag.i.tilstand[index.blir.friske] <- 1
      df.befolkning$boks[index.blir.lagtinn] <- "Hi"
      df.befolkning$dag.i.tilstand[index.blir.lagtinn] <- 1
      
      
      #Legg inn på sykehuset
      if(length(index.blir.friske) > 0)
      {
        for(m in 1:length(index.blir.friske))
        {
          
          legginn.check <- runif(1,0,1)
          if(df.befolkning$innleggelsesprosent[index.blir.friske[m]] >= legginn.check)
          {
            df.befolkning$boks[index.blir.friske[m]] <- "I" #Ble ikke friske allikevel.
            df.befolkning$dag.i.tilstand[index.blir.friske[m]] <- 5
          }
          
        }
      } 
      
      
      
      df.befolkning$boks[index.blir.syke] <- "I"
      df.befolkning$dag.i.tilstand[index.blir.syke] <- 1
      
      #Importer tilfeller her
      daglig.import <- sample(which(df.befolkning$boks == "S"), Import.verdier[i], replace = FALSE)
      
      df.befolkning$boks[daglig.import] <- sample(c("E" , "I") , Import.verdier[i] , replace = TRUE)
      df.befolkning$dag.i.tilstand[daglig.import] <- sample(1:tid.inkubasjon,n.smittede, replace = TRUE)
      
      
      simulering <- oppdater.simulering(df.befolkning, de.som.smittes, index.blir.syke, index.som.vaksineres, index.blir.friske, index.blir.friske.ukjent, index.blir.friske.kjent,index.blir.lagtinn, index.blir.utskrevet, total.befolkning)
                                       
      #print(i)
    }
    
    #QC
      
    #masser data
    simulering <- as.data.frame(simulering)
    simulering <- cbind(simulering, datoer)
    simulering <- cbind(simulering, "RTOT" = simulering$R + simulering$RU+ simulering$RV)
    simulering <- cbind(simulering, "RTOT 0-9år" = simulering$`R 0-9år` + simulering$`RU 0-9år`+ simulering$`RV 0-9år`)
    simulering <- cbind(simulering, "RTOT 10-19år" = simulering$`R 10-19år` + simulering$`RU 10-19år`+ simulering$`RV 10-19år` )
    simulering <- cbind(simulering, "RTOT 20-29år" = simulering$`R 20-29år` + simulering$`RU 20-29år`+ simulering$`RV 20-29år` )
    simulering <- cbind(simulering, "RTOT 30-39år" = simulering$`R 30-39år` + simulering$`RU 30-39år`+ simulering$`RV 30-39år` )
    simulering <- cbind(simulering, "RTOT 40-49år" = simulering$`R 40-49år` + simulering$`RU 40-49år`+ simulering$`RV 40-49år` )
    simulering <- cbind(simulering, "RTOT 50-59år" = simulering$`R 50-59år` + simulering$`RU 50-59år`+ simulering$`RV 50-59år`)
    simulering <- cbind(simulering, "RTOT 60-69år" = simulering$`R 60-69år` + simulering$`RU 60-69år`+ simulering$`RV 60-69år` )
    simulering <- cbind(simulering, "RTOT 70-79år" = simulering$`R 70-79år` + simulering$`RU 70-79år`+ simulering$`RV 70-79år` )
    simulering <- cbind(simulering, "RTOT 80år +" = simulering$`R 80år +` + simulering$`RU 80år +`+ simulering$`RV 80år +`)
    
    simulering <- cbind(simulering, "% andel friske" = (simulering$RTOT/total.befolkning))
    simulering <- cbind(simulering, "% andel friske 0-9 år" = (simulering$`RTOT 0-9år`/48909))
    simulering <- cbind(simulering, "% andel friske 10-19 år" = (simulering$`RTOT 10-19år`/48199))
    simulering <- cbind(simulering, "% andel friske 20-29 år" = (simulering$`RTOT 20-29år`/49296))
    simulering <- cbind(simulering, "% andel friske 30-39 år" = (simulering$`RTOT 30-39år`/54740))
    simulering <- cbind(simulering, "% andel friske 40-49 år" = (simulering$`RTOT 40-49år`/51135))
    simulering <- cbind(simulering, "% andel friske 50-59 år" = (simulering$`RTOT 50-59år`/46595))
    simulering <- cbind(simulering, "% andel friske 60-69 år" = (simulering$`RTOT 60-69år`/35913))
    simulering <- cbind(simulering, "% andel friske 70-79 år" = (simulering$`RTOT 70-79år`/23849))
    simulering <- cbind(simulering, "% andel friske 80år +" = (simulering$`RTOT 80år +`/12485))
    
    simulering <- cbind(simulering, "R totalt" = R.eff.daglig)
    simulering <- cbind(simulering, "import" = Import.verdier)
    simulering <- cbind(simulering,vaksinering.R.RU) 
    
    #Plot
    theme_set(theme_bw())
      
    
    simulering.plot <- melt(simulering, id.vars = "datoer")
    
    
    totalt <- ggplot(subset(simulering.plot, variable %in% c("S","E","I","RTOT","RV")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    
    I <- ggplot(subset(simulering.plot, variable %in% c("I")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)+
      scale_x_date(date_breaks = "1 month",date_labels = "%B")
    dE <- ggplot(subset(simulering.plot, variable %in% c("dE")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)+
      scale_x_date(date_breaks = "1 month",date_labels = "%B")
    andel.R <- ggplot(subset(simulering.plot, variable %in% c("% andel friske")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)+
      scale_x_date(date_breaks = "1 month",date_labels = "%B")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
      
      
    S.per.alder <- ggplot(subset(simulering.plot, variable %in% c("S 0-9år","S 10-19år","S 20-29år","S 30-39år","S 40-49år","S 50-59år","S 60-69år","S 70-79år","S 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    E.per.alder <- ggplot(subset(simulering.plot, variable %in% c("E 0-9år","E 10-19år","E 20-29år","E 30-39år","E 40-49år","E 50-59år","E 60-69år","E 70-79år","E 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    I.per.alder <- ggplot(subset(simulering.plot, variable %in% c("I 0-9år","I 10-19år","I 20-29år","I 30-39år","I 40-49år","I 50-59år","I 60-69år","I 70-79år","I 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    R.per.alder <- ggplot(subset(simulering.plot, variable %in% c("R 0-9år","R 10-19år","R 20-29år","R 30-39år","R 40-49år","R 50-59år","R 60-69år","R 70-79år","R 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    RV.per.alder <- ggplot(subset(simulering.plot, variable %in% c("RV 0-9år","RV 10-19år","RV 20-29år","RV 30-39år","RV 40-49år","RV 50-59år","RV 60-69år","RV 70-79år","RV 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    RU.per.alder <- ggplot(subset(simulering.plot, variable %in% c("RU 0-9år","RU 10-19år","RU 20-29år","RU 30-39år","RU 40-49år","RU 50-59år","RU 60-69år","RU 70-79år","RU 80år +")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    andel.R.per.alder <- ggplot(subset(simulering.plot, variable %in% c("% andel friske 0-9 år", "% andel friske 10-19 år","% andel friske 20-29 år","% andel friske 30-39 år","% andel friske 40-49 år",
                                                                        "% andel friske 50-59 år","% andel friske 60-69 år","% andel friske 70-79 år","% andel friske 80år +")), 
                                aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
    kjent.ukjent.vaksine <- ggplot(subset(simulering.plot, variable %in% c("Antall friske vaksineres", "Antall mottagelige vaksineres")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    
    nye.daglig.smittede <- ggplot(subset(simulering.plot, variable %in% c("dE 0-9år","dE 10-19år","dE 20-29år","dE 30-39år","dE 40-49år","dE 50-59år","dE 60-69år","dE 70-79år","dE 80år +")),
                                  aes(x = datoer, y = value, color  = variable, fill = variable))+
      geom_bar(stat = "identity")
    
    innlagte.plot <- ggplot(subset(simulering.plot, variable %in% c("Hi 0-9år","Hi 10-19år","Hi 20-29år","Hi 30-39år","Hi 40-49år","Hi 50-59år","Hi 60-69år","Hi 70-79år","Hi 80år +")),
                                  aes(x = datoer, y = value, color  = variable, fill = variable))+
      geom_bar(stat = "identity")
    
    de.ulike.R <- ggplot(subset(simulering.plot, variable %in% c("RTOT","R","RU","RV")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    
    import.plot <- ggplot(subset(simulering.plot, variable %in% c("import")),
                                  aes(x = datoer, y = value, color  = variable, fill = variable))+
      geom_bar(stat = "identity")
    
    R.plot <- ggplot(subset(simulering.plot, variable %in% c("R totalt")), aes(x = datoer, y = value, color  = variable))+
      geom_line(size = 1.5)
    
    
    if(beregn.R.R0.package)
    {
      #R_list.imp <- get_Rt_import_ind(datoer, simulering$dE, Import.verdier)
      R_list <- get_Rt_ind(datoer, simulering$dE)
      
      R <- data.frame()
      R <- head(R_list[[1]]$R, -1)
      #R <- cbind(R, head(R_list.imp[[1]]$R, -1))
      R <- cbind(R, head(R_list[[1]]$epid$t, -1))
      #R <- cbind(R,head(R_list[[1]]$epid$incid, -1))
      #R <- cbind(R, head(R_list.imp[[1]]$epid$incid, -1))
      
      colnames(R) <- c("R","Date")
      #colnames(R) <- c("R","R lokal spredning","Date", "nye smittede", "nye smittede eksludert import")
      #colnames(data.n.smittet) <- (c("Scenario 1", "Scenario 2","Scenario 3","Scenario 4"))
      R2 <- R[10:length(R[,1]),]
      R2 <- melt(R2)
      colnames(R2) <- (c("Dato", "Type", "Value"))
      
      HV_blue_dark <- rgb(0, 51, 141, max = 255, alpha = 150)
      HV_blue <- rgb(122, 178, 220, max = 255, alpha = 150)
      HV_blue_light <- rgb(178, 202, 233, max = 255, alpha = 200)
      
      R1 <- ggplot.dagligR <- ggplot(R2, aes(x = as.Date(Dato), y = Value, color = Type, linetype = Type))+
        geom_line(data=subset(R2,Type=="R"),size = 2)+
        #geom_line(data=subset(R2,Type=="R lokal spredning"),size = 2)+
        geom_hline(aes(yintercept = 1), color = "grey3", linetype="dashed", size = 2)+
        #ggtitle(paste("Estimert reproduksjonstall for nye daglige smittede, med og uten import HST. Data tom.", max(dates))) +
        #geom_text(aes(label=subset(R2,Type=="R nye smittede")), x=as.Date(Dato)+1, y= subset(R2,Type=="R nye smittede")+ 0.1, check_overlap = TRUE)+
        #geom_text(aes(label=subset(R2,Type=="R spredning")), x=as.Date(Dato)+1, y= subset(R2,Type=="R spredning")+ 0.1, check_overlap = TRUE)+
        ylab("tidsvarierende reproduksjonstall [Rt]") + 
        xlab("Dato") +
        #ylim(0,3)+
        theme(legend.position = "right")+
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +
        scale_color_manual(values = c(HV_blue, HV_blue_dark))+
    
        theme_bw()
     
    }
    
    
    if(start.etter.vaks.eldre == TRUE)
    {
      somePDFPath = paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/Ny IVM m vaks eldre, n.dager",n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"seed",seed.var,"vaks.strat",vaksine.strategi,"sim",sim,".pdf")
      pdf(file=somePDFPath, paper="USr",width=11, height=8.50)
      par(mfcol = c(1,1))
  
      write.xlsx(simulering, paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/Ny IVM m vaks eldre, n.dager" ,n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"seed",seed.var,"vaks.strat",vaksine.strategi,"sim",sim,".xlsx"))
    }
    if(start.etter.vaks.eldre != TRUE)
    {
      somePDFPath = paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/Ny IVM, n.dager",n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"seed",seed.var,"vaks.strat",vaksine.strategi,"sim",sim,".pdf")
      pdf(file=somePDFPath, paper="USr",width=11, height=8.50)
      par(mfcol = c(1,1))
  
      write.xlsx(simulering, paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/Ny IVM, n.dager",n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"seed",seed.var,"vaks.strat",vaksine.strategi,"sim",sim,".xlsx"))
  
    }
  
    #totalt
    if(beregn.R.R0.package)
    {
      print(R1)
    }
    I
    dE
    print(andel.R)
    print(innlagte.plot)
    # S.per.alder
    # E.per.alder
    # I.per.alder
    # R.per.alder
    # RV.per.alder
    # RU.per.alder
    # andel.R.per.alder
    # kjent.ukjent.vaksine
    print(nye.daglig.smittede)
    import.plot
    #R.plot
    # de.ulike.R
  
  
    dev.off()
    
    
    sim.data<- rbind(sim.data, c(sum(simulering$dE),sum(simulering$dHi), 
                                 sum(simulering$`dE 0-9år`), sum(simulering$`dE 10-19år`), sum(simulering$`dE 20-29år`), sum(simulering$`dE 30-39år`), 
                                 sum(simulering$`dE 40-49år`), sum(simulering$`dE 50-59år`), sum(simulering$`dE 60-69år`), sum(simulering$`dE 70-79år`), sum(simulering$`dE 80år +`),
                                 sum(simulering$`dHi 0-9år`), sum(simulering$`dHi 10-19år`), sum(simulering$`dHi 20-29år`), sum(simulering$`dHi 30-39år`), 
                                 sum(simulering$`dHi 40-49år`), sum(simulering$`dHi 50-59år`), sum(simulering$`dHi 60-69år`), sum(simulering$`dHi 70-79år`), sum(simulering$`dHi 80år +`)
                                 )
                     
                     )
  
  }
  colnames(sim.data) <- c("Totalt antall smittet", "Totalt innlagt", 
                          "Smittet 0-9år","Smittet 10-19år","Smittet 20-29år","Smittet 30-39år","Smittet 40-49år","Smittet 50-59år",
                          "Smittet 60-69år","Smittet 70-79år","Smittet 80år +",
                          "Innlagt 0-9år","Innlagt 10-19år","Innlagt 20-29år","Innlagt 30-39år","Innlagt 40-49år","Innlagt 50-59år","Innlagt 60-69år","Innlagt 70-79år","Innlagt 80år +")
  
  sim.data <- as.data.frame(sim.data)
  sim.data.plot <- melt(sim.data)#, id.vars = "simuleringer")
  write.xlsx(sim.data, paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/samlet simuleringer IVM, n.dager",n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"seed",seed.var,"vaks.strat",vaksine.strategi,".xlsx"))
  # 
  somePDFPath = paste("F:/DATA/Analyse/Nils/R-beregninger/R-pdf/Resultater individbasert vaksinemodell/Output simuleringer/Grafer simulering, IVM m vaks eldre, n.dager",n.dager, "imp =",n.snitt.daglig.import,"R =",round(mean(R.Verdier), digits = 2),"k =",k,"DV =",daglige.vaksiner,"vaks.strat",vaksine.strategi,".pdf")
  pdf(file=somePDFPath, paper="USr",width=11, height=8.50)
  par(mfcol = c(1,2))
  
  fordeling.smittede.tot <- ggplot(subset(sim.data.plot, variable %in% c("Totalt antall smittet")),
                               aes(x = variable, y = value, fill = variable))+
    geom_boxplot(size = 1)+
    xlab('Hele befolkningen')+
    ylab('Totalt antall smittet')+
    ggtitle(paste('Fordeling av total antall estimerte smittede ved vaksineringsscenario',vaksine.strategi))+
    scale_fill_brewer(palette="Blues") + theme_classic()
  
  fordeling.innlagte.tot <- ggplot(subset(sim.data.plot, variable %in% c("Totalt innlagt")),
                                   aes(x = variable, y = value, fill = variable))+
    geom_boxplot(size = 1)+
    xlab('Hele befolkningen')+
    ylab('Totalt antall innlagt')+
    ggtitle(paste('Fordeling av total antall estimerte innlagte ved vaksineringsscenario',vaksine.strategi))+
    scale_fill_brewer(palette="Reds") + theme_classic()
  
  par(mfcol = c(1,1))
  fordeling.smittede <- ggplot(subset(sim.data.plot, variable %in% c("Smittet 0-9år","Smittet 10-19år","Smittet 20-29år",
                                                                             "Smittet 30-39år","Smittet 40-49år","Smittet 50-59år",
                                                                             "Smittet 60-69år","Smittet 70-79år","Smittet 80år +")),
                                aes(x = variable, y = value, fill = variable))+
    geom_boxplot(size = 1)+
    xlab('Aldersgruppe')+
    ylab('Totalt antall smittet')+
    ggtitle(paste('Fordeling av total antall estimerte smittede ved vaksineringsscenario',vaksine.strategi))+
    scale_fill_brewer(palette="Blues") + theme_classic()
  
  fordeling.innlagte <- ggplot(subset(sim.data.plot, variable %in% c("Innlagt 0-9år","Innlagt 10-19år","Innlagt 20-29år",
                                                                             "Innlagt 30-39år","Innlagt 40-49år","Innlagt 50-59år",
                                                                             "Innlagt 60-69år","Innlagt 70-79år","Innlagt 80år +")),
                                       aes(x = variable, y = value, fill = variable))+
    geom_boxplot(size = 1)+
    xlab('Aldersgruppe')+
    ylab('Totalt antall Innlagt')+
    ggtitle(paste('Fordeling av total antall estimerte innleggelser ved vaksineringsscenario',vaksine.strategi))+
    scale_fill_brewer(palette="Reds") + theme_classic()
  
  
  print(fordeling.smittede.tot)
  print(fordeling.innlagte.tot)
  print(fordeling.smittede)
  print(fordeling.innlagte)
  
  dev.off()
  print(vaks)
  # res.sim.plot <- ggplot(sim.data, aes(y = `Totalt antall smittet`))+
  #   geom_boxplot(fill = HV_blue, size = 1)+
  #   theme_classic()
}