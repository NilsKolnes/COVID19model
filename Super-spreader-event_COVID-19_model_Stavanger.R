library(ggplot2) 
library(reshape)

rm(list=ls())
set.seed(123)

# Skript for simulering av smitte og arbeidsdager tapt gitt en superspreder event i Stavanger fra en kveld på byen en helg.
# Skrevet av Nils Henrik Kolnes 19.08.2020
# Helse Stavanger HF

steng.regelbrytere = TRUE  # Steng de som ikke følge regler
tolerert.grense <- 0.90


n.sim <- 1000

data.karantene <- data.frame(matrix(ncol=4,nrow=0))
#data.p.smitte <- data.frame(matrix(ncol=2,nrow=0))
#data.e.smitte <- data.frame(matrix(ncol=2,nrow=0))
#data.n.ute <- data.frame(matrix(ncol=2,nrow=0))
data.n.smittet <- data.frame(matrix(ncol=4,nrow=0))

n.50 <- 91
n.200 <- 70


smittevern.skalering.50 <- 0.7 
smittevern.skalering.200 <- 0.7

SAR <- 0.25 

n.t = n.50+n.200
kap.utested.vec.50.usv <- c()
kap.utested.vec.200.usv <- c()
kap.utested.vec.50.msv <- c()
kap.utested.vec.200.msv <- c()

n.belegg200.usv <- c()
n.belegg200.msv <- c()
n.belegg50.usv <- c()
n.belegg50.msv <- c()


# Define each location with a guest capacity for the different scenarios
for(i in 1:n.t)
{
  if(i <= n.50)
  {
    kap.utested.vec.50.usv[i] <- 50
    kap.utested.vec.200.usv[i] <- 50
    kap.utested.vec.50.msv[i] <- 50*rnorm(1,0.8,0.2)  
    kap.utested.vec.200.msv[i] <- 50*rnorm(1,0.8,0.2)  
  }
  if(i > n.50)
  {
    kap.utested.vec.50.usv[i] <- 50
    kap.utested.vec.200.usv[i] <- 200
    kap.utested.vec.50.msv[i] <- 50 # De store utestedene får automatisk smittevern ved å redusere antall gjester.
    kap.utested.vec.200.msv[i] <- 200*rnorm(1,0.8,0.2)  #*smittevern.skalering.200
  }
  
}

if(steng.regelbrytere)
{
  for(i in 1:n.t)
  {
    if(i <= n.50)
    {
      if((kap.utested.vec.50.msv[i]/50) >= tolerert.grense)
      {
        kap.utested.vec.50.msv[i] <- 0 #closed
        
      }
      if((kap.utested.vec.200.msv[i]/50) >= tolerert.grense)
      {
        kap.utested.vec.200.msv[i] <- 0 #closed
        
      }
      
    }
    if(i > n.50)
    {
      if((kap.utested.vec.200.msv[i]/200) >= tolerert.grense)
      {
        kap.utested.vec.200.msv[i] <- 0 #closed
      }
      
    }
    
  }
  
}


n.utested.vec.200.usv <- c(rep(0,n.t))
n.utested.vec.50.usv <- c(rep(0,n.t))
n.utested.vec.200.msv <- c(rep(0,n.t))
n.utested.vec.50.msv <- c(rep(0,n.t))

# Fills the locations with guests.
for(i in 1:n.t){
  
  p.belegg <- rbeta(1,5,3) #runif(1,0.7,1.0)  #Sett beleggsprosent.
  
  n.utested.vec.200.usv[i] <- round(kap.utested.vec.200.usv[i]*p.belegg)
  n.utested.vec.200.msv[i] <- round(kap.utested.vec.200.msv[i]*p.belegg)
  n.utested.vec.50.usv[i] <- round(kap.utested.vec.50.usv[i]*(p.belegg))
  n.utested.vec.50.msv[i] <- round(kap.utested.vec.50.msv[i]*(p.belegg))
  
  #Belegget skal ganges med SAR verdien senere
  n.belegg200.usv[i] <- p.belegg
  n.belegg200.msv[i] <- p.belegg*rnorm(1,0.8,0.2)  #*smittevern.skalering.200
  
  if(kap.utested.vec.200.usv[i] == 200)
  {

    n.belegg50.usv[i] <- p.belegg/4
    n.belegg50.msv[i] <- (p.belegg/4) #*smittevern.skalering.50
    
  }
  else
  {
    
    n.belegg50.usv[i] <- p.belegg
    n.belegg50.msv[i] <- p.belegg*rnorm(1,0.8,0.2)  #*smittevern.skalering.50
    
  }
}


N.guests.50.usv <- sum(n.utested.vec.50.usv)
N.guests.50.msv <- sum(n.utested.vec.50.msv)
N.guests.200.usv <- sum(n.utested.vec.200.usv)
N.guests.200.msv <- sum(n.utested.vec.200.msv)

print(N.guests.50.msv)
print(N.guests.50.usv)
print(N.guests.200.msv)
print(N.guests.200.usv)

for(i in 1:n.sim)
{
  n.besøk <- round(runif(1,0.5,3.49))
  utestedene.tmp.50.usv <- sample(1:n.t, n.besøk, replace = FALSE, prob = (n.utested.vec.50.usv/sum(n.utested.vec.50.usv)))
  utestedene.tmp.50.msv <- sample(1:n.t, n.besøk, replace = FALSE, prob = (n.utested.vec.50.msv/sum(n.utested.vec.50.msv)))
  utestedene.tmp.200.usv <- sample(1:n.t, n.besøk, replace = FALSE, prob = (n.utested.vec.200.usv/sum(n.utested.vec.200.usv)))
  utestedene.tmp.200.msv <- sample(1:n.t, n.besøk,  replace = FALSE, prob = (n.utested.vec.200.msv/sum(n.utested.vec.200.msv)))
  #utestedene.tmp <-runif(n.besøk,1,n.t)
  
  n.karantene.50.usv <- round(sum(n.utested.vec.50.usv[utestedene.tmp.50.usv]))
  n.karantene.50.msv <- round(sum(n.utested.vec.50.msv[utestedene.tmp.50.msv]))
  n.karantene.200.usv <- round(sum(n.utested.vec.200.usv[utestedene.tmp.200.usv]))
  n.karantene.200.msv <- round(sum(n.utested.vec.200.msv[utestedene.tmp.200.msv]))
  
  # Supersmitter
  n.smittet50.usv <- round(sum(n.utested.vec.50.usv[utestedene.tmp.50.usv]*SAR*n.belegg50.usv[utestedene.tmp.50.usv])) #Ganger SAR med belegg for å redusere SAR for steder med lavere belegg
  n.smittet50.msv <- round(sum(n.utested.vec.50.msv[utestedene.tmp.50.msv]*SAR*n.belegg50.msv[utestedene.tmp.50.msv]))
  n.smittet200.usv <- round(sum(n.utested.vec.200.usv[utestedene.tmp.200.usv]*SAR*n.belegg200.usv[utestedene.tmp.200.usv]))
  n.smittet200.msv <- round(sum(n.utested.vec.200.msv[utestedene.tmp.200.msv]*SAR*n.belegg200.msv[utestedene.tmp.200.msv]))
  
  data.karantene <- rbind(data.karantene,c(n.karantene.200.usv,n.karantene.200.msv,n.karantene.50.usv,n.karantene.50.msv))
  data.n.smittet <- rbind(data.n.smittet, c(n.smittet200.usv,n.smittet200.msv,n.smittet50.usv,n.smittet50.msv))
  
}



par(mfrow = c(2,2))

hist(n.utested.vec.50.msv, breaks = 25, main = paste("Distribution of the number of susceptible for scenario ",1), xlab = "number of guests", ylab= "Frequency")
hist(n.utested.vec.50.usv, breaks = 25, main = paste("Distribution of the number of susceptible for scenario ",2), xlab = "number of guests", ylab= "Frequency")
hist(n.utested.vec.200.msv, breaks = 25, main = paste("Distribution of the number of susceptible for scenario ",3), xlab = "number of guests", ylab= "Frequency") 
hist(n.utested.vec.200.usv, breaks = 25, main = paste("Distribution of the number of susceptible for scenario ",4), xlab = "number of guests", ylab= "Frequency") 



for(i in 1:4)
{
  hist(data.n.smittet[,i], prob = TRUE, breaks = 25 , main = paste("Distribution of the number of exposed for scenario ",i), xlab = "Number of exposed E", ylab= "Probability density")
  legend("topright",
         c(paste("Median: ",round(median(data.n.smittet[,i]))),paste("Average: ",round(mean(data.n.smittet[,i]))) ,paste("90 % q: ",round(quantile(data.n.smittet[,i], probs = 0.90))) ),
         lty = c(3,5,7))
}


for(i in 1:4)
{
  hist(data.karantene[,i], prob = TRUE, breaks = 25 , main = paste("Distribution of the number of quarantined susceptibles for scenario ",i), xlab = "Number of quarantined susceptibles", ylab= "Probability density")
  legend("topright",
         c(paste("Median: ",round(median(data.karantene[,i]))),paste("Average: ",round(mean(data.karantene[,i]))) ,paste("90 % q: ",round(quantile(data.karantene[,i], probs = 0.90))) ),
         lty = c(3,5,7))
}
# ggplotting
# Smittede:
colnames(data.n.smittet) <- (c("Scenario 1", "Scenario 2","Scenario 3","Scenario 4"))
data.n.smittet=   melt(data.n.smittet)
colnames(data.n.smittet) <- (c("Scenario", "value"))


ggplot(data.n.smittet, aes(x = value, color = Scenario, fill = Scenario)) +
  geom_density(alpha = 0.1, size = 1) +
  xlab("Number of exposed [E]")+
  ylab("Density")+
  ggtitle("Distribution of the esitmated number of exposed for each scenario") +

  theme_bw()

#Karantene: 

colnames(data.karantene) <- (c("Scenario 1", "Scenario 2","Scenario 3","Scenario 4"))
data.karantene=   melt(data.karantene)
colnames(data.karantene) <- (c("Scenario", "value"))


ggplot(data.karantene, aes(x = value, color = Scenario, fill = Scenario)) +
  geom_density(alpha = 0.1, size = 1) +
  xlab("Number of quarantined susceptibles")+
  ylab("Density")+
  ggtitle("Distribution of the esitmated number of quarantined susceptibles") +
  
  theme_bw()



