## Benefit/ charger

# Conventional
# VMT 10,230 miles
# MPG 33 miles/ gallon
# Gas required 310 gallons
# Gas unit price $2.49/ gallon
# Total cost $772
# 
# EV
# 30 kWh/100 mile
# 3069 kWh
# $0.077/ kWh
# Elec. cost $236.3
# 
# Annual benefit for one car $535.7
# 
# Cost 
# Installation $2,050

vmt=10230
mpg=33
gas=vmt/mpg
gas_c=2.49
c_cost=gas_c*gas

kw=3069
u_cost=0.077
e_cost=kw*u_cost

ben=c_cost-e_cost

inst=2050

rt=0.06

## case1, single owner - single user
npv = c()
dpp = c()

for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst
  q=data.frame(matrix(NA,10,6))
  for(i in 1:10){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=rgs*gas - kw*rei
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i+abs(pv/old),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
plot(dpp)
q

par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)

par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)


## case2, one owner - 5 renters 
npv = c()
dpp = c()

for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst * 6
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=rgs*gas - kw*rei
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i+abs(pv/old),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
plot(dpp)
q

par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)

par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)



## case3, only 6 renters

npv = c()
dpp = c()
com = 0.2

for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst * 6
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=0.2*6*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i+abs(pv/old),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
plot(dpp)
q

par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)

par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)

