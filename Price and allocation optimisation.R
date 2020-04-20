#Achilleas Drakou
#12.04.2020


#--------------Strategy 1---------------

library("nloptr")

# Differentiated Prices
eval_f <- function(x){
  # Let's set first element as package price
  # Set second element as walkinprice
  packagePrice = x[1]
  walkinPrice = x[2]
  packageDemand = max(0, 152 - packagePrice)
  walkinDemand = max(0, 23 - walkinPrice)
  revenue = packagePrice * packageDemand + walkinPrice * walkinDemand
  objfunction = -revenue
  return(objfunction)
}
eval_g_ineq <- function(x) {
  packagePrice = x[1]
  walkinPrice = x[2]
  packageDemand = max(0, 152 - packagePrice)
  walkinDemand = max(0, 23 - walkinPrice)
  cap = 64
  
  
  #Add Constraint 4: Package Price <= Walkin Price
  constraint <- c(packageDemand + walkinDemand - cap,
                  -packageDemand,
                  -walkinDemand,
                  x[1]-x[2])
  return(constraint)
}
# initial values
x0 <- c(79, 120)

# lower and upper bounds of control

lb <- c(0, 0)
ub <- c(200, 200)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel" = 1.0e-9,
              "maxeval" = 1000)
result <- nloptr(x0=x0, eval_f=eval_f, lb=lb, ub=ub,
                 eval_g_ineq=eval_g_ineq, opts=opts)

# print(result)
priceOpt <- result$solution
RevenueOpt <- -result$objective
soldPackage = max(0, 152 - priceOpt[1])
soldWalkin = max(0, 23 - priceOpt[2])
soldTickets = soldPackage + soldWalkin
print(paste("Optimal Price for Package:", priceOpt[1]))

print(paste("Optimal Price for Walk Ins:", priceOpt[2]))

print(paste("Optimal Revenue:", RevenueOpt))

#Revenue comparison
revenue_before = (8*120 + 55*79)*153 #yearly revenue with current prices
revenue_new = RevenueOpt*153 #yearly revenue with new prices
print(c(revenue_before,revenue_new))

increase1 = revenue_new-revenue_before

paste("Simply by applying our new pricing model, the Trust can increase revenue,
      by: £", increase1, "per year.")


#______BAR CHART1 Revenue Comparison_____
# Fitting Labels
par(las=1) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) 
counts <- c(revenue_before/1000, revenue_new/1000)

xticks <- seq(0, 900, by=100)
barplot(counts, main="Revenue with optimised differentiated pricing", horiz=TRUE, 
        names.arg=c("Base revenue", "Revenue w/ \noptimised pricing"), cex.names=0.8,
        xlim=c(0,900),  col = c("grey","navyblue"), xlab="Revenue in £1k",
        axes="F")


##Add text at specific coordinates
##Use trial and error to place them 
text(450, 1.9, "£861,696", col = "white")
text(450, 0.7, "£811,665", col = "white")
axis(side = 1, at = xticks)

#--------------Strategy 2 & 3---------------

# Capacity allocation & protection levels using current demand and pricing
mL=152 # Mean Demand for package
mH=23 # Mean Demand for walk-in
pL=79 # Price to golf from package
pH=120 # Price for walk-in
capacity=64 # Visitor capacity in package deal period
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:180){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:180){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for High-Fare Demand:", ProtectBest))

# Capacity allocation & protection using optimised price and demand numbers
mL_new=143 # Mean Demand for package
mH_new=20 # Mean Demand for walkin
pL_new=88 # Price for package
pH_new=125 # Price for walk in
capacity=64 # Visitor capacity in package deal period
ExpRevenue_new=rep(0,capacity+1)
for (h in 1:(capacity+1)){
  protect_new=h-1
  availforLowFare_new=capacity-protect_new;
  ExpRevenue_new[h]=0;
  for(dL_new in 0:180){
    soldLowFare_new=min(availforLowFare_new,dL_new)
    remainforHighFare_new=capacity-soldLowFare_new
    for(dH_new in 0:180){
      soldHighFare_new=min(remainforHighFare_new,dH_new)
      RevenueThisIter_new=pL_new*soldLowFare_new+pH_new*soldHighFare_new
      ExpRevenue_new[h]=ExpRevenue_new[h]+
        RevenueThisIter_new*dpois(dL_new,mL_new)*dpois(dH_new,mH_new)
    }
  }
}
Protectindexbest_new = which(ExpRevenue_new == max(ExpRevenue_new))
ProtectBest_new=Protectindexbest_new-1
OptimalExpRevenue_new=max(ExpRevenue_new)
print(paste("The Optimal Protection Level for High-Fare Demand:", ProtectBest_new))


#Graphing tee-times protected against revenue
xaxis=0:capacity

matplot(xaxis,cbind(ExpRevenue,ExpRevenue_new), main="Revenue per day vs Protection Limits",
        col=c("navyblue","darkorchid4"),pch = 16, cex = 0.5,las=1, xaxt="n",
        xlab="Walk-in tee-times protected",ylab="Expected Revenue £",
        ylim=c(2000,6500), xlim=c(0,75), axes="F")

xticks <- seq(0, 75, by=25)
yticks <- seq(2000, 8000, by=3000)

lines(c(ProtectBest,ProtectBest),c(0, max(ExpRevenue)),lty=2)
lines(c(ProtectBest_new,ProtectBest_new),c(0, max(ExpRevenue_new)),lty=2)
lines(c(0,ProtectBest),c(max(ExpRevenue), max(ExpRevenue)),lty=2)
lines(c(0,ProtectBest_new),c(max(ExpRevenue_new), max(ExpRevenue_new)),lty=2)

axis(side = 1, at = ProtectBest)
axis(side = 1, at = ProtectBest_new)
axis(side = 2, at = round(max(ExpRevenue),0),las=1)
axis(side = 2, at = round(max(ExpRevenue_new),0),las=1)
axis(side = 1, at = xticks)
axis(side = 2, at = yticks, las=2)

legend("topright", legend=c("Allocation w/Optimised Pricing",
                            "Original Suggested Allocation"),
       col=c('darkorchid4','navyblue'), lty=c(1, 1), lwd=2, cex = 0.8)


#______BAR CHART2 Revenue Comparison_____
# Fitting Labels
par(las=1) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
counts <- c(revenue_before/1000,(OptimalExpRevenue*153)/1000, OptimalExpRevenue_new*153/1000)

xticks <- seq(0, 1000, by=100)
barplot(counts, main="Revenue comparison w/ allocation &\n optimised pricing", horiz=TRUE, 
        names.arg=c("Base revenue", "Revenue w/ allocation",
                    "Revenue w/ allocation \n& optimized pricing"),
        cex.names=0.8, xlim=c(0,1000),  col = c("grey","navyblue","darkorchid4"),
        xlab="Revenue in £1k", axes="F")


##Add text at specific coordinates:
text(450, 3.1, "£944,748", col = "white")
text(450, 1.9, "£875,502", col = "white")
text(450, 0.7, "£811,665", col = "white")
axis(side = 1, at = xticks)

#with current system, 21 seats should be allocated for walk-in and
# 43 should be reserved for the package deal

#with new system, 17 seats should be allocated for walk-in and
# 47 should be reserved for the package deal

increase2 = round(OptimalExpRevenue_new*153-revenue_before, digits=2)
paste("By utilizing our new pricing model and decreasing the available package window,
      the trust can achieve an increase in revenue of: £", increase2, "per year.")




