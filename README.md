---
output:
  html_document: default
  pdf_document: default
---
# EV charger investment in Tacoma
* Cost-Benefit Analysis - Investor view
* Commission vs. incentive approaches
* Policy Design
* Optimization framework
* Equity & Efficiency Targeted Policies



# GIS social characteristics of Tacoma

It is known that EV onwers are more educated with higher income. Ye (2017) confirms, "Among the 15,408 survey respondents who specified their income, 77% had an annual household income of over $100,000. Among the 18,848 respondents who specified their education level, 34% of them obtained a Bachelor’s degree and 49% of them obtained a Graduate degree. Similarly, 81% of the respondents live in a single-family house; 75% of them are male, and 64% of them are white. According to this survey, EV owners are more likely to be male with relatively high income, possess a higher level of education attainment, and live in single family homes."

It is necessary in this regard to figure out the social characteristics of Tacoma for EV charger consideration. Hear are several maps showing them.

![City of Tacoma with MUD](./docs/figs/map02.jpg)

![Population](./docs/figs/pop.jpg)

![Rent household](./docs/figs/Rent.jpg)

![Nonfamily household](./docs/figs/Nonfamily.jpg)

![Commuters](./docs/figs/commute.jpg)

![Housing built before 2000](./docs/figs/built.jpg)

![Median household incomes](./docs/figs/income.jpg)

![Education](./docs/figs/education.jpg)

![Population age](./docs/figs/age.jpg)

![White race](./docs/figs/race.jpg)

![Rent households with No vehicle](./docs/figs/novehicle.jpg)


# Blocks with multi-family housings crossing the main area

A few blocks were chosen based on multi-family housings occupying the main area of the blocks. These blocks will be investigated against the rest of blocks to see any differences in terms of social characteristics.


14 variables (portion)

* Population density
* Male
* Age 35 to 64
* Non family household
* Education (Over bachelor)
* Rent household
* Median income
* Household income over $100,000
* Housing built before 2000
* Commuter more than 45 min to work
* Rent households with no vehicle
* Rent % income




![Selection of blocks](./docs/figs/selection.jpg){ height=800px }



```r
dat<-read.csv("./docs/raw/dat - Copy1.csv")
data= data.frame(matrix(NA,560,16))
data[,1]=dat[,2]
data[,2]=dat[,3]
data[,3]=dat[,6]
data[,4]=dat[,7]/dat[,5]
data[,5]=dat[,9]/dat[,5]
data[,6]=dat[,11]/dat[,5]
data[,7]=dat[,16]/dat[,14]
data[,8]=dat[,17]/dat[,14]
data[,9]=dat[,21]/dat[,18]
data[,10]=dat[,26]/dat[,14]
data[,11]=dat[,27]
data[,12]=dat[,32]/dat[,14]
data[,13]=dat[,36]/dat[,14]
data[,14]=dat[,38]
data[,15]=dat[,44]/dat[,42]
data[,16]=dat[,50]/dat[,49]
data[is.na(data)]=0
colnames(data)=c("Geoid","select","pop.density","male","age35_64","white","nonfamily","renter","master","over100,000","med.income","over20units","buitbf2000","rantpctincome","45min.commute","no.vechicle.renter")
# str(data)
```


```r
pairs(data[-c(1,2)])
```

![Independant variable plots](./docs/EV_files/figure-html/Fig1-1.png)

```r
library(corrplot)
```

```
## corrplot 0.84 loaded
```

```r
library(MASS)
```


```r
corrplot.mixed(cor(data[-c(1,2)]),upper="ellipse")
```

![Correlations of indipendeant variables](./docs/EV_files/figure-html/Fig2-1.png)

In the scope of this study the social characteristics of different blocks in Tacoma were analyzed and blocks with MUDs are likely to have higher population density and a higher proportion of rental housing. Therefore, in order to achieve social equity in EVSE accessibility, the government should consider suitable charging solutions for MUDs in addition to public, workplace and single-family residence charging. Moreover, based on a performed cluster analysis, two clusters of MUD-intense blocks were found to have different social characteristics. Compared to Cluster 1, Cluster 2 has a higher population density, lower average income, lower education level, higher building age and a higher proportion of rental housing. The government needs to develop different strategies to address EVSE installations for the two clusters.


```r
M1 <- glm(as.factor(select)~ . -Geoid,data, family=binomial())
# stepAIC(M1)
M2 <- glm(formula = as.factor(select) ~ pop.density + nonfamily + renter + no.vechicle.renter, family = binomial(), data = data)
s=summary(M2)

library(kableExtra)
kable(coef(s), caption = "Binomial logistic model [note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  kable_styling(latex_options = c("striped", "scale_down"))%>%
  add_footnote(c("AIC is 95.92"), notation = "symbol")
```


<table class="table table" style="margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<caption>Binomial logistic model <sup>*</sup></caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Std. Error </th>
   <th style="text-align:right;"> z value </th>
   <th style="text-align:right;"> Pr(&gt;|z|) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> -5.8072650 </td>
   <td style="text-align:right;"> 0.9263109 </td>
   <td style="text-align:right;"> -6.269240 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pop.density </td>
   <td style="text-align:right;"> 0.0001583 </td>
   <td style="text-align:right;"> 0.0000768 </td>
   <td style="text-align:right;"> 2.061436 </td>
   <td style="text-align:right;"> 0.0392615 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonfamily </td>
   <td style="text-align:right;"> -3.6263804 </td>
   <td style="text-align:right;"> 2.0501884 </td>
   <td style="text-align:right;"> -1.768803 </td>
   <td style="text-align:right;"> 0.0769267 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> renter </td>
   <td style="text-align:right;"> 3.3332370 </td>
   <td style="text-align:right;"> 1.2661521 </td>
   <td style="text-align:right;"> 2.632572 </td>
   <td style="text-align:right;"> 0.0084741 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> no.vechicle.renter </td>
   <td style="text-align:right;"> 4.3534712 </td>
   <td style="text-align:right;"> 1.8372864 </td>
   <td style="text-align:right;"> 2.369512 </td>
   <td style="text-align:right;"> 0.0178116 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> AIC is 95.92</td>
</tr>
</tfoot>
</table>

It is estimated that more population density, more portion of rent housing with higher portion of no vehicle households, and less portion of non family households or more portion of family households are more likely correlated with blocks with more multi-family households.

Other regression model was built excluding the 3 erratic bock groups as below.


```r
data1 = data[-c(6,120,545),]
M1 <- glm(as.factor(select)~ . -Geoid,data1, family=binomial())
# stepAIC(M1)
M2 <- glm(formula = as.factor(select) ~ pop.density + renter, family = binomial(), data = data1)
s=summary(M2)

library(kableExtra)
kable(coef(s), caption = "Binomial logistic model [note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  kable_styling(latex_options = c("striped", "scale_down"))%>%
  add_footnote(c("AIC is 72.75"), notation = "symbol")
```

<table class="table table" style="margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
<caption>Binomial logistic model <sup>*</sup></caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Std. Error </th>
   <th style="text-align:right;"> z value </th>
   <th style="text-align:right;"> Pr(&gt;|z|) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> -7.0291502 </td>
   <td style="text-align:right;"> 1.0766785 </td>
   <td style="text-align:right;"> -6.528551 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pop.density </td>
   <td style="text-align:right;"> 0.0001444 </td>
   <td style="text-align:right;"> 0.0000783 </td>
   <td style="text-align:right;"> 1.844661 </td>
   <td style="text-align:right;"> 0.0650868 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> renter </td>
   <td style="text-align:right;"> 3.7222820 </td>
   <td style="text-align:right;"> 1.5444505 </td>
   <td style="text-align:right;"> 2.410101 </td>
   <td style="text-align:right;"> 0.0159481 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>*</sup> AIC is 72.75</td>
</tr>
</tfoot>
</table>

Population density and portion of rent households are the main factor for MUD characteristics in Tacoma.


# Factor and cluster analysis

To verify the characteristics of those blocks chosen above, factor and cluster analysis was performed. Two factors were chosen and the first factor (PA1) is related to the variables: "non-family households", "buiding units over 20", "rent households","population density","rent households with no vehicle","housings built before 2000 year" and the rest of variables are in the 2nd factor (PA2).



```r
library(psych)
library (cluster)
library(reshape)
library(ggplot2)
library(som)
library(GPArotation)

da = data[,-c(1,2)]
da = data.matrix(da)

fa.parallel(da,fa="both",n.iter=100)
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```
## Parallel analysis suggests that the number of factors =  5  and the number of components =  3
```

```r
fa <- fa(da,nfactors=2,rotate="promax",fm="pa")
dat1 <- fa$scores
factor.plot(fa, labels=rownames(fa$loadings))
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
fa.diagram(fa,simple=F)
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
# dim(da)

kmeans<-kmeans(dat1,center=2)
# kmeans
# summary(kmeans) # slots in kmeans object

set.seed(5099)
sum(kmeans(dat1,center=1)$withinss)
```

```
## [1] 969.1251
```

```r
wss <- (nrow(dat1)-1)*sum(apply(dat1,2,var))
for (i in 2:9) wss[i] <- sum(kmeans(dat1,centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

```r
kmeans$centers # centroids of clusters
```

```
##          PA1        PA2
## 1 -0.5523910  0.5931719
## 2  0.7528367 -0.8084157
```

```r
#number of samples in each cluster
table(kmeans$cluster)
```

```
## 
##   1   2 
## 323 237
```

```r
kmeans$centers
```

```
##          PA1        PA2
## 1 -0.5523910  0.5931719
## 2  0.7528367 -0.8084157
```

```r
Group1<-kmeans$centers[1,]
Group2<-kmeans$centers[2,]
plot(Group1,Group2,type="n")
text(Group1,Group2, labels=colnames(dat))
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-5.png)<!-- -->

```r
pairs(dat1, col=kmeans$cluster)
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-6.png)<!-- -->

```r
clusplot(dat1, kmeans$cluster, color=TRUE,shade=TRUE, labels=5, lines=0)
```

![](./docs/EV_files/figure-html/unnamed-chunk-4-7.png)<!-- -->

```r
# library(rgl)
# plot3d(fa$scores, col = kmeans$cluster)
```

It is found the clusters are decided by the first factor (PA1). The first clustered group is more related to the higher value of PA1. Among the 11 chosen block groups with MUD, 3 block groups are clustered into the cluster 2nd group. We tried to go further analysis the difference between the 1st and the 2nd clustered group of the block groups chosen.


```r
GEOID_MUD = data[c(6,40,42,51,73,88,120,146,301,391,545),1]
GEOID_hetero = data[c(6,120,545), 1]

mud = data.frame(da[c(40,42,51,73,88,146,301,391),])
hetero = data.frame(da[c(6,120,545),])

par(mfrow=c(3,3))
for(i in 1:14) boxplot(mud[[i]], hetero[[i]], ylab= colnames(mud)[i], xlab="cluster 1, cluster 2")
```

![](./docs/EV_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](./docs/EV_files/figure-html/unnamed-chunk-5-2.png)<!-- -->



```r
summary(mud)
```

```
##   pop.density         male           age35_64          white       
##  Min.   : 3062   Min.   :0.3523   Min.   :0.1720   Min.   :0.4223  
##  1st Qu.: 5392   1st Qu.:0.4493   1st Qu.:0.2778   1st Qu.:0.4732  
##  Median : 8119   Median :0.4702   Median :0.3848   Median :0.5698  
##  Mean   : 8136   Mean   :0.4688   Mean   :0.3434   Mean   :0.5955  
##  3rd Qu.:10599   3rd Qu.:0.4974   3rd Qu.:0.4027   3rd Qu.:0.6908  
##  Max.   :12787   Max.   :0.5460   Max.   :0.4571   Max.   :0.8604  
##    nonfamily          renter           master          over100.000     
##  Min.   :0.2833   Min.   :0.4674   Min.   :0.003106   Min.   :0.02683  
##  1st Qu.:0.3104   1st Qu.:0.5332   1st Qu.:0.018097   1st Qu.:0.03581  
##  Median :0.4699   Median :0.6913   Median :0.031601   Median :0.10082  
##  Mean   :0.4288   Mean   :0.7194   Mean   :0.046371   Mean   :0.11098  
##  3rd Qu.:0.5166   3rd Qu.:0.9231   3rd Qu.:0.067806   3rd Qu.:0.17282  
##  Max.   :0.5400   Max.   :0.9787   Max.   :0.115686   Max.   :0.23667  
##    med.income     over20units       buitbf2000     rantpctincome  
##  Min.   :22727   Min.   :0.0000   Min.   :0.8405   Min.   :28.60  
##  1st Qu.:29696   1st Qu.:0.1211   1st Qu.:0.8879   1st Qu.:29.70  
##  Median :36070   Median :0.1542   Median :0.9551   Median :33.35  
##  Mean   :38050   Mean   :0.1639   Mean   :0.9331   Mean   :35.02  
##  3rd Qu.:45439   3rd Qu.:0.2114   3rd Qu.:0.9748   3rd Qu.:40.60  
##  Max.   :57031   Max.   :0.3256   Max.   :1.0000   Max.   :44.50  
##  X45min.commute    no.vechicle.renter
##  Min.   :0.06738   Min.   :0.05205   
##  1st Qu.:0.09991   1st Qu.:0.16511   
##  Median :0.10368   Median :0.20556   
##  Mean   :0.14269   Mean   :0.21595   
##  3rd Qu.:0.14302   3rd Qu.:0.25659   
##  Max.   :0.31761   Max.   :0.39344
```

```r
summary(hetero)
```

```
##   pop.density         male           age35_64          white       
##  Min.   : 3221   Min.   :0.4653   Min.   :0.2788   Min.   :0.1667  
##  1st Qu.: 4304   1st Qu.:0.4805   1st Qu.:0.3467   1st Qu.:0.3637  
##  Median : 5387   Median :0.4957   Median :0.4145   Median :0.5606  
##  Mean   : 6328   Mean   :0.5067   Mean   :0.3814   Mean   :0.5516  
##  3rd Qu.: 7881   3rd Qu.:0.5275   3rd Qu.:0.4328   3rd Qu.:0.7440  
##  Max.   :10374   Max.   :0.5592   Max.   :0.4510   Max.   :0.9274  
##    nonfamily          renter            master         over100.000    
##  Min.   :0.2004   Min.   :0.09643   Min.   :0.01205   Min.   :0.1206  
##  1st Qu.:0.2124   1st Qu.:0.37626   1st Qu.:0.03004   1st Qu.:0.1554  
##  Median :0.2244   Median :0.65610   Median :0.04803   Median :0.1902  
##  Mean   :0.2499   Mean   :0.49663   Mean   :0.11796   Mean   :0.2762  
##  3rd Qu.:0.2747   3rd Qu.:0.69673   3rd Qu.:0.17092   3rd Qu.:0.3541  
##  Max.   :0.3250   Max.   :0.73735   Max.   :0.29380   Max.   :0.5179  
##    med.income      over20units        buitbf2000     rantpctincome  
##  Min.   : 21250   Min.   :0.00000   Min.   :0.1070   Min.   :31.30  
##  1st Qu.: 39518   1st Qu.:0.03502   1st Qu.:0.4242   1st Qu.:33.40  
##  Median : 57786   Median :0.07004   Median :0.7415   Median :35.50  
##  Mean   : 59956   Mean   :0.07782   Mean   :0.6162   Mean   :35.07  
##  3rd Qu.: 79310   3rd Qu.:0.11673   3rd Qu.:0.8707   3rd Qu.:36.95  
##  Max.   :100833   Max.   :0.16341   Max.   :1.0000   Max.   :38.40  
##  X45min.commute    no.vechicle.renter
##  Min.   :0.06771   Min.   :0.03346   
##  1st Qu.:0.10956   1st Qu.:0.16488   
##  Median :0.15142   Median :0.29630   
##  Mean   :0.14371   Mean   :0.21458   
##  3rd Qu.:0.18171   3rd Qu.:0.30514   
##  Max.   :0.21199   Max.   :0.31398
```

It is also found the 2nd clustered group which has the lower value of PA1 has the noticeable characteristics with lower population density, lower non-family households, higher median income, lower unit in a building, etc.

This analysis is essential to make a decision to have EV chargers considering these different social characteristics with MUD especially addressing equity.



![Identifying Blocks](./docs/figs/mud.jpg)

![Clustered Blocks](./docs/figs/cluster.jpg)




# Cost-benefit analysis of charger

It is necessary to analyze how much it would benefit from installation of one charger. The benefit could be estimated by the fuel cost reduction as EV uses electricity. Assumping electricity price and gas price keep changing annually and annual discount factor is 6%, the cost - benefit analyses were constructed based on 4 cases. 1) onwer takes the installation cost of 1 charger and uses it, 2) owner takes the installation cost for the whole chargers (5 renters) and uses one of them, 3) owner doesn't live in the building, but pays the cost of all chargers (6 chargers) and get commission of 20% of the benefit generated from the chargers, and 4) with the case 3, there are additional 6 more EVs are using by sharing the 6 installed chargers. Net present value (NPV) and discounted payback period (DPP) are measured for each scenario. 

## Benefits

### Conventional vehicle
* VMT 10,230 miles
* MPG 33 miles/ gallon
* Gas required 310 gallons
* Gas unit price $2.49/ gallon
* Total cost $772

### EV
* Fuel economy 30 kWh/100 mile
* Elec. need per year 3069 kWh
* Elec. unit cost $0.077/ kWh
* Elec. cost $236.3

**Annual benefit for one car $535.7**
 
### Cost 
* Installation $2,050 (based on Megan's table)

After 100 times Monte Carlo simulations, it shows that case 1 takes less than 6 years to reach the break-even point, while case 2, over 20 years and case 3, about 18 years. This analysis indicates that government supports are necessary to benefit buiding owners for the same impact as the single family households. 

Moreover, MUD charger has potential as one charger could serve muliple EVs. The case 4 shows that it only takes about 10 years to reach the break-even point. Government support could be lessen compared to the case 3. 



```r
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
```



## case1, single owner - single user


```r
npv = c()
dpp = c()

for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst
  q=data.frame(matrix(NA,25,6))
  for(i in 1:25){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=rgs*gas - kw*rei
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot(dpp)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
colnames(q) = c("Gas price($/gallon)","Elec.price($/kWh)","FV($)","PV($)","NPV($)","DPP(year)")
q
```

```
##    Gas price($/gallon) Elec.price($/kWh)     FV($)    PV($)     NPV($)
## 1             2.708817        0.08377835  582.6174 549.6391 -1500.3609
## 2             2.954863        0.09171180  634.5441 564.7420  -935.6190
## 3             3.199790        0.09906988  687.8893 577.5651  -358.0538
## 4             3.478469        0.10864808  744.8843 590.0181   231.9643
## 5             3.819985        0.11805049  821.8984 614.1703   846.1346
## 6             4.167131        0.12846035  897.5659 632.7486  1478.8832
## 7             4.465540        0.14122454  950.8992 632.4023  2111.2854
## 8             4.812382        0.15355328 1020.5835 640.3267  2751.6121
## 9             5.198873        0.16590154 1102.4987 652.5673  3404.1794
## 10            5.662036        0.18205525 1196.5035 668.1213  4072.3007
## 11            6.144165        0.19802967 1296.9381 683.2108  4755.5115
## 12            6.595382        0.21393150 1388.0128 689.7998  5445.3113
## 13            7.224023        0.23182104 1527.9884 716.3806  6161.6919
## 14            7.882864        0.24811618 1682.2193 744.0472  6905.7391
## 15            8.557744        0.27087158 1821.5959 760.0883  7665.8274
## 16            9.209903        0.29009584 1964.7659 773.4228  8439.2502
## 17            9.915213        0.31789343 2098.1010 779.1601  9218.4103
## 18           10.706188        0.34231822 2268.3437 794.7001 10013.1104
## 19           11.472849        0.36904210 2423.9929 801.1612 10814.2716
## 20           12.319532        0.40592662 2573.2662 802.3566 11616.6282
## 21           13.437880        0.43769792 2822.4480 830.2383 12446.8665
## 22           14.524608        0.48062325 3027.5957 840.1732 13287.0397
## 23           15.962479        0.51833573 3357.5960 879.0094 14166.0492
## 24           17.209810        0.55472098 3632.6024 897.1749 15063.2240
## 25           18.879530        0.60350440 4000.4992 932.1108 15995.3349
##    DPP(year)
## 1   0.000000
## 2   0.000000
## 3   0.000000
## 4   3.606852
## 5   4.377687
## 6   6.337237
## 7   8.338517
## 8  10.297200
## 9  12.216595
## 10 14.095152
## 11 15.960533
## 12 17.894046
## 13 19.601143
## 14 21.281318
## 15 23.085443
## 16 24.911561
## 17 26.831215
## 18 28.599860
## 19 30.498247
## 20 32.478137
## 21 33.991920
## 22 35.814643
## 23 37.115924
## 24 38.789619
## 25 40.160336
```

```r
mean(npv)
```

```
## [1] 16436.91
```

```r
mean(dpp)
```

```
## [1] 3.616204
```

```r
par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-2-4.png)<!-- -->


## case2, one owner - 5 renters 


```r
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
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
plot(dpp)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
colnames(q) = c("Gas price($/gallon)","Elec.price($/kWh)","FV($)","PV($)","NPV($)","DPP(year)")
q
```

```
##    Gas price($/gallon) Elec.price($/kWh)     FV($)     PV($)       NPV($)
## 1             2.712655        0.08340358  584.9574  551.8466 -11748.15344
## 2             2.928734        0.08945977  633.3556  563.6842 -11184.46922
## 3             3.179087        0.09811344  684.4068  574.6411 -10609.82810
## 4             3.491802        0.10515526  759.7372  601.7831 -10008.04505
## 5             3.738334        0.11275526  812.8375  607.3995  -9400.64555
## 6             4.071284        0.12172904  888.5116  626.3656  -8774.27997
## 7             4.460843        0.13222851  977.0521  649.7954  -8124.48453
## 8             4.803775        0.14466448 1045.1949  655.7682  -7468.71635
## 9             5.161027        0.15583806 1121.6515  663.9038  -6804.81254
## 10            5.639265        0.17033787 1225.4052  684.2598  -6120.55270
## 11            6.134623        0.18248807 1341.6772  706.7788  -5413.77390
## 12            6.729782        0.19814110 1478.1373  734.5889  -4679.18495
## 13            7.208558        0.21657679 1569.9788  736.0673  -3943.11762
## 14            7.860620        0.23676904 1710.1479  756.4001  -3186.71754
## 15            8.561340        0.25489587 1871.7400  781.0117  -2405.70584
## 16            9.299462        0.27491470 2039.1201  802.6920  -1603.01380
## 17           10.090544        0.29825479 2212.7246  821.7272   -781.28661
## 18           10.846154        0.32192673 2374.3147  831.8264     50.53981
## 19           11.632831        0.35252427 2524.2806  834.3076    884.84738
## 20           12.537875        0.38710939 2698.7025  841.4682   1726.31556
## 21           13.505006        0.41499585 2912.9296  856.8540   2583.16954
## 22           14.753283        0.44571494 3205.6186  889.5755   3472.74505
## 23           15.848652        0.47831292 3445.1398  901.9282   4374.67322
## 24           17.432747        0.52411564 3795.6406  937.4418   5312.11502
## 25           19.120334        0.56183622 4203.0280  979.2998   6291.41480
## 26           20.613411        0.60206088 4542.4325  998.4722   7289.88702
## 27           22.465792        0.65171649 4964.2775 1029.4321   8319.31907
## 28           24.071289        0.70096857 5310.8271 1038.9579   9358.27694
## 29           25.911372        0.75772877 5707.0558 1053.2756  10411.55255
## 30           27.967962        0.82031382 6152.5251 1071.2169  11482.76949
##    DPP(year)
## 1    0.00000
## 2    0.00000
## 3    0.00000
## 4    0.00000
## 5    0.00000
## 6    0.00000
## 7    0.00000
## 8    0.00000
## 9    0.00000
## 10   0.00000
## 11   0.00000
## 12   0.00000
## 13   0.00000
## 14   0.00000
## 15   0.00000
## 16   0.00000
## 17   0.00000
## 18  17.93924
## 19  18.06058
## 20  20.05155
## 21  22.01471
## 22  23.90382
## 23  25.85036
## 24  27.66661
## 25  29.42440
## 26  31.30104
## 27  33.08146
## 28  35.00737
## 29  36.88493
## 30  38.71937
```

```r
mean(npv)
```

```
## [1] 21414.23
```

```r
mean(dpp)
```

```
## [1] 18.2751
```

```r
par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-3-4.png)<!-- -->



## case3, no onwer but only 6 renters living 

Here it is assumed that the owner doesn't live in the building while collecting a commission of 20% of profit generated from the charger installation. This commission could be interpreted as the increased rental or tax credits if government supports. 


```r
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
    fv=com*6*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
plot(dpp)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
colnames(q) = c("Gas price($/gallon)","Elec.price($/kWh)","FV($)","PV($)","NPV($)","DPP(year)")
q
```

```
##    Gas price($/gallon) Elec.price($/kWh)     FV($)     PV($)      NPV($)
## 1             2.709648        0.08401974  698.5613  659.0201 -11640.9799
## 2             2.919260        0.09087628  751.2857  668.6416 -10972.3383
## 3             3.191317        0.09770213  827.3526  694.6612 -10277.6771
## 4             3.501649        0.10568182  913.4085  723.5051  -9554.1720
## 5             3.754731        0.11560110  971.0244  725.6059  -8828.5661
## 6             4.068015        0.12620907 1048.4989  739.1503  -8089.4158
## 7             4.419561        0.13739502 1138.0783  756.8870  -7332.5288
## 8             4.823861        0.14832601 1248.2214  783.1496  -6549.3792
## 9             5.288867        0.16223161 1369.9919  810.8961  -5738.4831
## 10            5.716128        0.17453896 1483.6077  828.4388  -4910.0443
## 11            6.256245        0.19057369 1625.4785  856.2818  -4053.7625
## 12            6.872655        0.20776474 1791.4717  890.3066  -3163.4560
## 13            7.528693        0.22736551 1963.3320  920.4866  -2242.9693
## 14            8.133944        0.24833639 2111.2540  933.8097  -1309.1597
## 15            8.852600        0.26914879 2301.9459  960.5216   -348.6380
## 16            9.684812        0.29532931 2515.1114  990.0643    641.4262
## 17           10.437660        0.31914558 2707.4601 1005.4543   1646.8806
## 18           11.435041        0.34707650 2975.6219 1042.4906   2689.3712
## 19           12.515617        0.38139999 3251.1896 1074.5605   3763.9317
## 20           13.764150        0.41664957 3585.8267 1118.0777   4882.0094
## 21           14.999016        0.45600705 3900.2512 1147.2800   6029.2894
## 22           16.487921        0.50106764 4288.1746 1189.9903   7219.2797
## 23           17.684082        0.54221560 4581.6070 1199.4522   8418.7318
## 24           19.136553        0.59316552 4934.2876 1218.6632   9637.3950
## 25           20.578084        0.63615114 5312.2299 1237.7423  10875.1373
## 26           22.101893        0.68078798 5714.6984 1256.1480  12131.2853
## 27           23.879024        0.73361086 6181.2548 1281.7941  13413.0795
## 28           25.774433        0.79201516 6671.2555 1305.0987  14718.1781
## 29           27.648732        0.84999421 7154.9696 1320.4979  16038.6760
## 30           29.943366        0.92141591 7745.5418 1348.5773  17387.2533
##    DPP(year)
## 1    0.00000
## 2    0.00000
## 3    0.00000
## 4    0.00000
## 5    0.00000
## 6    0.00000
## 7    0.00000
## 8    0.00000
## 9    0.00000
## 10   0.00000
## 11   0.00000
## 12   0.00000
## 13   0.00000
## 14   0.00000
## 15   0.00000
## 16  15.35214
## 17  16.63795
## 18  18.57976
## 19  20.50276
## 20  22.36643
## 21  24.25529
## 22  26.06667
## 23  28.01881
## 24  29.90817
## 25  31.78627
## 26  33.65753
## 27  35.46430
## 28  37.27744
## 29  39.14593
## 30  40.89303
```

```r
mean(npv)
```

```
## [1] 26334.91
```

```r
mean(dpp)
```

```
## [1] 15.65745
```

```r
par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

## case4, 12 renters (sharing chargers) but rest conditions are the same as case3. 

This case, 2 EV owners are using one charger by sharing it given the same utilization rate as the case3. 


```r
npv = c()
dpp = c()
com = 0.2

for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst * 6
  q=data.frame(matrix(NA,20,6))
  for(i in 1:20){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=com*12*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(npv)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
plot(dpp)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
colnames(q) = c("Gas price($/gallon)","Elec.price($/kWh)","FV($)","PV($)","NPV($)","DPP(year)")
q
```

```
##    Gas price($/gallon) Elec.price($/kWh)    FV($)    PV($)      NPV($)
## 1             2.694964        0.08441211 1383.307 1305.007 -10994.9931
## 2             2.944724        0.09257842 1508.979 1342.986  -9652.0074
## 3             3.178140        0.09950404 1631.629 1369.947  -8282.0601
## 4             3.470896        0.10754221 1790.234 1418.033  -6864.0271
## 5             3.795707        0.11679844 1963.715 1467.402  -5396.6250
## 6             4.108283        0.12827266 2111.758 1488.706  -3907.9192
## 7             4.487345        0.13923268 2313.053 1538.312  -2369.6071
## 8             4.832763        0.15172808 2478.007 1554.732   -814.8749
## 9             5.245908        0.16411548 2694.146 1594.661    779.7861
## 10            5.734875        0.18011413 2940.098 1641.736   2421.5217
## 11            6.183330        0.19327714 3176.795 1673.496   4095.0177
## 12            6.667076        0.20708387 3435.007 1707.093   5802.1112
## 13            7.169104        0.22256056 3694.521 1732.136   7534.2471
## 14            7.684750        0.24016553 3948.491 1746.421   9280.6683
## 15            8.282255        0.26346796 4221.398 1761.442  11042.1103
## 16            9.076437        0.28952523 4620.342 1818.781  12860.8909
## 17            9.756064        0.31761900 4919.057 1826.763  14687.6536
## 18           10.470170        0.34567534 5243.700 1837.098  16524.7515
## 19           11.275461        0.37993249 5590.512 1847.737  18372.4886
## 20           12.320116        0.41204605 6131.200 1911.737  20284.2256
##    DPP(year)
## 1   0.000000
## 2   0.000000
## 3   0.000000
## 4   0.000000
## 5   0.000000
## 6   0.000000
## 7   0.000000
## 8   0.000000
## 9   8.511002
## 10  9.474977
## 11 11.446984
## 12 13.398825
## 13 15.349686
## 14 17.314107
## 15 19.268790
## 16 21.071161
## 17 23.040263
## 18 24.995030
## 19 26.943237
## 20 28.610364
```

```r
mean(npv)
```

```
## [1] 31217.16
```

```r
mean(dpp)
```

```
## [1] 8.5375
```

```r
par(mfrow= c(1,1))
plot(q[,3], main = "Cash flow", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3,cex.main=0.9)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("bottomright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
par(mfrow= c(1,2))
plot(q[,1], main = "Gas price", ylab="$/gallon", xlab="year",
     type = 'l', ylim = c(min(q[,1]),max(q[,1])), col = 'red',lwd=3,cex.main=0.9)

plot(q[,2], main = "Electricity price", ylab="$/kWh", xlab="year",
     type = 'l', ylim = c(min(q[,2]),max(q[,2])), col = 'red',lwd=3,cex.main=0.9)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-5-4.png)<!-- -->


# Commission vs. Incentive 

As the previous cost benefit analysis shows, in order to make the investment of chargers as
effective as for single family households, there should be support to MUD owners who need to
invest their private funds for the installation of chargers. Taking into account the previous case
analysis, a DPP of 5 years should be targeted if we aim at achieving a similar benefit as for
case 1. This section investigates how high the incentives would have to be to achieve this goal
for case 2, case 3 and case 4.

For case 2, the incentive would have to be at least 80% of the upfront cost. A figure below shows
that an incentive of 50% would still not be enough.


```r
npv = c()
dpp = c()
for(j in 1:100){
rgs=gas_c
rei=u_cost
cf=-inst *6*0.5
q=data.frame(matrix(NA,30,6))
for(i in 1:30){
rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
fv=(rgs*gas - kw*rei)
pv= fv/(1+rt)^i
old=cf
cf=cf+pv
dp=ifelse(cf>0,i-1+abs(old/pv),0)
q[i,]=c(rgs,rei,fv,pv,cf,dp)
}
npv[j]=sum(q[,4])-inst #net present value
dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
par(mfrow= c(1,2))
plot(q[,3], main = "Cash flow of incentive (50%)", ylab="$", xlab="year",
type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)


npv = c()
dpp = c()
com = 0.5
for(j in 1:100){
rgs=gas_c
rei=u_cost
cf=-inst * 6 * 0.2
q=data.frame(matrix(NA,30,6))
for(i in 1:30){
rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
fv=(rgs*gas - kw*rei)
pv= fv/(1+rt)^i
old=cf
cf=cf+pv
dp=ifelse(cf>0,i-1+abs(old/pv),0)
q[i,]=c(rgs,rei,fv,pv,cf,dp)
}
npv[j]=sum(q[,4])-inst #net present value
dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(q[,3], main = "Cash flow of incentive (80%)", ylab="$", xlab="year",
type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


For case 3, the results are very similar. Hence, both case 2 and case 3 would require an incentive to cover around 80% to receive a comparable DPP as single-family household or condo owners receive.


```r
npv = c()
dpp = c()
com=0.2
for(j in 1:100){
rgs=gas_c
rei=u_cost
cf=-inst *6*0.5
q=data.frame(matrix(NA,30,6))
for(i in 1:30){
rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
fv=com*6*(rgs*gas - kw*rei)
pv= fv/(1+rt)^i
old=cf
cf=cf+pv
dp=ifelse(cf>0,i-1+abs(old/pv),0)
q[i,]=c(rgs,rei,fv,pv,cf,dp)
}
npv[j]=sum(q[,4])-inst #net present value
dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
par(mfrow= c(1,2))
plot(q[,3], main = "Cash flow of incentive (50%)", ylab="$", xlab="year",
type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)


npv = c()
dpp = c()
com = 0.2
for(j in 1:100){
rgs=gas_c
rei=u_cost
cf=-inst * 6 * 0.2
q=data.frame(matrix(NA,30,6))
for(i in 1:30){
rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
fv=com*6*(rgs*gas - kw*rei)
pv= fv/(1+rt)^i
old=cf
cf=cf+pv
dp=ifelse(cf>0,i-1+abs(old/pv),0)
q[i,]=c(rgs,rei,fv,pv,cf,dp)
}
npv[j]=sum(q[,4])-inst #net present value
dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(q[,3], main = "Cash flow of incentive (80%)", ylab="$", xlab="year",
type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


To achieve the DPP of 5 years like the case 1, 1 owner single family, commission rate and incentive rate were estimated for each scenario: case 3, rental apartment (no owner living) and case 4, rental apartment (sharing chargers in addition to the case 3 above - 12 renters are sharing 6 installed chargers).

## Incentive comparison (case 3)

For the case 3, rental apartment, with the commission rate, 50%, it is estimated that about 40% incentive is required to meet the 5 year's DPP. It also shows there is not much improvement of shortening the DPP even if there is no incentive (in this case, the DPP is 8 years). We can see incentive supports a little from the investment perspectives. 


```r
par(mfrow = c(1,2))

npv = c()
dpp = c()
com = 0.5
for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst *6*0.6
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=com*6*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}

plot(q[,3], main = "Cash flow of incentive (40%) \n with 50% commission", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)


npv = c()
dpp = c()
com = 0.5
for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst * 6
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=com*6*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(q[,3], main = "Cash flow of incentive (0%) \n with 50% commission", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Commission comparison (case 4)

In case 4, rental apartment with the sharing chargers, a scenario of 50% commission rate and no incentive is estimated to have the same DPP of 5 years of the scenario of 20% commission rate and 60% incentive rate, which confirms that the impact of commission rate change is more than the incentive rate change on DPP. Policy makers can take this account to support investment of chargers.  


```r
npv = c()
dpp = c()
com = 0.2
for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst *6*0.4
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=com*12*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
par(mfrow= c(1,2))
plot(q[,3], main = "Cash flow of incentive (60%) \n with 20% commission", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)


npv = c()
dpp = c()
com = 0.5
for(j in 1:100){
  rgs=gas_c
  rei=u_cost
  cf=-inst * 6
  q=data.frame(matrix(NA,30,6))
  for(i in 1:30){
    rgs = rgs*(1+runif(1,0.07,0.1)) #annual gas price increase
    rei = rei*(1+runif(1,0.07,0.1)) #annual elec. price increase
    fv=com*12*(rgs*gas - kw*rei)
    pv= fv/(1+rt)^i
    old=cf
    cf=cf+pv
    dp=ifelse(cf>0,i-1+abs(old/pv),0)
    q[i,]=c(rgs,rei,fv,pv,cf,dp)
  }
  npv[j]=sum(q[,4])-inst #net present value
  dpp[j]=min(q[,6][q[,6]>0]) #discounted payback period
}
plot(q[,3], main = "Cash flow of incentive (0%) \n with 50% commission", ylab="$", xlab="year",
     type = 'l', ylim = c(min(q[,5]),max(q[,5])), col = 'red',lwd=3)
lines(q[,4], type = 'l', col='blue',lwd=3)
lines(q[,5], type = 'l', col='orange',lwd=3)
legend("topright", legend=c("Annual FV","Annual PV","NPV"),col=c("red","blue","orange"), lty=1, box.lty = 0, lwd=7,cex=0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


Adding a 60% incentive of upfront cost to case 4, shows a DPP of 5 years, which can also be achieved through changing the commission fee to 50%. It also confirms that commissions are more effective to reach the goal of a shorter DPP than incentivizing the upfront cost.

Furthermore, it was found that sharing chargers by two, is as effective as collecting commissions. By increasing the number of users for a given charger we can enhance the mechanism for owners to reach shorter DPP. By optimizing the utilization rate of chargers depending upon the local conditions, it can reduce the commissioning rate while getting the same result and thus deliver added value to both the renter and the landlord.


# Policy optimization

We start our policy effect analysis based on a case that assumes the installation of stand-alone level 2 chargers (\$2,050 installation cost) with a budget limited to 100 chargers (i.e. $205,000), For the eleven MUD block groups that were defined in the cluster analysis for Tacoma we find three of them related to the higher income cluster (1) while cluster 2 is more related to the characteristics of the remaining 8 block groups. To keep this analysis within simple bounds, we only consider the ownership structure cases 1 and 2 from the cost benefit analysis. 

Based on the identified renter and owner distribution structure of the MUD block clusters 1 and 2, the cluster 1 (i.e. 3 block groups) are assumed to include 30% of MUDs with the case 1 structure and 70% of the case 2 structure; cluster 2 (8 block groups) is assumed to have 10% of the case 1 structure and 90% of the case 2 structure.

The aim of the following analysis is to check how different social or rent household structures in MUDs affect the outcome of the selected policy application in terms of efficiency and equity. We considered a cash flow time horizon of 20 years to receive an average expected NPV per unit charger. Furthermore, based on the population of each block group, each of them was weighted in proportion to its population assuming that the EVSE demand is proportional to the population. 

These benefit weights were added to the objective functions of the maximization problem. The policy parameter α was set to different values depending on the policy goals. The efficiency-oriented policy seeks to maximize the benefit by focusing more on efficiency. By concentrating on cA_1, cA_2 and cA_3, which happen to be all cluster 1, the maximized benefit in terms of saved energy turns out to be $446,695 for a 20-year time horizon. This is the maximum benefit from installing 100 chargers. Thus, focusing on these block groups would maximize the efficiency. 

On the other hand, the equity policy seeks to maximize the benefit while focusing more on equity. The optimal solution was found and the expected benefit in terms of saved energy is estimated to $78,191 over a 20-year time horizon. This benefit is far lower than for the efficiency policy while it improves the equal resource allocation for all MUD block groups regardless of which cluster they are belong to. 



```r
library(nloptr)
c = 2050 #cost for one charger
m = 100*c #capacity
n = 11 #number of places for demand
npv = c(11741.5,1601) # (-5000,1500) #single,
c1 = c(0.3,0.7)
c2 = c(0.1,0.9)
cc1= t(c1)%*%npv
cc2= t(c2)%*%npv
b = c(rep(cc1,3),rep(cc2,8)) #benefit per cluster
pop=c(0.049,0.095,0.112,0.099,0.067,0.128,0.062,0.076,0.053,0.154,0.105) #population weight
bp=pop*b #total benefit

names(pop) = c("cA_1","cA_2","cA_3","cB_1","cB_2","cB_3","cB_4","cB_5","cB_6","cB_7","cB_8")
barplot(pop, col = "orange", main = "Population proportion in MUD blocks", 
        xlab="MUDs", ylab="Relative portion of population",cex.names = 0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Objective function EQUITY
alpha = 1 # fair
eval.f <- function(x) {return(-sum(b*log(x)))} #max

# Constraint function
# to be of the form g(x) <= 0
eval.g <- function(x) {
  return(sum(c*x) - m)
}

x0 <- rep(9,11)

opts <- list('algorithm' = 'NLOPT_GN_ISRES', 'xtol_rel' = 1.0e-8, 'maxeval' = 100000)

eq = nloptr(
  x0 = x0,
  eval_f = eval.f,
  lb = rep(0,11),
  ub = rep(100,11),
  eval_g_eq = eval.g,
  opts = opts
)

par(mfrow=c(1,2))
bar1 = eq$solution
names(bar1) = c("cA_1","cA_2","cA_3","cB_1","cB_2","cB_3","cB_4","cB_5","cB_6","cB_7","cB_8")
barplot(bar1, col = "blue", main = "Equity oriented policy", 
        xlab="MUDs", ylim = c(0,80),ylab="number of chargers",cex.names = 0.7)


# Objective function Efficiency
alpha = 0 
eval.f <- function(x) {return (-sum(b*x^(1-alpha)/(1-alpha)))} #max



# Constraint function
# to be of the form g(x) <= 0
eval.g <- function(x) {
  return(sum(c*x) - m)
}

x0 <- rep(9,11)

opts <- list('algorithm' = 'NLOPT_GN_ISRES', 'xtol_rel' = 1.0e-8, 'maxeval' = 100000)

ef = nloptr(
  x0 = x0,
  eval_f = eval.f,
  lb = rep(0,11),
  ub = rep(100,11),
  eval_g_eq = eval.g,
  opts = opts
)

bar = ef$solution
names(bar) = c("cA_1","cA_2","cA_3","cB_1","cB_2","cB_3","cB_4","cB_5","cB_6","cB_7","cB_8")
barplot(bar, col = "red", main = "Efficiency oriented policy", 
        xlab="MUDs", ylim = c(0,80), ylab="number of chargers",cex.names = 0.7)
```

![](./docs/analysis_files/figure-html/unnamed-chunk-10-2.png)<!-- -->



