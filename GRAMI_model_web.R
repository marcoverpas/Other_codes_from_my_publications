# This model replicates the baseline and the experiments discussed in
# Veronese Passarella, M. (2025) "Destabilising a Stable Economy: Minsky
# Meets Graziani’s Monetary Circuit", International Journal of Political
# Economy

# Last update: 03/06/2025

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prepare environment ####

# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

# Upload libraries
library(progress)
library(openxlsx)

# Import initial values ####
data <- read.xlsx("https://www.dropbox.com/scl/fi/i698mfk8fgyivqudolpck/Initial_values_GRAMI_web.xlsx?rlkey=te7khikgweqi0hjwo5xe5wpk9&st=wbo0t92e&dl=1", sheet = "Sheet1", rows = 1:97, cols = 1:2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Choose type of shock ####
shock_type<-1 #Note: 1 = interest rate increase; 2 = higher autonomous consumption

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Define time span, scenarios, industries and iterations ####
nPeriods1<-200
nPeriods2<-nPeriods1 
nIndustries<-3
nScenarios<-4
max_iterations<-100
tolerance<-0.000001

#Set parameters and exogenous variables ####
delta<-0.1                                                  #Depreciation rate of capital stock
pr<-1                                                       #Labor productivity
mum<-0                                                      #Risk premium on deposits
mub<-0                                                      #Risk premium on securities
w<-0.2                                                      #Wage rate 
alpha10w<-0.74                                              #Autonomous propensity to consume out of wages
alpha11w<-(alpha10w - 0.7)/0.02                             #Interest rate elasticity of workers' consumption (negative, note: "-" sign in equation)
alpha10z<-0.397 #(calibr. to make mon. pol. neutral in l/r) #Autonomous propensity to consume out of profits 
alpha11z<-(alpha10z - 0.4)/0.02                             #Interest rate elasticity of rentiers' consumption (positive, note: "-" sign in equation)
alpha2w<-0.15                                               #Propensity to consume out of workers' wealth
alpha2z<-0.08                                               #Propensity to consume out of capitalists' wealth
lambdaw<-0.5                                                #Workers' share of deposits to total wealth (liquidity preference)  
lambdaz<-0.5                                                #Capitalists' share of deposits to total wealth (liquidity preference)  
gamma<-0.22 #Note: crucial for amplitude of business cycle  #Speed of adjustment of current investment to target level
mup11<-0.001                                                #Sensitivity of consumer goods markup to output gap 
mup12<-0.001                                                #Sensitivity of luxury goods markup to output gap
mup13<-0.001                                                #Sensitivity of investment goods markup to output gap
omega<-0.3                                                  #Share of managerial salaries to total labour incomes
chi<-0.01                                                   #Target percentage of real investment to be funded by share issues
if(shock_type==1){
  gamma1<-0.216 #(chosen to generate fluctuations)          #Coefficient defining (positive) elasticities of target capital to output ratios  
  gamma2<-0.2205 #(chosen to generate fluctuations)         #Coefficient defining (negative) elasticities of target capital to output ratios
}else{
  gamma1<-0.011                                                            
  gamma2<-0.0125                                                            
}
alpha_lev<-4                                                #Coefficient defining positive elasticities of target capital to output ratios
gamma3<-0.004                                               #Coefficient in subjective expectations of investors
random_num<-0                                               #Initial value of random coefficient

#Set values of coefficients that are shocked ####
alpha0w<-matrix(data=data[1,2],nrow=nScenarios,ncol=nPeriods2)            #Autonomous consumption of workers
alpha0z<-matrix(data=data[2,2],nrow=nScenarios,ncol=nPeriods2)            #Autonomous consumption of capitalists
alpha1w<-matrix(data=0,nrow=nScenarios,ncol=nPeriods2)                    #Propensity to consume out of wages
alpha1z<-matrix(data=0,nrow=nScenarios,ncol=nPeriods2)                    #Propensity to consume out of profits 
r_bar<-matrix(data=0.02,nrow=nScenarios,ncol=nPeriods2)                   #Policy rate

#Define 2D variables as matrices ####
yg<-matrix(data=data[6,2],nrow=nScenarios,ncol=nPeriods2)                 #Nominal gross output
yn<-matrix(data=data[7,2],nrow=nScenarios,ncol=nPeriods2)                 #Nominal net output (or net income or value added)
c<-matrix(data=data[17,2],nrow=nScenarios,ncol=nPeriods2)                 #Total nominal consumption
cw<-matrix(data=data[18,2],nrow=nScenarios,ncol=nPeriods2)                #Real consumption of workers
cz<-matrix(data=data[19,2],nrow=nScenarios,ncol=nPeriods2)                #Real consumption of rentiers
id<-matrix(data=data[20,2],nrow=nScenarios,ncol=nPeriods2)                #Real investment
da<-matrix(data=data[21,2],nrow=nScenarios,ncol=nPeriods2)                #Real depreciation of capital
af<-matrix(data=data[22,2],nrow=nScenarios,ncol=nPeriods2)                #Amortization funds
pw<-matrix(data=data[38,2],nrow=nScenarios,ncol=nPeriods2)                #Average price of workers' consumption
pz<-matrix(data=data[39,2],nrow=nScenarios,ncol=nPeriods2)                #Average price of rentiers' consumption
pid<-matrix(data=data[40,2],nrow=nScenarios,ncol=nPeriods2)               #Average price of investment
ydw<-matrix(data=data[50,2],nrow=nScenarios,ncol=nPeriods2)               #Disposable income of workers
wb<-matrix(data=data[51,2],nrow=nScenarios,ncol=nPeriods2)                #Wage bill
paymw_m<-matrix(data=data[52,2],nrow=nScenarios,ncol=nPeriods2)           #Interest payments on deposits held by workers
paymw_b<-matrix(data=data[53,2],nrow=nScenarios,ncol=nPeriods2)           #Interest payments on securities held by workers       
paym_l<-matrix(data=data[54,2],nrow=nScenarios,ncol=nPeriods2)            #Interest payments on loans       
ydz<-matrix(data=data[55,2],nrow=nScenarios,ncol=nPeriods2)               #Disposable income of capitalists
pb<-matrix(data=data[56,2],nrow=nScenarios,ncol=nPeriods2)                #Profit of banks
pf<-matrix(data=data[57,2],nrow=nScenarios,ncol=nPeriods2)                #Profit of firms
paymz_m<-matrix(data=data[58,2],nrow=nScenarios,ncol=nPeriods2)           #Interest payments on deposits held by rentiers
paym_m<-matrix(data=data[59,2],nrow=nScenarios,ncol=nPeriods2)            #Total interest payments on deposits
paymz_b<-matrix(data=data[60,2],nrow=nScenarios,ncol=nPeriods2)           #Interest payments on securities held by rentiers 
paym_b<-matrix(data=data[61,2],nrow=nScenarios,ncol=nPeriods2)            #Total interest payments on securities 
yd<-matrix(data=data[62,2],nrow=nScenarios,ncol=nPeriods2)                #Total disposable income
vw<-matrix(data=data[63,2],nrow=nScenarios,ncol=nPeriods2)                #Net wealth of workers
vz<-matrix(data=data[64,2],nrow=nScenarios,ncol=nPeriods2)                #Net wealth of rentiers
vh<-matrix(data=data[65,2],nrow=nScenarios,ncol=nPeriods2)                #Total net wealth
bw<-matrix(data=data[66,2],nrow=nScenarios,ncol=nPeriods2)                #Demand for private securities by workers
mw<-matrix(data=data[67,2],nrow=nScenarios,ncol=nPeriods2)                #Bank deposits held by workers by workers
bz<-matrix(data=data[68,2],nrow=nScenarios,ncol=nPeriods2)                #Demand for private securities by capitalists
mz<-matrix(data=data[69,2],nrow=nScenarios,ncol=nPeriods2)                #Bank deposits held by workers by capitalists
bh<-matrix(data=data[70,2],nrow=nScenarios,ncol=nPeriods2)                #Total demand for private securities
mh<-matrix(data=data[71,2],nrow=nScenarios,ncol=nPeriods2)                #Total bank deposits held by workers
bs<-matrix(data=data[72,2],nrow=nScenarios,ncol=nPeriods2)                #Supply private securities
k<-matrix(data=data[73,2],nrow=nScenarios,ncol=nPeriods2)                 #Real stock of fixed capital  
kt<-matrix(data=data[74,2],nrow=nScenarios,ncol=nPeriods2)                #Target stock of capital  
kn<-matrix(data=data[75,2],nrow=nScenarios,ncol=nPeriods2)                #Nominal value of capital stock  
fin_i<-matrix(data=data[76,2],nrow=nScenarios,ncol=nPeriods2)             #Initial finance             
fin_f<-matrix(data=data[77,2],nrow=nScenarios,ncol=nPeriods2)             #Final finance
ld<-matrix(data=data[78,2],nrow=nScenarios,ncol=nPeriods2)                #Demand for loans  
ls<-matrix(data=data[79,2],nrow=nScenarios,ncol=nPeriods2)                #Supply of loans
ms<-matrix(data=data[80,2],nrow=nScenarios,ncol=nPeriods2)                #Supply of deposits
rm<-matrix(data=data[81,2],nrow=nScenarios,ncol=nPeriods2)                #Interest rate on deposits
rl<-matrix(data=data[82,2],nrow=nScenarios,ncol=nPeriods2)                #Interest rate on loans
rb<-matrix(data=data[83,2],nrow=nScenarios,ncol=nPeriods2)                #Rate of return on securities  
n<-matrix(data=data[87,2],nrow=nScenarios,ncol=nPeriods2)                 #Demand for labour (employment)
q<-matrix(data=data[91,2],nrow=nScenarios,ncol=nPeriods2)                 #Tobin's q
pbs<-matrix(data=data[92,2],nrow=nScenarios,ncol=nPeriods2)               #Price of private securities
bsr<-matrix(data=data[93,2],nrow=nScenarios,ncol=nPeriods2)               #Quantity of private securities issued by the firms
bwr<-matrix(data=data[94,2],nrow=nScenarios,ncol=nPeriods2)               #Quantity of private securities held by the workers
bzr<-matrix(data=data[95,2],nrow=nScenarios,ncol=nPeriods2)               #Quantity of private securities held by the rentiers
bhr<-matrix(data=data[96,2],nrow=nScenarios,ncol=nPeriods2)               #Quantity of private securities held by the households
cgw<-matrix(data=data[97,2],nrow=nScenarios,ncol=nPeriods2)               #Workers' capital gains on firms' securities 
cgz<-matrix(data=data[98,2],nrow=nScenarios,ncol=nPeriods2)               #Rentiers' capital gains on firms' securities 
lev<-matrix(data=0.964,nrow=nScenarios,ncol=nPeriods2)                    #Normalised leverage ratio (Minsky-like)
kappaM<-matrix(data=data[20,2],nrow=nScenarios,ncol=nPeriods2)            #Sensitivity of investment to deviations of q from 1
mul<-matrix(data=0.01,nrow=nScenarios,ncol=nPeriods2)                     #Risk premium on loans
mul1<-matrix(data=0,nrow=nScenarios,ncol=nPeriods2)                       #Elasticity of risk to leverage ratio
mul0<-matrix(data=-0.05,nrow=nScenarios,ncol=nPeriods2)                   #Risk premium on loans: autonomous component (note: negative to allow for a higher mul1)
test<-matrix(data=0,nrow=nScenarios,ncol=nPeriods2)

#Define matrix of technical coefficients and related matrices
A=t(matrix(data=c(data[26,2],data[29,2],data[32,2],
                  data[27,2],data[30,2],data[33,2],
                  data[28,2],data[31,2],data[34,2]),
           nrow=nIndustries,ncol=nIndustries))                    #Matrix of technical coefficients
I = diag(x=1, nrow=nIndustries, ncol=nIndustries)                 #Diagonal matrix
I_col<-matrix(data=1,nrow=nScenarios,ncol=nPeriods2)              #Column vector of 1s

#Define 3D variables as arrays ####
kappa<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))      #Target capital to output ratios
x<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))          #Real gross output
xN<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))         #Natural output
phi<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))        #Speed of adjustment of natural output to current one
d<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))          #Final demand in real terms
p<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))          #Unit prices
betaw<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))      #Shares of real consumption of workers
betaz<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))      #Shares of real consumption of rentiers
iota<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Shares of real investment in fixed capital
pr<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))         #Vector of labour productivities
mup<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))        #Vector of markups
mup0<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Vector of initial markups
mup1<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Vector of sensitivities of markups to output gaps
fin_i_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))    #Initial finance in each industry             
fin_f_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))    #Final finance in each industry
pf_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Firms profit in each industry
cost_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))     #Total cost in each industry
rev_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))      #Total revenue in each industry
k_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))        #Real capital stock in each industry
kt_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Target capital stock in each industry
id_j<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))       #Real investment in each industry
gamma0<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))     #Subjective expectations component in investment decisions
wd<-array(data=0,dim=c(nScenarios,nPeriods2,nIndustries))         #Share of industry-related value added to total value added

# Initialise 3D variables
for (z in 1:nIndustries){
  kappa[,,z] <- data[2+z,2]
  x[,,z] <- data[7+z,2]
  xN[,,z] <- data[10+z,2]
  phi[,,z] <- data[13+z,2]
  d[,,z] <- data[22+z,2]
  p[,,z] <- data[34+z,2]
  betaw[,,z] <- data[40+z,2]
  betaz[,,z] <- data[43+z,2]
  iota[,,z] <- data[46+z,2]
  pr[,,z] <- data[83+z,2]
  mup0[,,z] <- data[87+z,2]
  mup1[,,1] <- mup11
  mup1[,,2] <- mup12
  mup1[,,3] <- mup13
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a progress bar ####
pbar <- progress_bar$new( total = nScenarios * nPeriods2,
                          format = "Progress [:bar] :percent ETA: :eta")

#Define scenarios ####
for (j in 1:nScenarios){
  
  #Define time span ####
  for (i in 2:nPeriods2){
    
    #Set seeds for pseudo-random components every 2 periods (different intensities across shocks)
    if ((i - 2) %% 2 == 0) {
      set.seed(i)
      
      if (shock_type==1){
      random_num <- runif(1,min=-0.12,max=0.12)  
      }else{
      random_num <- runif(1,min=-0.05,max=0.05)
      }
      
    }
    
    # Update the progress bar ####
    pbar$tick()
    
    #Shock 1: policy rate increase in t = 50 ####
    if(i>50 & shock_type==1 & j>=2){
      r_bar[j,i]=0.04 # from 0.02
    }
    
    #Shock 2: aut. consumption increase in t = 50 ####
    if(i>50 & shock_type==2 & j>=2){
      alpha0z[j,i]=2.5 #from 2
    }
    
    #Define iterations for converging to simultaneous solution ####
    for (iterations in 1:max_iterations){
      
      #Model equations ####  
      test[j,i] = random_num
      
      #1) Industrial structure
      d[j,i,] = betaw[j,i,]*cw[j,i] + betaz[j,i,]*cz[j,i] + iota[j,i,]*id[j,i]    #Final demand vector in real terms
      x[j,i,] = solve(I-A) %*% d[j,i,]                                            #Real gross output
      yg[j,i] = t(p[j,i,]) %*% x[j,i,]                                            #Value of gross output for the whole economy
      yn[j,i] = t(p[j,i,]) %*% d[j,i,]                                            #Value added of the economy
      
      #2) Price setting
      p[j,i,1]  =  ((            p[j,i,2]*A[2] + p[j,i,3]*A[3])*(1 + kappa[j,i,1] * delta) * (1+mup[j,i,1]) 
                    + w/pr[j,i,1]) /(1 - A[1] * (1+mup[j,i,1]))                       #Price of consumer goods 
      p[j,i,2]  =  ((p[j,i,1]*A[4]             + p[j,i,3]*A[6])*(1 + kappa[j,i,2] * delta) * (1+mup[j,i,2]) 
                    + w/pr[j,i,2]) /(1 - A[5] * (1+mup[j,i,2]))                       #Price of luxury goods
      p[j,i,3]  =  ((p[j,i,1]*A[7] + p[j,i,2]*A[8]            )*(1 + kappa[j,i,3] * delta) * (1+mup[j,i,3]) 
                    + w/pr[j,i,3]) /(1 - A[9] * (1+mup[j,i,3]))                       #Price of investment goods
      mup[j,i,] = mup0[j,i,] + mup1[j,i,] * (x[j,i-1,] - xN[j,i-1,])              #Mark-ups over direct costs
      xN[j,i,] = xN[j,i-1,] + phi[j,i,]*(x[j,i-1,]-xN[j,i-1,])                    #Potential output
      pw[j,i] = t(p[j,i,]) %*% betaw[j,i,]                                        #Average price of workers' consumption
      pz[j,i] = t(p[j,i,]) %*% betaz[j,i,]                                        #Average price of rentiers' consumption
      pid[j,i] = t(p[j,i,]) %*% iota[j,i,]                                        #Average price of investment
      
      #3) Households
      ydw[j,i] = wb[j,i]*(1-omega) + paymw_m[j,i] + paymw_b[j,i]                  #Disposable income of workers
      ydz[j,i] = (wb[j,i]*omega + pb[j,i] + pf[j,i] + paymz_m[j,i]
                  + paymz_b[j,i])                                                 #Disposable income of capitalists
      yd[j,i] =  ydw[j,i] + ydz[j,i]                                              #Total disposable income of households
      vw[j,i] = vw[j,i-1] + ydw[j,i] - cw[j,i]*pw[j,i]                            #Stock of wealth of workers 
      vz[j,i] = vz[j,i-1] + ydz[j,i] - cz[j,i]*pz[j,i]                            #Stock of wealth of capitalists 
      vh[j,i] = vw[j,i] + vz[j,i]                                                 #Total stock of wealth of workers
      mw[j,i] = vw[j,i] - bw[j,i]                                                 #Demanded stock of deposits (hoarding) by workers 
      mz[j,i] = vz[j,i] - bz[j,i]                                                 #Demanded stock of deposits (hoarding) by capitalists 
      mh[j,i] = mw[j,i] + mz[j,i]                                                 #Total demanded stock of deposits (hoarding)
      cw[j,i] = (alpha0w[j,i] + alpha1w[j,i]*ydw[j,i]/pw[j,i] +
                   alpha2w*vw[j,i-1]/pw[j,i])                                     #Real consumption of workers
      cz[j,i] = (alpha0z[j,i] + alpha1z[j,i]*ydz[j,i]/pz[j,i] +
                   alpha2z*vz[j,i-1]/pz[j,i])                                     #Real consumption of capitalists
      alpha1w[j,i] = alpha10w - alpha11w*r_bar[j,i-1]                             #Workers' propensity to consume out of income
      alpha1z[j,i] = alpha10z - alpha11z*r_bar[j,i-1]                             #Rentiers' propensity to consume out of income 
      
      #4) Security issues and portfolio investment
      pbs[j,i] = ((1-lambdaw)*vw[j,i] + (1-lambdaz)*vz[j,i])/bhr[j,i]             #Unit price of private securities                        
      bsr[j,i] = bsr[j,i-1] + chi*pid[j,i]*id[j,i]/pbs[j,i]                       #Quantity of private securities supplied by the firms  
      bhr[j,i] = bsr[j,i]                                                         #Quantity of private securities held by the workers 
      bwr[j,i] = bhr[j,i]*(1-lambdaw)*vw[j,i]/((1-lambdaw)*vw[j,i]
                                               + (1-lambdaz)*vz[j,i])                                             #Quantity of private securities held by the workers
      bzr[j,i] = bhr[j,i] - bwr[j,i]                                              #Quantity of private securities held by the rentiers
      bs[j,i] = bsr[j,i]*pbs[j,i]                                                 #Nominal stock of private securities held by the households
      bh[j,i] = bw[j,i] + bz[j,i]                                                 #Nominal stock of private securities issued by the firms
      bw[j,i] = bwr[j,i] * pbs[j,i]                                               #Nominal stock of private securities held by the workers
      bz[j,i] = bzr[j,i] * pbs[j,i]                                               #Nominal stock of private securities held by the rentiers 
      cgw[j,i] = bwr[j,i-1]*(pbs[j,i] - pbs[j,i-1])                               #Capital gains of workers 
      cgz[j,i] = bzr[j,i-1]*(pbs[j,i] - pbs[j,i-1])                               #Capital gains of rentiers 
      
      #5A) Firms
      kt[j,i] = (t(p[j,i-1,]) %*% (kappa[j,i-1,] * x[j,i-1,]))/ pid[j,i]          #Target capital stock
      id[j,i] = gamma*(kt[j,i] - k[j,i-1]) + da[j,i]                              #Real investment in fixed capital
      da[j,i] = delta * k[j,i-1]                                                  #Depreciation allowances in real terms
      k[j,i] = k[j,i-1] + id[j,i] - da[j,i]                                       #Fixed capital stock in real terms
      kn[j,i] = kn[j,i-1] + pid[j,i]*id[j,i] - af[j,i]                            #Current nominal value of capital stock
      af[j,i] = da[j,i] * pid[j,i-1]                                              #Amortization funds
      pf[j,i] = (yn[j,i] - paym_l[j,i] - af[j,i] - paymz_b[j,i] +
                   - paymw_b[j,i] - wb[j,i])                                       #Total profit of firms
      
      #5B) Alternative investment function (Minsky-like) 
      for (z in 1:nIndustries){
      wd[j,i,z] = (d[j,i,z]*p[j,i,z]) / yn[j,i]                                   #Share of industry-specific value added to total value added 
      }
      q[j,i] = (pbs[j,i]*bs[j,i])/(pid[j,i]*k[j,i])                               #Modified Tobin's q (no loans)
      lev[j,i] = (pbs[j,i]*bs[j,i] + ld[j,i])/(
                  pbs[j,i]*bs[j,i] + ld[j,i] + pf[j,i])                           #Minsky-like leverage ratio (normalised)      
      
      # Scenarios 3 and 4 (combined)
      if (j %in% c(3, 4)) { 
        if (i < 51) {
          mul1[j,i] = (mul[j,i] - mul0[j,i])/lev[j,i-1]
          kappa[j,i,] = kappa[j,i-1,]
        } else {
          # Base calculation for both scenarios
          if (j == 3) kappa[j,i,] = kappa[j,i-1,] + gamma0[j,i,] + gamma1*wd[j,i,]*lev[j,i-1] - gamma2*wd[j,i,]*(lev[j,i-1])^alpha_lev
          # Additional term only for scenario 4
          if (j == 4) kappa[j,i,] = kappa[j,i-1,] + gamma0[j,i,] + (gamma1+random_num)*wd[j,i,]*lev[j,i-1] - (gamma2+random_num)*wd[j,i,]*(lev[j,i-1])^alpha_lev
          
          if (q[j,i] > q[j,i-1]) {
            gamma0[j,i,] = gamma3 * wd[j,i,] * q[j,i-1]
          } else { 
            gamma0[j,i,] = 0
          }
          mul1[j,i] = mul1[j,i-1]
          mul[j,i] = mul0[j,i] + mul1[j,i]*lev[j,i-1]  # Endogenous risk premium
        }
      }
      
      #6A) Banks, initial finance and final funding - macro level
      fin_i[j,i] = wb[j,i] + pid[j,i]*id[j,i]                                     #Initial finance to production (including purchase of investment goods)
      fin_f[j,i] = (cw[j,i]*pw[j,i] + cz[j,i]*pz[j,i] + pid[j,i]*id[j,i] 
                    + (bs[j,i] - bs[j,i-1]) +
                      - (paym_l[j,i] + paymw_b[j,i] + paymz_b[j,i] + pf[j,i]))     #Final finance obtained by firms (including purchase of investment goods)
      ld[j,i] = ld[j,i-1] + fin_i[j,i] - fin_f[j,i]                               #Stock of debt (bank loans) of firms at the end of the period
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1])                                 #Supply of bank loans
      ms[j,i] = ms[j,i-1] + (ls[j,i] - ls[j,i-1])                                 #Supply of bank deposits    
      pb[j,i] = paym_l[j,i] - paym_m[j,i]                                         #Bank profit
      rm[j,i] = r_bar[j,i] + mum                                                  #Rate of interest on deposits
      rl[j,i] = r_bar[j,i] + mul[j,i]                                             #Rate of interest on bank loans
      rb[j,i] = r_bar[j,i]  + mub                                                 #Rate of interest on private securities
      
      #6B) Industry-specific finance, investment and profit - meso level
      fin_i_j[j,i,] = (w*x[j,i-1,]/pr[j,i-1,]) + pid[j,i]*id_j[j,i,]              #Initial finance by industry
      if(id[j,i]==0){
        fin_f_j[j,i,] = 0}else{
          fin_f_j[j,i,] = p[j,i,]*d[j,i,] - pf_j[j,i,] + ((bs[j,i] - bs[j,i-1]) - (   
            paym_l[j,i] + paymw_b[j,i] + paymz_b[j,i]))*(
              id_j[j,i,]/id[j,i])}                                                #Final finance by industry
      kt_j[j,i,] = p[j,i-1,]*kappa[j,i-1,]*x[j,i-1,]/pid[j,i-1]                   #Target capital stock by industry 
      id_j[j,i,] = gamma*(kt_j[j,i,]-k_j[j,i-1,])+delta*k_j[j,i-1,]               #Real investment by industry
      k_j[j,i,] = k_j[j,i-1,] + gamma*(kt_j[j,i,] - k_j[j,i-1,])                  #Real capital stock by industry
      if(id[j,i]==0){
        rev_j[j,i,] = 0
        cost_j[j,i,] = 0}else{
          rev_j[j,i,] = p[j,i,]*d[j,i,]                                             #Total revenue by industry
          cost_j[j,i,] = (w*x[j,i-1,]/pr[j,i-1,]) + (af[j,i] +
                                                       + paym_l[j,i] + paymz_b[j,i] +
                                                       + paymw_b[j,i])*(id_j[j,i,]/id[j,i])                         #Total cost by industry
          pf_j[j,i,] = rev_j[j,i,] - cost_j[j,i,]  }                                #Profit by industry
      
      #7) Employment and wages
      wb[j,i] = w*n[j,i]                                                          #Wage bill  
      n[j,i] = t(x[j,i-1,]) %*% (I_col[j,i]/pr[j,i-1,])                           #Labor demand (with a lag to avoid simultaneity)
      
      #8) Interest payments 
      if (mh[j,i-1]==0){paym_l[j,i] = rl[j,i-1]*ld[j,i-1]} else{
        paym_l[j,i] = (rl[j,i-1]*ld[j,i-1] + (mw[j,i-1]/mh[j,i-1])
                       *rl[j,i-1]*fin_f[j,i-1]/2)}                                 #Interest payments on bank loans (including average interest payments on repaid share of loans)
      if (mh[j,i-1]==0){paymw_m[j,i] = rm[j,i-1]*mw[j,i-1]} else{
        paymw_m[j,i] = (rm[j,i-1]*mw[j,i-1] + (mw[j,i-1]/mh[j,i-1])
                        *rm[j,i-1]*fin_f[j,i-1]/2)}                                #Interest payments on bank deposits (see above) to workers
      paymz_m[j,i] = rm[j,i-1]*mz[j,i-1]                                          #Interest payments on bank deposits (see above) to capitalists
      paym_m[j,i] = paymw_m[j,i] + paymz_m[j,i]                                   #Total interest payments on bank deposits
      paymw_b[j,i] = rb[j,i-1]*bw[j,i-1] + cgw[j,i]                               #Interest payments and capital gains on private securities received by the workers
      paymz_b[j,i] = rb[j,i-1]*bz[j,i-1] + cgz[j,i]                               #Interest payments and capital gains on private securities received by the capitalists
      paym_b[j,i] = paymw_b[j,i] + paymz_b[j,i]                                   #Total interest payments and capital gains on private securities
      
      #9) Hidden equation
      #ms[j,i]=mh[j,i]                                                            #Equilibrium condition on market of bank deposits
      
      # Check the stopping condition for iterations
      if (abs(ms[j,i] - mh[j,i]) < tolerance) {
        break
      }
    }
  }
}

# Close the progress bar
pbar$terminate()

#Create consistency statement ####
aerror=0
error=0

for (i in 2:(nPeriods2-1)){error = error + (ms[j,i]-mw[j,i]-mz[j,i])^2}
aerror = error/nPeriods2
if ( aerror<0.01 ){cat(" *********************************** \n Good news! The model is watertight! \n", "Average error =", aerror, "< 0.01 \n", "Cumulative error =", error, "\n ***********************************")} else{
  if ( aerror<1 && aerror<1 ){cat(" *********************************** \n Minor issues with model consistency \n", "Average error =", aerror, "> 0.01 \n", "Cumulative error =", error, "\n ***********************************")}
  else{cat(" ******************************************* \n Warning: the model is not fully consistent! \n", "Average error =", aerror, "> 1 \n", "Cumulative error =", error, "\n *******************************************")} }      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create charts ####

#Calculate positions for yearly ticks (every 4 periods)
year_breaks <- seq(1, nPeriods1, by = 4)  
year_labels <- seq(0, (nPeriods1 - 1) / 4)

#Change layout
layout(matrix(c(1:16), 4, 4, byrow = TRUE))
par(mar = c(5.1+1, 4.1+1, 4.1+1, 2.1+1))
par(oma = c(0, 0, 3, 0))

#Create colours
mycol2 <- rgb(255,0,0, max = 255, alpha = 20)
mycol3 <- rgb(0, 0, 255, max = 255, alpha = 80)

#Plot 1
plot(mh[1,40:nPeriods1]-ms[1,40:nPeriods1],type="l",col=6,ylim=range(-1,1),font.main=1,main="a) Consistency check: \n hidden equation",ylab = '$',xlab = 'Years', xaxt = 'n')
axis(1, at = year_breaks*10, labels = year_labels*10)
lines(mh[2,40:nPeriods1]-ms[2,40:nPeriods1],col=6)
lines(mh[3,40:nPeriods1]-ms[3,40:nPeriods1],col=6)
rect(xleft=-5,xright=205,ybottom=-0.5,ytop=0.5,col=mycol2,border=NA)
abline(h=c(-0.5,0.5),col=1)

# Add a title above all plots
if (shock_type==1){title="Schock 1: Increase in policy rate"}else{title="Shock 2: Increase in autonomous consumption"}
mtext(title, outer = TRUE, cex = 1, font = 1)

#Plot 2
plot(yn[2,40:nPeriods1], type="l", col=2, font.main=1,
     main="b) Net value added", ylab = '$', xlab = 'Years',
     ylim=range(min(yn[3,40:nPeriods1],yn[4,40:nPeriods1]),max(yn[3,40:nPeriods1],yn[4,40:nPeriods1])), xaxt = 'n')
lines(yn[3,40:nPeriods1], col=4)
lines(yn[4,40:nPeriods1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 3
plot(d[2,40:nPeriods1,1],type="l",col=2,font.main=1,
     main="c) Real dem. for consumer goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(d[3,40:nPeriods1,1],d[4,40:nPeriods1,1]),max(d[3,40:nPeriods1,1],d[4,40:nPeriods1,1])), xaxt = 'n')
lines(d[3,40:nPeriods1,1],col=4)
lines(d[4,40:nPeriods1,1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 4
plot(d[2,40:nPeriods1,2],type="l",col=2,font.main=1,
     main="d) Real dem. for luxury goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(d[4,40:nPeriods1,2],d[2,40:nPeriods1,2]),max(d[4,40:nPeriods1,2],d[2,40:nPeriods1,2])), xaxt = 'n')
lines(d[3,40:nPeriods1,2],col=4)
lines(d[4,40:nPeriods1,2], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 5
plot(d[2,40:nPeriods1,3],type="l",col=2,font.main=1,
     main="e) Real dem. for investment goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(d[3,40:nPeriods1,3],d[4,40:nPeriods1,3]),max(d[3,40:nPeriods1,3],d[4,40:nPeriods1,3])), xaxt = 'n')
lines(d[3,40:nPeriods1,3],col=4)
lines(d[4,40:nPeriods1,3], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 6
plot(x[2,40:nPeriods1,1],type="l",col=2,font.main=1,
     main="f) Gross output: consumer goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(x[4,40:nPeriods1,1],x[2,40:nPeriods1,1]),max(x[4,40:nPeriods1,1],x[2,40:nPeriods1,1])), xaxt = 'n')
lines(x[3,40:nPeriods1,1],col=4)
lines(x[4,40:nPeriods1,1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 7
plot(x[2,40:nPeriods1,2],type="l",col=2,font.main=1,
     main="g) Gross output: luxury goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(x[4,40:nPeriods1,2],x[3,40:nPeriods1,2]),max(x[4,40:nPeriods1,2],x[3,40:nPeriods1,2])), xaxt = 'n')
lines(x[3,40:nPeriods1,2],col=4)
lines(x[4,40:nPeriods1,2], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 8
plot(x[2,40:nPeriods1,3],type="l",col=2,font.main=1,
     main="h) Gross output: investment goods",ylab = 'Index',xlab = 'Years',
     ylim=range(min(x[4,40:nPeriods1,3],x[3,40:nPeriods1,3]),max(x[4,40:nPeriods1,3],x[2,40:nPeriods1,3])), xaxt = 'n')
lines(x[3,40:nPeriods1,3],col=4)
lines(x[4,40:nPeriods1,3], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 9
plot(id[2,40:nPeriods1],type="l",col=2,font.main=1,
     main="i) Investment in fixed capital",ylab = '$',xlab = 'Years',
     ylim=range(min(id[4,40:nPeriods1],id[3,40:nPeriods1]),max(id[4,40:nPeriods1],id[3,40:nPeriods1])), xaxt = 'n')
lines(id[3,40:nPeriods1],col=4)
lines(id[4,40:nPeriods1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 10
plot(100*rl[2,40:nPeriods1],type="l",col=2,font.main=1,
     main="j) Interest rate on loans",ylab = '%',xlab = 'Years',
     ylim=range(min(100*rl[4,40:nPeriods1],100*rl[3,40:nPeriods1]),max(100*rl[4,40:nPeriods1],100*rl[3,40:nPeriods1])), xaxt = 'n')
lines(100*rl[3,40:nPeriods1],col=4)
lines(100*rl[4,40:nPeriods1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 11
plot(lev[2,40:nPeriods1]-lev[1,40:nPeriods1],type="l",col=2,font.main=1,
     main="k) Leverage rate",ylab = 'Index (change)',xlab = 'Years',
     ylim=range(min(lev[4,40:nPeriods1]-lev[1,40:nPeriods1],lev[3,40:nPeriods1]-lev[1,40:nPeriods1]),max(lev[4,40:nPeriods1]-lev[1,40:nPeriods1],lev[3,40:nPeriods1]-lev[1,40:nPeriods1])), xaxt = 'n')
lines(lev[3,40:nPeriods1]-lev[1,40:nPeriods1],col=4)
lines(lev[4,40:nPeriods1]-lev[1,40:nPeriods1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 12
plot(q[2,40:nPeriods1]-q[1,40:nPeriods1],type="l",col=2,font.main=1,
     main="l) Minsky's price ratio",ylab = 'Index (change)',xlab = 'Years',
     ylim=range(min(q[4,40:nPeriods1]-q[1,40:nPeriods1],q[3,40:nPeriods1]-q[1,40:nPeriods1]),max(q[4,40:nPeriods1]-q[1,40:nPeriods1],q[3,40:nPeriods1]-q[1,40:nPeriods1])), xaxt = 'n')
lines(q[3,40:nPeriods1]-q[1,40:nPeriods1],col=4)
lines(q[4,40:nPeriods1]-q[1,40:nPeriods1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 13
plot(fin_i[2,40:nPeriods1],type="l",col=2,font.main=1,
     main="m) Initial and final finance: total",ylab = '$',xlab = 'Years',
     ylim=range(min(fin_i[2,40:nPeriods1],fin_i[3,40:nPeriods1]),max(fin_i[2,40:nPeriods1],fin_i[3,40:nPeriods1])), xaxt = 'n')
lines(fin_i[3,40:nPeriods1],col=4)
lines(fin_f[2,40:nPeriods1],col="darkred", lty=2)
lines(fin_f[3,40:nPeriods1],col="midnightblue", lty=2)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani (init.)","Minsky (init.)","Graziani (fin.)","Minsky (fin.)"),  bty = 'n', cex=1, lty=c(1,1,2,2), lwd=c(1,1,1,1), col = c(2,4,"darkred","midnightblue"), box.lty=0)
#abline(v=10,col="black")

#Plot 14
plot(fin_i_j[2,40:nPeriods1,1],type="l",col=2,font.main=1,
     main="n) Initial finance: consumer goods",ylab = '$',xlab = 'Years',
     ylim=range(min(fin_i_j[4,40:nPeriods1,1],fin_i_j[3,40:nPeriods1,1]),max(fin_i_j[4,40:nPeriods1,1],fin_i_j[2,40:nPeriods1,1])), xaxt = 'n')
lines(fin_i_j[3,40:nPeriods1,1],col=4)
lines(fin_i_j[4,40:nPeriods1,1], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 14
plot(fin_i_j[2,40:nPeriods1,2],type="l",col=2,font.main=1,
     main="o) Initial finance: luxury goods",ylab = '$',xlab = 'Years',
     ylim=range(min(fin_i_j[4,40:nPeriods1,2],fin_i_j[3,40:nPeriods1,2]),max(fin_i_j[4,40:nPeriods1,2],fin_i_j[2,40:nPeriods1,2])), xaxt = 'n')
lines(fin_i_j[3,40:nPeriods1,2],col=4)
lines(fin_i_j[4,40:nPeriods1,2], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)

#Plot 15
plot(fin_i_j[2,40:nPeriods1,3],type="l",col=2,font.main=1,
     main="p) Initial finance: investment goods",ylab = '$',xlab = 'Years',
     ylim=range(min(fin_i_j[4,40:nPeriods1,3],fin_i_j[3,40:nPeriods1,3]),max(fin_i_j[4,40:nPeriods1,3],fin_i_j[2,40:nPeriods1,3])), xaxt = 'n')
lines(fin_i_j[3,40:nPeriods1,3],col=4)
lines(fin_i_j[4,40:nPeriods1,3], col=mycol3)
axis(1, at = year_breaks*10, labels = year_labels*10)
legend("bottom",c("Graziani","Minsky","Minsky plus"),  bty = 'n', cex=1, lty=c(1,1,1), lwd=c(1,1), col = c(2,4,mycol3), box.lty=0)
