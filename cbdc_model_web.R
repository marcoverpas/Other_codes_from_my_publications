# CBDC-SFC model --- web version

# Created by mvp. Last change on: 12/12/2025

# Prepare environment and other panes ####

# Clear Environment 
rm(list=ls(all=TRUE))

# Clear Plots 
if(!is.null(dev.list())) dev.off()

# Clear Console 
cat("\014")

# Load the progress package
library(progress)
library(openxlsx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define time span ####
nPeriods = 50

# Define number of scenarios ####
nScenarios = 6

# Define max iterations
MaxIterations <- 100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model identification: upload initial values ####

source1 <- "https://www.dropbox.com/scl/fi/bsp8de3okkx3kos3elzl6/Tables-from-ENECO-2024.xlsx?rlkey=p58l8u6fwmry4ymhy61wktrc6&st=1n34ln6l&dl=1"

source2 <- "https://www.dropbox.com/scl/fi/mxkc7p7gjl97cw3jpnb62/CBDC-example.xlsx?rlkey=v8g0key141s7nkgvblowde0kg&st=k3oze92v&dl=1"

# Upload initial values of stocks ####
bs_data <- read.xlsx(source1, sheet = "BS_Matrix", rows = 1:11, cols = 2:8)
rownames(bs_data) <- c("Cash and res.",
                       "Deposits",
                       "CBDCs",
                       "Loans",
                       "Advances",
                       "T-bills",
                       "Dom. Sec.",
                       "For. Sec.",
                       "Capital stock",
                       "Net wealth")

# Upload initial values of flows ####
tfm_data <- read.xlsx(source1, sheet = "TFM_Matrix", rows = 1:22, cols = 2:9)
rownames(tfm_data) <- c("Consumption",
                        "Investment",
                        "Government spending",
                        "Export",
                        "Import",
                        "[GDP]",
                        "Wages",
                        "Deprec. / Amort.",
                        "Firms profit",
                        "Banks profit",
                        "Tax revenue",
                        "Interests on reserves",
                        "Interests on deposits",
                        "Interests on CBDCs",
                        "Interests on loans",
                        "Interests on advances",
                        "Interests on T-bills",
                        "Interests on domestic sec.s",
                        "Interests on foreign sec.s",
                        "Seigniorage income",
                        "Change in net wealth")

# Upload initial values of interest rates ####
int_data <- read.xlsx(source1, sheet = "TFM_Matrix", rows = 2:8, cols = 13, colNames = FALSE)
rownames(int_data) <- c("r_star",
                        "rl",
                        "rm",
                        "re",
                        "rb",
                        "rr",
                        "rq")
colnames(int_data) <- "Rates"

# Upload stock values after CBDC experiment ####
bs_data00 <- read.xlsx(source2, rows = 1:11, cols = 2:8)
rownames(bs_data00) <- c("Cash and res.",
                         "Deposits",
                         "CBDCs",
                         "Loans",
                         "Advances",
                         "T-bills",
                         "Dom. Sec.",
                         "For. Sec.",
                         "Capital stock",
                         "Net wealth")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define equilibrium coefficients (or set them = 0)
deltaw<-0                                                #Workers' repayment rate of loans 
deltaz<-0                                                #Rentiers' repayment rate of loans 
delta<-tfm_data[8,4]/bs_data[9,3]                        #Depreciation rate of capital stock
kappa<-bs_data[9,3]/tfm_data[6,3]                        #Target capital to output ratio
r_bar0<-int_data[1,]                                     #Policy rate 
mum0<-int_data[3,]-r_bar0                                #Initial value of risk premium (or cut) on deposits
mub<-int_data[5,]-r_bar0                                 #Risk premium on government securities
mue0<-int_data[4,]-r_bar0                                #Initial value of risk premium on private securities
mue1<-1                                                  #Profit elasticity to risk premium on private securities
mue00<-((int_data[4,]-r_bar0) +
       - (mue1*tfm_data[9,2]/(-bs_data[7,3])))           #Autonomous component of risk premium on private securities
muf<-int_data[7,]-r_bar0                                 #Risk premium on foreign bills 
mul<-int_data[2,]-r_bar0                                 #Risk premium on loans
mur<-int_data[6,]-r_bar0                                 #Risk premium (or cut) on reserves at CB
mua<-0                                                   #Risk premium (or cut) on advances from CB
omega<-0.05                                              #Share of managerial salaries to total labour incomes
pi_t<-0                                                  #Target inflation rate 
pm_t<-1                                                  #Exogenous price level 
rho1<-(1/2)*bs_data[1,5]/(-bs_data[2,5])                 #Reserve requirement
rho2<-(1/2)*bs_data[1,5]/(-bs_data[2,5])                 #Extra reserve ratio
lambdacw<-bs_data[1,1]/(-tfm_data[1,1])                  #Workers' cash to consumption ratio  
lambdacz<-bs_data[1,2]/(-tfm_data[1,2])                  #Rentiers' cash to consumption ratio  
lambda40w<-bs_data[8,1]/(-bs_data[10,1])                 #Workers' share of foreign bills to total wealth  
lambda40z<-bs_data[8,2]/(-bs_data[10,2])                 #Rentiers' share of foreign bills to total wealth  
lambda20w<-0                                             #Workers' share of gov. securities to total wealth  
lambda20z<-0                                             #Rentiers' share of gov. securities to total wealth  
lambda30z<-0                                             #Rentiers' share of private securities to total wealth
lambda30w<-0                                             #Workers' share of private securities to total wealth 
alpha1z<-0                                               #Propensity to consume out of profits 
n0<-2215.128                                             #Initial value of employment
w<--tfm_data[7,3]/n0                                     #Wage rate
pr <- tfm_data[6,3]/n0                                   #Product per unit of labour
mup<-pr/w-1                                              #Percentage markup on direct costs (for p=1)

# Set free parameters and exogenous variables ####
alpha1w_1<-4                                             #Interest rate elasticity of workers' propensity to consume out of income
alpha2w<-0.07                                            #Propensity to consume out of workers' wealth
alpha2z<-0.05                                            #Propensity to consume out of rentiers' wealth
eta<-0         #Note: eta=0 avoids growth                #Share of undistributed corporate profit (in addition to AF)
gamma<-0.15                                              #Speed of adjustment of current investment to target level
lambda21w<--0.05                                         #Workers' elasticity of gov. sec. demand to deposit rate
lambda21z<--0.05                                         #Rentiers' elasticity of gov. sec. demand to deposit rate
lambda22w<-0.1                                           #Workers' elasticity of gov. sec. demand to gov. sec. rate
lambda22z<-0.1                                           #Rentiers' elasticity of gov. sec. demand to gov. sec. rate
lambda23w<--0.05                                         #Workers' elasticity of gov. sec. demand to private sec. rate 
lambda23z<--0.05                                         #Rentiers' elasticity of gov. sec. demand to private sec. rate 
lambda24w<--0.01                                         #Workers' elasticity of gov. sec. demand to liquidity
lambda24z<--0.01                                         #Rentiers' elasticity of gov. sec. demand to liquidity
lambda31z<--0.1                                          #Rentiers' elasticity of private sec. demand to deposit rate
lambda32z<--0.1                                          #Rentiers' elasticity of private sec. demand to gov. sec. rate
lambda33z<-0.1                                           #Rentiers' elasticity of private sec. demand to private sec. rate
lambda34z<--0.02                                         #Rentiers' elasticity of private sec. demand to liquidity
lambda31w<--0.1                                          #Workers' elasticity of private sec. demand to deposit rate
lambda32w<--0.1                                          #Workers' elasticity of private sec. demand to gov. sec. rate
lambda33w<-0.1                                           #Workers' elasticity of private sec. demand to private sec. rate
lambda34w<--0.02                                         #Workers' elasticity of private sec. demand to liquidity
sigmaw1<-0.0                                             #Weight of regressive component of expectations of workers 
sigmaw2<-0.1                                             #Adjustment coefficient of workers' inflation expectations 
sigmaz1<-0.0                                             #Weight of regressive component of expectations of rentiers 
sigmaz2<-0.1                                             #Adjustment coefficient of rentiers' inflation expectations 
thetaw0<-0.11                                            #Autonomous component in workers' gross loans to disposable income ratio 
thetaw1<-0.5                                             #Elasticity of Lw/YDw to interest rate 
thetaz0<-0.11                                            #Autonomous component in rentiers' gross loans to disposable income ratio 
thetaz1<-0.5                                             #Elasticity of Lz/YDz to interest rate 
nu1<-1.2       #(based on J curve)                       #Price elasticity of import
nu2<-0.8       #(based on J curve)                       #Income elasticity of import
nu0<-0.3783811                                           #Autonomous import (calculated to match im=im0)
eps0<--2.1     #(Godley and Lavoie 2007)                 # Autonomous export
eps1<-1.2      #(based on J curve)                       #Price elasticity of export
eps2<-1        #(Godley and Lavoie 2007)                 #Foreign income elasticity of export
yf0<-4754.263                                            #Initial value of foreign income (calculated to match ex=ex0)
deltarm<-0.15                                            #Speed of adjustment of interest on deposits
deltarl<-0.50                                            #Speed of adjustment of interest on bank loans
deltare<-0.50                                            #Speed of adjustment of interest on private securities
deltarb<-0.50                                            #Speed of adjustment of interest on government securities
deltara<-0.70                                            #Speed of adjustment of interest on advances from CB
deltarr<-0.70                                            #Speed of adjustment of interest on reserves at CB
deltarq<-0.01                                            #Speed of adjustment of interest rate on foreign securities
deltardc<-0.70                                           #Speed of adjustment of interest rate on CBDCs

# Set values of coefficients that are shocked ####
alpha0w<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)            #Autonomous consumption of workers
alpha0z<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)            #Autonomous consumption of rentiers
tauww<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Average tax rate on wage incomes
tauwz<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Average tax rate on managerial salaries
tauz<-matrix(data=0.3,nrow=nScenarios,ncol=nPeriods)             #Average tax rate on capital incomes
tauv<-matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)            #Average tax rate on wealth
gov0<-matrix(data=tfm_data[3,3],nrow=nScenarios,ncol=nPeriods)   #Autonomous government spending
mum<-matrix(data=mum0,nrow=nScenarios,ncol=nPeriods)             #Risk premium on deposits
mudc<-matrix(data=mum,nrow=nScenarios,ncol=nPeriods)             #Risk premium on CBDC (initial value)
r_bar<-matrix(data=r_bar0,nrow=nScenarios,ncol=nPeriods)         #Policy rate
alpha1w<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)            #Propensity to consume out of wages

# Define other variables as matrices ####

# Flows ####
mue<-matrix(data=mue0,nrow=nScenarios,ncol=nPeriods)               #Risk premium on private securities
af<-matrix(data=tfm_data[8,4],nrow=nScenarios,ncol=nPeriods)       #Amortization funds
c<-matrix(data=tfm_data[1,3],nrow=nScenarios,ncol=nPeriods)        #Total nominal consumption
cab<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Current account balance
cw<-matrix(data=-tfm_data[1,1],nrow=nScenarios,ncol=nPeriods)      #Real consumption of workers
cz<-matrix(data=-tfm_data[1,2],nrow=nScenarios,ncol=nPeriods)      #Real consumption of rentiers
da<-matrix(data=af,nrow=nScenarios,ncol=nPeriods)                  #Real depreciation allowance
def<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Government deficit
ex<-matrix(data=tfm_data[4,3],nrow=nScenarios,ncol=nPeriods)       #Real export
gov<-matrix(data=tfm_data[3,3],nrow=nScenarios,ncol=nPeriods)      #Government spending
id<-matrix(data=tfm_data[2,3],nrow=nScenarios,ncol=nPeriods)       #Real investment
im<-matrix(data=-tfm_data[5,3],nrow=nScenarios,ncol=nPeriods)      #Real import
yf<-matrix(data=yf0,nrow=nScenarios,ncol=nPeriods)                 #Foreign GDP
n<-matrix(data=n0,nrow=nScenarios,ncol=nPeriods)                   #Demand for labour (employment)
pB<-matrix(data=tfm_data[10,2],nrow=nScenarios,ncol=nPeriods)      #Profit of banks
paym_a<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Interest payments on advances from CB
paym_b<-matrix(data=-tfm_data[17,5],nrow=nScenarios,ncol=nPeriods) #Total interest payments on government bills
paym_dc<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Total interest payments on CBDC
paym_e<-matrix(data=-tfm_data[18,3],nrow=nScenarios,ncol=nPeriods) #Total interest payments on private securities
paym_h<-matrix(data=-tfm_data[15,1]-tfm_data[15,2],
              nrow=nScenarios,ncol=nPeriods)                       #Total interest payments on personal loans
paym_l<-matrix(data=-tfm_data[15,3],nrow=nScenarios,ncol=nPeriods) #Interest payments on loans to firms
paym_m<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Total interest payments on deposits
paym_q<-matrix(data=tfm_data[19,2],nrow=nScenarios,ncol=nPeriods)  #Total interest payments on foreign bills
paym_r<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Interest payments on reserves at CB
paymcb_b<-matrix(data=tfm_data[17,7],nrow=nScenarios,ncol=nPeriods)#Interest payments on government bills held by CB
paymb_b<-matrix(data=tfm_data[17,6],nrow=nScenarios,ncol=nPeriods) #Interest payments on government bills held by banks
paymf_b<-matrix(data=tfm_data[17,8],nrow=nScenarios,ncol=nPeriods) #Interest payments on government bills held by foreign sector
paymw_b<-matrix(data=tfm_data[17,1],nrow=nScenarios,ncol=nPeriods) #Interest payments on government bills held by workers
paymw_dc<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)             #Interest payments on CBDC held by workers
paymw_e<-matrix(data=tfm_data[18,1],nrow=nScenarios,ncol=nPeriods) #Interest payments on securities held by workers
paymw_h<-matrix(data=-tfm_data[15,1],nrow=nScenarios,ncol=nPeriods)#Workers' interest payments on personal loans
paymw_m<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Interest payments on deposits held by workers
paymw_q<-matrix(data=tfm_data[19,1],nrow=nScenarios,ncol=nPeriods) #Interest payments on foreign bills received by workers
paymz_b<-matrix(data=tfm_data[17,2],nrow=nScenarios,ncol=nPeriods) #Interest payments on government bills held by rentiers
paymz_dc<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)             #Interest payments on CBDC held by rentiers
paymz_e<-matrix(data=tfm_data[18,2],nrow=nScenarios,ncol=nPeriods) #Interest payments on securities held by rentiers
paymz_h<-matrix(data=-tfm_data[15,2],nrow=nScenarios,ncol=nPeriods)#Rentiers' interest payments on personal loans
paymz_m<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Interest payments on deposits held by rentiers
paymz_q<-matrix(data=tfm_data[19,2],nrow=nScenarios,ncol=nPeriods) #Interest payments on foreign bills received by rentiers
pf<-matrix(data=tfm_data[9,2],nrow=nScenarios,ncol=nPeriods)       #Profit of firms
seign<-matrix(data=tfm_data[20,5],nrow=nScenarios,ncol=nPeriods)   #CB Seigniorage income
tax<-matrix(data=tfm_data[11,5],nrow=nScenarios,ncol=nPeriods)     #Total taxes
taxw<-matrix(data=-tfm_data[11,1],nrow=nScenarios,ncol=nPeriods)   #Taxes paid workers
taxz<-matrix(data=-tfm_data[11,2],nrow=nScenarios,ncol=nPeriods)   #Taxes paid by rentiers
tax_net<-matrix(data=tfm_data[11,5],nrow=nScenarios,ncol=nPeriods)     #Total taxes net of QEP
taxw_net<-matrix(data=-tfm_data[11,1],nrow=nScenarios,ncol=nPeriods)   #Taxes paid workers net of QEP
taxz_net<-matrix(data=-tfm_data[11,2],nrow=nScenarios,ncol=nPeriods)   #Taxes paid by rentiers net of QEP
tb<-matrix(data=ex-im,nrow=nScenarios,ncol=nPeriods)               #Trade balance
upf<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Retained or undistributed profit (in addition to AF)
wab<-matrix(data=-tfm_data[7,3],nrow=nScenarios,ncol=nPeriods)     #Wage bill
ydw<-matrix(data=cw,nrow=nScenarios,ncol=nPeriods)                 #Disposable income of workers
ydz<-matrix(data=cz,nrow=nScenarios,ncol=nPeriods)                 #Disposable income of rentiers
yd<-matrix(data=cw+cz,nrow=nScenarios,ncol=nPeriods)               #Total disposable income
ydw_net<-matrix(data=cw,nrow=nScenarios,ncol=nPeriods)             #Disposable income of workers net of QEP
ydz_net<-matrix(data=cz,nrow=nScenarios,ncol=nPeriods)             #Disposable income of rentiers net of QEP
yd_net<-matrix(data=cw+cz,nrow=nScenarios,ncol=nPeriods)           #Total disposable income net of QEP
yn<-matrix(data=tfm_data[6,3],nrow=nScenarios,ncol=nPeriods)       #GDP

# Stocks ####
ad<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                   #Advances: demand
as<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                   #Advances: supply
bb<-matrix(data=bs_data[6,5],nrow=nScenarios,ncol=nPeriods)        #Bills held by banks
bcb<-matrix(data=bs_data[6,6],nrow=nScenarios,ncol=nPeriods)       #CB demand for government bills
bf<-matrix(data=bs_data[6,7],nrow=nScenarios,ncol=nPeriods)        #Domestic bills held by foreign sector
bs<-matrix(data=-bs_data[6,4],nrow=nScenarios,ncol=nPeriods)       #Supply of government bills
bw<-matrix(data=bs_data[6,1],nrow=nScenarios,ncol=nPeriods)        #Demand for government bills by workers
bz<-matrix(data=bs_data[6,2],nrow=nScenarios,ncol=nPeriods)        #Demand for government bills by rentiers
bh<-matrix(data=bw+bz,nrow=nScenarios,ncol=nPeriods)               #Total private demand for government bills
es<-matrix(data=-bs_data[7,3],nrow=nScenarios,ncol=nPeriods)       #Supply of private securities
ew<-matrix(data=bs_data[7,1],nrow=nScenarios,ncol=nPeriods)        #Demand for private securities by workers
ez<-matrix(data=bs_data[7,2],nrow=nScenarios,ncol=nPeriods)        #Demand for private securities by rentiers
eh<-matrix(data=ew+ez,nrow=nScenarios,ncol=nPeriods)               #Total demand for private securities
hb<-matrix(data=bs_data[1,5],nrow=nScenarios,ncol=nPeriods)        #Reserves
hs<-matrix(data=-bs_data[1,6],nrow=nScenarios,ncol=nPeriods)       #Supply of cash and reserves
hw<-matrix(data=bs_data[1,1],nrow=nScenarios,ncol=nPeriods)        #Demand for cash by workers
hz<-matrix(data=bs_data[1,2],nrow=nScenarios,ncol=nPeriods)        #Demand for cash by rentiers
hh<-matrix(data=hw+hz,nrow=nScenarios,ncol=nPeriods)               #Total demand for cash (from households)
k<-matrix(data=bs_data[9,3],nrow=nScenarios,ncol=nPeriods)         #Real stock of fixed capital
kn<-matrix(data=k,nrow=nScenarios,ncol=nPeriods)                   #Nominal value of capital stock
kt<-matrix(data=k,nrow=nScenarios,ncol=nPeriods)                   #Target stock of capital
ld<-matrix(data=bs_data[4,5],nrow=nScenarios,ncol=nPeriods)        #Total demand for loans
lf<-matrix(data=-bs_data[4,3],nrow=nScenarios,ncol=nPeriods)       #Firms' demand for loans
ls<-matrix(data=bs_data[4,5],nrow=nScenarios,ncol=nPeriods)        #Supply of loans
lw<-matrix(data=-bs_data[4,1],nrow=nScenarios,ncol=nPeriods)       #Personal loans to workers
lz<-matrix(data=-bs_data[4,2],nrow=nScenarios,ncol=nPeriods)       #Personal loans to rentiers
ms<-matrix(data=-bs_data[2,5],nrow=nScenarios,ncol=nPeriods)       #Supply of deposits
mw<-matrix(data=bs_data[2,1],nrow=nScenarios,ncol=nPeriods)        #Bank deposits held by workers by workers
mz<-matrix(data=bs_data[2,2],nrow=nScenarios,ncol=nPeriods)        #Bank deposits held by workers by rentiers
mh<-matrix(data=mw+mz,nrow=nScenarios,ncol=nPeriods)               #Total bank deposits held by households
niip<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                 #Net international investment position
qs<-matrix(data=-bs_data[8,7],nrow=nScenarios,ncol=nPeriods)       #Foreign bills held by domestic sectors (note: drop export from bf)
qw<-matrix(data=bs_data[8,1],nrow=nScenarios,ncol=nPeriods)        #Foreign bills held by workers
qz<-matrix(data=bs_data[8,2],nrow=nScenarios,ncol=nPeriods)        #Foreign bills demanded by rentiers
vw<-matrix(data=-bs_data[10,1],nrow=nScenarios,ncol=nPeriods)      #Net wealth of workers
vz<-matrix(data=-bs_data[10,2],nrow=nScenarios,ncol=nPeriods)      #Net wealth of rentiers
vh<-matrix(data=vw+vz,nrow=nScenarios,ncol=nPeriods)               #Total net wealth

# Prices, rates and ratios ####
p<-matrix(data=1,nrow=nScenarios,ncol=nPeriods)                    #Average price of domestic products 
p_e<-matrix(data=p,nrow=nScenarios,ncol=nPeriods)                  #Expected price level 
phi<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Impact of foreign prices to domestic prices
pi<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                   #Actual inflation rate 
pi_e<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                 #Expected inflation rate 
pm<-matrix(data=1,nrow=nScenarios,ncol=nPeriods)                   #Average price of import 
ra<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                   #Interest rate on advances 
rb<-matrix(data=int_data[5,1],nrow=nScenarios,ncol=nPeriods)       #Rate of return on government securities 
re<-matrix(data=int_data[4,1],nrow=nScenarios,ncol=nPeriods)       #Rate of return on private securities 
rl<-matrix(data=int_data[2,1],nrow=nScenarios,ncol=nPeriods)       #Interest rate on loans
rm<-matrix(data=int_data[3,1],nrow=nScenarios,ncol=nPeriods)       #Interest rate on deposits
rdc<-matrix(data=rm,nrow=nScenarios,ncol=nPeriods)                 #Interest on rate on CBDC 
rq<-matrix(data=int_data[7,1],nrow=nScenarios,ncol=nPeriods)       #Interest rate on foreign bills 
rr<-matrix(data=int_data[6,1],nrow=nScenarios,ncol=nPeriods)       #Interest on rate on reserves 
thetaw<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Workers' gross loans to disposable income ratio 
thetaz<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Rentiers' gross loans to disposable income ratio  

# Define variables related to CB digital currency ####
dcb<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Stock of CBDC held by banks
dcf<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Stock of CBDC held by foreign sector
dcs<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Supply of CBDC
dcw<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Stock of CBDC held by workers
dcz<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #Stock of CBDC held by rentiers
lambdadcw<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)            #Percentage of workers' CBDC to total wealth  
lambdadcz<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)            #Percentage of rentiers' CBDC to total wealth  
qep<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)                  #QE for the people
perc_qep<-matrix(data=0,nrow=nScenarios,ncol=nPeriods)             #Percentage of QE for the people going to workers

# New variables measuring bank disintermediation
lin <- matrix(0, nrow = nScenarios, ncol = nPeriods)               # Loan intensity ratio
ltd <- matrix(0, nrow = nScenarios, ncol = nPeriods)               # Loans to deposits
rcbf <- matrix(0, nrow = nScenarios, ncol = nPeriods)              # Reliance on central bank funding

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add a convergence tolerance and a vector to record iterations
iteration_count <- matrix(0, nrow = nScenarios, ncol = nPeriods)
tol <- 1e-6 # Convergence tolerance
iter_used <- array(0, dim=c(nScenarios, nPeriods)) # Record iterations used per scenario and period

# Create a progress bar ####
pb <- progress_bar$new( total = nScenarios * nPeriods,
                        format = "Progress [:bar] :percent ETA: :eta")

# Choose scenario ####
for (j in 1:nScenarios){
  
  # Define time ####
  for (i in 2:nPeriods){
    
    # Update the progress bar ####
    pb$tick()
    
    # Scenario 2: households replace deposits with CBDCs (with rdc=rm)  ####
    if(i>9 && j==2){for (t in 1:10){
      lambdadcw[j,9+t]=t/50
      lambdadcz[j,9+t]=t/50}        
      lambdadcw[j,19:nPeriods]=0.18
      lambdadcz[j,19:nPeriods]=0.18
      }
    
    # Scenario 3: households replace deposits with CBDCs (with rdc>rm)  ####
    if(j==3){
      mudc[j,1:nPeriods]=0     #Instead of mudc = mum = -0.025
      rdc[j,1:nPeriods]=r_bar0
      if(i>9){
      for (t in 1:10){
      lambdadcw[j,9+t]=t/50
      lambdadcz[j,9+t]=t/50}        
      lambdadcw[j,19:nPeriods]=0.18
      lambdadcz[j,19:nPeriods]=0.18
     }
    }
    
    # Scenario 4: policy rate cut without cbdc ####
    if(i>9 && j==4){r_bar[j,i] = 0.01}
    
    # Scenario 5: policy rate cut with CBDCs (with rdc=rm)  ####
    if(i>9 && j==5){
      r_bar[j,i] = 0.01
       for (t in 1:10){
        lambdadcw[j,9+t]=t/50
        lambdadcz[j,9+t]=t/50}        
        lambdadcw[j,19:nPeriods]=0.18
        lambdadcz[j,19:nPeriods]=0.18
    }
    
    # Scenario 6: QE for the people (with rdc=rm)  ####
    if(i>9 && j==6){
      qep[j,i] = 100
      perc_qep[j,i] = 0.5
      
    }                    
    
    # Initial guess for y (from previous period)
    cond_old <- abs(hs[j,i-1] - hh[j,i-1] - hb[j,i-1])
    
    # Define (max) iterations for converging to simultaneous solution ####
    for (iterations in 1:MaxIterations){
      
      # Model equations #### 
      
      # 1) Households (except for portfolio equations) ####
      ydw[j,i] = (wab[j,i]*(1-omega) + paymw_m[j,i] + paymw_e[j,i] +
                    + paymw_q[j,i] + paymw_b[j,i] - paymw_h[j,i] - taxw[j,i]
                  + paymw_dc[j,i])                                                #Disposable income of workers 
      ydz[j,i] = (wab[j,i]*omega + pB[j,i] + pf[j,i]*(1-eta) + paymz_m[j,i] 
                  + paymz_e[j,i] + paymz_q[j,i] + paymz_b[j,i] - paymz_h[j,i]
                  - taxz[j,i] + paymz_dc[j,i])                                    #Disposable income of rentiers 
      ydw_net[j,i] = (wab[j,i]*(1-omega) + paymw_m[j,i] + paymw_e[j,i] +
                    + paymw_q[j,i] + paymw_b[j,i] - paymw_h[j,i] - taxw_net[j,i]
                  + paymw_dc[j,i])                                                #Disposable income of workers net of QEP
      ydz_net[j,i] = (wab[j,i]*omega + pB[j,i] + pf[j,i]*(1-eta) + paymz_m[j,i] 
                  + paymz_e[j,i] + paymz_q[j,i] + paymz_b[j,i] - paymz_h[j,i]
                  - taxz_net[j,i] + paymz_dc[j,i])                                #Disposable income of rentiers net of QEP  
      yd[j,i] =  ydw[j,i] + ydz[j,i]                                              #Total disposable income of households
      yd_net[j,i] =  ydw_net[j,i] + ydz_net[j,i]                                  #Total disposable income of households net of QEP
      vw[j,i] = vw[j,i-1] + ydw[j,i] - cw[j,i]*p[j,i]                             #Stock of wealth of workers
      vz[j,i] = vz[j,i-1] + ydz[j,i] - cz[j,i]*p[j,i]                             #Stock of wealth of rentiers
      vh[j,i] = vw[j,i] + vz[j,i]                                                 #Total stock of wealth of workers
      if (i<6){
        alpha1w[j,i] = (cw[j,i] - alpha2w*(vw[j,i-1]/p[j,i-1]) - alpha0w[j,i])/(ydw[j,i]/p_e[j,i]) 
        alpha1z = (cz[j,i] - alpha2z*(vz[j,i-1]/p[j,i-1]) - alpha0z[j,i] )/(ydz[j,i]/p_e[j,i])
      }else{
        cw[j,i] = (alpha0w[j,i] + alpha1w[j,i]*ydw_net[j,i]/p_e[j,i] +
                   alpha2w*vw[j,i-1]/p[j,i-1])                                    #Real consumption of workers
        cz[j,i] = (alpha0z[j,i] + alpha1z*ydz_net[j,i]/p_e[j,i] +
                   alpha2z*vz[j,i-1]/p[j,i-1])                                    #Real consumption of rentiers
        alpha1w[j,i] <- alpha1w[j,i-1] - alpha1w_1*(rl[j,i] - rl[j,i-1]/2
                                                    - rl[j,i-2]/2 )               #Endogenous propensity to consume out of workers' income  
      }
      if (i<6){
      deltaw = (lw[j,i-1] + thetaw[j,i]*ydw[j,i] - lw[j,i])/lw[j,i-1]             #Personal loans demanded by workers
      deltaz = (lz[j,i-1] + thetaw[j,i]*ydz[j,i] - lz[j,i])/lz[j,i-1]
      }else{
      lw[j,i] = lw[j,i-1] + thetaw[j,i]*ydw[j,i] - deltaw*lw[j,i-1]               #Personal loans demanded by workers
      lz[j,i] = lz[j,i-1] + thetaw[j,i]*ydz[j,i] - deltaz*lz[j,i-1]               #Personal loans demanded by rentiers
      }
      thetaw[j,i] = thetaw0 - thetaw1 * rl[j,i-1]                                 #Workers' gross loans to disposable income ratio
      thetaz[j,i] = thetaz0 - thetaz1 * rl[j,i-1]                                 #Rentiers' gross loans to disposable income ratio 
      
      # 2) Non-financial firms ####
      yn[j,i] = (p[j,i]*(cw[j,i] + cz[j,i] + id[j,i] + gov[j,i] + ex[j,i]) +
                   - pm[j,i]*im[j,i])                                             #GDP
      kt[j,i] = (kappa * yn[j,i-1])/ p[j,i-1]                                     #Target capital stock
      id[j,i] = gamma*(kt[j,i] - k[j,i-1]) + da[j,i]                              #Real investment in fixed capital
      da[j,i] = delta * k[j,i-1]                                                  #Depreciation allowances in real terms
      k[j,i] = k[j,i-1] + id[j,i] - da[j,i]                                       #Fixed capital stock in real terms
      kn[j,i] = kn[j,i-1] + p[j,i]*id[j,i] - af[j,i]                              #Current nominal value of capital stock
      af[j,i] = da[j,i] * p[j,i-1]                                                #Amortization funds
      pf[j,i] = (yn[j,i] - paym_l[j,i] - af[j,i] - paymz_e[j,i] +
                   - paymw_e[j,i] - wab[j,i])                                     #Total profit of firms
      upf[j,i] = eta*pf[j,i]                                                      #Retained or undistributed profit (in addition to AF)        
      es[j,i] = eh[j,i]                                                           #Supply of private securities
      
      # 3) Government sector and central bank ####
      gov[j,i]=gov0[j,i]                                                          #Government spending
      if (i<6){
      tauww[j,i] = (taxw[j,i] - (tauz[j,i]*(paymw_m[j,i] + 
                 + paymw_e[j,i] + paymw_q[j,i] + paymw_b[j,i] + paymw_dc[j,i])
                 + tauv[j,i]*vw[j,i-1] ))/(wab[j,i]*(1-omega)) 
      tauwz[j,i] = (taxz[j,i] - (tauz[j,i]*(pB[j,i] + pf[j,i]
                 + paymz_m[j,i] + paymz_e[j,i] + paymz_q[j,i] + paymz_dc[j,i])
                 + paymz_b[j,i] + tauv[j,i]*vz[j,i-1]))/(wab[j,i]*omega)  
      }else{
      tauww[j,i] = tauww[j,5]  
      tauwz[j,i] = tauwz[j,5]    
      
      taxw_net[j,i] = (tauww[j,i]*(wab[j,i]*(1-omega)) + tauz[j,i]*(paymw_m[j,i] + 
                + paymw_e[j,i] + paymw_q[j,i] + paymw_b[j,i] + paymw_dc[j,i])
                   + tauv[j,i]*vw[j,i-1])                                         #Taxes paid by wage-earners
      taxz_net[j,i] = (tauwz[j,i]*wab[j,i]*omega + tauz[j,i]*(pB[j,i] + pf[j,i]
                + paymz_m[j,i] + paymz_e[j,i] + paymz_q[j,i] + paymz_dc[j,i])
                + paymz_b[j,i] + tauv[j,i]*vz[j,i-1])                             #Taxes paid by rentiers
      taxw[j,i] = taxw_net[j,i] - perc_qep[j,i]*(qep[j,i]-qep[j,i-1])             #Taxes paid by wage-earners including QEP effect
      taxz[j,i] = taxz_net[j,i] - (1-perc_qep[j,i])*(qep[j,i]-qep[j,i-1])         #Taxes paid by rentiers including QEP effect
      }
      tax[j,i] = taxw[j,i]+taxz[j,i]                                              #Total tax revenue including QE effect
      tax_net[j,i] = taxw_net[j,i]+taxz_net[j,i]                                  #Total tax revenue 
      def[j,i] = gov[j,i]*p[j,i] + paym_b[j,i] - tax[j,i] - seign[j,i]            #Government deficit (Note: payment on reserves/advances and CBDC reduces/increases the Seigniorage received from the CB)
      seign[j,i] = paymcb_b[j,i] - paym_r[j,i] - paym_dc[j,i] + paym_a[j,i]       #Seigniorage income
      bs[j,i] = bs[j,i-1] + def[j,i]                                              #Supply of government securities 
      bcb[j,i] = bs[j,i] - bh[j,i] - bb[j,i] - bf[j,i]                            #Government securities held by CB
      hs[j,i] = hs[j,i-1] + (bcb[j,i]-bcb[j,i-1])                                 #Cash supply (to households)
      as[j,i] = ad[j,i]                                                           #Advances supply (to banks)
      
      # 4.A) Banks, initial finance and final finance (funding) ####
      lf[j,i] = lf[j,i-1] + id[j,i] * p[j,i] - af[j,i] - upf[j,i] - (es[j,i] - es[j,i-1]) #Stock of bank loans demanded by firms
      ld[j,i] = lf[j,i] + lw[j,i] + lz[j,i]                                       #Total demand for bank loans
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1])                                 #Supply of bank loans
      ms[j,i] = mh[j,i]                                                           #Supply of bank deposits   
      pB[j,i] = (paym_l[j,i] + paymb_b[j,i] + paym_r[j,i] 
                 + paym_h[j,i] - paym_m[j,i] - paym_a[j,i])                       #Bank profit
      
      # 4.B) New bank ratio calculations
      lin[j, i] <- ls[j, i]/(ls[j, i] + bb[j, i] + hb[j, i])                      #Loan intensity ratio 
      ltd[j, i] <- ls[j, i] / ms[j, i]                                            #Loans to deposits ratio
      rcbf[j, i] <- ad[j, i] / (ad[j, i] + ms[j, i])                              #Reliance on central bank funding 
      
      # 5) Foreign sector ####
      if (i>6){
      ex[j,i] <- exp(eps0 - eps1*log(p[j,i]/pm[j,i]) + eps2*log(yf[j,i]))         #Real export of final and intermediate products 
      im[j,i] <- exp(nu0 - nu1*log(pm[j,i-1]/p[j,i-1]) 
                     + nu2*log(yn[j,i]/p[j,i]))                                   #Real import   
      }  
      bf[j,i] = bf[j,i-1] + (qs[j,i]-qs[j,i-1]) - cab[j,i]                        #Domestic bills held by foreign sector if > 0 
      qs[j,i] = qw[j,i] + qz[j,i]                                                 #Bills issued by foreign sectors 
      tb[j,i] = (p[j,i]*ex[j,i])-(pm[j,i]*im[j,i])                                #Trade balance (of domestic country)
      cab[j,i] = tb[j,i] + paym_q[j,i] - paymf_b[j,i]                             #Current account balance (of domestic country)
      niip[j,i] = qs[j,i] - bf[j,i]                                               #Net international investment financial position (of domestic country)
      
      # 6) Employment and wages ####
      wab[j,i] = w*n[j,i]                                                         #Wage bill  
      n[j,i] = yn[j,i]/(pr*p[j,i])                                                #Labor demand
      
      # 7) Price setting and expectations ####
      p[j,i] = (1-phi[j,i])*(1 + mup)*(w/pr) + phi[j,i]*pm_t                      #Average price of domestic goods
      pm[j,i] = pm_t                                                              #Average price of import
      if (i>5){phi[j,i] = pm[j,i-1]*im[j,i-1]/yn[j,i-1]}                          #Impact of foreign prices to domestic prices (measured by import to GDP ratio in previous period)
      pi[j,i] = (p[j,i]/p[j,i-1] - 1)                                             #Actual inflation rate 
      pi_e[j,i] = (sigmaw1*(pi[j,i-1] + sigmaw2*(pi_t - pi[j,i-1]))               #Expected inflation: mixed regressive & observed (annual basis)  
                   + (1-sigmaw1)*pi[j,i])
      p_e[j,i] = p_e[j,i-1]*(1 + pi_e[j,i])                                       #Expected price level 
      
      # 8) Interest payments ####
      mue[j,i] = mue00 + mue1*pf[j,i]/es[j,i-1]                                   #Endogenous risk premium on private securities
      re[j,i] = re[j,i-1] - deltare*(re[j,i]-r_bar[j,i-1]-mue[j,i])               #Rate of interest on private securities
      rm[j,i] = rm[j,i-1] - deltarm*(rm[j,i]-r_bar[j,i-1]-mum[j,i])               #Rate of interest on deposits
      rl[j,i] = rl[j,i-1] - deltarl*(rl[j,i]-r_bar[j,i-1]-mul)                    #Rate of interest on bank loans
      rb[j,i] = rb[j,i-1] - deltarb*(rb[j,i]-r_bar[j,i-1]-mub)                    #Rate of interest on government securities
      ra[j,i] = ra[j,i-1] - deltara*(ra[j,i]-r_bar[j,i-1]-mua)                    #Rate of interest on advances from CB
      rr[j,i] = rr[j,i-1] - deltarr*(rr[j,i]-r_bar[j,i-1]-mur) #with: mur<0       #Rate of interest on reserves at CB
      rq[j,i] = rq[j,i-1] - deltarq*(rq[j,i]-r_bar[j,i-1]-muf)                    #Interest rate on foreign bills 
      rdc[j,i] = rdc[j,i-1] - deltardc*(rdc[j,i]-r_bar[j,i-1]-mudc[j,i])          #Rate of interest on CBDC
      paym_l[j,i] = rl[j,i-1]*lf[j,i-1]                                           #Interest payments on bank loans to firms
      paymw_m[j,i] = rm[j,i-1]*mw[j,i-1]                                          #Interest payments on bank deposits to workers
      paymz_m[j,i] = rm[j,i-1]*mz[j,i-1]                                          #Interest payments on bank deposits to rentiers
      paym_m[j,i] = paymw_m[j,i] + paymz_m[j,i]                                   #Total interest payments on bank deposits
      paymw_e[j,i] = re[j,i-1]*ew[j,i-1]                                          #Interest payments on private securities to workers
      paymz_e[j,i] = re[j,i-1]*ez[j,i-1]                                          #Interest payments on private securities to rentiers
      paym_e[j,i] = paymw_e[j,i] + paymz_e[j,i]                                   #Total interest payments on private securities
      paymw_b[j,i] = rb[j,i-1]*bw[j,i-1]                                          #Interest payments on gov. securities to workers
      paymz_b[j,i] = rb[j,i-1]*bz[j,i-1]                                          #Interest payments on gov. securities to rentiers
      paymb_b[j,i] = rb[j,i-1]*bb[j,i-1]                                          #Interest payments on gov. securities to banks
      paymf_b[j,i] = rb[j,i-1]*bf[j,i-1]                                          #Interest payments on gov. securities to foreign sector   
      paymcb_b[j,i] = rb[j,i-1]*bcb[j,i-1]                                        #Interest payments on gov. securities to CB
      paym_b[j,i] = (paymw_b[j,i] + paymz_b[j,i] + paymb_b[j,i] + paymf_b[j,i]
                     + rb[j,i-1]*bcb[j,i-1])                                      #Total interest payments on gov. securities
      paym_a[j,i] = ra[j,i-1]*as[j,i-1]                                           #Interest payments on advances from CB
      paym_r[j,i] = rr[j,i-1]*hb[j,i-1]                                           #Interest payments on reserves at CB
      paymw_h[j,i] = rl[j,i-1]*lw[j,i-1]                                          #Workers' interest payments on personal loans
      paymz_h[j,i] = rl[j,i-1]*lz[j,i-1]                                          #Rentiers' interest payments on personal loans
      paym_h[j,i] = paymw_h[j,i] + paymz_h[j,i]                                   #Total interest payments on personal loans
      paymw_q[j,i] = rq[j,i-1]*qw[j,i-1]                                          #Interest payments on foreign bills received by workers    
      paymz_q[j,i] = rq[j,i-1]*qz[j,i-1]                                          #Interest payments on foreign bills received by rentiers  
      paym_q[j,i] = paymw_q[j,i] + paymz_q[j,i]                                   #Total interest payments on foreign bills 
      paymw_dc[j,i] = rdc[j,i-1]*dcw[j,i-1]                                       #Interest payments on CBDC to workers
      paymz_dc[j,i] = rdc[j,i-1]*dcz[j,i-1]                                       #Interest payments on CBDC to rentiers
      paym_dc[j,i] = paymw_dc[j,i] + paymz_dc[j,i]                                #Total interest payments on CBDC
      
      # 9) Portfolio equations ####
      hw[j,i] = lambdacw*cw[j,i-1]                                                #Demanded stock of cash by workers
      hz[j,i] = lambdacz*cz[j,i-1]                                                #Demanded stock of cash by rentiers
      if (i<6){
        lambda20w = (bw[j,i] - (lambda21w*vw[j,i]*rm[j,i] +
                                  lambda22w*vw[j,i]*rb[j,i] + lambda23w*vw[j,i]*re[j,i] +
                                  lambda24w*ydw[j,i]))/vw[j,i]  
        lambda20z = (bz[j,i] - (lambda21z*vz[j,i]*rm[j,i] +
                                  lambda22z*vz[j,i]*rb[j,i] + lambda23z*vz[j,i]*re[j,i] +
                                  lambda24z*ydz[j,i]))/vz[j,i]    
        lambda30w = (ew[j,i] - (lambda31w*vw[j,i]*rm[j,i] +
                                  lambda32w*vw[j,i]*rb[j,i] + lambda33w*vw[j,i]*re[j,i] +
                                  lambda34w*ydw[j,i]))/vw[j,i]  
        lambda30z = (ez[j,i] - (lambda31z*vz[j,i]*rm[j,i] +
                                  lambda32z*vz[j,i]*rb[j,i] + lambda33z*vz[j,i]*re[j,i] +
                                  lambda34z*ydz[j,i]))/vz[j,i]     
      }else{
        bw[j,i] = lambda20w*vw[j,i] + lambda21w*vw[j,i]*rm[j,i] +
          lambda22w*vw[j,i]*rb[j,i] + lambda23w*vw[j,i]*re[j,i] +
          lambda24w*ydw[j,i]                                                      #Demanded stock of gov. securities by workers
        bz[j,i] = lambda20z*vz[j,i] + lambda21z*vz[j,i]*rm[j,i] +
          lambda22z*vz[j,i]*rb[j,i] + lambda23z*vz[j,i]*re[j,i] +
          lambda24z*ydz[j,i]                                                      #Demanded stock of gov. securities by rentiers
        ew[j,i] = lambda30w*vw[j,i] + lambda31w*vw[j,i]*rm[j,i] +
          lambda32w*vw[j,i]*rb[j,i] + lambda33w*vw[j,i]*re[j,i] +
          lambda34w*ydw[j,i]                                                      #Demanded stock of private securities by workers
        ez[j,i] = lambda30z*vz[j,i] + lambda31z*vz[j,i]*rm[j,i] +
          lambda32z*vz[j,i]*rb[j,i] + lambda33z*vz[j,i]*re[j,i] +
          lambda34z*ydz[j,i]                                                      #Demanded stock of private securities by rentiers
      }
      hh[j,i] = hw[j,i] + hz[j,i]                                                 #Total demanded stock of cash
      bh[j,i] = bw[j,i] + bz[j,i]                                                 #Total demanded stock of gov. securities
      eh[j,i] = ew[j,i] + ez[j,i]                                                 #Total demanded stock of private securities
      qw[j,i] = lambda40w * vw[j,i]                                               #Foreign bills demanded by workers  
      qz[j,i] = lambda40z * vz[j,i]                                               #Foreign bills demanded by rentiers  
      
      # 10) CBDC-related operations ####
      bb[j,i] = ms[j,i] + ad[j,i] - ld[j,i] - hb[j,i]                             #Banks holdings of bills (new formulation) 
      dcw[j,i] = lambdadcw[j,i]*vw[j,i] + perc_qep[j,i]*qep[j,i]                  #Workers' holdings of CBDC
      dcz[j,i] = lambdadcz[j,i]*vz[j,i] + (1-perc_qep[j,i])*qep[j,i]              #Rentiers' holdings of CBDC
      mw[j,i] = (vw[j,i] + lw[j,i] - hw[j,i] - bw[j,i] - ew[j,i] - qw[j,i] 
                 - dcw[j,i])                                                      #Demanded stock of deposits by workers (new formulation)
      mz[j,i] = (vz[j,i] + lz[j,i] - hz[j,i] - bz[j,i] - ez[j,i] - qz[j,i]
                 - dcz[j,i])                                                      #Demanded stock of deposits by rentiers (new formulation)
      mh[j,i] = mw[j,i] + mz[j,i]                                                 #Total demanded of deposits = buffer stock
      dcs[j,i] = dcw[j,i]+dcz[j,i]                                                #Supply of CBDC
      if (dcs[j,i]<rho2*ms[j,i-1]){
        hb[j,i] = (rho1+rho2)*ms[j,i-1]-dcs[j,i]                                  #Bank reserves at CB (normal times)
        ad[j,i] = 0}                                                              #Banks advances from CB (normal times)
      else{
        hb[j,i] = rho1*ms[j,i-1]                                                  #Bank reserves at CB (with high CBDC emissions)
        ad[j,i] = dcs[j,i]                                                        #Banks advances from CB (with high CBDC emissions)
      } 
      
      # 11) Hidden equation ####
      ##hs[j,i] = hm[j,i] + hb[j,i]                                               #Equilibrium condition for liquidity
      
      # CHECK CONVERGENCE ####
      if (abs((hs[j,i] - hh[j,i] - hb[j,i]) - cond_old) < tol){
        iter_used[j,i] <- iterations
        break   # Stop iterating once convergence is reached
      }
      
      # Update cond_old for next iteration
      cond_old <- abs(hs[j,i] - hh[j,i] - hb[j,i])
      
      # If max iterations reached
      if (iterations == MaxIterations){
        iter_used[j,i] <- MaxIterations
      }

    }
  }
}

# Close the progress bar
pb$terminate()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create consistency statement and deliver other infos ####
aerror=0
error=0

for (j in 1:(nScenarios)){
  for (i in 2:(nPeriods-1)){error = error + abs(hs[j,i]-hh[j,i]-hb[j,i])}
}
aerror <- error/(nPeriods*nScenarios)
if ( aerror<0.1 ){cat("\n *********************************** \n Good news! The model is watertight! \n", "Average error =", aerror, "< 0.1 \n", "Cumulative error =", error, "\n ***********************************")} else{
  if ( aerror<1 && aerror<1 ){cat("\n *********************************** \n Minor issues with model consistency \n", "Average error =", aerror, "> 0.1 \n", "Cumulative error =", error, "\n ***********************************")}
  else{cat("\n ******************************************* \n Warning: the model is not fully consistent! \n", "Average error =", aerror, "> 1 \n", "Cumulative error =", error, "\n *******************************************")} }      
cat("\n Number of scenarios =", nScenarios)
cat("\n Number of periods =", nPeriods)
cat("\n Convergence tolerance =", tol)
cat("\n Max number of iterations =", max(iter_used),"/",MaxIterations)
cat("\n ***********************************")

# Add notification sound
library(beepr)
beep(sound = 2)