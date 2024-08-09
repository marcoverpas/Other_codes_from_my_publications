'OPENTWOFOUR MODEL
wfcreate(wf = OPENTWOFOUR, page = annual) a 1850 2250
smpl @all

'**********************************************************************************

'Amended: March 26th, 2020
'Version 15

'**********************************************************************************

'DEFINE EXCHANGE RATE MECHANISM
series er_mec = 0   'NOTE: 0 = new mechanism for floating exchange rate; 1 = old mechanism (Godley and Lavoie 2007)

'**********************************************************************************************************************************************************************************************

'REMINDER: CHANGE THE FILE PATH BELOW BEFORE EXECUTING THE CODE!

'Import steady state values of stocks and exogenous variables
read(b3) "C:\...\steady.csv"  h_us_h_ss h_ea_h_ss b_cb_usus_s_ss b_cb_eaea_s_ss b_us_s_ss b_usus_d_ss b_usus_s_ss b_usea_d_ss b_usea_s_ss b_ea_s_ss b_eaus_d_ss b_eaus_s_ss b_eaea_d_ss b_eaea_s_ss h_us_s_ss h_ea_s_ss xr_us_ss xr_ea_ss v_p_us_ss v_p_ea_ss n_v_p_us_ss  n_v_p_ea_ss v_r_us_ss v_r_ea_ss omega_us_ss omega_ea_ss b_us_bank_ss b_ea_bank_ss l_s_us_ss l_s_ea_ss l_h_us_ss l_h_ea_ss l_f_us_ss l_f_ea_ss k_us_ss k_ea_ss labf_us_ss labf_ea_ss n_d_us_ss n_d_ea_ss

import "C:\...\time_series.wf1" 'real time series

'**********************************************************************************************************************************************************************************************

' Creates new series

series eps3
 eps3.label(d) Sensitivity of US export to relative prices
series mu3
 mu3.label(d) Sensitivity of US import to relative prices
series parw1_us
 parw1_us.label(d) Coefficient of wage equation in US 
series parw1_ea
 parw1_ea.label(d) Coefficient of wage equation in EA
series parw2_us
 parw2_us.label(d) Coefficient of wage equation in US 
series parw2_ea
 parw2_ea.label(d) Coefficient of wage equation in EA
series un_us
 un_us.label(d) Unemployment rate in US 
series un_ea
 un_ea.label(d) Unemployment rate in EA
series nun_us
 nun_us.label(d) Normal unemployment rate in US 
series nun_ea
 nun_ea.label(d) Normal unemployment rate in EA
series labf_us
 labf_us.label(d) Labour force in US 
series labf_ea
 labf_ea.label(d) Labour force in EA
series l0_us
 l0_us.label(d) Coefficient of US labour force equation 
series l0_ea
 l0_ea.label(d) Coefficient of EA labour force equation 
series l1_us
 l1_us.label(d) Coefficient of US labour force equation 
series l1_ea
 l1_ea.label(d) Coefficient of EA labour force equation 
series n_d_us
 n_d_us.label(d) Labour demand in US 
series n_d_ea
 n_d_ea.label(d) Labour demand in EA
series n_s_us
 n_s_us.label(d) Labour supply in US 
series n_s_ea
 n_s_ea.label(d) Labour supply in EA
series p_us
 p_us.label(d) Price level in US 
series p_ea
 p_ea.label(d) Price level in EA
series ep_us
 ep_us.label(d) Expected price level in US 
series ep_ea
 ep_ea.label(d) Expected price level in EA
series a_us
 a_us.label(d) Average labour productivity in US 
series a_ea
 a_ea.label(d) Average labour productivity in EA
series mu_us
 mu_us.label(d) Mark-up on labour cost in US 
series mu_ea
 mu_ea.label(d) Mark-up on labour cost in EA
series w_us
 w_us.label(d) Money wage rate in US 
series w_ea
 w_ea.label(d) Money wage rate in EA
series f_f_us 
 f_f_us.label(d) Profit of US firms 
series f_f_ea 
 f_f_ea.label(d) Profit of EA firms 
series inv_us 
 inv_us.label(d) US investment in fixed capital 
series inv_ea 
 inv_ea.label(d) EA investment in fixed capital 
series k_us 
 k_us.label(d) US fixed capital stock
series k_ea 
 k_ea.label(d) EA fixed capital stock
series delta_us 
 delta_us.label(d) US capital depreciation rate
series delta_ea 
 delta_ea.label(d) EA capital depreciation rate
series gamma_us 
 gamma_us.label(d) US capital adjustment ratio
series gamma_ea 
 gamma_ea.label(d) EA capital adjustment ratio
series kappa_us 
 kappa_us.label(d) US desired capital to output ratio
series kappa_ea 
 kappa_ea.label(d) EA desired capital to output ratio
series l_f_us 
 l_f_us.label(d) Loans demanded by US firms
series l_f_ea 
 l_f_ea.label(d) Loans demanded by EA firms
series rho_us 
 rho_us.label(d) Normal % of household loans to disposable income in US
series rho_ea 
 rho_ea.label(d) Normal % of household loans to disposable income in EA
series rep_us 
 rep_us.label(d) Loans repayment rate in US
series rep_ea 
 rep_ea.label(d) Loans repayment rate in EA
series fu_us 
 fu_us.label(d) Firms retained profit (amortisation funds) in US
series fu_ea 
 fu_ea.label(d) Firms retained profit (amortisation funds) in EA

' Creates and documents series
series yd_r_us 
 yd_r_us.label(d) Disposable income of US rich households 
series yd_p_us 
 yd_p_us.label(d) Disposable income of US porr households 
series yd_r_ea 
 yd_r_ea.label(d) Disposable income of EA rich households 
series  yd_p_ea
 yd_p_ea.label(d) Disposable income of EA poor households 
series yd_hs_r_us
yd_hs_r_us.label(d) Haig-Simons disposable income rich households in US 
series yd_hs_r_ea
yd_hs_r_ea.label(d) Haig-Simons disposable income rich households in EA 
series wb_us 
 wb_us.label(d) Gross wage bill in US 
series wb_ea 
 wb_ea.label(d) Gross wage bill in EA
series f_bank_us
f_bank_us.label(d) Profit of US banks
series f_bank_ea
f_bank_ea.label(d) Profit of EA banks
series cons_r_us
cons_r_us.label(d) Consumption of rich US households
series cons_p_us
cons_p_us.label(d) Consumption of poor US households
series cons_r_ea
cons_r_ea.label(d) Consumption of rich EA households
series cons_p_ea
cons_p_ea.label(d) Consumption of poor EA households
series b_usus_d
b_usus_d.label(d) Bills issued by the US acquired by the US: demand
series b_usus_s
b_usus_s.label(d) Bills issued by the US acquired by the US: supply
series b_cb_usus_s
b_cb_usus_s.label(d) Bills issued by the US, supplied to the US Central bank
series b_us_s
b_us_s.label(d) Bills issued by the US - total supply
series b_usea_d
b_usea_d.label(d) Bills issued by the EA acquired by the US: demand
series b_usea_s
b_usea_s.label(d) Bills issued by the EA acquired by the US: supply
series b_eaus_d
b_eaus_d.label(d) Bills issued by the US acquired by the EA: demand
series b_eaus_s
b_eaus_s.label(d) Bills issued by the US acquired by the EA: supply
series b_ea_s
b_ea_s.label(d) Bills issued by the EA - total supply
series b_eaea_d
b_eaea_d.label(d) Bills issued by the EA acquired by the EA: demand
series b_eaea_s
b_eaea_s.label(d) Bills issued by the EA acquired by the EA: supply
series b_cb_eaea_s
b_cb_eaea_s.label(d) Bills issued by the EA supplied to the EA Central Bank
series dep_r_us
dep_r_us.label(d) Holding of money in bank deposits by rich households in US
series dep_p_us
dep_p_us.label(d) Holding of money in bank deposits by poor households in US
series dep_r_ea
dep_r_ea.label(d) Holding of money in bank deposits by rich households in EA
series dep_p_ea
dep_p_ea.label(d) Holding of money in bank deposits by poor households in EA
series h_us_r_h 
h_us_r_h.label(d) Holding of money in cash by rich households in US
series h_us_p_h 
h_us_p_h.label(d) Holding of money in cash by poor households in US
series h_ea_r_h 
h_ea_r_h.label(d) Holding of money in cash by rich households in EA
series h_ea_p_h 
h_ea_p_h.label(d) Holding of money in cash by poor households in EA
series beta_con_us
beta_con_us.label(d) Borrowing paramenter (active if consumption greater than income) for poor US households
series beta_con_ea
beta_con_ea.label(d) Borrowing paramenter (active if consumption greater than income) for poor EA households
series b_us_bank_not
b_us_bank_not.label(d) US bills notionally bought by US banks
series b_ea_bank_not
b_ea_bank_not.label(d) EA bills notionally bought by EA banks
series b_us_bank
b_us_bank.label(d) US bills actually bought by US banks
series b_ea_bank
b_ea_bank.label(d) EA bills actually bought by EA banks
series cab_us
cab_us.label(d) Current account balance in the US
series cab_ea
cab_ea.label(d) Current account balance in the EA
series f_cb_us
f_cb_us.label(d) Profits of Central Bank in the US
series f_cb_ea
f_cb_ea.label(d) Profits of Central Bank in the EA
series h_us_s
h_us_s.label(d) Supply of the US cash
series h_ea_s
h_ea_s.label(d) Supply of the EA cash
series im_us
im_us.label(d) Imports of the US from the EA
series im_ea
im_ea.label(d) Imports of the EA from the US
series kab_us
kab_us.label(d) Capital account balance in the US
series kab_ea
kab_ea.label(d) Current account balance in the EA
series psbr_us
psbr_us.label(d) Government deficit in the US
series psbr_ea
psbr_ea.label(d) Government deficit in the EA
series t_us
t_us.label(d) Tax revenue in the US
series t_ea
t_ea.label(d) Tax revenue in the EA
series x_us
x_us.label(d) Exports from the US to the EA
series x_ea
x_ea.label(d) Exports from the EA to the US
series xr_us
xr_us.label(d) Exchange rate: units of dollars against 1 unit of pound
series xr_ea
xr_ea.label(d) Exchange rate: units of pounds against 1 unit of dollar
series y_us
y_us.label(d) Income in the US
series y_ea
y_ea.label(d) Income in the EA
series h_us_h
h_us_h.label(d) Total holding of money in cash in US
series h_ea_h
h_ea_h.label(d) Total holding of money in cash in EA
series tb_us
tb_us.label(d) Trade balance US
series tb_ea
tb_ea.label(d) Trade balance EA
series bp_us
bp_us.label(d) US Balance of payment
series bp_ea
bp_ea.label(d) EA Balance of payment
series z_us
z_us.label(d) Trigger for positive notional US bills 
series z_ea
z_ea.label(d) Trigger for positive notional EA bills 
series a_d_us
a_d_us.label(d) Advances needed by US banks from the US central bank 
series a_d_ea
a_d_ea.label(d) Advances needed by EA banks from the EA central bank 
series a_s_us
a_s_us.label(d) Advances provided to US commercial banks by the US central bank 
series a_s_ea
a_s_ea.label(d) Advances provided to EA commercial banks by the EA central bank 
series dep_bank_us
dep_bank_us.label(d) Supply of deposits in US (liabilities of US banks)
series dep_bank_ea
dep_bank_ea.label(d) Supply of deposits in EA (liabilities of EA banks)
series v_r_us
v_r_us.label(d) Wealth accumulation of rich households in US
series v_r_ea
v_r_ea.label(d) Wealth accumulation of rich households in EA
series v_p_us
v_p_us.label(d) Gross wealth accumulation of poor households in US (debt not included)
series n_v_p_us
v_p_us.label(d) Net wealth accumulation of poor households in US 
series v_p_ea
v_p_ea.label(d) Gross wealth accumulation of poor households in EA (debt not included)
series n_v_p_ea
v_p_ea.label(d) Net wealth accumulation of poor households in EA
series b_us_bank
b_us_bank.label(d) US bills bought by US bank  
series b_ea_bank
b_ea_bank.label(d) EA bills bought by EA bank  
series l_h_us
l_h_us.label(d) Demand of loans by poor households in US 
series l_h_ea
l_h_ea.label(d) Demand of loans by poor households in EA
series l_s_us
l_s_us.label(d) Supply of loans in US 
series l_s_ea
l_s_ea.label(d) Supply of loans in EA
series gnp_us
gnp_us.label(d) Gross National product US
series gnp_ea
gnp_ea.label(d) Gross National product EA
series yd_tot_us
yd_tot_us.label(d) Total disposable income in US
series yd_tot_ea
yd_tot_ea.label(d) Total disposable income in EA
series gini_us 
gini_us.label(d) Disposable income of rich households over total disposable income in US
series gini_ea
gini_ea.label(d) Disposable income of rich households over total disposable income in EA
series v_tot_us
v_tot_us.label(d) Total net wealth in US
series v_tot_ea
v_tot_ea.label(d) Total net wealth in EA
series giniw_us 
giniw_us.label(d) Net wealth of rich households over total wealth in US
series giniw_ea
giniw_ea.label(d) Net wealth of rich households over total wealth in EA

'Exogenous variables 
series r_us
r_us.label(d) Interest rate on the US bills
series r_ea
r_ea.label(d) Interest rate on the EA bills
series g_us
g_us.label(d) Government expenditure in the US
series g_ea
g_ea.label(d) Government expenditure in the EA
series bo_us
bo_us.label(d) Share of borrowed money out of excessive consumption in US
series bo_ea
bo_ea.label(d) Share of borrowed money out of excessive consumption in EA

' Generate parameters
series alpha1_r_us
alpha1_r_us.label(d) Propensity to consume out of income for US rich households
series alpha1_p_us
alpha1_p_us.label(d) Propensity to consume out of income for US poor households 
series alpha2_us
alpha2_us.label(d) Propensity to consume out of wealth in the US
series alpha1_r_ea
alpha1_r_ea.label(d) Propensity to consume out of income for EA rich households
series alpha1_p_ea
alpha1_p_ea.label(d) Propensity to consume out of income for EA poor households
series alpha2_ea
alpha2_ea.label(d) Propensity to consume out of wealth in the EA
series eps0
eps0.label(d) Parameter determining real exports in the US
series eps1
eps1.label(d) Parameter determining real exports in the US
series eps2
eps2.label(d) Parameter determining real exports in the US
series lambda10
lambda10.label(d) Parameter in asset demand function
series lambda11
lambda11.label(d) Parameter in asset demand function
series lambda12
lambda12.label(d) Parameter in asset demand function
series lambda20
lambda20.label(d) Parameter in asset demand function
series lambda21
lambda21.label(d) Parameter in asset demand function
series lambda22
lambda22.label(d) Parameter in asset demand function
series lambda40
lambda40.label(d) Parameter in asset demand function
series lambda41
lambda41.label(d) Parameter in asset demand function
series lambda42
lambda42.label(d) Parameter in asset demand function
series lambda50
lambda50.label(d) Parameter in asset demand function
series lambda51
lambda51.label(d) Parameter in asset demand function
series lambda52
lambda52.label(d) Parameter in asset demand function
series mu0
mu0.label(d) Parameter determining real imports in the US
series mu1
mu1.label(d) Parameter determining real imports in the US
series mu2
mu2.label(d) Parameter determining real imports in the US
series theta_us
theta_us.label(d) Tax rate in the US
series theta_ea
theta_ea.label(d) Tax rate in the EA
series omega_us
omega_us.label(d) Wage share in US   'Note: omega_us = 1 - ic_us
series omega_ea
omega_ea.label(d) Wage share in EA
series emu_us
emu_us.label(d) Emulation parameter in US
series emu_ea
emu_ea.label(d) Emulation parameter in EA
series depsh_us
depsh_us.label(d) Percentage of money held as deposits in US
series depsh_ea
depsh_ea.label(d) Percentage of money held as deposits in EA
series p_$_ea
p_$_ea.label(d) EA prices converted in dollars 
series chi
chi.label(d) Trigger for household loans repayment

' Starting values for parameters
alpha1_r_us = 0.73
alpha1_p_us = 0.77
alpha1_r_ea = 0.73
alpha1_p_ea = 0.77
alpha2_us = 0.13333
alpha2_ea = 0.13333
eps0 = - 2.1
eps1 = 0.5
eps2 = 1.228
lambda10 = 0.7
lambda11 = 5
lambda12 = 5
lambda20 = 0.25
lambda21 = 5
lambda22 = 5
lambda40 = 0.7
lambda41 = 5
lambda42 = 5
lambda50 = 0.25
lambda51 = 5
lambda52 = 5
mu0 = - 2.1
mu1 = 0.5
mu2 = 1.228
theta_us = 0.2
theta_ea = 0.2
chi = 0

' Exogenous variables
g_us = 16
g_ea = 16
h_us_h = @mean(h_us_h_ss,"1851 1851")
h_ea_h = @mean(h_ea_h_ss,"1851 1851")
r_us = 0.03
r_ea = 0.03

' Starting values for stocks
b_cb_usus_s = @mean(b_cb_usus_s_ss,"1851 1851")
b_cb_eaea_s = @mean(b_cb_eaea_s_ss,"1851 1851")
b_us_s = @mean(b_us_s_ss,"1851 1851")
b_usus_d = @mean(b_usus_d_ss,"1851 1851")
b_usus_s = @mean(b_usus_s_ss,"1851 1851")
b_usea_d = @mean(b_usea_d_ss,"1851 1851")
b_usea_s = @mean(b_usea_s_ss,"1851 1851")
b_ea_s = @mean(b_ea_s_ss,"1851 1851")
b_eaus_d = @mean(b_eaus_d_ss,"1851 1851")
b_eaus_s = @mean(b_eaus_s_ss,"1851 1851")
b_eaea_d = @mean(b_eaea_d_ss,"1851 1851")
b_eaea_s = @mean(b_eaea_s_ss,"1851 1851")
h_us_s = @mean(h_us_s_ss,"1851 1851")
h_ea_s = @mean(h_ea_s_ss,"1851 1851")
xr_us = @mean(xr_us_ss,"1851 1851")
xr_ea = @mean(xr_ea_ss,"1851 1851")

'Additional parameters and stocks
v_p_us = @mean(v_p_us_ss,"1851 1851")
v_p_ea = @mean(v_p_ea_ss,"1851 1851")
n_v_p_us = @mean(n_v_p_us_ss,"1851 1851")
n_v_p_ea = @mean(n_v_p_ea_ss,"1851 1851")
v_r_us = @mean(v_r_us_ss,"1851 1851")
v_r_ea = @mean(v_r_ea_ss,"1851 1851")
omega_us = @mean(omega_us_ss,"1851 1851")
omega_ea = @mean(omega_ea_ss,"1851 1851")
b_us_bank = @mean(b_us_bank_ss,"1851 1851")
b_ea_bank = @mean(b_ea_bank_ss,"1851 1851")
bo_us = 0.5
bo_ea = 0.5
l_s_us = @mean(l_s_us_ss,"1851 1851")
l_s_ea = @mean(l_s_ea_ss,"1851 1851")
l_h_us = @mean(l_h_us_ss,"1851 1851")
l_h_ea = @mean(l_h_ea_ss,"1851 1851")
emu_us = 0
emu_ea = 0 
depsh_us = 0.7
depsh_ea = 0.7

'*************************************************************************************************************************************************
'New coefficients and intial values
kappa_us = 0.1 'Desired capital to outpu stock in US
kappa_ea = 0.1 'Desired capital to outpu stock in EA
l_f_us = @mean(l_f_us_ss,"1851 1851") 'Loans to firms in in US (initial value of stock)
l_f_ea = @mean(l_f_ea_ss,"1851 1851") 'Loans to firms in in EA (initial value of stock)
delta_us = 0.05 'Capital depreciation rate in US
delta_ea = 0.05 'Capital depreciation rate in EA
gamma_us = 0.15 'Speed of adjustment of capital to desired level in US
gamma_ea = 0.15 'Speed of adjustment of capital to desired level in EA
k_us = @mean(k_us_ss,"1851 1851") 'Fixed capital stock in US
k_ea = @mean(k_ea_ss,"1851 1851") 'Fixed capital stock in EA
rho_us = 0 'Min. percentage of (poor) housheold loans to disposable income in US
rho_ea = 0 'Min. percentage of (poor) housheold loans to disposable income in EA
rep_us = 0 'Repayment rate of housheold loans in US
rep_ea = 0 'Repayment rate of housheold loans in EA
w_us = 0.8 'Money wage rate in US
w_ea = 0.8 'Money wage rate in EA
a_us = 2 'Average labour productivity in US  [Note: omega = (w/p)/a = 0.4]
a_ea = 2 'Average labour productivity in EA
p_us = 1 'Price level in US (initial value)
p_ea = 1 'Price level in EA (initial value)
p_$_ea = 1 'EA prices converted in dollars 
ep_us = 1 'Expected price level in US (initial value)
ep_ea = 1 'Expected price level in EA (initial value)
mu_us = (p_us*a_us/w_us) - 1 'Mark-up over labour cost in US (to obtain p_us = 1)
mu_ea = (p_us*a_us/w_us) - 1 'Mark-up over labour cost in EA (to obtain p_ea = 1)
parw1_us = 0 'Coefficient in US wage equation (sensitivity to unemployment)
parw1_ea = 0 'Coefficient in EA wage equation (sensitivity to unemployment)
parw2_us = 0 'Coefficient in US wage equation (shock)
parw2_ea = 0 'Coefficient in EA wage equation (shock)
nun_us = 0 'Normal rate of unemployment in US
nun_ea = 0 'Normal rate of unemployment in EA
l0_us = 50 'Coefficient of US labour force equation (autonomous component)
l0_ea = 50 'Coefficient of EA labour force equation (autonomous component)
l1_us = 0.1 'Coefficient of US labour force equation (dependent component)
l1_ea = 0.1 'Coefficient of EA labour force equation (dependent component)
labf_us = @mean(labf_us_ss,"1851 1851") 'Labour force in US (initial value)
labf_ea = @mean(labf_ea_ss,"1851 1851") 'Labour force in EA (initial value)
n_d_us = @mean(n_d_us_ss,"1851 1851") 'Labour demand in US (initial value)
n_d_ea = @mean(n_d_ea_ss,"1851 1851") 'Labour demand in in EA (initial value)
un_us = 0 'Actual unemployment rate in US (initial value)
un_ea = 0 'Actual unemployment rate in in EA (initial value)
eps3 = 0.1 'Sensitivity of US export to relative prices
mu3 = 0.1 'Sensitivity of US import to relative prices

'*************************************************************************************************************************************************

model OPENTWOFOUR

'*************************************************************************************************************************************************

' DISPOSABLE INCOME AND WEALTH

' Disposable income of rich households in US - eq. 1 
OPENTWOFOUR.append yd_r_us = (f_f_us +  f_bank_us + r_us(-1)*b_usus_s(-1) + xr_ea*r_ea(-1)*b_usea_s(-1))*(1 - theta_us)

' Disposable income of poor households in US - eq. 2 
OPENTWOFOUR.append yd_p_us = (wb_us)*(1 - theta_us) - r_us(-1)*l_h_us(-1)

' Haig-Simons disposable income of rich households in US - eq. 3
OPENTWOFOUR.append yd_hs_r_us = yd_r_us + d(xr_ea)*b_usea_s(-1)

' Wealth accumulation rich of households in US - eq. 4
OPENTWOFOUR.append v_r_us = v_r_us(-1) + yd_hs_r_us - cons_r_us

' Gross wealth accumulation of poor households in US (liabilities not included) - eq. 5
OPENTWOFOUR.append v_p_us = v_p_us(-1) + yd_p_us - cons_p_us + d(l_h_us)

' Net wealth accumulation ofpoor households in US - eq. 6
OPENTWOFOUR.append n_v_p_us = v_p_us - l_h_us

' Disposable income of rich households in EA - eq. 7 
OPENTWOFOUR.append yd_r_ea = (f_f_ea + f_bank_ea + r_ea(-1)*b_eaea_s(-1) + xr_us*r_us(-1)*b_eaus_s(-1))*(1 - theta_ea)

' Disposable income of poor households in EA - eq. 8 
OPENTWOFOUR.append yd_p_ea = (wb_ea)*(1 - theta_ea)  - r_ea(-1)*l_h_ea(-1)

' Haig-Simons disposable income of rich households in EA - eq. 9
OPENTWOFOUR.append yd_hs_r_ea = yd_r_ea + d(xr_us)*b_eaus_s(-1)

' Wealth accumulation of rich households in EA - eq. 10
OPENTWOFOUR.append v_r_ea = v_r_ea(-1) + yd_hs_r_ea - cons_r_ea

' Gross wealth accumulation of poor households in EA (liabilities not included) - eq. 11
OPENTWOFOUR.append v_p_ea = v_p_ea(-1) + yd_p_ea - cons_p_ea  + d(l_h_ea)

' Net wealth accumulation of poor households in EA - eq. 12
OPENTWOFOUR.append n_v_p_ea = v_p_ea - l_h_ea

' Taxes paid in US - eq. 13 
OPENTWOFOUR.append t_us = theta_us*(wb_us +  f_f_us + f_bank_us + r_us(-1)*b_usus_d(-1) + xr_ea*r_ea(-1)*b_usea_s(-1))

' Taxes paid in in EA - eq. 14 
OPENTWOFOUR.append t_ea = theta_ea*(wb_ea + f_f_ea + f_bank_ea + r_ea(-1)*b_eaea_d(-1) + xr_us*r_us(-1)*b_eaus_s(-1))

'*************************************************************************************************************************************************

' CONSUMPTION AND TOTAL INCOME 

' Consumption of rich households in US - eq. 15 
OPENTWOFOUR.append cons_r_us = alpha1_r_us* yd_r_us*ep_us/p_us + alpha2_us*v_r_us(-1)

' Consumption of poor households in US - eq. 16 
OPENTWOFOUR.append cons_p_us  =  (1-emu_us)*alpha1_p_us* yd_p_us*ep_us/p_us + alpha2_us*n_v_p_us(-1)*(1-emu_us) + emu_us*cons_r_us 

' Total income in US - eq. 17 
OPENTWOFOUR.append y_us = cons_r_us + cons_p_us + g_us + x_us - im_us + inv_us

' Profit of US firms eq. 18 
OPENTWOFOUR.append f_f_us = y_us - wb_us - r_us(-1)*l_f_us(-1) - fu_us

' Gross wage bill in US eq. 19 
OPENTWOFOUR.append wb_us = w_us*n_d_us

' Consumption of rich households in EA - eq. 20 
OPENTWOFOUR.append cons_r_ea = alpha1_r_ea* yd_r_ea*ep_ea/p_ea + alpha2_ea*v_r_ea(-1)

'Consumption of poor households in EA - eq. 21 
OPENTWOFOUR.append cons_p_ea  =  (1-emu_ea)*alpha1_p_ea* yd_p_ea*ep_ea/p_ea + alpha2_ea*n_v_p_ea(-1)*(1-emu_ea) + emu_ea*cons_r_ea 

' Total income in EA - eq. 22 
OPENTWOFOUR.append y_ea = cons_r_ea + cons_p_ea + g_ea + x_ea - im_ea + inv_ea

' Profit of EA firms eq. 23 
OPENTWOFOUR.append f_f_ea = y_ea - wb_ea - r_ea(-1)*l_f_ea(-1) - fu_ea

' Gross wage bill in EA eq. 24 
OPENTWOFOUR.append wb_ea = w_ea*n_d_ea

'*************************************************************************************************************************************************

' TRADE

' Exports from US - eq. 25 
OPENTWOFOUR.append x_us = exp(eps0 - eps1*log(xr_us(-1)) + eps2*log(y_ea) + eps3*(log(p_$_ea(-1)) - log(p_us(-1))))

' Imports of US - eq. 26 
OPENTWOFOUR.append im_us = exp(mu0 + mu1*log(xr_us(-1)) + mu2*log(y_us) + mu3*(log(p_us(-1)) - log(p_$_ea(-1))))

' Exports of EA - eq. 27
OPENTWOFOUR.append x_ea = im_us*xr_us

' Imports of EA - eq. 28
OPENTWOFOUR.append im_ea = x_us*xr_us

'*************************************************************************************************************************************************

' ASSET DEMANDS

' Demand for US bills in US (by rich households) - eq. 29
OPENTWOFOUR.append b_usus_d = v_r_us*(lambda10 + lambda11*r_us - lambda12* r_ea)

' Demand for EA bills in US (by rich households) - eq. 30
OPENTWOFOUR.append b_usea_d = v_r_us*(lambda20 - lambda21*r_us + lambda22*r_ea)

' Holding of money in bank depositsby rich households  in US - eq. 31
OPENTWOFOUR.append dep_r_us = (v_r_us - b_usus_s - (b_usea_s*xr_ea))*depsh_us

' Holding of money in cash by rich households in US - eq. 32
OPENTWOFOUR.append h_us_r_h = v_r_us - b_usus_s - (b_usea_s*xr_ea) - dep_r_us

' Holding of money in bank deposits by poor households in US  - eq. 33
OPENTWOFOUR.append dep_p_us = v_p_us*depsh_us

' Holding of money in cash by poor housholds in US - eq. 34
OPENTWOFOUR.append h_us_p_h = v_p_us - dep_p_us

'Total holding of money in cash in US - eq. 35
OPENTWOFOUR.append h_us_h = h_us_p_h + h_us_r_h

' Demand for EA	bills in EA (by rich households) - eq. 36
OPENTWOFOUR.append b_eaea_d = v_r_ea*(lambda40 + lambda41*r_ea - lambda42*r_us)

' Demand for US bills in EA (by rich households) - eq. 37
OPENTWOFOUR.append b_eaus_d = v_r_ea*(lambda50 - lambda51*r_ea + lambda52*r_us)

' Holding of money in bank deposits by rich households  in EA - eq. 38
OPENTWOFOUR.append dep_r_ea = (v_r_ea - b_eaea_s - (b_eaus_s*xr_us))*depsh_ea

' Holding of money in cash by rich households in EA - eq. 39
OPENTWOFOUR.append h_ea_r_h = v_r_ea - b_eaea_s - (b_eaus_s*xr_us) - dep_r_ea

' Holding of money in bank deposits by poor households in EA  - eq. 40
OPENTWOFOUR.append dep_p_ea = v_p_ea*depsh_ea

' Holding of money in cash by poor housholds in US - eq. 41
OPENTWOFOUR.append h_ea_p_h = v_p_ea - dep_p_ea

'Total holding of money in cash in EA - eq. 42
OPENTWOFOUR.append h_ea_h = h_ea_p_h + h_ea_r_h

'*************************************************************************************************************************************************

' ASSET SUPPLIES

' Supply of US bills to US households - eq. 43
OPENTWOFOUR.append b_usus_s = b_usus_d

' Supply of EA bills to EA households - eq. 44
OPENTWOFOUR.append b_eaea_s = b_eaea_d

if (er_mec=0) then

' Suplly of US bills to EA households - eq. 45
OPENTWOFOUR.append b_eaus_s = b_eaus_d*xr_ea               

else

' Suplly of US bills to EA households - eq. 45 (alternative) 
OPENTWOFOUR.append b_eaus_s = b_us_s - b_usus_s - b_cb_usus_s - b_us_bank                                                             

endif

' Supply of EA bills to US households - eq. 46
OPENTWOFOUR.append b_usea_s = b_usea_d*xr_us

'*************************************************************************************************************************************************

'BANKING SECTOR 

' Supply of deposits in US (liabilities of US banks) - eq. 47
OPENTWOFOUR.append dep_bank_us = dep_p_us + dep_r_us

' Borrowing paramenter (active if consumption greater than income) for poor US households - eq. 48
OPENTWOFOUR.append beta_con_us = 0 + ((cons_p_us - yd_p_us)>0)

' Demand of loans by poor households in US  - eq. 49 
OPENTWOFOUR.append l_h_us = l_h_us(-1) + @pmax( ((cons_p_us - yd_p_us)*(1 - bo_us)*beta_con_us),  rho_us*yd_p_us ) - rep_us(-1)*l_h_us(-1)

' Supply of loans in US - eq. 50 
OPENTWOFOUR.append l_s_us=l_h_us + l_f_us

' US bills notionally bought by US bank - eq. 51
OPENTWOFOUR.append b_us_bank_not = dep_bank_us - l_s_us
 
'Trigger for notional US bills bought by US bank (whether balance sheet allow to buy goverment bills) - eq. 52
OPENTWOFOUR.append z_us = 0 + (b_us_bank_not>0)

' US bills actually bought by US bank - eq. 53
OPENTWOFOUR.append b_us_bank = b_us_bank_not*z_us

' Advances needed by US banks from the US central bank  - eq. 54
OPENTWOFOUR.append a_d_us = -b_us_bank_not*(1- z_us)

' Advances provided to US commercial banks by the US central bank  - eq. 55
OPENTWOFOUR.append a_s_us = a_d_us

' Profits of banks in US - eq. 56 
OPENTWOFOUR.append f_bank_us = r_us(-1)*b_us_bank(-1) + r_us(-1)*l_h_us(-1) + r_us(-1)*l_f_us(-1)

' Supply of deposits in EA (liabilities of US banks)  - eq. 57
OPENTWOFOUR.append dep_bank_ea = dep_p_ea + dep_r_ea

' Borrowing paramenter (active if consumption greater than income) for poor EA households - eq. 58
OPENTWOFOUR.append beta_con_ea = 0 + ((cons_p_ea - yd_p_ea)>0)

' Demand of loans by poor households in EA  - eq. 59 
OPENTWOFOUR.append l_h_ea = l_h_ea(-1) + @pmax ( ((cons_p_ea - yd_p_ea)*(1 - bo_ea)*beta_con_ea), rho_ea*yd_p_ea ) - rep_ea(-1)*l_h_ea(-1)

' Supply of loans in EA - eq. 60 
OPENTWOFOUR.append l_s_ea=l_h_ea + l_f_ea

' EA bills notionally bought by EA bank - eq. 61
OPENTWOFOUR.append b_ea_bank_not = dep_bank_ea - l_s_ea

' Trigger for notional EA bills bought by EA bank (whether balance sheet allow to buy goverment bills) - eq. 62
OPENTWOFOUR.append z_ea = 0 + (b_ea_bank_not>0)

' EA bills actually bought by EA bank - eq. 63
OPENTWOFOUR.append b_ea_bank = b_ea_bank_not*z_ea

'Advances needed by EA banks from the EA central bank  - eq. 64
OPENTWOFOUR.append a_d_ea = -b_ea_bank_not*(1- z_ea)

'Advances provided to EA commercial banks by the EA central bank  - eq. 65
OPENTWOFOUR.append a_s_ea = a_d_ea

' Profits of banks in EA - eq. 66 
OPENTWOFOUR.append f_bank_ea = r_ea(-1)*b_ea_bank(-1) + r_ea(-1)*l_h_ea(-1) + r_ea(-1)*l_f_ea(-1)

'*************************************************************************************************************************************************

'PUBLIC SECTOR

if (er_mec=0) then

'Supply of US bills to US central bank (US central bank lender of last resort) - eq. 67
OPENTWOFOUR.append b_cb_usus_s = b_us_s - b_usus_s - b_eaus_s - b_us_bank

' Suply of cash in US - eq. 68
OPENTWOFOUR.append h_us_s = b_cb_usus_s + a_s_us

else

'Supply of US bills to US central bank (US central bank lender of last resort) - eq. 67 (alternative) 
OPENTWOFOUR.append b_cb_usus_s = h_us_s - a_s_us   

' Suply of cash in US - eq. 68 (alternative) 
OPENTWOFOUR.append h_us_s = h_us_h

endif

' Profits of Central Bank in US - eq. 69
OPENTWOFOUR.append f_cb_us = r_us(-1)*b_cb_usus_s(-1) 

'Supply of EA bills to EA central bank  (EA central bank lander of last resort) - eq. 70
OPENTWOFOUR.append b_cb_eaea_s = b_ea_s - b_eaea_s - b_usea_s - b_ea_bank

' Suply of cash in EA - eq. 71
OPENTWOFOUR.append h_ea_s = b_cb_eaea_s + a_s_ea

' Profits of Central Bank in EA - eq. 72
OPENTWOFOUR.append f_cb_ea = r_ea(-1)*b_cb_eaea_s(-1)

' Government budget constraint in US - eq. 73
OPENTWOFOUR.append b_us_s = b_us_s(-1) + g_us + r_us(-1)*b_us_s(-1) - t_us - f_cb_us

' Government budget constraint in EA - eq. 74
OPENTWOFOUR.append b_ea_s = b_ea_s(-1) + g_ea + r_ea(-1)*b_ea_s(-1) - t_ea - f_cb_ea

'*************************************************************************************************************************************************

'EXCHANGE RATES 

if (er_mec=0) then

' US nominal exchange rate - eq. 75
OPENTWOFOUR.append xr_ea =  (r_us(-1)*b_eaus_s(-1) - d(b_eaus_s) - x_us + im_us)/(r_ea(-1)*b_usea_s(-1) - d(b_usea_s))

else

' US nominal exchange rate - eq. 75 (alternative) 
OPENTWOFOUR.append xr_ea = b_eaus_s/b_eaus_d

endif

'EA nominal exchange rate - eq. 76
OPENTWOFOUR.append xr_us = 1/xr_ea

'*************************************************************************************************************************************************

'REDUNDANT EQUATIONS 

'h_us_s = h_us_h 
'h_ea_s = h_ea_h 

'*************************************************************************************************************************************************

'Auxiliary equations

' Government deficit in the US - eq. 77
OPENTWOFOUR.append psbr_us = g_us + r_us(-1)*b_us_s(-1) - t_us - f_cb_us

' Government deficit in the EA - eq. 78
OPENTWOFOUR.append psbr_ea = g_ea + r_ea(-1)*b_ea_s(-1) - t_ea - f_cb_ea

' Net accumulation of financial assets in the US - eq. 79
OPENTWOFOUR.append nafa_us = psbr_us + cab_us

' Net accumulation of financial assets in the EA - eq. 80
OPENTWOFOUR.append nafa_ea = psbr_ea + cab_ea

' Current account balance - US - eq. 81
OPENTWOFOUR.append cab_us = x_us - im_us + xr_ea*r_ea(-1)*b_usea_s(-1) - r_us(-1)*b_eaus_s(-1) 

' Current account balance in EA - eq. 82
OPENTWOFOUR.append cab_ea = x_ea - im_ea + xr_us*r_us(-1)*b_eaus_s(-1) - r_ea(-1)*b_usea_s(-1) 

' Financial account balance in US - eq. 83
OPENTWOFOUR.append kabp_us = - d(b_usea_s)*xr_ea + d(b_eaus_s)

' Financial account balance in EA - eq. 84
OPENTWOFOUR.append kabp_ea = - d(b_eaus_s)*xr_us + d(b_usea_s)

'Trade balance US - eq. 85
OPENTWOFOUR.append tb_us =  x_us - im_us

'Trade balance EA - eq. 86
OPENTWOFOUR.append tb_ea = x_ea - im_ea

'Balance of payment US - eq. 87
OPENTWOFOUR.append bp_us = cab_us + kabp_us 

'Balance of payment EA - eq. 88
OPENTWOFOUR.append bp_ea = cab_ea + kabp_ea

'Gross National product US - eq. 89
OPENTWOFOUR.append gnp_us = y_us + xr_ea*r_ea(-1)*b_usea_s(-1) - r_us(-1)*b_eaus_s(-1) 

'Gross National product EA - eq. 90
OPENTWOFOUR.append gnp_ea = y_ea + xr_us*r_us(-1)*b_eaus_s(-1) - r_ea(-1)*b_usea_s(-1)  

'Total disposable income in US - eq. 91
OPENTWOFOUR.append  yd_tot_us = yd_p_us + yd_r_us

'Disposable income of rich households over total disposable income in US- eq. 92
OPENTWOFOUR.append  gini_us = yd_r_us/yd_tot_us

'Total disposable income in EA - eq. 93
OPENTWOFOUR.append  yd_tot_ea = yd_p_ea + yd_r_ea

'Disposable income of rich households over total disposable income in EA- eq. 94
OPENTWOFOUR.append  gini_ea  = yd_r_ea/yd_tot_ea

'Total net wealth in US - eq. 95
OPENTWOFOUR.append v_tot_us = v_r_us + n_v_p_us

'Net wealth of rich households over total wealth in US- eq. 96
OPENTWOFOUR.append  giniw_us  = v_r_us/v_tot_us

'Total net wealth in EA - eq. 97
OPENTWOFOUR.append v_tot_ea = v_r_ea + n_v_p_ea

'Net wealth of rich households over total wealth in US- eq. 98
OPENTWOFOUR.append  giniw_ea  = v_r_ea/v_tot_ea

'**********************************************************************************************************

'New equations for investment in fixed capital and related loans
OPENTWOFOUR.append fu_us = k_us(-1)*delta_us(-1)  'Retained profit (amortisation funds) in US - eq. 99                             
OPENTWOFOUR.append k_us = k_us(-1)*(1-delta_us(-1)) + inv_us  'Capital stock of US - eq. 100                             
OPENTWOFOUR.append inv_us = gamma_us*(y_us*kappa_us*ep_us/p_us - k_us(-1)) + k_us(-1)*delta_us(-1) 'Gross investment in US - eq. 101 
OPENTWOFOUR.append l_f_us = l_f_us(-1) + inv_us - fu_us 'US firms demand for loans - eq. 102 
OPENTWOFOUR.append fu_ea = k_ea(-1)*delta_ea(-1)  'Retained profit (amortisation funds) in EA - eq. 103                             
OPENTWOFOUR.append k_ea = k_ea(-1)*(1-delta_ea(-1)) + inv_ea 'Capital stock of EA - eq. 104                            
OPENTWOFOUR.append inv_ea = gamma_ea*(y_ea*kappa_ea*ep_ea/p_ea - k_ea(-1)) + k_ea(-1)*delta_ea(-1)  'Gross investment in EA  - eq. 105 
OPENTWOFOUR.append l_f_ea = l_f_ea(-1) + inv_ea - fu_ea 'EA firms demand for loans - eq. 106 

'New equations for price level and labour market
OPENTWOFOUR.append p_us = (w_us/a_us)*(1+mu_us)  'Price level in US - eq. 107                             
OPENTWOFOUR.append p_ea = (w_ea/a_ea)*(1+mu_ea)  'Price level in EA - eq. 108                             
OPENTWOFOUR.append ep_us = p_us(-1)   'Expected price level in US - eq. 109                            
OPENTWOFOUR.append ep_ea = p_ea(-1)   'Expected price level in EA - eq. 110                            
OPENTWOFOUR.append n_d_us = y_us/a_us  'Labour demand in US - eq. 111                             
OPENTWOFOUR.append n_d_ea = y_ea/a_ea  'Labour demand in EA - eq. 112                             
OPENTWOFOUR.append n_s_us = n_d_us '@pmin(n_d_us,labf_us)  'Labour supply in US - eq. 113                             
OPENTWOFOUR.append n_s_ea = n_d_ea '@pmin(n_d_ea,labf_ea)  'Labour supply in EA - eq. 114                             
OPENTWOFOUR.append omega_us = wb_us/y_us  'Endogenous wage share in US - eq. 115                             
OPENTWOFOUR.append omega_ea = wb_ea/y_ea  'Endogenous wage share in EA - eq. 116                             
OPENTWOFOUR.append labf_us = l0_us + l1_us*(n_d_us(-1) - labf_us(-1)) 'Labour force in US - eq. 117                           
OPENTWOFOUR.append labf_ea = l0_ea + l1_ea*(n_d_ea(-1) - labf_ea(-1)) 'Labour force in EA - eq. 118 
OPENTWOFOUR.append un_us = 1 - (n_d_us/labf_us) 'Actual rate of unemployment in US - eq. 119                            
OPENTWOFOUR.append un_ea = 1 - (n_d_ea/labf_ea) 'Actual rate of unemployment in EA - eq. 120                            
OPENTWOFOUR.append w_us = (1 + parw1_us*(un_us(-1) - nun_us))*w_us(-1)*ep_us/p_us(-1) + parw2_us   'Wage curve in US - eq. 121                            
OPENTWOFOUR.append w_ea = (1 + parw1_ea*(un_ea(-1) - nun_ea))*w_ea(-1)*ep_ea/p_ea(-1) + parw2_ea   'Wage curve in EA - eq. 122                            
OPENTWOFOUR.append p_$_ea = p_ea*xr_ea 'EA prices converted in dollars 
OPENTWOFOUR.append rep_us = chi*(@recode(@date>@dateval("2008"),0.50,0)) 'Repayment rate of household loans in US 

'**********************************************************************************************************

' Select the baseline Scenario and solve
smpl 1852 @last
OPENTWOFOUR.scenario Baseline
OPENTWOFOUR.solve(i=p, s=d, d=d)   
smpl @all

'**********************************************************************************************************

'Introduce shocks

'Shock to emulation coefficient of US
OPENTWOFOUR.scenario "Scenario 1"
OPENTWOFOUR.override emu_us
copy emu_us emu_us_1 
smpl 2005 @last
emu_us_1 = 0.15 'from 0
smpl @all
OPENTWOFOUR.solve

'Shock to wage rate of US with no change in unit price
OPENTWOFOUR.scenario(n) "Scenario 2"
OPENTWOFOUR.override parw2_us mu_us
copy parw2_us parw2_us_2 
copy mu_us mu_us_2 
smpl 2005 2005
parw2_us_2 = -0.2
smpl 2005 @last
mu_us_2 = 2.3333333333 'Necessary to keep price = 1
smpl @all
OPENTWOFOUR.solve

'Shock to wage rate of US and zero interest rate
OPENTWOFOUR.scenario(n) "Scenario 3"
OPENTWOFOUR.override parw2_us r_ea r_us mu_us
copy parw2_us parw2_us_3
copy mu_us mu_us_3
copy r_us r_us_3
copy r_ea r_ea_3 
smpl 2005 2005
parw2_us_3 = -0.2
smpl 2005 @last
mu_us_3 = 2.3333333333 'Necessary to keep price = 1
smpl @all
r_us_3 = 0
r_ea_3 = 0
smpl @all
OPENTWOFOUR.solve

'Shock to emulation coefficient of US when households pay back their debt
OPENTWOFOUR.scenario(n) "Scenario 4"
OPENTWOFOUR.override emu_us chi
copy emu_ea emu_us_4 
copy chi chi_4 
smpl 2005 @last
emu_us_4 = 0.15 'from 0
chi_4 = 1 'from 0
smpl @all
OPENTWOFOUR.solve

'Shock to wage rate of US (with no change in unit price) when households pay back their debt
OPENTWOFOUR.scenario(n) "Scenario 5"
OPENTWOFOUR.override parw2_us mu_us chi
copy parw2_us parw2_us_5 
copy mu_us mu_us_5 
copy chi chi_5
smpl 2005 2005
parw2_us_5 = -0.2
smpl 2005 @last
mu_us_5 = 2.3333333333 'Necessary to keep price = 1
chi_5 = 1 'from 0
smpl @all
OPENTWOFOUR.solve



'****************************************************************************************

' Creates charts from simulated variables

' 1a) Experiment 1: GDP in EA and US 
smpl 2000 2100
graph gdp.line y_us_1 y_ea_1
gdp.draw(shade,bottom, color(250,240,190)) 2004 2008
gdp.options linepat
gdp.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
gdp.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
gdp.name(1) United States (US)
gdp.name(2) Euro Area (EA)
gdp.addtext(t,just(c),font(20)) a) GDP (National income) 
'show gdp

' 1b) Experiment 1: disposable income in EA and US 
smpl 2000 2100
graph disp.line yd_r_us_1 yd_r_ea_1 yd_p_us_1 yd_p_ea_1
disp.draw(shade,bottom, color(250,240,190)) 2004 2008
disp.options linepat
disp.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
disp.setelem(2) lcolor(0,255,0) lwidth(2) lpat(1)
disp.setelem(3) lcolor(0,102,255) lwidth(2) lpat(3)
disp.setelem(4) lcolor(0,255,0) lwidth(2) lpat(3)
disp.name(1) Rentiers in US
disp.name(2) Rentiers in EA
disp.name(3) Wage-earners in US
disp.name(4) Wage-earners in EA
disp.addtext(t,just(c),font(20)) b) Disposable income in US and EA 
'show disp

' 1c) Experiment 1: inequality of income in EA and US 
smpl 2000 2100
graph gini.line gini_us_1 gini_ea_1
gini.draw(shade,bottom, color(250,240,190)) 2004 2008
gini.options linepat
gini.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
gini.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
gini.name(1) Income share of Rentiers in US
gini.name(2) Income share of Rentiers in EA
gini.addtext(t,just(c),font(20)) c) Income inequality in US and EA 
'show gini

' 1d) Experiment 1: inequality of wealth in EA and US 
smpl 2000 2100
graph gini2.line giniw_us_1 giniw_ea_1
gini2.draw(shade,bottom, color(250,240,190)) 2004 2008
gini2.options linepat
gini2.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
gini2.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
gini2.name(1) Wealth share of Rentiers in US
gini2.name(2) Wealth share of Rentiers in EA
gini2.addtext(t,just(c),font(20)) d) Wealth inequality in US and EA 
'show gini2

' 2) Experiment 1: current account in EA and US 
smpl 2000 2100
graph cab.line cab_us_1 xr_us_1
cab.draw(shade,bottom, color(250,240,190)) 2004 2008
cab.options linepat
cab.axis overlap
cab.setelem(2) axis(right)
cab.setelem(1) lcolor(purple) lwidth(2) lpat(1)
cab.setelem(2) lcolor(red) lwidth(2) lpat(3)
cab.name(1) Current account balance of US
cab.name(2) US dollar (right axis)
cab.addtext(t,just(c),font(20)) Figure 2. CAB and US Dollar after shock to emulation coefficient
'show cab

' 3) Experiment 2: GDP in EA and US 
smpl 2000 2100
graph gdpb.line y_us_2 y_ea_2
gdpb.draw(shade,bottom, color(250,240,190)) 2004 2008
gdpb.options linepat
gdpb.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
gdpb.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
gdpb.name(1) United States (US)
gdpb.name(2) Euro Area (EA)
gdpb.addtext(t,just(c),font(20)) Figure 3. GDP after shock to wage rate 
'show gdpb

' 4) Experiment 2: current account in EA and US 
smpl 2000 2100
graph cabb.line cab_us_2 tb_us_2 xr_us_2
cabb.draw(shade,bottom, color(250,240,190)) 2004 2008
cabb.options linepat
cabb.axis overlap
cabb.setelem(3) axis(right)
cabb.setelem(1) lcolor(purple) lwidth(2) lpat(1)
cabb.setelem(2) lcolor(orange) lwidth(2) lpat(2)
cabb.setelem(3) lcolor(red) lwidth(2) lpat(3)
cabb.name(1) Current account balance of US
cabb.name(2) Trade balance of US
cabb.name(3) US dollar (right axis)
cabb.addtext(t,just(c),font(20)) Figure 4. CAB, TB and US Dollar after shock to wage rate
'show cabb

' 5) Experiment 2: GDP in EA and US with 0 interest rate
smpl 2000 2100
graph zero.line y_us_3 y_ea_3
zero.draw(shade,bottom, color(250,240,190)) 2004 2008
zero.options linepat
zero.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
zero.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
zero.name(1) United States (US)
zero.name(2) Euro Area (EA)
zero.addtext(t,just(c),font(20)) Figure 5. GDP after shock to wage rate and zero interest rate
'show zero

' 6a) Experiment 2: disposable income in EA and US 
smpl 2000 2100
graph dispp.line yd_r_us_2 yd_r_ea_2 yd_p_us_2 yd_p_ea_2
dispp.draw(shade,bottom, color(250,240,190)) 2004 2008
dispp.options linepat
dispp.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
dispp.setelem(2) lcolor(0,255,0) lwidth(2) lpat(1)
dispp.setelem(3) lcolor(0,102,255) lwidth(2) lpat(3)
dispp.setelem(4) lcolor(0,255,0) lwidth(2) lpat(3)
dispp.name(1) Rentiers in US
dispp.name(2) Rentiers in EA
dispp.name(3) Wage-earners in US
dispp.name(4) Wage-earners in EA
dispp.addtext(t,just(c),font(20)) a) Disposable income 
'show dispp

' 6a) Experiment 2: net wealth stock in EA and US 
smpl 2000 2100
graph netw.line v_r_us_2 v_r_ea_2 n_v_p_us_2 n_v_p_ea_2
netw.draw(shade,bottom, color(250,240,190)) 2004 2008
netw.options linepat
netw.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
netw.setelem(2) lcolor(0,255,0) lwidth(2) lpat(1)
netw.setelem(3) lcolor(0,102,255) lwidth(2) lpat(3)
netw.setelem(4) lcolor(0,255,0) lwidth(2) lpat(3)
netw.name(1) Rentiers in US
netw.name(2) Rentiers in EA
netw.name(3) Wage-earners in US
netw.name(4) Wage-earners in EA
netw.addtext(t,just(c),font(20)) b) Net wealth in US and EA 
'show netw

' 6c) Experiment 2: inequality of income in EA and US 
smpl 2000 2100
graph ginib.line gini_us_2 gini_ea_2
ginib.draw(shade,bottom, color(250,240,190)) 2004 2008
ginib.options linepat
ginib.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
ginib.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
ginib.name(1) Income share of Rentiers in US
ginib.name(2) Income share of Rentiers in EA
ginib.addtext(t,just(c),font(20)) c) Income inequality  
'show ginib

' 6d) Experiment 2: inequality of wealth in EA and US 
smpl 2000 2100
graph gini2b.line giniw_us_2 giniw_ea_2
gini2b.draw(shade,bottom, color(250,240,190)) 2004 2008
gini2b.options linepat
gini2b.setelem(1) lcolor(0,102,255) lwidth(2) lpat(1)
gini2b.setelem(2) lcolor(0,255,0) lwidth(2) lpat(3)
gini2b.name(1) Wealth share of Rentiers in US
gini2b.name(2) Wealth share of Rentiers in EA
gini2b.addtext(t,just(c),font(20)) d) Wealth inequality  
'show gini2b

'****************************************************************************************
'****************************************************************************************

'Comparison with data

' 7a) Experiment 1: household debt in US (repayment activated)
smpl 2000 2019
graph hdeb.line l_h_us_1 l_h_us_4 l_h_us_ob
hdeb.draw(shade,bottom, color(250,240,190)) 2004 2008
hdeb.options linepat
hdeb.axis overlap
hdeb.setelem(3) axis(right)
hdeb.axis(right) range(75, 155)
hdeb.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
hdeb.setelem(2) lcolor(red) lwidth(2) lpat(1)
hdeb.setelem(3) lcolor(black) lwidth(2) lpat(2)
hdeb.name(1) Simulated (no repayment)
hdeb.name(2) Simulated (loans repayment)
hdeb.name(3) Observed household debt (right)
hdeb.addtext(t,just(c),font(20)) a) US household debt 
'show hdeb

' 7b) Experiment 1: EUR/USD 
smpl 2000 2019
graph exob.line xr_us_1 xr_us_4 xr_us_ob
exob.draw(shade,bottom, color(250,240,190)) 2004 2008
exob.options linepat
exob.axis overlap
exob.axis(left) range(0.93, 1.02)
exob.setelem(3) axis(right)
exob.setelem(4) axis(right)
exob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
exob.setelem(2) lcolor(red) lwidth(2) lpat(1)
exob.setelem(3) lcolor(black) lwidth(2) lpat(4)
exob.name(1) Simulated (no repayment)
exob.name(2) Simulated (loans repayment)
exob.name(3) Observed EUR/USD (right)
exob.addtext(t,just(c),font(20)) b) EUR/USD 
'show exob

' 7c) Experiment 1: USD index 
smpl 2000 2019
graph iexob.line xr_us_1 xr_us_4 xr_us_row_ob/100
iexob.draw(shade,bottom, color(250,240,190)) 2004 2008
iexob.options linepat
iexob.axis overlap
iexob.axis(right) range(0.96, 1.18)
iexob.setelem(3) axis(right)
iexob.setelem(4) axis(right)
iexob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
iexob.setelem(2) lcolor(red) lwidth(2) lpat(1)
iexob.setelem(3) lcolor(black) lwidth(2) lpat(2)
iexob.name(1) Simulated (no repayment)
iexob.name(2) Simulated (loans repayment)
iexob.name(3) Observed trade-wighted USD index (right)
iexob.addtext(t,just(c),font(20)) c) USD index 
'show iexob

' 7d) Experiment 1: current account balance in US 
smpl 2000 2019
graph cabob.line cab_us_1 cab_us_4 cab_us_ob
cabob.draw(shade,bottom, color(250,240,190)) 2004 2008
cabob.options linepat
cabob.axis overlap
cabob.setelem(3) axis(right)
cabob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
cabob.setelem(2) lcolor(red) lwidth(2) lpat(1)
cabob.setelem(3) lcolor(black) lwidth(2) lpat(2)
cabob.name(1) Simulated (no repayment)
cabob.name(2) Simulated (loans repayment)
cabob.name(3) Observed current account (right)
cabob.addtext(t,just(c),font(20)) d) US CAB 
'show cabob

' 7e) Experiment 1: trade balance in US 
smpl 2000 2019
graph tbob.line tb_us_1 tb_us_4 tb_us_ob
tbob.draw(shade,bottom, color(250,240,190)) 2004 2008
tbob.options linepat
tbob.axis overlap
tbob.setelem(3) axis(right)
tbob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
tbob.setelem(2) lcolor(red) lwidth(2) lpat(1)
tbob.setelem(3) lcolor(black) lwidth(2) lpat(2)
tbob.name(1) Simulated (no repayment)
tbob.name(2) Simulated (loans repayment)
tbob.name(3) Observed trade balance (right)
tbob.addtext(t,just(c),font(20)) e) US trade balance 
'show tbob

' 7f) Experiment 1: inequality of income in US 
smpl 2000 2019
graph giniob.line gini_us_1 gini_us_4 gini_us_ob
giniob.draw(shade,bottom, color(250,240,190)) 2004 2008
giniob.options linepat
giniob.axis overlap
giniob.setelem(3) axis(right)
giniob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
giniob.setelem(2) lcolor(red) lwidth(2) lpat(1)
giniob.setelem(3) lcolor(black) lwidth(2) lpat(2)
giniob.name(1) Simulated (no repayment)
giniob.name(2) Simulated (loans repayment)
giniob.name(3) Observed non-wage income to total income ratio (right)
giniob.addtext(t,just(c),font(20)) f) Income inequality 
'show giniob

' 7g) Experiment 1: inequality of wealth in US 
smpl 2000 2019
graph vrob.line v_r_us_1 v_r_us_4 v_r_us_ob
vrob.draw(shade,bottom, color(250,240,190)) 2004 2008
vrob.options linepat
vrob.axis overlap
vrob.setelem(3) axis(right)
vrob.setelem(1) lcolor(255,133,133) lwidth(2) lpat(3)
vrob.setelem(2) lcolor(red) lwidth(2) lpat(1)
vrob.setelem(3) lcolor(black) lwidth(2) lpat(2)
vrob.name(1) Simulated (no repayment)
vrob.name(2) Simulated (loans repayment)
vrob.name(3) Observed wealth inequality (right)
vrob.addtext(t,just(c),font(20)) g) Wealth inequality 
'show vrob 
'*****************************

' 8a) Experiment 2: household debt in US (repayment activated)
smpl 2000 2019
graph hdeb2.line l_h_us_5 l_h_us_2 l_h_us_ob
hdeb2.draw(shade,bottom, color(250,240,190)) 2004 2008
hdeb2.options linepat
hdeb2.axis overlap
hdeb2.setelem(3) axis(right)
hdeb2.axis(right) range(75, 140)
hdeb2.setelem(1) lcolor(black) lwidth(2) lpat(1)
hdeb2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
hdeb2.setelem(3) lcolor(red) lwidth(2) lpat(2)
hdeb2.name(1) Simulated (loans repayment)
hdeb2.name(2) Simulated (no repayment)
hdeb2.name(3) Observed household debt (right)
hdeb2.addtext(t,just(c),font(20)) a) US household debt 
'show hdeb2

' 8b) Experiment 2: EUR/USD 
smpl 2000 2019
graph exob2.line xr_us_5 xr_us_2 xr_us_ob
exob2.draw(shade,bottom, color(250,240,190)) 2004 2008
exob2.options linepat
exob2.axis overlap
exob2.axis(left) range(0.96, 1.02)
exob2.setelem(3) axis(right)
exob2.setelem(4) axis(right)
exob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
exob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
exob2.setelem(3) lcolor(red) lwidth(2) lpat(4)
exob2.name(1) Simulated (loans repayment)
exob2.name(2) Simulated (no repayment)
exob2.name(3) Observed EUR/USD (right)
exob2.addtext(t,just(c),font(20)) b) EUR/USD 
'show exob2

' 8c) Experiment 2: USD index 
smpl 2000 2019
graph iexob2.line xr_us_5 xr_us_2 xr_us_row_ob/100
iexob2.draw(shade,bottom, color(250,240,190)) 2004 2008
iexob2.options linepat
iexob2.axis overlap
iexob2.axis(right) range(0.96, 1.18)
iexob2.setelem(3) axis(right)
iexob2.setelem(4) axis(right)
iexob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
iexob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
iexob2.setelem(3) lcolor(red) lwidth(2) lpat(2)
iexob2.name(1) Simulated (loans repayment)
iexob2.name(2) Simulated (no repayment)
iexob2.name(3) Observed trade-wighted USD index (right)
iexob2.addtext(t,just(c),font(20)) c) USD index 
'show iexob2

' 8d) Experiment 2: current account balance in US 
smpl 2000 2019
graph cabob2.line cab_us_5 cab_us_2 cab_us_ob
cabob2.draw(shade,bottom, color(250,240,190)) 2004 2008
cabob2.options linepat
cabob2.axis overlap
cabob2.setelem(3) axis(right)
cabob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
cabob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
cabob2.setelem(3) lcolor(red) lwidth(2) lpat(2)
cabob2.name(1) Simulated (loans repayment)
cabob2.name(2) Simulated (no repayment)
cabob2.name(3) Observed current account (right)
cabob2.addtext(t,just(c),font(20)) d) US CAB 
'show cabob2

' 8e) Experiment 2: trade balance in US 
smpl 2000 2019
graph tbob2.line tb_us_5 tb_us_2 tb_us_ob
tbob2.draw(shade,bottom, color(250,240,190)) 2004 2008
tbob2.options linepat
tbob2.axis overlap
tbob2.setelem(3) axis(right)
tbob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
tbob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
tbob2.setelem(3) lcolor(red) lwidth(2) lpat(2)
tbob2.name(1) Simulated (loans repayment)
tbob2.name(2) Simulated (no repayment)
tbob2.name(3) Observed trade balance (right)
tbob2.addtext(t,just(c),font(20)) e) US trade balance 
'show tbob2

' 8f) Experiment 2: inequality of income in US 
smpl 2000 2019
graph giniob2.line gini_us_5 gini_us_2 gini_us_ob
giniob2.draw(shade,bottom, color(250,240,190)) 2004 2008
giniob2.options linepat
giniob2.axis overlap
giniob2.setelem(3) axis(right)
giniob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
giniob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
giniob2.setelem(3) lcolor(red) lwidth(2) lpat(2)
giniob2.name(1) Simulated (loans repayment)
giniob2.name(2) Simulated (no repayment)
giniob2.name(3) Observed non-wage income to total income ratio (right)
giniob2.addtext(t,just(c),font(20)) f) Income inequality 
'show giniob2


' 8g) Experiment 2: inequality of wealth in US 
smpl 2000 2019
graph vrob2.line v_r_us_5 v_r_us_2 v_r_us_ob
vrob2.draw(shade,bottom, color(250,240,190)) 2004 2008
vrob2.options linepat
vrob2.axis overlap
vrob2.setelem(3) axis(right)
vrob2.setelem(1) lcolor(black) lwidth(2) lpat(1)
vrob2.setelem(2) lcolor(blue) lwidth(2) lpat(3)
vrob2.setelem(3) lcolor(red) lwidth(2) lpat(2)
vrob2.name(1) Simulated (loans repayment)
vrob2.name(2) Simulated (no repayment)
vrob2.name(3) Observed wealth inequality (right)
vrob2.addtext(t,just(c),font(20)) g) Wealth inequality 
'show vrob2 

'****************************************************************************************

'Merge figures and print graphs

'Figure 1
graph fig1.merge gdp disp gini gini2 
fig1.legend font(20)
fig1.axis(left) font(20)
fig1.axis(right) font(20)
fig1.axis(bottom) font(20)
fig1.axis(top) font(20)
fig1.axis ticksnone
fig1.axis(right) ticksnone
fig1.options gridnone 
fig1.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig1.datelabel format(yyyy) 
fig1.align(2, 2.5, 3)
fig1.legend -inbox
fig1.addtext(t,just(c),font(22)) Figure 1. Shock to emulation coefficient 
show fig1

'Figure 2-3
graph fig23.merge cab gdpb 
fig23.legend font(20)
fig23.axis(left) font(20)
fig23.axis(right) font(20)
fig23.axis(bottom) font(20)
fig23.axis(top) font(20)
fig23.axis ticksnone
fig23.axis(right) ticksnone
fig23.options gridnone 
fig23.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig23.datelabel format(yyyy) 
fig23.align(2, 2.5, 2.5)
fig23.legend -inbox
show fig23

'Figure 4-5
graph fig45.merge cabb zero 
fig45.legend font(20)
fig45.axis(left) font(20)
fig45.axis(right) font(20)
fig45.axis(bottom) font(20)
fig45.axis(top) font(20)
fig45.axis ticksnone
fig45.axis(right) ticksnone
fig45.options gridnone 
fig45.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig45.datelabel format(yyyy) 
fig45.align(2, 2.5, 2.5)
fig45.legend -inbox
show fig45

'Figure 6
graph fig6.merge dispp netw ginib gini2b
fig6.legend font(20)
fig6.axis(left) font(20)
fig6.axis(right) font(20)
fig6.axis(bottom) font(20)
fig6.axis(top) font(20)
fig6.axis ticksnone
fig6.axis(right) ticksnone
fig6.options gridnone 
fig6.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig6.datelabel format(yyyy) 
fig6.align(2, 2.5, 3)
fig6.legend -inbox
fig6.addtext(t,just(c),font(22)) Figure 6. Shock to wage rate
show fig6

'Figure 7
graph fig7.merge hdeb exob iexob cabob tbob giniob vrob 
fig7.legend font(20)
fig7.axis(left) font(20)
fig7.axis(right) font(20)
fig7.axis(bottom) font(20)
fig7.axis(top) font(20)
fig7.axis ticksnone
fig7.axis(right) ticksnone
fig7.options -gridr  
fig7.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig7.datelabel format(yyyy) 
fig7.align(2, 2.5, 2.5)
fig7.legend -inbox
fig7.addtext(t,just(c),font(22)) Figure 7. Shock to emulation coefficient: simulations vs. observed series 
show fig7

graph fig10.merge cabob2 tbob2
fig10.legend font(20)
fig10.axis(left) font(20)
fig10.axis(right) font(20)
fig10.axis(bottom) font(20)
fig10.axis(top) font(20)
fig10.axis ticksnone
fig10.axis(right) ticksnone
fig10.options gridnone 
fig10.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig10.datelabel format(yyyy) 
fig10.align(3, 2.5, 2.5)
fig10.legend -inbox
fig10.addtext(t,just(c),font(22))  
show fig10

graph fig12.merge  giniob2  vrob2
fig12.legend font(20)
fig12.axis(left) font(20)
fig12.axis(right) font(20)
fig12.axis(bottom) font(20)
fig12.axis(top) font(20)
fig12.axis ticksnone
fig12.axis(right) ticksnone
fig12.options gridnone 
fig12.options fillcolor(white) backcolor(white) 'gridcolor(180,255,170)   
fig12.datelabel format(yyyy) 
fig12.align(3, 2.5, 2.5)
fig12.legend -inbox
fig12.addtext(t,just(c),font(22))
show fig12


