# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A stock-flow consistent (SFC) model with non-bank financial intermediaries
# (Economic Function 2, or EF2), workers and rentiers
#
# Companion code to:
#   Canelli, R., Fontana, G., Realfonzo, R. and Veronese Passarella, M. (2026).
#   "Keynes, Graziani, and Non-Bank Financial Intermediaries: A Stock-Flow
#   Consistent Analysis." Review of Political Economy. Open access.
#   DOI: 10.1080/09538259.2025.2601163
#
# Author:  Marco Veronese Passarella
# Version: 2 October 2026
#
# What this script does.
#   It builds and simulates the SFC model of Section 4 of the paper under
#   three scenarios (baseline, credit exclusion of workers, and EF2 replacing
#   bank loans to workers), performs an accounting consistency check based on
#   the redundant equation, and then draws the corrected version of Figure 4
#   of the paper. The figure shows output, employment, income inequality and
#   wealth inequality in Scenarios 2 and 3. Each series is plotted as the
#   difference from the baseline scenario, multiplied by 100.
#
# Corrections with respect to the published article.
#   This version corrects the balance-sheet identity of banks, that is,
#   equation (34) of the paper, where the securities of EF2 held by banks now
#   enter with a negative sign. It also corrects the coefficient on re in the
#   demand of workers for securities of firms. As a result, the figure drawn
#   by this script differs from the published Figure 4. See the README file
#   of this repository for details.
#
# Licence.
#   This code is released under the Creative Commons Attribution-NonCommercial
#   4.0 International licence (CC BY-NC 4.0), as stated in the LICENSE file of
#   this repository. The article itself is open access under a Creative Commons
#   Attribution-NonCommercial-NoDerivatives (CC BY-NC-ND 4.0) licence. If you
#   use or adapt the code, please cite the paper above.
#
# How to use.
#   Run the whole script. The consistency statement is printed in the console
#   and the figures are drawn on screen. The packages ggplot2 and patchwork
#   are required (with ggplot2 4.0.0 or later, patchwork 1.3.1 or later is
#   needed). The packages progress (progress bar) and beepr (notification
#   sound) are optional and are used only if installed.
#
# Notation.
#   The suffix l (lower class) marks workers and the suffix u (upper class)
#   marks rentiers. The corresponding symbol used in the paper, when it
#   differs from the name used in the code, is reported in brackets.
#
# Variable legend.
#   Output and income.
#           y output (Y), cons total consumption, consl consumption of
#           workers (Cw), consu consumption of rentiers (Cr), g government
#           spending (G), id gross investment (Id), yd total disposable
#           income, ydl disposable income of workers (YDw), ydu disposable
#           income of rentiers (YDr), wb wage bill (WB), w wage rate,
#           n employment, pr labour productivity, t total taxes (T),
#           tl taxes paid by workers (Tw), tu taxes paid by rentiers (Tr),
#           Pf profit of firms, Pb profit of banks, Pq profit of EF2.
#   Capital.
#           k capital stock (K), kt target capital stock (KT),
#           da depreciation allowances (DA), af amortisation funds (AF).
#   Stocks of workers.
#           vl net wealth (Vw), hhl cash (Hw), mhl bank deposits (Mw),
#           bhl government bonds (Bw), ehl securities of firms (Sw),
#           ll_t bank loans demanded (LLw), ll bank loans granted (Lw),
#           fl loans granted by EF2 (LCw).
#   Stocks of rentiers.
#           vu net wealth (Vr), hhu cash (Hr), mhu bank deposits (Mr),
#           bhu government bonds (Br), ehu securities of firms (Sr),
#           qhu securities of EF2 (Zr), lu bank loans (Lr).
#   Other stocks.
#           v total net wealth of households, hh total cash held, hs cash
#           supplied (Hs), mh total deposits held (Md), ms deposits supplied
#           (Ms), bh total bonds held by households, bb bonds held by banks
#           (Bb), bcb bonds held by the central bank (Bcb), bs bonds supplied
#           (Bs), eh total securities of firms held, es securities of firms
#           supplied (Ss), lf bank loans to firms (Lf), ls total bank loans
#           (Ls), qs securities supplied by EF2 (Zs), qb securities of EF2
#           held by banks (Zb).
#   Rates and shares.
#           r_t policy rate, rm rate on deposits, rl rate on bank loans,
#           rb rate on government bonds, re return rate on securities of
#           firms (rs), rq return rate on securities of EF2 (rz), rlq rate on
#           EF2 loans (rlc), sigmab share of the new loan demand of workers
#           accommodated by banks, sigmaq share of the unmet loan demand of
#           workers covered by EF2 (sigma z), rhol loan repayment rate of
#           workers, alpha1l propensity to consume out of income of workers.
#   Indices.
#           TB_yd income inequality (rentiers to workers income ratio),
#           TB_v wealth inequality (rentiers to workers wealth ratio),
#           WSH wage share, PSH profit share.
#   Scenarios (rows of each matrix).
#           1 baseline (banks fully accommodate the loan demand of workers)
#           2 credit exclusion of workers from period 101 (neither banks nor
#             EF2 grant new loans to workers)
#           3 EF2 replaces banks from period 101 (banks grant no new loans
#             to workers, EF2 fully covers the unmet demand)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PREPARE THE WORKSPACE ####
# Clear environment
rm(list = ls(all = TRUE))

# Clear plots
if (!is.null(dev.list())) dev.off()

# Clear console
cat("\014")

# Load required packages
library(ggplot2)
library(patchwork)

# Check whether the optional packages are available
use_progress <- requireNamespace("progress", quietly = TRUE)
use_beepr <- requireNamespace("beepr", quietly = TRUE)

# SET THE SIMULATION ####
# Number of periods
nPeriods <- 180

# Number of scenarios
nScenarios <- 3

# Maximum number of iterations per period
MaxIterations <- 200

# Convergence tolerance (on the redundant equation)
tol <- 1e-6

# Last period before the shocks (shocks start in period shock_period + 1)
shock_period <- 100

# SET THE PORTFOLIO COEFFICIENTS ####
# Workers: demand for government bonds
lambda10l <- 0.1     # Autonomous share of wealth held in bonds
lambda11l <- 0.2     # Coefficient on rb
lambda12l <- -0.1    # Coefficient on rm
lambda13l <- -0.1    # Coefficient on re
lambda15l <- -0.05   # Coefficient on the disposable income to wealth ratio

# Workers: demand for securities issued by firms
lambda30l <- 0       # Autonomous share of wealth held in securities of firms
lambda31l <- -0.025  # Coefficient on rb
lambda32l <- -0.025  # Coefficient on rm
lambda33l <- 0.05    # Coefficient on re
lambda35l <- -0.005  # Coefficient on the disposable income to wealth ratio

# Rentiers: demand for government bonds
lambda10u <- 0.2     # Autonomous share of wealth held in bonds
lambda11u <- 0.3     # Coefficient on rb
lambda12u <- -0.1    # Coefficient on rm
lambda13u <- -0.1    # Coefficient on re
lambda14u <- -0.1    # Coefficient on rq
lambda15u <- -0.05   # Coefficient on the disposable income to wealth ratio

# Rentiers: demand for securities issued by firms
lambda30u <- 0.2     # Autonomous share of wealth held in securities of firms
lambda31u <- -0.1    # Coefficient on rb
lambda32u <- -0.1    # Coefficient on rm
lambda33u <- 0.3     # Coefficient on re
lambda34u <- -0.1    # Coefficient on rq
lambda35u <- -0.05   # Coefficient on the disposable income to wealth ratio

# Rentiers: demand for securities issued by EF2
lambda40u <- 0.0001  # Autonomous share of wealth held in securities of EF2
lambda41u <- 0       # Coefficient on rb
lambda42u <- 0       # Coefficient on rm
lambda43u <- 0       # Coefficient on re
lambda44u <- 0       # Coefficient on rq
lambda45u <- 0       # Coefficient on the disposable income to wealth ratio

# Demand for cash
lambda_cl <- 0.3     # Cash to past consumption ratio of workers
lambda_cu <- 0.2     # Cash to past consumption ratio of rentiers

# SET THE OTHER PARAMETERS AND THE EXOGENOUS VARIABLES ####
# Interest rate set by the central bank
r_t <- 0.01          # Policy rate

# Consumption
alpha10l <- 0.55     # Autonomous propensity to consume out of income, workers
alpha11l <- 0.2      # Sensitivity of alpha1l to credit access
alpha12l <- 0.025    # Sensitivity of alpha1l to the debt to income ratio
alpha2l <- 0.4       # Propensity to consume out of wealth of workers
alpha1u <- 0.6       # Propensity to consume out of income of rentiers
alpha2u <- 0.4       # Propensity to consume out of wealth of rentiers

# Taxation
thetal <- 0.3        # Tax rate on the income of workers
thetau <- 0.3        # Tax rate on the income of rentiers
thetal_v <- 0.10     # Tax rate on the wealth of workers
thetau_v <- 0.10     # Tax rate on the wealth of rentiers

# Production and investment
delta <- 0.1         # Depreciation rate of fixed capital
gamma <- 0.15        # Speed of adjustment of capital to its target level
kappa <- 1.2         # Target capital to output ratio
omega <- 0.5         # Labour hysteresis coefficient

# Personal loans
betal <- 0.1         # New loan demand to disposable income ratio of workers
betau <- 0.1         # New loan demand to disposable income ratio of rentiers
rhol0 <- 0.05        # Autonomous loan repayment rate of workers
rhol1 <- 0.025       # Sensitivity of the repayment rate of workers to rlq
rhou <- 0.05         # Loan repayment rate of rentiers
g_rlq <- 0.002       # Growth rate of the interest rate on EF2 loans

# Loan repayment rate of workers
rhol <- matrix(rhol0, nScenarios, nPeriods)

# Share of the new loan demand of workers accommodated by banks
sigmab <- matrix(1, nScenarios, nPeriods)

# Share of the unmet loan demand of workers covered by EF2
sigmaq <- matrix(0, nScenarios, nPeriods)

# Interest rate on bank deposits
rm <- matrix(r_t, nScenarios, nPeriods)

# Interest rate on bank loans
rl <- matrix(r_t + 0.03, nScenarios, nPeriods)

# Interest rate on government bonds
rb <- matrix(r_t + 0.05, nScenarios, nPeriods)

# Return rate on securities issued by firms
re <- matrix(r_t + 0.05, nScenarios, nPeriods)

# Return rate on securities issued by EF2
rq <- matrix(r_t + 0.16, nScenarios, nPeriods)

# Interest rate on EF2 loans to workers
rlq <- matrix(r_t + 0.20, nScenarios, nPeriods)

# Government spending
g <- matrix(20, nScenarios, nPeriods)

# Wage rate
w <- matrix(0.6, nScenarios, nPeriods)

# Labour productivity
pr <- matrix(1, nScenarios, nPeriods)

# DEFINE THE ENDOGENOUS VARIABLES ####
# Propensity to consume out of disposable income of workers
alpha1l <- matrix(alpha10l + alpha11l * sigmab, nScenarios, nPeriods)

# Output and its components
y <- matrix(0, nScenarios, nPeriods)       # Output (income)
cons <- matrix(0, nScenarios, nPeriods)    # Total consumption
consl <- matrix(0, nScenarios, nPeriods)   # Consumption of workers
consu <- matrix(0, nScenarios, nPeriods)   # Consumption of rentiers
id <- matrix(0, nScenarios, nPeriods)      # Gross investment

# Capital
k <- matrix(0, nScenarios, nPeriods)       # Capital stock
kt <- matrix(0, nScenarios, nPeriods)      # Target capital stock
da <- matrix(0, nScenarios, nPeriods)      # Depreciation allowances
af <- matrix(0, nScenarios, nPeriods)      # Amortisation funds

# Income, taxes and wealth
yd <- matrix(0, nScenarios, nPeriods)      # Total disposable income
ydl <- matrix(0, nScenarios, nPeriods)     # Disposable income of workers
ydu <- matrix(0, nScenarios, nPeriods)     # Disposable income of rentiers
t <- matrix(0, nScenarios, nPeriods)       # Total tax revenue
tl <- matrix(0, nScenarios, nPeriods)      # Taxes paid by workers
tu <- matrix(0, nScenarios, nPeriods)      # Taxes paid by rentiers
v <- matrix(0, nScenarios, nPeriods)       # Total net wealth of households
vl <- matrix(0, nScenarios, nPeriods)      # Net wealth of workers
vu <- matrix(0, nScenarios, nPeriods)      # Net wealth of rentiers

# Labour market and profits
wb <- matrix(0, nScenarios, nPeriods)      # Wage bill
n <- matrix(0, nScenarios, nPeriods)       # Employment
Pf <- matrix(0, nScenarios, nPeriods)      # Profit of firms
Pb <- matrix(0, nScenarios, nPeriods)      # Profit of banks
Pq <- matrix(0, nScenarios, nPeriods)      # Profit of EF2

# Cash
hh <- matrix(0, nScenarios, nPeriods)      # Total cash held by households
hhl <- matrix(0, nScenarios, nPeriods)     # Cash held by workers
hhu <- matrix(0, nScenarios, nPeriods)     # Cash held by rentiers
hs <- matrix(0, nScenarios, nPeriods)      # Cash supplied by the central bank

# Bank deposits
mh <- matrix(0, nScenarios, nPeriods)      # Total deposits held by households
mhl <- matrix(0, nScenarios, nPeriods)     # Deposits held by workers
mhu <- matrix(0, nScenarios, nPeriods)     # Deposits held by rentiers
ms <- matrix(0, nScenarios, nPeriods)      # Deposits supplied by banks

# Government bonds
bs <- matrix(0, nScenarios, nPeriods)      # Bonds supplied by the government
bh <- matrix(0, nScenarios, nPeriods)      # Total bonds held by households
bhl <- matrix(0, nScenarios, nPeriods)     # Bonds held by workers
bhu <- matrix(0, nScenarios, nPeriods)     # Bonds held by rentiers
bb <- matrix(0, nScenarios, nPeriods)      # Bonds held by banks
bcb <- matrix(0, nScenarios, nPeriods)     # Bonds held by the central bank

# Securities issued by firms
es <- matrix(0, nScenarios, nPeriods)      # Securities supplied by firms
eh <- matrix(0, nScenarios, nPeriods)      # Total securities of firms held
ehl <- matrix(0, nScenarios, nPeriods)     # Securities of firms, workers
ehu <- matrix(0, nScenarios, nPeriods)     # Securities of firms, rentiers

# Bank loans
lf <- matrix(0, nScenarios, nPeriods)      # Bank loans to firms
ls <- matrix(0, nScenarios, nPeriods)      # Total supply of bank loans
ll_t <- matrix(0, nScenarios, nPeriods)    # Bank loans demanded by workers
ll <- matrix(0, nScenarios, nPeriods)      # Bank loans granted to workers
lu <- matrix(0, nScenarios, nPeriods)      # Bank loans to rentiers

# EF2 loans and securities
fl <- matrix(0, nScenarios, nPeriods)      # Loans granted by EF2 to workers
qs <- matrix(0, nScenarios, nPeriods)      # Securities supplied by EF2
qhu <- matrix(0, nScenarios, nPeriods)     # Securities of EF2, rentiers
qb <- matrix(0, nScenarios, nPeriods)      # Securities of EF2 held by banks

# Distribution indices
TB_yd <- matrix(0, nScenarios, nPeriods)   # Rentiers to workers income ratio
TB_v <- matrix(0, nScenarios, nPeriods)    # Rentiers to workers wealth ratio
WSH <- matrix(0, nScenarios, nPeriods)     # Wage share
PSH <- matrix(0, nScenarios, nPeriods)     # Profit share

# Number of iterations used in each scenario and period
iter_used <- matrix(0, nScenarios, nPeriods)

# RUN THE MODEL ####
# Create the progress bar (if the progress package is installed)
if (use_progress) {
  pb <- progress::progress_bar$new(
    total = nScenarios * nPeriods,
    format = "Progress [:bar] :percent ETA: :eta"
  )
}

# Loop over scenarios
for (j in 1:nScenarios) {

  # Loop over periods
  for (i in 2:nPeriods) {

    # Update the progress bar
    if (use_progress) pb$tick()

    # Scenario 2: credit exclusion of workers
    if (i > shock_period && j == 2) {
      # Banks grant no new loans to workers
      sigmab[j, i] = 0
      # EF2 does not step in
      sigmaq[j, i] = 0
    }

    # Scenario 3: EF2 replaces banks
    if (i > shock_period && j == 3) {
      # Banks grant no new loans to workers
      sigmab[j, i] = 0
      # EF2 fully covers the unmet loan demand of workers
      sigmaq[j, i] = 1
    }

    # Iterate until the simultaneous solution is found
    for (iterations in 1:MaxIterations) {

      # Production firms ####
      # Output
      y[j, i] = cons[j, i] + g[j, i] + id[j, i]

      # Target capital stock
      kt[j, i] = kappa * y[j, i-1]

      # Depreciation allowances
      da[j, i] = delta * k[j, i-1]

      # Amortisation funds
      af[j, i] = da[j, i]

      # Gross investment
      id[j, i] = gamma * (kt[j, i] - k[j, i-1]) + da[j, i]

      # Capital stock
      k[j, i] = k[j, i-1] + id[j, i] - da[j, i]

      # Securities supplied by firms (on demand)
      es[j, i] = eh[j, i]

      # Bank loans to firms (residual source of finance)
      lf[j, i] = lf[j, i-1] + id[j, i] - (es[j, i] - es[j, i-1]) - af[j, i]

      # Households: income, consumption and wealth ####
      # Disposable income of workers
      ydl[j, i] = wb[j, i] - tl[j, i] + re[j, i-1] * ehl[j, i-1] +
        rb[j, i-1] * bhl[j, i-1] + rm[j, i-1] * mhl[j, i-1] -
        rl[j, i-1] * ll[j, i-1] - rlq[j, i-1] * fl[j, i-1]

      # Disposable income of rentiers
      ydu[j, i] = Pf[j, i] + Pb[j, i] + Pq[j, i] - tu[j, i] +
        re[j, i-1] * ehu[j, i-1] + rq[j, i-1] * qhu[j, i-1] +
        rb[j, i-1] * bhu[j, i-1] + rm[j, i-1] * mhu[j, i-1] -
        rl[j, i-1] * lu[j, i-1]

      # Total disposable income
      yd[j, i] = ydl[j, i] + ydu[j, i]

      # Consumption of workers
      consl[j, i] = alpha1l[j, i] * ydl[j, i] + alpha2l * vl[j, i-1]

      # Propensity to consume out of income of workers (it increases with
      # past credit access and decreases with the past debt to income ratio)
      if (i > 2) {
        alpha1l[j, i] = alpha10l +
          alpha11l * (ll[j, i-1] + fl[j, i-1]) / ll_t[j, i-1] -
          alpha12l * (ll[j, i-1] + fl[j, i-1]) / ydl[j, i-1]
      }

      # Consumption of rentiers
      consu[j, i] = alpha1u * ydu[j, i] + alpha2u * vu[j, i-1]

      # Total consumption
      cons[j, i] = consl[j, i] + consu[j, i]

      # Taxes paid by workers
      tl[j, i] = thetal * (wb[j, i] + re[j, i-1] * ehl[j, i-1] +
        rb[j, i-1] * bhl[j, i-1] + rm[j, i-1] * mhl[j, i-1] -
        rl[j, i-1] * ll[j, i-1] - rlq[j, i-1] * fl[j, i-1]) +
        thetal_v * vl[j, i-1]

      # Taxes paid by rentiers
      tu[j, i] = thetau * (Pf[j, i] + Pb[j, i] + Pq[j, i] +
        re[j, i-1] * ehu[j, i-1] + rq[j, i-1] * qhu[j, i-1] +
        rb[j, i-1] * bhu[j, i-1] + rm[j, i-1] * mhu[j, i-1] -
        rl[j, i-1] * lu[j, i-1]) + thetau_v * vu[j, i-1]

      # Net wealth of workers
      vl[j, i] = vl[j, i-1] + (ydl[j, i] - consl[j, i])

      # Net wealth of rentiers
      vu[j, i] = vu[j, i-1] + (ydu[j, i] - consu[j, i])

      # Total net wealth of households
      v[j, i] = vu[j, i] + vl[j, i]

      # Portfolio decisions of workers ####
      # Bank loans demanded by workers
      ll_t[j, i] = ll[j, i-1] + betal * ydl[j, i] - rhol[j, i] * ll[j, i-1]

      # Cash held by workers (based on past consumption)
      hhl[j, i] = lambda_cl * consl[j, i-1]

      # Government bonds held by workers
      bhl[j, i] = vl[j, i] * (lambda10l + lambda11l * rb[j, i] -
        lambda12l * rm[j, i] - lambda13l * re[j, i] -
        lambda15l * (ydl[j, i] / vl[j, i]))

      # Securities of firms held by workers
      ehl[j, i] = vl[j, i] * (lambda30l - lambda31l * rb[j, i] -
        lambda32l * rm[j, i] + lambda33l * re[j, i] -
        lambda35l * (ydl[j, i] / vl[j, i]))

      # Bank deposits held by workers (buffer stock)
      mhl[j, i] = vl[j, i] + ll[j, i] + fl[j, i] - bhl[j, i] - ehl[j, i] -
        hhl[j, i]

      # Portfolio decisions of rentiers ####
      # Bank loans to rentiers
      lu[j, i] = lu[j, i-1] + betau * ydu[j, i] - rhou * lu[j, i-1]

      # Cash held by rentiers (based on past consumption)
      hhu[j, i] = lambda_cu * consu[j, i-1]

      # Government bonds held by rentiers
      bhu[j, i] = vu[j, i] * (lambda10u + lambda11u * rb[j, i] -
        lambda12u * rm[j, i] - lambda13u * re[j, i] - lambda14u * rq[j, i] -
        lambda15u * (ydu[j, i] / vu[j, i]))

      # Securities of firms held by rentiers
      ehu[j, i] = vu[j, i] * (lambda30u - lambda31u * rb[j, i] -
        lambda32u * rm[j, i] + lambda33u * re[j, i] - lambda34u * rq[j, i] -
        lambda35u * (ydu[j, i] / vu[j, i]))

      # Securities of EF2 held by rentiers (capped by the supply)
      qhu[j, i] = min(qs[j, i], vu[j, i] * (lambda40u - lambda41u * rb[j, i] -
        lambda42u * rm[j, i] - lambda43u * re[j, i] + lambda44u * rq[j, i] -
        lambda45u * (ydu[j, i] / vu[j, i])))

      # Bank deposits held by rentiers (buffer stock)
      mhu[j, i] = vu[j, i] + lu[j, i] - bhu[j, i] - ehu[j, i] - qhu[j, i] -
        hhu[j, i]

      # Aggregate asset holdings of households ####
      # Total cash held
      hh[j, i] = hhl[j, i] + hhu[j, i]

      # Total government bonds held
      bh[j, i] = bhl[j, i] + bhu[j, i]

      # Total bank deposits held
      mh[j, i] = mhl[j, i] + mhu[j, i]

      # Total securities of firms held
      eh[j, i] = ehl[j, i] + ehu[j, i]

      # Commercial banks ####
      # Bank loans granted to workers (credit constraint)
      ll[j, i] = ll[j, i-1] + sigmab[j, i] * (ll_t[j, i] - ll[j, i-1])

      # Total supply of bank loans
      ls[j, i] = ls[j, i-1] + (lf[j, i] - lf[j, i-1]) +
        (ll[j, i] - ll[j, i-1]) + (lu[j, i] - lu[j, i-1])

      # Supply of bank deposits (on demand)
      ms[j, i] = mh[j, i]

      # Government bonds held by banks (if negative, advances from the
      # central bank). Note: this corrects the sign of qb in equation (34)
      # of the paper
      bb[j, i] = ms[j, i] - qb[j, i] - ls[j, i]

      # Profit of banks
      Pb[j, i] = rl[j, i-1] * ls[j, i-1] + rq[j, i-1] * qb[j, i-1] +
        rb[j, i-1] * bb[j, i-1] - rm[j, i-1] * ms[j, i-1]

      # EF2 ####
      # Loans granted by EF2 to workers
      fl[j, i] = sigmaq[j, i] * (ll_t[j, i] - ll[j, i])

      # Securities supplied by EF2
      qs[j, i] = fl[j, i]

      # Securities of EF2 held by banks (residual buyers)
      qb[j, i] = qs[j, i] - qhu[j, i]

      # Profit of EF2
      Pq[j, i] = rlq[j, i-1] * fl[j, i-1] - rq[j, i-1] * qs[j, i-1]

      # Interest rate on EF2 loans and repayment rate of workers (they
      # change only when EF2 loans are positive)
      if (fl[j, i] > 0) {
        rlq[j, i] = rlq[j, i-1] * (1 + g_rlq)
        rhol[j, i] = rhol0 - rhol1 * rlq[j, i-1]
      }

      # Government ####
      # Total tax revenue
      t[j, i] = tl[j, i] + tu[j, i]

      # Supply of government bonds (net of central bank interest payments)
      bs[j, i] = bs[j, i-1] + (g[j, i] + rb[j, i-1] * bs[j, i-1]) -
        (t[j, i] + rb[j, i-1] * bcb[j, i-1])

      # Central bank ####
      # Government bonds held by the central bank (residual buyer)
      bcb[j, i] = bs[j, i] - bh[j, i] - bb[j, i]

      # Supply of cash
      hs[j, i] = hs[j, i-1] + bcb[j, i] - bcb[j, i-1]

      # Labour market and profit of firms ####
      # Wage bill
      wb[j, i] = w[j, i] * n[j, i]

      # Employment (with a smoothing component)
      n[j, i] = omega * n[j, i-1] + (1 - omega) * (y[j, i] / pr[j, i])

      # Profit of firms
      Pf[j, i] = y[j, i] - wb[j, i] - af[j, i] - rl[j, i-1] * lf[j, i-1] -
        re[j, i-1] * eh[j, i-1]

      # Distribution indices ####
      # Income inequality index
      TB_yd[j, i] = ydu[j, i] / ydl[j, i]

      # Wealth inequality index
      TB_v[j, i] = vu[j, i] / vl[j, i]

      # Wage share
      WSH[j, i] = wb[j, i] / y[j, i]

      # Profit share
      PSH[j, i] = (Pf[j, i] + Pb[j, i] + Pq[j, i]) / y[j, i]

      # Convergence check ####
      # Stop iterating once the redundant equation (cash supplied equal to
      # cash held) is met within the tolerance
      if (i > 2) {
        if (abs(hs[j, i] - hh[j, i]) < tol) {
          iter_used[j, i] <- iterations
          break
        }
      }

      # Record when the maximum number of iterations is reached
      if (iterations == MaxIterations) {
        iter_used[j, i] <- MaxIterations
      }

    }
  }
}

# Close the progress bar
if (use_progress) pb$terminate()

# CHECK THE CONSISTENCY OF THE MODEL ####
# Compute the cumulative and average absolute gap of the redundant equation
error <- 0
for (j in 1:nScenarios) {
  for (i in 2:(nPeriods - 1)) {
    error <- error + abs(hh[j, i] - hs[j, i])
  }
}
aerror <- error / (nPeriods * nScenarios)

# Print the consistency statement
if (aerror < 0.1) {
  cat(" \n **************************************** \n",
      "Good news! The model is watertight! \n",
      "Average error =", aerror, "< 0.1 \n",
      "Cumulative error =", error,
      "\n ****************************************")
} else if (aerror < 1) {
  cat(" \n **************************************** \n",
      "Minor issues with model consistency \n",
      "Average error =", aerror, "> 0.1 \n",
      "Cumulative error =", error,
      "\n ****************************************")
} else {
  cat(" \n ******************************************* \n",
      "Warning: the model is not fully consistent! \n",
      "Average error =", aerror, "> 1 \n",
      "Cumulative error =", error,
      "\n *******************************************")
}
cat("\n Number of scenarios =", nScenarios)
cat("\n Number of periods =", nPeriods)
cat("\n Convergence tolerance =", tol)
cat("\n Max number of iterations =", max(iter_used), "/", MaxIterations)
cat("\n ****************************************\n")

# Visual consistency check (redundant equation, baseline scenario)
plot(hh[1, 2:nPeriods] - hs[1, 2:nPeriods],
     type = "l", col = 3, lwd = 2, lty = 1, font.main = 1, cex.main = 1,
     main = expression("Consistency check: " * italic(H[h] - H[s])),
     ylab = "", xlab = "", ylim = range(-1, 1))

# Add a shaded band around zero
mycol1 <- rgb(20, 200, 20, max = 255, alpha = 20)
rect(xleft = -5, xright = 205, ybottom = -0.5, ytop = 0.5,
     col = mycol1, border = NA)

# Play a notification sound (if the beepr package is installed)
if (use_beepr) beepr::beep(sound = 2)

# PLOT FIGURE 4 ####
# Scenarios to compare with the baseline
scenarios_to_plot <- c(2, 3)

# First and last period shown in the charts
init <- 99
nPeriods2 <- 130

# Variables to plot
vars_to_plot <- c("y", "n", "TB_yd", "TB_v")

# Titles of the panels
var_labels <- c(
  y = "Output",
  n = "Employment",
  TB_yd = "Income inequality",
  TB_v = "Wealth inequality"
)

# Collect the selected matrices into a list
data_list <- mget(vars_to_plot)

# Create one chart for each variable
plots <- lapply(names(data_list), function(varname) {

  # Compute the difference from the baseline scenario, multiplied by 100
  df <- do.call(rbind, lapply(scenarios_to_plot, function(s) {
    data.frame(
      Period = init:nPeriods2,
      Value = 100 * data_list[[varname]][s, init:nPeriods2] -
        100 * data_list[[varname]][1, init:nPeriods2],
      Scenario = paste("Scenario", s),
      Variable = varname
    )
  }))

  # Draw the chart
  ggplot(df, aes(x = Period, y = Value,
                 colour = Scenario,
                 linetype = Scenario)) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(title = var_labels[[varname]], x = "", y = "") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title.x = element_text(size = 11),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank()
    )
})

# Combine the charts and collect the legend at the bottom
combined_plot <- wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "lines")
  )

# Display the combined figure
print(combined_plot)
