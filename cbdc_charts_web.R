# Upload libraries
library(tidyverse)
library(ggplot2)

# Define time span (< nPeriods)
tspan = 40

# Create a data frame
df <- expand.grid(Scenario = 1:5, Period = 5:tspan)

# Attribute names to scenarios
df <- df %>%
  mutate(Scenario = case_when(
    Scenario == 1 ~ "1. CBDC issues (with rdc=rm)",
    Scenario == 2 ~ "2. CBDC issues (with rdc>rm)",
    Scenario == 3 ~ "3. Policy rate cut (no CBDCs)",
    Scenario == 4 ~ "4. Policy rate cut (CBDCs)",
    Scenario == 5 ~ "5. QE for the people",
    TRUE ~ as.character(Scenario)  # Keep other scenarios as they are
  ))

# Replace variables with values to baseline

# Apply the operations to yn
yn[5, 5:tspan] <- yn[5, 5:tspan] / yn[2, 5:tspan]
yn[2, 5:tspan] <- yn[2, 5:tspan] / yn[1, 5:tspan]
yn[3, 5:tspan] <- yn[3, 5:tspan] / yn[1, 5:tspan]
yn[4, 5:tspan] <- yn[4, 5:tspan] / yn[1, 5:tspan]
yn[6, 5:tspan] <- yn[6, 5:tspan] / yn[1, 5:tspan]

# Apply the operations to ydw_net
ydw_net[5, 5:tspan] <- ydw_net[5, 5:tspan] / ydw_net[2, 5:tspan]
ydw_net[2, 5:tspan] <- ydw_net[2, 5:tspan] / ydw_net[1, 5:tspan]
ydw_net[3, 5:tspan] <- ydw_net[3, 5:tspan] / ydw_net[1, 5:tspan]
ydw_net[4, 5:tspan] <- ydw_net[4, 5:tspan] / ydw_net[1, 5:tspan]
ydw_net[6, 5:tspan] <- ydw_net[6, 5:tspan] / ydw_net[1, 5:tspan]

# Apply the operations to ydz_net
ydz_net[5, 5:tspan] <- ydz_net[5, 5:tspan] / ydz_net[2, 5:tspan]
ydz_net[2, 5:tspan] <- ydz_net[2, 5:tspan] / ydz_net[1, 5:tspan]
ydz_net[3, 5:tspan] <- ydz_net[3, 5:tspan] / ydz_net[1, 5:tspan]
ydz_net[4, 5:tspan] <- ydz_net[4, 5:tspan] / ydz_net[1, 5:tspan]
ydz_net[6, 5:tspan] <- ydz_net[6, 5:tspan] / ydz_net[1, 5:tspan]

# Apply the operations to vw
vw[5, 5:tspan] <- vw[5, 5:tspan] / vw[2, 5:tspan]
vw[2, 5:tspan] <- vw[2, 5:tspan] / vw[1, 5:tspan]
vw[3, 5:tspan] <- vw[3, 5:tspan] / vw[1, 5:tspan]
vw[4, 5:tspan] <- vw[4, 5:tspan] / vw[1, 5:tspan]
vw[6, 5:tspan] <- vw[6, 5:tspan] / vw[1, 5:tspan]

# Apply the operations to vz
vz[5, 5:tspan] <- vz[5, 5:tspan] / vz[2, 5:tspan]
vz[2, 5:tspan] <- vz[2, 5:tspan] / vz[1, 5:tspan]
vz[3, 5:tspan] <- vz[3, 5:tspan] / vz[1, 5:tspan]
vz[4, 5:tspan] <- vz[4, 5:tspan] / vz[1, 5:tspan]
vz[6, 5:tspan] <- vz[6, 5:tspan] / vz[1, 5:tspan]

# Apply the operations to cw
cw[5, 5:tspan] <- cw[5, 5:tspan] / cw[2, 5:tspan]
cw[2, 5:tspan] <- cw[2, 5:tspan] / cw[1, 5:tspan]
cw[3, 5:tspan] <- cw[3, 5:tspan] / cw[1, 5:tspan]
cw[4, 5:tspan] <- cw[4, 5:tspan] / cw[1, 5:tspan]
cw[6, 5:tspan] <- cw[6, 5:tspan] / cw[1, 5:tspan]

# Apply the operations to cz
cz[5, 5:tspan] <- cz[5, 5:tspan] / cz[2, 5:tspan]
cz[2, 5:tspan] <- cz[2, 5:tspan] / cz[1, 5:tspan]
cz[3, 5:tspan] <- cz[3, 5:tspan] / cz[1, 5:tspan]
cz[4, 5:tspan] <- cz[4, 5:tspan] / cz[1, 5:tspan]
cz[6, 5:tspan] <- cz[6, 5:tspan] / cz[1, 5:tspan]

# Apply the operations to lw
lw[5, 5:tspan] <- lw[5, 5:tspan] / lw[2, 5:tspan]
lw[2, 5:tspan] <- lw[2, 5:tspan] / lw[1, 5:tspan]
lw[3, 5:tspan] <- lw[3, 5:tspan] / lw[1, 5:tspan]
lw[4, 5:tspan] <- lw[4, 5:tspan] / lw[1, 5:tspan]
lw[6, 5:tspan] <- lw[6, 5:tspan] / lw[1, 5:tspan]

# Apply the operations to lz
lz[5, 5:tspan] <- lz[5, 5:tspan] / lz[2, 5:tspan]
lz[2, 5:tspan] <- lz[2, 5:tspan] / lz[1, 5:tspan]
lz[3, 5:tspan] <- lz[3, 5:tspan] / lz[1, 5:tspan]
lz[4, 5:tspan] <- lz[4, 5:tspan] / lz[1, 5:tspan]
lz[6, 5:tspan] <- lz[6, 5:tspan] / lz[1, 5:tspan]

# Apply the operations to id
id[5, 5:tspan] <- id[5, 5:tspan] / id[2, 5:tspan]
id[2, 5:tspan] <- id[2, 5:tspan] / id[1, 5:tspan]
id[3, 5:tspan] <- id[3, 5:tspan] / id[1, 5:tspan]
id[4, 5:tspan] <- id[4, 5:tspan] / id[1, 5:tspan]
id[6, 5:tspan] <- id[6, 5:tspan] / id[1, 5:tspan]

# Apply the operations to da
da[5, 5:tspan] <- da[5, 5:tspan] / da[2, 5:tspan]
da[2, 5:tspan] <- da[2, 5:tspan] / da[1, 5:tspan]
da[3, 5:tspan] <- da[3, 5:tspan] / da[1, 5:tspan]
da[4, 5:tspan] <- da[4, 5:tspan] / da[1, 5:tspan]
da[6, 5:tspan] <- da[6, 5:tspan] / da[1, 5:tspan]

# Apply the operations to k
k[5, 5:tspan] <- k[5, 5:tspan] / k[2, 5:tspan]
k[2, 5:tspan] <- k[2, 5:tspan] / k[1, 5:tspan]
k[3, 5:tspan] <- k[3, 5:tspan] / k[1, 5:tspan]
k[4, 5:tspan] <- k[4, 5:tspan] / k[1, 5:tspan]
k[6, 5:tspan] <- k[6, 5:tspan] / k[1, 5:tspan]

# Apply the operations to pf
pf[5, 5:tspan] <- pf[5, 5:tspan] / pf[2, 5:tspan]
pf[2, 5:tspan] <- pf[2, 5:tspan] / pf[1, 5:tspan]
pf[3, 5:tspan] <- pf[3, 5:tspan] / pf[1, 5:tspan]
pf[4, 5:tspan] <- pf[4, 5:tspan] / pf[1, 5:tspan]
pf[6, 5:tspan] <- pf[6, 5:tspan] / pf[1, 5:tspan]

# Apply the operations to tax_net
tax_net[5, 5:tspan] <- tax_net[5, 5:tspan] / tax_net[2, 5:tspan]
tax_net[2, 5:tspan] <- tax_net[2, 5:tspan] / tax_net[1, 5:tspan]
tax_net[3, 5:tspan] <- tax_net[3, 5:tspan] / tax_net[1, 5:tspan]
tax_net[4, 5:tspan] <- tax_net[4, 5:tspan] / tax_net[1, 5:tspan]
tax_net[6, 5:tspan] <- tax_net[6, 5:tspan] / tax_net[1, 5:tspan]

# Apply the operations to taxw_net
taxw_net[5, 5:tspan] <- taxw_net[5, 5:tspan] / taxw_net[2, 5:tspan]
taxw_net[2, 5:tspan] <- taxw_net[2, 5:tspan] / taxw_net[1, 5:tspan]
taxw_net[3, 5:tspan] <- taxw_net[3, 5:tspan] / taxw_net[1, 5:tspan]
taxw_net[4, 5:tspan] <- taxw_net[4, 5:tspan] / taxw_net[1, 5:tspan]
taxw_net[6, 5:tspan] <- taxw_net[6, 5:tspan] / taxw_net[1, 5:tspan]

# Apply the operations to taxz_net
taxz_net[5, 5:tspan] <- taxz_net[5, 5:tspan] / taxz_net[2, 5:tspan]
taxz_net[2, 5:tspan] <- taxz_net[2, 5:tspan] / taxz_net[1, 5:tspan]
taxz_net[3, 5:tspan] <- taxz_net[3, 5:tspan] / taxz_net[1, 5:tspan]
taxz_net[4, 5:tspan] <- taxz_net[4, 5:tspan] / taxz_net[1, 5:tspan]
taxz_net[6, 5:tspan] <- taxz_net[6, 5:tspan] / taxz_net[1, 5:tspan]

# Apply the operations to def
def[5, 5:tspan] <- def[5, 5:tspan] - def[2, 5:tspan]
def[2, 5:tspan] <- def[2, 5:tspan] - def[1, 5:tspan]
def[3, 5:tspan] <- def[3, 5:tspan] - def[1, 5:tspan]
def[4, 5:tspan] <- def[4, 5:tspan] - def[1, 5:tspan]
def[6, 5:tspan] <- def[6, 5:tspan] - def[1, 5:tspan]

# Apply the operations to seign
seign[5, 5:tspan] <- seign[5, 5:tspan] / seign[2, 5:tspan]
seign[2, 5:tspan] <- seign[2, 5:tspan] / seign[1, 5:tspan]
seign[3, 5:tspan] <- seign[3, 5:tspan] / seign[1, 5:tspan]
seign[4, 5:tspan] <- seign[4, 5:tspan] / seign[1, 5:tspan]
seign[6, 5:tspan] <- seign[6, 5:tspan] / seign[1, 5:tspan]

# Apply the operations to bs
bs[5, 5:tspan] <- bs[5, 5:tspan] / bs[2, 5:tspan]
bs[2, 5:tspan] <- bs[2, 5:tspan] / bs[1, 5:tspan]
bs[3, 5:tspan] <- bs[3, 5:tspan] / bs[1, 5:tspan]
bs[4, 5:tspan] <- bs[4, 5:tspan] / bs[1, 5:tspan]
bs[6, 5:tspan] <- bs[6, 5:tspan] / bs[1, 5:tspan]

# Apply the operations to bcb
bcb[5, 5:tspan] <- bcb[5, 5:tspan] / bcb[2, 5:tspan]
bcb[2, 5:tspan] <- bcb[2, 5:tspan] / bcb[1, 5:tspan]
bcb[3, 5:tspan] <- bcb[3, 5:tspan] / bcb[1, 5:tspan]
bcb[4, 5:tspan] <- bcb[4, 5:tspan] / bcb[1, 5:tspan]
bcb[6, 5:tspan] <- bcb[6, 5:tspan] / bcb[1, 5:tspan]

# Apply the operations to hs
hs[5, 5:tspan] <- hs[5, 5:tspan] / hs[2, 5:tspan]
hs[2, 5:tspan] <- hs[2, 5:tspan] / hs[1, 5:tspan]
hs[3, 5:tspan] <- hs[3, 5:tspan] / hs[1, 5:tspan]
hs[4, 5:tspan] <- hs[4, 5:tspan] / hs[1, 5:tspan]
hs[6, 5:tspan] <- hs[6, 5:tspan] / hs[1, 5:tspan]

# Apply the operations to as
as[5, 5:tspan] <- as[5, 5:tspan] - as[2, 5:tspan]
as[2, 5:tspan] <- as[2, 5:tspan] - as[1, 5:tspan]
as[3, 5:tspan] <- as[3, 5:tspan] - as[1, 5:tspan]
as[4, 5:tspan] <- as[4, 5:tspan] - as[1, 5:tspan]
as[6, 5:tspan] <- as[6, 5:tspan] - as[1, 5:tspan]

# Apply the operations to lf
lf[5, 5:tspan] <- lf[5, 5:tspan] / lf[2, 5:tspan]
lf[2, 5:tspan] <- lf[2, 5:tspan] / lf[1, 5:tspan]
lf[3, 5:tspan] <- lf[3, 5:tspan] / lf[1, 5:tspan]
lf[4, 5:tspan] <- lf[4, 5:tspan] / lf[1, 5:tspan]
lf[6, 5:tspan] <- lf[6, 5:tspan] / lf[1, 5:tspan]

# Apply the operations to ls
ls[5, 5:tspan] <- ls[5, 5:tspan] / ls[2, 5:tspan]
ls[2, 5:tspan] <- ls[2, 5:tspan] / ls[1, 5:tspan]
ls[3, 5:tspan] <- ls[3, 5:tspan] / ls[1, 5:tspan]
ls[4, 5:tspan] <- ls[4, 5:tspan] / ls[1, 5:tspan]
ls[6, 5:tspan] <- ls[6, 5:tspan] / ls[1, 5:tspan]

# Apply the operations to ms
ms[5, 5:tspan] <- ms[5, 5:tspan] / ms[2, 5:tspan]
ms[2, 5:tspan] <- ms[2, 5:tspan] / ms[1, 5:tspan]
ms[3, 5:tspan] <- ms[3, 5:tspan] / ms[1, 5:tspan]
ms[4, 5:tspan] <- ms[4, 5:tspan] / ms[1, 5:tspan]
ms[6, 5:tspan] <- ms[6, 5:tspan] / ms[1, 5:tspan]

# Apply the operations to pB
pB[5, 5:tspan] <- pB[5, 5:tspan] / pB[2, 5:tspan]
pB[2, 5:tspan] <- pB[2, 5:tspan] / pB[1, 5:tspan]
pB[3, 5:tspan] <- pB[3, 5:tspan] / pB[1, 5:tspan]
pB[4, 5:tspan] <- pB[4, 5:tspan] / pB[1, 5:tspan]
pB[6, 5:tspan] <- pB[6, 5:tspan] / pB[1, 5:tspan]

# Apply the operations to im
im[5, 5:tspan] <- im[5, 5:tspan] / im[2, 5:tspan]
im[2, 5:tspan] <- im[2, 5:tspan] / im[1, 5:tspan]
im[3, 5:tspan] <- im[3, 5:tspan] / im[1, 5:tspan]
im[4, 5:tspan] <- im[4, 5:tspan] / im[1, 5:tspan]
im[6, 5:tspan] <- im[6, 5:tspan] / im[1, 5:tspan]

# Apply the operations to tb
tb[5, 5:tspan] <- tb[5, 5:tspan] - tb[2, 5:tspan]
tb[2, 5:tspan] <- tb[2, 5:tspan] - tb[1, 5:tspan]
tb[3, 5:tspan] <- tb[3, 5:tspan] - tb[1, 5:tspan]
tb[4, 5:tspan] <- tb[4, 5:tspan] - tb[1, 5:tspan]
tb[6, 5:tspan] <- tb[6, 5:tspan] - tb[1, 5:tspan]

# Apply the operations to cab
cab[5, 5:tspan] <- cab[5, 5:tspan] - cab[2, 5:tspan]
cab[2, 5:tspan] <- cab[2, 5:tspan] - cab[1, 5:tspan]
cab[3, 5:tspan] <- cab[3, 5:tspan] - cab[1, 5:tspan]
cab[4, 5:tspan] <- cab[4, 5:tspan] - cab[1, 5:tspan]
cab[6, 5:tspan] <- cab[6, 5:tspan] - cab[1, 5:tspan]

# Apply the operations to niip
niip[5, 5:tspan] <- niip[5, 5:tspan] - niip[2, 5:tspan]
niip[2, 5:tspan] <- niip[2, 5:tspan] - niip[1, 5:tspan]
niip[3, 5:tspan] <- niip[3, 5:tspan] - niip[1, 5:tspan]
niip[4, 5:tspan] <- niip[4, 5:tspan] - niip[1, 5:tspan]
niip[6, 5:tspan] <- niip[6, 5:tspan] - niip[1, 5:tspan]

# Apply the operations to bf
bf[5, 5:tspan] <- bf[5, 5:tspan] / bf[2, 5:tspan]
bf[2, 5:tspan] <- bf[2, 5:tspan] / bf[1, 5:tspan]
bf[3, 5:tspan] <- bf[3, 5:tspan] / bf[1, 5:tspan]
bf[4, 5:tspan] <- bf[4, 5:tspan] / bf[1, 5:tspan]
bf[6, 5:tspan] <- bf[6, 5:tspan] / bf[1, 5:tspan]

# Apply the operations to qs
qs[5, 5:tspan] <- qs[5, 5:tspan] / qs[2, 5:tspan]
qs[2, 5:tspan] <- qs[2, 5:tspan] / qs[1, 5:tspan]
qs[3, 5:tspan] <- qs[3, 5:tspan] / qs[1, 5:tspan]
qs[4, 5:tspan] <- qs[4, 5:tspan] / qs[1, 5:tspan]
qs[6, 5:tspan] <- qs[6, 5:tspan] / qs[1, 5:tspan]

# Apply the operations to wab
wab[5, 5:tspan] <- wab[5, 5:tspan] / wab[2, 5:tspan]
wab[2, 5:tspan] <- wab[2, 5:tspan] / wab[1, 5:tspan]
wab[3, 5:tspan] <- wab[3, 5:tspan] / wab[1, 5:tspan]
wab[4, 5:tspan] <- wab[4, 5:tspan] / wab[1, 5:tspan]
wab[6, 5:tspan] <- wab[6, 5:tspan] / wab[1, 5:tspan]

# Apply the operations to n
n[5, 5:tspan] <- n[5, 5:tspan] / n[2, 5:tspan]
n[2, 5:tspan] <- n[2, 5:tspan] / n[1, 5:tspan]
n[3, 5:tspan] <- n[3, 5:tspan] / n[1, 5:tspan]
n[4, 5:tspan] <- n[4, 5:tspan] / n[1, 5:tspan]
n[6, 5:tspan] <- n[6, 5:tspan] / n[1, 5:tspan]

# Apply the operations to hw
hw[5, 5:tspan] <- hw[5, 5:tspan] / hw[2, 5:tspan]
hw[2, 5:tspan] <- hw[2, 5:tspan] / hw[1, 5:tspan]
hw[3, 5:tspan] <- hw[3, 5:tspan] / hw[1, 5:tspan]
hw[4, 5:tspan] <- hw[4, 5:tspan] / hw[1, 5:tspan]
hw[6, 5:tspan] <- hw[6, 5:tspan] / hw[1, 5:tspan]

# Apply the operations to hz
hz[5, 5:tspan] <- hz[5, 5:tspan] / hz[2, 5:tspan]
hz[2, 5:tspan] <- hz[2, 5:tspan] / hz[1, 5:tspan]
hz[3, 5:tspan] <- hz[3, 5:tspan] / hz[1, 5:tspan]
hz[4, 5:tspan] <- hz[4, 5:tspan] / hz[1, 5:tspan]
hz[6, 5:tspan] <- hz[6, 5:tspan] / hz[1, 5:tspan]

# Apply the operations to bw
bw[5, 5:tspan] <- bw[5, 5:tspan] / bw[2, 5:tspan]
bw[2, 5:tspan] <- bw[2, 5:tspan] / bw[1, 5:tspan]
bw[3, 5:tspan] <- bw[3, 5:tspan] / bw[1, 5:tspan]
bw[4, 5:tspan] <- bw[4, 5:tspan] / bw[1, 5:tspan]
bw[6, 5:tspan] <- bw[6, 5:tspan] / bw[1, 5:tspan]

# Apply the operations to bz
bz[5, 5:tspan] <- bz[5, 5:tspan] / bz[2, 5:tspan]
bz[2, 5:tspan] <- bz[2, 5:tspan] / bz[1, 5:tspan]
bz[3, 5:tspan] <- bz[3, 5:tspan] / bz[1, 5:tspan]
bz[4, 5:tspan] <- bz[4, 5:tspan] / bz[1, 5:tspan]
bz[6, 5:tspan] <- bz[6, 5:tspan] / bz[1, 5:tspan]

# Apply the operations to ew
ew[5, 5:tspan] <- ew[5, 5:tspan] / ew[2, 5:tspan]
ew[2, 5:tspan] <- ew[2, 5:tspan] / ew[1, 5:tspan]
ew[3, 5:tspan] <- ew[3, 5:tspan] / ew[1, 5:tspan]
ew[4, 5:tspan] <- ew[4, 5:tspan] / ew[1, 5:tspan]
ew[6, 5:tspan] <- ew[6, 5:tspan] / ew[1, 5:tspan]

# Apply the operations to ez
ez[5, 5:tspan] <- ez[5, 5:tspan] / ez[2, 5:tspan]
ez[2, 5:tspan] <- ez[2, 5:tspan] / ez[1, 5:tspan]
ez[3, 5:tspan] <- ez[3, 5:tspan] / ez[1, 5:tspan]
ez[4, 5:tspan] <- ez[4, 5:tspan] / ez[1, 5:tspan]
ez[6, 5:tspan] <- ez[6, 5:tspan] / ez[1, 5:tspan]

# Apply the operations to es
es[5, 5:tspan] <- es[5, 5:tspan] / es[2, 5:tspan]
es[2, 5:tspan] <- es[2, 5:tspan] / es[1, 5:tspan]
es[3, 5:tspan] <- es[3, 5:tspan] / es[1, 5:tspan]
es[4, 5:tspan] <- es[4, 5:tspan] / es[1, 5:tspan]
es[6, 5:tspan] <- es[6, 5:tspan] / es[1, 5:tspan]

# Apply the operations to bb
bb[5, 5:tspan] <- bb[5, 5:tspan] / bb[2, 5:tspan]
bb[2, 5:tspan] <- bb[2, 5:tspan] / bb[1, 5:tspan]
bb[3, 5:tspan] <- bb[3, 5:tspan] / bb[1, 5:tspan]
bb[4, 5:tspan] <- bb[4, 5:tspan] / bb[1, 5:tspan]
bb[6, 5:tspan] <- bb[6, 5:tspan] / bb[1, 5:tspan]

# Apply the operations to dcw
dcw[5, 5:tspan] <- dcw[5, 5:tspan] - dcw[2, 5:tspan]
dcw[2, 5:tspan] <- dcw[2, 5:tspan] - dcw[1, 5:tspan]
dcw[3, 5:tspan] <- dcw[3, 5:tspan] - dcw[1, 5:tspan]
dcw[4, 5:tspan] <- dcw[4, 5:tspan] - dcw[1, 5:tspan]
dcw[6, 5:tspan] <- dcw[6, 5:tspan] - dcw[1, 5:tspan]

# Apply the operations to dcz
dcz[5, 5:tspan] <- dcz[5, 5:tspan] - dcz[2, 5:tspan]
dcz[2, 5:tspan] <- dcz[2, 5:tspan] - dcz[1, 5:tspan]
dcz[3, 5:tspan] <- dcz[3, 5:tspan] - dcz[1, 5:tspan]
dcz[4, 5:tspan] <- dcz[4, 5:tspan] - dcz[1, 5:tspan]
dcz[6, 5:tspan] <- dcz[6, 5:tspan] - dcz[1, 5:tspan]

# Apply the operations to mw
mw[5, 5:tspan] <- mw[5, 5:tspan] / mw[2, 5:tspan]
mw[2, 5:tspan] <- mw[2, 5:tspan] / mw[1, 5:tspan]
mw[3, 5:tspan] <- mw[3, 5:tspan] / mw[1, 5:tspan]
mw[4, 5:tspan] <- mw[4, 5:tspan] / mw[1, 5:tspan]
mw[6, 5:tspan] <- mw[6, 5:tspan] / mw[1, 5:tspan]

# Apply the operations to mz
mz[5, 5:tspan] <- mz[5, 5:tspan] / mz[2, 5:tspan]
mz[2, 5:tspan] <- mz[2, 5:tspan] / mz[1, 5:tspan]
mz[3, 5:tspan] <- mz[3, 5:tspan] / mz[1, 5:tspan]
mz[4, 5:tspan] <- mz[4, 5:tspan] / mz[1, 5:tspan]
mz[6, 5:tspan] <- mz[6, 5:tspan] / mz[1, 5:tspan]

# Apply the operations to vh
vh[5, 5:tspan] <- vh[5, 5:tspan] / vh[2, 5:tspan]
vh[2, 5:tspan] <- vh[2, 5:tspan] / vh[1, 5:tspan]
vh[3, 5:tspan] <- vh[3, 5:tspan] / vh[1, 5:tspan]
vh[4, 5:tspan] <- vh[4, 5:tspan] / vh[1, 5:tspan]
vh[6, 5:tspan] <- vh[6, 5:tspan] / vh[1, 5:tspan]

# New variables ####

# LEVERAGE RATIO
lin[5, 5:tspan] <- lin[5, 5:tspan] - lin[2, 5:tspan]
lin[2, 5:tspan] <- lin[2, 5:tspan] - lin[1, 5:tspan]
lin[3, 5:tspan] <- lin[3, 5:tspan] - lin[1, 5:tspan]
lin[4, 5:tspan] <- lin[4, 5:tspan] - lin[1, 5:tspan]
lin[6, 5:tspan] <- lin[6, 5:tspan] - lin[1, 5:tspan]

# LOAN TO DEPOSIT RATIO
ltd[5, 5:tspan] <- ltd[5, 5:tspan] - ltd[2, 5:tspan]
ltd[2, 5:tspan] <- ltd[2, 5:tspan] - ltd[1, 5:tspan]
ltd[3, 5:tspan] <- ltd[3, 5:tspan] - ltd[1, 5:tspan]
ltd[4, 5:tspan] <- ltd[4, 5:tspan] - ltd[1, 5:tspan]
ltd[6, 5:tspan] <- ltd[6, 5:tspan] - ltd[1, 5:tspan]

# NET STABLE FUNDING RATIO
rcbf[5, 5:tspan] <- rcbf[5, 5:tspan] - rcbf[2, 5:tspan]
rcbf[2, 5:tspan] <- rcbf[2, 5:tspan] - rcbf[1, 5:tspan]
rcbf[3, 5:tspan] <- rcbf[3, 5:tspan] - rcbf[1, 5:tspan]
rcbf[4, 5:tspan] <- rcbf[4, 5:tspan] - rcbf[1, 5:tspan]
rcbf[6, 5:tspan] <- rcbf[6, 5:tspan] - rcbf[1, 5:tspan]

# Add your variables to the data frame
df$yn <- c(yn[2:6,5:tspan])  # Flatten the matrix into a vector
df$ydw_net <- c(ydw_net[2:6,5:tspan])  
df$ydz_net <- c(ydz_net[2:6,5:tspan])  
df$vw <- c(vw[2:6,5:tspan])    
df$vz <- c(vz[2:6,5:tspan])  
df$cw <- c(cw[2:6,5:tspan])    
df$cz <- c(cz[2:6,5:tspan])  
df$lw <- c(lw[2:6,5:tspan])    
df$lz <- c(lz[2:6,5:tspan]) 
df$id <- c(id[2:6,5:tspan]) 
df$da <- c(da[2:6,5:tspan]) 
df$k <- c(k[2:6,5:tspan]) 
df$pf <- c(pf[2:6,5:tspan]) 
df$tax_net <- c(tax_net[2:6,5:tspan])
df$taxw_net <- c(taxw_net[2:6,5:tspan])
df$taxz_net <- c(taxz_net[2:6,5:tspan])
df$def <- c(def[2:6,5:tspan]) 
df$seign <- c(seign[2:6,5:tspan])
df$bs <- c(bs[2:6,5:tspan])
df$bcb <- c(bcb[2:6,5:tspan])
df$hs <- c(hs[2:6,5:tspan])
df$as <- c(as[2:6,5:tspan])
df$lf <- c(lf[2:6,5:tspan])
df$ls <- c(ls[2:6,5:tspan])
df$ms <- c(ms[2:6,5:tspan])
df$pB <- c(pB[2:6,5:tspan])
df$im <- c(im[2:6,5:tspan])
df$tb <- c(tb[2:6,5:tspan])
df$cab <- c(cab[2:6,5:tspan])
df$niip <- c(niip[2:6,5:tspan])
df$bf <- c(bf[2:6,5:tspan])
df$qs <- c(qs[2:6,5:tspan])
df$wab <- c(wab[2:6,5:tspan])
df$n <- c(n[2:6,5:tspan])
df$hw <- c(hw[2:6,5:tspan])
df$hz <- c(hz[2:6,5:tspan])
df$bw <- c(bw[2:6,5:tspan])
df$bz <- c(bz[2:6,5:tspan])
df$ew <- c(ew[2:6,5:tspan])
df$ez <- c(ez[2:6,5:tspan])
df$es <- c(es[2:6,5:tspan])
df$bb <- c(bb[2:6,5:tspan])
df$dcw <- c(dcw[2:6,5:tspan])
df$dcz <- c(dcz[2:6,5:tspan])
df$mw <- c(mw[2:6,5:tspan])
df$mz <- c(mz[2:6,5:tspan])
df$vh <- c(vh[2:6,5:tspan])
df$lin <- c(lin[2:6,5:tspan])
df$ltd <- c(ltd[2:6,5:tspan])
df$rcbf <- c(rcbf[2:6,5:tspan])

# Reshape the data to long format
df_long <- df %>%
  pivot_longer(cols = c(yn,
                        ydw_net,
                        ydz_net,
                        vw,
                        vz,
                        cw,
                        cz,
                        lw,
                        lz,
                        id,
                        da,
                        k,
                        pf,
                        taxw_net,
                        taxz_net,
                        tax_net,
                        def,
                        seign,
                        bs,
                        bcb,
                        hs,
                        as,
                        lf,
                        ls,
                        ms,
                        pB,
                        im,
                        tb,
                        cab,
                        niip,
                        bf,
                        qs,
                        wab,
                        n,
                        hw,
                        hz,
                        bw,
                        bz,
                        ew,
                        ez,
                        es,
                        bb,
                        dcw,
                        dcz,
                        mw,
                        mz,
                        vh,
                        lin,
                        ltd,
                        rcbf
  ),
  names_to = "Variable",
  values_to = "Value"
  )

# Replace variable names with expressions
df_long <- df_long %>%
  mutate(Variable = case_when(
    Variable == "yn" ~ "italic(Y)",
    Variable == "ydw_net" ~ "italic(YD)[w]",
    Variable == "ydz_net" ~ "italic(YD)[z]",
    Variable == "vw" ~ "italic(V)[w]",
    Variable == "vz" ~ "italic(V)[z]",
    Variable == "cw" ~ "italic(c)[w]",
    Variable == "cz" ~ "italic(c)[z]",
    Variable == "lw" ~ "italic(L)[w]",
    Variable == "lz" ~ "italic(L)[z]",
    Variable == "id" ~ "italic(i)[d]",
    Variable == "da" ~ "italic(da)",
    Variable == "k" ~ "italic(k)",
    Variable == "pf" ~ "italic(Pi)[f]",
    Variable == "taxw_net" ~ "italic(T)[w]",
    Variable == "taxz_net" ~ "italic(T)[z]",
    Variable == "tax_net" ~ "italic(T)",
    Variable == "def" ~ "italic(DEF)",
    Variable == "seign" ~ "italic(SEIGN)",
    Variable == "bs" ~ "italic(B)[s]",
    Variable == "bcb" ~ "italic(B)[cb]",
    Variable == "hs" ~ "italic(H)[s]",
    Variable == "as" ~ "italic(A)[s]",
    Variable == "lf" ~ "italic(L)[f]",
    Variable == "ls" ~ "italic(L)[s]",
    Variable == "ms" ~ "italic(M)[s]",
    Variable == "pB" ~ "italic(Pi)[b]",
    Variable == "im" ~ "italic(im)",
    Variable == "tb" ~ "italic(TB)",
    Variable == "cab" ~ "italic(CAB)",
    Variable == "niip" ~ "italic(NIIP)",
    Variable == "bf" ~ "italic(B)[row]",
    Variable == "qs" ~ "italic(Q)[s]",
    Variable == "wab" ~ "italic(WB)",
    Variable == "n" ~ "italic(N)",
    Variable == "hw" ~ "italic(H)[w]",
    Variable == "hz" ~ "italic(H)[z]",
    Variable == "bw" ~ "italic(B)[w]",
    Variable == "bz" ~ "italic(B)[z]",
    Variable == "ew" ~ "italic(E)[w]",
    Variable == "ez" ~ "italic(E)[z]",
    Variable == "es" ~ "italic(E)[s]",
    Variable == "bb" ~ "italic(B)[b]",
    Variable == "dcw" ~ "italic(DC)[w]",
    Variable == "dcz" ~ "italic(DC)[z]",
    Variable == "mw" ~ "italic(M)[w]",
    Variable == "mz" ~ "italic(M)[z]",
    Variable == "vh" ~ "italic(V)[h]",
    Variable == "lin" ~ "italic(LIN)",
    Variable == "ltd" ~ "italic(LTD)",
    Variable == "rcbf" ~ "italic(RCBF)",
    
    TRUE ~ Variable  # Keep other variables unchanged
  ))

# Create a named vector of colors
scenario_colors <- c(
  "1. CBDC issues (with rdc=rm)" = "violetred1",    
  "2. CBDC issues (with rdc>rm)" = "dodgerblue",   
  "3. Policy rate cut (no CBDCs)" = "springgreen3",  
  "4. Policy rate cut (CBDCs)" = "gold3",
  "5. QE for the people" = "orangered",
  "Default" = "gray"      # Default color for other scenarios
)

# Plot each variable across periods and scenarios
allplots <- ggplot(df_long, aes(x = Period, y = Value, color = as.factor(Scenario))) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y", labeller = label_parsed) +  
  scale_x_continuous(
    breaks = c(7, 17, 27, 37),  # Specify the breaks (original x-axis values)
    labels = c(25, 35, 45, 55)  # Specify the corresponding labels
  ) +
  scale_color_manual(values = scenario_colors) +
    labs( #title = "Selected endogenous variables across periods and scenarios",
    x = "Year",  
    y = " ",
    color = "Scenario") +
  theme_bw()

# Show charts
print(allplots)