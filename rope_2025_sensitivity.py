# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sensitivity analysis of the SFC model with non-bank financial
# intermediaries (Economic Function 2, or EF2), workers and rentiers
#
# Companion code to:
#   Canelli, R., Fontana, G., Realfonzo, R. and Veronese Passarella, M. (2026).
#   "Keynes, Graziani, and Non-Bank Financial Intermediaries: A Stock-Flow
#   Consistent Analysis." Review of Political Economy. Open access.
#   DOI: 10.1080/09538259.2025.2601163
#
# Author:  Marco Veronese Passarella
# Version: 3 October 2026
#
# What this script does.
#   It contains a vectorised Python port of the corrected model in
#   rope_2025_web.R, which simulates many parameter configurations at once.
#   It draws 4,000 random configurations, runs the three scenarios for each
#   of them, and reports how often Scenario 3 (EF2 replaces banks in lending
#   to workers) raises or lowers output, employment, income inequality and
#   wealth inequality relative to the baseline scenario. The results are
#   summarised in the file README_EF2_model.md of this repository.
#
# Licence.
#   This code is released under the Creative Commons Attribution-NonCommercial
#   4.0 International licence (CC BY-NC 4.0), as stated in the LICENSE file of
#   this repository. If you use or adapt the code, please cite the paper above.
#
# How to use.
#   Run the whole script with Python 3 (the only requirement is numpy). The
#   summary is printed in the console. The run takes a few minutes. The seed
#   is fixed, so the figures reported in README_EF2_model.md are reproduced
#   exactly.
#
# Notation.
#   Variable and parameter names are the same as in rope_2025_web.R, where
#   they are documented. The interest rates are defined here as spreads over
#   the policy rate (s_rl, s_rb, s_re, s_rq and s_rlq).
#
# Sampling ranges (uniform draws).
#   alpha10l 0.40 to 0.70, alpha11l 0 to 0.40, alpha12l 0 to 0.10,
#   alpha2l 0.2 to 0.5, alpha1u 0.30 to 0.65, alpha2u 0.2 to 0.5,
#   thetal 0.20 to 0.40, thetau 0.15 to 0.40, thetal_v and thetau_v 0 to 0.15,
#   gamma 0.05 to 0.25, kappa 0.9 to 1.6, omega 0 to 0.8, betal 0.05 to 0.20,
#   rhol0 0.03 to 0.08, rhol1 0 to 0.10, g_rlq 0 to 0.006,
#   s_rlq 0.05 to 0.30, s_rq 30% to 95% of s_rlq, lambda40u 0 to 0.2,
#   w 0.5 to 0.75, sigmaq after the shock 0.5 to 1.
#   All other parameters are kept at the values used in rope_2025_web.R.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PREPARE THE WORKSPACE ####
import numpy as np

# DEFINE THE MODEL ####
# Parameter values used in rope_2025_web.R
DEFAULT = dict(
    lambda10l=0.1, lambda11l=0.2, lambda12l=-0.1, lambda13l=-0.1, lambda15l=-0.05,
    lambda30l=0.0, lambda31l=-0.025, lambda32l=-0.025, lambda33l=0.05, lambda35l=-0.005,
    lambda10u=0.2, lambda11u=0.3, lambda12u=-0.1, lambda13u=-0.1, lambda14u=-0.1, lambda15u=-0.05,
    lambda30u=0.2, lambda31u=-0.1, lambda32u=-0.1, lambda33u=0.3, lambda34u=-0.1, lambda35u=-0.05,
    lambda40u=0.0001, lambda41u=0.0, lambda42u=0.0, lambda43u=0.0, lambda44u=0.0, lambda45u=0.0,
    lambda_cl=0.3, lambda_cu=0.2,
    r_t=0.01, alpha10l=0.55, alpha11l=0.2, alpha12l=0.025, alpha2l=0.4, alpha1u=0.6, alpha2u=0.4,
    thetal=0.3, thetau=0.3, thetal_v=0.10, thetau_v=0.10,
    delta=0.1, gamma=0.15, kappa=1.2, omega=0.5,
    betal=0.1, betau=0.1, rhol0=0.05, rhol1=0.025, rhou=0.05, g_rlq=0.002,
    s_rl=0.03, s_rb=0.05, s_re=0.05, s_rq=0.16, s_rlq=0.20,   # Spreads over r_t
    g=20.0, w=0.6, pr=1.0,
    sigmab_shock=0.0,   # Value of sigmab after the shock (scenarios 2 and 3)
    sigmaq_shock=1.0,   # Value of sigmaq after the shock (scenario 3)
    bb_sign=-1.0,       # Sign of qb in the balance sheet of banks (-1 corrected, +1 as published)
    ehl_pub=0.0,        # Set to 1 to use lambda13l in ehl, as in the published code
)

# Variables kept for reference
VARS = ["y", "n", "TB_yd", "TB_v", "fl", "ll", "ll_t", "hh", "hs", "ydl", "ydu", "consl", "consu",
        "vl", "vu", "Pb", "Pq", "Pf", "alpha1l", "rlq", "rhol", "qb", "qhu", "bb", "bcb", "id", "cons",
        "tl", "tu", "mhl", "lu", "wb"]


def simulate(params, scen, T=180, iters=200, shock=100):
    """params: dict name -> array (M,) or scalar
    scen: int array (M,) in {1,2,3}."""
    scen = np.asarray(scen)
    M = scen.shape[0]
    p = {k: np.broadcast_to(np.asarray(params.get(k, v), dtype=float), (M,)).copy() for k, v in DEFAULT.items()}
    g_ = p
    rm = g_["r_t"]
    rl = g_["r_t"] + g_["s_rl"]
    rb = g_["r_t"] + g_["s_rb"]
    re = g_["r_t"] + g_["s_re"]
    rq = g_["r_t"] + g_["s_rq"]
    rlq0 = g_["r_t"] + g_["s_rlq"]
    names = ["y", "cons", "consl", "consu", "id", "k", "kt", "da", "af", "yd", "ydl", "ydu", "t", "tl", "tu",
             "v", "vl", "vu", "wb", "n", "Pf", "Pb", "Pq", "hh", "hhl", "hhu", "hs", "mh", "mhl", "mhu", "ms",
             "bs", "bh", "bhl", "bhu", "bb", "bcb", "es", "eh", "ehl", "ehu", "lf", "ls", "ll_t", "ll", "lu",
             "fl", "qs", "qhu", "qb", "TB_yd", "TB_v"]
    S = {nm: np.zeros((M, T)) for nm in names}
    S["alpha1l"] = np.tile((g_["alpha10l"] + g_["alpha11l"])[:, None], (1, T))
    S["rlq"] = np.tile(rlq0[:, None], (1, T))
    S["rhol"] = np.tile(g_["rhol0"][:, None], (1, T))
    sigmab = np.ones((M, T))
    sigmaq = np.zeros((M, T))
    sigmab[:, shock:] = np.where(scen >= 2, g_["sigmab_shock"], 1.0)[:, None]
    sigmaq[:, shock:] = np.where(scen == 3, g_["sigmaq_shock"], 0.0)[:, None]
    L = lambda nm: g_[nm]
    for i in range(1, T):
        P = {nm: S[nm][:, i - 1] for nm in S}          # Previous period
        C = {nm: S[nm][:, i].copy() for nm in S}       # Current period (working copy)
        sb = sigmab[:, i]
        sq = sigmaq[:, i]
        with np.errstate(divide="ignore", invalid="ignore"):
            for _ in range(iters):
                C["y"] = C["cons"] + L("g") + C["id"]
                C["kt"] = L("kappa") * P["y"]
                C["da"] = L("delta") * P["k"]
                C["af"] = C["da"]
                C["id"] = L("gamma") * (C["kt"] - P["k"]) + C["da"]
                C["k"] = P["k"] + C["id"] - C["da"]
                C["es"] = C["eh"]
                C["lf"] = P["lf"] + C["id"] - (C["es"] - P["es"]) - C["af"]
                C["ydl"] = (C["wb"] - C["tl"] + re * P["ehl"] + rb * P["bhl"] + rm * P["mhl"]
                            - rl * P["ll"] - P["rlq"] * P["fl"])
                C["ydu"] = (C["Pf"] + C["Pb"] + C["Pq"] - C["tu"] + re * P["ehu"] + rq * P["qhu"]
                            + rb * P["bhu"] + rm * P["mhu"] - rl * P["lu"])
                C["yd"] = C["ydl"] + C["ydu"]
                C["consl"] = C["alpha1l"] * C["ydl"] + L("alpha2l") * P["vl"]
                if i > 1:
                    C["alpha1l"] = (L("alpha10l") + L("alpha11l") * (P["ll"] + P["fl"]) / P["ll_t"]
                                    - L("alpha12l") * (P["ll"] + P["fl"]) / P["ydl"])
                C["consu"] = L("alpha1u") * C["ydu"] + L("alpha2u") * P["vu"]
                C["cons"] = C["consl"] + C["consu"]
                C["tl"] = (L("thetal") * (C["wb"] + re * P["ehl"] + rb * P["bhl"] + rm * P["mhl"]
                                          - rl * P["ll"] - P["rlq"] * P["fl"]) + L("thetal_v") * P["vl"])
                C["tu"] = (L("thetau") * (C["Pf"] + C["Pb"] + C["Pq"] + re * P["ehu"] + rq * P["qhu"]
                                          + rb * P["bhu"] + rm * P["mhu"] - rl * P["lu"])
                           + L("thetau_v") * P["vu"])
                C["vl"] = P["vl"] + (C["ydl"] - C["consl"])
                C["vu"] = P["vu"] + (C["ydu"] - C["consu"])
                C["v"] = C["vu"] + C["vl"]
                C["ll_t"] = P["ll"] + L("betal") * C["ydl"] - C["rhol"] * P["ll"]
                C["hhl"] = L("lambda_cl") * P["consl"]
                C["bhl"] = C["vl"] * (L("lambda10l") + L("lambda11l") * rb - L("lambda12l") * rm
                                      - L("lambda13l") * re - L("lambda15l") * (C["ydl"] / C["vl"]))
                lam_e = np.where(L("ehl_pub") > 0.5, L("lambda13l"), L("lambda33l"))
                C["ehl"] = C["vl"] * (L("lambda30l") - L("lambda31l") * rb - L("lambda32l") * rm
                                      + lam_e * re - L("lambda35l") * (C["ydl"] / C["vl"]))
                C["mhl"] = C["vl"] + C["ll"] + C["fl"] - C["bhl"] - C["ehl"] - C["hhl"]
                C["lu"] = P["lu"] + L("betau") * C["ydu"] - L("rhou") * P["lu"]
                C["hhu"] = L("lambda_cu") * P["consu"]
                C["bhu"] = C["vu"] * (L("lambda10u") + L("lambda11u") * rb - L("lambda12u") * rm
                                      - L("lambda13u") * re - L("lambda14u") * rq
                                      - L("lambda15u") * (C["ydu"] / C["vu"]))
                C["ehu"] = C["vu"] * (L("lambda30u") - L("lambda31u") * rb - L("lambda32u") * rm
                                      + L("lambda33u") * re - L("lambda34u") * rq
                                      - L("lambda35u") * (C["ydu"] / C["vu"]))
                C["qhu"] = np.minimum(C["qs"], C["vu"] * (L("lambda40u") - L("lambda41u") * rb
                                      - L("lambda42u") * rm - L("lambda43u") * re + L("lambda44u") * rq
                                      - L("lambda45u") * (C["ydu"] / C["vu"])))
                C["mhu"] = C["vu"] + C["lu"] - C["bhu"] - C["ehu"] - C["qhu"] - C["hhu"]
                C["hh"] = C["hhl"] + C["hhu"]
                C["bh"] = C["bhl"] + C["bhu"]
                C["mh"] = C["mhl"] + C["mhu"]
                C["eh"] = C["ehl"] + C["ehu"]
                C["ll"] = P["ll"] + sb * (C["ll_t"] - P["ll"])
                C["ls"] = P["ls"] + (C["lf"] - P["lf"]) + (C["ll"] - P["ll"]) + (C["lu"] - P["lu"])
                C["ms"] = C["mh"]
                C["bb"] = C["ms"] + L("bb_sign") * C["qb"] - C["ls"]
                C["Pb"] = rl * P["ls"] + rq * P["qb"] + rb * P["bb"] - rm * P["ms"]
                C["fl"] = sq * (C["ll_t"] - C["ll"])
                C["qs"] = C["fl"]
                C["qb"] = C["qs"] - C["qhu"]
                C["Pq"] = P["rlq"] * P["fl"] - rq * P["qs"]
                pos = C["fl"] > 0
                C["rlq"] = np.where(pos, P["rlq"] * (1 + L("g_rlq")), C["rlq"])
                C["rhol"] = np.where(pos, L("rhol0") - L("rhol1") * P["rlq"], C["rhol"])
                C["t"] = C["tl"] + C["tu"]
                C["bs"] = P["bs"] + (L("g") + rb * P["bs"]) - (C["t"] + rb * P["bcb"])
                C["bcb"] = C["bs"] - C["bh"] - C["bb"]
                C["hs"] = P["hs"] + C["bcb"] - P["bcb"]
                C["wb"] = L("w") * C["n"]
                C["n"] = L("omega") * P["n"] + (1 - L("omega")) * (C["y"] / L("pr"))
                C["Pf"] = C["y"] - C["wb"] - C["af"] - rl * P["lf"] - re * P["eh"]
                C["TB_yd"] = C["ydu"] / C["ydl"]
                C["TB_v"] = C["vu"] / C["vl"]
        for nm in S:
            S[nm][:, i] = C[nm]
    return S



# RUN THE SENSITIVITY ANALYSIS ####
# Number of random configurations
N = 4000

# Fix the seed, so that the results can be reproduced exactly
rng = np.random.default_rng(20261003)


def U(a, b):
    """Draw N values from a uniform distribution between a and b."""
    return rng.uniform(a, b, N)


# Draw the parameters (the order of the draws must not be changed)
P = dict(
    alpha10l=U(0.40, 0.70), alpha11l=U(0.0, 0.40), alpha12l=U(0.0, 0.10),
    alpha2l=U(0.2, 0.5), alpha1u=U(0.30, 0.65), alpha2u=U(0.2, 0.5),
    thetal=U(0.20, 0.40), thetau=U(0.15, 0.40), thetal_v=U(0, 0.15), thetau_v=U(0, 0.15),
    gamma=U(0.05, 0.25), kappa=U(0.9, 1.6), omega=U(0, 0.8),
    betal=U(0.05, 0.20), rhol0=U(0.03, 0.08), rhol1=U(0, 0.10), g_rlq=U(0, 0.006),
    s_rlq=U(0.05, 0.30), lambda40u=U(0, 0.2), w=U(0.5, 0.75),
    sigmaq_shock=U(0.5, 1.0),
)

# Return rate on EF2 securities, as a share of the interest rate on EF2 loans
P["s_rq"] = P["s_rlq"] * U(0.3, 0.95)

# Run the three scenarios for each configuration
print("Running", N, "configurations under three scenarios (a few minutes)...", flush=True)
big = {k: np.tile(v, 3) for k, v in P.items()}
with np.errstate(over="ignore", invalid="ignore"):
    S = simulate(big, np.repeat([1, 2, 3], N))
S = {k: v.reshape(3, N, -1) for k, v in S.items()}


def dev(name, scenario):
    """Difference from the baseline scenario, multiplied by 100."""
    return 100 * (S[name][scenario - 1] - S[name][0])


# COMPUTE THE INDICATORS ####
with np.errstate(over="ignore", invalid="ignore"):
    # Short-run effect on output and employment (mean of periods 102 to 106)
    y_sr = dev("y", 3)[:, 101:106].mean(axis=1)
    n_sr = dev("n", 3)[:, 101:106].mean(axis=1)
    y_sr_s2 = dev("y", 2)[:, 101:106].mean(axis=1)

    # Effect on the inequality indices in periods 110 and 130
    tbyd_110 = dev("TB_yd", 3)[:, 109]
    tbyd_130 = dev("TB_yd", 3)[:, 129]
    tbv_110 = dev("TB_v", 3)[:, 109]
    tbv_130 = dev("TB_v", 3)[:, 129]

    # Largest gap of the redundant equation (accounting consistency)
    err = np.abs(S["hh"] - S["hs"]).max(axis=(0, 2))

    # Drift of baseline output between periods 90 and 100 (stationarity)
    drift = np.abs(S["y"][0][:, 99] - S["y"][0][:, 89])

    # Baseline propensity to consume out of income of workers in period 100
    a1l = S["alpha1l"][0][:, 99]

    # Loans granted by EF2 in Scenario 3 in period 105
    fl3 = S["fl"][2][:, 104]

    # Configurations with finite and positive baseline values
    ok = (np.isfinite(S["y"]).all(axis=(0, 2)) & (S["y"][0][:, 99] > 0) & (S["vl"][0][:, 99] > 0)
          & (S["vu"][0][:, 99] > 0) & (S["ydl"][0][:, 99] > 0))

    # Usable configurations: stable and consistent baseline, positive EF2 loans
    usable = ok & (drift < 0.01) & (err < 1e-3) & np.isfinite(y_sr) & (fl3 > 0.01)

    # Original pattern: output and employment up, both inequality indices up
    boom = (y_sr > 0) & (n_sr > 0)
    ineq = (tbyd_110 > 0) & (tbv_110 > 0) & (tbyd_130 > 0) & (tbv_130 > 0)
    original = usable & boom & ineq

    # Workers consume a larger share of income than rentiers
    mpc = a1l > P["alpha1u"]

    # Rentiers are not taxed more lightly than workers
    tax = P["thetau"] >= P["thetal"]

# PRINT THE SUMMARY ####


def share(cond, base):
    """Percentage of the configurations in base that meet cond."""
    return 100 * (cond & base).sum() / base.sum()


print("")
print("Configurations drawn:", N)
print("Usable configurations:", usable.sum())
print("")
print("Scenario 3 against the baseline (share of usable configurations)")
print("  Output falls in the short run:        %5.1f%%" % share(y_sr < 0, usable))
print("  Employment falls in the short run:    %5.1f%%" % share(n_sr < 0, usable))
print("  Income inequality rises (period 110): %5.1f%%" % share(tbyd_110 > 0, usable))
print("  Income inequality rises (period 130): %5.1f%%" % share(tbyd_130 > 0, usable))
print("  Wealth inequality rises (period 110): %5.1f%%" % share(tbv_110 > 0, usable))
print("  Wealth inequality rises (period 130): %5.1f%%" % share(tbv_130 > 0, usable))
print("  Output lower than in Scenario 2:      %5.1f%%" % share(y_sr < y_sr_s2, usable))
print("")
print("Original pattern (output, employment and both inequality indices up)")
print("  Cases:", original.sum(), "(%.1f%% of usable configurations)" % share(original, usable))
print("  of which workers' propensity above rentiers':", (original & mpc).sum())
print("  of which rentiers not taxed more lightly:", (original & tax).sum())
print("  of which both conditions hold:", (original & mpc & tax).sum())
print("")
both = usable & mpc & tax
print("Configurations where both conditions hold:", both.sum())
print("  Output falls in the short run:        %5.1f%%" % share(y_sr < 0, both))
print("  Income inequality rises (period 110): %5.1f%%" % share(tbyd_110 > 0, both))
print("  Wealth inequality rises (period 110): %5.1f%%" % share(tbv_110 > 0, both))
