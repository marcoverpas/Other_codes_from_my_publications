# Keynes, Graziani, and Non-Bank Financial Intermediaries: replication code

R code for the stock-flow consistent (SFC) model presented in:

> Canelli, R., Fontana, G., Realfonzo, R. and Veronese Passarella, M. (2026). "Keynes, Graziani, and Non-Bank Financial Intermediaries: A Stock-Flow Consistent Analysis." *Review of Political Economy*. Open access. DOI: [10.1080/09538259.2025.2601163](https://doi.org/10.1080/09538259.2025.2601163)

**Important.** The code in this repository corrects two errors found after publication. The figure it produces therefore differs from Figure 4 of the published article. The corrections and their implications are documented below.

## Contents

| File | Description |
|---|---|
| `rope_2025_web.R` | Builds and simulates the model under three scenarios, checks its accounting consistency, and draws the corrected version of Figure 4. |

## How to run

Open `rope_2025_web.R` in R or RStudio and run the whole script. The consistency statement is printed in the console and the figures are drawn on screen.

Required packages: `ggplot2` and `patchwork` (with `ggplot2` 4.0.0 or later, `patchwork` 1.3.1 or later is needed). Optional packages: `progress` (progress bar) and `beepr` (notification sound).

## Scenarios

1. **Baseline.** Banks fully accommodate the loan demand of workers.
2. **Credit exclusion.** From period 101, neither banks nor EF2 grant new loans to workers.
3. **EF2 replaces banks.** From period 101, banks grant no new loans to workers and EF2 fully covers the unmet demand.

## Corrections with respect to the published article

### 1. Balance sheet of commercial banks (equation 34)

The published article reports the government bonds held by banks as

```
Bb = Ms + Zb - Ls        (published)
```

Banks hold EF2 securities (`Zb`) as an asset, alongside loans and government bonds, while deposits are their only liability. The balance-sheet identity of banks therefore requires

```
Bb = Ms - Zb - Ls        (corrected)
```

The same sign error was present in the code used for the published simulations. In the script, the corrected line is

```r
bb[j, i] = ms[j, i] - qb[j, i] - ls[j, i]
```

The error did not affect the baseline or Scenario 2, where EF2 securities are zero. In Scenario 3 it created an accounting leak: the redundant equation (cash supplied equal to cash held) was violated by about 0.5 from period 101 onwards. With the correction, the redundant equation holds in all three scenarios, with a maximum gap below 0.00004.

### 2. Demand of workers for securities issued by firms (code only)

In the code used for the published simulations, the coefficient on the return rate on securities of firms in the demand of workers for those securities was `lambda13l` instead of `lambda33l`. Equation (9) of the article is correct. The code has been aligned with it. The numerical effect of this correction is negligible.

## Implications for the results

The corrected version of Figure 4, as drawn by `rope_2025_web.R`, is shown below. The solid line is Scenario 2 and the dashed line is Scenario 3.

![Corrected Figure 4: output, employment, income inequality and wealth inequality in Scenarios 2 and 3, as differences from the baseline](rope_2025_figure4_corrected.png)

The table compares Scenario 3 before and after the corrections. As in Figure 4, each value is the difference from the baseline scenario multiplied by 100.

| Scenario 3 | Period | Published | Corrected |
|---|---|---|---|
| Output | 105 | 4.31 | -1.68 |
| | 110 | 7.02 | -0.33 |
| | 130 | 5.38 | -0.27 |
| Employment | 105 | 3.16 | -1.70 |
| | 110 | 6.70 | -0.55 |
| | 130 | 5.36 | -0.27 |
| Income inequality | 105 | 0.34 | 0.20 |
| | 110 | 0.31 | 0.21 |
| | 130 | 0.29 | 0.21 |
| Wealth inequality | 105 | 0.34 | 0.17 |
| | 110 | 0.38 | 0.22 |
| | 130 | 0.36 | 0.24 |

**What still holds.** When EF2 replaces banks in lending to workers, both income inequality and wealth inequality rise relative to the baseline. The increase is smaller than in the published figure (roughly two thirds of it), but the sign and the persistence of the effect are unchanged. The results for Scenario 2 (credit exclusion of workers) are also unchanged.

**What does not hold.** The published figure shows a marked expansion of output and employment in Scenario 3. That expansion was produced by the accounting leak described above. In the corrected model, output and employment fall slightly below the baseline after the shock (the trough is about -2.1 for output) and then remain marginally below it. The statements in Section 5 of the article that the increased lending activity of EF2 leads to a significant expansion of output and employment, and that it may stimulate short-term growth, are therefore not supported by the corrected simulations.

**Overall.** The main distributive conclusion of the article, namely that the expansion of EF2 lending tends to raise income and wealth inequality, is confirmed. The corrected model suggests that this happens without any gain in output and employment.

## Licence

The code is released under the Creative Commons Attribution-NonCommercial 4.0 International licence (CC BY-NC 4.0), as stated in the `LICENSE` file of this repository. The article is open access under a Creative Commons Attribution-NonCommercial-NoDerivatives (CC BY-NC-ND 4.0) licence. If you use or adapt the code, please cite the article above.

## Contact

Marco Veronese Passarella
