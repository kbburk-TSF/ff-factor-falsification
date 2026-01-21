# Factor Betas Are Path-Dependent Regression Artifacts

**Falsifying Fama-French with 6,560 Controlled Experiments**

**Preregistration**: [10.5281/zenodo.18304121](https://doi.org/10.5281/zenodo.18304121)

## Overview

This repository contains complete replication code for the paper:

> Burk, K.B. (2026). Factor Betas Are Path-Dependent Regression Artifacts: Falsifying Fama-French with 6,560 Controlled Experiments.

We demonstrate that Fama-French factor loadings fail equivalence tests at an **87% rate** when portfolios hold identical stocks but differ only in exit timing. A random entry strategy with zero information content shows factor loading shifts up to **0.457**. These results prove that factor betas measure return paths, not portfolio holdings, invalidating their use in evaluating timing strategies.

## The Core Finding

Standard academic practice dismisses trading strategies by showing their "alpha disappears" when controlling for Fama-French factors. This assumes factor loadings measure risk exposure determined by what securities you hold.

**We prove this assumption is false.**

Using controlled experiments where NATIVE and PATIENCE portfolios hold identical stocks, weights, and entry dates—differing only in exit timing—we show factor loadings change dramatically. Factor models cannot distinguish timing skill from "risk exposure" because the measurement conflates the two.

## Repository Structure

```
├── R/
│   ├── benchmark_scheduled_exposure_v3_1.R   # Calendar-based benchmark strategies
│   ├── benchmark_variable_exposure_v3_1.R    # Signal-based benchmark strategies
├── data/
│   └── README.md                              # Data sourcing instructions
├── docs/
│   └── preregistration.pdf                    # Preregistered hypotheses
└── README.md
```

## Quick Start

### Requirements

- R 4.0+
- Required packages: `data.table`, `zoo`, `openxlsx`, `sandwich`, `lmtest`

```r
install.packages(c("data.table", "zoo", "openxlsx", "sandwich", "lmtest"))
```

### Data Requirements

1. **Price Data**: Daily OHLCV for your stock universe in CSV format:
   ```
   ticker,date,open,high,low,close,volume
   AAPL,2006-01-03,10.34,10.68,10.32,10.68,201808600
   ...
   ```

2. **Fama-French Factors**: Download from [Kenneth French's Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html):
   - `F-F_Research_Data_5_Factors_2x3_daily.csv`
   - `F-F_Momentum_Factor_daily.csv`

### Running the Analysis

1. Edit the configuration section at the top of each script:
   ```r
   UNIVERSE <- "YOUR_UNIVERSE_NAME"
   INPUT_FILE <- "your_price_data.csv"
   ```

2. Run scheduled benchmark analysis:
   ```bash
   Rscript R/benchmark_scheduled_exposure_v3_1.R
   ```

3. Run variable benchmark analysis:
   ```bash
   Rscript R/benchmark_variable_exposure_v3_1.R
   ```

### Output

Each script generates:
- `*_NET.xlsx` — Performance metrics (returns, Sharpe, etc.)
- `*_COMPARISONS.xlsx` — NATIVE vs PATIENCE statistical comparisons
- `*_AGGREGATE_HYPOTHESIS_TESTS.xlsx` — Summary of all hypothesis tests
- `*_FF_HM_TM_SUMMARY.xlsx` — Factor regression outputs
- `daily_returns/*.csv` — Daily return series for each strategy

## Methodology

### Experimental Design

We construct paired portfolios:

- **NATIVE**: Enter per benchmark signal → Exit per native benchmark rule
- **PATIENCE**: Enter per identical signal → Exit per graduated profit-taking

**Patience Gradient Exit Rule:**

| Holding Period | Exit Target |
|----------------|-------------|
| 0-59 days      | 15% profit  |
| 60-89 days     | 10% profit  |
| 90-119 days    | 5% profit   |
| 120-179 days   | 2% profit   |
| 180+ days      | Forced exit |

### Benchmark Strategies

**Scheduled (Calendar-Based):**
- sell_in_may, january, turn_of_month, sept_avoid, end_of_quarter, weekend, friday
- random_21d, random_63d, random_126d (fixed-hold random controls)

**Variable (Signal-Based):**
- bollinger, rsi, ma_cross, macd, donchian, roc
- dip_5, dip_10, sd_2, sd_3, down_3, down_5
- 52w_low, 52w_high, below_ma, above_ma
- random_tsf, random_uniform (random signal controls)

### Statistical Tests

- **TOST Equivalence Testing**: ±0.10 bounds for factor loadings
- **HAC Standard Errors**: Newey-West correction for autocorrelation
- **Benjamini-Hochberg**: FDR correction across all comparisons

### Hypotheses Tested

**Control Hypotheses (H_FF4-H_FF8)**: Factor loadings should be equivalent between NATIVE and PATIENCE when holdings are identical.

- H_FF4: SMB loading equivalence
- H_FF5: HML loading equivalence
- H_FF6: RMW loading equivalence
- H_FF7: CMA loading equivalence
- H_FF8: MOM loading equivalence

**Result**: 87% failure rate across 6,560 tests.

## Reproducing Our Results

To reproduce the exact results from the paper:

1. Use S&P 500 constituents with 20+ years of price history (346 stocks as of 2006-2025)
2. Download FF factors from Ken French's library
3. Run both scripts for each universe:
   - DEFENSIVE (Utilities, Consumer Staples, Healthcare)
   - AGGRESSIVE (Technology, Consumer Discretionary, Financials)
   - FULL (all 346 stocks)
   - COMMUNICATION_SERVICES (10-stock control)

## Adapting to Your Data

The scripts work with any price dataset. Modify the configuration:

```r
# Your universe name (used for output filenames)
UNIVERSE <- "MY_STOCKS"

# Your price data file
INPUT_FILE <- "my_price_data.csv"

# Adjust position sizing if desired
POSITION_SIZES <- list(
  list(name = "p05", pct = 0.05),
  list(name = "p10", pct = 0.10)
)

# Adjust time periods
PERIODS <- list(
  list(name = "2010-2020", start = as.Date("2010-01-01"), end = as.Date("2020-12-31"))
)
```

## Citation

If you use this code or methodology, please cite:

```bibtex
@article{burk2026factor,
  title={Factor Betas Are Path-Dependent Regression Artifacts: Falsifying Fama-French with 6,560 Controlled Experiments},
  author={Burk, Kevin B.},
  year={2026}
}
```

## License

MIT License. See [LICENSE](LICENSE) for details.

## Contact

Kevin B. Burk  
ORCID: [0009-0005-5343-7913](https://orcid.org/0009-0005-5343-7913)

---

## Why This Matters

Every quant who has had a strategy dismissed with "your alpha is just factor exposure" now has a peer-citable response:

> Factor loadings are path-dependent regression artifacts that change based on exit timing, not holdings. Burk (2026) demonstrates this with 6,560 controlled experiments showing 87% equivalence test failure rates for identical portfolios differing only in exit timing.

The standard academic toolkit for dismissing timing strategies is broken. This repository provides the evidence.
