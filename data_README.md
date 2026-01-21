# Data Requirements

This study requires two data sources:

## 1. Stock Price Data

Daily OHLCV (Open, High, Low, Close, Volume) data for your stock universe.

### Format

CSV with columns:
```
ticker,date,open,high,low,close,volume
AAPL,2006-01-03,10.34,10.68,10.32,10.68,201808600
AAPL,2006-01-04,10.73,10.85,10.64,10.71,154896000
...
```

### Sources

- **Yahoo Finance** (free): Use `quantmod` or `tidyquant` in R
- **Alpha Vantage** (free tier): API access
- **Polygon.io** (paid): High-quality historical data
- **Norgate Data** (paid): Survivorship-bias-free S&P 500 history

### Our Study

We used 346 S&P 500 stocks with complete price history from 2006-01-01 to 2025-12-31. The 346-stock constraint exists for consistency with companion studies requiring longer history; this study's methodology works with any liquid equity universe.

## 2. Fama-French Factors

Download from [Kenneth French's Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html):

### Required Files

1. **Fama/French 5 Factors (2x3) [Daily]**
   - Download: `F-F_Research_Data_5_Factors_2x3_daily_CSV.zip`
   - Extract to: `F-F_Research_Data_5_Factors_2x3_daily.csv`

2. **Momentum Factor (Mom) [Daily]**
   - Download: `F-F_Momentum_Factor_daily_CSV.zip`
   - Extract to: `F-F_Momentum_Factor_daily.csv`

### Format

The scripts automatically parse Ken French's format (header rows, percentage values, YYYYMMDD dates).

## Notes

- **No proprietary data required**: All benchmark strategies use only OHLCV data
- **No survivorship bias adjustment needed**: Results hold regardless of universe construction
- **Reproducible with any dataset**: Substitute your own price data and run
