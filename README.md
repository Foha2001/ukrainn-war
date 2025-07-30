# Return and Volatility Spillovers Among Global Assets: Comparing Health Crisis with Geopolitical Crisis

This repository contains code, data, and analysis related to the study:

**"Return and volatility spillovers among global assets: Comparing health crisis with geopolitical crisis"**  
by Muhammad Abubakr Naeem et al., published in *International Review of Economics and Finance* (2023).  
DOI: [10.1016/j.iref.2023.06.008](https://doi.org/10.1016/j.iref.2023.06.008)

## Overview

This study investigates the dynamic return and volatility spillovers across global equity, cryptocurrency, and commodity markets during two major crises: the Covid-19 pandemic and the Russia-Ukraine war. Using advanced time-varying parameter VAR (TVP-VAR) and DCC-GARCH models, it quantifies how shocks are transmitted among markets from 2010 to 2023.

Key contributions include:  
- Comparison of spillover effects during health vs geopolitical crises  
- Analysis of market interconnectedness across regions (Americas, Europe, Asia) and asset types  
- Frequency decomposition of spillovers to understand short-term vs long-term transmission channels  
- Insights on the role of cryptocurrency markets in global risk transmission


## Data Description

- Daily returns and volatilities from 2010 to 2023  
- Markets covered include:  
  - Equity markets of G20 countries (US, Canada, China, France, Germany, India, Japan, Mexico, Russia, South Africa, UK)  
  - Commodity markets (Gold, Crude Oil, Aluminum, Sugar, Wheat, Soybean, Corn, Natural Gas)  
  - Cryptocurrencies (Bitcoin, Ethereum, Tether)  
- Data sources: Thomson Reuters DataStream, Yahoo Finance

## Methodology

- **TVP-VAR Extended Joint Connectedness Approach**: Measures time-varying return and volatility spillovers without requiring arbitrary rolling windows.  
- **DCC-GARCH Model**: Captures dynamic conditional correlations to validate spillover findings.  
- **Frequency Domain Analysis**: Decomposes spillovers into short-, medium-, and long-term components to differentiate transmission channels during crises.

Detailed model equations and explanations are provided in the paper.


## Key Findings

- Spillovers surged significantly during Covid-19 and Russia-Ukraine war periods.  
- American and European markets act as primary risk transmitters; Asian and commodity markets are net receivers.  
- Commodity markets, especially wheat, were heavily impacted by both crises.  
- Cryptocurrencies played a minor yet increasing role in risk transmission.  
- Health crises mainly transmit risks via low-frequency (long-term) components; geopolitical crises predominantly via high-frequency (short-term) components.

---

## Citation

Please cite the original article if you use this repository for your research:

Muhammad Abubakr Naeem, Foued Hamouda, Sitara Karim, Samuel A. Vigne (2023). Return and volatility spillovers among global assets: Comparing health crisis with geopolitical crisis. International Review of Economics and Finance. https://doi.org/10.1016/j.iref.2023.06.008


## Contact


For questions or collaborations: • Foued Hamouda – foha2001@gmail.com • Paper [DOI](https://www.sciencedirect.com/science/article/abs/pii/S1059056023001764?via%3Dihub)




