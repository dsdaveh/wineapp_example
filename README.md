# Australian Wine Sales Analysis

🍷 **Live App:** https://dsdaveh-ads506-wines-ts-example.share.connect.posit.cloud

## Overview

Interactive Shiny application for analyzing Australian wine sales data using time series modeling and forecasting.

## Features

-   **Visualize**: Interactive time series plots with customizable date ranges and varietals
-   **Model**: Compare performance of multiple forecasting models (ETS, ARIMA, TSLM, ensemble)
-   **Forecast**: Generate 12-month forecasts with confidence intervals

## Data

Australian wine sales data by varietal and month, measuring sales in thousands of liters.

## Models

-   ETS (Error, Trend, Seasonal)
-   ARIMA (AutoRegressive Integrated Moving Average)
-   TSLM (Time Series Linear Model)
-   STL + SNAIVE baseline
-   Ensemble combination

## Usage

-   Select the training period. The forecast will be estimated for the following year

## Dependencies

Built with R using `shiny`, `fpp3`, `tidyverse`, `gt`, and related time series packages.