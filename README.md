# Time series for cybersecurity

Final project for my Survival Models and Time Series course (Actuarial Science, Faculty of Sciences, UNAM). It is a Quarto presentation, written in R, about two uses of time series in security operations: detecting unusual activity in access logs and forecasting how many alerts a team will have to review.

**[Open the slides](https://manuelrazoalc.github.io/time-series-cybersecurity/)** (in Spanish, with interactive charts).

## The question

The starting point is the literature review by [Landauer et al. (2025)](https://doi.org/10.1007/s10207-024-00921-0), which maps where time series are used in cybersecurity, which data they rely on and what limits them. The review compares many studies, but each one uses different data, so it cannot say which method works best. To compare methods under the same rules, I simulated the activity of a single company and tested four models on it.

## What I did

- **Simulated data.** 120 days of access attempts counted every 5 minutes, with a daily cycle, autocorrelated noise and a second series of alerts from an existing monitor. Everything is repeated with three random seeds.
- **Attack scenarios.** On top of the normal activity I added a short spike, a sustained increase, periodic bursts and an authorized test that produces exactly the same traffic as the spike.
- **Models.** ARIMA(2,0,1), SARIMA(1,0,1)(1,0,0), Holt-Winters additive (ETS) and Isolation Forest. The Isolation Forest is written from scratch in base R (200 trees, 256 observations per tree).
- **Fair evaluation.** 84 days to fit, 18 days to calibrate thresholds (99th percentile of calibration errors) and 18 days to test. The models never see the test labels.
- **Forecasting.** Hourly alerts forecast up to 48 hours ahead from 65 rolling origins, compared with a seasonal naive benchmark, with Ljung-Box tests on the residuals and coverage of the prediction bands.

## Main findings

- All four methods detect the sustained attack, but ARIMA flags only 21 of its 920 intervals, while Isolation Forest flags 621.
- With the same traffic, no model can tell an attack from an authorized test. That takes context the counts do not have.
- For forecasting alerts, ETS has the lowest error at 48 hours (MAE of 3.35 alerts per hour, against 4.35 for the seasonal naive). Its 90% bands only cover 84.1% of the observations, though.
- No model can anticipate an attack that leaves no earlier signal. In the first affected hour, ETS predicted 15.7 alerts and 52 arrived.

## How it is organized

- `R/analisis.R`: the whole analysis. It simulates the data, fits the models, scores detection and forecasts, runs internal consistency checks and writes everything to `datos/` and `resultados/`.
- `presentacion.qmd`: the slides. It runs `R/analisis.R` and builds the text and charts from its results.
- `presentacion.html`: the rendered slides, self-contained.
- `recursos_presentacion/`: style, title slide, fonts and the chart interactions.

## How to reproduce it

You need R and the `jsonlite` package. To run only the analysis (about 20 seconds):

```sh
Rscript R/analisis.R
```

To rebuild the slides you also need [Quarto](https://quarto.org/):

```sh
quarto render presentacion.qmd
```

The simulation uses fixed seeds, so the results are the same on every run.

## Reference

Landauer, M., Skopik, F., Stojanović, B., Flatscher, A., & Ullrich, T. (2025). A review of time-series analysis for cyber security analytics: From intrusion detection to attack prediction. *International Journal of Information Security, 24*, Article 3.

---

Manuel Razo Alcántara · [LinkedIn](https://www.linkedin.com/in/manuel-razo-alcantara) · Code under the [MIT License](LICENSE). Fonts and Plotly keep their own licenses (see `recursos_presentacion/`).
