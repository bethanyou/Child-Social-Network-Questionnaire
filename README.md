# SNQ Distribution Hub

Shiny application for processing and visualizing Child Social Network Questionnaire (CSNQ/SNQ) data.

## Features

- Process Data (admin-only): upload and run the SNQ pipeline to generate `ego_level_network_summary.csv` and `node_level_long.csv`.
- Plotting Dashboard: embedded HTML dashboard to explore networks by child, component, language, race, etc.
- Offline Use: one-click “Download Dashboard HTML” to run the plotting tool locally in a browser without Shiny.

## Run locally

```r
install.packages(c(
  'shiny','bslib','DT','jsonlite','readr','readxl','writexl',
  'ggplot2','stringr','shinyjs','dplyr','tidyr','purrr','lubridate'
))
shiny::runApp('.')
```

Admin password: set env var `SNQ_ADMIN_PASSWORD`; default is `woodwardlab` (for development only).

## Deployment (shinyapps.io)

```r
rsconnect::setAccountInfo(name = '<account>', token = '<token>', secret = '<secret>')
rsconnect::deployApp(appDir = '.', appName = 'snq-distribution-hub')
```

For Shiny Server, copy the repo to `/srv/shiny-server/<appname>/` and restart the service.

## Using the dashboard offline

- In the app Welcome page, click “Download This Dashboard”.
- Open the downloaded `SNQ_Dashboard_*.html` file directly in a browser.
- Place your processed CSVs in the same folder as the HTML and use the upload tiles.

## Project layout

- `app.R` — Shiny app
- `www/snq_dash/dashboard.html` — standalone plotting dashboard
- `snq_pipeline.rmd` / `snq_pipeline_extracted.R` — data pipeline
- `www/` — static assets
- `data/` — runtime storage (uploads, logs); usually empty in deployment

## Admin access note

Upload tiles in the dashboard are locked behind the Woodward Lab admin password. Non-admin users should use the “Download This Dashboard” option.

## License

For research use. Please cite CSNQ where appropriate.
