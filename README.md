# SNQ Distribution Hub 
Shiny application for processing and visualizing Child Social Network Questionnaire (CSNQ) data.: https://csnq.shinyapps.io/csnq-dashboard/#home (deployed using app.R)


## Features
- Inquiring Questionnaire: you will fill out request access form on our SNQ distribution hub page to request qsf for the Child Social Network Questionnaire 
- Process Data : downloadsnq_pipeline.rmd to process the qualtrics csv data into analysis-ready data or upload and run the SNQ pipeline to generate `ego_level_network_summary.csv` and `node_level_long.csv`(admin-only).
- Plotting Dashboard: “Download dashboarduser.html” to run the network mapping tool locally or useembedded HTML dashboard (admin-only) To test this dashboard, use `ego_level_network_summary.csv` and `node_level_long.csv`
  

## Using the mapping dashboard offline

- In the app Welcome page, click “Download This Dashboard”.
- Open the downloaded `SNQ_Dashboard_*.html` file directly in a browser.
- Place your processed CSVs in the same folder as the HTML and use the upload tiles.


## Admin access note

Upload tiles in the dashboard are locked behind the Woodward Lab admin password. Non-admin users should use the “Download This Dashboard” option.

## License

For research use. Please cite Woodward Lab CSNQ Pipeline where appropriate.
