# server load

A simple tool for displaying the server load on LUX

# Method

* Andre Noll has a CRON job that write the server load every minute to /ebio/abt3_projects/server-load
* Nick Youngblut has a CRON job that logs this information (+ time stamp) to /tmp/SERVER-LOAD-LOG.csv
* This shiny app reads the log file and displays the results