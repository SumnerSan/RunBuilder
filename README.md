# *RunBuilder*
*This is a rShiny app that takes a user upload of a table of dates and numbers and produces a downloadable runchart using HIS specifications. These specifications are detailed at http://www.healthcareimprovementscotland.org/previous_resources/implementation_support/guide_to_using_run_charts.aspx*

### Directories
  * `functions` - contains one function "stable" which does the wrangling.

### Code scripts
  * `ui` - user interface: Users can upload data, annotate the chart, adjust sensitivity of rebasing and download the chart using the interface.
  * `server` - code that produces the outputs shown in the ui.
  * `global` - non-reactive elements used by both server and ui: packages, functions. 
  * `rsessioninfo` - R version 3.5.1 (2018-07-02) -- "Feather Spray".

### How to use
  * This app creates charts from user uploaded data - there is none contained within. This is currently configured to accept an Excel file of two columns: "date" and "value".
  
