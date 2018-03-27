# Silo - Interactive Signal Averaging App

This app accepts takes data exported from labchart and allows the user to pull cardiovascular and respiratory data points based off physiological triggers.  It then averages the data and allows the user to export the data in either csv form or in the form of a figure. 

## Getting Started
 
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

These r libraries are required

```
 c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown")
```

### Installing

Install the libraries above and load data into Silo\rawData folder <br>
  File name format: typeOfData_subjectID_cond1_cond2.txt <br>
    typeOfData: breath, beat or burst <br>
      eg. breath_07_hx_pre.txt 

### Deployment

1. Open Silo.Rproj
2. From the file explorer in RStudion, open Silo x.y.z.R 
3. Click run app in the script editor (source) pane
  * be sure to run app externally (from drop down menu next to "run")

## Authors

* **Lindsey Boulet** 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Stuckless T
* Brown CV
* Vermeulen TD
* Foster GE

# Silo
