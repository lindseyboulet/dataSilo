# silo - Interactive Signal Averaging App

This app accepts takes data exported from labchart and allows the user to pull cardiovascular and respiratory data points based off physiological triggers.  It then averages the data and allows the user to export the data in either csv form or in the form of a figure. 

## Getting Started
 
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

R and RStudio installed
Required ibraries download automatically


### File Extraction from LabChart

#### Carviovascular and Respiratory Files

1. Set time mode to: **"Show time from start of file"**
    - check **"Show time as seconds"**
2. Arrange **Data Pad** columns to include channels of interest
    - Be sure to include a column for **Time**
3. Setting **Event Markers**<br>
    **Respiratory**
      - **Tidal Volume** channel is calculated from the integrated **Flow** channel
      - **Breathing Freqency** channel calculated as cyclic measurements from **Tidal Volume**
          - under cyclic measurements check **Event Markers**<br>
    **Cardiovascular**
      - Add **Event Markers** to trigger channel of choice (ECG, BP)
4. Select **Multiple Add to Data Pad**
      - Find using: **Event Marker**
      - From channel: **Volume** for respiratory file; **BP or ECG** for cardiovascular file
      - Select: **event marker**
      - Step through: **Whole file**
      - **Add**
5. Click the **File** > **Export..**
      - Save as type: **Data Pad Only as Test File**
      - Name file based on naming convention (below)
6. Step 4 & 5 should be done twice per LabChart file, once for resp file, once for CV file
      
#### MSNA Files (Optional)

1. Analyze the **MSNA integrated** channel using the **peak analysis** module in Labchart.
  - Set cutoff according to baseline noise
  - Optimize peak detection settings.
  - Setup table in peak analysis settings to include time and date, along with variables of interest. Time and date should be in the furthest right column       of your table.
  - When happy with peak detection, export in .csv format from the peak analysis table view.
2. Open in Excel and review each burst. Delete those that are not MSNA bursts.
          
### File Naming Conventions

  File name format: **typeOfData_subjectID_cond1_cond2.txt** <br>
  typeOfData: breath (Respiratory File), beat (CV File) or burst (MSNA File) <br>
  SubjectID: Unique subject identifier <br>
  Cond(s): Unique conditons (up to 3) <br>
      eg. breath_07_hx_pre.txt <br>
  NB. Naming must be consitent

### Deployment
1. Download Zip file from Git
2. Extract to folder of choice
3. load beat, breath and burst data files into silo\rawData folder 
3. Open Silo.Rproj
4. From the file explorer in RStudion, open Silo x.y.z.R 
5. Click run app in the script editor (source) pane
  - be sure to run app externally (from drop down menu next to "run")

## Authors

* **Lindsey M Boulet** 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Stuckless T
* Brown CV
* Vermeulen TD
* Foster GE

# Silo
