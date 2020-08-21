# Azavea Summer of Maps
This is the repository for the [OpenStreetMap US](https://www.openstreetmap.us/) project for the 2020 edition of the [Azavea Summer of Maps Fellowship](https://www.summerofmaps.com/).

* **Fellow:** [Eugene Chong](https://e-chong.github.io/)
* [Final presentation recording](https://drive.google.com/file/d/1p1Ah5jGqwYu9T0ZoUwC2Vuoxg3fY7hJW/view?usp=sharing)
* [Project description](https://www.summerofmaps.com/projects/shortlist/2020-openstreetmap-us)

# Files

## SDG Methodology Summary

**SDG Methodology.html:** Summarizes the methodologies used to calculate the SDG indicators.

## OSM QGIS tutorial

**OSM QGIS tutorial.html:** The tutorial for downloading, manipulating, and mapping OSM data in QGIS. Also available on [TeachOSM](https://teachosm.org/).

## Code

All scripts can be run in the order that they are numbered.

### ~scripts

**00 - Admin.R:** Load packages and define filepath for Dropbox data. Run this whenever starting an analysis task.

**01 - Utility Functions.R:** Any functions written in this project will be saved here.

**1\_.R:** Read in or collect raw data

**2\_.R:** Clean data for analysis

**3\_.R:** Analysis

**4\_.R:** Plots

### Objects

#### TransitSDG GTFS data

Downloaded GTFS csvs

#### Intermediate objects

All intermediate objects are saved as `.rds` and are saved using the following format.

**Format:** `/~outputs/[scriptRange]/[script#]_[objectName].rds`

**Example:** `/~outputs/10/11_BGs.rds` is object `BGs` produced in script `11 - Read Census data.R.