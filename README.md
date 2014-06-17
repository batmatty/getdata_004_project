# getdata_004_project

This repo contains the R analysis and data files for the getting and cleaning data course on Coursera.

This repo contains the following files:

* run_analysis.R
* cleanDataSet.csv

Each of these are explained below.

# run_analysis.R

run_analysis.R contains 2 functions:

* getData()
* importData()

**Note: You will need to have downloaded and installed the data.table R package!**

### getData()

getData is helper function that downloads the data from the URL provided in the course. It creates a "data" directory in the current working direction, downloads the raw data file to this directory and unzips the file ready for use.

Run this function before you do anything else to ensure that the data is in the correct location. 

### importData()

importData does the actual manipulation of the raw data. It imports only the mean and standard deviation variables into a data.table. It uses this data table to create the clean data set and saves it to a the csv file. 

# cleanDataSet.csv

This is a csv file containing the clean data set generated by the getData() function. 
