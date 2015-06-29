# wcde
Wittgenstein Centre Data Explorer

Contains all code and data used to build the shiny app for the Wittgenstein Centre Data Explorer, available at
http://www.wittgensteincentre.org/dataexplorer/

An overview of app is given at
https://gjabel.wordpress.com/2015/06/15/shiny-app-for-the-wittgenstein-centre-population-projections/

Folder df0 contains the raw data from Samir.

Folders df1-df7 contain formatted data for quick reading into the shiny app. Produced by the saves packages in the do_data.R script 

Folder pdf contains all the country profiles for downloading.

Folder www contains images.

Folder server contains all the R code that is dependent (reactive) on the user inputs including
*	[foo]_build.R: returns plots or data frames.
*	[foo]_ui.R to: returns reactive user inputs where choices are dependent on other user inputs.
*	[foo]_fn.R: R functions that do not involve any shiny specific functions.

do_label.R produces the set of objects in label.RData. It contains
 * lists of indicator names, country names, age groups, etc., etc., used in the drop down fields
 * ind data.frame: details on which indicators are available by which dimension. 
 * geog data.frame: details on country, areas and region names and codes. 
 * dimen data.frame: details on scenario, age, sex and education dimensions.
 * faq data.frame: displayed in the FAQ tab.
 * assump data.frame: details on projection assumptions used in each scenario-country combination
The objects in label.RData are loaded at the top of the ui.R script. 

The ui.R script refers to a number of files in order to control the layout including:
*	[foo].md files are markdown text generally located at the top of the page above the tabs. 
*	[foo].html  files are HTML code to display various images.
*	[foo]_select.R scripts are shiny ui functions to create and arrange user inputs in each part of the app.
*	data_show.R script arranges the layout of the data tab.
