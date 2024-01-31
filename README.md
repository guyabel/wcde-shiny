# Wittgenstein Centre Data Explorer

Contains all code and data used to build the shiny app for the Wittgenstein Centre Data Explorer, available at <http://www.wittgensteincentre.org/dataexplorer/>

An overview of the app is given at <https://gjabel.wordpress.com/2015/06/15/shiny-app-for-the-wittgenstein-centre-population-projections/>

Data formatting and upload based in the separate repo: <https://github.com/guyabel/wcde-data>

`www` folder contains images used on site and additional files.

`server` folder contains all the R code that is dependent (reactive) on the user inputs, referred to in `server.R`, including

-   `[foo].R`: returns plots or data frames in a given tab.

-   `[foo]_choices.R`: returns reactive user inputs where choices are dependent on other user inputs.

-   `[foo]_fn.R:` R functions that do not involve any shiny specific functions.

`label.R` produces the set of objects in `label.RData`. It contains

-   lists of indicator names, country names, age groups, etc., etc., used in the drop-down fields

In addition to data frames from the meta folder:

-   `ind`: details on which indicators are available by which dimension.

-   `geog`: details on country, areas and region names and codes.

-   `dimen` data frame: details on the scenario, age, sex and education dimensions.

-   `faq`: displayed in the FAQ tab.

-   `assump`: details on projection assumptions used in each scenario-country combination.

The objects in `label.RData` are loaded at the top of the `ui.R` script.

The `ui.R` script refers to a number of files in the `ui` folder to control the layout including:

-   `[foo].md` files are markdown text generally located at the top of the page above the tabs.

-   `[foo].html` files are HTML code to display various images.

-   `[foo].R` scripts are shiny ui functions to create and arrange user inputs in each part of the app.
