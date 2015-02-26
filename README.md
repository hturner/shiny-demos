# shiny-demos

A set of shiny demos for teaching statistical modelling

### Install Required Pacakges

The packages required by each app are as follows:

linreg
:    RColorBrewer, scales, ISwR, MASS, shinytools, shiny

linreg3
:    MASS, shiny

poisreg
:    RColorBrewer, scales, shinytools, shiny

All packages are available on CRAN apart from `shinytools` which is part of this repository. The following code will install all required packages:
    
    required <- c("RColorBrewer", "scales", "ISwR", "MASS", "devtools", "shiny")
    notInstalled <- required[!(required %in% installed.packages()[, "Package"])]
    install.packages(notInstalled)
    library(devtools)
    install_github("hturner/shiny-demos", subdir = "shinytools")
    
### Run the Apps

Once the required packages are installed you can run the apps directly from GitHub, e.g.

    library(shiny)
    runGitHub("shiny-demos", username = "hturner", subdir = "linreg/")
    
Alternatively you can download the source files and use `runApp` from the `shiny` package.