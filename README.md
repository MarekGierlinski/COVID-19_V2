# Processing and visualisation of COVID-19 data

## Sources

The following data sources are used:

 - ECDC - provides with weekly cases/deaths data from all countries around the world.
 - UK government - provides daily data about cases, deaths, hospital admissions, testing, vaccination and more.
 - Financial Times - their GitHub repository contains processed data about mortality excess in selected countries.

## How to use

Make sure packages listed in `R/packages` are all installed. From the top project directory, within R/RStudio type:

```
library(drake)
r_make()
```

Figures will be created in `fig` folder.
