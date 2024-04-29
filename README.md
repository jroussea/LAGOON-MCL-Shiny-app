# LAGOON-MCL-Shiny-app
A shiny application to explore the results produced by LAGOON-MCL

## Installing the app

To use this application, you can use the conda environment `environment.yaml` which contains all the necessary libraries.

```
conda env create -f environment.yam

```

## Using the app

Run conda environment
```
conda activate lagoon-mcl-shiny-app
```

Run R

```
R
```

Adding an Internet browser (important to be able to launch the Shiny application)
Tester with `firefox` and `google-chrome`

```
options(browser = "firefox")
```

Run the R Shiny app

```
shiny::runApp("app.R", launch.browser = TRUE)
```
