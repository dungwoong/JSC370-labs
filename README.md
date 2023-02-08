Lab 05 - Data Wrangling
================

# Learning goals

-   Use the `merge()` function to join two datasets.
-   Deal with missings and impute data.
-   Identify relevant observations using `quantile()`.
-   Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
fn <- "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz"
if (!file.exists("met_all.gz"))
  download.file(fn, destfile = "met_all.gz")
met <- data.table::fread("met_all.gz")
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

``` r
merged <- merge(x=met,
                y=stations,
                by.x="USAFID",
                by.y="USAF",
                all.x=T,
                all.y=F)

nrow(merged) # should be 2377343
```

    ## [1] 2377343

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

I averaged variables of interest for each weather station.

``` r
met_avg <- merged[ , .(
  temp = mean(temp, na.rm = TRUE),
  rh = mean(rh, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm=TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  vis.dist = mean(vis.dist, na.rm = TRUE),
  dew.point = mean(dew.point, na.rm = TRUE),
  lat = mean(lat), lon = mean(lon),
  elev = mean(elev, na.rm = TRUE)
), by = "USAFID"]

get_station <- function(col, number) {
  return(met_avg[col == number])
}
tempq <- quantile(met_avg$temp, c(0.5), na.rm=TRUE)
windspq <- quantile(met_avg$wind.sp, 0.5, na.rm=T)
atpq <- quantile(met_avg$atm.press, 0.5, na.rm=T)
```

Some of the medians were calculated by averaging values for 2 different
stations, so I had to search for multiple stations and couldn’t use
simple indexing.

``` r
met_avg[abs(met_avg$temp - tempq) < 0.005]
```

    ##    USAFID     temp       rh atm.press  wind.sp vis.dist dew.point    lat
    ## 1: 720458 23.68173 76.66544       NaN 1.209682 15404.34  18.70838 37.751
    ## 2: 725515 23.68639 82.24308       NaN 2.709164 15514.82  20.24690 40.301
    ##        lon elev
    ## 1: -82.637  372
    ## 2: -96.754  404

``` r
get_station(met_avg$wind.sp, windspq)
```

    ##    USAFID     temp       rh atm.press  wind.sp vis.dist dew.point    lat
    ## 1: 720929 17.43278 75.73968       NaN 2.461838 15435.15  12.58262 45.506
    ##        lon elev
    ## 1: -91.981  378

``` r
met_avg[abs(met_avg$atm.press - atpq) < 0.001]
```

    ##    USAFID     temp       rh atm.press  wind.sp vis.dist dew.point      lat
    ## 1: 722238 26.13978 85.51716  1014.691 1.472656 13366.12  22.07447 31.34990
    ## 2: 723200 25.82436 74.35313  1014.692 1.537661 14549.19  20.29591 34.34823
    ##          lon elev
    ## 1: -85.66667   97
    ## 2: -85.16164  196

The representative stations for the US, in terms of temperature, wind
speed and atmospheric pressure, are all different.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

-   low: temp &lt; 20
-   Mid: temp &gt;= 20 and temp &lt; 25
-   High: temp &gt;= 25

Once you are done with that, you can compute the following:

-   Number of entries (records),
-   Number of NA entries,
-   Number of stations,
-   Number of states included, and
-   Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

-   using your data with the median values per station, examine the
    association between median temperature (y) and median wind speed
    (x). Create a scatterplot of the two variables using ggplot2. Add
    both a linear regression line and a smooth line.

-   fit both a linear model and a spline model (use `gam()` with a cubic
    regression spline on wind speed). Summarize and plot the results
    from the models and interpret which model is the best fit and why.
