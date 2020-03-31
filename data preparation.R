### Data preparation for visualizing ESS data

### Libraries and functions -------------------------------------------------
library(plyr) # for the rounding functions
library(tidyverse)
library(haven) # to read sav (SPSS) files with labels
library(labelled) # to work with labelled data 
library(questionr) # for weighted count tables
library(countrycode) # to switch between country names and codes
library(png) # to read png images
library(emojifont) # to insert logos and emojis
library(extrafont) # to embed extra fonts
library(sysfonts) # to check available fonts and download fonts from google
library(showtext) # to use the extra fonts

## Function to place images by center points on the two axes rather than corners by Stack Overflow user 'Marc in the box', retrieved from: https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}



### Load and inspect data -------------------------------------------------
## Get data from original file (with labels)
dat<-read_sav('./data/ESS7e02_2.sav')

## Look at attributes
attributes(dat$cntry)$label
attributes(dat$cntry)$labels

labelled::var_label(dat)
# lapply(dat, function(x) attributes(x)$label) # same as above, but more verbose

## Look for variables
labelled::look_for(dat, 'weight')
labelled::look_for(dat, 'poor')

### Variable manipulation -------------------------------------------------
## Weights
summary(dat$dweight) # design weights 

summary(dat$pspwght) # post-stratification weights

summary(dat$pweight) # population size weights

### Using the weights
## Country-population-weighted mean of the design-weighted means of the weights of the respondents to Wave 7 of ESS 
weighted.mean(dat$weight, dat$dweight*dat$pweight, na.rm=T)

## Inspect the opposition to immigration from Europe
attributes(dat$eimpcnt)
na_values(dat$eimpcnt)
table(dat$eimpcnt) # distribution of responses (raw)
table(is.na(dat$eimpcnt), dat$cntry) # missing data per country

## Rename the variable and convert to factor
dat$allow.f<-to_factor(dat$eimpcnt, drop_unused_labels=TRUE, ordered=TRUE)
table(dat$allow.f, dat$cntry) # distribution of responses per country (raw)

### Generate weighted aggregates -------------------------------------------------
## Weighted count per country 
dplyr::count(x = dat, allow.f, wt=dweight, by=cntry)

## Another way to get a weighted count per country
questionr::wtd.table(dat$allow.f, dat$cntry, weights=dat$pspwght, digits = 0, na.show=FALSE)
temp.table<-wtd.table(dat$allow.f, dat$cntry, weights=dat$pspwght, digits = 0, na.show=FALSE) # let's assign it

## Now let's get the relative percentages
questionr::cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE)
temp.cprop.table<-cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE) # let's assign it

## Transpose so that countries are rows and make it a data frame (unexpectedly, it's not)
## Note that it needs to the special data.frame.matrix() and not just data.frame()
pt<-as.data.frame.matrix(t(temp.cprop.table)) 
pt<-pt[rownames(pt)!='All',]  # remove the row with the totals if it's included and not needed

## Create new columns for convenience later
pt$afewplus <- pt[ , 'Allow a few'] + pt[, 'Allow many to come and live here']
pt$someplus <- pt[, 'Allow a few'] + pt[ , 'Allow some'] + pt[, 'Allow many to come and live here']

## Order the dataset by the specified column
pt <- pt[order(pt$`Allow none`, decreasing = TRUE),]

### Preparing country flag data ---------------------------------------------
## This block prepares the country flag data that will be used in the plotting
## The first thing to do is to download the free library of country flags rom FlatIcon, available at: https://www.flaticon.com/packs/countrys-flags
## the licence of the flags provides for free use provided that the following atrribution is given: "Icon made by Freepik from www.flaticon.com"
## Nota bene: the name of the file for Poland is manually changed from republic-of-poland.png to poland.png
## Once the flags are downloaded, we can accss, read as png-s and place in the plots, for example: 
temp.flag <- png::readPNG('./flags/197373-countrys-flags/png/albania.png')
dim(temp.flag)

## We can place a png on a plot after we draw from it a raster image, e.g.:
# rasterImage(temp.flag, 1,1,10,71.5) # not run
## However, it is much more convenient to use the custom addImg() function written by 'Marc in a box' that only requires the center of the x- and y- coordinates of the image, rather than the four corners.






