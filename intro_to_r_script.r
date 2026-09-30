############################################################################

# Script for the 'Introduction to R' workshop
# Programming on the Vlaai session on 2026-09-29, 15:00-17:00
#
# Author:  Wolfgang Viechtbauer (https://www.wvbauer.com)
# License: CC BY-NC-SA 4.0
#
# last updated: 2026-09-28

############################################################################

### start RStudio

# open this script: Menu 'File' - Open File (or Ctrl+o / Command+o)

# elements ('panes') of RStudio:
# - top left:     Script Editor
# - bottom left:  Console
# - top right:    Environment, Command History, Connections
# - bottom right: File Browser, Plots, Packages, Help, Viewer

############################################################################

### running commands from a script file

# you can run a command from the script by putting the cursor in the same line
# as the command and then using the keyboard shortcut:
# - Windows: Ctrl+Enter
# - macOS:   Command+Enter
#
# try this out with the following two lines

x <- c(4,2,3,6)
mean(x)

# note that the cursor automatically moves to the next line each time you use
# the shortcut, so this way you can quickly run through a bunch of commands
#
# you can also select/highlight multiple lines and run them all at once

############################################################################

### working directory

# the 'working directory' is the directory (i.e., folder) where R will look
# for files (e.g., datasets you want to load) or where it will save files to
# (e.g., graphs you want to save so that they can be imported into a paper or
# presentation)

# check your working directory

getwd()

# if this is *not* the directory/folder where this script is located:
#
# Menu 'Session' - 'Set Working Directory' - 'To Source File Location'
#
# this sets the working directory to the location of the script (note that
# this actually runs the setwd() command with the correct location)

# check your working directory again

getwd()

# another approach: in the 'Files' tab in the bottom right pane, click your
# way to the directory/folder with the materials, then click 'More' (or the
# symbol that looks like a gear), and select 'Set As Working Directory'

# don't forget to save the script once in a while (Ctrl+s / Command+s) and add
# comments to the script as needed

############################################################################

### importing data

# reading in a rectangular tab-delimited plain-text data file
# - header=TRUE   : first row of the file gives the variable names
# - sep="\t"      : tabs are used as the separator between variables
# - na.strings="" : blank values are interpreted as NA

dat <- read.table("data_survey.dat", header=TRUE, sep="\t", na.strings="")

# note: as long as you don't get an error message, the data were read in

# look at the first 6 rows of 'dat'

head(dat)

# see data_survey.pdf for a description of the variables

# but if there are many variables, then these are wrapped, which is also
# confusing; instead, large datasets are more easily inspected with View()

View(dat)

# in RStudio, can also click on 'dat' in the 'Environment' pane (top right)

# note: in this spreadsheet view, RStudio shows 50 variables at a time; if
# there are more than 50 variables in the dataset, can click on the << < > >>
# buttons to see the other variables

# it is also possible to read in SPSS, Excel, etc. files directly (for this,
# we can use packages such as foreign, readxl, haven, ...)

############################################################################

### inspecting the data

str(dat)   # shows the 'structure' of the object (for data frames, gives info about the variables)
dim(dat)   # dimensions (number of rows and columns)
names(dat) # variable names

# quantitative variables

mean(dat$age, na.rm=TRUE)  # mean
sd(dat$age, na.rm=TRUE)    # standard deviation
range(dat$age, na.rm=TRUE) # minimum and maximum

# by default, if there is at least one missing value in the variable, these
# functions will return NA (not available = missing value); na.rm=TRUE means
# to remove the missings before computing the mean, SD, etc.

# frequency table of a variable

table(dat$source)

############################################################################

### data preparation

# recode items as needed (see 'data_survey.pdf')

dat$lotr2 <- 6 - dat$lotr2
dat$lotr4 <- 6 - dat$lotr4
dat$lotr5 <- 6 - dat$lotr5

dat$mastery1 <- 5 - dat$mastery1
dat$mastery3 <- 5 - dat$mastery3
dat$mastery4 <- 5 - dat$mastery4
dat$mastery6 <- 5 - dat$mastery6
dat$mastery7 <- 5 - dat$mastery7

dat$pss4 <- 6 - dat$pss4
dat$pss5 <- 6 - dat$pss5
dat$pss7 <- 6 - dat$pss7
dat$pss8 <- 6 - dat$pss8

dat$rses3  <- 5 - dat$rses3
dat$rses5  <- 5 - dat$rses5
dat$rses8  <- 5 - dat$rses8
dat$rses9  <- 5 - dat$rses9
dat$rses10 <- 5 - dat$rses10

# compute scale totals

dat <- transform(dat,
   lotr    = lotr1 + lotr2 + lotr3 + lotr4 + lotr5 + lotr6,
   mastery = mastery1 + mastery2 + mastery3 + mastery4 + mastery5 + mastery6 + mastery7,
   posaff  = panas1 + panas4 + panas6 + panas7 + panas9 + panas12 + panas13 + panas15 + panas17 + panas18,
   negaff  = panas2 + panas3 + panas5 + panas8 + panas10 + panas11 + panas14 + panas16 + panas19 + panas20,
   swls    = swls1 + swls2 + swls3 + swls4 + swls5,
   pss     = pss1 + pss2 + pss3 + pss4 + pss5 + pss6 + pss7 + pss8 + pss9 + pss10,
   rses    = rses1 + rses2 + rses3 + rses4 + rses5 + rses6 + rses7 + rses8 + rses9 + rses10
)

# save the dataset in R's own file format

save(dat, file="data_survey_edit.rdata")

############################################################################

# say you now close R/Rstudio (if it asks you to save the workspace, say No!);
# then restart RStudio; you should see that the 'Environment' is empty; we can
# then load the edited version of the dataset with (but remember to set the
# working directory first!)

load("data_survey_edit.rdata")

# now we have 'dat' in our workspace again and we can continue working with it

############################################################################

### some basic plotting

# histograms

hist(dat$age, xlab="Age", main="Histogram of Age", col="skyblue")

# xlab = x-axis label
# ylab = y-axis label
# main = title
# col  = to adjust the color

# built-in color names

colors()

# scatterplots

plot(posaff ~ pss, data=dat, xlab="Stress", ylab="Positive Affect",
     main="Scatterplot of Stress versus Positive Affect",
     pch=19, xlim=c(10,50), ylim=c(10,50), col="gray40", bty="l")

# pch  = point symbol (see help(points) for options)
# xlim = x-axis limits
# ylim = y-axis limits
# bty  = box type around the plot (l = like an L-shape)

############################################################################

### some basic statistics

# t-test

t.test(pss ~ sex, data=dat)

# one-way ANOVA

res <- aov(pss ~ marital, data=dat)
summary(res)

# correlations

cor.test(~ posaff + pss, data=dat)

# linear regression

res <- lm(posaff ~ pss, data=dat)
summary(res)

plot(posaff ~ pss, data=dat, xlab="Stress", ylab="Positive Affect",
     main="Scatterplot of Stress versus Positive Affect",
     pch=19, xlim=c(10,50), ylim=c(10,50), col="gray40", bty="l")
abline(res, lwd=6, col="dodgerblue")

# lwd = to adjust the line width

# polynomial regression

res <- lm(posaff ~ pss + I(pss^2), data=dat)
summary(res)

newdat <- data.frame(pss = 10:50)
pred <- predict(res, newdata=newdat)
lines(newdat$pss, pred, lwd=6, col="firebrick")

# smoother

res <- loess(posaff ~ pss, data=dat)
pred <- predict(res, newdata=newdat)
lines(newdat$pss, pred, col="forestgreen", lwd=6)

# add a legend

legend("topright", legend=c("Linear Model","Quadratic Model","Smoother"),
       col=c("dodgerblue","firebrick","forestgreen"), lwd=6)

############################################################################

### working with packages

# install the 'skimr' package

install.packages("skimr")

# you only need to do this once; so after installing the package, you could
# put a # in front of install.packages("skimr") so that you don't reinstall
# the package over and over when running this script

# load the package

library(skimr)

# use the skim() function from the package on the dataset

skim(dat)

# note: for character variables, the information provided is not so useful; it
# helps to turn such variables into so-called 'factors' (i.e., declare these
# to be categorical variables)

dat$sex      <- factor(dat$sex)
dat$marital  <- factor(dat$marital)
dat$children <- factor(dat$children)
dat$educ     <- factor(dat$educ)
dat$source   <- factor(dat$source)
dat$smoke    <- factor(dat$smoke)

# rerun the skim() function

skim(dat)

############################################################################

### some basic programming

# load the dataset needed for this example

load("data_mirna.rdata")

# examine the first 10 rows and the first 5 columns

dat[1:10, 1:5]

# Braak staging: https://en.wikipedia.org/wiki/Braak_staging
# this variable indicates severity of Alzheimer's disease
# stages 0-2 are coded as Braak = 0
# stages 5-6 are coded as Braak = 1
# (and stages 3 and 4 are dropped from this dataset)

# the remaining variables are microRNA expression levels
# microRNA: https://en.wikipedia.org/wiki/MicroRNA

# the goal of this analysis is to examine which microRNAs are differentially
# expressed in the two Braak groups

# fit 490 simple regression models with 'Braak' as the predictor variable and
# each of the other variables as outcome variables

res <- matrix(data=NA, nrow=490, ncol=4)

for (i in 2:491) {
   fit <- lm(dat[,i] ~ Braak, data=dat)
   res[i-1,] <- coef(summary(fit))[2,]
}

colnames(res) <- c("Estimate","SE","t","p_value")
rownames(res) <- colnames(dat)[2:491]

# examine the first 6 rows

head(res)

# turn the matrix into a data frame

res <- as.data.frame(res)

# order the data frame by the p-values (lowest to highest)

res <- sort_by(res, ~ p_value)

# inspect the first 6 rows (the most significant microRNAs)

head(res)

# plot of the -log10() transformed p-value (higher = more significant)

plot(-log10(res$p_value), pch=19, col="gray40", ylab="-log10(p-value)", bty="l")

# instead of using -log10(0.05) as the threshold for significance, we want to
# apply a correction for multiple testing; using a Bonferroni correction, we
# use -log10(0.05 / 490) as the threshold for significance

-log10(0.05 / 490)

# add this threshold as a horizontal line to the plot

abline(h=-log10(0.05 / 490), lty="dotted")

############################################################################

# where to go from here? maybe: https://www.bigbookofr.com

############################################################################
