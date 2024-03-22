# NVCNutTrends.R

# Produce trend analysis for update of NVC report.
# Require output of a summary table with a row for each WER water body.

# Original column names (types):
# Water body (character);
# First year (num); Last year (num); n (num);
# Test S (num); Test Z (num); Significance (symbols (*s))

#install.packages("trend")
library(tidyverse);library(trend) #mann-kendal & Sen analysis functions

set.seed(pi);x <- rnorm(20)
x <- x[order(x)]
yr <- seq(2001, 2020, by=1)

df <- data.frame(yr,x)

trend::mk.test(df$x)
trend::sens.slope(df$x)
plot(df$x~df$yr)

bu.test(df$x)
plot(bu.test(df$x))

set.seed(pi)
df2 <- data.frame(yr=as.character(sample(yr,5)))
df$yr <- as.character(df$yr)
left_join(df2,df,by=yr)

###
data(Nile)
(out1 <- br.test(Nile))
plot(out1)
(out2 <- bu.test(Nile))
plot(out2)
mk.test(Nile)
cs.test(Nile)

## We have also assessed the **mean** winter nitrogen concentrations for estuaries
## and coastal water bodies and how these have changed over the last one to
## two decades. (Appendix A, Table A3). We have looked at all water bodies with
## sufficient nitrogen data, covering those associated with the existing
## Polluted Waters (Eutrophic) as well as saline water bodies more generally.

## ** looking at MEAN concentrations, so in the code, collapse the ~4 'winter'
## measurements (i.e., Nov to Feb) to a single value