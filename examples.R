library(assert_that)
library(dplyr)
library(ffbase)

ds <- source_ffdf(mtcars)
ds


 data("baseball", package = "plyr")
 
 for (n in colnames(baseball)){
   x <- baseball[[n]]
   if (is.character(x)) baseball[[n]] <- factor(x) 
 }

 # If you start with a ffdf, you end up with a ffdf
 baseball <- as.ffdf(baseball)
 filter(baseball, year > 2005, g > 130)
 head(select(baseball, id:team))
 summarise(baseball, g = mean(g))
 head(mutate(baseball, rbi = r / ab, rbi2 = rbi ^ 2))
 head(arrange(baseball, id, desc(year)))

 # If you start with a source, you end up with a source
 baseball_s <- as.source(baseball)
