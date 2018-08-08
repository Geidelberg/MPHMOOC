g <- matrix(nrow=66,ncol=9,data=0)
g[,1] <- 1:66 # patient ID
g[,2] <- floor(rnorm(66, mean=60, sd=10)) # age
g[,3] <- floor(runif(66,min=0,max=2)) # gender
g[,4] <- rnorm(66,mean=24, sd=5) + g[,3]/0.5 # BMI, with gender=1 a bit heavier
g[,5] <- floor(runif(66,min=0,max=3)) # smoking: 0,1,2
g[,6] <- c(rep(0,22),rep(1,22),rep(2,22)) # exercise level: 1,2,3
g[,7] <- rbinom(66,10,0.2) # fruit
g[,8] <- rbinom(66,10,0.25) # veg
g[,9] <- rbinom(66,1,0.2) # cancer

# Make some holes in the BMI dist at the higher end
g[g[,4]>30 & g[,4]<35,4] <- 27.3

g[66,5] <- NA # set one smoking value to missing

# Now induce some corr with cancer
u <- floor(runif(66,min=0,max=3))
g[,7] <- g[,7]-g[,9]*u -1 # set fruit intake lower if cancer and subtract 1
g[g[,7]<0,7] <- 0 # can't eat fewer than zero fruit!
g[,4] <- g[,4]-g[,9]*u # make BMI lower if cancer=1

# Make a few people really into their veg
g[64:66,8] <- 9

dimnames(g)[[2]] <- c('patient_id','age','gender','bmi','smoking','exercise',
                      'fruit','veg','cancer') # name the columns

# Export as csv without the row names
write.csv(g, file = "C:/Users/rab97/Documents/Alex work/cancer data for MOOC 1.csv",row.names=FALSE)

# Import it back in

g <- read.csv(file = "C:/Users/rab97/Documents/Alex work/cancer data for MOOC 1.csv",
              header=TRUE, sep=',') # 'sep' not needed but good to be explicit

# If you've set a working directory, then the 'file =' bit will just be the file name
# read.table will also work, but some blogger found it can give errors
# To set the working directory, do this:
# setwd("C:/Documents and Settings/Data")
# Have to use / or \\ as Windows convention of \ will NOT work in file names
# This guy has lots of helpful R stuff: http://rprogramming.net/

age <- g[,2]
gender <- factor(g[,3])
bmi <- g[,4]
smoking <- factor(g[,5])
exercise <- factor(g[,6])
fruit <- g[,7]
veg <- g[,8]
fruitveg <- fruit + veg # just add them together
cancer <- factor(g[,9])
# show the NAs in smoking (a factor)
smoking <- factor(g[,5], exclude=NULL) # prob need to do this for all factors
table(smoking)
# Dichotomise fruitveg
five_a_day <- ifelse(fruitveg>4,1,0) # 1 if 5+, 0 otherwise


# do some summaries
summary(age)
summary(bmi)
table(fruitveg)
table(five_a_day)
table(fruit,veg) # cross-tab
# prop.table(fruitveg) # not useful - gives % of total for each each
tapply(age,gender,mean) # mean age by gender
tapply(bmi,gender,mean) # gender=1 have mean BMI higher by about 1
table(smoking) # note that the table fails to mention the NA - a bug

# Table ignores missing values. To include NA as a category in counts,
# include the table option exclude=NULL if the variable is a vector.
# If the variable is a factor you have to create a new factor using
# newfactor <- factor(oldfactor, exclude=NULL) as above

# histogram
hist(fruitveg)

# t-test (unpaired, unequal variances by default) for BMI and gender
hist(bmi[gender==1]) # visual check of normality (gap between 30 and 35)
t.test(bmi~cancer) # p=0.38 (maybe rerun to get this bigger)
t.test(bmi~cancer, var.equal=TRUE)

# chi-sq test: can use either a matrix or x and y
chisq.test(x=five_a_day,y=cancer)
