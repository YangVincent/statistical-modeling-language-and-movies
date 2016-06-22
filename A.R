# This function searches through all the needed files and gets rid of anything that has
# two vertical lines in a row. This allows R to read the files into a data frame properly.
convert <- function() {
  f = "ml-100k/u.item" #make a file object
  x <- readLines(f) #read in the file
  y <- gsub("\\|\\|", "\\|nodata\\| ", x) #search and replace || with |nodata|
  y <- gsub("'", "", y) #sub out quotes which mess up ordering
  cat(y, file=f, sep="\n") #Remake the file
}

#This function prepares the item file for the merging. It prepares 
# row and column titles, and separates them with commas for an output file
# for easy readability. This is done in a csv. 
itemf <- function () {
  convert()
  item <- read.table("ml-100k/u.item", sep="|", fill=TRUE) #read in item 
  names(item) <- c("MovieID", "title", "ReleaseDate", "vidreleasedate", "imdb", "Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western") #assign new column titles
  write.table(item, file="item.csv", row.names=FALSE, col.names=TRUE, sep=",") #output the file in a csv.
  return(item)
}


#This function prepares the data file for the merging. It prepares 
# row and column titles, and separates them with commas for an output file
# for easy readability. This is done in a csv. 
dataf <- function() {
  data <- read.table("ml-100k/u.data", fill=TRUE) #read in user
  names(data) <- c("UserID", "MovieID", "Rating", "Timestamp")
  write.table(data, file="data.csv", row.names=FALSE, sep=",")
  return(data)
}

#This function prepares the user file for the merging. It prepares 
# row and column titles, and separates them with commas for an output file
# for easy readability. This is done in a csv. 
userf <- function() {
  user <- read.table("ml-100k/u.user", sep="|") #read in user
  names(user) <- c("UserID", "Age", "Gender", "Occupation", "ZIPCode")
  write.table(user, file="user.csv", row.names=FALSE, sep=",")
  return(user)
}

# this file merges the UserID and MovieID by data and user, then the merged with item.
# this creates a large file with all needed bits of information together.
mergef <- function () {
  d <- dataf()
  u <- userf()
  i <- itemf()
  two <- merge(d, u, by="UserID") #this works
  write.table(two, "two.csv", row.names=FALSE, col.names=TRUE, sep=",")# this works

  three <- merge(two, i, by="MovieID")
  write.table(three, "merged.csv", col.names=TRUE, row.names=FALSE, sep=",")
  return(three)
}

# this file removes unneeded fields, and gets names ready for problem A. 
# this method also removes duplicates for each user's ratings, and replaces them with the average
# this makes it so that each user only gets to 'vote' once.
splitf <- function() {
  merged <- mergef()
  cp <- userf()
  cp$Occupation <- NULL
  cp$ZIPCode <- NULL
  cp$Age<-NULL
  #break up into groups = by person
  #within each group, apply a function
  ready <- aggregate(merged$Rating, list(merged$UserID), mean, simple=FALSE) #original
  names(ready) <- c("UserID", "AverageRating")

  #merge
  ready <- merge(ready, cp, by="UserID")
  #delete columns until only id and gender, then merge that to ready.
  #end merge

  write.table(ready, "ready.csv", row.names=FALSE, col.names=TRUE, sep=",")
  return(ready)
}

# this method gets the tables ready for h. This is important because
# some fields have missing information and have to be removed. Also, 
# this allows the group to have different breakups depending on what
# field we want to be separated.
preph <- function() {
  merged <- mergef()
  cp <- userf()
  cp$Occupation <- NULL
  cp$ZIPCode <- NULL
  #break up into groups = by person
  #within each group, apply a function
  ready <- aggregate(merged$Rating, list(merged$UserID), mean, simple=FALSE) #original
  names(ready) <- c("UserID", "AverageRating")

  #merge
  ready <- merge(ready, cp, by="UserID")
  #delete columns until only id and gender, then merge that to ready.
  #end merge

  write.table(ready, "h.csv", row.names=FALSE, col.names=TRUE, sep=",")
  return(ready)
}

prepa <- function() {
  table <- mergeall()
  write.table(table, file="output.csv", sep=",")
}

aa <- function() { #problem 1 done
  #find an approximate 95% confidence intervalfor the population mean rating by men
    #find all men's ratings
    #find confidence interval
  #references: http://www.cyclismo.org/tutorial/R/confidence.html
  table <- splitf()
  #get all ratings from table where gender = M
  mtable <- subset(table, !(Gender %in% c("F"))) #grab data where v2 doesn't have F
  sdev <- sd(mtable$AverageRating)
  size <- nrow(mtable)
  mean <- sum(mtable$AverageRating)/size

  error <- qt(0.975, df=size-1) * sdev/sqrt(size)
  cat("95% interval:", mean-error, mean+error, "\n")
}

ab <- function() { #problem 2 done
  #find an approximate 95% confidence intervalfor the population mean rating by men
    #find all women's ratings
    #find confidence interval
  table <- splitf()
  #get all ratings from table where gender = F
  ftable <- subset(table, !(Gender %in% c("M"))) #grab data where v2 doesn't have F
  sdev <- sd(ftable$AverageRating)
  size <- nrow(ftable)
  mean <- sum(ftable$AverageRating)/size

  error <- qt(0.975, df=size-1) * sdev/sqrt(size)
  cat("95% interval:", mean-error, mean+error, "\n")
}

ac <- function() {
  #find approximate 95% interval for difference between two means
  table <- splitf()
  mindex <- table$Gender == "M"
  male <- table[mindex,]$AverageRating

  female <- table[!mindex,]$AverageRating
  print(t.test(male, female, var.equal = TRUE))
}

ad <- function() {#done
  table <- splitf()
  mtable <- subset(table, !(Gender %in% c("F")))
  ftable <- subset(table, !(Gender %in% c("M")))
  msize <- nrow(mtable)
  fsize <- nrow(ftable)
  xbar <- sum(mtable$AverageRating)/msize
  ybar <- sum(ftable$AverageRating)/fsize
  msdev <- sd(mtable$AverageRating)
  fsdev <- sd(ftable$AverageRating)
  se <- sqrt(msdev*msdev/msize + fsdev*fsdev/fsize)
  z = (xbar - ybar - 0)/se
  cat("Z: ", z, "\n", "For values of Z > 1.96, we reject the Hypothesis that the population means are equal\n")
}

ae <- function() { #done
  #plot male and female ratings on a histogram
  table <- splitf()
  ftable <- subset(table, !(Gender %in% c("M"))) #grab data where v2 doesn't have F
  mtable <- subset(table, !(Gender %in% c("F"))) #grab data where v2 doesn't have F

  library(ggplot2) #the plot object is the total sets of information needed
  #for ggplot. This allows us to save to a file, in savePlot. 
  # The first part sets the table, which is also the data used to graph.
  # the next line sets the first histogram - Male, and makes binwidth = 0.1.
  # This makes what is seemingly continous points into ranges, so that they can be
  # graphed on a histogram. The same is done on the last line, but for women.
  plot <- ggplot(table, aes(x=AverageRating))+
    geom_histogram(data = subset(table, Gender %in% c("M")), binwidth=0.1, fill="blue", alpha=0.2)+
    geom_histogram(data = subset(table, Gender %in% c("F")), binwidth=0.1, fill="red", alpha=0.2)

  savePlot(plot, "histogram.png")
}

savePlot <- function(plot, filename) {
  pdf(filename)
  print(plot)
  dev.off()
}

af <- function() {
  table <- splitf()
  n <- nrow(table)
  # calculate difference in sums of male and female raters
  k <- sum(table$Gender == "M") - sum(table$Gender == "F")
  pbar <- k / n     # calculate sample mean
  se <- sqrt(pbar * (1 - pbar) / n)   # calculate standard error
  e <- qnorm(0.975) * se
  print(pbar + c(-e, e))
}

ag1 <- function() { 
  table <- splitf()
  n <- nrow(table)
  k <- sum(table$Gender == "M")
  pbar <- k/n
  se <- sqrt(pbar * (1-pbar) / n)
  e <- qnorm(0.975) * se
  print(pbar + c(-e, e))
}

ag <- function() { 
  table <- splitf()

  n <- nrow(table)
  k <- sum(table$Gender == "M") #number of males
  print(prop.test(k, n))
  # find the number of males, and project the actual number in the real world
  # based off of the sample.
}

ah1 <- function() {
  table <- preph() 
  gender <- table$Gender #isolate the needed variables
  age <- table$Age
  gender <- as.integer(gender=="F") #make indicator varialbes by Gender
  y <- table$AverageRating
  fit <- lm(y ~ age + gender)
  print(summary(fit)) #figure out the needed information from linear regression from guessing from age and gender
  summaryfit <- summary(fit)
  fitcov <- vcov(fit) #fit the covariance
  se <- sqrt(fitcov[2,2])
  bAge <- coef(fit)[2]
  cat("95% Confidence interval for age coefficient: (", bAge-1.96*se,", ", bAge+1.96*se, ")\n")
  #make a function for a Z test
  cat("bAge = ", bAge, "\n")
  Z <- (bAge - 0)/se
  cat("Z = ", Z, "\n")

  if (Z > 1.96) cat("We reject The Hypothesis at the 5% level\n")
  if (Z > 1.96) cat("The p-value was ", (1 - pnorm(Z)) * 2, "\n")
}

ah2 <- function() {#done
  table <- preph()
  gender <- table$Gender
  age <- table$Age
  gender <- as.integer(gender=="F")
  
  y <- table$AverageRating
  fit <- lm(y ~ age + gender)
  h2val <- rbind(1, 28, 1)
  h2mean <- sum(coef(fit)*h2val)
  
  A <- vcov(fit)
  se <- sqrt( c(1,28,1) %*% A %*% h2val)
  cat("95% Confidence Interval for mean population rating among women of age 28: (", h2mean-(1.96*se),", ", h2mean+(1.96*se),")\n")
}
