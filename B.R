# This function reads in the table with needed information
prep <- function() {
	table <- read.table("vocabulary_norms_data.csv", sep=",", fill=TRUE, header=TRUE)
}

# This predicts vocab from the person's age, and graphs it
vocab_from_age <- function() {
	table <- prep()
	model <- lm(table$age~table$vocab) #predict vocab given age, and find correlation
	print(summary(model))

  # plot the given vocab for each age, and the trendline.
  # The plot object is for saving the image
  # the first line sets points as well as the data needed for creating the graph
  # The second line creates a line that goes through, to demonstrate trend.
  # The second and third line put in tites for what each label should be on the graph
	library(ggplot2)
	plot <- ggplot(model, aes(x=table$age, y=table$vocab)) + geom_point() + 
		stat_smooth(method="lm", col="red") + ylab(label = "Vocabulary") + 
			xlab("Age")

	savePlot(plot, "vocab_from_age.pdf")
}

# This is a function I created to save images
savePlot <- function(plot, filename) {
	pdf(filename)
	print(plot)
	dev.off()
}

# This will predict ethnicity from vocab
vocab_from_ethnicity <- function() {
	table <- prep()
	table <- na.omit(table) #get rid of the useless/unusable data
	model <- lm(table$vocab ~ table$ethnicity, data=table) #guess vocab from ethnicity
  #R makes indicator variables, so each ethnicity is separated and labeled
	print(summary(model)) #find the correlations between ethnicity and the amount
  # of vocab each person knows

	data1 <- data.frame(table$ethnicity, table$vocab) #Isolate the needed data
	library(ggplot2) #import ggplot 
  # The plot object is for outputting to a file
  # the first line lets use decide which function to plot the points on. It tries
  # to predict vocab given ethnicity. 
  # Each ethnicity is separated, and the average from vocab is added in
	plot <- ggplot(data1, aes(x=table$ethnicity, y=table$vocab)) +  
		geom_point(size=2) + 
			stat_smooth(method=lm) + ylab("Vocab") + xlab("Ethnicity") + 
				stat_summary(fun.y="mean", geom="point", color='red', size=5)
	savePlot(plot, "vocab_from_ethnicity.pdf")
}

vocab_from_mom_ed <- function() {
	table <- prep()
	table <- na.omit(table)
	model <- lm(table$vocab ~ table$mom_ed, data=table)
	print(summary(model))

	data1 <- data.frame(table$sex, table$vocab)
	library(ggplot2)
	plot <- ggplot(data1, aes(x=table$mom_ed, y=table$vocab)) + 
		geom_point(size=2) + 
			geom_smooth(method=lm) + ylab("Vocab") + xlab("Mom's Education") + 
				stat_summary(fun.y="mean", geom="point", color='red', size=5)
	savePlot(plot, "vocab_from_mom_ed.pdf")
}

vocab_from_age_and_ethnicity <- function() {
	table <- prep()
	model <- lm(table$vocab ~ table$age + table$ethnicity, data=table)
	print(summary(model))

	relevant <- table[c("vocab", "age", "ethnicity")]
	
	table$model <- stats::predict(model, newdata = table)
	err <- stats::predict(model, newdata=table, se=TRUE)
	table$ucl <- err$fit + 1.96 * err$se.fit
	table$lcl <- err$fit - 1.96 * err$se.fit
	
	Ethnicity <- table$ethnicity

	library(ggplot2)
	g <- ggplot(table)
	g <- g + geom_point(aes(x=table$age, y=table$vocab, color=Ethnicity))
	g <- g + geom_smooth(aes(x=table$age, y=model, color=Ethnicity))
	g <- g + labs(x = "Age", y = "Vocab")
	g <- g + ggtitle("Vocab vs. Age split by ethnicity")
	g <- g + scale_fill_discrete(name="Ethnicity")	
	savePlot(g, "vocab_from_age_and_ethnicity.pdf")
}
# prints the currently displayed graph to the
# file filename; suffix can be "pdf", "png" or "jpg"
pr2file <- function (filename)
{
	origdev <- dev.cur()
	parts <- strsplit(filename,".",fixed=TRUE)
	nparts <- length(parts[[1]])
	suff <- parts[[1]][nparts]
	if (suff == "pdf") {
		pdf(filename)
	}
	else if (suff == "png") {
		png(filename)
	}
	else jpeg(filename)
	devnum <- dev.cur()
	dev.set(origdev)
	dev.copy(which = devnum)
	dev.set(devnum)
	dev.off()
	dev.set(origdev)
}
