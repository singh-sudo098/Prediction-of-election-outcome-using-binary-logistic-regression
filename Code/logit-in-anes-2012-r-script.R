
###################################################

rm(list=ls(all=TRUE))  # Standard code to clear R workspace

# setwd("DIRECTORY")  # Replace "DIRECTORY" with the correct path

setwd("~/Dropbox/Tom/Odum/SAGE/Data Project/a-Version 3/ANES 2012/Subset1")

# Use only one of the two following lines to load
# either the .RData file or the .csv file that
# includes the variables for this example

# ANES <- load("dataset-anes-2012-subset1.RData")

ANES <- read.csv("dataset-anes-2012-subset1.csv")

# This line attaches the data file so R
# can find the variables inside.

attach(ANES)


# You can see the names of the variables
# by using the following code

names(ANES)


# Frequency table for the vote variable
table(vote_obama)


# Estimate the logit model and store the
# results in an object named L1

L1 <- glm(vote_obama ~ ft_dem + ft_rep
          + black + hispanic + other
          + income,
          family=binomial(link=logit))


# Display the results of logit model

summary(L1)


# Use the exponetial function to 
# compute factor changes in the odds
# for every independent variable in the model

round(exp(coef(L1)),digits=2)


###########################################
# Computing the Predicted Probability of
# voting for Obama for black respondents
###########################################

# Set specific values of the independent variables
profile.b <- with(ANES,data.frame(ft_dem=mean(ft_dem,na.rm=TRUE),
                                   ft_rep=mean(ft_rep,na.rm=TRUE),
                                   black=1,
                                   hispanic=0,
                                   other=0,
                                   income=mean(income,na.rm=TRUE)))

# Use predict() to combine model estimates with profile established above
black.prob <- as.numeric(predict(L1,newdata = profile.b, type="response",se.fit=TRUE)[1])
black.prob.se <- as.numeric(predict(L1,newdata = profile.b, type="response",se.fit=TRUE)[2])

# Print the results to the screen
cat(" Predicted Probability = ",black.prob,"\n",
    "95% Confidence Interval: ",black.prob-1.96*black.prob.se,
    "to",
    black.prob+1.96*black.prob.se,"\n")


###########################################
# Computing the Predicted Probability of
# voting for Obama for Hispanic respondents
###########################################

# Sets specific values of the independent variables
profile.h <- with(ANES,data.frame(ft_dem=mean(ft_dem,na.rm=TRUE),
                                ft_rep=mean(ft_rep,na.rm=TRUE),
                                black=0,
                                hispanic=1,
                                other=0,
                                income=mean(income,na.rm=TRUE)))

# Uses predict() to combine model estimates with profile established above
hispanic.prob <- as.numeric(predict(L1,newdata = profile.h, type="response",se.fit=TRUE)[1])
hispanic.prob.se <- as.numeric(predict(L1,newdata = profile.h, type="response",se.fit=TRUE)[2])

# Print the results to the screen
cat(" Predicted Probability = ",hispanic.prob,"\n",
    "95% Confidence Interval: ",hispanic.prob-1.96*hispanic.prob.se,"to",hispanic.prob+1.96*hispanic.prob.se,"\n")

###########################################
# Computing the Predicted Probability of
# voting for Obama for "other" respondents
###########################################

# Sets specific values of the independent variables
profile.o <- with(ANES,data.frame(ft_dem=mean(ft_dem,na.rm=TRUE),
                                ft_rep=mean(ft_rep,na.rm=TRUE),
                                black=0,
                                hispanic=0,
                                other=1,
                                income=mean(income,na.rm=TRUE)))

# Uses predict() to combine model estimates with profile established above
other.prob <- as.numeric(predict(L1,newdata = profile.o, type="response",se.fit=TRUE)[1])
other.prob.se <- as.numeric(predict(L1,newdata = profile.o, type="response",se.fit=TRUE)[2])

# Print the results to the screen
cat(" Predicted Probability = ",other.prob,"\n",
    "95% Confidence Interval: ",other.prob-1.96*other.prob.se,"to",other.prob+1.96*other.prob.se,"\n")


###########################################
# Computing the Predicted Probability of
# voting for Obama for white respondents
###########################################

# Sets specific values of the independent variables
profile.w <- with(ANES,data.frame(ft_dem=mean(ft_dem,na.rm=TRUE),
                                  ft_rep=mean(ft_rep,na.rm=TRUE),
                                  black=0,
                                  hispanic=0,
                                  other=0,
                                  income=mean(income,na.rm=TRUE)))

# Uses predict() to combine model estimates with profile established above
white.prob <- as.numeric(predict(L1,newdata = profile.w, type="response",se.fit=TRUE)[1])
white.prob.se <- as.numeric(predict(L1,newdata = profile.w, type="response",se.fit=TRUE)[2])

# Print the results to the screen
cat(" Predicted Probability = ",white.prob,"\n",
    "95% Confidence Interval: ",white.prob-1.96*white.prob.se,"to",white.prob+1.96*white.prob.se,"\n")



########################################################
# Predicted Probabilities and 95% CI bounds for values
# of the Democratic Party feeling thermometer
########################################################

# Creat an sequence from 0 through 100
# to capture the range of the feeling thermometer

therm <- seq(0,100,by=1)


# Define the number of predicted probabilities
# that will be computed in the loop.  This
# includes one for each value of the feeling thermometer

Sims <- length(therm)


# Create an empty object to store the results
# of every predicted probability calculated
# in the look that is coming

dt.probs <- matrix(NA,Sims,4)


# Using a for() loop to compute the
# predicted probability, standard error,
# and confidence interval for every value
# of the feeling thermometer

for(i in 1:Sims){
profile.dt <- with(ANES,data.frame(ft_dem=therm[i],
                                  ft_rep=mean(ft_rep,na.rm=TRUE),
                                  black=mean(black,na.rm=TRUE),
                                  hispanic=mean(hispanic,na.rm=TRUE),
                                  other=mean(other,na.rm=TRUE),
                                  income=mean(income,na.rm=TRUE)))

dt.prob <- as.numeric(predict(L1,newdata = profile.dt, type="response",se.fit=TRUE)[1])
dt.prob.se <- as.numeric(predict(L1,newdata = profile.dt, type="response",se.fit=TRUE)[2])
dt.prob.ci <- c(dt.prob,dt.prob-1.96*dt.prob.se,dt.prob+1.96*dt.prob.se)
dt.probs[i,] <- c(therm[i],dt.prob.ci)

}


########################################################
# Make the plot of the Effect of the Democratic Party
# Feeling Thermometer on the Probability of Voting
# for Obama, holding all other variables in the model
# at their mean values.
########################################################

par(mar = c(4, 5, .1, .1))
plot(dt.probs[,1], dt.probs[,2], type = "n", 
     ylim = c(-.025, 1.025), xlab = "", ylab = "", axes = FALSE)
lines(dt.probs[,1], dt.probs[,2], lwd = 4, lty = 1, col="red") 
lines(dt.probs[,1], dt.probs[,3], lwd = 2, lty = 2, col="blue")
lines(dt.probs[,1], dt.probs[,4], lwd = 2, lty = 2, col="blue")
title(ylab = expression("Predicted Prob. of Voting for Obama"),
      line = 3.5, cex.lab = 1)
title(xlab = expression("Democratic Party Feeling Thermometer"),
      line = 2.75, cex.lab = 1)
axis(1, at=seq(0,100,10),las=2,cex.axis=1)
axis(2, at = seq(0, 1, .1), las = 2, cex.axis = 1)
box()
rug(jitter(ft_dem, factor=4), ticksize = .015)
legend("topleft", bty = "n", 
       c(expression("Point Est."), 
         expression("95% CI")),
       col=c("red","blue"),lty = c(1, 2), lwd = c(4, 2), cex = 1)
