#install.packages('foreign')
#install.packages('tidyverse')
library(foreign) 
library(tidyverse)
library(dplyr)
library(VGAM)

# USA 2008 Presidential Pre-election polling data (STATA dta-file) http://www.stat.columbia.edu/~gelman/book/data/pew_research_center_june_elect_wknd_data.dta
# USA 2008 Presidential Election results http://www.stat.columbia.edu/~gelman/book/data/2008ElectionResult.csv
# setwd("~/.../...")

# --- meet the data --- #

pre_election_polls <- read.dta('pew_research_center_june_elect_wknd_data.dta')
election_results <- read.csv(file = '2008ElectionResult.csv')

# check if datasets have same states in same order (purkka)
col1 <- cbind(summary(pre_election_polls$state))
col2 <- cbind(election_results$state)
cbind(col1, col2)

# remove missing values, Alaska (n=0), Hawaii (n=1)
election_polls_ <- subset(pre_election_polls, 
                          state != "alaska" & state != "hawaii")

# --- prepare the data --- #

# pick ideological voters by state
data_by_state_ideo <- election_polls_ %>%
    group_by(state, ideo) %>%
    summarise(count = n())
    
# pick only very liberal voters by state
data_by_state_veryliberal <- subset(data_by_state_ideo, 
                                    ideo == "very liberal", 
                                    state != "alaska")

# drop factor levels -> int, (count of poll answers per state)
data_by_state_veryliberal$polls_per_state <- as.integer(table(droplevels(election_polls_$state)))

# ratio per state (very liberals / number of poll responses per state)
data_by_state_veryliberal$mean <- data_by_state_veryliberal$count/data_by_state_veryliberal$polls_per_state

# add proportional Obama voters from election data by region (state)
election <- subset(election_results, state != "Alaska" & state != "Hawaii") # remove alaska, hawaii
data_by_state_veryliberal$obama_voters_prop_per_state <- election$vote_Obama_pct / 100

# data ready to use
View(data_by_state_veryliberal)


# --- visualise the data --- #

# scatter plot 1: 
# x=: proportion of very liberal poll participants ~ y=: proportion of Obama voters per region

plot(data_by_state_veryliberal$mean,
     data_by_state_veryliberal$obama_voters_prop_per_state,
     xlab = "Poll: Voters who describe their views as very liberal (proportion per state)",
     ylab = "Obama voters (proportional per state)",
     col = "green",
     main = "Presidential elections of USA 2008",
     text(data_by_state_veryliberal$mean,
     data_by_state_veryliberal$obama_voters_prop_per_state,
     labels = data_by_state_veryliberal$state,
     cex= 0.5, pos = 2))

# scatter plot 2:
# x=: number of poll participants ~ y=: proportion of very liberals per region

plot(data_by_state_veryliberal$polls_per_state,
           data_by_state_veryliberal$mean,
           xlab = "Number of participants of the poll in the state",
           ylab = "proportion of very liberals per state",
           col = "orange",
           main = "Presidential elections of USA 2008",
           text(data_by_state_veryliberal$polls_per_state,
          data_by_state_veryliberal$mean,
          labels = data_by_state_veryliberal$state,
          cex= 0.5, pos = 1))



# --- likelihood and empirical Bayes analysis --- #

" Using a little shortcut known called ´empirical Bayes´, not setting priors for the prior parameters, 
but estimating the prior parameters from the data.
-> observed number of ’very liberals’ with random variables Y1, . . . Y49 as samples from binomial distributions 
-> own parameters θj for each state, and these parameters are a sample from the common beta distribution."

# Drop levels & reform factors to integers
data_by_state_veryliberal <- (droplevels(data_by_state_veryliberal))
data_by_state_veryliberal$count <- as.integer(data_by_state_veryliberal$count)
data_by_state_veryliberal$polls_per_state <- as.integer(data_by_state_veryliberal$polls_per_state) # number of poll answers per state

# negative log likelihood of data given alpha; beta. 
# [Likelihood] Beta-binomial PDF for P(X|θ). Modeling how the data X will look like given the parameter θ.
# f(y) ~ Beta-Bin(n, alpha, beta)

ll <- function(alpha, beta) {
    -sum(dbetabinom.ab(data_by_state_veryliberal$count, 
                       data_by_state_veryliberal$polls_per_state,
                       alpha, beta,
                       log = TRUE))	# log = TRUE takes log of dbetabinom
}


mm <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B")

(alpha0 <- coef(mm)[1])
(beta0 <- coef(mm)[2])
cat("ML estimate: alpha0 = ", alpha0, "\n")
cat("ML estimate: beta0 = ", beta0, "\n")

# mean of Beta(alpha, beta)
mean_beta <- function(alpha, beta) {
    alpha / (alpha + beta)
} 

# histogram of all state level proportions of very liberal participants
# plotted together with Beta(alpha0, beta0) density
hist(data_by_state_veryliberal$mean,
     breaks = 10, col = "light blue",
     probability = TRUE,
     ylim = c(0,40),
     xlab = "Proportion of very liberal poll participants",
     main = "Informative prior estimated from the data" ) 
x <- seq(0,0.1, by = .001)

lines(x,dbeta(x, alpha0, beta0), lwd = 2, col = "darksalmon")
legend('topright', inset = 0.1,
       legend = paste0('Beta(', round(alpha0, 1), ', ', round(beta0,1), ')'),
       col = "darksalmon", lwd = 4)


# --- Posterior --- #

# posterior means on state level
data_by_state_veryliberal$posterior_means <-
    mean_beta(alpha0 + data_by_state_veryliberal$count,
              beta0 + data_by_state_veryliberal$polls_per_state - data_by_state_veryliberal$count)

# scatterplot: sample size vs posterior mean
p2 <- plot(data_by_state_veryliberal$polls_per_state,
           data_by_state_veryliberal$posterior_means,
           type = 'n',
           xlab = 'Number of participants',
           ylab = 'Ideological ground: Very liberal (proportion, posterior mean)',
           ylim = c(0,0.2))
text(data_by_state_veryliberal$polls_per_state,
     data_by_state_veryliberal$posterior_means,
     labels = data_by_state_veryliberal$state, cex= 0.5, pos = 1)


# Let's compare data scatter plot and posterior plot into one overall graph

par(mfrow=c(1,2))

plot(data_by_state_veryliberal$polls_per_state,
     data_by_state_veryliberal$mean,
     xlab = "Number of participants per state",
     ylab = "proportion of very liberals per state",
     col = "orange",
     main = "Presidential elections of USA 2008",
text(data_by_state_veryliberal$polls_per_state,
     data_by_state_veryliberal$mean,
     labels = data_by_state_veryliberal$state, 
     cex= 0.5, pos = 1))

plot(data_by_state_veryliberal$polls_per_state,
     data_by_state_veryliberal$posterior_means,
     type = 'n',
     xlab = 'Number of participants per state',
     ylab = 'Ideological ground: Very liberal (proportion, posterior mean)',
     ylim = c(0,0.10),
     main = "Posterior results, USA 2008")
    
text(data_by_state_veryliberal$polls_per_state,
     data_by_state_veryliberal$posterior_means,
     labels = data_by_state_veryliberal$state, 
     cex= 0.5, pos = 2, col = "blue")


"The observations are now scattered on a smaller region on y-axis.
We see that especially the locations of states with small participant numbers
have changed. The prior Beta(15.6, 317.5) is quite informative and therefore,
influences the estimates most when the amount of observations ni
is small. California stays almost at the same location while Washington D.C 
moved quite close to others"


