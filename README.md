# bayes-trials

Bayesian Data Analysis on 2008 Presidential elections of USA

Inspecting the relationship between states with high proportion of voters identified as very liberal, and the
amount of Obama voters. Using pre-election polling data and election results.

First, the only outlier seems to be Washington DC, where the proportion of very liberals is around 0.12 , and the proportion of Obama voters around 0.9. In the other states, the amount of Obama voters seems to be between 50%-60% of all voters, and the amount of ”very liberals” mostly between 2% and 10% of all participants.

Using empirical Bayesian model, we see that especially the locations of states with small participant numbers
have changed. The prior Beta(15.6, 317.5) is quite informative and therefore influences the estimates most when the amount of observations
is small. California (CA) stays almost at the same location while Washington D.C (DC) moved quite close to others.


Source: Exercise in 2.21 from Gelman, et.al: Bayesian Data Analysis, 3rd edition
Data source: http://www.stat.columbia.edu/~gelman/book/data/
