#                   SIMULATION-BASED POWER ANALYSIS                           #
#                  Workshop 24.03.2021, OSF Frankfurt                         #
#                         Author:  Leah Kumle                                 #
# --------------------------------------------------------------------------- #

# prepare ------------------------------------------------
rm(list = ls())

# install all packages if not already available
if (!require("lme4")) {
  install.packages("lme4", dependencies = TRUE)}

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}

if (!require("mixedpower")) {
  devtools::install_github("DejanDraschkow/mixedpower")}

# load packages
library(lme4)
library(mixedpower)

# -------------------------------------------------------



# --------------------------------------------------------------------------- #
# INTRODUCTION TO SIMULATION-BASED POWER ANALYSES IWTH MIXEDPOWER

# In this script, we will explore the R-package mixedpower and the different 
# functions it offers!

# In order to base your simulation on your own data/ model you will need to load your data into this 
# R environment and then fit your model on it. Maybe you are planing an experiment which follows
# roughly the same setup of an experiment you or someone else already conducted?
# You can either copy paste your data/model pipeline into this R script 
# (below in the section "LOAD DATA AND FIT MODEL") or open 
# your own data analysis script and run the the model there. 

# 1) First, we will load the data and fit a LMM corresponding to our planed design. 
# 2) Next, we will use the mixedpower() package to check what happens if we vary 
# sample size and the numbe of words.
# 3) Finally, we will vary both to try and find the combination of subject/words
# that works best for us as a researcher (while ensuring ample power).


# --------------------------------------------------------------------------- #
# 1. LOAD DATA AND FIT MODEL: 
# --------------------------------------------------------------------------- #

# load data: 


# fit model

# --------------------------------------------------------------------------- #
# 2)  VARYING ONE RANDOM VARIABLE WITH mixedpower(): 
# --------------------------------------------------------------------------- #

# And now the fun begins! We will start with using mixedpower(). 
# For clarity, we will specify the parameters explicitly before we hand them to mixedpower()
# Below you can find a few suggestions on which parameters you can change to explore different
# question and decide on a design that reaches at least 80% power:

# A) The obvious: Explore power for different sample sizes (using the simvar and steps parameter).
#    --> remember: all other levels of your other random variables will stay the same (i.e. number of trial each 
#                  subject reacts to) 
# B) What happens if we keep participants fix and include different numbers of trials?
# C) How many participants/trials do you need to reach power > 80? What if you aim for power > 90% ?
#    --> Which fixed effect do you base your decision on?
# C) Include a SESOI: How do your design choices change if you reduce all beta coefficients by 15%? (or 20%?)

# ------------------------------------------------- #
# specify parameters: 

# essential parameters: 
data <- # enter name of your data here
model <-  # enter name of your model here
fixed_effects <- c("fixef1", "fixef2") # enter all your fixed effects here

# ---------- #   
# YOUR TURN: 
# ---------- #  
simvar <- "insert simvar name here" # which random factor do we want to vary?
steps <- c(X,X,X) # which levels of this random variable do we want to include?

# A few words regarding the SESOI: Since we want to change the beta coefficients in our model, we need to 
# specify the new beta coefficients (including one for the intercept and all interactions!)

# Let's get the original beta coefficients first: 
(betas <- model@beta) # the order corresponds to the order the effects are listed in the model summary
summary(model, corr = F)

# now we can change those values! let's make them 15% smaller! (but feel free to also change them otherwise)
betas_smaller <- betas*0.85

# default in mixedpower() is SESOI = F and will not include a SESOI in the simulation. Assigns your betas_smaller 
# to SESOI to include it in the simulation
SESOI <- F 
            
# ------------------------------------------------- #
# run:
(power <- mixedpower(model = model, data = data,
                    fixed_effects = fixed_effects,
                    simvar = simvar, steps = steps,
                    critical_value = 2, n_sim = 50, # let's not kill your laptop
                    SESOI = SESOI))                 #  -> but your "real" simulation should include 1000 runs!

# mixedpower() and R2power() automatically saves the simulation results to file in your current working directory
# TIPP: if you run multiple simulations with different parameters make sure to go into your working directory
#       and change the name of the simulation result to something meaningful. Otherwise the results will be overwritten
#       by the next simulation. 



# --------------------------------------------------------------------------- #
# SET PARAMETERS OF SIMULATION FOR VARYING TWO RANDOM VARIABLE: 
# --------------------------------------------------------------------------- #

# Which combination of subjects/trials did you settle on using mixedpower() ? 
# Maybe there is a more optimal combination that ensures power < 80% but let's us
# save resources or spares our subjects the pain of doing a long experiment. (You know who you are...)
# Next, we will use the flexibility of R2power to find out.

# A) Again, start with exploring power for a range of sample sizes  - but this time they maybe react
#    to 50 trials or 100.. or 150? You decide. 
# B) Or: maybe we have funding to pay/time to test 30 (or 40) participants - explore power over a range 
#        of trial numbers to find a combination that ensures > 80% power. 
# c) And of course: Also include a SESOI to see how your decisions would change if you base your simulations on
#    different effect sizes.

# ------------------------------------------------- #
# specify parameters: 

# model, data and fixed effects stay the same so we don't need to specify them again! 

# ---------- #   
# YOUR TURN: 
# ---------- #  
simvar <- "insert simvar (name of first random variable) name here" # which random factor do we want to vary?
steps <- c(X,X,X) # which levels of this random variable do we want to include?

R2var <- "insert name of second random variable here"
R2level <- X # which level should the second random variable take


# SESOI: same as above
# Let's get the original beta coefficients first: 
(betas <- model@beta) # the order corresponds to the order the effects are listed in the model summary

# now we can change those values! let's make them 15% smaller! (but feel free to also change them otherwise)
betas_smaller <- betas*0.85

# default in mixedpower() is SESOI = F and will not include a SESOI in the simulation. Assigns your betas_smaller 
# to SESOI to include it in the simulation
SESOI <- F 

# ------------------------------------------------- #
# run:
(power <- R2power(model = model, data = data,
                     fixed_effects = fixed_effects,
                     simvar = simvar, steps = steps,
                     R2var = R2var, R2level = R2level, 
                     critical_value = 2, n_sim = 30, # let's not kill your laptop
                     SESOI = SESOI))                 #  -> but your "real" simulation should include 1000 runs!

# R2power() takes a bit longer to run... that is because we basically have to simulate twice: we first have to change the number of one
# random variable and then also simulate according to the second one! So for test purposes we will only include 30 runs!


