#                   SIMULATION-BASED POWER ANALYSIS                           #
#                  Workshop 24.03.2021, OSF Frankfurt                         #
#                         Author:  Levi Kumle                                 #
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

if (!require("languageR")) {
  install.packages("languageR", dependencies = TRUE)}

# load packages
library(lme4)
library(mixedpower)
library(languageR)
# -------------------------------------------------------



# --------------------------------------------------------------------------- #
# INTRODUCTION TO SIMULATION-BASED POWER ANALYSES IWTH MIXEDPOWER

# In this script, we will explore the R-package mixedpower and the different 
# functions it brings!
#
# Let's pretend we are interested in whether native English speakers are able to 
# discriminate English words faster from non-words than non-native speakers in a 
# Lexical Decision Task. We also hypothesize that this effect is more pronounced 
# for common words and therefore also want to include the effect of word frequency 
# as well as the interaction of native language and word frequency
# in our study. We plan to analyze our study using a LMM (with subject and word (i.e. stimuli) 
# as random factors). 
# We now wonder how many subjects we need to test and how many words each subject 
# needs to react to in order to achieve at least 80% power. 

# We plan to conduct a simulation-based power analyses and therefore need a data set
# + LMM to inform the simulation. Luckily, we know that a similar data set exists in 
# the languageR package (Baayen, 2013) and therefore will use it as a basis of
# the following power simulation. The data set contains 21 subjects who each react
# to 79 words. 

# 1) First, we will load the data and fit a LMM corresponding to our planed design. 
# 2) Next, we will use the mixedpower() package to check what happens if we vary 
# sample size and the numbe of words.
# 3) Finally, we will vary both to try and find the combination of subject/words
# that works best for us as a researcher (while ensuring ample power).


# --------------------------------------------------------------------------- #
# 1. LOAD DATA AND FIT MODEL: 
# --------------------------------------------------------------------------- #

# just run the next couple of lines - nothing to do for us at this point. 

##### load data: 
tutorial_variables <- c("Subject", "RT", "NativeLanguage", "Word", "Frequency")
LEXDEC <- lexdec[tutorial_variables]


##### preprocess data in order to fit model: 

## transform RT
LEXDEC$RT = exp(LEXDEC$RT)
LEXDEC <- transform(LEXDEC, logRT=log(RT), speed=1000/RT )

## let's scale our fixed effects if we want to look at interactions
LEXDEC$Frequency.c <- scale(LEXDEC$Frequency, center=TRUE, scale=FALSE)
LEXDEC$NativeLanguage.c <- ifelse(LEXDEC$NativeLanguage == "English", -1/2, 1/2)

## mixedpower requires simvar to be numeric - let's create numeric dummy variables
# for both Subject and word
LEXDEC$subject <- sort(c(rep(1:21, 79)))
temp <- LEXDEC[with(LEXDEC, order(Word)), ]
temp$word <- sort(c(rep(1:79, 21)))
LEXDEC <- temp[with(temp, order(Subject)), ]

###### fit model with lme4: 

## this is the final model
SPEEDmodel <- lmer(speed ~ NativeLanguage.c * Frequency.c 
                   + (1 | subject) + (1 | word), data=LEXDEC)

## lets have a look at this model
summary(SPEEDmodel, corr = FALSE)



# --------------------------------------------------------------------------- #
# 2)  VARYING ONE RANDOM VARIABLE WITH mixedpower(): 
# --------------------------------------------------------------------------- #

# And now the fun begins! We will start with using mixedpower(). 
# For clarity, we will specify the parameters explicitly before we hand them to mixedpower() instead 
# of doing that in the function call directly. 
# Below you can find a few suggestions on which parameters you can change to explore different
# question and decide on a design that reaches at least 80% power - but feel free to play around if you have 
# different ideas. 

# A) The obvious: Explore power for different sample sizes (using the simvar and steps parameter).
#    --> remember: Each participant will react to 79 words because this is the amount of words
#        specified in the data set we use to inform the simulation. 
# B) What happens if we keep participants fix (21) and include different numbers of words? (simvar = words)
# C) How many participants/words do you need to reach power > 80? What if you aim for power > 90% ?
#    --> Which fixed effect do you base your decision on?
# C) Include a SESOI: How do your design choices change if you reduce all beta coefficients by 15%? (or 20%?)

# ------------------------------------------------- #
# specify parameters: 

# essential parameters: 
data <- LEXDEC # name of the data frame
model <- SPEEDmodel # name of the model we just fitted
fixed_effects <- c("NativeLanguage.c", "Frequency.c") # we need to specify them as character types

# ---------- #   
# YOUR TURN: 
# ---------- #  
simvar <- "insert simvar name here" # which random factor do we want to vary?
steps <- c(X,X,X) # which levels of this random variable do we want to include?

# A few words regarding the SESOI: Since we want to change the beta coefficients in our model, we need to 
# specify the new beta coefficients (including one for the intercept and all interactions!). Basically, each
# effect shown in the model summary needs to be assigned a new beta value. 

# Let's get the original beta coefficients first: 
(betas <- SPEEDmodel@beta) # the order corresponds to the order the effects are listed in the model summary
summary(SPEEDmodel, corr = F) # let's check if this is correct

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
                    critical_value = 2, n_sim = 50, # let's not kill your laptop.. 
                    SESOI = SESOI))                 #  if it is still taking too long feel free to change this to 20 or 30
#                                                     -> but your "real" simulation should include 1000 runs!

# mixedpower() and R2power() automatically saves the simulation results to file in your current working directory
# TIPP: if you run multiple simulations with different parameters make sure to go into your working directory
#       and change the name of the simulation result to something meaningful. Otherwise the results will be overwritten
#       by the next simulation. 



# --------------------------------------------------------------------------- #
# SET PARAMETERS OF SIMULATION FOR VARYING TWO RANDOM VARIABLE: 
# --------------------------------------------------------------------------- #

# Which combination of subjects/words did you settle on using mixedpower() and ensuring > 80% power ? 
# Maybe there is a more optimal combination that ensures power > 80% but let's us
# save resources or spares our subjects the pain of doing a long experiment?  (You know who you are...)
# Next, we will use the flexibility of R2power to find out.

# A) Again, start with exploring power for a range of sample sizes  - but this time let them  react
#    to  maybe 50 words.. or 100.. or 150? You decide. 
#    --> different scenario: you already compiled your stimulus list which consists of 70 words. How many
#        participants do you need? 
# B) Or: maybe we have funding to pay/time to test 30 (or 40) participants - explore power over a range 
#        of word numbers to find a combination that still ensures > 80% power. 
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

R2var <- "insert name of second random variable here" # word if simvar = subject, subject if simvar = word
R2level <- X # which level should the second random variable take


# SESOI: same as above
# Let's get the original beta coefficients first: 
(betas <- SPEEDmodel@beta) # the order corresponds to the order the effects are listed in the model summary

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


