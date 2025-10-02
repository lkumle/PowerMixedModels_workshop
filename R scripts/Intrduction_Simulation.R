# --------------------------------------------------------------------------- #
#                   SIMULATION-BASED POWER ANALYSIS                           #
#                  Workshop 24.03.2021, OSF Frankfurt                         #
#                         Author:  Levi Kumle                                 #
# --------------------------------------------------------------------------- #

# prepare ------------------------------------------------
rm(list = ls())
set.seed(2403)

# install all packages if not already available
if (!require("lsr")) {
  install.packages("lsr", dependencies = TRUE)}

if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)}

library(lsr)
library(ggplot2)
# -------------------------------------------------------


# --------------------------------------------------------------------------- #
# INTRODUCTION TO SIMULATION-BASED POWER ANALYSES

# What to do with this script: 
# 1. please first go through it leaving all parameters as they are 
#     - make sure you understand the general logic of the script
#     - have a look at the results
# 2. your turn! start varying the parameters of the simulation and observe what happens
#     - try to make predictions: how will your change influence power (direction? magnitude?)
#        --> you can even do this alone or with the members of your breakout room
#     - also compare with closed caption formula - can it pick up the changes you made to the data? 
#     - which effect sizes are you able to detect? 


# ------- IDEAS FOR CHANGING PARAMETERS --------
# 1) Difference in means (mean_G1, mean_G2)
# 2) Variance of groups (what happens if we change one of them? or both at the same time?) (SD_G1, SD_G2)
# 3) alpha level
# 4) Sample size: 
#     - overall smaller sample size (balanced groups)
#     - overall larger sample size (balanced groups)
#     - unbalanced groups! (make one group big and one small, try different levels of unbalanced)
# 5) changing the means back to 100 and 105 (sd = 10 for both groups)... 
#     - which sample size(s) do you need to choose for power > 80%? 
#     - which effect sizes are you able to detect? 
#     - what if the true means of both groups is 100 and 104.. is the sample size you selected still enough to achieve 80% power?
# 6) can you achieve power = 0? (or power = 1?)
# 7) If non of your simulation parameters resulted in low power (< 20%) so far - try underpowering your design
#    and observe the effect it has on the detected effect sizes

# --------------------------------------------------------------------------- #


# Scenario: 
# You plan a study investigation the effect of your newly developed App "BrainJogging".
# You plan to measure the IQ of two groups, one of them played "BrainJogging" on 
# their phone for a month before you measure IQ, the other one did not. 
# (And for the next 15 minutes we will assume that this is a sound research design.)


# --------------------------------------------------------------------------- #
# SET PARAMETERS OF SIMULATION: 
# --------------------------------------------------------------------------- #

# --------  CHANGE PARAMETERS HERE: -----
# (--> don't forget to also run the lines after you changed the values!)

# --- GROUP 1: ---
mean_G1 <- 100  # mean IQ in group 1
sd_G1   <- 10   # sd of IQ in group 1
n_G1    <- 20   # sample size group 1

# --- GROUP 2: ---
mean_G2 <- 102  # mean IQ in group 2
sd_G2   <- 10   # sd of IQ in group 2
n_G2    <- 20  # sample size group 2

# alpha:
alpha <- 0.05

#--- INSPECT DATA ---
# what is the true effect size? (this is still an estimate! - but a good one)
(true_d <- cohensD(rnorm(1000000, mean_G1, sd_G1), rnorm(1000000, mean_G2, sd_G2)))


# --------------------------------------------------------------------------- #
# SIMULATION
# --------------------------------------------------------------------------- #
nsim <- 1000 # how many simulations do we want to run? 1000 is a good start! 

# --------------------------------- #
# ALWAYS RE-RUN THIS WHOLE SECTION! 
# --------------------------------- #
pvalues <- numeric(nsim)  # prepare storing p values
cohensd <- numeric(nsim)  # also store cohens d of each simulation

# simulate experiment nsim times: 
# --> This is "step 1 and 2" of the simulation process! 
# (remember the figure on the last slide?)
for(i in 1:nsim){
  
  # Simulate data: Sample from normal distributions with the parameters of both groups!
  data_1 <- rnorm(n_G1, mean_G1, sd_G1) # we know that IQ follows a normal distribution, so we sample from one!
  data_2 <- rnorm(n_G2, mean_G2, sd_G2)
  
  # perform test
  result <-t.test(data_1, data_2) #perform the t-test 
  pvalues[i]<-result$p.value # get the p-value and store it
  
  # also store cohensD
  cohensd[i] <- cohensD(data_1, data_2)
}

# --------------------------------------------------------------------------- #
# RESULTS
# --------------------------------------------------------------------------- #

# --- SIMULATED POWER --- 
(sim_power <- (sum(pvalues < alpha)/nsim))
# significant (pvalue < alpha) simulations divided by all simulations (nsim)
# This is "Step 3" of the simulation process

# --- COMPARE WITH CLOSE CAPTION FORMULA ---
# ? Can you compute power with this formula for the data you just created ? 
power.t.test(n = n_G1, # observations per group
             delta = (mean_G1-mean_G2), # true difference between means
             sd = sd_G1, # standard deviation
             sig.level = alpha)$power


# --- LET'S HAVE A CLOSER LOOK AT SIMULATED RESULTS --
results <- data.frame(pvalues = pvalues, 
                      cohensD = cohensd, 
                      detected = ifelse(pvalues < alpha, "detected", "not detected"))

# PVALUES
# get distribution of simulated pvalues:
# --> these are the results of our simulated studies, the red line is our alpha level
ggplot(results, aes(pvalues))+
  geom_histogram(bins = 50) +
  geom_vline(xintercept = alpha,linetype="dashed", color = "red")

# EFFECT SIZES
# get distribution of cohens D --> red line is "true effect" size
# --> which effects were we able to detect, which not?
# --> "detect" = effect sizes from significant simulations
# --> "not detected" = effect ssizes found in not significant simulations
ggplot(results, aes(cohensD))+ 
  geom_histogram(bins = 50) +
  facet_grid(rows = vars(detected)) +
  geom_vline(xintercept = true_d,linetype="dashed", color = "red")

# --> compare this to the "true" effect size and remember how 
# smallest effect size we were able to detect?
min(results[results$detected == "detected",]$cohensD)

# average effect size we detected
mean(results[results$detected == "detected",]$cohensD)


