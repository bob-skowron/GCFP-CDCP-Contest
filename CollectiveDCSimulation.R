##################################################################
########                                                 #########
########     MIT Golub Center for Finance and Policy     #########
########  Collective Defined Contribution Plan Contest   #########
########                                                 #########
##################################################################

# This R program provides a basic structure for a collective DC plan for the MIT GCFP crowd-sourced contest.
# Participants can access and modify the code, but certain assumptions must be maintained for all submitted entries.
# See contest instructions for further details.
# Objects with labels beginning with "user_" should be set to values chosen by contestants.
# Comment lines beginning with double hash marks ## indicate that user input is required.

# Note: Depending on the number of simulations you run and your computer's capabilities, it may take 30-60 minutes to execute this code.

########  BASIC PARAMETERS FOR CONTESTANTS TO CHOOSE  ########

# All contestants should choose the following parameters. Choosing these parameters constitutes a complete entry.
# Contestants wishing to submit a more complex or dynamic strategy may modify additional code,
# as explained in the Model Overview.

## Input simulation parameters 
user_Nyear <- 35  ## Choose the number of years to simulate
user_NMonte <- 1000  ## Choose the number of simulations to run. We recommend at least 10,000 for your final entry.

## Input plan return and reserve fund parameters 
user_rmin <- 0.04  ## Choose a minimum return/floor (when reserve assets are available)
user_rmax <- 0.1  ## Choose a maximum return/ceiling; surplus goes to reserve
user_multiple <- 2 ## Choose a multiple for capping reserve (e.g. 1 means capping reserve at 1 year's floor payment)

## Input investment strategy (a dynamic strategy will require additional code changes)
user_AssetShareStocks <- 0.6 ## Choose the proportion of assets you want to be stocks
user_AssetShareBonds <- 0.3 ## Choose the proportion of assets you want to be bonds
user_AssetShareAlternatives <- 1 - (user_AssetShareStocks + user_AssetShareBonds) # The remainder will be alternatives


########  INPUTS FOR ANALYSIS  ########  

# To see how your strategy performs for certain workers and years, choose the following inputs:

## Analyze outcomes for workers with tenures of your choosing
user_starting_age <- 27   ## Choose a starting age for the worker whose outcomes you want to check
user_separating_age <- 60 ## Choose a separating age for the worker whose outcomes you want to check

user_analysis_year <- 35 ## Choose a year for which you want to examine outcomes
# Note that user_analysis_year must be less than or equal to user_Nyear

# NOTE: That concludes all of the input required for a fixed investment strategy. If you want to
# create a more complex strategy, you may modify the code below within the bounds of the contest rules.

########  OTHER PARAMETERS  ########

# Fixed economic parameters
BaseWage <- 50000  # Nominal wage in 2018
RealWageGrth <- 0.005  # Real annual wage growth

SaveWorker <- 0.10 # Percent of wage that worker contributes to plan
SaveEmployer <- 0.10 # Percent of wage that employer contributes to plan

Annuity <- 0.04  # Annual cashflow per $ of annuity purchased

Nasset <- 3  # Number of asset classes (stocks, bonds, alternatives)
rf <- 0.025  # Annual nominal risk-free rate
Inflation <- 0.02 # Annual inflation rate

# Fixed plan parameters
NWorker <- 100000  # Workforce remains constant over time
AgeMinStartWork <- 25  # Youngest age of new workers at beginning of first work year
AgeNormalRetire <- 62  # Turn AgeNormalRetire + 1 after last year of work
RetireAge <- AgeNormalRetire - (AgeMinStartWork - 1)  # Max number of years in labor force
NyearFullBenefit <- 32  # Number of years of service to get full benefit treatment

rsep <- rf  # Fixed return for unvested separators (must be set to rf for main contest solution)

AssetShare <- c(user_AssetShareStocks, user_AssetShareBonds, user_AssetShareAlternatives)

BaseProbSeparate <- c(0.02, 0.02835, 0.0942)  # Annual separation rate for each age bracket (26-44, 45-54, 55-61)
BaseProbHire <- c(0.5, 0.25, 0.2, 0.05)  # Annual hiring rate for each age bracket (25-34, 35-44, 45-54, 55-61)

# Fixed investment parameters
ExpRet <- c(0.065, 0.03, 0.05)  # Nominal expected returns for stocks, bonds, alternatives
SD <- c(0.2, 0.098, 0.182)  # Standard deviations (volatility) for stocks, bonds, alternatives
TradingCost <- c(0.0005, 0.0005, 0.0015)  # Trading costs for stocks, bonds, alternatives


########  LOAD DATA  ######## 

# Load steady state distribution file 
SteadyDist <- read.csv("steady_state_dist.csv", header = FALSE) # Distribution of workers by current age and age of joining plan in the steady state
# If you encounter an error, confirm that "steady_state_dist.csv" is in your working directory.


########  SET-UP AND SIMULATIONS  (no user input required) ########  

# Convert correlation matrix to covariance matrix
Correlation <- matrix(c(1, -0.01, 0.54, -0.01, 1, 0.06, 0.54, 0.06, 1), nrow = 3, ncol = 3, byrow = TRUE)  # Correlation matrix for stocks, bonds, alternatives
D <- diag(SD)
Covariance <- D %*% Correlation %*% D  # Covariance matrix for stocks, bonds, alternatives

# Cholesky transformation for generating correlated random shocks
Cholesky <- matrix(t(chol(Covariance)), nrow = 3, byrow = TRUE)

count1 <- 0 # Setting up the tally of instances in which realized benefits < scheduled benefits
count2 <- 0 # Setting up the tally of instances in which realized benefits < 80% of scheduled benefits 
count3 <- 0 # Setting up the tally of instances in which realized benefits < 50% of scheduled benefits
prob1 <- 0  # Setting up proportion of instances in which realized benefits < scheduled benefits
prob2 <- 0  # Setting up proportion of instances in which realized benefits <  80% of scheduled benefits
prob3 <- 0  # Setting up proportion of instances in which realized benefits <  50% of scheduled benefits

tol <- 1e-5  # This tolerance prevents a floating point issue

RealizedBenefits <- NA
ReplaceRates <- NA
reserves <- matrix(NA, user_NMonte, user_Nyear)

# Initial seed for random number generator 
set.seed(123) # For replicability

# Start of Monte Carlo Loop 

for (n in 1:user_NMonte) {

# Create tables to save data
ProbTable <- array(0, dim = c(3))


# Initialize Asset Holdings, Payouts, Reserve, Replacement Rates, Lost Workers 
WorkerAssets <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
WorkerAssetsEmployer <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
WorkerAssetsEmployee <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
CohortAssets <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
FloorAssets <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))

PayoutRetirees <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
PayoutSeparators <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))

ReserveStock <- array(0, dim = c(user_Nyear))
ReplaceRate <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
LostWorkers <- array(0, dim = c(user_Nyear))
ScheduledBenefits <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))

# Input steady-state workers distribution at start year 
Workers <- array(0, dim = c(RetireAge + 1, RetireAge, user_Nyear + 1))
for (i in 1:RetireAge) {
  for (j in 1:RetireAge) {
        Workers[i, j, 1] <- SteadyDist[i, j] 
      }
}

  reserve <- 0  # Initialize the Reserve Fund at start year  
  
  # Start of Time Loop ------------------------------
  for (t in 1:user_Nyear) {
    LostWorker <- 0
    
    # Generate current year's returns ------------------------------
    normal_shock <- rnorm(Nasset)
    corr_shock <- Cholesky %*% normal_shock
    
    AnnualRet <- 0
    ret <- c(Nasset)
    for (i in 1:Nasset) {
      ret[i] <- ExpRet[i] + corr_shock[i]
      AnnualRet <- AnnualRet + AssetShare[i] * ret[i]
    }

    # Update accumulated assets ------------------------------
    rcohort <- min(AnnualRet, user_rmax)
    reserve <- reserve * (1 + rf)
    for (i in 1:RetireAge) {
      for (j in 1:i) {
        WorkerAssetsEmployer[i + 1, j, t + 1] <- WorkerAssetsEmployer[i, j, t] * (1 + rsep) + Workers[i, j, t] * BaseWage * SaveEmployer * (1 + RealWageGrth + Inflation) ^ (t - 1)
        WorkerAssetsEmployee[i + 1, j, t + 1] <- WorkerAssetsEmployee[i, j, t] * (1 + AnnualRet) + Workers[i, j, t] * BaseWage * SaveWorker * (1 + RealWageGrth + Inflation) ^ (t - 1)
        WorkerAssets[i + 1, j, t + 1] <- WorkerAssetsEmployer[i + 1, j, t + 1] + WorkerAssetsEmployee[i + 1, j, t + 1]
        
        CohortAssets[i + 1, j, t + 1] <- CohortAssets[i, j, t] * (1 + rcohort) + Workers[i, j, t] * BaseWage * (SaveWorker + SaveEmployer) * (1 + RealWageGrth + Inflation) ^ (t - 1)
        FloorAssets[i + 1, j, t + 1] <- FloorAssets[i, j, t] * (1 + user_rmin) + Workers[i, j, t] * BaseWage * (SaveWorker + SaveEmployer) * (1 + RealWageGrth + Inflation) ^ (t - 1)
        
        reserve <- reserve + max(AnnualRet - user_rmax, 0) * CohortAssets[i, j, t]
      }
    }
    
    # Cap reserve at a multiple of a year's floor payment ------------------------------
    SumCohortAssets <- 0
    SumFloorAssets <- 0
    for (i in 1:RetireAge) {
      ProbSeparate <- 0
      if (i > 1 & i < 20) {
        ProbSeparate <- BaseProbSeparate[1]
      } else if (i >= 20 & i < 30) {
        ProbSeparate <- BaseProbSeparate[2]
      } else if (i >= 30 & i <= 37) {
        ProbSeparate <- BaseProbSeparate[3]
      } else if (i == RetireAge) {
        ProbSeparate <- 1
      }
      
      for (j in 1:i) {
        if (Workers[i, j, t] > 0) {
          SumFloorAssets <- SumFloorAssets + FloorAssets[i + 1, j, t + 1] * ProbSeparate
        }
      }  
    }
    
    # Distribute bonus shares when available ------------------------------
    SumCohortAssets <- sum(CohortAssets[ , , t + 1])
    if (reserve > user_multiple * SumFloorAssets) {
      BonusShare <- CohortAssets[ , , t + 1] / SumCohortAssets
      CohortAssets[ , , t + 1] <- CohortAssets[ , , t + 1] + BonusShare * (reserve - user_multiple * SumFloorAssets)
      reserve <- user_multiple * SumFloorAssets
    }
    
    # Distribution phase ------------------------------
    for (i in 1:RetireAge) {
      ProbSeparate <- 0
      if (i > 1 & i < 20) {
        ProbSeparate <- BaseProbSeparate[1]
      } else if (i >= 20 & i < 30) {
        ProbSeparate <- BaseProbSeparate[2]
      } else if (i >= 30 & i <= 37) {
        ProbSeparate <- BaseProbSeparate[3]
      } else if (i == RetireAge) {
        ProbSeparate <- 1
      }

      for (j in 1:i) {
        LostWorker <- LostWorker + Workers[i, j, t] * ProbSeparate

        if (t == 1) {  # Benchmark wage when calculating replacement rate
          WageReplace <- BaseWage
        } else if (t == 2) {
          WageReplace <- (BaseWage + BaseWage * (1 + RealWageGrth + Inflation)) / 2
        } else if (t == 3) {
          WageReplace <- (BaseWage + BaseWage * (1 + RealWageGrth + Inflation) + BaseWage * (1 + RealWageGrth + Inflation) ^ 2) / 3
        } else if (t == 4) {
          WageReplace <- (BaseWage + BaseWage * (1 + RealWageGrth + Inflation) + BaseWage * (1 + RealWageGrth + Inflation) ^ 2 + BaseWage * (1 + RealWageGrth + Inflation) ^ 3) / 4
        } else if (t >= 5) {
          WageReplace <- (BaseWage * (1 + RealWageGrth + Inflation) ^ (t - 5)) / 5 * ((1 + RealWageGrth + Inflation) ^ 5 - 1) / (RealWageGrth + Inflation)  # The average wage over the last 5 years of employment
        }
        
        if ((i - j) >= NyearFullBenefit) {  # Workers with sufficient tenure get full benefits at separation or retirement
          Floor <- min(reserve + CohortAssets[i + 1, j, t + 1] * ProbSeparate, FloorAssets[i + 1, j, t + 1] * ProbSeparate)
          if (Workers[i, j, t] * ProbSeparate > 0) {
            if(i < RetireAge) {  # Payout for vested separators
              ScheduledBenefits[i + 1, j, t + 1] <- FloorAssets[i + 1, j, t + 1] / Workers[i, j, t]
              PayoutSeparators[i + 1, j, t + 1] <- max(CohortAssets[i + 1, j, t + 1] * ProbSeparate, Floor) / (Workers[i, j, t] * ProbSeparate)
              reserve <- reserve - max(PayoutSeparators[i + 1, j, t + 1] * Workers[i, j, t] * ProbSeparate - CohortAssets[i + 1, j, t + 1] * ProbSeparate, 0)
              ReplaceRate[i + 1, j, t + 1] <- PayoutSeparators[i + 1, j, t + 1] * Annuity / WageReplace
            } else {   # Payout for vested retirees
              PayoutRetirees[i + 1, j, t + 1] <- max(CohortAssets[i + 1, j, t + 1], Floor) / Workers[i, j, t]
              reserve <- reserve - max(PayoutRetirees[i + 1, j, t + 1] * Workers[i, j, t] * ProbSeparate - CohortAssets[i + 1, j, t + 1] * ProbSeparate, 0)
              ReplaceRate[i + 1, j, t + 1] <- PayoutRetirees[i + 1, j, t + 1] * Annuity / WageReplace
            }
          }
        } else {  # Those without sufficient tenure only have a claim to WorkerAssets
          reserve <- reserve + (CohortAssets[i + 1, j, t + 1] - WorkerAssets[i + 1, j, t + 1]) * ProbSeparate # Reserve gets/gives excess funds
          hold <- 0
          if (reserve < 0) {
            hold <- -reserve
            reserve <- 0  
          }
          if (Workers[i, j, t] * ProbSeparate > 0) {
            if (i < RetireAge) {
              PayoutSeparators[i + 1, j, t + 1] <- (WorkerAssets[i + 1, j, t + 1] * ProbSeparate - hold) / (Workers[i, j, t] * ProbSeparate) # Per capita payout reduced if reserve insufficient
              ReplaceRate[i + 1, j, t + 1] <- PayoutSeparators[i + 1, j, t + 1] * Annuity / WageReplace
            } else {
              PayoutRetirees[i + 1, j, t + 1] <- (WorkerAssets[i + 1, j, t + 1] * ProbSeparate - hold) / (Workers[i, j, t] * ProbSeparate) # Per capita payout reduced if reserve insufficient
              ReplaceRate[i + 1, j, t + 1] <- PayoutRetirees[i + 1, j, t + 1] * Annuity / WageReplace
            }
          }
        }
        CohortAssets[i + 1, j, t + 1] <- CohortAssets[i + 1, j, t + 1] * (1 - ProbSeparate)  # Assets of separated workers taken off books
        WorkerAssetsEmployer[i + 1, j, t + 1] <- WorkerAssetsEmployer[i + 1, j, t + 1] * (1 - ProbSeparate)  # Assets of separated workers taken off books
        WorkerAssetsEmployee[i + 1, j, t + 1] <- WorkerAssetsEmployee[i + 1, j, t + 1] * (1 - ProbSeparate)  # Assets of separated workers taken off books
        FloorAssets[i + 1, j, t + 1] <- FloorAssets[i + 1, j, t + 1] * (1 - ProbSeparate)  # Assets of separated workers taken off books
        Workers[i + 1, j, t + 1] <- Workers[i, j, t] * (1 - ProbSeparate)  # Remaining workers from this cohort going into next year
      }
    }
    
    # Replace separating and retiring workers at start of t + 1 according to fixed new hire rules ------------------------------
    for (i in 1:(RetireAge - 1)) {
      if (i <= 10) {
        Workers[i, i, t + 1] <- Workers[i, i, t + 1] + BaseProbHire[1] * LostWorker / 10
      } else if(i > 10 & i <= 20) {
        Workers[i, i, t + 1] <- Workers[i, i, t + 1] + BaseProbHire[2] * LostWorker / 10
      } else if(i > 20 & i <= 30) {
        Workers[i, i, t + 1] <- Workers[i, i, t + 1] + BaseProbHire[3] * LostWorker / 10
      } else if(i > 30 & i <= 37) {
        Workers[i, i, t + 1] <- Workers[i, i, t + 1] + BaseProbHire[4] * LostWorker / 7 
      }
    }
    
    # Record annual results ------------------------------
    ReserveStock[t] <- reserve
    LostWorkers[t] <- LostWorker
  }
  count1 <- count1 + (ScheduledBenefits[user_separating_age - 24, user_starting_age - 24, user_analysis_year] > PayoutSeparators[user_separating_age - 24, user_starting_age - 24, user_analysis_year] + tol)
  count2 <- count2 + (0.8 * ScheduledBenefits[user_separating_age - 24, user_starting_age - 24, user_analysis_year] > PayoutSeparators[user_separating_age - 24, user_starting_age - 24, user_analysis_year] + tol)
  count3 <- count3 + (0.5 * ScheduledBenefits[user_separating_age - 24, user_starting_age - 24, user_analysis_year] > PayoutSeparators[user_separating_age - 24, user_starting_age - 24, user_analysis_year] + tol)
  RealizedBenefits[n] <-  PayoutSeparators[user_separating_age - 24, user_starting_age - 24, user_analysis_year]
  ReplaceRates[n] <- ReplaceRate[user_separating_age - 24, user_starting_age - 24, user_analysis_year]
  reserves[n,] <- ReserveStock
}

########  RESULTS  ########

shortfall <- ScheduledBenefits[user_separating_age - 24, user_starting_age - 24, user_analysis_year] - RealizedBenefits

prob1 <- count1 / user_NMonte # Probability that realized benefit falls short of scheduled benefit
prob2 <- count2 / user_NMonte # Probability that realized benefit falls short of 80% of scheduled benefit
prob3 <- count3 / user_NMonte # Probability that realized benefit falls short of 50% of scheduled benefit

ProbTable[1] <- prob1
ProbTable[2] <- prob2
ProbTable[3] <- prob3

rownames(ProbTable) <- c("Realized < Scheduled", "Realized < 80% Scheduled", "Realized < 50% Scheduled") # Create labels for probabilities
print(ProbTable) # Returns probabilities with labels for contest risk scenarios

summary(RealizedBenefits) # Summary statistics about realized benefits for specified worker in specified year for all simulations

summary(RealizedBenefits*Annuity) # Summary statistics about annuitized realized benefits for specified worker in specified year for all simulations

summary(ReplaceRates) # Summary statistics about replacement rate, the ratio of annuitized benefits to the the worker's average annual income from the last 5 years of work

summary(shortfall) # Summary statistics about size of shortfall for specified worker in specified year for all simulations

summary(reserves[,user_analysis_year]) # Summary statistics about the size of the Reserve Fund for all simulations
