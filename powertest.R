rm(list=ls());

# Function to estimate the p value between cases and controls, where we have 10 cases and 10 controls. Each individual has n.replicas.
# The value of each individual i is computed as mean.status + error.status + error.individual.i
# mean.status is 0 for controls and mean.cases for cases
power.test <- function(mean.cases, sd.individual, sd.cases.controls, n.replicas)
{
  # create 10 cases from a normal distribution with mean = mean.cases and sd = sd.cases.controls
  cases <- rnorm(10, mean.cases, sd.cases.controls);
  # create 10 controls from a normal distribution with mean = 0 and sd = sd.cases.controls 
  controls <- rnorm(10, 0, sd.cases.controls);
  
  # create list of status
  status <- as.factor(c(rep("CASES", n.replicas * length(cases)), rep("CONTROLS", n.replicas * length(cases))));
  # create list of individuals
  individuals <- as.factor(sort(rep(1:(length(cases) + length(controls)), n.replicas)));
  # values of the variable to test
  response <- c();
  
  for(c in 1:length(cases))
  {
    response <- c(response,rnorm(n.replicas,cases[c], sd.individual));
  }
  for(c in 1:length(controls))
  {
    response <- c(response,rnorm(n.replicas,controls[c], sd.individual));
  }
  
  # Nested ANOVA: individuals within status
  nest <- aov(response ~ status / individuals)
  
  k <- summary(nest)           
  # pvalue
  return(k[[1]]$`Pr(>F)`[1]);
}

# Create a grid of values for each of the three parameters of interest
mean.cases <- seq(from = 0, to = 2, by = 0.1);
sd.individuals <- seq(from = 1, to = 10, by = 1);
sd.cases.controls <- seq(from = 1, to = 10, by = 1);
n.replicas <- 500;

# mean pvalue out of 100 datasets with the same parameters
meanpvalue <- c();

for(m in mean.cases)
{
  for(si in sd.individuals)
  {
    for(sc in sd.cases.controls)
    {
      pv <- 0;
      for(rep in 1:100)
      {
        pv <- pv + power.test(m,si,sc, n.replicas);
      }
      pv <- pv / 100;
      meanpvalue <- c(meanpvalue,pv);
    }
  }
}

# amount of times the pvalue is smaller than 0.05
mean(meanpvalue <= 0.05);
