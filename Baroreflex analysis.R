#------------------ Script for baroreflex analysis

# Load packages:

library(segmented)       # For piecewise regressions
library(minpack.lm)      # For logistic regressions

#------------------

# For the piecewise regression, it is necessary first to fit the data to a linear regression 

# Fitting the linear regression:

linear_model <- lm(y ~ x, data = YourDataFrame)

# Results:
summary(linear_model)    # Results of the linear regression


# Fitting the piecewise regression

piecewise_model <- segmented(linear_model, 
                              seg.Z = ~x, 
                              psi = c(1, 2))


# The argument "seg.Z" determines the axis of reference for the breakpoints
# The argument "psi" determines how many breakpoints will be calculated. This is done by concatenating proximate values for the different breakpoints. 



#Results:
summary(piecewise_model)    # Results of the piecewise regression
slope(piecewise_model)      # Informs the slope for each segment separately
piecewise_model$psi         # Returns the breakpoints calculated by the piecewise regression



#------------------

# Symmetric logistic regression - equation 1 (Kent et al., 1971) 

symmetric_model <- nlsLM(y ~ d+(a-d)/(1+(exp(b*(x-c)))), 
                         data = YourDataFrame, 
                         start = list(a = 300, b = 1, c = 5, d = 100),
                         control = nls.lm.control(maxiter = 1024,
                                                  maxfev = 10000*(length(par) + 1)))


summary(symmetric_model)   # Results of the logistic regression
coef(symmetric_model)      # Returns the coefficients of the logistic regression


# Asymmetric logistic regression - equation 2 (Rodbard, 1974) 

asymmetric_model <- nlsLM(y ~ d+(a-d)/(1+(x/c)^b), 
                          data = YourDataFrame, 
                          start = list(a = 300, b = 6, c = 5, d = 100),
                          control = nls.lm.control(maxiter = 1024,
                                                   maxfev = 10000*(length(par) + 1)))


summary(asymmetric_model)   # Results of the logistic regression
coef(asymmetric_model)      # Returns the coefficients of the logistic regression

# To run the function "nlsLM()", it is necessary to write the equation to be used to fit the data. Equation 1: y ~ d+(a-d)/(1+(exp(b*(x-c)))); Equation 2: y ~ d+(a-d)/(1+(x/c)^b)

# "start" is necessary for all non-linear functions. It establishes initial values for testing the fit for each parameter (in the present case, parameters "a", "b", "c" and "d" of the logistic equation). Though the values do not need to be precise, those that are too far from the actual values will cause errors and will need to be corrected.

# "maxiter" establishes the maximum interactions the program will test to find the best fit. 

# "maxfev" indicates the number of functions evaluated and is a measure of the velocity of function convergence (maxfev = "max function evaluation").



#------------------

# The Akaike information criterion (AIC), compares the least square from different models (the lower values indicate the best fit).

AIC(linear_model,piecewise_model, asymmetric_model,asymmetric_model)