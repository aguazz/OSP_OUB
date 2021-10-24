#### Description ----
# This function computes the Optimal Stopping Boundary Boundary (OSB) 
# that maximizes the mean of an Ornstein-Uhlenbeck Bridge (OUB)
#### Details ----
# A Picard's iteration algorithm is implemented to solve the free-boundary equation
# that characterizes the OSB. Details can be found in the companion paper
# Optimal Stopping of an Ornstein-Uhlenbeck bridge, 
# by D'Auria, B., García-Portugués E., and Guada A.
#### Arguments ----
# tol: tolerance to stop the Picard iterations
# z: pinning point
# a: slope of the OUB
# b: pulling level of the OUB
# v: volatility of the OUB
# t_eval: time discretization
# H: horizon of the OUB
# bnd: initial boundary to initialize the Picard's iteration
#### Functions ----
# Boundary computation:
OUB_boundary_picard <- function (tol = 1e-4, z = NULL, a = NULL, b = NULL, v = NULL, 
                                 t_eval = NULL, H = NULL, bnd = NULL, errors = FALSE) {
  
  #### Checking if the required arguments are provided...
  if (is.null(t_eval) || is.null(z) || is.null(a) || 
      is.null(b) || is.null(v) || is.null(H)) {
    
    stop("some of the required arguments are not provided")
    
  }
  
  z <- z - b
  
  #### Setting parameters... 
  N <- length(t_eval) - 1                  # number of subintervals
  delta <- t_eval[2:(N + 1)] - t_eval[1:N] # length step
  if (errors) er <- c()                    # errors vector if required
  if (is.null(bnd)) bnd <- rep(z, (N + 1)) # boundary if not given
  
  #### Computing the boundary...
  e <- 1  # error in the while loop
  # Fixed point algorithm
  while (e > tol) {
    
    bnd_old <- bnd
    
    for (i in (N-1):1) {  
      
      # Evaluate the kernel
      K <- OUB_kernel(t_eval[i], bnd_old[i], t_eval[(i + 1):N], bnd_old[(i + 1):N],
                  z, H, a, v)
      
      # Update the boundary at t[i]
      bnd[i] <- z - sum(K * delta[i:(N-1)])
      
    }
    
    # L_infty relative error
    e <- max(abs(bnd - bnd_old))
    if (errors) er <- c(er, e)
    
  }
  
  if (errors) return(list(bnd + b, er))
  
  return(bnd + b)
  
}

# Kernel of the free-boundary equation:
OUB_kernel <- function(present_time, present_space, future_time, future_space, 
                   z, H, a, v){
  
  m <- (present_space * sinh(a * (H - future_time)) + 
          z * sinh(a * (future_time - present_time))) /
    sinh(a * (H - present_time))
  vv <- sqrt((v^2 / a) * sinh(a * (H - future_time)) * 
               sinh(a * (future_time - present_time)) /
               sinh(a * (H - present_time)))
  
  NormDist <- pnorm((future_space - m) / vv, mean = 0, sd = 1, lower.tail = F)
  NormDens <- dnorm((future_space - m) / vv, mean = 0, sd = 1)
  
  K <- (z * NormDist - cosh(a * (H - future_time)) * (m * NormDist + vv * NormDens)) *
    (a / sinh(a * (H - future_time)))
  
  return(K)
  
}