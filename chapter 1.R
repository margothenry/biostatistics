#Exercise 1.1
  #poisson
  #binomial

#Excercise 1.2
  dbinom( 2, 10, 0.3) 
  sum(dbinom(0:2, 10, 0.3))
  pbinom( 2, 10, 0.3)
  
#Exercise 1.3
  poisson_max_fucntion = function(n, m, lambda) {
    prob = 1 - ppois( m-1, lambda )^n
    return(prob)
  }
  # check that the function works with an example
  poisson_max_fucntion(100, 7, 2)
  
#Exercise 1.4
  poisson_max_fucntion = function(n = 100, m = 1, lambda = 5 ) {
    prob = 1 - ppois( m-1, lambda )^n
    return(prob)
  } 
  poisson_max_fucntion()
#Exercise 1.5
  poisson_max_fucntion(100, 9, 0.5)

#Exercise 1.6
  
  
#Exercise 1.7  
  rv = rpois( 100, 3)
  mean(rv)
  var(rv)
  
#Exercise 1.8 
  #A
  
  #B