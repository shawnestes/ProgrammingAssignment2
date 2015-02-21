## The concept is to provide a resemblence of caching functions to matrix values
## on an inverted matrix. The exercise is more about lexical scoping and 
## the use of <-- than caching as this would likely never be used in production code.

## While the code is unique, this solution was inspired by the example given in the assignment, 
## posts to the Coursera discussion forum. More formal testing was done using code provided by Jules Stuifbergen on the class forum.


## makeCacheMatrix: 
## Set the values of both the vector and the inverse matrix
## Return those values

makeCacheMatrix <- function(x = matrix()) {
  ## inverse matrix must be calculated if we're making the matrix
  inverse_matrix<-NULL
  
  set<-function(y){
    ## An example of lexical scoping. We want to change x to the new value of y passed along
    ## in the function.
    x<<-y
    ## Kill off the previous inversed matrix data in the environment. It's considered invalid.
    inverse_matrix<<-NULL
  }
  get<-function() { 
    x
  } 
  set_inverse_matrix<-function(inverse) { 
    inverse_matrix<<-inverse
  }
  get_inverse_matrix<-function() inverse_matrix  
  list(set=set, get=get,set_inverse_matrix=set_inverse_matrix,get_inverse_matrix=get_inverse_matrix)
}

## Tests if a cache can be used or not
## Returns cached inverse matrix or a solves/sets a new inverse matrix 

cacheSolve <- function(x, ...) {
  ## Attempt to use cached inverse matrix
  inverse_matrix<-x$get_inverse_matrix()
  if(!is.null(inverse_matrix)){
    message("Cache data has been used for this")
    return(inverse_matrix)
  }
  ## inverse_matrix is null, so we set it up using our matrix
  data<-x$get()
  ## create the inverse matrix, by solve()
  inverse_matrix <- solve(data)
  ## set the new value
  x$set_inverse_matrix(inverse_matrix)
  ## return what we have set
  inverse_matrix
}

## tests: 
## > x = rbind(c(1, -1/8), c(-1/8, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##  [,1]   [,2]
##  [1,]  1.000 -0.125
##  [2,] -0.125  1.000
## First run without cache
## > cacheSolve(m)
##  [,1]      [,2]
##  [1,] 1.0158730 0.1269841
##  [2,] 0.1269841 1.0158730
## Now the cache results
##  > cacheSolve(m)
##  Cache data has been used for this
##  [,1]      [,2]
##  [1,] 1.0158730 0.1269841
##  [2,] 0.1269841 1.0158730

## Identical, but let's have R show us that it is.

## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > original_m = makeCacheMatrix(x)
## > solved_1 = cacheSolve(original_m)
## > solved_2 = cacheSolve(original_m)
##  Cache data has been used for this
## > identical(solved_1, solved_2)
## [1] TRUE