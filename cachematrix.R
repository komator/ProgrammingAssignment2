## These code implements two functions related to the Programming Assignment 2: Lexical Scoping.
## The main idea is to create a pair of functions that cache the inverse of a matrix. 

#' This function creates a special "matrix" object that can cache its inverse
#'
#' @param  x Input matrix
#'
#' @return A list containing four functions to set and get the value of the matrix and its inverse
#

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  # Define a function to set the value of the matrix and to clear the inverse (if cached)
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  # Define a function to get the value of the matrix
  get <- function() x
  
  # Define a function to set the inverse
  setInverse <- function(solve) m <<- solve
  
  # Define a function to get the inverse
  getInverse <- function() m
  
  # Return the list with the 4 functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#' should retrieve the inverse from the cache.
#'
#' @param  x a special matrix created with makeCacheMatrix
#'
#' @return The inverse of the matrix created with makeCacheMatrix
#

cacheSolve <- function(x, ...) 
{
    # Check if we already have the inverse cached
    inv <- x$getInverse()
    
    # Inverse already calculated
    if (!is.null(inv))
    {
      message ("Getting cached data")
    }
    
    # Inverse not calculated
    else 
    {
      # Get the matrix
      mat <- x$get()
        
      # Calculate the inverse
      inv <- solve (mat,...)
        
      # Cache the result
      x$setInverse(inv)
    }
    
    # Return the inverse
    inv
}
