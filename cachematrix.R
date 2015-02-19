## R Programming Assignment 2: 
## Lexical Scoping--caching the inverse of a matrix
## Matrix inversion is usually a time-consuming computation. 
## Therefore, caching the inverse of a matrix may be beneficial
## rather than computing it simultaneously.
## Following code is given for caching the inverse of a matrix.  

	## makeCacheMatrix function creates a special matrix object
	## that can cache its inverse, which is really a list containing
	## a function to:  
		## set the value of the matrix,
		## get the value of the matrix,
		## set the value of the inverse of the matrix,
		## get the value of the inverse of the matrix.
## My code for this assignment is given as follows:

    makeCacheMatrix <- function(Serdar=matrix()) {
    Caglak <- NULL           # This is where the result of inversion is stored
    set <- function(FSU) {
    Serdar <<- FSU
    Caglak <<- NULL          # It also initialises Caglak to null
    }
	get <- function() Serdar # Returns the input matrix
    setInverse <- function(inverse) Caglak <<-inverse # Set the inversed matrix
    getInverse <- function() Caglak  				          # return the inversed matrix
	  # return a list that contains these functions, so that we can use
    # makeCacheMatrix object like these
    # USA <- makeCacheMatrix(Serdar=matrix(a:b,c))
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
     }

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above, unless the inverse 
## has already been calculated, in such cases it retrieves it from
## the cache and prints "getting cached data". 

    cacheSolve <- function(Serdar, ...) { # get the inversed matrix from object Serdar
    Caglak <- Serdar$getInverse()
    if (!is.null(Caglak)) {               # if the inversion result is there 
    print("getting cached data")          # message "getting cached data"
    return(Caglak)                        # return the calculated inversion
    }
    Caglak <- solve(Serdar$get())         # if the inversion is not there, this solves it
    Serdar$setInverse(Caglak)             # then set it to the object
    Caglak                                # return the solved result
    }

  ## Following code is used to test the both makeCacheMatrix 
	## and cacheSolve Functions. Let`s use a 2x2 matrix called "Serdar". 
	## If the inverse of the matrix Serdar has already obtained, it gets
  ## the inverse of Serdar from cache and skips the computation; 
	## if not, it calculates the inverse of the matrix and sets the inverse
  ## in the cache via the setInverse function. 	

    USA <- makeCacheMatrix(Serdar=matrix(9:12,2))
    USA$get() 
    USA$getInverse()
    USA$set(matrix(1:4,2)) # to change matrix
    USA$get()              # to get the setted matrix
    cacheSolve(USA)
    cacheSolve(USA)        # to check if the message is delivered
    USA$getInverse()       # to set the inversed matrix
