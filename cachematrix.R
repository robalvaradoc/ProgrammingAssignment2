rm(list = ls(all.names = TRUE))    #clears everything from the environment
makeCacheMatrix <- function(x = matrix()) { #Set the m and x variables
  m <- NULL
  set <- function(y){   
    x <<- y               #Assign the input argument to the x object in the parent environment
    m <<- NULL            #Assign the value of NULL to the m object in the parent environment.
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse  #Sets the inverse of the matrix
  getInverse <- function() m                     #Gets the inverse of the matrix
  list(set = set, get = get,                     #Sets each functions of the MakeCacheMatrix as an element in the list
       setInverse = setInverse, 
       getInverse = getInverse)
}

##Please include your own comment to explain your code (Required in Rubric)

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()             #calculates the inverse
  if(!is.null(m)){                #checks if the inverse of m is alreade calculated
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)             #calculates the inverse of the matrix
  x$setInverse(m)                 #sets the inverse of the matrix of makeCacheMatrix
  m
}


### Tested wit this values##
#a<-makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))a
#cacheSolve(a)
