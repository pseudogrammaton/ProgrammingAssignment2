## FUNCTION (object) makeCacheMatrix(): 
#   f() set/get nxn matrix
#   f() set/get "inverted" nxn matrix (setSolve()/getSolve())
#   inits runtime properties
makeCacheMatrix <- function(pmnOrigMatrix = matrix() ) {
  
  mInvert <- NULL
  
  set <- function( pmnSetMatrix = matrix(), ... ) {
    pmnOrigMatrix <<- pmnSetMatrix
    mInvert<<- NULL
    # this.get()
  }
  
  # return orig. matrix
  get <- function() pmnOrigMatrix
  
  # set invert matrix RT property & get it
  setSolve <- function(pmInvert) mInvert <<- pmInvert
  # return "solve()d" inverted matrix
  getSolve <- function() mInvert
  
  # publish methods()
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
} # end object


cacheSolve <- function(pObject, ...) {
  # init variable
  mInvert <- pObject$getSolve()
  
  # if result already in runtime object, just display & return
  if(!is.null( mInvert )) {
    message("getting cached data")
    return( mInvert ) # bye
  } # end if
  
  # get matrix from rt obj get() method
  mnOrigMatrix <- pObject$get()
  
  # get result from operation
  mInvert <- solve(mnOrigMatrix, ...)
  
  # set the rt obj property
  pObject$setSolve(mInvert)
  
  #  output result
  mInvert
} # end function

## 
## Example usage:
#  source('C:/shared/projR/ProgrammingAssignment2/z-cachematrix-02.R')
#  m2x2Matrix = rbind(c(1, -1/4), c(-1/4, 1))
#  oCacheMatrix = makeCacheMatrix(m2x2Matrix)
#  oCacheMatrix$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
#  cacheSolve(oCacheMatrix)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
#  oCacheMatrix$getSolve()
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.06
#  det( oCacheMatrix$get())
## [1] 0.9375
#  det(oCacheMatrix$getSolve())
## [1] 1.066667
#  solve(det(oCacheMatrix$getSolve()))
## [,1]
## [1,] 0.9375


