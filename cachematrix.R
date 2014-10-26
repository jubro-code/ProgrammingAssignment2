# creates a list object that contains the methods for:
# get and set a matrix m; get and set a the inverse matrix of m
# input m should be an invertable matrix
makeCacheMatrix <- function(m = matrix()){
  mi <- NULL
  
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  
  get<-function() m
  
  getmi <- function() mi 
  
  setmi <- function(mainverse) mi <<- mainverse 
  
  list(set=set, get=get, getmi=getmi, setmi=setmi)
}

# gets the inverse matrix by computing or getting from cache
# input obj should to be a makeCacheMatrix list object
cacheSolve <- function(obj,..){
  mainverse <- obj$getmi()
  if(!is.null(mainverse)){
    print("getting cached matrix")
    return(mainverse)
  }
  mainverse <- solve(obj$get(),..)
  obj$setmi(mainverse)
  ## Return a matrix that is the inverse of 'x'
  mainverse
}
