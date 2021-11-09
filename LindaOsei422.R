##There are two functions makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for non square and squared matrices 
library(MASS)
makeCacheMatrix <- function(y = matrix()){
  inv<-NULL            #making the inverse as NULL 
  set<- function(z){
                   y<<-z
                   inv<<-NULL
                   }
  get<-function()y                 #This is the function to get matrix y
  setinv<- function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(y)
                    inver%*%y      #function to obtain the inverse of the matrix
                    }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##Short comments describing this function
##This function is used to get the cache data

cachesolve <- function(y, ....) ##gets cache data
  {
  inv<-y$getinv()
  if(!is.null(inv)){               #checking if inverse is NULL
                     message("getting cached data!")
                     return(inv)
  }
  data<-y$get()
  inv<-solve(data,....)           #calculates the value og inverse 
  y$setinv(inv)
  inv   ## Returns an inverse matrix of 'y'
}