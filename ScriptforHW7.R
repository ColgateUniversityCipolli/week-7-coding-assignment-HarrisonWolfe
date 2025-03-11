
pois.prob <- function(x, lambda, type="<="){
 x = floor(x)
   if(x<0){
    return("x must be a positive integer")
  }
  if(lambda < 0){
    return("lambda must be a positive integer")
  }
  if(type == "<="){#Less than or equal to
    return(ppois(x,lambda))
  }
  if(type == "="){#Equal to
    return(dpois(x,lambda))
  }
  if(type == "!="){#Not equal to
    return(1-dpois(x,lambda))}
  if(type == ">="){# Greater than or equal to
    return(1-ppois(x-1,lambda))}
  if(type == ">"){# Greater than
    return(1-ppois(x,lambda))
  }
  if(type == "<"){#Less than
    return(ppois(x-1,lambda))
  }
}


beta.prob <- function(x, alpha, beta, type="<="){
  if(x<0){
    return("x must be positive")
  }
  if(alpha<0){
    return("alpha must be positive")
  }
  if(beta<0){
    return("beta must be postive")
  }
  if(type == "<="){#Less than or equal to
    return(pbeta(x,alpha,beta))
  }
  if(type == "="){#Equal to
    return(0)
  }
  if(type == "!="){#Not equal to
    return(1)
  }
  if(type == ">="){# Greater than or equal to
    return(1-pbeta(x,alpha,beta))
    }
  if(type == ">"){# Greater than
    return(1-pbeta(x,alpha,beta))
  }
  if(type == "<"){#Less than
    return(pbeta(x,alpha,beta))
  }
}


