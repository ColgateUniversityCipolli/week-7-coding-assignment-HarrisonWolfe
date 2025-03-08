
pois.prob <- function(x, lambda, type="<="){
  if(x<0){
    return("x must be a positive integer")
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




