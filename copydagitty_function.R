
copydagitty <- function(x, autoalign=FALSE, autoalign.tol=0.01){
  
  
  
  split <- str_split(string, "]")
  
  length <- length(split[[1]])
  
  #first name and position contains some junk that we'd like to get rid of
  
  first <- str_split(split[[1]][1], "\n")
  
  first <- first[[1]][length(first[[1]])]
  
  first <- gsub("[[:blank:]]", "", first)
  
  
  #other names and positions contains line changes, so let's get rid of those
  
  namepos <- gsub("\n|[[:blank:]]", "", split[[1]][2:(length-1)])
  
  
  #didn't come up with this regex, source: https://stackoverflow.com/a/31530950
  #this grabs everything between " and [
  
  # name <- gsub('^.*\"\\s*|\\s*\\[.*$', '', first)
  
  #this grabs everything between = and "
  # 
  # pos <-  gsub('^.*=\\s*|\\s*\\\".*$', '', first)
  # 
  # x <- str_split(pos, ",")[[1]][1]
  # 
  # y <- str_split(pos, ",")[[1]][2]
  
  
  #the loop
  
  namepos <- c(first, namepos)
  
  namepos.df <- lapply(namepos, function(z){
    
    #z <- namepos[1]
    
    name <- gsub('^.*\"\\s*|\\s*\\[.*$', '', z)
    
    pos <-  gsub('^.*=\\s*|\\s*\\\".*$', '', z)
    
    x <- as.numeric(str_split(pos, ",")[[1]][1])
    
    y <- as.numeric(str_split(pos, ",")[[1]][2])
    
    
    
    
    if (str_detect(z, "outcome")) {
      out_ex <- "outcome"
    } else if (str_detect(z, "exposure")) {
      
      out_ex <- "exposure"
      
    } else {
      
      out_ex <- "none"
    }
    
    df <- data.frame(name=name, x=x, y=y, out_ex=out_ex)
    
    
  })
  
  namepos.df <- do.call("rbind", namepos.df)
  
  dag <- paste("dag{", split[[1]][length], sep="")
  
  dag <- dagitty(dag)
  
  
  if (autoalign==TRUE){
    
    
    
    
    #autoalign
    #find every pair of vertices
    #see if their coordinates on either the x- or y-axis are reasonably close
    #set coordinates to be the same for those pairs that are within reasonable distance of each other.
    
    pairs <- t(combn(namepos.df$name, 2))
    
    #the loop
    
    for (i in 1:length(pairs[,1])) {
      
      
      pair <- c(as.character(pairs[i,1]), as.character(pairs[i,2]))
      
      x.dist <- namepos.df$x[namepos.df$name==pair[1]]-namepos.df$x[namepos.df$name==pair[2]]
      
      if (abs(x.dist)<autoalign.tol) {
        
        namepos.df$x[namepos.df$name==pair[1]] <- namepos.df$x[namepos.df$name==pair[2]]
        
      }
      
      y.dist <- namepos.df$y[namepos.df$name==pair[1]]-namepos.df$y[namepos.df$name==pair[2]]
      
      if (abs(y.dist)<autoalign.tol) {
        
        namepos.df$y[namepos.df$name==pair[1]] <- namepos.df$y[namepos.df$name==pair[2]]
        
      }
      
      
      
    }
    
    
  } 
  
  
  coordinates(dag) <- coords2list(namepos.df[1:3])
  
  ex <- unique(as.character(namepos.df$name[namepos.df$out_ex=="exposure"]))
  
  out <- unique(as.character(namepos.df$name[namepos.df$out_ex=="outcome"]))
  
  dagitty::exposures(dag) <- ex
  
  dagitty::outcomes(dag) <- out
  
  
  
  #tidy <- tidy_dagitty(dag)
  
  return(dag)
  
}