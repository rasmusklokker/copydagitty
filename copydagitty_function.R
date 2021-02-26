
copydagitty <- function(x, autoalign=FALSE, autoalign.tol=0.01){
  
  
  #x <- dag.raw
  

  #first we get the names and coordinates of the vertices
  
  
  split <- str_split(x, "]") #we split the string by ], which will give us some chunks that we can work with
  
  length <- length(split[[1]]) #we'll assign this as an object, or else some of the code later gets very dense
  
  #first name and position contains some junk that we'd like to get rid of
  
  first <- str_split(split[[1]][1], "\n") #the junk is located before the line change, \n, so we'll split at that
  
  first <- first[[1]][length(first[[1]])] #We'll retain the last element, length(first[[1]]),  
  #in "first" since this where the name and the position of the first vertex is located.
  
  space.first <- str_detect(first, " ") #detect space in node names
  
  first <- gsub("[[:blank:]](\\[)", "\\1", first) #we get rid of blank space
  
  first <- trimws(first)
  
  #other names and positions contains line changes and blank spaces, so let's get rid of those
  

  
  
  namepos <- gsub("\n|[[:blank:]](\\[)", "\\1", split[[1]][2:(length-1)]) #the last element of "split" contains the specification of the dag
  #so we won't include that here
  
  namepos <- trimws(namepos)
  
  
  
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
  
  namepos <- c(first, namepos) #we get a vector of strings containing the names and the coordinates of the vertices.
  
  namepos.df <- lapply(namepos, function(z){ #we loop across elements of "namepos"
    
    #z <- namepos[1]
    
    name <- gsub('^.*\"\\s*|\\s*\\[.*$', '', z) #we get the name of the vertex of element z in namepos
    
    pos <-  gsub('^.*=\\s*|\\s*\\\".*$', '', z) #same for coordinates, but now coordinates are bundled in one string
    #fortunately coordinates are separated by a "," so easy to get X and Y coordinates
    
    x <- as.numeric(str_split(pos, ",")[[1]][1])
    
    y <- as.numeric(str_split(pos, ",")[[1]][2])
    
    
    #next we'd like to know if a vertex is outcome or exposure. This is included in the code pasted from Dagitty, so we can use str_detect
    #to fetch that info
    
    if (str_detect(z, "outcome")) { #if element z of "namepos" contains the string "outcome"
      out_ex <- "outcome"
    } else if (str_detect(z, "exposure")) { #same for "exposure"
      
      out_ex <- "exposure"
      
    } else {
      
      out_ex <- "none"
    }
    
    df <- data.frame(name=name, x=x, y=y, out_ex=out_ex) #we put it all in a dataframe
    
    
  })
  
  namepos.df <- do.call("rbind", namepos.df) #As usual, lapply spits out a list of dataframes instead just the one dataframe containing all the info
  #so we just "stack" the dataframes on top of each other, i.e. we rbind the dataframes using do.call
  
  space.other <- str_detect(namepos.df$name, " ") #detect space in node names
  
  if( any(space.other) ) warning('some node names contained spaces. These have been replaced by "_"')
  
  
  namepos.df$name <- gsub(" ", "_", namepos.df$name) #get rid of spaces in names
  
  dag <- paste("dag{", split[[1]][length], sep="") #now we get to extracting the actual specification of the dag. Since we threw away the 
  #"dag{" part earlier we'll have to add that here using paste

  
  #source: https://stackoverflow.com/a/953496
 # gsub("(\\n) .*? (->)","\\1 name \\2", dag)
  
  dag <- gsub("([a-z]) ([a-z])","\\1_\\2", dag) #this gets rid of spaces in the dag part, and replaces them with "_"
  
  
  
  
  #gsub("(?=.{2}$).","blah", dag, perl=TRUE)
  
  dag <- dagitty(dag) #and then we can use the dagitty package to parse the code from dagitty.net
  
  #autoalign
  
  if (autoalign==TRUE){
    
    
    
    
    #autoalign
    #find every pair of vertices
    #see if their coordinates on either the x- or y-axis are reasonably close
    #set coordinates to be the same for those pairs that are within reasonable distance of each other.
    
    pairs <- t(combn(namepos.df$name, 2)) #get combinations of vertices, i.e. all the pairs of vertices
    
    #the loop
    
    for (i in 1:length(pairs[,1])) { #for loop here, since i need to assign values to "namepos" within the loop, and i only know
      #how to do that in an lapply-loop using the "<<-" operator, but that feels hacky to me(some would say every piece of code i write is hacky
      #but what can i say, i just like square brackets and dollar signs, ok)
      
      
      pair <- c(as.character(pairs[i,1]), as.character(pairs[i,2])) #get a vector of vertex names in row i of "pairs"
      
      x.dist <- namepos.df$x[namepos.df$name==pair[1]]-namepos.df$x[namepos.df$name==pair[2]] #difference in x-coordinates for current vertex pair
      
      if (abs(x.dist)<=autoalign.tol) { #if difference in coordinates is equal to or lower than the tolerance limit
        
        namepos.df$x[namepos.df$name==pair[1]] <- namepos.df$x[namepos.df$name==pair[2]] #we simply set the X coordinate of vertex 1 to be
        #the X coordinate of vertex 2.
        
      }
      
      #same for y-coordinates
      
      y.dist <- namepos.df$y[namepos.df$name==pair[1]]-namepos.df$y[namepos.df$name==pair[2]]
      
      if (abs(y.dist)<=autoalign.tol) {
        
        namepos.df$y[namepos.df$name==pair[1]] <- namepos.df$y[namepos.df$name==pair[2]]
        
      }
      
      
      
    }
    
    
  } 
  
  
  #use the names and coordinates contained in "namepos", columns 1-3, to specify coordinates in the dag. We use the excellent function
  #ggdag::coords2list which will convert the dataframe to a named list which complies with the format of the dag.
  
  coordinates(dag) <- coords2list(namepos.df[1:3])
  
  ex <- unique(as.character(namepos.df$name[namepos.df$out_ex=="exposure"])) #we get the name of the vertex that is "exposure"
  
  out <- unique(as.character(namepos.df$name[namepos.df$out_ex=="outcome"])) #same for outcome
  
  #we set the exposure and outcome of the dag
  
  dagitty::exposures(dag) <- ex
  
  dagitty::outcomes(dag) <- out
  
  
  
  #tidy <- tidy_dagitty(dag)
  
  return(dag)
  
}
