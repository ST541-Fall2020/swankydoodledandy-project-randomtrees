basic_deterministic_trees <- function(splits = 3, 
                                      length = 2,
                                      scale_length = T,
                                      length_scale = 1.272018^2,
                                      children = 2,
                                      start_angle = 0,
                                      angle = pi/(splits/2 + 1),
                                      scale_angle = T,
                                      angle_scale = sqrt(1.272018),
                                      thickness = 2,
                                      scale_thickness = T,
                                      thickness_scale = 1.61803,
                                      taper = T,
                                      man_lengths = 0,
                                      man_angles = 0,
                                      man_split_thickness = 0,
                                      man_begin_thick = 0,
                                      man_end_thick = 0,
                                      man_children = 0,
                                      sib_ratio = 0,
                                      title = NA,
                                      plot = T,
                                      datadump = F){
  
  if(typeof(splits) != "double" || splits %% 1 != 0 || splits <= 0){
    return("error: splits must be a positive integer")
  }
  if(!any(c(0,1,F,T,FALSE,TRUE) == scale_length)){
    return("error: scale_length should be given a logical value")
  }
  if(!any(c(0,1,F,T,FALSE,TRUE) == scale_angle)){
    return("error: scale_angle should be given a logical value")
  }
  if(!any(c(0,1,F,T,FALSE,TRUE) == taper)){
    return("error: taper should be given a logical value")
  }
  if(!any(c(0,1,F,T,FALSE,TRUE) == scale_thickness)){
    return("error: scale_thickness should be given a logical value")
  }
  
  inputs = list(splits = splits,
                length = length,
                scale_length = scale_length,
                length_scale = length_scale,
                children = children,
                start_angle = start_angle,
                angle = angle,
                scale_angle = scale_angle,
                angle_scale = angle_scale,
                thickness = thickness,
                scale_thickness = scale_thickness,
                thickness_scale = thickness_scale,
                taper = taper,
                man_lengths = man_lengths,
                man_split_thickness = man_split_thickness,
                man_begin_thick = man_begin_thick,
                man_end_thick = man_end_thick,
                man_children = man_children,
                sib_ratio = sib_ratio,
                title = title,
                plot = plot,
                datadump = datadump)
  
  # Get information on number of splits at each level
  if(any(as.logical(man_children))){ # Uses manually selected common children amounts at each split
    if(length(man_children) != splits){
      splits <- length(man_children)
    }
    children <- man_children[1:splits]
  }
  else if(any(as.logical(sib_ratio))){ # Uses manually selected common children amounts at each split
    children <- rep(length(sib_ratio), splits)
  }
  else{ # All splits have same number of children
    children <- rep(children, splits)
  }
  if(any(children %% 1 != 0 || children <= 0 || typeof(children) != "double")){
    paste("error: input for children/man_children/sib_ratio should be an integer/vector of integers.")
  }
  
  # Get branch angle information
  if(man_angles){ # Uses manually selected angles between branches for each split level
    angles <- man_angles
    scale_angle <- F # Manual angles not scaleable within current scope
  } else if(scale_angle){ # Iteratively scales selected angle by constant factor at each split
    angles <- c(start_angle, map_dbl(1:splits, ~ angle/angle_scale^(. - 1)))
  } else{ # Uses constant angle between branches at splits
    angles <- c(start_angle, rep(angle, splits))
  }
  # Get the total angle for each branch, including starting branch
  angles <- c(rep(angles[1], prod(children)), 
              unlist(map(1:splits,
                         ~ rep(rep(angles[.+1]*((-(children[.]-1)/2):((children[.]-1)/2)), 
                                   each = prod(children[-(1:.)])), 
                               times = if(.==1){1}else{prod(children[1:(.-1)])}))))
  angles_matrix <- matrix(angles, ncol = splits + 1)
  angles <- c(start_angle, 
              unlist(map(1:splits, ~ 
                           angles_matrix[seq(1,prod(children),prod(children[-(1:.)])),1:(.+1)] 
                         %*% rep(1,.+1))))
  # Get branch length information and make table of starting lines
  X <- rep(0, 100)
  if(man_lengths){
    lengths <- man_lengths
    scale_length <- F # Manual lengths not scaleable within current scope
    Zs <- suppressMessages(map_dfc(lengths, ~ seq(0, ., length.out=100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  }
  if(scale_length){
    lengths <- map_dbl(0:(splits), ~ length/length_scale^(.))
    Zs <- suppressMessages(map_dfc(lengths, ~ seq(0, ., length.out=100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  } else{
    lengths <- rep(length, splits+1)
    Zs <- suppressMessages(map_dfc(lengths, ~ seq(0, 1, length.out=100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  }
  
  # Make matrices of unrotated/unstacked coordinates
  Z_coords <- matrix(unlist(map(1:(splits+1), 
                                ~ rep(Zs[,.], times = if(.==1){1}else{prod(children[1:(.-1)])}))), 
                     ncol = length(angles))
  # If "sib_ratio" selected, rescales lengths
  if(length(sib_ratio)>1){
    sib_ratio <- sib_ratio/max(sib_ratio)
    sib_ratio <- c(1, rep(sib_ratio, sum(cumprod(c(1,children[-1])))))
    Z_coords <- Z_coords %*% diag(sib_ratio)
  } else {
    sib_ratio <- rep(1, sum(c(1,cumprod(children))))
  }
  # Rotate coordinates
  X_coords <- - Z_coords %*% diag(sin(angles))
  Z_coords <- Z_coords %*% diag(cos(angles))
  # Make branch address matrix
  levels <- rep(0:splits, times = c(1,cumprod(children)))
  gensize <- cumprod(children)
  gen_index <- cbind(unlist(map(1:length(gensize), ~1:gensize[.])), 
                     c(rep(1:splits, times = cumprod(children))))
  
  family <- rbind(rep(1,prod(children)),
                  do.call(rbind, map(1:splits, ~ rep((cumsum(c(1,gensize))[.]+1):(cumsum(c(1,gensize))[.+1]), 
                                                     each = prod(children[-(1:.)])))))
  family <- map(1:(splits+1), ~ matrix(family[1:.,seq(1,prod(children),
                                                      by=if(.==(splits+1)){1}else{prod(children[.:splits])})],
                                       ncol = c(1,gensize)[.]))
  
  paths <- unlist(map(1:splits, ~ rep(rep(1:children[.], each = prod(children[-(1:.)])), 
                                      times = if(.==1){1}else{prod(children[1:(.-1)])})))
  paths <- matrix(paths, ncol = splits)
  paths <- map(1:splits, 
               ~ matrix(paths[seq(1,prod(children),by=if(.==splits){1}else{prod(children[(.+1):splits])}),1:.],
                        nrow = gensize[.]))
  # Branch naming
  # names1 favors relative sibling information at each split
  names1 <- c("b1_0", map_chr(1:length(levels[-1]), 
                              ~ paste(c("b",.+1,"_0", paths[[levels[-1][.]]][gen_index[.,1], 1:gen_index[.,2]]), collapse = "")))
  # names2 favors parent information
  names2 <- c("b_1", map_chr(1:length(levels[-1]), 
                             ~ paste(c("b_1", matrix(t(family[[levels[-1][.]+1]])[,-1], 
                                                     nrow = gensize[levels[.+1]])[gen_index[.,1], 1:gen_index[.,2]]), collapse = "_")))
  # Stack coordinates
  X_coords <- unlist(map(1:ncol(X_coords), ~ if(.==1){X_coords[,.]}
                         else{X_coords[,.]<-X_coords[,.]+
                           sum(X_coords[100,family[[levels[.]+1]][,which(family[[levels[.]+1]] == ., 
                                                                         arr.ind = T)[2]][-(levels[.]+1)]])}))
  Z_coords <- unlist(map(1:ncol(Z_coords), ~ if(.==1){Z_coords[,.]}
                         else{Z_coords[,.]<-Z_coords[,.]+
                           sum(Z_coords[100,family[[levels[.]+1]][,which(family[[levels[.]+1]] == .,
                                                                         arr.ind = T)[2]][-(levels[.]+1)]])}))
  # Get thickness information
  if(man_split_thickness & taper){ # Tapers manual thicknesses to match at splits
    thicknesses <- c(man_begin_thick,
                     man_split_thickness,
                     man_end_thick)
    ts <- suppressMessages(map_dfc(1:(splits+1), ~ seq(thicknesses[.], thicknesses[.+1], length.out=100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  } else if(man_split_thickness){ # Does not taper thicknesses to match at splits
    ts <- suppressMessages(map_dfc(1:(splits+1), ~ rep(thicknesses[.], 100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  } else if(!taper & scale_thickness){ # Decreases from chosen starting thickness by constant scaling factor at each split
    thicknesses <- map_dbl(0:(splits+1), ~ thickness/thickness_scale^(.))
    ts <- suppressMessages(map_dfc(1:(splits+1), ~ rep(thicknesses[.], each = 100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  } else if(taper & scale_thickness){ # Tapers from chosen starting thickness by constant scaling factor at each split
    thicknesses <- map_dbl(0:(splits+1), ~ thickness/thickness_scale^(.))
    ts <- suppressMessages(map_dfc(1:(splits+1), ~ seq(thicknesses[.], thicknesses[.+1], length.out=100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  } else{ # Uses chosen starting thickness throughout
    ts <- suppressMessages(map_dfc(1:(splits+1), ~ rep(thickness, 100))) %>% 
      set_names(map_chr(0:splits, ~ paste(.)))
  }
  # Create of vectors of thicknesses to pair with vectors of coordinates
  thickness_per_point <- unlist(map(1:(splits+1), 
                                    ~ rep(ts[,.], times = if(.==1){1}else{prod(children[1:(.-1)])})))
  # Collect variables made by function
  fun_variables <- list(splits = splits,
                        length = length,
                        scale_length = scale_length,
                        length_scale = length_scale,
                        children = children,
                        start_angle = start_angle,
                        angle = angle,
                        angles = angles,
                        angles_matrix = angles_matrix,
                        scale_angle = scale_angle,
                        angle_scale = angle_scale)
  
  # Create tree tibble
  tree <- tibble(X = X_coords,
                 Z = Z_coords,
                 thickness = thickness_per_point)
  
  branch_info <- tibble(branch = 1:length(angles),
                        sibling_path_name = names1,
                        parents_path_name = names2,
                        generation_size = rep(c(1,cumprod(children)), c(1,cumprod(children))),
                        level = levels,
                        length = rep(lengths, times =  c(1,cumprod(children))) %*% diag(sib_ratio),
                        angle_rad = angles,
                        angle_deg = angles*360/(2*pi),
                        start_thickness = thickness_per_point[seq(1,(length(angles)*100), by = 100)],
                        end_thickness = thickness_per_point[seq(100,(length(angles)*100), by = 100)])
  
  
  
  plotinfo <- list(x = tree$X, y = tree$Z,
                   pch = 16, cex = tree$thickness,
                   xaxt = "n", yaxt = "n", asp = 1,
                   main = deparse(title),
                   xlab = NA, ylab = NA)
  
  deterministic_tree <- list(inputs = inputs,
                             fun_variables = fun_variables,
                             tree = tree,
                             branch_info = branch_info,
                             plot_info = plotinfo)
  
  
  if(plot) {
    par(mar=c(1,1,1,1))
    plot(x = tree$X, y = tree$Z,
         pch = 16,cex = tree$thickness,
         xaxt = "n", yaxt = "n", asp = 1,
         main = title,
         xlab = NA, ylab = NA)
  }
  
  if(datadump) return(deterministic_tree)
}