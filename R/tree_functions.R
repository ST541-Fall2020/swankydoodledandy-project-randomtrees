#
#
#
#
# Random Trees
#
random_trees <- function(splits = 3, 
                         length = 2,
                         scale_length = T,
                         length_scale = 1.4,
                         trunk_scale = 1,
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
                         sib_lgth_ratio = 0,
                         sib_thk_ratio = 0,
                         title = NA,
                         plot = T,
                         datadump = F,
                         random_angles = T,
                         angle_variance = 0,
                         random_lengths = T,
                         length_variance = 0){
  
  
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
                trunk_scale = trunk_scale,
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
                sib_lgth_ratio = sib_lgth_ratio,
                sib_thk_ratio = sib_thk_ratio,
                title = title,
                plot = plot,
                datadump = datadump,
                random_angles = random_angles,
                angle_variance = angle_variance,
                random_lengths = random_lengths,
                length_variance = length_variance)
  
  # Get information on number of children at each split
  if(any(as.logical(man_children))){ # Uses manually selected common children amounts at each split
    if(length(man_children) != splits){
      splits <- length(man_children)
    }
    children <- man_children[1:splits]
  }
  else if(any(as.logical(sib_lgth_ratio))){ # Uses length of sib_lgth_ratio to determine children amount
    children <- rep(length(sib_lgth_ratio), splits)
  }
  else if(any(as.logical(sib_thk_ratio))){ # Uses length of sib_thk_ratio to determine children amount
    children <- rep(length(sib_thk_ratio), splits)
  }
  else{ # All splits have same number of children
    children <- rep(children, splits)
  }
  if(any(children %% 1 != 0 || children <= 0 || typeof(children) != "double")){
    paste("error: input for children/man_children/sib_ratio should be an integer/vector of integers.")
  }
  
  # Get branch, level, and geneology info
  branch_count <- 1 + sum(cumprod(children))
  level_gen_size <- c(1,cumprod(children)) # number of branches in a particular level
  branch_gen_size <- rep(level_gen_size, c(1,cumprod(children))) # number of branches in a level per branch
  branch_route_count <- rep(rev(level_gen_size), c(1,cumprod(children))) # number of routes containing branch
  family_tree <- t(matrix(rep(1:sum(level_gen_size), branch_route_count), ncol = splits+1)) # columns indicate branch geneology
  sibling_history <- rbind(rep(0,prod(children)), 
                           t(matrix(unlist(lapply(1:splits, function(x) 
                             rep(rep(1:children[x], each = rev(level_gen_size)[x+1]), 
                                 level_gen_size[x]))), ncol = splits))) # columns indicate sibling of branch in geneology
  
  # Get branch angle information
  if(man_angles){ # Uses manually selected angles between branches for each split level
    angles <- man_angles
    scale_angle <- F # Manual angles not scaleable within current scope
  } else if(scale_angle){ # Iteratively scales selected angle by constant factor at each split
    angles <- c(start_angle, rep(angle, splits)/(angle_scale^(0:(splits-1))))
  } else{ # Uses constant angle between branches at splits
    angles <- c(start_angle, rep(angle, splits))
  }
  # Get total branch angles at each angle
  branch_delta_angle <- unlist(lapply(1:(splits+1), function(x) 
    rep(angles[x]*(c(1,children)[x]/2-1/2):(-c(1,children)[x]/2+1/2), c(1,level_gen_size)[x])))
  angle_matrix <- t(apply(matrix(branch_delta_angle[family_tree], nrow = splits+1), 2, cumsum)) 
  angles <- c(angle_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
  # Add randomness for angles if applicable
  if(random_angles){
    if(angle_variance == 0){
      angle_variance <- (angles[2]/(children[1]+1))^2 # Default random angle variance
    }
    angle_noise <- rnorm(branch_count-1, mean = 0, sd = sqrt(angle_variance)) # uses rnorm
    angles <- angles + c(0,angle_noise)
  } else{
    angle_noise <- rep(0, length(angles[-1])) # indicates no noise for branch table
  }
  
  # Get branch length information
  if(man_lengths){
    lengths <- man_lengths
    scale_length <- F # Manual lengths not scaleable within current scope
  } 
  else if(scale_length & length(length_scale) < splits){
    length_scale <- cumprod(c(1,rep(length_scale, splits)))
    lengths <- rep(length, splits+1)/length_scale
  } 
  else if(scale_length & length(length_scale) == splits){
    length_scale <- c(1,length_scale)
    lengths <- rep(length, splits+1)/length_scale
  } 
  else{ # All branches same length
    lengths <- rep(length, splits+1)
  }
  
  # If trunk_scale, rescale trunk length
  if(trunk_scale){
    lengths <- lengths * c(trunk_scale,rep(1,splits))
  }
  
  # Make lengths for each branch
  lengths <- rep(lengths, level_gen_size)
  
  # Add randomness to lengths if applicable
  if(random_lengths){
    if(length_variance == 0){
      length_variance <- lengths[1]/24 # Default random length variance
    }
    length_noise <- rnorm(branch_count-1, mean = 0, sd = sqrt(length_variance)) # uses rnorm
    length_noise <- c(0, length_noise)/rep(length_scale, level_gen_size) # scales noise same as lengths
    lengths <- lengths + length_noise
  } else{
    length_noise <- rep(0, branch_count)
  }
  # If sib_lgth_ratio given used, rescales lengths
  if(length(sib_lgth_ratio)>1 & length(unique(children)) == 1){
    sib_lgth_ratio <- sib_lgth_ratio/max(sib_lgth_ratio)
    sib_lgth_ratio <- c(1, rep(sib_lgth_ratio, sum(cumprod(c(1,children[-1])))))
    lengths <- c(diag(sib_lgth_ratio) %*% lengths)
  } else {
    sib_lgth_ratio <- rep(1, sum(c(1,cumprod(children))))
  }
  
  # Make matrix of unrotated/unstacked height coordinates
  # 100 points per branch and rescale by lengths
  Z_coords <- matrix(rep(seq(0,1,length.out=100), branch_count), nrow = 100)
  
  Z_coords <- Z_coords %*% diag(lengths)
  
  
  # If meander
  #if(meander == T){
  #  X_coords <- matrix(c(rep(0,100), na.omit(stats::filter(
  #    cumsum(rnorm(100*sum(cumprod(children)) + 18, mean = 0, sd = 0.02)), rep(1 / 19, 19), sides = 2))), 
  #    nrow = 100) %*% diag(rep(1/length_scale, level_gen_size)) # scale meander same as lengths
  #  X_coords - matrix(rep(X_coords[1,], each = 100), nrow = 100) # return x[1]'s back to zero
  #} else{
  #  X_coords <- matrix(rep(0, 100*branch_count), nrow = 100)
  #}
  
  # Rotate coordinates
  #X_coords <- X_coords %*% diag(cos(angles)) - Z_coords %*% diag(sin(angles))
  #Z_coords <- Z_coords %*% diag(cos(angles)) + X_coords %*% diag(sin(angles))
  # For some reason adding a matrix of 0's drastically effects outputs
  X_coords <- - Z_coords %*% diag(sin(angles))
  Z_coords <- Z_coords %*% diag(cos(angles))
  
  
  
  # Stack coordinates
  branch_delta_X <- rep(c(0, X_coords[100,1:(sum(rev(level_gen_size)[-1]))]),
                        c(1, rep(children, level_gen_size[-(splits+1)])))
  X_matrix <- t(apply(matrix(branch_delta_X[family_tree], nrow = splits+1), 2, cumsum))
  branch_start_X <- c(X_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
  X_coords_stacked <- X_coords + matrix(rep(branch_start_X, each = 100), nrow = 100)
  
  branch_delta_Z <- rep(c(0, Z_coords[100,1:(sum(rev(level_gen_size)[-1]))]),
                        c(1, rep(children, level_gen_size[-(splits+1)])))
  Z_matrix <- t(apply(matrix(branch_delta_Z[family_tree], nrow = splits+1), 2, cumsum))
  branch_start_Z <- c(Z_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
  Z_coords_stacked <- Z_coords + matrix(rep(branch_start_Z, each = 100), nrow = 100)
  
  # Get thickness information
  if(any(as.logical(man_split_thickness)) & taper){ # Tapers manual thicknesses to match at splits
    thicknesses <- c(man_begin_thick,
                     man_split_thickness,
                     man_end_thick)
    ts <- matrix(unlist(lapply(1:(splits+1), function(x)
      rep(seq(thicknesses[x], thicknesses[x+1], length.out = 100), level_gen_size[x]))), nrow = 100)
  } else if(any(as.logical(man_split_thickness))){ # Does not taper thicknesses to match at splits
    thicknesses <- c(man_begin_thick, man_split_thickness)
    ts <- matrix(rep(thickness, 100*(splits+1)), nrow = 100)
  } else if(!taper & scale_thickness & length(sib_thk_ratio) == 1){ # Decreases from chosen starting thickness by constant scaling factor at each split
    thicknesses <- rep(thickness, splits+1)/thickness_scale^(0:splits)
    ts <- matrix(rep(thicknesses, 100*level_gen_size), nrow = 100)
  } else if(taper & scale_thickness & length(sib_thk_ratio) == 1){ # Tapers from chosen starting thickness by constant scaling factor at each split
    thicknesses <- rep(thickness, splits+2)/thickness_scale^(0:(splits+1))
    ts <- matrix(unlist(lapply(1:(splits+1), function(x)
      rep(seq(thicknesses[x], thicknesses[x+1], length.out = 100), level_gen_size[x]))), nrow = 100)
  } else if(!taper & scale_thickness & length(sib_thk_ratio) > 1){
    sib_thk_ratio <- sib_thk_ratio/min(sib_thk_ratio)
    scale_thk_split <- thickness_scale*sib_thk_ratio/
      (1 + thickness_scale*(sib_thk_ratio-1))
    branch_delta_thk <- c(1, rep(scale_thk_split, sum(cumprod(c(1,children[-splits])))))
    thickness_matrix <- thickness/t(apply(matrix(branch_delta_thk[family_tree], nrow = splits+1), 2, cumprod))
    start_thickness <- c(thickness_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
    ts <- matrix(rep(start_thickness, each = 100), nrow = 100)
  }
  else if(taper & scale_thickness & length(sib_thk_ratio) > 1){
    sib_thk_ratio <- sib_thk_ratio/min(sib_thk_ratio)
    scale_thk_split <- thickness_scale*sib_thk_ratio/
      (1 + thickness_scale*(sib_thk_ratio-1))
    branch_delta_thk <- c(min(scale_thk_split), rep(scale_thk_split, sum(cumprod(c(1,children[-splits])))))
    thickness_matrix <- thickness/t(apply(matrix(branch_delta_thk[family_tree], nrow = splits+1), 2, cumprod))
    end_thickness <- c(thickness_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
    start_thickness <- c(rep(thickness, prod(children)), c(thickness_matrix))[cumsum(c(1,branch_route_count[-branch_count]))]
    ts <- matrix(unlist(lapply(1:branch_count, function(x) seq(start_thickness[x], end_thickness[x], length.out = 100))), nrow = 100)
  } 
  else{ # Uses chosen starting thickness throughout
    ts <- matrix(rep(thickness, 100*branch_count), nrow = 100)
  }
  # Create of vectors of thicknesses to pair with vectors of coordinates
  thickness_per_point <- c(ts)
  
  # Collect variables made by function
  fun_variables <- list(splits = splits,
                        length = length,
                        scale_length = scale_length,
                        length_scale = length_scale,
                        children = children,
                        start_angle = start_angle,
                        angle = angle,
                        angles = angles,
                        angle_matrix = angle_matrix,
                        scale_angle = scale_angle,
                        angle_scale = angle_scale,
                        unstacked_X_coords = X_coords,
                        unstacked_Z_coords = Z_coords,
                        family_tree = family_tree,
                        sibling_history = sibling_history,
                        branch_count = branch_count,
                        branch_route_count = branch_route_count,
                        level_gen_size = level_gen_size)
  
  
  # Create tree tibble
  tree <- tibble::tibble(X = c(X_coords_stacked),
                         Z = c(Z_coords_stacked),
                         thickness = thickness_per_point)
  
  
  
  branch_info <- tibble::tibble(branch = 1:branch_count,
                                generation_size = branch_gen_size,
                                length = lengths,
                                length_noise = length_noise,
                                angle_rad = angles,
                                angle_noise_rad = c(0,angle_noise),
                                angle_deg = angles*360/(2*pi),
                                angle_noise_deg = c(0,angle_noise)*360/(2*pi),
                                branch_start_X = branch_start_X,
                                branch_start_Y = branch_start_Z)
  
  
  
  plotinfo <- list(x = tree$X, y = tree$Z,
                   pch = 16, cex = tree$thickness,
                   xaxt = "n", yaxt = "n", asp = 1,
                   main = deparse(title),
                   xlab = NA, ylab = NA)
  
  random_tree <- list(inputs = inputs,
                      fun_variables = fun_variables,
                      tree = tree,
                      branch_info = branch_info,
                      plot_info = plotinfo)
  
  
  if(plot) {
    par(mar=c(1,1,1,1))
    plot(x = tree$X, y = tree$Z,
         pch = 16, cex = tree$thickness,
         xaxt = "n", yaxt = "n", asp = 1,
         main = title,
         xlab = NA, ylab = NA)
  }
  
  
  if(datadump) return(random_tree)
}
#
#
#
#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#
#
#
#
# Deterministic Trees
#
deterministic_tree <- function(splits = 3, 
                               length = 2,
                               scale_length = T,
                               length_scale = 1.4,
                               trunk_scale = 1,
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
                               sib_lgth_ratio = 0,
                               sib_thk_ratio = 0,
                               title = NA,
                               plot = T,
                               datadump = F){
  
  # Just called the function random_trees() but turns off randomness
  
  random_trees(splits = splits, 
               length = length,
               scale_length = scale_length,
               length_scale = length_scale,
               trunk_scale = trunk_scale,
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
               man_angles = man_angles,
               man_split_thickness = man_split_thickness,
               man_begin_thick = man_begin_thick,
               man_end_thick = man_end_thick,
               man_children = man_children,
               sib_lgth_ratio = sib_lgth_ratio,
               sib_thk_ratio = sib_thk_ratio,
               title = title,
               plot = plot,
               datadump = datadump,
               random_angles = F,
               angle_variance = 0,
               random_lengths = F,
               length_variance = 0)
}
#
#
#
#
#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#
#
#
# Plot Tree
#
plot_tree <- function(fractal_tree, xlim = NULL, ylim = NULL){
  par(mar=c(1,1,1,1))
  plot(x = fractal_tree$tree$X, y = fractal_tree$tree$Z,
       pch = 16, cex = fractal_tree$tree$thickness,
       xaxt = "n", yaxt = "n", asp = 1,
       main = random_tree$inputs$title,
       xlab = NA, ylab = NA,
       xlim = xlim, ylim = ylim)
}
#
#
#
#
#
#
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#
#
#
#
#
# Swaying Tree
#
swaying_tree <- function(fractal_tree, var = 0.02, scale = 0.4){
  
  xlim <- c((min(fractal_tree$tree$X) - 0.1*abs(min(fractal_tree$tree$X))),
            (max(fractal_tree$tree$X) + 0.1*abs(max(fractal_tree$tree$X))))
  ylim <- c((min(fractal_tree$tree$Z) - 0.1*abs(min(fractal_tree$tree$Z))),
            (max(fractal_tree$tree$Z) + 0.1*abs(max(fractal_tree$tree$Z))))
  
  level_gen_size <- fractal_tree$fun_variables$level_gen_size
  branch_count <- fractal_tree$fun_variables$branch_count
  children <- fractal_tree$fun_variables$children
  splits <- fractal_tree$fun_variables$splits
  family_tree <- fractal_tree$fun_variables$family_tree
  branch_route_count <- fractal_tree$fun_variables$branch_route_count
  
  model <- RandomFields::RMexp(var = var, scale = scale)
  
  x <- seq(0, 10, length.out = 50)
  y <- seq(0, 10, length.out = branch_count)
  simu <- suppressMessages(as.matrix(RandomFields::RFsimulate(model, x, y, grid=TRUE)))
  
  rename <- function(x){
    if(x < 10){
      return(name <- paste('00', x,'plot.png', sep=''))
    } 
    else if(x < 100){
      return(name <- paste('0', x,'plot.png', sep=''))
    }
    else if(x < 1000){
      return(name <- paste(x,'plot.png', sep=''))
    }
  }
  
  path <- fs::path("swaying_trees")
  suppressMessages(usethis::use_directory(path))
  
  for(i in 0:49){
    name <- rename(i+1)
    wind_angles <- simu[i+1,]/
      rep((fractal_tree$fun_variables$splits+1):1, times = c(1,cumprod(fractal_tree$fun_variables$children)))^1.8
    
    X_coords <- fractal_tree$fun_variables$unstacked_X_coords %*% diag(cos(c(wind_angles))) - 
      fractal_tree$fun_variables$unstacked_Z_coords %*% diag(sin(c(wind_angles)))
    Z_coords <- fractal_tree$fun_variables$unstacked_X_coords %*% diag(sin(c(wind_angles))) +
      fractal_tree$fun_variables$unstacked_Z_coords %*% diag(cos(c(wind_angles)))
    
    branch_delta_X <- rep(c(0, X_coords[100,1:(sum(rev(level_gen_size)[-1]))]),
                          c(1, rep(children, level_gen_size[-(splits+1)])))
    X_matrix <- t(apply(matrix(branch_delta_X[family_tree], nrow = splits+1), 2, cumsum))
    branch_start_X <- c(X_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
    X_coords_stacked <- X_coords + matrix(rep(branch_start_X, each = 100), nrow = 100)
    
    branch_delta_Z <- rep(c(0, Z_coords[100,1:(sum(rev(level_gen_size)[-1]))]),
                          c(1, rep(children, level_gen_size[-(splits+1)])))
    Z_matrix <- t(apply(matrix(branch_delta_Z[family_tree], nrow = splits+1), 2, cumsum))
    branch_start_Z <- c(Z_matrix)[cumsum(c(1,branch_route_count[-branch_count]))]
    Z_coords_stacked <- Z_coords + matrix(rep(branch_start_Z, each = 100), nrow = 100)
    
    png(name)
    par(mar=c(1,1,1,1))
    plot(x = X_coords_stacked, y = Z_coords_stacked,
         pch = 16, cex = fractal_tree$tree$thickness,
         xaxt = "n", yaxt = "n", asp = 1,
         xlab = NA, ylab = NA,
         xlim = xlim, ylim = ylim)
    
    dev.off()
  }
  
  #run ImageMagick
  filename <- paste("swaying_tree_", gsub(" ", "_", format(Sys.time(), format = "%F %T %Z")),".gif", sep = "")
  system(paste("convert *.png -delay 200x100 -loop 0 ", "swaying_trees/", filename, sep = ""))
  #invisible(file.remove(list.files(pattern=".png")))
  invisible(file.remove(list.files(pattern=".png")))
  
  rm(xlim, ylim, X_coords, Z_coords, X_coords_stacked, 
     Z_coords_stacked, model, branch_count, x, y, simu, name, wind_angles)
  
  return(noquote(paste("GIF file saved as ", filename, " in folder 'swaying_trees'.", sep = "")))
}
#
#
#
#
#
#
#
#
###################################################################################
###################################################################################
#
#
#
# Growing Tree
#
#
#
#
growing_tree <- function(fractal_tree){
  
  xlim <- c((min(fractal_tree$tree$X) - 0.1*abs(min(fractal_tree$tree$X))),
            (max(fractal_tree$tree$X) + 0.1*abs(max(fractal_tree$tree$X))))
  ylim <- c((min(fractal_tree$tree$Z) - 0.1*abs(min(fractal_tree$tree$Z))),
            (max(fractal_tree$tree$Z) + 0.1*abs(max(fractal_tree$tree$Z))))
  
  branch_count <- fractal_tree$fun_variables$branch_count
  splits <- fractal_tree$fun_variables$splits
  level_gen_size <- fractal_tree$fun_variables$level_gen_size
  
  frames <- t(matrix(1:(5*(splits+1)), nrow = 5))
  grow_index <- rep(unlist(lapply(1:(splits+1), function(x) rep(frames[x,], level_gen_size[x]))), each = 20)
  
  fractal_tree$tree$grow_index <- grow_index
  
  rename <- function(x){
    if(x < 10){
      return(name <- paste('00', x,'plot.png', sep=''))
    } 
    else if(x < 100){
      return(name <- paste('0', x,'plot.png', sep=''))
    }
    else if(x < 1000){
      return(name <- paste(x,'plot.png', sep=''))
    }
  }
  
  path <- fs::path("growing_trees")
  suppressMessages(usethis::use_directory(path))
  
  for(i in 1:max(frames)){
    name <- rename(i)
    
    tree <- fractal_tree$tree[which(fractal_tree$tree$grow_index < i+1),]
    
    png(name)
    par(mar=c(1,1,1,1))
    plot(x = tree$X, 
         y = tree$Z,
         pch = 16, cex = tree$thickness,
         xaxt = "n", yaxt = "n", asp = 1,
         xlab = NA, ylab = NA,
         xlim = xlim, ylim = ylim)
    
    dev.off()
  }
  
  #run ImageMagick
  filename <- paste("growing_tree_", gsub(" ", "_", format(Sys.time(), format = "%F %T %Z")),".gif", sep = "")
  system(paste("convert *.png -delay 200x100 -loop 0 ", "growing_trees/", filename, sep = ""))
  #invisible(file.remove(list.files(pattern=".png")))
  invisible(file.remove(list.files(pattern=".png")))
  
  rm(tree, grow_index, frames, xlim, ylim)
  
  return(noquote(paste("GIF file saved as ", filename, " in folder 'growing_trees'.", sep = "")))
}