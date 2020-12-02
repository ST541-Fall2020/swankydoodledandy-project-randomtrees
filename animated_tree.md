Animated Tree
================

Using animation guidance from:
<https://davetang.org/muse/2015/02/12/animated-plots-using-r/>

``` r
devtools::load_all()
```

    ## Loading swankydoodledandy-project-randomtrees

    ## Invalid DESCRIPTION:
    ## Malformed package name
    ## 
    ## See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    ## manual.

``` r
fractal_tree <- suppressMessages(random_trees(splits = 8, children = 2, angle = pi/4, scale_angle = F, random_angles = T, 
                                              random_lengths = T, length_scale = 1.4, plot = F, datadump = T))
swaying_tree(fractal_tree, var = 0.02, scale = 0.4)
```

    ## [1] GIF file saved as swaying_tree_2020-12-02_15:25:16_PST.gif in current directory.

# Swaying Tree

``` r
swaying_tree <- function(fractal_tree, var = 0.02, scale = 0.4){
  family <- fractal_tree$fun_variables$family
  levels <- fractal_tree$fun_variables$levels
  xlim <- c((min(fractal_tree$tree$X) - 0.1*abs(min(fractal_tree$tree$X))),
            (max(fractal_tree$tree$X) + 0.1*abs(max(fractal_tree$tree$X))))
  ylim <- c((min(fractal_tree$tree$Z) - 0.1*abs(min(fractal_tree$tree$Z))),
            (max(fractal_tree$tree$Z) + 0.1*abs(max(fractal_tree$tree$Z))))
  
  model <- RandomFields::RMexp(var = var, scale = scale)
  branch_count <- sum(cumprod(fractal_tree$fun_variables$children)) + 1
  x <- seq(0, 10, length.out = 100)
  y <- seq(0, 10, length.out = branch_count)
  simu <- suppressMessages(as.matrix(RandomFields::RFsimulate(model, x, y, grid=TRUE)))
  
  rename <- function(x){
    return(name <- paste('00', x,'plot.png', sep=''))
  }
  
  path <- fs::path("swaying_trees")
  suppressMessages(usethis::use_directory(path))
  
  for(i in 0:99){
    name <- rename(i+1)
    wind_angles <- simu[i+1,]/
      rep((fractal_tree$fun_variables$splits+1):1, times = c(1,cumprod(fractal_tree$fun_variables$children)))^1.8
    
    X_coords <- fractal_tree$fun_variables$unstacked_X_coords %*% diag(cos(c(wind_angles))) - 
      fractal_tree$fun_variables$unstacked_Z_coords %*% diag(sin(c(wind_angles)))
    Z_coords <- fractal_tree$fun_variables$unstacked_X_coords %*% diag(sin(c(wind_angles))) +
      fractal_tree$fun_variables$unstacked_Z_coords %*% diag(cos(c(wind_angles)))
    
    X_coords_stacked <- unlist(purrr::map(1:ncol(X_coords), ~ if(.==1){X_coords[,.]}
                                          else{X_coords[,.]<-X_coords[,.]+
                                            sum(X_coords[100,family[[levels[.]+1]][,which(family[[levels[.]+1]] == ., 
                                                                                          arr.ind = T)[2]][-(levels[.]+1)]])}))
    Z_coords_stacked <- unlist(purrr::map(1:ncol(Z_coords), ~ if(.==1){Z_coords[,.]}
                                          else{Z_coords[,.]<-Z_coords[,.]+
                                            sum(Z_coords[100,family[[levels[.]+1]][,which(family[[levels[.]+1]] == .,
                                                                                          arr.ind = T)[2]][-(levels[.]+1)]])}))
    
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
  system(paste("convert *.png -delay 20x100 -loop 0 ", filename, sep = ""))
  invisible(file.remove(list.files(pattern=".png")))
  
  rm(levels, family, xlim, ylim, X_coords, Z_coords, X_coords_stacked, 
     Z_coords_stacked, model, branch, x, y, simu, file_name, name, wind_angles)
  
  return(noquote(paste("GIF file saved as ", filename, " in current directory.", sep = "")))
}
```

<img src="swaying_tree_2020-12-02_14:54:08_PST.gif">