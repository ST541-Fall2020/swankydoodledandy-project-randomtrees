Nature of Code - Example 8.6
================

``` r
devtools::load_all()
```

<img src="nature_of_code_examples_files/ch08_ex06a.png">
<img src="nature_of_code_examples_files/ch08_ex06b.png">
<img src="nature_of_code_examples_files/ch08_ex06c.png">

``` r
par(mfrow=c(1,3), mar=c(1,1,1,1))
deterministic_tree(splits = 8, children = 2, angle = 9*pi/12, scale_angle = F, 
                   scale_thickness = F, taper = F, thickness = 0.5, length_scale = 1.5)
deterministic_tree(splits = 8, children = 2, angle = pi/4, scale_angle = F, 
                   scale_thickness = F, taper = F, thickness = 0.5, length_scale = 1.5)
deterministic_tree(splits = 8, children = 2, angle = pi/20, scale_angle = F, 
                   scale_thickness = F, taper = F, thickness = 0.5, length_scale = 1.5)
```

![](nature_of_code_examples_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
