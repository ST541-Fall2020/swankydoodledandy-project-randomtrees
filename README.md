# Random Trees
#### Name: Sean Gilligan

This project is a "package" that contains functions for making trees in R, starting with deterministic methods and introducing randomness to angles and lengths.  


## Basic Deterministic Trees

<b>basic_deterministic_trees</b>(splits = 3, length = 2, scale_length = T, length_scale = 1.272018^2, children = 2, start_angle = 0, 
angle = pi/(splits/2 + 1), scale_angle = T, angle_scale = sqrt(1.272018), thickness = 2, scale_thickness = T, thickness_scale = 1.61803, 
taper = T, man_lengths = 0, man_angles = 0, man_split_thickness = 0, man_begin_thick = 0, man_end_thick = 0, man_children = 0, 
sib_ratio = 0, title = NA, plot = T, datadump = F)

### Input Arguments

<b>splits</b>                 : (int) Indicates number of levels beyond starting branch. <br>
<b>length</b>                 : (dbl) Indicates length of a branch. <br>
<b>scale_length</b>           : (lgl) Indicates if lengths should be scaled at each new level. <br>
<b>length_scale</b>           : (dbl) Indicates rate in which branch lengths shorten at each level. <br>
<b>children</b>               : (int) Indicates number of new branches at each new level. <br>
<b>start_angle</b>            : (dbl) Indicates angle in radians of starting branch, measured ccw from +y direction. <br>
<b>angle</b>                  : (dbl) Indicates angle in radian between each branch at a split. <br>
<b>scale_angle</b>            : (lgl) Indicates if branch split angles should be scaled at each new level. <br>
<b>angle_scale</b>            : (lgl) Indicates rate at which angles should decrease <br>
<b>thickness</b>              : (dbl) Indicates thickness of a branch. <br>
<b>scale_thickness</b>        : (lgl) Indicates if thickness should should shrink at each new level <br>
<b>thickness_scale</b>        : (dbl) Indicates rate in which branch thicknesses should shrink <br>
<b>taper</b>                  : (lgl) Indicates if branches should taper. <br>
<br>
<b>man_lengths</b>            : Manually select branch length for starting branch and branches at each level. <br>
<b>man_angles</b>             : Manually select angles between branches at each split by level. <br>
<b>man_split_thickness</b>    : Manually select thickness of each branch at each split in order. <br>
<b>man_begin_thick</b>        : Manually select starting thickness by level. <br>
<b>man_end_thick</b>          : Manually select ending thickness. <br>
<b>man_children</b>           : Manually select number of branches at split by level. <br>
<b>sib_ratio</b>              : Only works if equal number of children at each split. Vector ndicates relative size of children at each split. <br>
<br>
<b>title</b>                  : (chr) Optional title for output tree. <br>
<b>plot</b>                   : (lgl) Default to T for plotting <br>
<b>datadump</b>               : (lgl) Default to F. Set to T get relevant data.

Try to keep children^splits < around 700. Function does not perform well otherwise.

## Random Trees

<b>random_trees</b>(splits = 3, length = 2, scale_length = T, length_scale = 1.4, trunk_scale = 1, children = 2, start_angle = 0, 
angle = pi/(splits/2 + 1), scale_angle = T, angle_scale = sqrt(1.272018), thickness = 2, scale_thickness = T, thickness_scale = 1.61803, 
taper = T, man_lengths = 0, man_angles = 0, man_split_thickness = 0, man_begin_thick = 0, man_end_thick = 0, man_children = 0, 
sib_ratio = 0, title = NA, plot = T, datadump = F, <b>random_angles = T, angle_variance = 0, random_lengths = T, length_variance = 0</b>)

Essentially the same as <b>basic_deterministic_trees()</b> but adds a few input arguments for randomization.

### Added Input Arguments

<b>random_angles</b>                 : (lgl) Toggles angle noise on/off. Random values currently chosen via sampling from normal distribution. <br>
<b>angle_variance</b>                : (dbl) Indicates base variance for angle noise. By default "set" to zero be given value based on other inputs. In particular by (angles[2]/4)^2, where angles[2] is the angle between branches at the first split.<br>
<b>random_lengths</b>                : (lgl) Toggles length noise on/off. Random values currently chosen via sampling from normal distribution. <br>
<b>length_variance</b>               : (dbl) Indicates base variance for length noise. By default "set" to zero be given value later based on other inputs. In particular by lengths[1]/24, where lengths[1] is the length of the starting branch.

