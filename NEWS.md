# AIG 0.1.0

* Added first function to generate linear syllogism items.


# AIG 0.1.1

### MINOR CHANGES

* Added S3 class to improve the item format of the return object when using the `lisy()`. 

* Change the author\'s name to include the chinese name of the package creator. 

* Added a new function to generate arithmetic items using the `arith()`. 

* Updated the AIG description page to include arithmetic generator. 

* Updated minor grammatical errors.

* Added new references. 

* Now the items can generate linear array items for up to 3 inferences. (i.e. Adjacent clues have matching operators.)

* Added a new argument to include the creation of item clones for up to 3 inferences. 

* Items with linear array only returns false distractors. Invalid distractors cannot be created. 

* Reduce to 4 distractors and 1 answer, giving a total of 5 options. More can be created, but it may be more suitable by hand at the moment. 


# AIG 0.1.2

### MINOR CHANGES

* Added a new function to generate spatial 2D reasoning items using the `spatial2d()`.

* The `spatial2d()` generates 4 2D distractors and 1 2D answer. 

* The `spatial2d()` can generate 2D or 3D (top or bottom perspective) display figures. 

* Added a new function to generate spatial 3D reasoning items using the `spatial3d()`. 

* Added a new function to generate a mirror image of the spatial 3D reasoning item using the `spatial3d_mirror()`. 

* Included arguments so that user can interactively change the view of the figure, or programmatically alter the figure rotation.

# AIG 0.1.3

### MINOR CHANGES

* Fixed grammatical error.

* Fixed bugs from `lisy()`.

# AIG 0.1.4

### MINOR CHANGES

* Improved examples for `spatial3d()`.


# AIG 0.1.5

### MINOR CHANGES

* Improved on `lisy()` description.

* Added more help to save items in jpg format for 3d items.

* Included a new argument for 2d spatial items. This will allow users to decide on the rotation degree for the answer. 

* 2d items are saved as jpg rather than pdf now. 

* Updated bugs on 2d items. Now users can generate X number of 2d spatial items at one go. Users can also choose the specific 2d design given the seed number.

* Fixed bugs in `lisy()` to make sure that the argument `direct = alt` can be used when ninfer is 3 or greater. 

# AIG 0.1.6

### MINOR CHANGES

* users can decide on how many connected blocks they want in their 3d items to make up the cube. 

* fixed bug in 2d spatial items. 

* Two new spatial functions are developed to allow 2 figures to be generated per image. 

---
# AIG 0.1.7

### MINOR CHANGES

* Updated contributors and copyright holders to the description file and the corresponding generators. 
