gladePlusPlus
=============

C++ source generator for gtkmm projects using glade

To compile with lazarus:
 - open the project file (gladePlusPlus.lpi) in lazarus
 - build with shift+f9
 
To compile without lazarus (just using fpc)
 - (don't know, never tried)
 
To take a glade file and produce headers and sources with a class for each top-level...

    gladePlusPlus -i <path-to-ui.glade-file> -o <output-folder> -x <glade-file-relative-to-output-dir> 
    -i = --input
    -o = --outputFolder
    -x = --xmlPath
