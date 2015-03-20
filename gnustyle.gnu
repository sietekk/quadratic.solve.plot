#########################################################################################
#DESCRIPTION:                                                                           #
#Filename: gnustyle.gnu                                                                 #
#gnuplot script file for plotting data in file 'quadsolvplot.data'                      #
#Taken from YouTube user hexafoil's video "Modern Fortran by Example (9) Gnuplot Part 3"#
#and from http://people.duke.edu/~hpgavin/gnuplot.html                                  #
#This sets the styles for gnuplot when called on in 'quadsolvplotmodule.mod' to         #
#plot the data in file 'quadsolvplot.data'.                                             #
#ORIGIN INFO:                                                                           #
#Code written in gedit (pseudocode) and emacs (hard code)                               #
#Created Monday, September 13, 2013                                                     #
#Author: Michael Conroy                                                                 #
#Contact: sietekk@gmail.com                                                             #
#########################################################################################
set terminal jpeg transparent enhanced font "arial,10" fontscale 1.0 size 1024, 768
set output 'quadsolvplot.jpg'
set title 'Quadratic Function Plot Acccording to Entered Coefficients'
set xlabel 'x-axis'
set ylabel 'y-axis'
set grid
set key off
plot '/media/hdd/SCSU\ Fall\ 2013/PHY\ 499/Code/quadsolvplot.data' using 1:2 with linespoints ls 6 lc 7
	