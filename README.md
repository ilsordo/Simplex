# Simplex

#### Maxime LESOURD
#### Yassine HAMOUDI

https://github.com/nagaaym/Simplex

******************************************************************************

1. Dependences
2. Compile and run
3. Produce a PDF file
4. Options

******************************************************************************


1. Dependences
==============


2. Compile and run
==================

Compile the program:

  make

Run the simplex on the file test.lp:

  ./simplex test.lp

3. Produce a PDF file
=====================

You can produce a PDF file describing the execution of the simplex. To print it into record.pdf enter the option:

  -print record.pdf

This file contains:
  - the different steps of the algorithm (dictionaries, entering/leaving variables)
  - the dual
  - the solution
  
4. Other options
================

Display the help:

  --help

Use floating point numbers (even into the input):

  -field float

Use gmp numbers (bit integers):

  -flied gmp

Display some statistics:

  -d
    
Display some timers:

  -p
