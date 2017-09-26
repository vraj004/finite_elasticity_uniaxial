

==========================
Template OpenCMISS example
==========================

This repository is intended to serve as a guide for setting up an OpenCMISS example.  The *master* branch is an over annotated example structure for a bare bones example structure try the *barebones* branch of this repository.

Directory structure
===================

The basic directory structure for an example is to have two child directories *docs* and *src*.  The *docs* directory contains the documentation on the example and the *src* directory contains the source code required to build the example.  The *src* directory can be further split into language subdirectories; *python*, *c*, *fortran*.  These are only required if that language is implemented by the example::

    .
    +-- docs
    +-- src
        +-- c
        +-- fortran
        +-- python


Example executable
==================

The example should create an executable that can be run, how we configure and build the executable will be explained in the the `CMake files` section.  The code required to create the executable is placed into the correspoding language directory inside the *src* directory.  For instance if we are creating a Fortran example with the code written in the file *fluid_flow.F90* this file belongs in *src/fortran*.

CMake files
===========

The CMake files describe how the example is to be configured and built.  The first CMakeLists.txt file that we add contains the minimum version of CMake that the example works with, the project statement where it is good practive to provide a version number for the example, the find package statement for finding the OpenCMISS libraries, and the sub-directory where the details of the executable are defined.

.. literalinclude::
   CMakeLists.txt


Inputs
======

If the example requries external inputs to be supplied these arguments are described in the inputs.txt file.

Documentation
=============

The documentation should be written in re-structured text a basic Sphinx configuration file is provided.
