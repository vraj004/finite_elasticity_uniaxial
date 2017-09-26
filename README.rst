

==========================
Template OpenCMISS example
==========================

This repository is intended to serve as a guide for setting up an OpenCMISS example.  The *master* branch is an over annotated example structure for a bare bones example structure try the *barebones* branch of this repository.

.. contents::
   :backlinks:

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

The CMake files describe how the example is to be configured and built.  The first CMakeLists.txt file that we add contains the minimum version of CMake that the example works with, the project statement where it is good practice to provide a version number for the example, the find package statement for finding the OpenCMISS libraries, and the sub-directory where the details of the executable are defined.  Below is how this all looks in the CMake language (also see the file *CMakeLists.txt* in the root directory).::

  # Specify the minimum version of CMake, OpenCMISS itself requires
  # at least version 3.4 of CMake so that will also constrian the 
  # minimum version we are able to set here.  We can't continue if this
  # is not the case so we may as well stop right here.
  cmake_mininum_version(VERSION 3.4 FATAL_ERROR)
  
  # Declare the project name and version number and specify the languages
  # used.  We must specify the *C* language irrespective of whether we use 
  # it or not as it is required by MPI.
  project(XXXXXXXX VERSION 1.0.0 LANGUAGES C Fortran)
  
  # Get CMake to find the OpenCMISS libraries.  Because the OpenCMISS libraries
  # are not usually available in the system directories we will have to 
  # specify where OpenCMISS libraries can be found.  We can do this through the
  # command line by setting the argument *OpenCMISSLibs_DIR* to the location
  # of the OpenCMISS libraries install directory.
  find_package(OpenCMISSLibs 1.3.0 REQUIRED CONFIG)
  
  # Add the subdirectory for further CMakeLists.txt files that define any
  # executables that are to be built.  Python examples do not have a configure
  # and build phase so CMake has no work to do thus we will not see any mention
  # of Python in the CMake files.
  add_subdirectory(src/fortran)

Inputs
======

If the example requries external inputs to be supplied these are stored in a directoy named *inputs*.

Expected results
================

If the example has some expected results these are stored in a directory named *expected_results*.

Documentation
=============

The documentation should be written in re-structured text a basic Sphinx configuration file is provided.
