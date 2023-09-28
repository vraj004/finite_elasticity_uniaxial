# Install script for directory: /home/unimelb.edu.au/vrajagopal/applications/opencmiss-examples/finite_elasticity_uniaxial/src/fortran

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran"
         RPATH "/home/unimelb.edu.au/vrajagopal/applications/opencmiss/install/x86_64_linux/intel-C20.2-intel-F20.2/no_mpi/lib:/home/unimelb.edu.au/vrajagopal/applications/opencmiss/install/x86_64_linux/intel-C20.2-intel-F20.2/intel_system/lib")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/home/unimelb.edu.au/vrajagopal/applications/opencmiss-examples/finite_elasticity_uniaxial/build/src/fortran/finite_elasticity_uniaxial_Fortran")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran"
         OLD_RPATH "/home/unimelb.edu.au/vrajagopal/applications/opencmiss/install/x86_64_linux/intel-C20.2-intel-F20.2/intel_system/lib:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"
         NEW_RPATH "/home/unimelb.edu.au/vrajagopal/applications/opencmiss/install/x86_64_linux/intel-C20.2-intel-F20.2/no_mpi/lib:/home/unimelb.edu.au/vrajagopal/applications/opencmiss/install/x86_64_linux/intel-C20.2-intel-F20.2/intel_system/lib")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/finite_elasticity_uniaxial_Fortran")
    endif()
  endif()
endif()

