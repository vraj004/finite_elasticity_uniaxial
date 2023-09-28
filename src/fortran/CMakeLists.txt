add_executable(${ExampleName}_Fortran finite_elasticity_uniaxial.F90)

target_link_libraries(${ExampleName}_Fortran PRIVATE opencmisslibs)

install(TARGETS ${ExampleName}_Fortran DESTINATION bin)

