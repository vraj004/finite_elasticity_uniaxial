!> \file
!> \author Vijay Rajagopal
!> \brief This is an example program to simulate uniaxial extension of a unit cube using OpenCMISS calls.
!>
!> \section LICENSE
!>
!> Version: MPL 1.1/GPL 2.0/LGPL 2.1
!>
!> The contents of this file are subject to the Mozilla Public License
!> Version 1.1 (the "License"); you may not use this file except in
!> compliance with the License. You may obtain a copy of the License at
!> http://www.mozilla.org/MPL/
!>
!> Software distributed under the License is distributed on an "AS IS"
!> basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
!> License for the specific language governing rights and limitations
!> under the License.
!>
!> The Original Code is OpenCMISS
!>
!> The Initial Developer of the Original Code is University of Auckland, 
!> Auckland, New Zealand and University of Oxford, Oxford, United
!> Kingdom. Portions created by the University of Auckland and University
!> of Oxford are Copyright (C) 2007 by the University of Auckland and
!> the University of Oxford. All Rights Reserved.
!>
!> Contributor(s): Vijay Rajagopal
!>
!> Alternatively, the contents of this file may be used under the terms of
!> either the GNU General Public License Version 2 or later (the "GPL"), or
!> the GNU Lesser General Public License Version 2.1 or later (the "LGPL"), 
!> in which case the provisions of the GPL or the LGPL are applicable instead
!> of those above. If you wish to allow use of your version of this file only
!> under the terms of either the GPL or the LGPL, and not to allow others to
!> use your version of this file under the terms of the MPL, indicate your
!> decision by deleting the provisions above and replace them with the notice
!> and other provisions required by the GPL or the LGPL. If you do not delete
!> the provisions above, a recipient may use your version of this file under
!> the terms of any one of the MPL, the GPL or the LGPL.
!>

!> \example FiniteElasticity/UniAxialExtension/src/UniAxialExtensionExample.f90
!! Example program to solve a finite elasticity equation using OpenCMISS calls.
!! \par Latest Builds:
!! \li < a href='http://autotest.bioeng.auckland.ac.nz/opencmiss-build/logs_x86_64-linux/FiniteElasticity/UniAxialExtension/build-intel'>Linux Intel Build</a>
!! \li < a href='http://autotest.bioeng.auckland.ac.nz/opencmiss-build/logs_x86_64-linux/FiniteElasticity/UniAxialExtension/build-gnu'>Linux GNU Build</a>
!<

!> Main program
PROGRAM FINITE_ELASTICITY_UNIAXIAL_EXAMPLE

  USE OpenCMISS
  USE OpenCMISS_Iron
!#ifndef NOMPIMOD
  USE MPI
!#endif

!#ifdef WIN32
!  USE IFQWIN
!#endif

  IMPLICIT NONE

!#ifdef NOMPIMOD
!#include "mpif.h"
!#endif


  !Test program parameters

  !\todo: don't hard code, read in+default


  REAL(CMISSRP), PARAMETER:: HEIGHT = 1.0_CMISSRP
  REAL(CMISSRP), PARAMETER:: WIDTH = 1.0_CMISSRP
  REAL(CMISSRP), PARAMETER:: DEPTH = 1.0_CMISSRP
  REAL(CMISSRP), PARAMETER:: C1 = 2.0_CMISSRP
  REAL(CMISSRP), PARAMETER:: C2 = 6.0_CMISSRP
  INTEGER(CMISSIntg), PARAMETER:: NumberOfSpatialCoordinates = 3
  
  INTEGER(CMISSIntg), PARAMETER:: ContextUserNumber = 1 
  INTEGER(CMISSIntg), PARAMETER:: CoordinateSystemUserNumber = 1

  INTEGER(CMISSIntg), PARAMETER:: RegionUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: QuadraticBasisUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: LinearBasisUserNumber = 2
  INTEGER(CMISSIntg), PARAMETER:: DecompositionUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: DecomposerUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: MeshUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: QuadraticMeshComponentNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: LinearMeshComponentNumber = 2
  INTEGER(CMISSIntg), PARAMETER:: FieldGeometryUserNumber = 1

  INTEGER(CMISSIntg), PARAMETER:: FieldGeometryNumberOfVariables = 1
  INTEGER(CMISSIntg), PARAMETER:: FieldGeometryNumberOfComponents = 3

  INTEGER(CMISSIntg), PARAMETER:: FieldFibreUserNumber = 2
  INTEGER(CMISSIntg), PARAMETER:: FieldFibreNumberOfVariables = 1
  INTEGER(CMISSIntg), PARAMETER:: FieldFibreNumberOfComponents = 3

  INTEGER(CMISSIntg), PARAMETER:: FieldMaterialUserNumber = 3
  INTEGER(CMISSIntg), PARAMETER:: FieldMaterialNumberOfVariables = 1
  INTEGER(CMISSIntg), PARAMETER:: FieldMaterialNumberOfComponents = 2

  INTEGER(CMISSIntg), PARAMETER:: FieldDependentUserNumber = 4
  INTEGER(CMISSIntg), PARAMETER:: FieldDependentNumberOfVariables = 2
  INTEGER(CMISSIntg), PARAMETER:: FieldDependentNumberOfComponents = 4


  INTEGER(CMISSIntg), PARAMETER:: EquationSetUserNumber = 1
  INTEGER(CMISSIntg), PARAMETER:: EquationsSetFieldUserNumber = 5
  INTEGER(CMISSIntg), PARAMETER:: ProblemUserNumber = 1

  !Program types


  !Program variables
  INTEGER(CMISSIntg):: MPI_IERROR, IO
  INTEGER(CMISSIntg):: EquationsSetIndex, DecompositionIndex 
  INTEGER(CMISSIntg):: NumberOfComputationalNodes, NumberOfDomains, ComputationalNodeNumber
  INTEGER(CMISSIntg) ::   NumberGlobalXElements, NumberGlobalYElements, NumberGlobalZElements


  !CMISS variables
  TYPE(cmfe_ComputationEnvironmentType):: computationEnvironment
  TYPE(cmfe_ContextType):: Context
  TYPE(cmfe_BasisType):: QuadraticBasis, LinearBasis
  TYPE(cmfe_BoundaryConditionsType):: BoundaryConditions
  TYPE(cmfe_CoordinateSystemType):: CoordinateSystem, WorldCoordinateSystem
  TYPE(cmfe_MeshType):: Mesh
  TYPE(cmfe_MeshElementsType):: QuadMeshElements, LinMeshElements
  TYPE(cmfe_DecompositionType):: Decomposition
  TYPE(cmfe_DecomposerType):: Decomposer
  TYPE(cmfe_EquationsType):: Equations
  TYPE(cmfe_EquationsSetType):: EquationsSet
  TYPE(cmfe_FieldType):: GeometricField, FibreField, MaterialField
  TYPE(cmfe_FieldType):: DependentField, EquationsSetField, AnalyticField
  TYPE(cmfe_FieldsType):: Fields
  TYPE(cmfe_ProblemType):: Problem
  TYPE(cmfe_RegionType):: Region, WorldRegion
  TYPE(cmfe_SolverType):: Solver, LinearSolver
  TYPE(cmfe_SolverEquationsType):: SolverEquations
  TYPE(cmfe_NodesType):: Nodes
  TYPE(cmfe_ControlLoopType):: ControlLoop
  TYPE(cmfe_WorkGroupType):: worldWorkGroup
  !Other variables
  INTEGER(CMISSIntg):: NE, E, NumNodes, NumElements, Dimensions, line, loopiter, chariter, NodeNumCharLen, NodeNum, i, j, k
  INTEGER(CMISSIntg), ALLOCATABLE:: FileArray(:,:), NodeNumbers(:), ElemArray(:,:), LinElemArray(:,:)
  REAL(CMISSDP), ALLOCATABLE:: NodesArray(:,:)
  REAL(CMISSDP):: zinc, xinc, yinc, x, y, z
  CHARACTER(LEN = 33), DIMENSION(3048):: elemtype
  INTEGER(CMISSIntg):: Err, status, readlinepos
  
  !#ifdef WIN32
  !Quickwin type
  !LOGICAL:: QUICKWIN_STATUS=.FALSE.
  !TYPE(WINDOWCONFIG):: QUICKWIN_WINDOW_CONFIG
!#endif

  !Generic CMISS variables

!#ifdef WIN32
  !Initialise QuickWin
  !QUICKWIN_WINDOW_CONFIG%TITLE="General Output" !Window title
  !QUICKWIN_WINDOW_CONFIG%NUMTEXTROWS = -1  ! Max possible number of rows
  !QUICKWIN_WINDOW_CONFIG%MODE = QWIN$SCROLLDOWN
  !Set the window parameters
  !QUICKWIN_STATUS = SETWINDOWCONFIG(QUICKWIN_WINDOW_CONFIG)
  !If attempt fails set with system estimated values
  !IF(.NOT.QUICKWIN_STATUS) QUICKWIN_STATUS = SETWINDOWCONFIG(QUICKWIN_WINDOW_CONFIG)
!#endif

  !Intialise cmiss
  CALL cmfe_Initialise(Err)

  CALL cmfe_ErrorHandlingModeSet(CMFE_ERRORS_TRAP_ERROR, Err)

  WRITE(*,'(A)') "Program starting."

  !Set all diganostic levels on for testing
  !CALL cmfe_DiagnosticsSetOn(CMFE_FROM_DIAG_TYPE, [1, 2, 3, 4, 5],"Diagnostics",["PROBLEM_FINITE_ELEMENT_CALCULATE"],Err)

  CALL cmfe_Context_Initialise(Context, Err)
  CALL cmfe_Context_Create(ContextUserNumber, Context, Err)
  CALL cmfe_Region_Initialise(worldRegion, Err)
  CALL cmfe_Context_WorldRegionGet(Context, worldRegion, Err)
 
  !Get the number of computational nodes and this computational node number
  CALL cmfe_ComputationEnvironment_Initialise(computationEnvironment, Err)
  CALL cmfe_Context_computationEnvironmentGet(Context, computationEnvironment, Err)
  CALL cmfe_WorkGroup_Initialise(worldWorkGroup, Err)
  CALL cmfe_ComputationEnvironment_WorldWorkGroupGet(computationEnvironment, worldWorkGroup, Err)
  CALL cmfe_WorkGroup_NumberOfGroupNodesGet(worldWorkGroup, NumberOfComputationalNodes, Err)
  CALL cmfe_WorkGroup_GroupNodeNumberGet(worldWorkGroup, ComputationalNodeNumber, Err)

  write(*,*) "NumberOfDomains=",NumberOfComputationalNodes
  NumberOfDomains = NumberOfComputationalNodes  ! 1

  !Broadcast the number of elements in the X, Y and Z directions and the number of partitions to the other computational nodes
  !CALL MPI_BCAST(NumberGlobalXElements, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, MPI_IERROR)  ! NOW A PARAMETER
  !CALL MPI_BCAST(NumberGlobalYElements, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, MPI_IERROR)  ! NOW A PARAMETER
  !CALL MPI_BCAST(NumberGlobalZElements, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, MPI_IERROR)  ! NOW A PARAMETER 
  !CALL MPI_BCAST(NumberOfDomains, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, MPI_IERROR)
  
  write(*,*) "Create a CS-default is 3D rectangular cartesian CS with 0, 0, 0 as origin"
  CALL cmfe_CoordinateSystem_Initialise(CoordinateSystem, Err)
  CALL cmfe_CoordinateSystem_CreateStart(CoordinateSystemUserNumber, Context, CoordinateSystem, Err)
  !CALL cmfe_CoordinateSystem_TypeSet(CoordinateSystem, CMFE_COORDINATE_RECTANGULAR_CARTESIAN_TYPE, Err)
  CALL cmfe_CoordinateSystem_DimensionSet(CoordinateSystem, NumberOfSpatialCoordinates, Err)
  CALL cmfe_CoordinateSystem_OriginSet(CoordinateSystem, [0.0_CMISSRP, 0.0_CMISSRP, 0.0_CMISSRP],Err)
  CALL cmfe_CoordinateSystem_CreateFinish(CoordinateSystem, Err)

  write(*,*) "Create a region and assign the CS to the region"
  CALL cmfe_Region_Initialise(Region, Err)
  CALL cmfe_Region_CreateStart(RegionUserNumber, WorldRegion, Region, Err)
  CALL cmfe_Region_CoordinateSystemSet(Region, CoordinateSystem, Err)
  CALL cmfe_Region_CreateFinish(Region, Err)

  write(*,*) "Define basis functions-tri-linear Lagrange and tri-Quadratic Lagrange"
  CALL cmfe_Basis_Initialise(LinearBasis, Err)
  CALL cmfe_Basis_CreateStart(LinearBasisUserNumber, Context, LinearBasis, Err)
  CALL cmfe_Basis_QuadratureNumberOfGaussXiSet(LinearBasis, &
    & [CMFE_BASIS_MID_QUADRATURE_SCHEME, CMFE_BASIS_MID_QUADRATURE_SCHEME, CMFE_BASIS_MID_QUADRATURE_SCHEME],Err)
  CALL cmfe_Basis_QuadratureLocalFaceGaussEvaluateSet(LinearBasis, .true.,Err)  ! Have to do this (unused) due to field_interp setup
  CALL cmfe_Basis_CreateFinish(LinearBasis, Err)

  CALL cmfe_Basis_Initialise(QuadraticBasis, Err)
  CALL cmfe_Basis_CreateStart(QuadraticBasisUserNumber, Context, QuadraticBasis, Err)
  CALL cmfe_Basis_InterpolationXiSet(QuadraticBasis, [CMFE_BASIS_QUADRATIC_LAGRANGE_INTERPOLATION, &
    & CMFE_BASIS_QUADRATIC_LAGRANGE_INTERPOLATION, CMFE_BASIS_QUADRATIC_LAGRANGE_INTERPOLATION],Err)
  CALL cmfe_Basis_QuadratureNumberOfGaussXiSet(QuadraticBasis, &
    & [CMFE_BASIS_MID_QUADRATURE_SCHEME, CMFE_BASIS_MID_QUADRATURE_SCHEME, CMFE_BASIS_MID_QUADRATURE_SCHEME],Err)
  CALL cmfe_Basis_QuadratureLocalFaceGaussEvaluateSet(QuadraticBasis, .true.,Err)  ! Enable 3D interpolation on faces
  CALL cmfe_Basis_CreateFinish(QuadraticBasis, Err)

  WRITE(*,*) 'Creating nodes for unit cube'
  NumElements = 1
  zinc = HEIGHT/3.0_CMISSDP
  yinc = DEPTH/3.0_CMISSDP
  xinc = WIDTH/3.0_CMISSDP
  NumNodes = 27
  Dimensions = 3
  ALLOCATE(ElemArray(NumElements, NumNodes))
  ALLOCATE(LinElemArray(NumElements, 8))
  ALLOCATE(NodesArray(NumNodes, Dimensions))
  ALLOCATE(NodeNumbers(NumNodes))

  CALL cmfe_Nodes_Initialise(Nodes, Err)
  CALL cmfe_Nodes_CreateStart(Region, NumNodes, Nodes, Err)
  CALL cmfe_Nodes_CreateFinish(Nodes,  Err)
  WRITE(*,*) 'Defining node coordinates for mesh: ', NumNodes 
  NodeNum = 0
  z = 0_CMISSDP
  DO k = 1, 3
    y = 0_CMISSDP
    DO j = 1, 3
      x = 0_CMISSDP
      DO i = 1, 3
        NodeNum = NodeNum+1
        NodeNumbers(NodeNum) = NodeNum
        NodesArray(NodeNum, :) = [x, y, z]
        x = x+xinc
      ENDDO
      y = y+yinc
    ENDDO
    z = z+zinc
  ENDDO


  WRITE(*,*) 'Creating elements'
  DO loopiter = 1, NumElements
    ElemArray(loopiter, :) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27]
    LinElemArray(loopiter, :) = [1, 3, 7, 9, 19, 21, 25, 27]
  ENDDO

  WRITE(*,*) "Finish the creation of mesh in the region"
  CALL cmfe_Mesh_Initialise(Mesh, Err)
  CALL cmfe_MeshElements_Initialise(QuadMeshElements, Err)
  CALL cmfe_Mesh_CreateStart(MeshUserNumber, Region, 3, Mesh, Err)
  CALL cmfe_Mesh_NumberOfComponentsSet(Mesh, 2, Err)  
  CALL cmfe_Mesh_NumberOfElementsSet(Mesh, NumElements, Err)
  CALL cmfe_MeshElements_CreateStart(Mesh, 1, QuadraticBasis, QuadMeshElements, Err)
  CALL cmfe_MeshElements_CreateStart(Mesh, 2, LinearBasis, LinMeshElements, Err)
  DO loopiter = 1, NumElements
    WRITE(*,*) 'Element being defined: ',loopiter
    CALL cmfe_MeshElements_NodesSet(QuadMeshElements, loopiter,  ElemArray(loopiter, 1:27), Err)
    CALL cmfe_MeshElements_NodesSet(LinMeshElements, loopiter, LinElemArray(loopiter, 1:8), Err)
  ENDDO
  CALL cmfe_MeshElements_CreateFinish(QuadMeshElements, Err)
  CALL cmfe_MeshElements_createFinish(LinMeshElements, Err)
  CALL cmfe_Mesh_CreateFinish(Mesh, Err)
  write(*,*) "Create a decomposition"
  !CALL cmfe_RandomSeedsSet(0_CMISSIntg, Err)  ! To keep the automatic decomposition same each time
  CALL cmfe_Decomposition_Initialise(Decomposition, Err)
  CALL cmfe_Decomposition_CreateStart(DecompositionUserNumber, Mesh, Decomposition, Err)
  !Automatic decomposition
  !CALL cmfe_Decomposition_TypeSet(Decomposition, CMFE_DECOMPOSITION_CALCULATED_TYPE, Err)
  !CALL cmfe_Decomposition_NumberOfDomainsSet(Decomposition, NumberOfDomains, Err)
  !Manual decomposition
  !IF(NumberOfDomains > 1) THEN
  !  CALL cmfe_Decomposition_TypeSet(Decomposition, CMFE_DECOMPOSITION_USER_DEFINED_TYPE, Err)
  !  !Set all elements but last one to first domain
  !  CALL cmfe_Mesh_NumberOfElementsGet(Mesh, NE, Err)
  !  do E = 1, NE/2
  !    CALL cmfe_Decomposition_ElementDomainSet(Decomposition, E, 0, Err)
  !  enddo
  !  do E = NE/2+1, NE
  !    CALL cmfe_Decomposition_ElementDomainSet(Decomposition, E, 1, Err)
  !  enddo
  !  CALL cmfe_Decomposition_NumberOfDomainsSet(Decomposition, NumberOfDomains, Err)
  !ENDIF
  !CALL cmfe_Decomposition_CalculateFacesSet(Decomposition, .TRUE.,Err)
  CALL cmfe_Decomposition_CreateFinish(Decomposition, Err)

  write(*,*) "Decompose"
  CALL cmfe_Decomposer_Initialise(Decomposer, Err)
  CALL cmfe_Decomposer_CreateStart(DecomposerUserNumber, Region, worldWorkGroup, Decomposer, Err)
  CALL cmfe_Decomposer_DecompositionAdd(Decomposer, Decomposition, DecompositionIndex, Err)
  CALL cmfe_Decomposer_CreateFinish(Decomposer, Err)
  
  write(*,*) "Create a field to put the geometry (default is geometry)"
  CALL cmfe_Field_Initialise(GeometricField, Err)
  CALL cmfe_Field_CreateStart(FieldGeometryUserNumber, Region, GeometricField, Err)
  CALL cmfe_Field_DecompositionSet(GeometricField, Decomposition, Err)
  CALL cmfe_Field_TypeSet(GeometricField, CMFE_FIELD_GEOMETRIC_TYPE, Err)  
  CALL cmfe_Field_NumberOfVariablesSet(GeometricField, FieldGeometryNumberOfVariables, Err)
  CALL cmfe_Field_NumberOfComponentsSet(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, FieldGeometryNumberOfComponents, Err)  
  CALL cmfe_Field_ComponentMeshComponentSet(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, 1, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, 2, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, 3, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_VariableLabelSet(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, "Geometry",Err)
  CALL cmfe_Field_ScalingTypeSet(GeometricField, CMFE_FIELD_UNIT_SCALING, Err)
  CALL cmfe_Field_CreateFinish(GeometricField, Err)

  write(*,*) "Update the geometric field parameters"
  DO loopiter = 1, NumNodes
  
    CALL cmfe_Field_ParameterSetUpdateNode(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
            &  1, 1, NodeNumbers(loopiter), 1, NodesArray(loopiter, 1), Err)

    CALL cmfe_Field_ParameterSetUpdateNode(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
            & 1, 1, NodeNumbers(loopiter), 2, NodesArray(loopiter, 2), Err)

    CALL cmfe_Field_ParameterSetUpdateNode(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
            & 1, 1, NodeNumbers(loopiter), 3, NodesArray(loopiter, 3), Err)


  ENDDO
  
  write(*,*) "Create a fibre field and attach it to the geometric field" 
  CALL cmfe_Field_Initialise(FibreField, Err)
  CALL cmfe_Field_CreateStart(FieldFibreUserNumber, Region, FibreField, Err)
  CALL cmfe_Field_TypeSet(FibreField, CMFE_FIELD_FIBRE_TYPE, Err)
  CALL cmfe_Field_DecompositionSet(FibreField, Decomposition, Err)        
  CALL cmfe_Field_GeometricFieldSet(FibreField, GeometricField, Err)
  CALL cmfe_Field_NumberOfVariablesSet(FibreField, FieldFibreNumberOfVariables, Err)
  CALL cmfe_Field_NumberOfComponentsSet(FibreField, CMFE_FIELD_U_VARIABLE_TYPE, FieldFibreNumberOfComponents, Err)  
  CALL cmfe_Field_ComponentMeshComponentSet(FibreField, CMFE_FIELD_U_VARIABLE_TYPE, 1, LinearMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(FibreField, CMFE_FIELD_U_VARIABLE_TYPE, 2, LinearMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(FibreField, CMFE_FIELD_U_VARIABLE_TYPE, 3, LinearMeshComponentNumber, Err)
  CALL cmfe_Field_VariableLabelSet(FibreField, CMFE_FIELD_U_VARIABLE_TYPE, "Fibre",Err)
  CALL cmfe_Field_ScalingTypeSet(FibreField, CMFE_FIELD_UNIT_SCALING, Err)
  CALL cmfe_Field_CreateFinish(FibreField, Err)

  write(*,*) "Create a material field and attach it to the geometric field"  
  CALL cmfe_Field_Initialise(MaterialField, Err)
  CALL cmfe_Field_CreateStart(FieldMaterialUserNumber, Region, MaterialField, Err)
  CALL cmfe_Field_TypeSet(MaterialField, CMFE_FIELD_MATERIAL_TYPE, Err)
  CALL cmfe_Field_DecompositionSet(MaterialField, Decomposition, Err)        
  CALL cmfe_Field_GeometricFieldSet(MaterialField, GeometricField, Err)
  CALL cmfe_Field_NumberOfVariablesSet(MaterialField, FieldMaterialNumberOfVariables, Err)
  CALL cmfe_Field_NumberOfComponentsSet(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, FieldMaterialNumberOfComponents, Err)  
  CALL cmfe_Field_ComponentInterpolationSet(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, 1, CMFE_FIELD_CONSTANT_INTERPOLATION, Err)
  CALL cmfe_Field_ComponentInterpolationSet(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, 2, CMFE_FIELD_CONSTANT_INTERPOLATION, Err)
  CALL cmfe_Field_VariableLabelSet(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, "Material",Err)
  CALL cmfe_Field_ScalingTypeSet(MaterialField, CMFE_FIELD_UNIT_SCALING, Err)
  CALL cmfe_Field_CreateFinish(MaterialField, Err)

  write(*,*) "Set Mooney-Rivlin constants c10 and c01 to 2.0 and 6.0 respectively."
  CALL cmfe_Field_ComponentValuesInitialise(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 1, C1, Err)
  CALL cmfe_Field_ComponentValuesInitialise(MaterialField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 2, C2, Err)

  write(*,*) "Create the equations_set"
  CALL cmfe_Field_Initialise(EquationsSetField, Err)
  CALL cmfe_EquationsSet_Initialise(EquationsSet, Err)
  CALL cmfe_EquationsSet_CreateStart(EquationSetUserNumber, Region, FibreField, [CMFE_EQUATIONS_SET_ELASTICITY_CLASS, &
    & CMFE_EQUATIONS_SET_FINITE_ELASTICITY_TYPE, CMFE_EQUATIONS_SET_MOONEY_RIVLIN_SUBTYPE],EquationsSetFieldUserNumber, &
    & EquationsSetField, EquationsSet, Err)
  CALL cmfe_EquationsSet_CreateFinish(EquationsSet, Err)

  write(*,*) "Create the dependent field with 2 variables and 4 components (3 displacement, 1 pressure)"
  CALL cmfe_Field_Initialise(DependentField, Err)
  CALL cmfe_Field_CreateStart(FieldDependentUserNumber, Region, DependentField, Err)
  CALL cmfe_Field_TypeSet(DependentField, CMFE_FIELD_GEOMETRIC_GENERAL_TYPE, Err)
  CALL cmfe_Field_DecompositionSet(DependentField, Decomposition, Err)
  CALL cmfe_Field_GeometricFieldSet(DependentField, GeometricField, Err)
  CALL cmfe_Field_DependentTypeSet(DependentField, CMFE_FIELD_DEPENDENT_TYPE, Err)
  CALL cmfe_Field_NumberOfVariablesSet(DependentField, FieldDependentNumberOfVariables, Err)
  CALL cmfe_Field_NumberOfComponentsSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, FieldDependentNumberOfComponents, Err)
  CALL cmfe_Field_NumberOfComponentsSet(DependentField, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, FieldDependentNumberOfComponents, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, 1, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, 2, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, 3, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, 4, LinearMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 1, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 2, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 3, QuadraticMeshComponentNumber, Err)
  CALL cmfe_Field_ComponentMeshComponentSet(DependentField, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 4, LinearMeshComponentNumber, Err)
  CALL cmfe_Field_ScalingTypeSet(DependentField, CMFE_FIELD_UNIT_SCALING, Err)
  CALL cmfe_Field_VariableLabelSet(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, "Dependent",Err)
  CALL cmfe_Field_CreateFinish(DependentField, Err)

  CALL cmfe_EquationsSet_DependentCreateStart(EquationsSet, FieldDependentUserNumber, DependentField, Err)
  CALL cmfe_EquationsSet_DependentCreateFinish(EquationsSet, Err)

  CALL cmfe_EquationsSet_MaterialsCreateStart(EquationsSet, FieldMaterialUserNumber, MaterialField, Err)  
  CALL cmfe_EquationsSet_MaterialsCreateFinish(EquationsSet, Err)


  write(*,*) "Create the equations set equations"
  CALL cmfe_Equations_Initialise(Equations, Err)
  CALL cmfe_EquationsSet_EquationsCreateStart(EquationsSet, Equations, Err)
  CALL cmfe_Equations_SparsityTypeSet(Equations, CMFE_EQUATIONS_SPARSE_MATRICES, Err)
  CALL cmfe_Equations_OutputTypeSet(Equations, CMFE_EQUATIONS_NO_OUTPUT, Err)
  CALL cmfe_EquationsSet_EquationsCreateFinish(EquationsSet, Err)   

  write(*,*) "Initialise dependent field from undeformed geometry and displacement bcs and set hydrostatic pressure"
  CALL cmfe_Field_ParametersToFieldParametersComponentCopy(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
    & 1, DependentField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 1, Err)
  CALL cmfe_Field_ParametersToFieldParametersComponentCopy(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
    & 2, DependentField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 2, Err)
  CALL cmfe_Field_ParametersToFieldParametersComponentCopy(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
    & 3, DependentField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 3, Err)
  CALL cmfe_Field_ComponentValuesInitialise(DependentField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, 4, &
    & -14.0_CMISSRP, &
    & Err)

  write(*,*) "Define the problem"
  CALL cmfe_Problem_Initialise(Problem, Err)
  CALL cmfe_Problem_CreateStart(ProblemUserNumber, Context, [CMFE_PROBLEM_ELASTICITY_CLASS, & 
          & CMFE_PROBLEM_FINITE_ELASTICITY_TYPE, CMFE_PROBLEM_NO_SUBTYPE], &
          & Problem, Err)
  CALL cmfe_Problem_CreateFinish(Problem, Err)

  !Create the problem control loop
  CALL cmfe_Problem_ControlLoopCreateStart(Problem, Err)
  CALL cmfe_ControlLoop_Initialise(ControlLoop, Err)
  CALL cmfe_Problem_ControlLoopGet(Problem, CMFE_CONTROL_LOOP_NODE, ControlLoop, Err)
  CALL cmfe_ControlLoop_MaximumIterationsSet(ControlLoop, 1, Err)  ! this one sets the increment loop counter
  CALL cmfe_Problem_ControlLoopCreateFinish(Problem, Err)
  
  !Create the problem solvers
  CALL cmfe_Solver_Initialise(Solver, Err)
  CALL cmfe_Solver_Initialise(LinearSolver, Err)
  CALL cmfe_Problem_SolversCreateStart(Problem, Err)
  CALL cmfe_Problem_SolverGet(Problem, CMFE_CONTROL_LOOP_NODE, 1, Solver, Err)
  CALL cmfe_Solver_OutputTypeSet(Solver, CMFE_SOLVER_PROGRESS_OUTPUT, Err)
  !CALL cmfe_Solver_NewtonJacobianCalculationTypeSet(Solver, CMFE_SOLVER_NEWTON_JACOBIAN_FD_CALCULATED, Err)  !Slower
  CALL cmfe_Solver_NewtonJacobianCalculationTypeSet(Solver, CMFE_SOLVER_NEWTON_JACOBIAN_EQUATIONS_CALCULATED, Err)
  CALL cmfe_Solver_NewtonRelativeToleranceSet(Solver, 1.0E-7_CMISSRP, Err)
  CALL cmfe_Solver_NewtonSolutionToleranceSet(Solver, 1.0E-7_CMISSRP, Err)
  CALL cmfe_Solver_NewtonLinearSolverGet(Solver, LinearSolver, Err)
  CALL cmfe_Solver_LinearTypeSet(LinearSolver, CMFE_SOLVER_LINEAR_DIRECT_SOLVE_TYPE, Err)
  CALL cmfe_Problem_SolversCreateFinish(Problem, Err)

  !Create the problem solver equations
  CALL cmfe_Solver_Initialise(Solver, Err)
  CALL cmfe_SolverEquations_Initialise(SolverEquations, Err)
  CALL cmfe_Problem_SolverEquationsCreateStart(Problem, Err)   
  CALL cmfe_Problem_SolverGet(Problem, CMFE_CONTROL_LOOP_NODE, 1, Solver, Err)
  CALL cmfe_Solver_SolverEquationsGet(Solver, SolverEquations, Err)
  CALL cmfe_SolverEquations_SparsityTypeSet(SolverEquations, CMFE_SOLVER_SPARSE_MATRICES, Err)
  CALL cmfe_SolverEquations_EquationsSetAdd(SolverEquations, EquationsSet, EquationsSetIndex, Err)
  CALL cmfe_Problem_SolverEquationsCreateFinish(Problem, Err)

!! MANUAL BC ASSIGNMENT-REPLACED BY THE ANALYTIC BC ROUTINE BELOW
!   !Prescribe boundary conditions (absolute nodal parameters)
!   CALL cmfe_BoundaryConditions_Initialise(BoundaryConditions, Err)
!   CALL cmfe_EquationsSetBoundaryConditionsCreateStart(EquationsSet, BoundaryConditions, Err)
! 
!   !Get surfaces-will fix two nodes on bottom face, pressure conditions inside
!   CALL cmfe_GeneratedMesh_SurfaceGet(GeneratedMesh, CMFE_GENERATED_MESH_CYLINDER_BOTTOM_SURFACE, BottomSurfaceNodes, BottomNormalXi, Err)
!   CALL cmfe_GeneratedMesh_SurfaceGet(GeneratedMesh, CMFE_GENERATED_MESH_CYLINDER_INNER_SURFACE, InnerSurfaceNodes, InnerNormalXi, Err)
!   CALL cmfe_GeneratedMesh_SurfaceGet(GeneratedMesh, CMFE_GENERATED_MESH_CYLINDER_OUTER_SURFACE, OuterSurfaceNodes, OuterNormalXi, Err)
! 
!   !Set all inner surface nodes to inner pressure
!   DO NN = 1, SIZE(InnerSurfaceNodes, 1)
!       CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 1, InnerSurfaceNodes(NN), &
!       & abs(InnerNormalXi), CMFE_BOUNDARY_CONDITION_PRESSURE_INCREMENTED, INNER_PRESSURE, Err)   ! INNER_PRESSURE
!     IF(Err /= 0) WRITE(*,*) "ERROR WHILE ASSIGNING INNER PRESSURE TO NODE", InnerSurfaceNodes(NN)
!   ENDDO
! 
!   !Set all outer surface nodes to outer pressure
!   DO NN = 1, SIZE(OuterSurfaceNodes, 1)
!     CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_DELUDELN_VARIABLE_TYPE, 1, OuterSurfaceNodes(NN), &
!       & abs(OuterNormalXi), CMFE_BOUNDARY_CONDITION_PRESSURE_INCREMENTED, OUTER_PRESSURE, Err)
!     IF(Err /= 0) WRITE(*,*) "ERROR WHILE ASSIGNING OUTER PRESSURE TO NODE", OuterSurfaceNodes(NN)
!   ENDDO
! 
!   !Set all top nodes fixed in z plane at the set height
!   DO NN = 1, SIZE(TopSurfaceNodes, 1)
!     CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_U_VARIABLE_TYPE, 1, TopSurfaceNodes(NN), &
!       & 3, CMFE_BOUNDARY_CONDITION_FIXED, deformedHeight, Err)
!     IF(Err /= 0) WRITE(*,*) "ERROR WHILE ASSIGNING FIXED CONDITION TO NODE", TopSurfaceNodes(NN)
!   ENDDO
! 
!   !Set all bottom nodes fixed in z plane
!   DO NN = 1, SIZE(BottomSurfaceNodes, 1)
!     CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_U_VARIABLE_TYPE, 1, BottomSurfaceNodes(NN), &
!       & 3, CMFE_BOUNDARY_CONDITION_FIXED, 0.0_CMISSRP, Err)
!     IF(Err /= 0) WRITE(*,*) "ERROR WHILE ASSIGNING FIXED CONDITION TO NODE", BottomSurfaceNodes(NN)
!   ENDDO
! 
!   !Set two nodes on the bottom surface to axial displacement only
!   X_FIXED=.FALSE.
!   Y_FIXED=.FALSE.
!   DO NN = 1, SIZE(BottomSurfaceNodes, 1)
!     IF (.NOT.X_FIXED) THEN
!       CALL cmfe_Field_ParameterSetGetNode(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
!         & 1, BottomSurfaceNodes(NN), 1, xValue, Err)
!       IF(abs(xValue)<1e-5_CMISSRP) THEN
!         !Constrain it in x direction
!         CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_U_VARIABLE_TYPE, 1, BottomSurfaceNodes(NN), 1, &
!           & CMFE_BOUNDARY_CONDITION_FIXED, 0.0_CMISSRP, Err)
!         X_FIXED=.TRUE.
!         WRITE(*,*) "CyliderInflationExample: SUCCESSFULLY CONSTRAINED IN X DIRECTION NODE",BottomSurfaceNodes(NN)
!       ENDIF
!     ENDIF
!     IF(.NOT.Y_FIXED) THEN
!       CALL cmfe_Field_ParameterSetGetNode(GeometricField, CMFE_FIELD_U_VARIABLE_TYPE, CMFE_FIELD_VALUES_SET_TYPE, &
!         & 1, BottomSurfaceNodes(NN), 2, yValue, Err)
!       IF(abs(yValue)<1e-5_CMISSRP) THEN
!         !Constrain it in y direction
!         CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions, CMFE_FIELD_U_VARIABLE_TYPE, 1, BottomSurfaceNodes(NN), 2, &
!           & CMFE_BOUNDARY_CONDITION_FIXED, 0.0_CMISSRP, Err)
!         Y_FIXED=.TRUE.
!         WRITE(*,*) "CyliderInflationExample: SUCCESSFULLY CONSTRAINED IN Y DIRECTION NODE",BottomSurfaceNodes(NN)
!       ENDIF
!     ENDIF
!     IF (X_FIXED .AND. Y_FIXED) EXIT
!   ENDDO
!   !Check
!   IF(.NOT.X_FIXED .OR. .NOT.Y_FIXED) THEN
!     Write(*,*) "Couldn't fix bottom surface. No node lies on x or y axis, try changing number of elements"// &
!       & " in theta coordinate"
!     STOP
!   ENDIF
! 
!   CALL cmfe_EquationsSetBoundaryConditionsCreateFinish(EquationsSet, Err)

!! END OF MANUAL BC ASSIGNMENT

  !Set the bc using the analytic solution routine
!  CALL cmfe_BoundaryConditions_Initialise(BoundaryConditions, Err)
!  CALL cmfe_SolverEquations_BoundaryConditionsCreateStart(SolverEquations, BoundaryConditions, Err)
!  CALL cmfe_SolverEquations_BoundaryConditionsAnalytic(SolverEquations, Err)
!  CALL cmfe_SolverEquations_BoundaryConditionsCreateFinish(SolverEquations, Err)

  !Solve problem
  CALL cmfe_Problem_Solve(Problem, Err)

  !Output Analytic analysis
!  Call cmfe_AnalyticAnalysis_Output(DependentField, "outputs/CylinderInflation",Err)

  !Output solution  
  CALL cmfe_Fields_Initialise(Fields, Err)
  CALL cmfe_Fields_Create(Region, Fields, Err)
  CALL cmfe_Fields_NodesExport(Fields, "outputs/CylinderInflation","FORTRAN",Err)
  CALL cmfe_Fields_ElementsExport(Fields, "outputs/CylinderInflation","FORTRAN",Err)
  CALL cmfe_Fields_Finalise(Fields, Err)
  
  !Destroy context
  CALL cmfe_Context_Destroy(Context, Err)
  CALL cmfe_Finalise(Err)

  WRITE(*,'(A)') "Program successfully completed."

  STOP

END PROGRAM FINITE_ELASTICITY_UNIAXIAL_EXAMPLE


