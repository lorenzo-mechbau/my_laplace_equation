! Being used to test trees and HASH
PROGRAM LAPLACE_EQUATION

  USE OpenCMISS
  USE OpenCMISS_Iron
  USE hash_routines

#ifndef NOMPIMOD
  USE MPI
#endif

#ifdef WIN32
  USE IFQWIN
#endif

  IMPLICIT NONE

#ifdef NOMPIMOD
#include "mpif.h"
#endif

  !-----------------------------------------------------------------------------------------------------------
  ! PROGRAM VARIABLES AND TYPES
  !-----------------------------------------------------------------------------------------------------------
  
  !Test program parameters
  REAL(CMISSRP), PARAMETER :: HEIGHT=1.0_CMISSRP
  REAL(CMISSRP), PARAMETER :: WIDTH=1.0_CMISSRP
  REAL(CMISSRP), PARAMETER :: LENGTH=1.0_CMISSRP

  INTEGER(CMISSIntg), PARAMETER :: CoordinateSystemUserNumber=1
  INTEGER(CMISSIntg), PARAMETER :: RegionUserNumber=2
  INTEGER(CMISSIntg), PARAMETER :: BasisUserNumber=3
  INTEGER(CMISSIntg), PARAMETER :: GeneratedMeshUserNumber=4
  INTEGER(CMISSIntg), PARAMETER :: MeshUserNumber=5
  INTEGER(CMISSIntg), PARAMETER :: DecompositionUserNumber=6
  INTEGER(CMISSIntg), PARAMETER :: GeometricFieldUserNumber=7
  INTEGER(CMISSIntg), PARAMETER :: EquationsSetFieldUserNumber=8
  INTEGER(CMISSIntg), PARAMETER :: DependentFieldUserNumber=9
  INTEGER(CMISSIntg), PARAMETER :: EquationsSetUserNumber=10
  INTEGER(CMISSIntg), PARAMETER :: ProblemUserNumber=11
 
  !Program types
  
  !Program variables
  INTEGER(CMISSIntg) :: NUMBER_OF_ARGUMENTS,ARGUMENT_LENGTH,STATUS
  INTEGER(CMISSIntg) :: NUMBER_GLOBAL_X_ELEMENTS,NUMBER_GLOBAL_Y_ELEMENTS,NUMBER_GLOBAL_Z_ELEMENTS, &
    & INTERPOLATION_TYPE,NUMBER_OF_GAUSS_XI
  CHARACTER(LEN=255) :: COMMAND_ARGUMENT,Filename
  
  ! customize .out file name
  CHARACTER(LEN=255) :: my_exnode_exelem

  !CMISS variables
  TYPE(cmfe_BasisType) :: Basis
  TYPE(cmfe_BoundaryConditionsType) :: BoundaryConditions
  TYPE(cmfe_CoordinateSystemType) :: CoordinateSystem,WorldCoordinateSystem
  TYPE(cmfe_DecompositionType) :: Decomposition
  TYPE(cmfe_EquationsType) :: Equations
  TYPE(cmfe_EquationsSetType) :: EquationsSet
  TYPE(cmfe_FieldType) :: GeometricField,EquationsSetField,DependentField
  TYPE(cmfe_FieldsType) :: Fields
  TYPE(cmfe_GeneratedMeshType) :: GeneratedMesh  
  TYPE(cmfe_MeshType) :: Mesh
  TYPE(cmfe_NodesType) :: Nodes
  TYPE(cmfe_ProblemType) :: Problem
  TYPE(cmfe_RegionType) :: Region,WorldRegion
  TYPE(cmfe_SolverType) :: Solver
  TYPE(cmfe_SolverEquationsType) :: SolverEquations

#ifdef WIN32
  !Quickwin type
  LOGICAL :: QUICKWIN_STATUS=.FALSE.
  TYPE(WINDOWCONFIG) :: QUICKWIN_WINDOW_CONFIG
#endif
  
  !Generic CMISS variables  
  INTEGER(CMISSIntg) :: NumberOfComputationalNodes,ComputationalNodeNumber
  INTEGER(CMISSIntg) :: EquationsSetIndex
  INTEGER(CMISSIntg) :: FirstNodeNumber,LastNodeNumber
  INTEGER(CMISSIntg) :: FirstNodeDomain,LastNodeDomain
  INTEGER(CMISSIntg) :: Err
  
#ifdef WIN32
  !Initialise QuickWin
  QUICKWIN_WINDOW_CONFIG%TITLE="General Output" !Window title
  QUICKWIN_WINDOW_CONFIG%NUMTEXTROWS=-1 !Max possible number of rows
  QUICKWIN_WINDOW_CONFIG%MODE=QWIN$SCROLLDOWN
  !Set the window parameters
  QUICKWIN_STATUS=SETWINDOWCONFIG(QUICKWIN_WINDOW_CONFIG)
  !If attempt fails set with system estimated values
  IF(.NOT.QUICKWIN_STATUS) QUICKWIN_STATUS=SETWINDOWCONFIG(QUICKWIN_WINDOW_CONFIG)
#endif

! my variables
  INTEGER(CMISSIntg) :: tree_DOMAIN_NUMBER, localElementNumber, user_num_inc
  LOGICAL :: elementExists, ghostElement

  INTEGER(CMISSIntg) :: key, val
  TYPE(HTable) :: my_table

! vbls for Fredman hashing
    INTEGER(CMISSIntg), DIMENSION(:), ALLOCATABLE :: T_key, T_val ! the hash table
    INTEGER(CMISSIntg), DIMENSION(:), ALLOCATABLE :: S_key, S_val ! array of keys and vls for query
    INTEGER(CMISSIntg) :: n,p, q, index_found ! dimension of subset and prime number >m
    LOGICAL :: is_found, enters_if

  !-----------------------------------------------------------------------------------------------------------
  ! PROBLEM CONTROL PANEL
  !-----------------------------------------------------------------------------------------------------------

  NUMBER_OF_ARGUMENTS = COMMAND_ARGUMENT_COUNT()
  IF(NUMBER_OF_ARGUMENTS >= 4) THEN
    !If we have enough arguments then use the first four for setting up the problem. The subsequent arguments may be used to
    !pass flags to, say, PETSc.
    CALL GET_COMMAND_ARGUMENT(1,COMMAND_ARGUMENT,ARGUMENT_LENGTH,STATUS)
    IF(STATUS>0) CALL HANDLE_ERROR("Error for command argument 1.")
    READ(COMMAND_ARGUMENT(1:ARGUMENT_LENGTH),*) NUMBER_GLOBAL_X_ELEMENTS
    IF(NUMBER_GLOBAL_X_ELEMENTS<=0) CALL HANDLE_ERROR("Invalid number of X elements.")
    CALL GET_COMMAND_ARGUMENT(2,COMMAND_ARGUMENT,ARGUMENT_LENGTH,STATUS)
    IF(STATUS>0) CALL HANDLE_ERROR("Error for command argument 2.")
    READ(COMMAND_ARGUMENT(1:ARGUMENT_LENGTH),*) NUMBER_GLOBAL_Y_ELEMENTS
    IF(NUMBER_GLOBAL_Y_ELEMENTS<=0) CALL HANDLE_ERROR("Invalid number of Y elements.")
    CALL GET_COMMAND_ARGUMENT(3,COMMAND_ARGUMENT,ARGUMENT_LENGTH,STATUS)
    IF(STATUS>0) CALL HANDLE_ERROR("Error for command argument 3.")
    READ(COMMAND_ARGUMENT(1:ARGUMENT_LENGTH),*) NUMBER_GLOBAL_Z_ELEMENTS
    IF(NUMBER_GLOBAL_Z_ELEMENTS<0) CALL HANDLE_ERROR("Invalid number of Z elements.")
    CALL GET_COMMAND_ARGUMENT(4,COMMAND_ARGUMENT,ARGUMENT_LENGTH,STATUS)
    IF(STATUS>0) CALL HANDLE_ERROR("Error for command argument 4.")
    READ(COMMAND_ARGUMENT(1:ARGUMENT_LENGTH),*) INTERPOLATION_TYPE
    IF(INTERPOLATION_TYPE<=0) CALL HANDLE_ERROR("Invalid Interpolation specification.")
  ELSE
    !If there are not enough arguments default the problem specification 
    NUMBER_GLOBAL_X_ELEMENTS=1
    NUMBER_GLOBAL_Y_ELEMENTS=3
    NUMBER_GLOBAL_Z_ELEMENTS=1
!    INTERPOLATION_TYPE=1
    
    ! basis_... = 1,2,...,9 in opencmiss_iron.f90 + basis_routines.f90
    INTERPOLATION_TYPE=CMFE_BASIS_LINEAR_LAGRANGE_INTERPOLATION
!    INTERPOLATION_TYPE=CMFE_BASIS_QUADRATIC_LAGRANGE_INTERPOLATION
!    INTERPOLATION_TYPE=CMFE_BASIS_CUBIC_LAGRANGE_INTERPOLATION    
    
  ENDIF

  !Intialise OpenCMISS
  CALL cmfe_Initialise(WorldCoordinateSystem,WorldRegion,Err)

  CALL cmfe_ErrorHandlingModeSet(CMFE_ERRORS_TRAP_ERROR,Err)

  CALL cmfe_RandomSeedsSet(9999,Err)
  
!  CALL cmfe_DiagnosticsSetOn(CMFE_IN_DIAG_TYPE,[1,2,3,4,5],"Diagnostics",["HASH_TABLE_PUT_ONE"],Err) ! HASH_TABLE_PUT_ONE
! Print to screen wo file name
! e.g. levels: diagnostics3 (constants in base routines)
   
!  CALL cmfe_DiagnosticsSetOn(CMFE_FROM_DIAG_TYPE, [1,2,3,4,5],"Diagnostics",["HashTable_PutAll"],Err) ! HASH_TABLE_PUT_ONE
 
!CALL cmfe_DiagnosticsSetOn(CMFE_IN_DIAG_TYPE,[1,2,3,4,5],"Diagnostics",["HashTable_PutAll              ",& 
!                                                                       &"Laplace_FiniteElementCalculate"],err)
CALL cmfe_DiagnosticsSetOn(CMFE_IN_DIAG_TYPE,[1,2,3,4,5],"Diagnostics",&
    & ["HashTable_PutAll                 ",&
    &  "HashTable_PutOne                 ",&
    &  "HashTable_Insert                 ",&
    &  "DistributedMatrix_CMISSInitialise"],err)

   !CALL cmfe_DiagnosticsSetOn(CMFE_IN_DIAG_TYPE, [1,2,3,4,5],"Diagnostics",["HashTable_PutAll","HashTable_PutOne"],Err) ! HASH_TABLE_PUT_ONE
   !CALL cmfe_DiagnosticsSetOn(CMFE_IN_DIAG_TYPE, [1,2,3,4,5],"Diagnostics",["DECOMPOSITION_ELEMENT_DOMAIN_CALCULATE"],Err) ! HASH_TABLE_PUT_ONE
   !CALL cmfe_DiagnosticsSetOn(CMFE_ALL_DIAG_TYPE,[1,2,3,4,5],"Diagnostics",[""],Err)

  WRITE(Filename,'(A,"_",I0,"x",I0,"x",I0,"_",I0)') "Laplace",NUMBER_GLOBAL_X_ELEMENTS,NUMBER_GLOBAL_Y_ELEMENTS, &
    & NUMBER_GLOBAL_Z_ELEMENTS,INTERPOLATION_TYPE
  
  ! controls writing of .out file (NOT of on-screen info!!)
  ! CALL cmfe_OutputSetOff(Err) ! deactivates file .out, on-screen info still there!
  CALL cmfe_OutputSetOn(Filename,Err)
  
  !Get the computational nodes information
  CALL cmfe_ComputationalNumberOfNodesGet(NumberOfComputationalNodes,Err) ! that is 1
  CALL cmfe_ComputationalNodeNumberGet(ComputationalNodeNumber,Err) ! that is 0

  !-----------------------------------------------------------------------------------------------------------
  !COORDINATE SYSTEM
  !-----------------------------------------------------------------------------------------------------------  
 
  !Start the creation of a new RC coordinate system
  CALL cmfe_CoordinateSystem_Initialise(CoordinateSystem,Err)
  CALL cmfe_CoordinateSystem_CreateStart(CoordinateSystemUserNumber,CoordinateSystem,Err)
  IF(NUMBER_GLOBAL_Z_ELEMENTS==0) THEN
    !Set the coordinate system to be 2D
    CALL cmfe_CoordinateSystem_DimensionSet(CoordinateSystem,2,Err)
  ELSE
    !Set the coordinate system to be 3D
    CALL cmfe_CoordinateSystem_DimensionSet(CoordinateSystem,3,Err)
  ENDIF
  !Finish the creation of the coordinate system
  CALL cmfe_CoordinateSystem_CreateFinish(CoordinateSystem,Err)

  !-----------------------------------------------------------------------------------------------------------
  !REGION
  !-----------------------------------------------------------------------------------------------------------

  !Start the creation of the region
  CALL cmfe_Region_Initialise(Region,Err)
  CALL cmfe_Region_CreateStart(RegionUserNumber,WorldRegion,Region,Err)
  CALL cmfe_Region_LabelSet(Region,"laplace_equation",Err)
  !Set the regions coordinate system to the 2D RC coordinate system that we have created
  CALL cmfe_Region_CoordinateSystemSet(Region,CoordinateSystem,Err)
  !Finish the creation of the region
  CALL cmfe_Region_CreateFinish(Region,Err)

  !-----------------------------------------------------------------------------------------------------------
  !BASIS
  !-----------------------------------------------------------------------------------------------------------

  !Start the creation of a basis (default is trilinear lagrange)
  CALL cmfe_Basis_Initialise(Basis,Err)
  CALL cmfe_Basis_CreateStart(BasisUserNumber,Basis,Err)
  SELECT CASE(INTERPOLATION_TYPE)
  CASE(1,2,3,4)
    CALL cmfe_Basis_TypeSet(Basis,CMFE_BASIS_LAGRANGE_HERMITE_TP_TYPE,Err)
  CASE(7,8,9)
    CALL cmfe_Basis_TypeSet(Basis,CMFE_BASIS_SIMPLEX_TYPE,Err)
  CASE DEFAULT
    CALL HANDLE_ERROR("Invalid interpolation type.")
  END SELECT
  SELECT CASE(INTERPOLATION_TYPE)
  CASE(1)
    NUMBER_OF_GAUSS_XI=2
  CASE(2)
    NUMBER_OF_GAUSS_XI=3
  CASE(3,4)
    NUMBER_OF_GAUSS_XI=4
  CASE DEFAULT
    NUMBER_OF_GAUSS_XI=0 !Don't set number of Gauss points for tri/tet
  END SELECT
  IF(NUMBER_GLOBAL_Z_ELEMENTS==0) THEN
    !Set the basis to be a bi-interpolation basis
    CALL cmfe_Basis_NumberOfXiSet(Basis,2,Err)
    CALL cmfe_Basis_InterpolationXiSet(Basis,[INTERPOLATION_TYPE,INTERPOLATION_TYPE],Err)
    IF(NUMBER_OF_GAUSS_XI>0) THEN
      CALL cmfe_Basis_QuadratureNumberOfGaussXiSet(Basis,[NUMBER_OF_GAUSS_XI,NUMBER_OF_GAUSS_XI],Err)
    ENDIF
  ELSE
    !Set the basis to be a tri-interpolation basis
    CALL cmfe_Basis_NumberOfXiSet(Basis,3,Err)
    CALL cmfe_Basis_InterpolationXiSet(Basis,[INTERPOLATION_TYPE,INTERPOLATION_TYPE,INTERPOLATION_TYPE],Err)
    IF(NUMBER_OF_GAUSS_XI>0) THEN
      CALL cmfe_Basis_QuadratureNumberOfGaussXiSet(Basis,[NUMBER_OF_GAUSS_XI,NUMBER_OF_GAUSS_XI,NUMBER_OF_GAUSS_XI],Err)
    ENDIF
  ENDIF
  !Finish the creation of the basis
  CALL cmfe_Basis_CreateFinish(Basis,Err)
   
  !-----------------------------------------------------------------------------------------------------------
  !MESH
  !-----------------------------------------------------------------------------------------------------------

  !Start the creation of a generated mesh in the region
  CALL cmfe_GeneratedMesh_Initialise(GeneratedMesh,Err)
  CALL cmfe_GeneratedMesh_CreateStart(GeneratedMeshUserNumber,Region,GeneratedMesh,Err)
  !Set up a regular x*y*z mesh
  CALL cmfe_GeneratedMesh_TypeSet(GeneratedMesh,CMFE_GENERATED_MESH_REGULAR_MESH_TYPE,Err)
  !Set the default basis
  CALL cmfe_GeneratedMesh_BasisSet(GeneratedMesh,Basis,Err)   
  !Define the mesh on the region
  IF(NUMBER_GLOBAL_Z_ELEMENTS==0) THEN
    CALL cmfe_GeneratedMesh_ExtentSet(GeneratedMesh,[WIDTH,HEIGHT],Err)
    CALL cmfe_GeneratedMesh_NumberOfElementsSet(GeneratedMesh,[NUMBER_GLOBAL_X_ELEMENTS,NUMBER_GLOBAL_Y_ELEMENTS],Err)
  ELSE
    CALL cmfe_GeneratedMesh_ExtentSet(GeneratedMesh,[WIDTH,HEIGHT,LENGTH],Err)
    CALL cmfe_GeneratedMesh_NumberOfElementsSet(GeneratedMesh,[NUMBER_GLOBAL_X_ELEMENTS,NUMBER_GLOBAL_Y_ELEMENTS, &
      & NUMBER_GLOBAL_Z_ELEMENTS],Err)
  ENDIF    
  !Finish the creation of a generated mesh in the region
  CALL cmfe_Mesh_Initialise(Mesh,Err)
  CALL cmfe_GeneratedMesh_CreateFinish(GeneratedMesh,MeshUserNumber,Mesh,Err)

  !Create a decomposition
  CALL cmfe_Decomposition_Initialise(Decomposition,Err)
  CALL cmfe_Decomposition_CreateStart(DecompositionUserNumber,Mesh,Decomposition,Err)
! Set the decomposition to be a general decomposition with the specified number of domains
  CALL cmfe_Decomposition_TypeSet(Decomposition,CMFE_DECOMPOSITION_CALCULATED_TYPE,Err)
  CALL cmfe_Decomposition_NumberOfDomainsSet(Decomposition,NumberOfComputationalNodes,Err)
  !Finish the decomposition
  CALL cmfe_Decomposition_CreateFinish(Decomposition,Err)
 
! tree/hash inquiries
! tree is built in DECOMPOSITION_TOPOLOGY_ELEMENTS_CALCULATE
! replace there with hash!?

Err=0
user_num_inc=1

! Tree inquiry
IF (.FALSE.) THEN

  CALL HTable_Init(my_table, err, 5) ! create a hash table of dim 5 

  DO ! loop over all user numbers (elements)
     ! which in this case correspond to global numbers
   IF (Err==0) THEN
     !  SUBROUTINE DECOMPOSITION_ELEMENT_DOMAIN_GET(DECOMPOSITION,USER_ELEMENT_NUMBER,DOMAIN_NUMBER,ERR,ERROR,*)     
     CALL cmfe_Decomposition_ElementDomainGet(Decomposition, user_num_inc, tree_DOMAIN_NUMBER, Err)
     PRINT *, "Found in domain ", tree_DOMAIN_NUMBER

     CALL HTable_Put (my_table, user_num_inc, user_num_inc, err) ! key and value are the same

     user_num_inc = user_num_inc+1  

   ELSE
     EXIT
   END IF

  END DO

END IF

! Decomposition topology is private!!! Then no calls here.
! SUBROUTINE DecompositionTopology_ElementCheckExists(decompositionTopology,userElementNumber,elementExists,localElementNumber, &
!    & ghostElement,err,error,*)  
! HOW can I access the topology, which is the distribution among the ranks!!??
!  CALL cmfe_DecompositionTopology_ElementCheckExists(Decomposition%decomposition%TOPOLOGY,3,elementExists,localElementNumber, &
!    & ghostElement,Err)

! Simple hash inquiry OLD, delete me!
     ERR = 0 
     user_num_inc = 1

enters_if = .FALSE.
IF (enters_if) THEN
  DO
     !CALL HTable_Get (my_table, user_num_inc, val, err) ! key and value are the same
     IF (err==0) THEN
      PRINT*, 'Hash:  ', MOD(user_num_inc,my_table%vec_len)
      PRINT*, 'Value: ', val
      user_num_inc = user_num_inc+1 
     ELSE
      EXIT
     END IF 
  END DO

  CALL HTable_Free(my_table, err)
  PRINT*, "Length of hash after destruction ", my_table%vec_len
  PRINT*, "Initialised after destruction?", my_table%is_init
END IF

! Test Fredman Hash
enters_if = .FALSE.
IF (enters_if) THEN
  n = 6
  p = 31
  ALLOCATE(S_key(n), STAT=ERR)
  ALLOCATE(S_val(n), STAT=ERR)
  S_key = [2,4,5,15,18,30]
  S_val = S_key+100

  CALL HTable_Put_All(S_key, S_val, p, T_key, T_val, ERR)
  ! query for q

  DO q=1,100
    CALL HTable_Get(q, n, p, index_found, is_found, T_key, ERR)
    !PRINT *, "Index ", index_found
    IF (is_found) THEN
     PRINT *, q, "Found! With value ", T_val(index_found)
    ELSE
     PRINT *, q, "Key does not exist!"
    END IF 
  END DO
END IF



  ! we can stop here for now
  !PRINT *, "Abort program after mesh generation. Learning trees."
  !STOP


  !Destory the mesh now that we have decomposed it
  !CALL cmfe_Mesh_Destroy(Mesh,Err)
 
  !-----------------------------------------------------------------------------------------------------------
  !GEOMETRIC FIELD
  !-----------------------------------------------------------------------------------------------------------

  !Start to create a default (geometric) field on the region
  CALL cmfe_Field_Initialise(GeometricField,Err)
  CALL cmfe_Field_CreateStart(GeometricFieldUserNumber,Region,GeometricField,Err)
  !Set the decomposition to use
  CALL cmfe_Field_MeshDecompositionSet(GeometricField,Decomposition,Err)
  !Set the domain to be used by the field components.
  CALL cmfe_Field_ComponentMeshComponentSet(GeometricField,CMFE_FIELD_U_VARIABLE_TYPE,1,1,Err)
  CALL cmfe_Field_ComponentMeshComponentSet(GeometricField,CMFE_FIELD_U_VARIABLE_TYPE,2,1,Err)
  IF(NUMBER_GLOBAL_Z_ELEMENTS/=0) THEN
    CALL cmfe_Field_ComponentMeshComponentSet(GeometricField,CMFE_FIELD_U_VARIABLE_TYPE,3,1,Err)
  ENDIF
  !Finish creating the field
  CALL cmfe_Field_CreateFinish(GeometricField,Err)

  !Update the geometric field parameters
  CALL cmfe_GeneratedMesh_GeometricParametersCalculate(GeneratedMesh,GeometricField,Err)
  
  !-----------------------------------------------------------------------------------------------------------
  !EQUATIONS SETS
  !-----------------------------------------------------------------------------------------------------------

  !Create the Standard Laplace Equations set
  CALL cmfe_EquationsSet_Initialise(EquationsSet,Err)
  CALL cmfe_Field_Initialise(EquationsSetField,Err)
  CALL cmfe_EquationsSet_CreateStart(EquationsSetUserNumber,Region,GeometricField,[CMFE_EQUATIONS_SET_CLASSICAL_FIELD_CLASS, &
    & CMFE_EQUATIONS_SET_LAPLACE_EQUATION_TYPE,CMFE_EQUATIONS_SET_STANDARD_LAPLACE_SUBTYPE],EquationsSetFieldUserNumber, &
    & EquationsSetField,EquationsSet,Err)
  !Finish creating the equations set
  CALL cmfe_EquationsSet_CreateFinish(EquationsSet,Err)

  !-----------------------------------------------------------------------------------------------------------
  ! DEPENDENT FIELD
  !-----------------------------------------------------------------------------------------------------------

  !Create the equations set dependent field variables
  CALL cmfe_Field_Initialise(DependentField,Err)
  CALL cmfe_EquationsSet_DependentCreateStart(EquationsSet,DependentFieldUserNumber,DependentField,Err)
  !Set the DOFs to be contiguous across components ! then why SEPARATED??

!  CALL cmfe_Field_DOFOrderTypeSet(DependentField,CMFE_FIELD_U_VARIABLE_TYPE,       CMFE_FIELD_SEPARATED_COMPONENT_DOF_ORDER,Err)
!  CALL cmfe_Field_DOFOrderTypeSet(DependentField,CMFE_FIELD_DELUDELN_VARIABLE_TYPE,CMFE_FIELD_SEPARATED_COMPONENT_DOF_ORDER,Err)

  CALL cmfe_Field_DOFOrderTypeSet(DependentField,CMFE_FIELD_U_VARIABLE_TYPE,       CMFE_FIELD_CONTIGUOUS_COMPONENT_DOF_ORDER,Err)
  CALL cmfe_Field_DOFOrderTypeSet(DependentField,CMFE_FIELD_DELUDELN_VARIABLE_TYPE,CMFE_FIELD_CONTIGUOUS_COMPONENT_DOF_ORDER,Err)

  !Finish the equations set dependent field variables
  CALL cmfe_EquationsSet_DependentCreateFinish(EquationsSet,Err)

  !Initialise the field with an initial guess
  CALL cmfe_Field_ComponentValuesInitialise(DependentField,CMFE_FIELD_U_VARIABLE_TYPE,CMFE_FIELD_VALUES_SET_TYPE,1,0.5_CMISSRP, &
    & Err)
    ! From opencmiss_iron.f90:
    !>Initialises the values of parameter set of a field variable component to a double precision constant value for a field identified by an object.
 ! SUBROUTINE cmfe_Field_ComponentValuesInitialiseDPObj(field,variableType,fieldSetType,componentNumber,value,err)

  !-----------------------------------------------------------------------------------------------------------
  ! EQUATIONS
  !-----------------------------------------------------------------------------------------------------------

  !Create the equations set equations
  CALL cmfe_Equations_Initialise(Equations,Err)
  CALL cmfe_EquationsSet_EquationsCreateStart(EquationsSet,Equations,Err)
  !Set the equations matrices sparsity type
  CALL cmfe_Equations_SparsityTypeSet(Equations,CMFE_EQUATIONS_SPARSE_MATRICES,Err)
! CALL cmfe_Equations_SparsityTypeSet(Equations,CMFE_EQUATIONS_FULL_MATRICES,Err)
 
  !Set the equations set output
   CALL cmfe_Equations_OutputTypeSet(Equations,CMFE_EQUATIONS_NO_OUTPUT,Err)
   CALL cmfe_Equations_OutputTypeSet(Equations,CMFE_EQUATIONS_TIMING_OUTPUT,Err)
! CALL cmfe_Equations_OutputTypeSet(Equations,CMFE_EQUATIONS_MATRIX_OUTPUT,Err)
! CALL cmfe_Equations_OutputTypeSet(Equations,CMFE_EQUATIONS_ELEMENT_MATRIX_OUTPUT,Err)
  
  !Finish the equations set equations
  CALL cmfe_EquationsSet_EquationsCreateFinish(EquationsSet,Err)

  !-----------------------------------------------------------------------------------------------------------
  !PROBLEM
  !-----------------------------------------------------------------------------------------------------------  

  !Start the creation of a problem.
  CALL cmfe_Problem_Initialise(Problem,Err)
  CALL cmfe_Problem_CreateStart(ProblemUserNumber,[CMFE_PROBLEM_CLASSICAL_FIELD_CLASS,CMFE_PROBLEM_LAPLACE_EQUATION_TYPE, &
    & CMFE_PROBLEM_STANDARD_LAPLACE_SUBTYPE],Problem,Err)
  !Finish the creation of a problem.
  CALL cmfe_Problem_CreateFinish(Problem,Err)

  !Start the creation of the problem control loop
  CALL cmfe_Problem_ControlLoopCreateStart(Problem,Err)
  !Finish creating the problem control loop
  CALL cmfe_Problem_ControlLoopCreateFinish(Problem,Err)
 
  !-----------------------------------------------------------------------------------------------------------
  !SOLVER
  !-----------------------------------------------------------------------------------------------------------

  !Start the creation of the problem solvers
  CALL cmfe_Solver_Initialise(Solver,Err)
  CALL cmfe_Problem_SolversCreateStart(Problem,Err)
  CALL cmfe_Problem_SolverGet(Problem,CMFE_CONTROL_LOOP_NODE,1,Solver,Err)
  CALL cmfe_Solver_OutputTypeSet(Solver,CMFE_SOLVER_NO_OUTPUT,Err)

 CALL cmfe_Solver_OutputTypeSet(Solver,CMFE_SOLVER_PROGRESS_OUTPUT,Err)
 CALL cmfe_Solver_OutputTypeSet(Solver,CMFE_SOLVER_TIMING_OUTPUT,Err)
 CALL cmfe_Solver_OutputTypeSet(Solver,CMFE_SOLVER_SOLVER_OUTPUT,Err)
 CALL cmfe_Solver_OutputTypeSet(Solver,CMFE_SOLVER_MATRIX_OUTPUT,Err)
  
! CALL cmfe_Solver_LinearTypeSet(Solver,CMFE_SOLVER_LINEAR_ITERATIVE_SOLVE_TYPE,Err)
! CALL cmfe_Solver_LinearIterativeAbsoluteToleranceSet(Solver,1.0E-12_CMISSRP,Err)
! CALL cmfe_Solver_LinearIterativeRelativeToleranceSet(Solver,1.0E-12_CMISSRP,Err)

!  CALL cmfe_Solver_LinearTypeSet(Solver,CMFE_SOLVER_LINEAR_DIRECT_SOLVE_TYPE,Err)
  
! CALL cmfe_Solver_LinearTypeSet(Solver,CMFE_SOLVER_LINEAR_DIRECT_SOLVE_TYPE,Err)
! CALL cmfe_Solver_LibraryTypeSet(Solver,CMFE_SOLVER_MUMPS_LIBRARY,Err)
 CALL cmfe_Solver_LibraryTypeSet(Solver,CMFE_SOLVER_LAPACK_LIBRARY,Err)
! CALL cmfe_Solver_LibraryTypeSet(Solver,CMFE_SOLVER_SUPERLU_LIBRARY,Err)
! CALL cmfe_Solver_LibraryTypeSet(Solver,CMFE_SOLVER_PASTIX_LIBRARY,Err)
  !Finish the creation of the problem solver
  CALL cmfe_Problem_SolversCreateFinish(Problem,Err)

  !-----------------------------------------------------------------------------------------------------------
  !SOLVER EQUATIONS
  !-----------------------------------------------------------------------------------------------------------  

  !Start the creation of the problem solver equations
  CALL cmfe_Solver_Initialise(Solver,Err)
  CALL cmfe_SolverEquations_Initialise(SolverEquations,Err)
  CALL cmfe_Problem_SolverEquationsCreateStart(Problem,Err)
  !Get the solve equations
  CALL cmfe_Problem_SolverGet(Problem,CMFE_CONTROL_LOOP_NODE,1,Solver,Err)
  CALL cmfe_Solver_SolverEquationsGet(Solver,SolverEquations,Err)
  !Set the solver equations sparsity
  CALL cmfe_SolverEquations_SparsityTypeSet(SolverEquations,CMFE_SOLVER_SPARSE_MATRICES,Err)
  !CALL cmfe_SolverEquations_SparsityTypeSet(SolverEquations,CMFE_SOLVER_FULL_MATRICES,Err)  
  !Add in the equations set
  CALL cmfe_SolverEquations_EquationsSetAdd(SolverEquations,EquationsSet,EquationsSetIndex,Err)
  !Finish the creation of the problem solver equations
  CALL cmfe_Problem_SolverEquationsCreateFinish(Problem,Err)

  !-----------------------------------------------------------------------------------------------------------
  !BOUNDARY CONDITIONS
  !-----------------------------------------------------------------------------------------------------------

  !Start the creation of the equations set boundary conditions
  CALL cmfe_BoundaryConditions_Initialise(BoundaryConditions,Err)
  CALL cmfe_SolverEquations_BoundaryConditionsCreateStart(SolverEquations,BoundaryConditions,Err)
  !Set the first node to 0.0 and the last node to 1.0
  FirstNodeNumber=1
  CALL cmfe_Nodes_Initialise(Nodes,Err)
  CALL cmfe_Region_NodesGet(Region,Nodes,Err)
  CALL cmfe_Nodes_NumberOfNodesGet(Nodes,LastNodeNumber,Err)
  ! In which decomposition first and last node
  CALL cmfe_Decomposition_NodeDomainGet(Decomposition,FirstNodeNumber,1,FirstNodeDomain,Err)
  CALL cmfe_Decomposition_NodeDomainGet(Decomposition,LastNodeNumber,1,LastNodeDomain,Err)
  
  ! write(*,*) "Hey this is the C.Number:", ComputationalNodeNumber, FirstNodeDomain, LastNodeDomain, NumberOfComputationalNodes
  ! all 0! NumberOfComputationalNodes:1 
  
  IF(FirstNodeDomain==ComputationalNodeNumber) THEN
    CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions,DependentField,CMFE_FIELD_U_VARIABLE_TYPE,1,1,FirstNodeNumber,1, &
      & CMFE_BOUNDARY_CONDITION_FIXED,0.0_CMISSRP,Err)
  ENDIF
  IF(LastNodeDomain==ComputationalNodeNumber) THEN
    CALL cmfe_BoundaryConditions_SetNode(BoundaryConditions,DependentField,CMFE_FIELD_U_VARIABLE_TYPE,1,1,LastNodeNumber,1, &
      & CMFE_BOUNDARY_CONDITION_FIXED,1.0_CMISSRP,Err)
  ENDIF
  !Finish the creation of the equations set boundary conditions
  CALL cmfe_SolverEquations_BoundaryConditionsCreateFinish(SolverEquations,Err)

  !-----------------------------------------------------------------------------------------------------------
  !SOLVE
  !-----------------------------------------------------------------------------------------------------------

  !Solve the problem
  CALL cmfe_Problem_Solve(Problem,Err)

  PRINT *, "Abort program after matrix generation. Testing hash."
  STOP

  !-----------------------------------------------------------------------------------------------------------
  !OUTPUT
  !-----------------------------------------------------------------------------------------------------------

  !Export results
  CALL cmfe_Fields_Initialise(Fields,Err)
  CALL cmfe_Fields_Create(Region,Fields,Err)
  
  WRITE(my_exnode_exelem,'(A,"_",I0,"x",I0,"x",I0,"_",I0)') "laplace_equation",NUMBER_GLOBAL_X_ELEMENTS,NUMBER_GLOBAL_Y_ELEMENTS, &
    & NUMBER_GLOBAL_Z_ELEMENTS,INTERPOLATION_TYPE
!  CALL cmfe_Fields_NodesExport(Fields,"laplace_equation","FORTRAN",Err)
!  CALL cmfe_Fields_ElementsExport(Fields,"laplace_equation","FORTRAN",Err)
  CALL cmfe_Fields_NodesExport(Fields,my_exnode_exelem,"FORTRAN",Err)
  CALL cmfe_Fields_ElementsExport(Fields,my_exnode_exelem,"FORTRAN",Err)
 
  CALL cmfe_Fields_Finalise(Fields,Err)
  
  !Finialise CMISS
  CALL cmfe_Finalise(Err)

  WRITE(*,'(A)') "Program successfully completed."
  
  STOP
  
CONTAINS

  SUBROUTINE HANDLE_ERROR(ERROR_STRING)

    CHARACTER(LEN=*), INTENT(IN) :: ERROR_STRING

    WRITE(*,'(">>ERROR: ",A)') ERROR_STRING(1:LEN_TRIM(ERROR_STRING))
    STOP

  END SUBROUTINE HANDLE_ERROR
    
END PROGRAM LAPLACE_EQUATION
