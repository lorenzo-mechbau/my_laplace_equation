# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg

# Include any dependencies generated for this target.
include src/fortran/CMakeFiles/laplace_equation.dir/depend.make

# Include the progress variables for this target.
include src/fortran/CMakeFiles/laplace_equation.dir/progress.make

# Include the compile flags for this target's objects.
include src/fortran/CMakeFiles/laplace_equation.dir/flags.make

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o: src/fortran/CMakeFiles/laplace_equation.dir/flags.make
src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o: /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/laplace_equation.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/laplace_equation.F90 -o CMakeFiles/laplace_equation.dir/laplace_equation.F90.o

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/laplace_equation.dir/laplace_equation.F90.i"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/laplace_equation.F90 > CMakeFiles/laplace_equation.dir/laplace_equation.F90.i

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/laplace_equation.dir/laplace_equation.F90.s"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/laplace_equation.F90 -o CMakeFiles/laplace_equation.dir/laplace_equation.F90.s

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.requires:

.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.requires

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.provides: src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.requires
	$(MAKE) -f src/fortran/CMakeFiles/laplace_equation.dir/build.make src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.provides.build
.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.provides

src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.provides.build: src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o


src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o: src/fortran/CMakeFiles/laplace_equation.dir/flags.make
src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o: /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/hash_routines_local.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/hash_routines_local.F90 -o CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o

src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/laplace_equation.dir/hash_routines_local.F90.i"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/hash_routines_local.F90 > CMakeFiles/laplace_equation.dir/hash_routines_local.F90.i

src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/laplace_equation.dir/hash_routines_local.F90.s"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran/hash_routines_local.F90 -o CMakeFiles/laplace_equation.dir/hash_routines_local.F90.s

src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.requires:

.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.requires

src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.provides: src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.requires
	$(MAKE) -f src/fortran/CMakeFiles/laplace_equation.dir/build.make src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.provides.build
.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.provides

src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.provides.build: src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o


# Object files for target laplace_equation
laplace_equation_OBJECTS = \
"CMakeFiles/laplace_equation.dir/laplace_equation.F90.o" \
"CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o"

# External object files for target laplace_equation
laplace_equation_EXTERNAL_OBJECTS =

src/fortran/laplace_equation: src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o
src/fortran/laplace_equation: src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o
src/fortran/laplace_equation: src/fortran/CMakeFiles/laplace_equation.dir/build.make
src/fortran/laplace_equation: /data/homes/zanon/software/opencmiss/opencmiss/install/lib/libiron_cd.so
src/fortran/laplace_equation: /data/homes/zanon/software/opencmiss/opencmiss/install/lib/libirond.so
src/fortran/laplace_equation: /usr/lib/x86_64-linux-gnu/libmpichfort.so
src/fortran/laplace_equation: /usr/lib/x86_64-linux-gnu/libmpich.so
src/fortran/laplace_equation: src/fortran/CMakeFiles/laplace_equation.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking Fortran executable laplace_equation"
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/laplace_equation.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/fortran/CMakeFiles/laplace_equation.dir/build: src/fortran/laplace_equation

.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/build

src/fortran/CMakeFiles/laplace_equation.dir/requires: src/fortran/CMakeFiles/laplace_equation.dir/laplace_equation.F90.o.requires
src/fortran/CMakeFiles/laplace_equation.dir/requires: src/fortran/CMakeFiles/laplace_equation.dir/hash_routines_local.F90.o.requires

.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/requires

src/fortran/CMakeFiles/laplace_equation.dir/clean:
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran && $(CMAKE_COMMAND) -P CMakeFiles/laplace_equation.dir/cmake_clean.cmake
.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/clean

src/fortran/CMakeFiles/laplace_equation.dir/depend:
	cd /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation/src/fortran /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran /data/homes/zanon/Desktop/OpenCMISS_CBM/my_tests/laplace_equation/laplace_equation-build-dbg/src/fortran/CMakeFiles/laplace_equation.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/fortran/CMakeFiles/laplace_equation.dir/depend

