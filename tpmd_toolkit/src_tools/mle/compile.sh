#!/bin/bash

#==============================================================================================
#
#   Copyright 2020 Abhijit Chatterjee
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#==============================================================================================


#```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
#Script name: compile.sh
#
#Purpose: 
#
#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
#
#Date of Modification: 15 May 2020
#.......................................................................................................................................

DEBUG=-g
DEBUG= 
gfortran -c $DEBUG tpmdvariables.f90 
gfortran -c $DEBUG tpmdmodule1.f90 
gfortran -c $DEBUG tpmdmodule2.f90 
gfortran -c $DEBUG tpmdmodule3.f90 
gfortran -c $DEBUG main.f90
gfortran -o tpmd-analysis.x tpmdvariables.o tpmdmodule1.o tpmdmodule2.o tpmdmodule3.o main.o

rm *o *mod

#gfortran -o  tpmd-analysis.x tpmdvariables.f90 tpmdmodule1.f90 tpmdmodule2.f90 tpmdmodule3.f90 main.f90
