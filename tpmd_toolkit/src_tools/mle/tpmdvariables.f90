!#==============================================================================================
!#
!#   Copyright 2020 Abhijit Chatterjee
!#
!#   Licensed under the Apache License, Version 2.0 (the "License");
!#   you may not use this file except in compliance with the License.
!#   You may obtain a copy of the License at
!#
!#       http://www.apache.org/licenses/LICENSE-2.0
!#
!#   Unless required by applicable law or agreed to in writing, software
!#   distributed under the License is distributed on an "AS IS" BASIS,
!#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!#   See the License for the specific language governing permissions and
!#   limitations under the License.
!#==============================================================================================

!#```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
!#Script name: tpmdvariables.f90
!#
!#Purpose: Basic TPMD variables
!#
!#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
!#
!#Date of Modification: 15 May 2020
!#.......................................................................................................................................

MODULE TPMDVariables
   IMPLICIT NONE

   DOUBLE PRECISION :: kB=8.617343000e-5
   TYPE TPMDData
      INTEGER :: npts=0
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: time
      INTEGER, DIMENSION(:), ALLOCATABLE :: fstate
      DOUBLE PRECISION :: tau=0.,T0=0.,deltaT=0.,Tmax=0.
   END TYPE TPMDData
   TYPE TPMDStagePartition
      INTEGER :: Nmax=0,m=0
      DOUBLE PRECISION :: invTna_sum=0.d0
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: StagewiseBreakupState
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: StagewiseBreakupTime
      INTEGER, DIMENSION(:), ALLOCATABLE :: count,StagewisePoints
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: theta
   END TYPE TPMDStagePartition

   CONTAINS

   SUBROUTINE InitializeRandomSeed(iseed)
      IMPLICIT NONE
      INTEGER :: iseed,n
      integer, allocatable :: seed(:)

      call random_seed(size = n)
      allocate(seed(n))
      seed=iseed
      call random_seed(put=seed)
   END SUBROUTINE InitializeRandomSeed
   !xxxxxxxxxxxxx
   SUBROUTINE CreateTPMDDataset(d,tau,T0,deltaT,Tmax)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      DOUBLE PRECISION :: tau,T0,deltaT,Tmax

      ALLOCATE(d)
      d%tau=tau
      d%T0=T0
      d%deltaT=deltaT
      d%Tmax=Tmax
   END SUBROUTINE CreateTPMDDataset
   !xxxxxxxxxxxxx
   SUBROUTINE DeleteTPMDDataset(d)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d

      IF (ALLOCATED(d%time)) DEALLOCATE(d%time)
      IF (ALLOCATED(d%fstate)) DEALLOCATE(d%fstate)
      IF (ASSOCIATED(d)) DEALLOCATE(d)
   END SUBROUTINE DeleteTPMDDataset
END MODULE TPMDVariables
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
