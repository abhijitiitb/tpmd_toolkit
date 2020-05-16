
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
!#Script name: mail.f90
!#
!#Purpose: Main code for performing TPMD analysis
!#
!#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
!#
!#Date of Modification: 15 May 2020
!#.......................................................................................................................................

PROGRAM MAIN
   USE TPMDVariables
   USE TPMDModule1
   USE TPMDModule2
   USE TPMDModule3
   IMPLICIT NONE
   INTEGER :: runoption,istate,nfstate,npts,i,allstates,cnt
   INTEGER, DIMENSION(:), POINTER :: fstate=>NULL(),fstate1=>NULL()
   DOUBLE PRECISION :: tau,T0,deltaT,Tmax,Ea,nu,rate,effnu,effEa,gamma,stderr_effnu,stderr_effEa,stderr_gamma
   TYPE(TPMDData), POINTER :: dataset=>NULL()
   CHARACTER(len=1000) :: filename


   CALL ReadInput()

   IF (nfstate>0) THEN
      WRITE(6,*) "Analyzing rates for the paths ..."
      DO i=1,nfstate
         WRITE(UNIT=6,FMT='(" >>> ",I4,"->",I4)',ADVANCE="NO") istate,fstate(i)
      END DO
   ELSE !invert fstate
      ALLOCATE(fstate1(allstates+nfstate-1)) !also exclude istate
      cnt=0
      DO i=1,allstates
         IF (.NOT. (ANY(i==fstate(1:ABS(nfstate))) .OR. i==istate)) THEN !add
            cnt=cnt+1
            fstate1(cnt)=i
            WRITE(UNIT=6,FMT='(" >>> ",I4,"->",I4)',ADVANCE="NO") istate,fstate1(cnt)
         END IF
      END DO
      WRITE(6,*) "Final states excluded:",fstate(1:ABS(nfstate))
      DEALLOCATE(fstate)
      fstate=>fstate1
      NULLIFY(fstate1)
      nfstate=allstates+nfstate-1
      IF (cnt/=nfstate) THEN
         WRITE(6,*) "Number of final states appears to be wrong",cnt,nfstate
         STOP
      END IF
   END IF

   WRITE(6,*) " from file: ",TRIM(filename)
   CALL ReadDataSet(dataset,filename,npts,tau,T0,deltaT,Tmax)
   WRITE(6,*) "File has been read .."
   WRITE(UNIT=6,FMT='(" >>> T0(K):",F10.3)') T0
   WRITE(UNIT=6,FMT='(" >>> deltaT(K):",F10.3)') deltaT
   WRITE(UNIT=6,FMT='(" >>> Tmax(K):",F10.3)') Tmax
   WRITE(UNIT=6,FMT='(" >>> tau(ps):",F10.3)') tau
   WRITE(UNIT=6,FMT='(" >>> #datapoints:",I6)') npts
   WRITE(6,*) ">>> MLE for Arrhenius parameters"
   gamma=0.d0

   WRITE(6,*) " "
   WRITE(6,*) " "
   WRITE(6,*) "################# STEP 1. Calculate Arrhenius parameters #################"

   CALL MLEArrhenius(dataset,fstate,nfstate,effnu,effEa,lprint=.TRUE.)
   !CALL MLEArrhenius(dataset,fstate,nfstate,effnu,effEa,gamma,.TRUE.)

   WRITE(6,*) "################# STEP 2. Calculate uncertainty in Arrhenius parameters #################"

   CALL BootstrapArrheniusParameters(dataset,fstate,nfstate,100,stderr_effnu,stderr_effEa)
   !CALL BootstrapNonArrheniusParameters(dataset,fstate,nfstate,100,stderr_effnu,stderr_effEa,stderr_gamma)
   WRITE(UNIT=6,FMT='(" >>> Effective prefactor:",ES15.5,"  +/-",ES10.2)') effnu,stderr_effnu
   WRITE(UNIT=6,FMT='(" >>> Effective barrier:",ES15.5,"  +/-",ES10.2)') effEa,stderr_effEa
   !WRITE(UNIT=6,FMT='(" >>> gamma:",ES15.5,"  +/-",ES10.2)') gamma,stderr_gamma
   WRITE(6,*) ">>> "
   WRITE(6,*) ">>> "

   WRITE(6,*) " "
   WRITE(6,*) " "
   WRITE(6,*) "################# STEP 3. MLE for rates @ constant temperature #################"
   CALL CheckArrheniusRate(dataset,fstate,nfstate,effnu,effEa,stderr_effnu,stderr_effEa,.TRUE.)
   stop

   CONTAINS

   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   SUBROUTINE ReadInput()
      !reads input from fpt.input
      IMPLICIT NONE

      OPEN(UNIT=353,FILE="fpt.input")
      READ(353,*) filename !file containing first passage times (FPTs)
      READ(353,*) allstates !number of total states
      READ(353,*) npts !number of FPTs
      READ(353,*) tau  !stage duration
      READ(353,*) T0 !initial TPMD temperature
      READ(353,*) deltaT !temperature step
      READ(353,*) Tmax !maximum temperature
      READ(353,*) istate !initial state
      READ(353,*) nfstate !# states to be included in group
      ALLOCATE(fstate(ABS(nfstate)))
      IF (nfstate<0 .AND. allstates+nfstate<=0) THEN
         WRITE(6,*) "Err>> Cannot exclude all states"
         STOP
      END IF
      DO i=1,ABS(nfstate)
         READ(353,*) fstate(i)
         IF (fstate(i)<1 .OR. fstate(i)>allstates .OR. fstate(i)==istate) THEN
            WRITE(6,*) "Err >> Final state index appears incorrect"
            STOP
         END IF
      END DO
      CLOSE(353)
   END SUBROUTINE ReadInput
   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   SUBROUTINE Sbrtn1 !generate a table of waiting times
      CALL InitializeRandomSeed(2334)
      CALL CreateTPMDDataset(dataset,tau,T0,deltaT,Tmax) !in tpmdvariables.f90
      CALL GenerateWaitingTimes(dataset,istate,fstate,nfstate,nu,Ea,npts) !in tpmdmodule1.f90
   END SUBROUTINE Sbrtn1
   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   SUBROUTINE Sbrtn2 !bootstrap based on list of waiting times provided
   END SUBROUTINE Sbrtn2
   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   SUBROUTINE PrintDataSet(d,iunit,filename)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: iunit,ipt
      CHARACTER(len=1000), OPTIONAL :: filename

      IF (iunit==6) THEN !print to screen
      ELSE
         IF (.NOT. PRESENT(filename)) THEN
            WRITE(6,*) "Filename should have been provided.."
            STOP
         END IF
         OPEN(UNIT=iunit,FILE=TRIM(filename))
      END IF

      DO ipt=1,d%npts
         WRITE(iunit,*) d%time(ipt)
      END DO

      IF (iunit/=6) CLOSE(iunit)
   END SUBROUTINE PrintDataSet
   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   SUBROUTINE ReadDataSet(d,filename,npts,tau,T0,deltaT,Tmax)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: iunit,ipt,npts
      DOUBLE PRECISION :: tau,T0,deltaT,Tmax
      CHARACTER(len=1000), OPTIONAL :: filename
      CHARACTER :: commentline

      CALL CreateTPMDDataset(d,tau,T0,deltaT,Tmax)
      ALLOCATE(d%time(npts))
      ALLOCATE(d%fstate(npts))
      d%npts=npts

      OPEN(UNIT=553,FILE=TRIM(filename))
      READ(553,*) commentline
      DO ipt=1,npts
         READ(553,*) d%fstate(ipt),d%time(ipt)
      END DO
      CLOSE(553)
   END SUBROUTINE ReadDataSet
   !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
END PROGRAM MAIN
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
