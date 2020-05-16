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
!#Script name: tpmdmodule1.f90
!#
!#Purpose: Generate waiting times given the rate parameters (mainly for testing code)
!#
!#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
!#
!#Date of Modification: 15 May 2020
!#.......................................................................................................................................

MODULE TPMDModule1
   USE TPMDVariables
   IMPLICIT NONE

   CONTAINS
   !xxxxxxxxxxxxx
   SUBROUTINE GenerateWaitingTimes(d,istate,fstate,nfstate,nu,Ea,npts,lprint)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: istate,nfstate,npts,Nmax,ipt,istage,stage,i
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: nu,Ea,tau,T,r,dt,k
      LOGICAL :: EscapeFound,lprint1
      LOGICAL, OPTIONAL :: lprint

      IF (PRESENT(lprint)) THEN
         lprint1=lprint
      ELSE
         lprint1=.FALSE.
      END IF

      IF (ALLOCATED(d%time)) THEN
         IF (SIZE(d%time)/=npts) THEN
            DEALLOCATE(d%time)
            DEALLOCATE(d%fstate)
            ALLOCATE(d%time(npts))
            ALLOCATE(d%fstate(npts))
            d%npts=npts
         END IF
      ELSE
         ALLOCATE(d%time(npts))
         ALLOCATE(d%fstate(npts))
         d%npts=npts
      END IF

      Nmax=(d%Tmax-d%T0)/d%deltaT
      IF (lprint1) THEN
         WRITE(UNIT=6,FMT='("Temperature stages: 0 -",i2)') Nmax
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='("Stage ",i2," .. temperature ",F8.1," K")') istage,MIN(d%T0+istage*d%deltaT,d%Tmax)
         END DO
         WRITE(6,*) ""
         WRITE(6,*) ""
      END IF

      tau=d%tau
      DO ipt=1,npts
         d%time(ipt)=0.
         EscapeFound=.FALSE.
         DO istage=0,Nmax-1
            T=d%T0+istage*d%deltaT
            k=nu*EXP(-Ea/kB/T)
            CALL RANDOM_NUMBER(r)
            dt=-LOG(r)/k
            IF (dt<tau) THEN
               d%time(ipt)=d%time(ipt)+dt
               EscapeFound=.TRUE.
               stage=istage
               EXIT
            ELSE
               d%time(ipt)=d%time(ipt)+tau
            END IF
         END DO
         IF (.NOT. EscapeFound) THEN
            T=d%T0+Nmax*d%deltaT
            k=nu*EXP(-Ea/kB/T)
            CALL RANDOM_NUMBER(r)
            dt=-LOG(r)/k
            d%time(ipt)=d%time(ipt)+dt
            stage=Nmax
         END IF
         CALL RANDOM_NUMBER(r)
         i=CEILING(r*REAL(nfstate))
         d%fstate(ipt)=fstate(i) !since all states are equivalent
      END DO
   END SUBROUTINE GenerateWaitingTimes
   !xxxxxxxxxxxxx
END MODULE TPMDModule1
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
