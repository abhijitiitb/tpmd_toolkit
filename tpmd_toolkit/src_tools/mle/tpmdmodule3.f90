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
!#Script name: tpmdmodule3.f90
!#
!#Purpose: Maximum likelihood analysis
!#
!#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
!#
!#Date of Modification: 15 May 2020
!#.......................................................................................................................................

MODULE TPMDModule3
   USE TPMDVariables
   USE TPMDModule1
   USE TPMDModule2
   IMPLICIT NONE

   CONTAINS
   !xxxxxxxxxxxxx
   SUBROUTINE BootstrapArrheniusParameters(d,fstate,nfstate,nsample,stderr_effnu,stderr_effEa)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d,d1
      INTEGER :: nsample,isample,nfstate
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: nu_sum,nu2_sum,E_sum,E2_sum,mean,mean2,var,effEa,effnu
      DOUBLE PRECISION :: stderr_effnu,stderr_effEa

      nu_sum=0.d0
      nu2_sum=0.d0
      E_sum=0.d0
      E2_sum=0.d0
      DO isample=1,nsample
         CALL CreateBootstrapSample(d,d1)
         CALL MLEArrhenius(d1,fstate,nfstate,effnu,effEa,lprint=.FALSE.)
         CALL DeleteTPMDDataset(d1)
         nu_sum=nu_sum+effnu
         nu2_sum=nu2_sum+effnu*effnu
         E_sum=E_sum+effEa
         E2_sum=E2_sum+effEa*effEa
      END DO

      !calculate std error
      mean=nu_sum/DBLE(nsample)
      mean2=nu2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_effnu=SQRT(var)
      !WRITE(6,*) "Std error in eff nu:",SQRT(stderr_effnu)
      mean=E_sum/DBLE(nsample)
      mean2=E2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_effEa=SQRT(var)

!~       stderr_effEa=0.d0
!~       stderr_effnu=0.d0
      !WRITE(6,*) "Std error in eff barrier:",SQRT(stderr_effEa)
   END SUBROUTINE BootstrapArrheniusParameters
   !xxxxxxxxxxxxx
   SUBROUTINE BootstrapNonArrheniusParameters(d,fstate,nfstate,nsample,stderr_effnu,stderr_effEa,stderr_gamma)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d,d1
      INTEGER :: nsample,isample,nfstate
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION ::  nu_sum,nu2_sum,E_sum,E2_sum,g_sum,g2_sum,mean,mean2,var,effEa,effnu,gamma
      DOUBLE PRECISION :: stderr_effnu,stderr_effEa,stderr_gamma

      nu_sum=0.d0
      nu2_sum=0.d0
      E_sum=0.d0
      E2_sum=0.d0
      g_sum=0.d0
      g2_sum=0.d0
      DO isample=1,nsample
         CALL CreateBootstrapSample(d,d1)
         CALL MLEArrhenius(d1,fstate,nfstate,effnu,effEa,gamma,.FALSE.)
         CALL DeleteTPMDDataset(d1)
         nu_sum=nu_sum+effnu
         nu2_sum=nu2_sum+effnu*effnu
         E_sum=E_sum+effEa
         E2_sum=E2_sum+effEa*effEa
         g_sum=g_sum+gamma
         g2_sum=g2_sum+gamma*gamma
      END DO

      !calculate std error
      mean=nu_sum/DBLE(nsample)
      mean2=nu2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_effnu=SQRT(var)
      !WRITE(6,*) "Std error in eff nu:",SQRT(stderr_effnu)
      mean=E_sum/DBLE(nsample)
      mean2=E2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_effEa=SQRT(var)
      !gamma
      mean=g_sum/DBLE(nsample)
      mean2=g2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_gamma=SQRT(var)

!~       stderr_effEa=0.d0
!~       stderr_effnu=0.d0
      !WRITE(6,*) "Std error in eff barrier:",SQRT(stderr_effEa)
   END SUBROUTINE BootstrapNonArrheniusParameters
   !xxxxxxxxxxxxx
   SUBROUTINE BootstrapRate(d,fstate,nfstate,istage,nsample,stderr_effk)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d,d1
      INTEGER :: nsample,isample,nfstate,istage
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: k_sum,k2_sum,mean,mean2,var,effk
      DOUBLE PRECISION :: stderr_effk

      k_sum=0.d0
      k2_sum=0.d0
      DO isample=1,nsample
         CALL CreateBootstrapSample(d,d1)
         CALL MLERateTPMD(d1,fstate,nfstate,effk,istage)
         CALL DeleteTPMDDataset(d1)
         k_sum=k_sum+effk
         k2_sum=k2_sum+effk*effk
      END DO
      mean=k_sum/DBLE(nsample)
      mean2=k2_sum/DBLE(nsample)
      var=mean2-mean*mean
      stderr_effk=SQRT(var)

   END SUBROUTINE BootstrapRate
   !xxxxxxxxxxxxx
   SUBROUTINE CreateBootstrapSample(d,d1)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d,d1
      DOUBLE PRECISION :: r
      INTEGER :: ipt,ipos

      CALL CreateTPMDDataset(d1,d%tau,d%T0,d%deltaT,d%Tmax)
      d1%npts=d%npts
      ALLOCATE(d1%time(d%npts))
      ALLOCATE(d1%fstate(d%npts))
      DO ipt=1,d%npts
         CALL RANDOM_NUMBER(r)
         ipos=CEILING(DBLE(d%npts)*r)
         d1%time(ipt)=d%time(ipos)
         d1%fstate(ipt)=d%fstate(ipos)
      END DO
   END SUBROUTINE CreateBootstrapSample
   !xxxxxxxxxxxxx
   SUBROUTINE MLEArrheniusWithStdDev(d,tau,T0,deltaT,Tmax,istate,fstate,nfstate,nu,Ea,npts)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      DOUBLE PRECISION :: tau,T0,deltaT,Tmax,nu,Ea
      INTEGER :: istate,nfstate,npts,iter,nsample
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: nu_sum,nu2_sum,E_sum,E2_sum,mean,mean2,var,effEa,effnu

      nsample=100
      nu_sum=0.d0
      nu2_sum=0.d0
      E_sum=0.d0
      E2_sum=0.d0
      DO iter=1,nsample
         CALL CreateTPMDDataset(d,tau,T0,deltaT,Tmax) !in tpmdvariables.f90
         CALL GenerateWaitingTimes(d,istate,fstate,nfstate,nu,Ea,npts) !in tpmdmodule1.f90
         CALL MLEArrhenius(d,fstate,nfstate,effnu,effEa)
         nu_sum=nu_sum+effnu
         nu2_sum=nu2_sum+effnu*effnu
         E_sum=E_sum+effEa
         E2_sum=E2_sum+effEa*effEa
         CALL DeleteTPMDDataset(d)
      END DO

      !calculate std error
      mean=nu_sum/DBLE(nsample)
      mean2=nu2_sum/DBLE(nsample)
      var=mean2-mean*mean
      WRITE(6,*) "Eff nu:",mean,"+/-",SQRT(var)," av+/-stdev"
      mean=E_sum/DBLE(nsample)
      mean2=E2_sum/DBLE(nsample)
      var=mean2-mean*mean
      WRITE(6,*) "Eff barrier:",mean,"+/-",SQRT(var)," av+/-stdev"
   END SUBROUTINE MLEArrheniusWithStdDev
   !xxxxxxxxxxxxx
   SUBROUTINE CheckArrheniusRate(d,fstate,nfstate,effnu,effEa,stderr_effnu,stderr_effEa,lprint)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,nfstate,istage
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: k,kArr,effnu,effEa,stderr_effnu,stderr_effEa,stderr_effk,stderr_effkArr
      LOGICAL, OPTIONAL :: lprint
      LOGICAL :: lprint1

      IF (PRESENT(lprint)) THEN
         lprint1=lprint
      ELSE
         lprint1=.FALSE.
      END IF

      IF (lprint1) WRITE(6,*) "Validating Arrhenius approximation by comparing MLE rate to Arrhenius rate from TPMD data ..."
      Nmax=CEILING((d%Tmax-d%T0)/d%deltaT)

      DO istage=0,Nmax
         IF (lprint1) WRITE(6,*) "_________________________________________"
         CALL MLERateTPMD(d,fstate,nfstate,k,istage)
         CALL BootstrapRate(d,fstate,nfstate,istage,100,stderr_effk)
         kArr=effnu*EXP(-effEa/kB/MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax))

         !use propagation of errors
         stderr_effkArr=kArr*SQRT((stderr_effnu/effnu)**2+ &
           (stderr_effEa/kB/MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax))**2)

         WRITE(UNIT=6,FMT='(" >>>Stage ",I3," MLERate",ES12.3," +/- ",ES10.2)') istage,k,stderr_effk
         WRITE(UNIT=6,FMT='(" >>>          ArrRate",ES12.3," +/- ",ES10.2)') kArr,stderr_effkArr
         WRITE(UNIT=6,FMT='(" >>> Temperature",F12.3," Ratio:",ES12.3)') &
          MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax),k/kArr
      END DO
   END SUBROUTINE CheckArrheniusRate
   !xxxxxxxxxxxxx
END MODULE TPMDModule3
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
