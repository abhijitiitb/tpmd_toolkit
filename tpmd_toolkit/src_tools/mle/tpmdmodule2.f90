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
!#Script name: tpmdmodule2.f90
!#
!#Purpose: Contains all subroutines required for estimating rates, pre-exponential factor and barrier
!#
!#Authors: Abhijit Chatterjee (abhijit@che.iitb.ac.in)
!#
!#Date of Modification: 15 May 2020
!#.......................................................................................................................................


MODULE TPMDModule2
   USE TPMDVariables
   IMPLICIT NONE
   
   CONTAINS
   !xxxxxxxxxxxxx
   SUBROUTINE MLERate(d,rate)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      DOUBLE PRECISION :: rate
      
      WRITE(6,*) "Obtain MLE rate (const temperature)"
      rate=DBLE(d%npts)/SUM(d%time(1:d%npts))
   END SUBROUTINE MLERate
   !xxxxxxxxxxxxx
   SUBROUTINE PartitionTPMDTimesToStages(d,s,fstate,nfstate,lprint)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      TYPE(TPMDStagePartition), POINTER :: s
      INTEGER :: NMax,nstage,ipt,istage,m,nfstate,x
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: invTna_sum,t
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: theta,dtheta
      INTEGER, DIMENSION(:), ALLOCATABLE :: count,StagewisePoints
      LOGICAL, OPTIONAL :: lprint
      LOGICAL :: lprint1

      IF (PRESENT(lprint)) THEN
         lprint1=lprint
      ELSE
         lprint1=.FALSE.
      END IF 

      IF (lprint1) WRITE(6,*) "Partitioning TPMD waiting times according to stages"
      !STEP 1: first we shall find size of TPMDStagePartition object
      Nmax=CEILING((d%Tmax-d%T0)/d%deltaT)
      IF (lprint1) THEN
         WRITE(UNIT=6,FMT='("Temperature stages: 0 -",i2)') Nmax
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='("Stage ",i2," .. temperature ",F8.1," K")') istage,MIN(d%T0+istage*d%deltaT,d%Tmax)
         END DO
         WRITE(6,*) ""
         WRITE(6,*) ""
      END IF
 
      !calculate theta and temperature at last stage
      ALLOCATE(theta(0:Nmax))
      ALLOCATE(dtheta(0:Nmax))
      ALLOCATE(count(0:Nmax))
      ALLOCATE(StagewisePoints(0:Nmax))
      theta=0.d0
      invTna_sum=0.d0
      m=0
      dtheta=0.d0
      count=0
      StagewisePoints=0
      DO ipt=1,d%npts
         
         t=d%time(ipt)
         nstage=MIN(FLOOR(t/d%tau-1.e-10),Nmax) !e.g., any time between 0-tau belongs to stage 0
         
         DO istage=0,nstage-1
            dtheta(istage)=dtheta(istage)+MIN(t,d%tau)
            t=t-MIN(t,d%tau)
            StagewisePoints(istage)=StagewisePoints(istage)+1
            IF (t<=0.d0) EXIT
         END DO
         IF (t>0.d0) THEN
            dtheta(Nmax)=dtheta(Nmax)+t
            StagewisePoints(nstage)=StagewisePoints(nstage)+1
         END IF
         
         IF (ANY(d%fstate(ipt)==fstate(1:nfstate))) THEN
            invTna_sum=invTna_sum+1.d0/(MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax))
            count(nstage)=count(nstage)+1
            m=m+1 !# transitions to fstate
            theta(0:Nmax)=theta(0:Nmax)+dtheta(0:Nmax)
            IF (m<4 .AND. lprint1) THEN
               WRITE(UNIT=6,FMT='(I4,":")',ADVANCE="NO") m
               DO istage=0,Nmax
                  WRITE(UNIT=6,FMT='(F10.3)',ADVANCE="NO") dtheta(istage)
               END DO
               WRITE(6,*) "FinalTemperature:",MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax),"Stage:",nstage
            END IF
            dtheta=0.d0 !reinitialize
         END IF
      END DO
     
      !Step 2: Now fill this information in TPMDStagePartition
      IF (ASSOCIATED(s)) THEN
         WRITE(6,*) "Err>> TPMDStagePartition object is associated"
         STOP
      END IF
      ALLOCATE(s)
      s%Nmax=Nmax
      ALLOCATE(s%theta(0:Nmax)); s%theta(0:Nmax)=theta(0:Nmax)
      ALLOCATE(s%count(0:Nmax)); s%count(0:Nmax)=count(0:Nmax)
      s%invTna_sum=invTna_sum
      s%m=m

      !Step 3: Fill stagewise partitioned times
      ALLOCATE(s%StagewiseBreakupTime(0:Nmax,1:MAXVAL(StagewisePoints(0:Nmax)))) !what time was accessed in this stage
      ALLOCATE(s%StagewiseBreakupState(0:Nmax,1:MAXVAL(StagewisePoints(0:Nmax)))) !which state did we move to
      ALLOCATE(s%StagewisePoints(0:Nmax)) !number of times collected in each stage
      s%StagewiseBreakupTime(0:Nmax,1:MAXVAL(StagewisePoints(0:Nmax)))=0.d0
      s%StagewiseBreakupState(0:Nmax,1:MAXVAL(StagewisePoints(0:Nmax)))=0
      s%StagewisePoints(0:Nmax)=0
      
      DO ipt=1,d%npts
         
         t=d%time(ipt)
         nstage=MIN(FLOOR(t/d%tau-1.e-10),Nmax) !e.g., any time between 0-tau belongs to stage 0
         
         DO istage=0,nstage-1
            x=s%StagewisePoints(istage)
            x=x+1
            IF (x>MAXVAL(StagewisePoints(0:Nmax))) THEN
               WRITE(6,*) "Wierd x:",x
               STOP
            END IF
            s%StagewiseBreakupTime(istage,x)=MIN(t,d%tau)
            s%StagewisePoints(istage)=x
            t=t-MIN(t,d%tau)
            IF (t<=0.d0) EXIT
         END DO
         IF (t>0.d0) THEN
            x=s%StagewisePoints(nstage)
            x=x+1
            s%StagewisePoints(nstage)=x
            s%StagewiseBreakupTime(nstage,x)=t
            s%StagewiseBreakupState(nstage,x)=d%fstate(ipt) !state where we move to
         END IF
      END DO
      
      DEALLOCATE(theta,dtheta,count,StagewisePoints)
      IF (lprint1) THEN
         WRITE(6,*) "inverse T_finalstage:",s%invTna_sum
         write(6,*) "npts:",s%m
         WRITE(UNIT=6,FMT='("theta:")',ADVANCE="NO") 
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(ES15.5)',ADVANCE="NO") s%theta(istage)
         END DO
         WRITE(6,*) " SUM:",SUM(s%theta)
         WRITE(UNIT=6,FMT='("count:")',ADVANCE="NO") 
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(I6)',ADVANCE="NO") s%count(istage)
         END DO
         WRITE(6,*) " SUM:",SUM(s%count)
      END IF
      
      IF (lprint1) THEN
         WRITE(6,*) "Printing stagewise information ..."
         WRITE(6,*) "# points collected:",s%StagewisePoints(0:s%Nmax)
         WRITE(6,*) "times collected:"
         WRITE(UNIT=6,FMT='("Stage ")',ADVANCE="NO")
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(I12)',ADVANCE="NO") istage
         END DO
         WRITE(6,*) ""
         DO ipt=1,MIN(15,MINVAL(s%StagewisePoints(0:s%Nmax)))
            WRITE(UNIT=6,FMT='("       ")',ADVANCE="NO") 
            DO istage=0,Nmax
               WRITE(UNIT=6,FMT='(ES12.2)',ADVANCE="NO") s%StagewiseBreakupTime(istage,ipt)
            END DO
            WRITE(6,*) ""
         END DO
         WRITE(6,*) "states collected:"
         WRITE(UNIT=6,FMT='("Stage ")',ADVANCE="NO")
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(I12)',ADVANCE="NO") istage
         END DO
         WRITE(6,*) ""
         DO ipt=1,MIN(15,MINVAL(s%StagewisePoints(0:s%Nmax)))
            WRITE(UNIT=6,FMT='("       ")',ADVANCE="NO") 
            DO istage=0,Nmax
               WRITE(UNIT=6,FMT='(I12)',ADVANCE="NO") s%StagewiseBreakupState(istage,ipt)
            END DO
            WRITE(6,*) ""
         END DO
      END IF
   END SUBROUTINE PartitionTPMDTimesToStages
   !xxxxxxxxxxxxx
   SUBROUTINE MLEArrhenius(d,fstate,nfstate,effnu,effEa,gamma,lprint)
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,nstage,ipt,istage,iter,nfstate,m
      INTEGER :: ix,iy
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION, OPTIONAL :: gamma
      DOUBLE PRECISION :: invTna_sum,effnu,effEa,t,res,dres,Ed,dE,dEmax,gd,dgamma,resnew,dEuse,dgammause
      DOUBLE PRECISION :: lnTna_sum,sclfn(30)
      DOUBLE PRECISION :: Ja,Jb,Jc,Jd,determinant,f1x,f1y,f2x,f2y,f1,f2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: theta,dtheta
      INTEGER, DIMENSION(:), ALLOCATABLE :: count
      LOGICAL, OPTIONAL :: lprint
      LOGICAL :: lprint1
      
      IF (PRESENT(lprint)) THEN
         lprint1=lprint
      ELSE
         lprint1=.FALSE.
      END IF 
      
      IF (lprint1) WRITE(6,*) "Obtain MLE rate (Arrhenius) ..."
      Nmax=CEILING((d%Tmax-d%T0)/d%deltaT)
      IF (lprint1) THEN
         WRITE(UNIT=6,FMT='("Temperature stages: 0 -",i2)') Nmax
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='("Stage ",i2," .. temperature ",F8.1," K")') istage,MIN(d%T0+istage*d%deltaT,d%Tmax)
         END DO
         WRITE(6,*) ""
         WRITE(6,*) ""
      END IF
      
      !calculate theta and temperature at last stage
      ALLOCATE(theta(0:Nmax))
      ALLOCATE(dtheta(0:Nmax))
      ALLOCATE(count(0:Nmax))
      theta=0.d0
      invTna_sum=0.d0
      lnTna_sum=0.d0
      m=0 !number of transitions
      dtheta=0.d0
      count=0
      
      IF (lprint1) WRITE(6,*) "Breakup of theta for first 3 transitions..."
      DO ipt=1,d%npts
         
         t=d%time(ipt)
         nstage=MIN(FLOOR(t/d%tau-1.e-10),Nmax) !e.g., any time between 0-tau belongs to stage 0
         
         DO istage=0,Nmax-1
            dtheta(istage)=dtheta(istage)+MIN(t,d%tau)
            t=t-MIN(t,d%tau)
            IF (t<=0.d0) EXIT
         END DO
         dtheta(Nmax)=dtheta(Nmax)+t
         
         IF (ANY(d%fstate(ipt)==fstate(1:nfstate))) THEN !collect the time
            invTna_sum=invTna_sum+1.d0/(MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax))
            lnTna_sum=lnTna_sum+LOG((MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax))/d%T0)
            count(nstage)=count(nstage)+1
            m=m+1 !# transitions to fstate
            theta(0:Nmax)=theta(0:Nmax)+dtheta(0:Nmax)
            IF (m<4 .AND. lprint1) THEN
               WRITE(UNIT=6,FMT='(I4,":")',ADVANCE="NO") m
               DO istage=0,Nmax
                  WRITE(UNIT=6,FMT='(F10.3)',ADVANCE="NO") dtheta(istage)
               END DO
               WRITE(6,*) "FinalTemperature:",MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax),"Stage:",nstage
            END IF
            dtheta=0.d0 !reinitialize
         END IF
      END DO
      
      IF (lprint1) WRITE(6,*) ""
      IF (lprint1) WRITE(6,*) ""

      IF (lprint1) THEN
         WRITE(6,*) "Sum of inverse T_finalstage:",invTna_sum/DBLE(m)
         WRITE(6,*) "Sum of ln T_finalstage:",lnTna_sum/DBLE(m)
         write(6,*) "npts:",m
         WRITE(UNIT=6,FMT='("theta:")',ADVANCE="NO") 
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(ES15.5)',ADVANCE="NO") theta(istage)
         END DO
         WRITE(6,*) " SUM:",SUM(theta)
         WRITE(UNIT=6,FMT='("count:")',ADVANCE="NO") 
         DO istage=0,Nmax
            WRITE(UNIT=6,FMT='(I6)',ADVANCE="NO") count(istage)
         END DO
         WRITE(6,*) " SUM:",SUM(count)
      END IF

      IF (m<10) THEN
         effEa=0.d0
         effnu=0.d0
         IF (lprint1) WRITE(6,*) "Insufficient points.."
         RETURN
      END IF
      !estimate the parameters
      invTna_sum=invTna_sum/DBLE(m)
      lnTna_sum=lnTna_sum/DBLE(m)
      effEa=0.21
      dEmax=0.1
      !minimization for residual 
!~       DO iter=-10,10
!~          effEa=0.4+DBLE(iter)*0.01
!~          WRITE(6,*) effEa,fn(effEa,Nmax,theta,invTna_sum,d)
!~       END DO
!~       stop

!~       !root-finding
!~       WRITE(6,*) "  Iteration           EffBarrier          Residual                 DervResidual"

!do ix=1,20
!do iy=1,20
!ed=0.1+(ix-1)*0.0158
!gd=0.1+(iy-1)*0.0737
!f1=fnA(ed,gd,Nmax,theta,invTna_sum,d)
!f2=fnB(ed,gd,Nmax,theta,lnTna_sum,d)
!write(unit=6,fmt='(es12.3)',advance="no") f2
!end do
!write(6,*) ""
!end do
!stop
      IF (PRESENT(gamma)) THEN !non-Arrhenius case
         gamma=0.d0
         DO iter=1,30
            f1=fnA(effEa,gamma,Nmax,theta,invTna_sum,d)
            f2=fnB(effEa,gamma,Nmax,theta,lnTna_sum,d)
            !find the Jacobian matrix [Ja Jb; Jc Jd]
            !let f1 and f2 denote the functions and x denote Ea and y
            !denote gamma (non-Arrhenius power law term)
            Ed=effEa+0.001d0; f1x=fnA(Ed,gamma,Nmax,theta,invTna_sum,d)
            Ed=effEa-0.001d0; f1x=f1x-fnA(Ed,gamma,Nmax,theta,invTna_sum,d)
            f1x=f1x/0.002d0
            !xxxxxxxxxxx
            gd=gamma+0.001d0; f1y=fnA(effEa,gd,Nmax,theta,invTna_sum,d)
            gd=gamma-0.001d0; f1y=f1y-fnA(effEa,gd,Nmax,theta,invTna_sum,d)
            f1y=f1y/0.002d0
            !xxxxxxxxxxx
            Ed=effEa+0.001d0; f2x=fnB(Ed,gamma,Nmax,theta,lnTna_sum,d)
            Ed=effEa-0.001d0; f2x=f2x-fnB(Ed,gamma,Nmax,theta,lnTna_sum,d)
            f2x=f2x/0.002d0
            !xxxxxxxxxxx
            gd=gamma+0.001d0; f2y=fnB(effEa,gd,Nmax,theta,lnTna_sum,d)
            gd=gamma-0.001d0; f2y=f2y-fnB(effEa,gd,Nmax,theta,lnTna_sum,d)
            f2y=f2y/0.002d0
            !Finally
            Ja=f1x
            Jb=f1y
            Jc=f2x
            Jd=f2y
            determinant=Jb*Jc-Ja*Jd
            !xxxxxxxxxxxxx
            dE=-(f2*Jb-f1*Jd)/determinant
            dgamma=(f2*Ja-f1*Jc)/determinant
            !line search
            DO ix=1,30
               Ed=effEa+REAL(ix-1)*0.05*dE
               gd=gamma+REAL(ix-1)*0.05*dgamma
               sclfn(ix)=fnA(Ed,gd,Nmax,theta,invTna_sum,d)**2+fnB(Ed,gd,Nmax,theta,lnTna_sum,d)**2
            END DO
            ix= minloc(sclfn,1)
            dE=REAL(ix-1)*0.05*dE
            dgamma=REAL(ix-1)*0.05*dgamma
!            stop
!write(6,*) "[f1,f2]=",f1,f2
!write(6,*) "J=",Ja,Jb,Jc,Jd
!write(6,*) "det=[",determinant
!write(6,*) "dE,dg=",dE,dgamma
!            dEuse=MIN(0.05,ABS(dE))
!            dE=dE/ABS(dE)*dEuse
!            dgammause=MIN(0.01,ABS(dgamma))
!            dgamma=dgamma/ABS(dgamma)*dgammause
            IF (lprint1) WRITE(6,*) "...",iter,effEa,gamma,f1,f2,MINVAL(sclfn)
            IF (ABS(f1)<1.e-8 .AND. ABS(f2)<1.e-8) EXIT
            effEa=effEa+dE
            gamma=gamma+dgamma
         END DO
         
         !pre-exponential factor
         effnu=DBLE(m)/fnC(effEa,gamma,Nmax,theta,invTna_sum,d)
         IF (lprint1) WRITE(6,*) "Effective nu, barrier and gamma:",effnu,effEa,gamma

      ELSE !Arrhenius case

         IF (lprint1) WRITE(6,*) "  Iteration   Effective Ea(eV)    Residual    DeltaResidual"
         DO iter=1,30
            res=fn(effEa,Nmax,theta,invTna_sum,d)
            Ed=effEa+0.001d0
            dres=fn(Ed,Nmax,theta,invTna_sum,d)
            Ed=effEa-0.001d0
            dres=dres-fn(Ed,Nmax,theta,invTna_sum,d)
            dres=dres/0.002d0
            dE=-res/dres*0.8
            !line search
            DO ix=1,30
               Ed=effEa+REAL(ix-1)*0.05*dE
               sclfn(ix)=fn(Ed,Nmax,theta,invTna_sum,d)**2
            END DO
            ix= minloc(sclfn,1)
            dE=REAL(ix-1)*0.05*dE
            !dEuse=MIN(0.03,ABS(dE))
            !dE=dE/ABS(dE)*dEuse
            IF (lprint1) WRITE(UNIT=6,FMT='("...",I3,3("      ",ES12.3))') iter,effEa,res,dres
            IF (ABS(res)<1.e-10) EXIT
            effEa=effEa+dE
         END DO
   
         
         !pre-exponential factor
         effnu=DBLE(m)/fn1(effEa,Nmax,theta,invTna_sum,d)
         IF (lprint1) WRITE(6,*) "Effective nu & barrier:",effnu,effEa
      
      END IF
      
!~       WRITE(6,*) "Printing Arrhenius corrections ..."
!~       DO istage=0,Nmax
!~          IF (theta(istage)>0.d0) THEN
!~             WRITE(UNIT=6,FMT='(ES14.4," @",ES12.3)') DBLE(count(istage))/theta(istage)/effnu/ &
!~               EXP(-effEa/kB/MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)), &
!~               MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
!~          ELSE
!~             WRITE(UNIT=6,FMT='(ES14.4," @",ES12.3)') 0.d0,MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
!~          END IF
!~       END DO
      
      DEALLOCATE(theta,dtheta,count)
   END SUBROUTINE MLEArrhenius
   !xxxxxxxxxxxxx
   FUNCTION fn(E,Nmax,theta,invTna_sum,d) !Arrhenius
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,istage
      DOUBLE PRECISION, DIMENSION(0:Nmax) :: theta
      DOUBLE PRECISION :: E,term1,term2,k,T,invTna_sum,fn
      
      term1=0.d0
      term2=0.d0
      DO istage=0,Nmax
         T=MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
         k=EXP(-E/kB/T)
         term1=term1+k*theta(istage)/T
         term2=term2+k*theta(istage)
      END DO
      fn=term1/term2-invTna_sum
   END FUNCTION fn
   !xxxxxxxxxxxxx
   FUNCTION fnA(E,g,Nmax,theta,invTna_sum,d) !non-Arrhenius
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,istage
      DOUBLE PRECISION, DIMENSION(0:Nmax) :: theta
      DOUBLE PRECISION :: E,g,term1,term2,k,T,T0,invTna_sum,fnA
      
      term1=0.d0
      term2=0.d0
      T0=d%T0
      DO istage=0,Nmax
         T=MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
         k=EXP(-E/kB/T)
         term1=term1+k*(T/T0)**g*theta(istage)/T
         term2=term2+k*(T/T0)**g*theta(istage)
      END DO
      fnA=term1/term2-invTna_sum
   END FUNCTION fnA
   !xxxxxxxxxxxxx
   FUNCTION fnB(E,g,Nmax,theta,lnTna_sum,d) !non-Arrhenius
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,istage
      DOUBLE PRECISION, DIMENSION(0:Nmax) :: theta
      DOUBLE PRECISION :: E,g,term1,term2,k,T,T0,lnTna_sum,fnB

      term1=0.d0
      term2=0.d0
      T0=d%T0
      DO istage=0,Nmax
         T=MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
         k=EXP(-E/kB/T)
         term1=term1+k*(T/T0)**g*theta(istage)*LOG(T/T0)
         term2=term2+k*(T/T0)**g*theta(istage)
      END DO
      fnB=term1/term2-lnTna_sum
      fnB=term1-term2*lnTna_sum
   END FUNCTION fnB
   !xxxxxxxxxxxxx
   FUNCTION fn1(E,Nmax,theta,invTna_sum,d) !Arrhenius case
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,istage
      DOUBLE PRECISION, DIMENSION(0:Nmax) :: theta
      DOUBLE PRECISION :: E,term2,k,T,invTna_sum,fn1
      
      term2=0.d0
      DO istage=0,Nmax
         T=MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
         k=EXP(-E/kB/T)
         term2=term2+k*theta(istage)
      END DO
      fn1=term2
   END FUNCTION fn1
   !xxxxxxxxxxxxx
   FUNCTION fnC(E,g,Nmax,theta,invTna_sum,d) !non-Arrhenius
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,istage
      DOUBLE PRECISION, DIMENSION(0:Nmax) :: theta
      DOUBLE PRECISION :: E,g,term2,k,T,T0,invTna_sum,fnC
      
      term2=0.d0
      T0=d%T0
      DO istage=0,Nmax
         T=MIN(d%T0+DBLE(istage)*d%deltaT,d%Tmax)
         k=EXP(-E/kB/T)*(T/T0)**g
         term2=term2+k*theta(istage)
      END DO
      fnC=term2
   END FUNCTION fnC
   !xxxxxxxxxxxxx
   SUBROUTINE MLERateTPMD(d,fstate,nfstate,k,stage,lprint)
   !find the rate "k" at the selected stage "stage"
      IMPLICIT NONE
      TYPE(TPMDData), POINTER :: d
      INTEGER :: Nmax,nstage,ipt,stage,iter,nfstate,m
      INTEGER, DIMENSION(:), POINTER :: fstate
      DOUBLE PRECISION :: theta,dtheta,k,t
      LOGICAL, OPTIONAL :: lprint
      LOGICAL :: lprint1
      
      IF (PRESENT(lprint)) THEN
         lprint1=lprint
      ELSE
         lprint1=.FALSE.
      END IF 
      
      IF (lprint1) WRITE(6,*) "Obtain MLE rate (Arrhenius) ..."
      Nmax=CEILING((d%Tmax-d%T0)/d%deltaT)
      
      !calculate theta and temperature at last stage
      theta=0.d0
      m=0
      dtheta=0.d0
      DO ipt=1,d%npts
         
         t=d%time(ipt)
         nstage=MIN(FLOOR(t/d%tau-1.e-10),Nmax) !e.g., any time between 0-tau belongs to stage 0
         t=t-d%tau*DBLE(stage) !stage 1 we should remove 
         IF (t>0.d0) dtheta=dtheta+MIN(t,d%tau)
         
         IF (ANY(d%fstate(ipt)==fstate(1:nfstate)) .AND. nstage==stage) THEN !transition happens at stage to fstate
            m=m+1 !# transitions to fstate
            theta=theta+dtheta
            IF (m<4 .AND. lprint1) THEN
               WRITE(UNIT=6,FMT='(I4,":",ES15.5)',ADVANCE="NO") m,dtheta
               WRITE(6,*) "FinalTemperature:",MIN(d%T0+DBLE(nstage)*d%deltaT,d%Tmax),"Stage:",nstage
            END IF
            dtheta=0.d0 !reinitialize
         END IF
      END DO
      
      IF (lprint1) THEN
         write(6,*) "npts:",m
         WRITE(UNIT=6,FMT='("theta:",ES15.5)') theta
      END IF

      IF (m<10) THEN
         k=0.d0
         IF (lprint1) WRITE(6,*) "Unable to estimate rates due to insufficient points.."
         RETURN
      END IF
      !estimate the parameters
      k=DBLE(m)/theta
      IF (lprint1) WRITE(6,*) "Effective rate:",k
      
   END SUBROUTINE MLERateTPMD
   !xxxxxxxxxxxxx
END MODULE TPMDModule2
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
