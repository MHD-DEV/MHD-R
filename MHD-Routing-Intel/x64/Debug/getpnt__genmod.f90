        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 17 09:57:16 2016
        MODULE GETPNT__genmod
          INTERFACE 
            SUBROUTINE GETPNT(IDIST,ISEED,X,STD,XI)
              USE VARS_CALIB, ONLY :                                    &
     &          NOPT,                                                   &
     &          BLOW,                                                   &
     &          BUPP
              INTEGER(KIND=4) :: IDIST
              INTEGER(KIND=4) :: ISEED
              REAL(KIND=8) :: X(NOPT)
              REAL(KIND=8) :: STD(NOPT)
              REAL(KIND=8) :: XI(NOPT)
            END SUBROUTINE GETPNT
          END INTERFACE 
        END MODULE GETPNT__genmod
