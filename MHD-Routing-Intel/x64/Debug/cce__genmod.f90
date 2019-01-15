        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 17 09:57:16 2016
        MODULE CCE__genmod
          INTERFACE 
            SUBROUTINE CCE(S,SF,ICALL,ISEED)
              USE VARS_CALIB, ONLY :                                    &
     &          NOPT,                                                   &
     &          NPS,                                                    &
     &          BLOW,                                                   &
     &          BUPP,                                                   &
     &          MAXN,                                                   &
     &          XNSTD
              REAL(KIND=8) :: S(NPS,NOPT)
              REAL(KIND=8) :: SF(NPS)
              INTEGER(KIND=4) :: ICALL
              INTEGER(KIND=4) :: ISEED
            END SUBROUTINE CCE
          END INTERFACE 
        END MODULE CCE__genmod
