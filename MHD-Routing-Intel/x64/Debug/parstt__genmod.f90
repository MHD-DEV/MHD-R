        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 12 11:48:56 2016
        MODULE PARSTT__genmod
          INTERFACE 
            SUBROUTINE PARSTT(NPT,GNRNG,IPCNVG)
              USE VARS_CALIB, ONLY :                                    &
     &          X,                                                      &
     &          XNSTD,                                                  &
     &          BOUND,                                                  &
     &          NOPT, ONLY :                                            &
     &          NPTMAX =>NPT
              INTEGER(KIND=4) :: NPT
              REAL(KIND=8) :: GNRNG
              INTEGER(KIND=4) :: IPCNVG
            END SUBROUTINE PARSTT
          END INTERFACE 
        END MODULE PARSTT__genmod
