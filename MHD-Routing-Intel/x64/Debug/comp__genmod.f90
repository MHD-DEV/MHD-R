        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 17 09:57:16 2016
        MODULE COMP__genmod
          INTERFACE 
            SUBROUTINE COMP(NPT1,NGS1,NGS2,AF)
              USE VARS_CALIB, ONLY :                                    &
     &          NPG,                                                    &
     &          NPT, ONLY :                                             &
     &          A =>X,                                                  &
     &          
                N =>NOPT,                                               &
     &          
                B =>CX,                                                 &
     &          
                BF =>CF
              INTEGER(KIND=4) :: NPT1
              INTEGER(KIND=4) :: NGS1
              INTEGER(KIND=4) :: NGS2
              REAL(KIND=8) :: AF(NPT)
            END SUBROUTINE COMP
          END INTERFACE 
        END MODULE COMP__genmod
