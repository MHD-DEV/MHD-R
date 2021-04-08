SUBROUTINE Gera_Medias
USE Vars_main
IMPLICIT NONE
integer, dimension(1:12):: diasmes=(/31,28,31,30,31,30,31,31,30,31,30,31/)
integer:: iaux, k, ano, mes, dia, h, iauxa, iauxm(12)
real, allocatable:: qdiaria(:),qmensal(:,:),qanual(:)
diasmes=30
WRITE(*,*)'GERANDO MÉDIAS'
qmensal=0.


!write(*,*) QRG(19,it)
allocate(qmensal(NHIDG,12),qdiaria(NHIDG),qanual(NHIDG))

open(51, FILE=DIR_DADOS//'Saida/VAZAO_DIARIA.HIG', status='unknown',decimal='COMMA')
open(52, FILE=DIR_DADOS//'Saida/VAZAO_ANUAL.HIG', status='unknown',decimal='COMMA')
it=0
iauxa=0
iauxm=0
iaux=0
do ano = IANOI, IANOFIN

    do mes=imesi,12

        do dia=idiai,diasmes(mes)

            do h=ihorai,21,3

                iaux=iaux+1
                it=it+1
                iauxa=iauxa+1
                iauxm(mes)=iauxm(mes)+1
                DO k=1, NHIDG
                    qdiaria(k) =  qdiaria(k) + QRG(k,it)
                    qanual(k) = qanual(k) + QRG(k,it)
                ENDDO
            enddo

            write(51,'(2(I2.2,A1),I4,<NHIDG>F15.3)') dia,'/',mes,'/',ano,(qdiaria(k)/iaux,k=1,NHIDG)
            ihorai=0
            qdiaria=0.
            iaux=0
        enddo
        idiai=1
    enddo
    write(52,'(I4, A1, <NHIDG>F15.3)') ano,'/', (qanual(k)/iauxa,k=1,NHIDG)
    imesi=1
    iauxa=0
enddo
close(51)
close(52)

!open(51, FILE=DIR_DADOS//'Saida/VAZAO_DIARIA.HIG', status='unknown')
!open(52, FILE=DIR_DADOS//'Saida/VAZAO_ANUAL.HIG', status='unknown')
!do ano=IANOI+3, IANOFIN
!    iaux=iaux+1
!    qanual=0.
!    do mes=1, 12
!!        diasmes(2)=28
!!        if(mes==2 .and. mod(ano,4)==0) diasmes(2)=29
!        do dia=1, diasmes(mes)
!            qdiaria=0.
!            do h=1,8
!                it=it+1
!                do k=1, NHIDG
!                        qdiaria(k)=qdiaria(k) + QRG(k,it)
!                        qmensal(k,mes)=qmensal(k,mes) + QRG(k,it)/real(diasmes(mes))/8
!                        qanual(k)=qanual(k) + qdiaria(k)/365
!                    enddo !subbacias
!                    write(51,'(2(I2.2,A1),I4,<NHIDG>F15.3)') dia,'/',mes,'/',ano,(qdiaria(k)/8,k=1,NHIDG)
!            enddo !horas
!        enddo !dia
!    enddo !mes
!    write(52, '(I4,<NHIDG>F15.3)') ano,(qanual(k),k=1,NHIDG)
!enddo !ano
!
!close(51)
!close(52)
!
!open(50, FILE=DIR_DADOS//'Saida/VAZAO_MENSAL.HIG', status='unknown')
!do mes = 1, 12
!        write(50,'(I2.2,<NHIDG>F12.2)') mes,(qmensal(k,mes)/(IANOFIN-IANOI-1), k=1, NHIDG)
!enddo
!
!close(50)
!deallocate(qmensal)

return
End
