! From Leonard J. Moss of SLAC:

! Here's a hybrid QuickSort I wrote a number of years ago.  It's
! based on suggestions in Knuth, Volume 3, and performs much better
! than a pure QuickSort on short or partially ordered input arrays.  
      SUBROUTINE SORT(N,VALUES,ISEQ)
!===================================================================
!
!     SORT -- SORT, Real input, ISEQ output
!
!
!     Input:  N     INTEGER
!             VALUES  REAL
!
!     Output: ISEQ INTEGER (DIMENSION N)
!
! This routine performs an in-memory sort of the first N elements of
! array VALUES, returning into array ISEQ the indices of elements of
! VALUES arranged in ascending order.  Thus,
!
!    VALUES(ISEQ(1)) will be the smallest number in array VALUES;
!    VALUES(ISEQ(N)) will be the largest number in VALUES.
!
! The original data is not physically rearranged.  The original order
! of equal input values is not necessarily preserved.
!
!===================================================================
!
! SORTRX uses a hybrid QuickSort algorithm, based on several
! suggestions in Knuth, Volume 3, Section 5.2.2.  In particular, the
! "pivot key" [my term] for dividing each subsequence is chosen to be
! the median of the first, last, and middle values of the subsequence;
! and the QuickSort is cut off when a subsequence has 9 or fewer
! elements, and a straight insertion sort of the entire array is done
! at the end.  The result is comparable to a pure insertion sort for
! very short arrays, and very fast for very large arrays (of order 12
! micro-sec/element on the 3081K for arrays of 10K elements).  It is
! also not subject to the poor performance of the pure QuickSort on
! partially ordered data.
!
! Created:  15 Jul 1986  Len Moss
!
!===================================================================
 
      INTEGER   N,ISEQ(N)
      REAL      VALUES(N)
 
      INTEGER   LSTK(31),RSTK(31),ISTK
      INTEGER   L,R,I,J,P,ISEQP,ISEQT
      REAL      VALUESP
 
!     QuickSort Cutoff
!
!     Quit QuickSort-ing when a subsequence contains M or fewer
!     elements and finish off at end with straight insertion sort.
!     According to Knuth, V.3, the optimum value of M is around 9.
 
      INTEGER   M
      PARAMETER (M=9)
 
!===================================================================
!
!     Make initial guess for ISEQ
 
      DO I=1,N
         ISEQ(I)=I
      enddo
 
!     If array is short, skip QuickSort and go directly to
!     the straight insertion sort.
 
      IF (N.LE.M) GOTO 900
 
!===================================================================
!
!     QuickSort
!
!     The "Qn:"s correspond roughly to steps in Algorithm Q,
!     Knuth, V.3, PP.116-117, modified to select the median
!     of the first, last, and middle elements as the "pivot
!     key" (in Knuth's notation, "K").  Also modified to leave
!     data in place and produce an ISEQ array.  To simplify
!     comments, let VALUES[I]=VALUES(ISEQ(I)).
 
! Q1: Initialize
      ISTK=0
      L=1
      R=N
 
  200 CONTINUE
 
! Q2: Sort the subsequence VALUES[L]..VALUES[R].
!
!     At this point, VALUES[l] <= VALUES[m] <= VALUES[r] for all l < L,
!     r > R, and L <= m <= R.  (First time through, there is no
!     VALUES for l < L or r > R.)
 
      I=L
      J=R
 
! Q2.5: Select pivot key
!
!     Let the pivot, P, be the midpoint of this subsequence,
!     P=(L+R)/2; then rearrange ISEQ(L), ISEQ(P), and ISEQ(R)
!     so the corresponding VALUES values are in increasing order.
!     The pivot key, VALUESP, is then VALUES[P].
 
      P=(L+R)/2
      ISEQP=ISEQ(P)
      VALUESP=VALUES(ISEQP)
 
      IF (VALUES(ISEQ(L)) .GT. VALUESP) THEN
         ISEQ(P)=ISEQ(L)
         ISEQ(L)=ISEQP
         ISEQP=ISEQ(P)
         VALUESP=VALUES(ISEQP)
      ENDIF
 
      IF (VALUESP .GT. VALUES(ISEQ(R))) THEN
         IF (VALUES(ISEQ(L)) .GT. VALUES(ISEQ(R))) THEN
            ISEQ(P)=ISEQ(L)
            ISEQ(L)=ISEQ(R)
         ELSE
            ISEQ(P)=ISEQ(R)
         ENDIF
         ISEQ(R)=ISEQP
         ISEQP=ISEQ(P)
         VALUESP=VALUES(ISEQP)
      ENDIF
 
!     Now we swap values between the right and left sides and/or
!     move VALUESP until all smaller values are on the left and all
!     larger values are on the right.  Neither the left or right
!     side will be internally ordered yet; however, VALUESP will be
!     in its final position.
 
  300 CONTINUE
 
! Q3: Search for datum on left >= VALUESP
!
!     At this point, VALUES[L] <= VALUESP.  We can therefore start scanning
!     up from L, looking for a value >= VALUESP (this scan is guaranteed
!     to terminate since we initially placed VALUESP near the middle of
!     the subsequence).
 
         I=I+1
         IF (VALUES(ISEQ(I)).LT.VALUESP) GOTO 300
 
  400 CONTINUE
 
! Q4: Search for datum on right <= VALUESP
!
!     At this point, VALUES[R] >= VALUESP.  We can therefore start scanning
!     down from R, looking for a value <= VALUESP (this scan is guaranteed
!     to terminate since we initially placed VALUESP near the middle of
!     the subsequence).
 
         J=J-1
         IF (VALUES(ISEQ(J)).GT.VALUESP) GOTO 400
 
! Q5: Have the two scans collided?
 
      IF (I.LT.J) THEN
 
! Q6: No, interchange VALUES[I] <--> VALUES[J] and continue
 
         ISEQT=ISEQ(I)
         ISEQ(I)=ISEQ(J)
         ISEQ(J)=ISEQT
         GOTO 300
      ELSE
 
! Q7: Yes, select next subsequence to sort
!
!     At this point, I >= J and VALUES[l] <= VALUES[I] == VALUESP <= VALUES[r],
!     for all L <= l < I and J < r <= R.  If both subsequences are
!     more than M elements long, push the longer one on the stack and
!     go back to QuickSort the shorter; if only one is more than M
!     elements long, go back and QuickSort it; otherwise, pop a
!     subsequence off the stack and QuickSort it.
 
         IF (R-J .GE. I-L .AND. I-L .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=J+1
            RSTK(ISTK)=R
            R=I-1
         ELSE IF (I-L .GT. R-J .AND. R-J .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=L
            RSTK(ISTK)=I-1
            L=J+1
         ELSE IF (R-J .GT. M) THEN
            L=J+1
         ELSE IF (I-L .GT. M) THEN
            R=I-1
         ELSE
! Q8: Pop the stack, or terminate QuickSort if empty
            IF (ISTK.LT.1) GOTO 900
            L=LSTK(ISTK)
            R=RSTK(ISTK)
            ISTK=ISTK-1
         ENDIF
         GOTO 200
      ENDIF
 
  900 CONTINUE
 
!===================================================================
!
! Q9: Straight Insertion sort
 
      DO 950 I=2,N
         IF (VALUES(ISEQ(I-1)) .GT. VALUES(ISEQ(I))) THEN
            ISEQP=ISEQ(I)
            VALUESP=VALUES(ISEQP)
            P=I-1
  920       CONTINUE
               ISEQ(P+1) = ISEQ(P)
               P=P-1
               IF (P.GT.0) THEN
                  IF (VALUES(ISEQ(P)).GT.VALUESP) GOTO 920
               ENDIF
            ISEQ(P+1) = ISEQP
         ENDIF
  950    CONTINUE
 
!===================================================================
!
!     All done
      return
      END

