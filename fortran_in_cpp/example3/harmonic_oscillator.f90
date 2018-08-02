MODULE constants
  INTEGER,  PARAMETER :: dp = KIND(1.0D0)
  INTEGER, PARAMETER :: dpc = KIND((1.0D0,1.0D0))
  ! min and max isospin projection
  INTEGER, PUBLIC :: itzmin, itzmax
  ! min and max total two-body angular momentum in lab frame
  INTEGER, PUBLIC :: j_lab_min, j_lab_max
  ! min and max total two-body angular momentum in Rel-CoM frame
  INTEGER, PUBLIC :: jmin, jmax
  INTEGER, PUBLIC :: occ_protons, occ_neutrons
  real*8, public :: com_beta,com_switch, j2_beta
  REAL*8, public :: hcom_value, e0,total_energy, eccsd
  INTEGER, PUBLIC :: mass_nucleus, tot_orbs, below_ef, above_ef
  REAL(DP), PUBLIC ::  oscl, hbar_omega
  REAL(DP) , PARAMETER, PUBLIC :: p_mass =938.926_dp
  REAL(DP), PARAMETER, PUBLIC :: theta_rot = 0.0_dp! 0.125_dp
  REAL(DP), PARAMETER, PUBLIC :: hbarc = 197.326968_dp
  REAL(DP), PARAMETER, PUBLIC :: hb2ip = hbarc*hbarc/p_mass
  REAL(DP), PUBLIC, PARAMETER :: pi = 3.141592741012573_dp
  REAL(DP), PUBLIC, PARAMETER :: pi_2 = 1.570796370506287_dp
  REAL(DP), PUBLIC, PARAMETER :: pi_4 = 0.7853981852531433_dp
  LOGICAL :: switch_density
  
END MODULE constants


MODULE single_particle_orbits
  TYPE, PUBLIC :: single_particle_descript
     INTEGER :: total_orbits
     INTEGER, DIMENSION(:), POINTER :: nn, ll, jj, itzp, nshell
     CHARACTER (LEN=10), DIMENSION(:), POINTER :: orbit_status, orb_type, model_space
     REAL*8, DIMENSION(:), POINTER :: e
  END TYPE single_particle_descript
  TYPE (single_particle_descript), PUBLIC :: all_orbit, neutron_data, &
       proton_data
  
CONTAINS
  SUBROUTINE allocate_sp_array(this_array,n)
    TYPE (single_particle_descript), INTENT(INOUT) :: this_array
    INTEGER , INTENT(IN) :: n
    IF (ASSOCIATED (this_array%nn) ) DEALLOCATE(this_array%nn)
    ALLOCATE(this_array%nn(n))
    IF (ASSOCIATED (this_array%ll) ) DEALLOCATE(this_array%ll)
    ALLOCATE(this_array%ll(n))
    IF (ASSOCIATED (this_array%jj) ) DEALLOCATE(this_array%jj)
    ALLOCATE(this_array%jj(n))
    IF (ASSOCIATED (this_array%itzp) ) DEALLOCATE(this_array%itzp)
    ALLOCATE(this_array%itzp(n))
    IF (ASSOCIATED (this_array%e) ) DEALLOCATE(this_array%e)
    ALLOCATE(this_array%e(n))
    IF (ASSOCIATED (this_array%nshell) ) DEALLOCATE(this_array%nshell)
    ALLOCATE(this_array%nshell(n))
    IF (ASSOCIATED (this_array%orbit_status) ) DEALLOCATE(this_array%orbit_status)
    ALLOCATE(this_array%orbit_status(n))
    IF (ASSOCIATED (this_array%model_space) ) DEALLOCATE(this_array%model_space)
    ALLOCATE(this_array%model_space(n))
    !           blank all characters and zero all other values
    DO i= 1, n
       this_array%model_space(i)= ' '
       this_array%orbit_status(i)= ' '
       this_array%e(i)=0.
       this_array%nn(i)=0
       this_array%ll(i)=0
       this_array%jj(i)=0
       this_array%nshell(i)=0
       this_array%itzp(i)=0
    ENDDO

  END SUBROUTINE allocate_sp_array

  SUBROUTINE deallocate_sp_array(this_array)
    TYPE (single_particle_descript), INTENT(INOUT) :: this_array
    DEALLOCATE(this_array%nn) ; DEALLOCATE(this_array%ll)
    DEALLOCATE(this_array%jj) ;DEALLOCATE(this_array%itzp)
    DEALLOCATE(this_array%e) ;DEALLOCATE(this_array%nshell)
    DEALLOCATE(this_array%orbit_status); DEALLOCATE(this_array%model_space)
  END SUBROUTINE deallocate_sp_array
END MODULE single_particle_orbits



!
!            
!     This module contains the angular momentun functions
!     and transformation coefficients when going from 
!     lab system  <--> cm system
!
MODULE ang_mom_functions
  REAL*8, PRIVATE :: f_mb(50),g_mb(50),w_mb(50)
  INTEGER, PRIVATE :: kh(200)
  REAL*8, PARAMETER, PRIVATE :: pi=3.141592654
  REAL*8, PRIVATE :: q(50,50), cn(0:51,0:51)

CONTAINS
  !
  !     factorials for 3j,6j and 9j symbols             
  !     for moshinsky trans brackets and for            
  !     vector brackets                                 
  !
  SUBROUTINE commons_to_angmom
    IMPLICIT NONE 
    INTEGER :: l, k, i, j
    REAL*8 :: a , sq_pi, fj, tfj, fk
    !    3j, 6j and 9j symbols
    kh=1
    kh(100) =0
    DO l=1,50
       q(l,1)=1.0d0
       q(l,l)=1.0d0
       kh(l+l+100)=0
    ENDDO
    DO l=2,49
       DO k=2,l
          q(l+1,k)=q(l,k-1)+q(l,k)
       ENDDO
    ENDDO
    !    Moshinsky brackets
    f_mb(1)=0.
    g_mb(1)=LOG(0.5D0)
    w_mb(1)=0.
    DO i=2,50
       a=i-1
       f_mb(i)=f_mb(i-1)+LOG(a)
       g_mb(i)=g_mb(i-1)+LOG(a+0.5D0)
       w_mb(i)=LOG(a+a+1.)
    ENDDO
    !    spherical harmonics
    cn=0.
    sq_pi=1./SQRT(2.*pi)
    DO j=0,51
       cn(0,j)=SQRT(0.5*(2.*j+1.))
    ENDDO
    DO j=1,51
       tfj=2.*j
       cn(j,j)=cn(j-1,j-1)*SQRT((tfj+1.)/tfj)
    ENDDO
    DO j=0,51
       fj=FLOAT(j)
       DO k=1,j-1
          fk=FLOAT(k)
          cn(k,j)=cn(k-1,j)*SQRT((fj+fk)*(fj-fk+1.))*0.5/fk
       ENDDO
    ENDDO
    cn=cn*sq_pi

  END SUBROUTINE commons_to_angmom
  
  ! Computes the Clebsch Gordan Coefficients
      real(8) FUNCTION CG(L1,M1,L2,M2,L,M)
      IMPLICIT real(8)(A-H,O-Z)
!
!     CLEBSCH-GORDAN COEFFICIENTS
!     N.B. L1,M1,L2,M2,L,M ARE DOUBLED
!
      CG=0.D0
      IF(M1+M2.NE.M) RETURN
      IF(L.LT.IABS(L1-L2).OR.L.GT.(L1+L2)) RETURN
      IF(IABS(M1).GT.L1) RETURN
      IF(IABS(M2).GT.L2) RETURN
      IF(IABS(M).GT.L) RETURN
!
      LL=(L1-L2+M)/2
      SIGN=dfloat((-1)**LL)
      CG=SIGN*dsqrt(dfloat(L+1))*tjs(L1,L2,L,M1,M2,-M)
      RETURN
      END
  
  
  !
  !     calculates 3j-symbols            
  !
  REAL*8 FUNCTION tjs(j_a,j_b,j_c,m_a,m_b,m_c)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: j_a,j_b,j_c,m_a,m_b,m_c
    INTEGER :: ja, jb, jc, mb, ma, mc, la, lb, lc, lt, ld, ja2, jb2, &
         jc2, i, k0, k1, k, ip
    REAL*8 :: x, fn, p

    tjs=0.
    ja=(j_a+m_a)/2+1
    ma=(j_a-m_a)/2+1
    jb=(j_b+m_b)/2+1
    mb=(j_b-m_b)/2+1
    jc=(j_c+m_c)/2+1
    mc=(j_c-m_c)/2+1
    la=(j_b+j_c-j_a)/2+1
    lb=(j_c+j_a-j_b)/2+1
    lc=(j_a+j_b-j_c)/2+1
    lt=(j_a+j_b+j_c)/2+1
    ld=MIN(ja,jb,jc,ma,mb,mc,la,lb,lc)
    IF(((m_a+m_b+m_c) <= 0).AND.(ld > 0)) THEN
       ja2=j_a+m_a
       jb2=j_b+m_b
       jc2=j_c+m_c
       i=ja2+jb2+jc2-ja2/2*2-jb2/2*2-jc2/2*2
       IF(i == 0) then 
          fn=q(ja+ma-1,lc)*q(jb+mb-1,lc)/(q(lt,jc+mc-1)*q(lt+1,2) &
               *q(ja+ma-1,ja)*q(jb+mb-1,jb)*q(jc+mc-1,jc))
          k0=MAX(0,lc-ja,lc-mb)+1
          k1=MIN(lc,ma,jb)
          x=0.
          DO k=k0,k1
             x=-x-q(lc,k)*q(lb,ma-k+1)*q(la,jb-k+1)
          ENDDO
          ip=k1+lb+jc
          p=1-2*(ip-ip/2*2)
          tjs=p*x*SQRT(fn)
       ENDIF
    ENDIF

  END FUNCTION tjs
  !
  !     calculates 6j-symbols            
  !
  REAL*8 FUNCTION sjs(j_a,j_b,j_c,l_a,l_b,l_c)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: j_a,j_b,j_c,l_a,l_b,l_c
    INTEGER :: ja,jb,jc,la,lb,lc,i,mt,ma,mb,mc,na,nb,nc,ka,&
         kb,kc,l,l0,l1
    REAL*8 :: x, fs, fss

    sjs=0.0d0
    ja=j_a + 1
    jb=j_b + 1
    jc=j_c + 1
    la=l_a + 1
    lb=l_b + 1
    lc=l_c + 1
    i=kh(ja+jb-jc+99)+kh(jb+jc-ja+99)+kh(jc+ja-jb+99)+kh(ja+lb-lc+99) &
         +kh(lb+lc-ja+99)+kh(lc+ja-lb+99)+kh(la+jb-lc+99)+kh(jb+lc-la+99) &
         +kh(lc+la-jb+99)+kh(la+lb-jc+99)+kh(lb+jc-la+99)+kh(jc+la-lb+99)
    IF(i <= 0) THEN
       mt=(j_a+j_b+j_c)/2 + 2
       ma=(j_a+l_b+l_c)/2+ 2
       mb=(l_a+j_b+l_c)/2+ 2
       mc=(l_a+l_b+j_c)/2+ 2
       na=mt-ja
       nb=mt-jb
       nc=mt-jc
       ka=ma-lc
       kb=mb-lc
       kc=mc-jc
       fss=q(mt,ja+1)*q(ja,nc)/(q(ma,ja+1)*q(ja,ka)*q(mb,la+1)* &
            q(la,kb)*q(mc,la+1)*q(la,kc))
       fs=SQRT(fss)/(l_a + 1.)
       l0=MAX(mt,ma,mb,mc)+1
       l1=MIN(ma+na,mb+nb,mc+nc)
       x=0.
       DO l=l0,l1
          x=-x+q(l-1,mt)*q(na,l-ma)*q(nb,l-mb)*q(nc,l-mc)
       ENDDO
       sjs=-(1+2*(l1/2*2-l1))*fs*x
    ENDIF

  END FUNCTION sjs
  !
  !     calculates ninej-symbols
  !       
  REAL*8 FUNCTION snj (ia,ib,ie,ic,id,if,ig,ih,it)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ia,ib,ie,ic,id,if,ig,ih,it
    INTEGER :: ja,jb,je,jc,jd,jf,jg,jh,jt,i,la,ld,ma,mc,na,nb,le,lf,&
         lg,me,mf,mg,ne,nf,ng,lx,mx,nx,jsi,jsf, js,is,lb, lc, &
         mb, ly, my,ny,l,l0,m0,n0,l1,m1,n1,m,n,ihx
    REAL*8 :: x, fn, fd, ps, fs, u, y, z, ud, p

    snj=0.
    ja=ia+1
    jb=ib+1
    jc=ic+1
    jd=id+1
    je=ie+1
    jf=IF+1
    jg=ig+1
    jh=ih+1
    jt=it+1
    i=kh(ja+jb-je+99)+kh(jb+je-ja+99)+kh(je+ja-jb+99)+kh(jc+jd-jf+99) &
         +kh(jd+jf-jc+99)+kh(jf+jc-jd+99)+kh(jg+jh-jt+99)+kh(jh+jt-jg+99) &
         +kh(jt+jg-jh+99)+kh(ja+jc-jg+99)+kh(jc+jg-ja+99)+kh(jg+ja-jc+99) &
         +kh(jb+jd-jh+99)+kh(jd+jh-jb+99)+kh(jh+jb-jd+99)+kh(je+jf-jt+99) &
         +kh(jf+jt-je+99)+kh(jt+je-jf+99)
    IF(i <= 0) THEN
       la=(ie+IF+it)/2+2
       ld=(ig+ih+it)/2+2
       ma=(ia+ic+ig)/2+2
       mc=(IF+ic+id)/2+2
       na=(ib+id+ih)/2+2
       nb=(ib+ie+ia)/2+2
       le=(ie+IF-it)/2+1
       lf=(IF+it-ie)/2+1
       lg=(it+ie-IF)/2+1
       me=(ia+ic-ig)/2+1
       mf=(ic+ig-ia)/2+1
       mg=(ig+ia-ic)/2+1
       ne=(ib+id-ih)/2+1
       nf=(id+ih-ib)/2+1
       ng=(ih+ib-id)/2+1
       lx=(it+ig-ih)/2+1
       mx=(ic+id-IF)/2+1
       nx=(ib+ie-ia)/2+1
       fn=q(la,jt+1)*q(jt,lg)*q(ma,jc+1)*q(jc,mf)*q(na,jb+1)*q(jb,ne)
       fd=q(ld,jt+1)*q(jt,lx)*q(mc,jc+1)*q(jc,mx)*q(nb,jb+1)*q(jb,nx)
       jsi=MAX(ABS(je-jh),ABS(jg-jf),ABS(ja-jd))+1
       jsf=MIN(je+jh,jg+jf,ja+jd)-1
       ps=-1-2*(jsi/2*2-jsi)
       fs=ps*SQRT(fn/fd)/FLOAT((ig+1)*(ie+1))
       u=0.
       DO js=jsi,jsf,2
          is=js-1
          lb=(ie+ih+is)/2+2
          lc=(ig+IF+is)/2+2
          mb=(ia+id+is)/2+2
          ly=(ie+ih-is)/2+1
          my=(ig+IF-is)/2+1
          ny=(ia-id+is)/2+1
          ud=q(lb,je+1)*q(je,ly)*q(lc,jg+1)*q(jg,my)*q(mb,js+1)*q(js,ny)
          l0=MAX(la,lb,lc,ld)+1
          m0=MAX(ma,mb,mc,lc)+1
          n0=MAX(na,nb,mb,lb)+1
          l1=MIN(le+ld,lf+lb,lg+lc)
          m1=MIN(me+lc,mf+mb,mg+mc)
          n1=MIN(ne+lb,nf+nb,ng+mb)
          x=0.
          DO l=l0,l1
             x=-x-q(l-1,la)*q(le,l-ld)*q(lf,l-lb)*q(lg,l-lc)
          ENDDO
          y=0.
          DO m=m0,m1
             y=-y-q(m-1,ma)*q(me,m-lc)*q(mf,m-mb)*q(mg,m-mc)
          ENDDO
          z=0.
          DO n=n0,n1
             z=-z-q(n-1,na)*q(ne,n-lb)*q(nf,n-nb)*q(ng,n-mb)
          ENDDO
          ihx=l1+m1+n1
          p=1+2*(ihx/2*2-ihx)
          u=u+p*x*y*z/ud
       ENDDO
       snj=u*fs
    ENDIF

  END FUNCTION snj

  !
  !     This routine calculates the moshinsky vector bracket       
  !     Note that D=mass1/mass2                                    
  !     Ref  m.sotona and m.gmitro  comp.phys.comm 3(1972)53       
  !
  REAL*8 FUNCTION gmosh &
       (n,l,nc,lc,n1,l1,n2,l2,lr,d)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n,l,nc,lc,n1,l1,n2,l2,lr
    REAL*8, INTENT(IN) :: d 
    INTEGER :: ip,ixf,ix, iyi, iyf, j1f,j2,k1i,k1f,m1f,iy,m2f,k2, &
         m2,m2i,m1,j1,k2f,k2i,k1
    REAL*8 :: dl, d1l, bb, ba, anorm, y, p, bc, cfac, bm , &
         sm, s, sxy, bxy

    gmosh=0.
    IF(n+n+nc+nc+l+lc-n1-n1-n2-n2-l1-l2 /= 0 ) RETURN
    IF(l+lc-lr < 0 ) RETURN
    IF(l1+l2-lr < 0 ) RETURN
    IF(ABS(l-lc)-lr > 0 ) RETURN
    IF(ABS(l1-l2)-lr > 0 ) RETURN
    DL=LOG(D)
    D1L=LOG(D+1.)
    bb=f_mb(n1+1)+f_mb(n2+1)+f_mb(n+1)-f_mb(nc+1)+ &
         g_mb(n1+l1+1)+g_mb(n2+l2+1) &
         -g_mb(n+l+1)-g_mb(nc+lc+1) 
    ba=w_mb(l1+1)+w_mb(l2+1)+w_mb(lc+1)+w_mb(l+1)+ &
         f_mb(l1+l2-lr+1)+f_mb(l+lc+lr+2) &
         +f_mb(l+lc-lr+1)+f_mb(lc+lr-l+1)+ &
         f_mb(lr+l-lc+1)-f_mb(l1+l2+lr+2) &
         -f_mb(l1+lr-l2+1)-f_mb(l2+lr-l1+1)-DBLE(l)*d1l
    ip=lr+n+n1+n2
    p=1+2*(ip/2*2-ip)
    anorm=p*EXP(0.5D0*(bb+ba))
    y=0.
    j1f=l+1
    DO j1=1,j1f
       j2=l+2-j1
       k1i=ABS(l1-j1+1)+1
       k1f=l1+j1
       DO k1=k1i,k1f,2
          m1f=n1-(j1+k1-l1)/2+2
          IF(m1f-1 < 0 )  CYCLE
          k2i=MAX(ABS(l2-j2+1),ABS(lc-k1+1))+1
          k2f=MIN(l2+j2,lc+k1)
          IF(k2i-k2f > 0 ) CYCLE
          DO k2=k2i,k2f,2
             m2f=n2-(j2+k2-l2)/2+2
             IF(m2f-1 < 0 )  CYCLE
             ip=j2-1+(l1+k1+j1+l2+k2+j2)/2
             p=1+2*(ip/2*2-ip)
             bc=0.5D0*(DBLE(k1+j2-2)*dl-DBLE(k1+k2-2)*d1l) &
                  +f_mb(k1+l1-j1+1)+f_mb(k1+k2-lc-1)+ &
                  f_mb(k2+l2-j2+1)-f_mb(k1+l1+j1)-f_mb(k1+k2+lc)- &
                  f_mb(k2+l2+j2)+w_mb(k1)+w_mb(k2)+f_mb((k1+l1+j1)/2)+ &
                  f_mb((k1+k2+lc)/2)+f_mb((k2+l2+j2)/2)- &
                  f_mb((k1+l1-j1)/2+1)-f_mb((l1+j1-k1)/2+1)- &
                  f_mb((j1+k1-l1)/2)-f_mb((k1+k2-lc)/2)- &
                  f_mb((k2+lc-k1)/2+1)-f_mb((lc+k1-k2)/2+1) &
                  -f_mb((k2+l2-j2)/2+1)-f_mb((l2+j2-k2)/2+1)- &
                  f_mb((j2+k2-l2)/2)
             cfac=p*EXP(bc)
             sxy=0.
             ixf=MIN(k1+k1,k1+k2-lc)-1
             DO ix=1,ixf
                iyi=MAX(1,ix+j1+l2-k1-lr)
                iyf=MIN(l2+l2+1,l1+l2-lr+1,l2+lc+ix-k1-j2+2)
                IF(iyi-iyf > 0 ) CYCLE
                DO iy=iyi,iyf
                   ip=ix+iy
                   p=1+2*(ip/2*2-ip)
                   bxy=f_mb(k1+k1-ix)+f_mb(l2+l2-iy+2)+ &
                        f_mb(k2+lc-k1+ix)+f_mb(l1+lr-l2+iy) &
                        -f_mb(ix)-f_mb(iy)-f_mb(k1+k2-lc-ix)- &
                        f_mb(l1+l2-lr-iy+2)-f_mb(k1-l2+lr-j1+iy-ix+1)- &
                        f_mb(l2-k1+lc-j2+ix-iy+3)
                   sxy=sxy+p*EXP(bxy)
                ENDDO
             ENDDO
             s=cfac*sxy
             sm=0.
             DO m1=1,m1f
                m2i=MAX(1,nc-m1-(k1+k2-lc)/2+3)
                IF(m2i-m2f > 0 ) CYCLE
                DO m2=m2i,m2f
                   ip=m1+m2
                   p=1+2*(ip/2*2-ip)
                   bm=DBLE(m1-1)*DL-DBLE(m1+m2-2)*d1l+g_mb(1) &
                        +g_mb(m1+m2+(k1+k2+lc)/2-2)-g_mb(k1+m1-1)- &
                        g_mb(k2+m2-1)+f_mb(m1+m2+(k1+k2-lc)/2-2)- &
                        f_mb(m1)-f_mb(m2)-f_mb(n1-m1-(j1+k1-l1)/2+3)- &
                        f_mb(n2-m2-(j2+k2-l2)/2+3) &
                        -f_mb(m1+m2-nc+(k1+k2-lc)/2-2)
                   sm=sm+p*EXP(bm)
                ENDDO
             ENDDO
             y=y+s*sm
          ENDDO
       ENDDO
    ENDDO
    gmosh=anorm*y

  END FUNCTION  gmosh


  ! parity function

  REAL*8 FUNCTION parity(m)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: m
     parity= -1.0
     if(mod(m,2) == 0)parity=1.0
   END FUNCTION parity

  !  Spherical harmonics from Num. Recipes  

  REAL*8 FUNCTION spherical_harmonics(m1,l,x)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: m1, l
    REAL*8, INTENT(IN) ::  x
    REAL*8, DIMENSION(0:51) :: y
    INTEGER :: iphase, m, j
    REAL*8 :: fj, z, fac, div, sum, a, b, c
    spherical_harmonics=0.
    m=IABS(m1)
    IF(m.LT.0) m=-m1
    y(0)=1.
    IF(l.EQ.0) THEN
       sum=y(0)
    ELSE
       a=m-l
       b=l+m+1
       c=m+1
       z=0.5-x*0.5
       DO j=1,l-m+1
          fj=j-1
          y(j)=y(j-1)*(a+fj)*(b+fj)*z
          div=(c+fj)*(fj+1.)
          y(j)=y(j)/div
       ENDDO
       IF(m > 0) then
          fac=(1.-x*x)**m
          fac=SQRT(fac)
       ELSE
          fac=1.
       ENDIF
       sum=0.
       DO j=0,l-m
          sum=sum+y(j)
       ENDDO
       iphase=m
       IF(m1.LT.0) then
          iphase=0
       ENDIF
       sum=sum*fac*((-1)**iphase)
    ENDIF
    spherical_harmonics=cn(m,l)*sum

  END FUNCTION spherical_harmonics

END MODULE ang_mom_functions


!     Modules specific to the g-matrix calculation and effective operators
!     only
!     arrays containing mesh points and harmonic oscillator wave functions
!     In addition, routines for various wave functions are also included

MODULE wave_functions

  use constants
      
  INTEGER , PUBLIC :: n_k1, n_k, n_k2
  DOUBLE PRECISION, ALLOCATABLE, PUBLIC :: ra(:),wra(:), krel(:), wkrel(:)
  DOUBLE PRECISION, PUBLIC :: k_cutoff, k_max
  DOUBLE PRECISION, ALLOCATABLE, PUBLIC :: rnlr(:,:,:)
CONTAINS



  
  SUBROUTINE gauss_laguerre(n,x,w,alf)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    INTEGER :: i, its, j
    REAL(dp), INTENT(IN) :: alf 
    REAL(dp), DIMENSION(n), INTENT(INOUT) :: x,w
    REAL(dp), PARAMETER :: eps=3.E-13
    INTEGER, PARAMETER :: maxit=50
    REAL(KIND = 8) :: p1,p2,p3,pp,z,z1, ai

    DO i=1, n
       IF  (i == 1) THEN                         
          z=(1.0D0+alf)*(3.0D0+0.92D0*alf)/(1.0D0+2.4D0*n+1.8D0*alf)
       ELSEIF (i == 2) THEN
          z = z+(15.0D0+6.25D0*alf)/(1.0D0+0.9D0*alf+2.5D0*n)
       ELSE                               
          ai=i-2;
          z = z+((1.0D0+2.55D0*ai)/(1.9D0*ai)+1.26D0*ai*  &
               alf/(1.0D0+3.5D0*ai))*(z-x(i-2))/(1.0D0+0.3D0*alf)
       ENDIF
       DO its=1, MAXIT
          p1=1.0D0;
          p2=0.0D0;
          DO j=1, n
             p3=p2                               
             p2=p1
             p1=((2*j-1+alf-z)*p2-(j-1+alf)*p3)/j
          ENDDO
          pp=(n*p1-(n+alf)*p2)/z
          z1=z
          z=z1-p1/pp                          
          IF (ABS(z-z1) <= EPS) GOTO 10
       ENDDO
10     CONTINUE
       IF (its > MAXIT) THEN
          WRITE(6,*) 'Too many iterations in gauss-Laguerre'
          STOP 
       ENDIF
       x(i)=z                             
       w(i) = -EXP(gammln(alf+n)-gammln(n*1.D0))/(pp*n*p2)
    ENDDO
  END SUBROUTINE gauss_laguerre

  DOUBLE PRECISION FUNCTION gammln(xx)
    IMPLICIT NONE
    INTEGER :: j
    REAL (dp), INTENT(IN) :: xx
    REAL (dp)  cof(6),stp,half,one,fpf,x,tmp,ser
    DATA cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0, &
         -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
    DATA half,one,fpf/0.5d0,1.0d0,5.5d0/
    x=xx-one
    tmp=x+fpf
    tmp=(x+half)*LOG(tmp)-tmp
    ser=one
    DO j=1,6
       x=x+one
       ser=ser+cof(j)/x
    ENDDO
    gammln=tmp+LOG(stp*ser)

  END FUNCTION gammln

  DOUBLE PRECISION FUNCTION factrl(n)
    IMPLICIT NONE
    REAL(dp), DIMENSION(33) :: a
    INTEGER, INTENT(IN)  :: n
    INTEGER :: j, ntop
    DATA ntop,a(1)/0,1./

    IF (n <= ntop) THEN
       factrl=a(n+1)
    ELSEIF (n <= 32) THEN
       DO j=ntop+1,n
          a(j+1)=j*a(j)
       ENDDO
       ntop=n
       factrl=a(n+1)
    ELSE
       factrl=exp(gammln(n+1.D0))
    ENDIF

  END  FUNCTION factrl

  SUBROUTINE laguerre_general( n, alpha, x, cx )
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: n
    REAL (dp ) ::  alpha
    REAL ( dp ) :: cx(0:n)
    !complex ( dpc ) :: cx(0:n)
    INTEGER :: i
    REAL*16, INTENT(IN) ::  x
    !complex ( dpc ), INTENT(IN) ::  x

    IF ( alpha <= -1.0D+00 ) THEN
       WRITE ( *, '(a)' ) ' '
       WRITE ( *, '(a)' ) 'LAGUERRE_GENERAL - Fatal error!'
       WRITE ( *, '(a,g14.6)' ) '  The input value of ALPHA is ', alpha
       WRITE ( *, '(a)' ) '  but ALPHA must be greater than -1.'
       STOP
    END IF
    IF ( n < 0 ) THEN
       RETURN
    END IF
    cx(0) = 1.0D+00
    IF ( n == 0 ) THEN
       RETURN
    END IF
    cx(1) = 1.0D+00 + alpha - x
    DO i = 2, n
       cx(i) = ( ( REAL ( 2 * i - 1, kind = 8 ) + alpha - x ) * cx(i-1)   &
            + ( REAL (   - i + 1, kind = 8 ) - alpha     ) * cx(i-2) ) &
            / REAL (     i,     kind = 8 )
    END DO

  END SUBROUTINE laguerre_general

  DOUBLE PRECISION FUNCTION  fac(m)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: m
    INTEGER :: i

    fac = 0.0D0
    IF(m == 0) return
    DO i=1,m
       fac=fac+LOG(FLOAT(i))
    ENDDO

  END FUNCTION  fac

  DOUBLE PRECISION FUNCTION  dfac(m)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: m
    INTEGER :: i

    IF (MOD(m,2).ne.1) stop 'wrong argument to dfac'
    dfac = 0.0D0
    IF (m == 1)return
    DO i=3,m,2
       dfac=dfac+LOG(FLOAT(i))
    ENDDO
  END FUNCTION  dfac



  !
  !     H.O. functions using Kummers function   
  !
  REAL*16 FUNCTION rnl(n,l,z)
    IMPLICIT NONE
    INTEGER :: lll, nn
    INTEGER, INTENT(IN) :: l, n
    REAL*16 :: y, dl, gamfaa, pi, dfll, gamfab, dfnn
    REAL*16, INTENT(IN) :: z

    rnl=0. ; y=0.5*z*z
    IF(y > 60.0) RETURN
    dl = l
    IF((ABS(z) < 1.0d-6) .AND. (l == 0)) rnl = 1.0d0
    IF( ABS(z) > 1.0d-6) rnl = (z**l) * EXP(-y) * hypkum(n,dl+1.5,z*z)
    pi = 3.1415926535897932
    gamfaa = 0.5 * SQRT(pi)
    IF(l /= 0) THEN
       DO lll = 1, l
          dfll = lll - 1
          gamfaa = gamfaa * (dfll + 1.5)
       ENDDO
    ENDIF
    gamfab = gamfaa
    IF(n /= 0) THEN
       dfll = dl + 0.5
       DO nn = 1, n
          dfnn = nn
          gamfab = gamfab * ((dfnn + dfll) / dfnn)
       ENDDO
    ENDIF
    rnl = rnl * (SQRT(2.0 * gamfab) / gamfaa)

  END FUNCTION rnl
  !
  !     Kummers function, Abramowitz & Stegun   
  !     exp. 13.1.2. a(there) equals (-n)       
  !  
  REAL*16 FUNCTION hypkum(n,b,z)
    IMPLICIT NONE
    INTEGER :: nmax, nf
    INTEGER, INTENT(IN)  :: n
    REAL*16 :: af, bf, zf, term, dfnf, xadd, sum
    REAL*16, INTENT(IN) :: b, z

    IF(n < 0) WRITE (6,*)' error exit in hypkum ',  n,b,z
    hypkum = 1.0
    IF(n == 0) RETURN
    nmax = n ; af = - n ; bf = b ; zf = z ; sum = 1.0 ; term = 1.0
    DO nf = 1, nmax
       dfnf = nf
       xadd = dfnf - 1.0
       term = term * ((af + xadd) / (bf + xadd)) * (zf / dfnf)
       IF(ABS(term) <  1.0d-12) EXIT
       sum = sum + term
    ENDDO
    hypkum = sum

  END FUNCTION hypkum


  !  This function sets up the recursive relation
  !  for the associated Legendre polynomials

  DOUBLE PRECISION FUNCTION legendre_polynomials(l, m, x)
    IMPLICIT NONE
    DOUBLE PRECISION ::  fact,pll,pmm,pmmp1,somx2
    DOUBLE PRECISION, INTENT(IN)  :: x
    INTEGER ::  i,ll
    INTEGER, INTENT(IN) :: l, m

    !  check whether m, l and x are ok

    IF((M < 0).OR.(M > L).OR.(ABS(X) > 1.)) THEN
       WRITE(6,*) 'bad arguments', m, l, x; RETURN
    ENDIF

    !  calculate now pmm as starting point for iterations

    pmm=1.0
    IF (m > 0) THEN
       somx2=SQRT((1.0-x)*(1.0+x))
       fact=1.0;
       DO i=1, m
          pmm = -fact*somx2*pmm
          fact = fact+2.0
       ENDDO
    ENDIF

    !  if l == m we do not need to use recursion relation

    IF (l == m) THEN
       legendre_polynomials=pmm

       !  recursive relation for associated Legendre polynomials

    ELSE
       pmmp1=x*(2*m+1)*pmm

       !  analytical formula for the case l == m+1

       IF (l == (m+1)) THEN
          legendre_polynomials=pmmp1
       ELSE
          DO ll=m+2, l
             pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
             pmm=pmmp1
             pmmp1=pll
          ENDDO
          legendre_polynomials= pll
       ENDIF
    ENDIF

  END FUNCTION legendre_polynomials

  !
  !
  !      This routine calculates gauss-legendre mesh points and weights      
  !      input:                                                              
  !      x1   : lower limit of the integration interval                      
  !      x2   : upper limit ---------- "" -------------                      
  !      n    : the desired number of mesh points                            
  !      output :                                                            
  !      x     : gauss-legendre mesh points on the interval (x1,x2)          
  !      w     : the corresponding weights                                   
  !      From  : Numerical recipes
  !      F90 version : M. Hjorth-Jensen
  !
  SUBROUTINE gauss_legendre(x1,x2,x,w,n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    INTEGER :: i, j, m
    DOUBLE PRECISION, INTENT(IN) :: x1, x2
    DOUBLE PRECISION, INTENT(INOUT) :: x, w
    DOUBLE PRECISION :: eps
    DIMENSION :: x(n), w(n)
    PARAMETER (eps=3.D-14)
    DOUBLE PRECISION :: p1,p2,p3,pp,xl,xm,z,z1

    m=(n+1)/2
    xm=0.5d0*(x2+x1)
    xl=0.5d0*(x2-x1)
    DO i=1,m
       z1=0.
       z=COS(3.141592654d0*(i-.25d0)/(n+.5d0))
       DO WHILE ( ABS(z-z1) > EPS)
          p1=1.
          p2=0.
          DO j=1,n
             p3=p2
             p2=p1
             p1=((2.*j-1.)*z*p2-(j-1.)*p3)/j
          ENDDO
          pp=n*(z*p1-p2)/(z*z-1.)
          z1=z
          z=z-p1/pp
       ENDDO
       x(i)=xm-xl*z
       x(n+1-i)=xm+xl*z
       w(i)=2.*xl/((1.-z*z)*pp*pp)
       w(n+1-i)=w(i)
    ENDDO

  END SUBROUTINE gauss_legendre

END MODULE wave_functions




!
! this calculates the radial single-particle density
! 
subroutine radial_density_osc(rho)

  
  use single_particle_orbits
  use constants
  USE ang_mom_functions 
  use wave_functions
  
  implicit none
  
  real*8, intent(in)  :: rho(tot_orbs, tot_orbs) 
  integer :: iph, a, na, la,ja,tza,c, nc,lc,jc,tzc, i, nn
  double precision :: ang_fac, sump, sumn, radii_n, radii_p, radii
  double precision :: osc_a, osc_c, vsum, osc_c_deriv, factor
  double precision, allocatable, dimension(:) :: rr ,wrr
  double precision, allocatable :: hol(:,:)
  DOUBLE PRECISION ::  dr, rshift, sum_rel, cx(0:200), xp
  real*8, allocatable :: rhor(:), rhor_n(:), rhor_p(:)
  real*16 :: zlab
  integer :: ph
  character(len=100) :: input1

  
  read(5,*); read(5,*) input1 
  

  
  open(777,file=input1)
  
  
  nn = 600
  allocate( rr(nn), wrr(nn) )
  call gauss_legendre(0.d0, 25.d0, rr, wrr, nn)
  allocate( hol(all_orbit%total_orbits, nn) )
  allocate( rhor(nn) )
  allocate( rhor_n(nn) )
  allocate( rhor_p(nn) )
  rhor_n = 0.d0
  rhor_p = 0.d0

  oscl=hbarc/SQRT(p_mass*hbar_omega)
  write(6,*) oscl
  
  hol = 0.d0
  do a = 1, all_orbit%total_orbits
     na=all_orbit%nn(a) 
     la=all_orbit%ll(a)
     ja=all_orbit%jj(a)
     tza = all_orbit%itzp(a)

     ph = 1.d0 
     factor = 0.5D0*((na+la+2)*LOG(2.D0)+fac(na)-dfac(2*na+2*la+1)-0.5D0*LOG(pi))
     factor = EXP(factor)
     sum_rel=0.
     DO i=1,nn
        
        
        rshift = rr(i) !* (mass_nucleus-1)/mass_nucleus
        dr = wrr(i) !*(mass_nucleus-1)/mass_nucleus
        !zlab= rr(i)/oscl
        zlab = rshift/oscl
        
        
        CALL laguerre_general( na, la+0.5D0, zlab*zlab, cx )
        xp = cx(na)*exp(-zlab*zlab*0.5d0)*(zlab**la)
        hol(a,i) = xp*factor*(1.d0/oscl**(1.5D0))
        sum_rel = sum_rel + hol(a, i)**2 * rshift**2*dr
        
     end DO
     
     write(6,*) 'Norm oscillator functions', sum_rel
  end do
  
  rhor = 0.d0
  do a = 1, all_orbit%total_orbits
     na=all_orbit%nn(a) 
     la=all_orbit%ll(a)
     ja=all_orbit%jj(a)
     tza = all_orbit%itzp(a)
     
     do c = 1, all_orbit%total_orbits
        nc=all_orbit%nn(c) 
        lc=all_orbit%ll(c)
        jc=all_orbit%jj(c)
        tzc = all_orbit%itzp(c)
  
        do i = 1, nn
           rhor(i) = rhor(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
        end do
        
        if ( all_orbit%itzp(a) == -1 .and. all_orbit%itzp(c) == -1 ) then
           do i = 1, nn
              rhor_p(i) = rhor_p(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
           end do
        elseif( all_orbit%itzp(a) == 1 .and. all_orbit%itzp(c) == 1 ) then
           do i = 1, nn
              rhor_n(i) = rhor_n(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
           end do
        end if
           
           
     end do
  end do
  
  
  !
  ! Check radial density
  ! 
  sum_rel=0.
  sumn = 0.d0
  sump = 0.d0
  radii = 0.d0
  radii_p = 0.d0
  radii_n = 0.d0
  DO i=1,nn
     
     
     !
     ! Rescale densities 
     !
     rshift = rr(i)  !* (mass_nucleus-1)/mass_nucleus
     dr = wrr(i)   !*(mass_nucleus-1)/mass_nucleus
          
     sump = sump + rhor_p(i) * rshift**2*dr
     sumn = sumn + rhor_n(i) * rshift**2*dr
     sum_rel = sum_rel + rhor(i) * rshift**2*dr
     write(777,'(4e12.4)') rr(i), rhor(i)/pi/4.d0, rhor_p(i)/pi/4.d0, rhor_n(i)/pi/4.d0
     radii = radii + rhor(i) * rshift**4*dr 
     radii_p = radii_p + rhor_p(i) * rshift**4*dr 
     radii_n = radii_n + rhor_n(i) * rshift**4*dr 

  end DO
  write(6,*) 'Check radial density',  sum_rel
  write(6,*) 'Number of protons and neutrons',  sump, sumn
  write(6,*) 'RMS radii', sqrt( radii/sum_rel )
  write(6,*) 'Proton RMS radii', sqrt( radii_p/sump )
  write(6,*) 'Neutron RMS radii', sqrt( radii_n/sumn )
  
  
end subroutine radial_density_osc

!
! this calculates the momentum space single-particle density
! 
subroutine mom_density_osc(rho)

  
  use single_particle_orbits
  use constants
  USE ang_mom_functions 
  use wave_functions
  
  implicit none
  
  real*8, intent(in)  :: rho(tot_orbs, tot_orbs) 
  integer :: iph, a, na, la,ja,tza,c, nc,lc,jc,tzc, i, nn
  double precision :: ang_fac, sump, sumn, radii_n, radii_p, radii
  double precision :: osc_a, osc_c, vsum, osc_c_deriv, factor
  double precision, allocatable, dimension(:) :: rr ,wrr
  double precision, allocatable :: hol(:,:)
  DOUBLE PRECISION ::  dr, rshift, sum_rel, cx(0:200), xp
  real*8, allocatable :: rhor(:), rhor_n(:), rhor_p(:)
  real*16 :: zlab
  integer :: ph
  character(len=100) :: input1

  
  read(5,*); read(5,*) input1 
  

  
  open(777,file=input1)
  
  
  nn = 600
  allocate( rr(nn), wrr(nn) )
  call gauss_legendre(0.d0, 10.d0, rr, wrr, nn)
  allocate( hol(all_orbit%total_orbits, nn) )
  allocate( rhor(nn) )
  allocate( rhor_n(nn) )
  allocate( rhor_p(nn) )
  rhor_n = 0.d0
  rhor_p = 0.d0
  
  wrr = wrr !*hbarc
  rr = rr   !* hbarc

  oscl=hbarc/SQRT(p_mass*hbar_omega)
  write(6,*) oscl
  
  hol = 0.d0
  do a = 1, all_orbit%total_orbits
     na=all_orbit%nn(a) 
     la=all_orbit%ll(a)
     ja=all_orbit%jj(a)
     tza = all_orbit%itzp(a)

     ph = 1.d0 
     factor = 0.5D0*((na+la+2)*LOG(2.D0)+fac(na)-dfac(2*na+2*la+1)-0.5D0*LOG(pi))
     factor = EXP(factor)
     sum_rel=0.
     DO i=1,nn
        
        
        rshift = rr(i) !* (mass_nucleus-1)/mass_nucleus
        dr = wrr(i) !*(mass_nucleus-1)/mass_nucleus
        !zlab= rr(i)/oscl
        !zlab = rshift/oscl
        zlab = rshift*oscl !/hbarc
        
        CALL laguerre_general( na, la+0.5D0, zlab*zlab, cx )
        xp = cx(na)*exp(-zlab*zlab*0.5d0)*(zlab**la)
        hol(a,i) = (-1.d0)**na*xp*factor*(oscl**(1.5D0)) !/hbarc**(1.5D0)         
        
        sum_rel = sum_rel + hol(a, i)**2 * rshift**2*dr
        
     end DO
     
     write(6,*) 'Norm oscillator functions', sum_rel
  end do
  
  rhor = 0.d0
  do a = 1, all_orbit%total_orbits
     na=all_orbit%nn(a) 
     la=all_orbit%ll(a)
     ja=all_orbit%jj(a)
     tza = all_orbit%itzp(a)
     
     do c = 1, all_orbit%total_orbits
        nc=all_orbit%nn(c) 
        lc=all_orbit%ll(c)
        jc=all_orbit%jj(c)
        tzc = all_orbit%itzp(c)
  
        do i = 1, nn
           rhor(i) = rhor(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
        end do
        
        if ( all_orbit%itzp(a) == -1 .and. all_orbit%itzp(c) == -1 ) then
           do i = 1, nn
              rhor_p(i) = rhor_p(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
           end do
        elseif( all_orbit%itzp(a) == 1 .and. all_orbit%itzp(c) == 1 ) then
           do i = 1, nn
              rhor_n(i) = rhor_n(i) + rho(a,c)*hol(a,i)*hol(c,i) * sqrt( all_orbit%jj(a)+1.d0)* sqrt( all_orbit%jj(c)+1.d0)
           end do
        end if
           
           
     end do
  end do
  
  
  !
  ! Check radial density
  ! 
  sum_rel=0.
  sumn = 0.d0
  sump = 0.d0
  radii = 0.d0
  radii_p = 0.d0
  radii_n = 0.d0
  DO i=1,nn
     
     
     !
     ! Rescale densities 
     !
     rshift = rr(i)  !* (mass_nucleus-1)/mass_nucleus
     dr = wrr(i)   !*(mass_nucleus-1)/mass_nucleus
          
     sump = sump + rhor_p(i) * rshift**2*dr
     sumn = sumn + rhor_n(i) * rshift**2*dr
     sum_rel = sum_rel + rhor(i) * rshift**2*dr
     write(777,'(4e12.4)') rr(i), rhor(i)/pi/4.d0, rhor_p(i)/pi/4.d0, rhor_n(i)/pi/4.d0
     radii = radii + rhor(i) * rshift**4*dr 
     radii_p = radii_p + rhor_p(i) * rshift**4*dr 
     radii_n = radii_n + rhor_n(i) * rshift**4*dr 

  end DO
  write(6,*) 'Check radial density',  sum_rel
  write(6,*) 'Number of protons and neutrons',  sump, sumn
  write(6,*) 'RMS radii', sqrt( radii/sum_rel )
  write(6,*) 'Proton RMS radii', sqrt( radii_p/sump )
  write(6,*) 'Neutron RMS radii', sqrt( radii_n/sumn )
  
  
end subroutine mom_density_osc
