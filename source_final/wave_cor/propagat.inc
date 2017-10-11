subroutine propagat
  implicit none

  integer :: j, k, i, iyyz, ixx, jyy, ixx1, jyy1, iwk, iwk1, i1, jth, jth1
  real :: x0, y0, d0, dddx0, dddy0, duxdx0, duxdy0, duydy0, duydx0, th0
  real :: sinth, costh, wk0, ws0, dk0, cg, cgx, cgy 
  !angx_d, angy_d, , delts, radx_d, rady_d, wecs
  real :: xx, yy, x1, x2, y1, y2, dsidd
  real :: ssr1, ssr2, ssrwk, ssrth, wks, fien, fien1, wk1, wk2, dtth0, ths
  real :: e11, e12, e13, e14, e1, e21, e22, e23, e24, e2
  real :: e32, e33, e34, e3, e41, e42, e43, e44, e4, e31
  real :: th1, th2, exxyy, x_d, y_d
  real :: dx, dy
  integer :: iii
  
  real:: Jmatrix(12,3)
  real:: Kmatrix(25,5)
 ! real:: deltt_p180p

  integer :: Jflag,Kflag

 ! deltt_p180p=p180p * delttm * 60. 
  
  integer :: pre_ixx,pre_jyy,d_pre1,d_pre2,d_pre,tmp,d_index  
 
  pre_ixx=0
  pre_jyy=0
   
!-------------------------------------------------------------------------------
  do 100 ia=ixs,ixl

    do 101 ic=iys,iyl
    
    if(nsp(ia,ic).ne.1) cycle
    x0=x(ia)
    y0=y(ic)
    d0=d(ia,ic)
    dx = deltx(ia)
    dy = delty(ic)
    dddx0 = dddx(ia, ic) ! yinxq
    dddy0 = dddy(ia, ic) ! yinxq
    duxdx0=uxx(ia,ic)
    duxdy0=uxy(ia,ic)
    duydx0=uyx(ia,ic)
    duydy0=uyy(ia,ic)
    do 200 j=1,jl
	
	  if (Jflag==0) then
		  th0=thet(j)
		  sinth=sin(th0)
		  costh=cos(th0)
		  Jmatrix(j,1)=th0
		  Jmatrix(j,2)=sinth
		  Jmatrix(j,3)=costh
	  else	  
	      	  th0=Jmatrix(j,1)
		  sinth=Jmatrix(j,2)
		  costh=Jmatrix(j,3)
	  endif
		  
      do 500 k=1,kl
	    if (Kflag==0) then
			wk0=wk(k)
			ws0=zpi*wf(k,ia,ic)
			dk0=d0*wk0
			cg=ccg(k,ia,ic)
			
			Kmatrix(k,1)=wk0
			Kmatrix(k,2)=ws0
			Kmatrix(k,3)=dk0
			Kmatrix(k,4)=cg
			if (dk0.lt.40.) then
          		dsidd=0.5*g/cosh(dk0)*wk0**2/ws0/cosh(dk0)
        	else
          		dsidd=0.
			endif
			Kmatrix(k,5)=dsidd
			
		else
			wk0=Kmatrix(k,1)
			ws0=Kmatrix(k,2)
			dk0=Kmatrix(k,3)
			cg=Kmatrix(k,4)
			dsidd=Kmatrix(k,5)
		endif

		
				
        cgx=cg*costh
        cgy=cg*sinth
	ssr1=(dsidd*dddx0+wk0*costh*duxdx0+wk0*sinth*duydx0)*p180p
        ssr2=(dsidd*dddy0+wk0*costh*duxdy0+wk0*sinth*duydy0)*p180p
        ssrwk=-(ssr1*costh/rslat(ic)+ssr2*sinth/rs)
        ssrth=(ssr1*sinth/rslat(ic)-ssr2*costh/rs)/wk0
	ssrth=ssrth-cg*costh*tand(y(ic))/rs
	wks=wk0-deltt*ssrwk
        xx=x0-(cgx+ux(ia,ic))/rslat(ic)*deltt_p180p
        yy=y0-(cgy+uy(ia,ic))/rs*deltt_p180p
		dtth0=deltt*ssrth
		if (wks.lt.0.) wks=0.
        if (wks.le.wkmin) then
          iwk=1
          iwk1=1
          fien=0.
          fien1=1.
          wk1=0.
          wk2=wk(iwk1)
        else
          if (wks.lt.wk(kld)) then
            do iyyz=1,kld
              if(wks.ge.wk(iyyz).and.wks.lt.wk(iyyz+1))iwk=iyyz
            enddo
            iwk1=iwk+1
            wk1=wk(iwk)
            wk2=wk(iwk+1)
            if (iwk.lt.kl) then
              iwk1=iwk+1
              fien=1.
              fien1=1.
            else
              i=iwk-kl+1
              i1=i+1
              fien=wkh(i)
              fien1=wkh(i1)
              iwk=kl
              iwk1=kl
            endif
          else
            wks=wk(kld)
            iwk=kl
            iwk1=kl
            i=kld-kl+1
            i1=i+1
            fien=wkh(i)
            fien1=wkh(i1)
          endif
        endif


        ths=th0-dtth0
        if (ths.ge.(-1.0)*zpi.and.ths.lt.(2.0)*zpi) then
          goto 1200
        else
          ths=th0
        endif
        1200      continue
        if (ths.ge.zpi) ths=ths-zpi
        if (ths.lt. 0.) ths=ths+zpi
        jth=int(ths/deltth)+1
        jth1=jth+1
        if(jth.eq.jlp1)then
          jth=jl
          jth1=1
        endif
        if (jth1.eq.jlp1) jth1=1
        th1=thet(jth)
        th2=thet(jth+1)
		!-------------------------------------------------------------------------------
		
		
        if(glbflag == 0)then
	        if(xx < x(ixs))xx = xx + 360.
	        if(xx > x(ixl))xx = xx - 360.
        else
	        if(xx.lt.x(ixs))xx=x(ixs)
	        if(xx.ge.x(ixl))xx=x(ixl)
        endif
        

	ixx=0


	!do iii = ixs, ixl-1
        !  if(xx >= x(iii) .and. xx <= x(iii+1))then
        !  	ixx = iii; exit
        !  endif
        !enddo
	
	d_pre1=pre_ixx-ixs
	d_pre2=ixl-1-pre_ixx
	if(d_pre1>d_pre2) then
	  d_pre=d_pre1
	else	
	  d_pre=d_pre2
	endif
		
        do d_index=0,d_pre
	
	 tmp=pre_ixx-d_index
	 if(tmp>=ixs .and. tmp < ixl )then
	    if(xx >= x(tmp) .and. xx <= x(tmp+1))then
		ixx = tmp; exit
	    endif
	 endif
			
	 tmp=pre_ixx+d_index
	  if(tmp<ixl .and. tmp>=ixs) then
	    if(xx >= x(tmp) .and. xx <= x(tmp+1))then
		ixx = tmp; exit
	    endif
	  endif
	enddo
		
	pre_ixx=ixx 


			
        if(ixx < 1 .or. ixx > ixl)cycle
        
        if(yy < y(iys))yy = y(iys)
        if(yy > y(iyl))yy = y(iyl)
		
		
        jyy = 0

	!do iii = iys, iyl-1
        !  if(yy >= y(iii) .and. yy <= y(iii+1))then
        !  	jyy = iii; exit
        !  endif
        !enddo

	d_pre1=pre_jyy-iys
	d_pre2=iyl-1-pre_jyy
	if(d_pre1>d_pre2) then
	  d_pre=d_pre1
	else	
	  d_pre=d_pre2
	endif
		
        do d_index=0,d_pre
		
	 tmp=pre_jyy-d_index
	 if(tmp>=iys .and. tmp < iyl)then
	    if(yy >= y(tmp) .and. yy <= y(tmp+1))then
		jyy = tmp; exit
	    endif
	 endif
			
	 tmp=pre_jyy+d_index
	  if(tmp<iyl .and. tmp >=iys) then
	    if(yy >= y(tmp) .and. yy <= y(tmp+1))then
		jyy = tmp; exit
	    endif
	  endif
	enddo
		
	pre_jyy=jyy


        if(jyy < 0 .or. jyy > iyl)cycle

        ixx1=ixx+1
        jyy1=jyy+1
        x1=x(ixx)
        x2=x(ixx1)
        y1=y(jyy)
        y2=y(jyy1)

        e11=ee(iwk ,jth ,ixx,jyy)*fien
        e12=ee(iwk1,jth ,ixx,jyy)*fien1
        e13=ee(iwk ,jth1,ixx,jyy)*fien
        e14=ee(iwk1,jth1,ixx,jyy)*fien1
        call inter(wk1,wk2,th1,th2,e11,e12,e13,e14,wks,ths,e1)

        e21=ee(iwk ,jth ,ixx1,jyy)*fien
        e22=ee(iwk1,jth ,ixx1,jyy)*fien1
        e23=ee(iwk ,jth1,ixx1,jyy)*fien
        e24=ee(iwk1,jth1,ixx1,jyy)*fien1
        call inter(wk1,wk2,th1,th2,e21,e22,e23,e24,wks,ths,e2)

        e31=ee(iwk ,jth ,ixx,jyy1)*fien
        e32=ee(iwk1,jth ,ixx,jyy1)*fien1
        e33=ee(iwk ,jth1,ixx,jyy1)*fien
        e34=ee(iwk1,jth1,ixx,jyy1)*fien1
        call inter(wk1,wk2,th1,th2,e31,e32,e33,e34,wks,ths,e3)

        e41=ee(iwk ,jth ,ixx1,jyy1)*fien
        e42=ee(iwk1,jth ,ixx1,jyy1)*fien1
        e43=ee(iwk ,jth1,ixx1,jyy1)*fien
        e44=ee(iwk1,jth1,ixx1,jyy1)*fien1
        call inter(wk1,wk2,th1,th2,e41,e42,e43,e44,wks,ths,e4)

        call inter(x1,x2,y1,y2,e1,e2,e3,e4,xx,yy,exxyy)
        e(k,j,ia,ic)=max(exxyy, small)
		
      500      continue
	Kflag=1  	 
    200      continue
    Jflag=1
    Kflag=0
    101   continue
    Jflag=0
  100	continue
  return
end subroutine propagat
