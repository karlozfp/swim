!     FILE veget.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine  vegmd(j,je,k,n)	hydrotop



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine vegmd(j,je,k,n,wet)
!**** PURPOSE: CALC daily growth of plant biomass for natural vegetation
!**** CALLED IN:  HYDROTOP 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!      alai(j,je)     = leaf area index 
!      ald1(iv)       = shape parameter for the LAI developement equation 
!                       for veget iv
!      ald2(iv)       = shape parameter for the LAI developement equation 
!                       for veget iv
!      almn(iv)       = LAI minimum (for forest and natural vegetation)
!      be(iv)         = biomass-energy ratio for crop, kg m2 MJ-1 ha-1 d-1
!      blai(iv)       = max LAI for crop
!      cva(j,je)      = vegetation cover, kg/ha
!      daylen(j)      = day length in subbasin, h, calc in readcli
!      daylmn(j)      = min day length, h, calc. in readwet 
!      dlai(iv)       = fraction of season, when LAI declines
!      dm(j,je)       = total biomass, kg/ha
!      flu(j)         = fraction of subbasin area in the basin
!      g(j,je)        = fraction of heat units to maturity accumulated
!      huharv(j,je)   = harvest index heat unit
!      hun(iv)        = potential heat units required for maturity of crop
!      ida            = current day
!      idorm(j,je)    = index for dormant period
!      ih1,ih2,ih3    = hydrotopes for HYDROTOPE PRINTOUT
!      isb1,isb2,isb3 = subbasins for HYDROTOPE PRINTOUT
!      iy             = current year as counter (1,...,nbyr)
!      iyr            = current year
!      nveg(j,je)     = number of vegetation from crop database
!      olai(j,je)     = alai(j,je) - leaf area index
!      ra(j)          = solar radiation in subbasin j, J/cm^2
!      rd(j,je)       = root depth, mm
!      rdmx(iv)       = maximum root depth, mm 
!      rsd(j,je,2)    = residue, kg/ha
!      rwt(j,je)      = fraction of root weight
!      sdt            = sum temp stress days
!      sdw            = sum water stress days
!      sla(iv)        = specific leaf area, m2/kg, LAI/SLA  in kg/m2
!      tb(iv)         = base temperature for plant growth, degrees C
!      ts             = temp. stress
!      tsav(j,je)     = temp. stress, accumulated
!      tx(j)          = daily aver temp in the subbasin, degrees C    
!      uap            = P uptake, kg/ha
!      ws(j,je)       = water stress
!      wsav(j,je)     = water stress, accumulated
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      ddm        = delta dm
!      delg       = delta g
!      deltalai   = delta LAI
!      f          = fraction of plant's maximum leaf area index corresponding  
!                   to a given fraction of potential heat units for plant
!      ff         = delta f for the day
!      par        = local par
!      reg        = local par
!      resnew     = new residue
!      tgx        = local par
!      xx         = local par
!      yy         = local par
!      zz         = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_snow !#### SNOW MODULE       ####
implicit NONE
   integer j,je,k,n,wet
   real(8) ddm,delg,deltalai,f,ff,par,reg,resnew,tgx,xx,yy,zz
   integer :: veg_end
   real(8)    :: veg_wstress, veg_dieoff, dieoff
   
   ! ToDo: veg_end (end of vegetation period to restart growth)
   ! this parameter is to be defined somewhere as input parameter, not fixed like here!
   veg_end     = 365
   veg_wstress = .1
   veg_dieoff  = .99
   dieoff      = 0.

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      tx_tmp = tmit
   else
      tx_tmp = tx(j)
   end if
   !###########################
   
   uap = 0.
   ts = 0.    

!**** CALC land cover for natural vegetation:
   cva(j,je) = 1000. * alai(j,je) + rsd(j,je,1) 

!**** CHECK if start of dormant period
!     CALC rsd(), dm(), alai(), g()
!     Residue allocation formula is changed: to check!
!      if (idorm(j,je).eq.0) then
!      if (daylen(j)-1..lt.daylmn(j)) then
!      if (iy.ne.1.or.ida.ge.180) then
   if ( bDormancy ) then
      if ( idorm(j,je) == 0 ) then
         if ( daylen(j)+dormhr(j) < daylmn(j) ) then
            if ( iy > 1.OR.ida >= veg_end ) then
               idorm(j,je) = 1
               resnew = (olai(j,je)-alai(j,je))*10000./sla(nveg(j,je)) ! sla=specific leaf area, m2/kg, LAI/SLA  in kg/m2
               rsd(j,je,1) = rsd(j,je,1) + resnew*0.5
               rsd(j,je,2) = rsd(j,je,2) + resnew*0.5
               dm(j,je) = dm(j,je) - resnew
               alai(j,je) = almn(nveg(j,je))
               g(j,je) = 0.
            else
               idorm(j,je) = 1
               alai(j,je) = almn(nveg(j,je))
               g(j,je) = 0.
            end if
         end if
      end if
   else ! ( bDormancy )
      if (idorm(j,je).eq.0) then
         if (daylen(j)-1..lt.daylmn(j)) then
            if (iy.ne.1.or.ida.ge.180) then 
               idorm(j,je) = 1
               resnew = (olai(j,je)-alai(j,je))*10000./sla(nveg(j,je)) ! sla=specific leaf area, m2/kg, LAI/SLA  in kg/m2
               rsd(j,je,1) = rsd(j,je,1) + resnew*0.5
               rsd(j,je,2) = rsd(j,je,2) + resnew*0.5
               dm(j,je) = dm(j,je) - resnew
               alai(j,je) = almn(nveg(j,je))
               g(j,je) = 0.
            else
               idorm(j,je) = 1
               alai(j,je) = almn(nveg(j,je))
               g(j,je) = 0.
            end if
         end if
      end if
   end if ! ( bDormancy )

      
!**** check if end of dormant period
   if ( bDormancy ) then
      if ( (idorm(j,je) >= 1).AND.(daylen(j)+dormhr(j) >= daylmn(j)).AND.ida < veg_end ) then
         idorm(j,je) = 0
      end if
   else
      if (idorm(j,je).ge.1) then
         if( daylen(j)-1..ge.daylmn(j)) then
        idorm(j,je) = 0.
         end if
      end if
   end if ! ( bDormancy )

   !**** Assuming that the root depth of natural vegetation is not allowed to decrease
   rd(j,je)=rdmx(nveg(j,je))

!#### CALL WSTRESS
   call wstress(j,je,k,n,wet)
      

!sl begin
!###########################################################
!**** This code snippet has been included to better account for vegetation growth
!     controlled by water stress rather than temperature or daylength.
!     Vegetation growth is limited to water availability using the soil water index.
!     This part is only active if parameter nat_veg is 1 (*.bsn)
   if ( bDormancy ) then
      ! swe(j,jea)/sumfc(k) = soil water index (0-1)
      ! swe                 = soil water content [mm]
      ! sumfc               = sum of field capacity in soil [mm]
      if ( swe(j,je)/sumfc(k) <= veg_wstress ) then
         idorm(j,je) = 1                         ! vegetation stops growing (is dormant due to water stress)
         olai(j,je) = alai(j,je)                 ! olai = alai of previous day
         alai(j,je) = alai(j,je) * veg_dieoff    ! assuming dying vegetation up to a certain amount per time step
         alai(j,je) = max(alai(j,je),almn(nveg(j,je)))

         ! calcuate residue from difference between acutal lai and lai of previous day
         resnew = (olai(j,je)-alai(j,je))*10000./sla(nveg(j,je)) ! sla=specific leaf area, m2/kg, LAI/SLA  in kg/m2
         resnew = max(0.,resnew)
         
         rsd(j,je,1) = rsd(j,je,1) + resnew * .5 ! half of this residue is contributing to residue of upper storage
         rsd(j,je,2) = rsd(j,je,2) + resnew * .5 ! the other half contributes to second storage
         
         dm(j,je) = dm(j,je) - resnew            ! reduce biomass by value of residue
         dm = max(0.,dm)
      else
         idorm(j,je) = 0
      end if

      ! fraction of heat units and alai need to be initialized after growing season
      ! ToDo: the definition of the growing season needs re-thinking!
      if ( idorm(j,je) == 0 ) then
         if ( g(j,je) >= 1. ) then
            g(j,je) = 0.
            !alai(j,je) = almn(nveg(j,je))
         end if
      end if

      if ( ida == veg_end ) then
         g(j,je) = 0.
      end if
   end if ! if ( bDormancy )


!###########################################################
!sl end

!       if (idorm(j,je).ge.1) goto 10
   if ( idorm(j,je) == 0) then


!**** COMPUTE DAILY INCREASE IN HEAT UNITS delg
      delg = (tx_tmp-tb(nveg(j,je)))/hun(nveg(j,je))
      if (delg.lt.0.) delg = 0.
      g(j,je) = g(j,je) + delg
      if (g(j,je).gt.1.) g(j,je) = 1.     

!*********************************************************** START IF (G<=1)  
!**** GROWTH SEASON
      if (g(j,je).le.1) then

!####    CALL TSTRESS: COMPUTE TEMPERATURE STRESS 
         tgx = tx_tmp - tb(nveg(j,je))
         if (tgx.le.0.) then
            ts = 0.
         else
            call tstress(tgx,j,je,n)
         end if      
            
!****    CALC daily biomass increase: ddm
         par = .005 * ra(j) * (1.-exp(-.65*(alai(j,je)+.05)))
         ddm = be(nveg(j,je)) * par
         if (ddm.lt.0.) ddm = 0.

!####    CALL NUPTAKE, PUPTAKE: CALCULATE N AND P UPTAKE
         call nuptake(j,je,nveg(j,je))
         call puptake(j,je,nveg(j,je))

!****    CALC BIOMASS dm(), ROOT WEIGHT rwt(9 
         xx = dm(j,je) + ddm
         reg = amin1(ws(j,je),ts)
         if(reg.lt.0.) reg = 0.
         if(reg.gt.1.) reg = 1.        
         dm(j,je) = dm(j,je) + ddm * reg
         rwt(j,je) = (.4-.2*g(j,je))
        
         tsav(j,je) = tsav(j,je) + ts
         wsav(j,je) = wsav(j,je) + ws(j,je)
        
!****    CALC f, ff, huharv()
         f = g(j,je) / (g(j,je) + exp(ald1(nveg(j,je))-ald2(nveg(j,je))*g(j,je)))
         ff = f - huharv(j,je)
         huharv(j,je) = f

!****    CALC SUM STRESS DAYS 
         sdw = sdw + (1.-ws(j,je)) * flu(j)
         sdt = sdt + (1.-ts) * flu(j)

!****    CALC LAI and adjust for lower limit of LAI for forest alnm()
         if ( g(j,je).le.dlai(nveg(j,je)) ) then
            if ( alai(j,je).gt.blai(nveg(j,je)) ) alai(j,je)= blai(nveg(j,je))
            deltalai = ff * blai(nveg(j,je)) * (1.-exp(5.*(alai(j,je) - blai(nveg(j,je)))))*sqrt(reg)
            alai(j,je) = alai(j,je) + deltalai
            if ( alai(j,je).gt.blai(nveg(j,je)) ) alai(j,je)= blai(nveg(j,je))
            olai(j,je) = alai(j,je)
            if ( alai(j,je).lt.almn(nveg(j,je)) )alai(j,je)= almn(nveg(j,je))
         else
            yy = sqrt(1.-g(j,je))
            zz=1./sqrt(1.-dlai(nveg(j,je)))
            alai(j,je) = zz * olai(j,je) * yy
            if ( alai(j,je).lt.almn(nveg(j,je)) ) alai(j,je)= almn(nveg(j,je))
         end if ! ( g(j,je).le.dlai(nveg(j,je)) )
      end if ! (g(j,je).le.1)
!*********************************************************** END IF (G<=1)  
   else ! ( idorm(j,je) == 0)

      if(j.eq.isb1.and.je.eq.ih1) then            
         if(ida.eq.1) write (55,107)
         write (55,101)  iyr-1900,ida,j,je,ws(j,je),ts,g(j,je) &
                         ,dm(j,je),alai(j,je) 
      endif
      if(j.eq.isb2.and.je.eq.ih2) then
      if(ida.eq.1) write (56,107)
         write (56,101)  iyr-1900,ida,j,je,ws(j,je),ts,g(j,je) &
                        ,dm(j,je),alai(j,je) 
      endif
      if(j.eq.isb3.and.je.eq.ih3) then
      if(ida.eq.1) write (57,107)
         write (57,101)  iyr-1900,ida,j,je,ws(j,je),ts,g(j,je) &
                        ,dm(j,je),alai(j,je) 
      endif

   end if ! ( idorm(j,je) == 0)
   
   !if (j==isb2.and.je==ih2) write(876,*)ida,idorm(j,je),daylen(j),daylmn(j),alai(j,je)

   
  101 format (4i5,3f6.2,8f15.2)             
  107 format(/,'   YR  DAY  SUB  HTP  WATS  TEMS HEATU',13X,'DM',12X,'LAI')
            
end subroutine vegmd



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
