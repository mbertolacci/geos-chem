! $Id: upbdflx_mod.f,v 1.2 2003/07/08 15:30:13 bmy Exp $
      MODULE UPBDFLX_MOD
!
!******************************************************************************
!  Module UPBDFLX_MOD contains subroutines which impose stratospheric boundary
!  conditions on O3 and NOy (qli, bdf, mje, bmy, 6/28/01, 4/14/03)
!
!  Module Routines:
!  ============================================================================
!  (1 ) DO_UPBDFLX     : Driver for stratospheric flux boundary conditions
!  (2 ) UPBDFLX_O3     : Computes flux of O3 from stratosphere, using Synoz
!  (3 ) UPBDFLX_NOY    : Computes flux of NOy from stratosphere
!
!  GEOS-CHEM modules referenced by upbdflx_mod.f
!  ============================================================================
!  (1 ) bpch2_mod.f    : Module containing routines for binary punch file I/O
!  (2 ) error_mod.f    : Module containing NaN and other error check routines
!  (3 ) tracerid_mod.f : Module containing pointers to tracers & emissions
!  (4 ) upbdflx_mod.f  : Module containing routines to compute P(I,J,L)
!
!  NOTES: 
!  (1 ) Routine "upbdflx_noy" now correctly reprocessed P(NOy) files from
!        /data/ctm/GEOS_4x5/pnoy_200106 or /data/ctm/GEOS_2x2.5/pnoy_200106.
!        (mje, bmy, 6/28/01)
!  (2 ) Updated comments (bmy, 9/4/01)
!  (3 ) Fixes for reading binary punch files of global size (bmy, 9/27/01)
!  (4 ) Removed obsolete commented out code from 9/01 (bmy, 10/24/01)
!  (5 ) Removed obsolete commented out code from 7/01 (bmy, 11/26/01)
!  (6 ) Updated comments (bmy, 5/28/02)
!  (7 ) Replaced all instances of IM with IIPAR and JM with JJPAR, in ordr
!        to prevent namespace confusion for the new TPCORE (bmy, 6/25/02)
!  (8 ) Now references "pressure_mod.f" (dsa, bdf, bmy, 8/21/02)
!  (9 ) Now references BXHEIGHT from "dao_mod.f".  Also deleted obsolete
!        code from 8/02.  Now references IDTNOx, IDTOX, from "tracerid_mod.f" 
!        instead of from "comtrid.h". (bmy, 11/6/02)
!  (10) Added driver routine DO_UPBDFLX.  Also added lat limits for 1x1 in
!        UPBDFLX_O3. (bmy, 3/14/03)
!  (11) Now references AD from "dao_mod.f" in UPBDFLX_NOY (bnd, bmy, 4/14/03)
!******************************************************************************
!      
      IMPLICIT NONE

      !=================================================================
      ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
      !=================================================================
      CONTAINS

!------------------------------------------------------------------------------
      
      SUBROUTINE DO_UPBDFLX( IORD, JORD, KORD )
!
!******************************************************************************
!  Subroutine DO_UPBDFLX is the driver routine for the stratospheric (upper-
!  boundary) routines for Ox and NOy. (bmy, 3/11/03)
!
!  Arguments as Input:
!  ===========================================================================
!  (1 ) IORD (INTEGER) : TPCORE E/W      transport option flag 
!  (2 ) JORD (INTEGER) : TPCORE N/S      transport option flag
!  (3 ) KORD (INTEGER) : TPCORE vertical transport option flag
!  
!  NOTES:
!******************************************************************************
!
      ! References to F90 modules
      USE ERROR_MOD, ONLY : DEBUG_MSG

#     include "CMN_SIZE"  ! Size parameters
#     include "CMN"       ! NSRCX, LPRT

      ! Arguments
      INTEGER, INTENT(IN) :: IORD, JORD, KORD

      !=================================================================
      ! DO_UPBDFLX begins here!
      !=================================================================
      SELECT CASE ( NSRCX )

         ! Full chem w/ SMVGEAR: set O3 and NOY fluxes
         CASE ( 3 )
            CALL UPBDFLX_O3( IORD, JORD, KORD )
            CALL UPBDFLX_NOY( 1 )

         ! Tagged Ox: set O3 fluxes
         CASE ( 6 )
            CALL UPBDFLX_O3( IORD, JORD, KORD )

         CASE DEFAULT
            ! Nothing
                  
      END SELECT

      !### Debug
      IF ( LPRT ) CALL DEBUG_MSG( '### DO_UPBDFLX: after strat fluxes' )

      ! Return to calling program
      END SUBROUTINE DO_UPBDFLX

!------------------------------------------------------------------------------

      SUBROUTINE UPBDFLX_O3( IORD, JORD, KORD )      
!
!******************************************************************************
!  Subroutine UPBDFLX_O3 establishes the flux boundary condition for Ozone
!  coming down from the stratosphere, using the Synoz algorithm of
!  McLinden et al, 2000. (qli, bmy, 12/13/99, 4/14/03)
!
!  Arguments as Input:
!  ===========================================================================
!  (1 ) IORD (INTEGER) : TPCORE flag for longitude transport options 
!  (2 ) JORD (INTEGER) : TPCORE flag for latitude  transport options 
!  (3 ) KORD (INTEGER) : TPCORE flag for vertical  transport options 
!
!  Reference:
!  ===========================================================================
!  C. A. McLinden, S. Olsen, B. Hannegan, O. Wild, M. J. Prather, and
!  J. Sundet, "Stratospheric Ozone in 3-D models: A simple chemistry
!  and the cross-tropopause flux".
!
!  NOTES:
!  (1 ) The parameter Rdg0 from "CMN_GCTM" = R / g0 = 28.97. 
!  (2 ) Pass PW = PS - PTOP to UPBDFLX via "CMN".
!  (3 ) Now pass IORD, JORD, KORD as arguments (bmy, 12/6/00)
!  (4 ) Now compute the proper value of PO3_vmr that will yield 475 Tg O3/yr
!        for various settings of IORD, JORD, KORD (rvm, bey, bmy, 12/5/00)
!
!        **************************************************************
!        ***** You must use this version of UPBDFLX_O3 if you are *****
!        ***** using the Parallel Processor TPCORE v. 7.1         *****
!        **************************************************************
!
!  (5 ) Added to "upbdflx_mod.f".  Also updated comments and made some
!        cosmetic changes. (bmy, 6/28/01)
!  (6 ) Now reference CMN_SETUP for LSPLIT.  Also store strat O3 into
!        tracer #11 for multi-tracer Ox run. (amf, bmy, 7/3/01)
!  (7 ) Removed IREF, JREF -- these are obsolete.  Also T(IREF,JREF,L) is
!        now T(I,J,L). (bmy, 9/27/01)
!  (8 ) Also replace PW(I,J) with P(I,J) (bmy, 10/3/01)
!  (9 ) Removed obsolete commented out code from 9/01 (bmy, 10/24/01)
!  (10) Removed obsolete commented out code from 7/01 (bmy, 11/26/01)
!  (11) Now write file names to stdout (bmy, 4/3/02)
!  (12) Replaced all instances of IM with IIPAR and JM with JJPAR, in order
!        to prevent namespace confusion for the new TPCORE (bmy, 6/25/02)
!  (13) Now use GET_PEDGE and GET_PCENTER from "pressure_mod.f" to compute
!        the pressure at the bottom edge and center of grid box (I,J,L).
!        Also removed obsolete, commented-out code.  Removed G_SIG and
!        G_SIGE from the arg list. (dsa, bdf, bmy, 8/21/02)
!  (14) Now reference BXHEIGHT and T from "dao_mod.f".  Also reference routine
!        ERROR_STOP from "error_mod.f".  Now references IDTOX from F90 module
!        "tracerid_mod.f" instead of from "comtrid.h". (bmy, 11/6/02)
!  (15) Now define J30S and J30N for 1x1 nested grid (bmy, 3/11/03)
!  (16) Make sure to pass AD via "dao_mod.f" for GEOS-1 (bnd, bmy, 4/14/03)
!******************************************************************************
!      
      ! References to F90 modules
      USE DAO_MOD,      ONLY : AD, BXHEIGHT, T
      USE ERROR_MOD,    ONLY : ERROR_STOP
      USE PRESSURE_MOD, ONLY : GET_PEDGE, GET_PCENTER
      USE TIME_MOD,     ONLY : GET_TS_DYN
      USE TRACERID_MOD, ONLY : IDTOX
      
#     include "CMN_SIZE"   ! Size parameters
#     include "CMN"        ! STT, NSRCX
#     include "CMN_GCTM"   ! Rdg0
#     include "CMN_SETUP"  ! LSPLIT 

      ! Arguments
      INTEGER, INTENT(IN)  :: IORD, JORD, KORD

      ! Local variables
      INTEGER              :: I, J, L, L70mb
      INTEGER              :: NTRACER, NTRACE2
      REAL*8               :: P1, P2, P3, T1, T2, DZ, ZUP
      REAL*8               :: DTDYN, H70mb, PO3, PO3_vmr

      ! Select the grid boxes at the edges of the O3 release region, 
      ! for the proper model resolution (qli, bmy, 12/5/00)
#if   defined( GRID4x5 ) 
      INTEGER, PARAMETER   :: J30S = 16, J30N = 31  

#elif defined( GRID2x25 ) 
      INTEGER, PARAMETER   :: J30S = 31, J30N = 61

#elif defined( GRID1x1 ) 
      INTEGER, PARAMETER   :: J30S = 1, J30N = JJPAR   

#endif

      ! Lower pressure bound for O3 release (unit: mb)
      REAL*8,  PARAMETER   :: P70mb = 70d0

      !=================================================================
      ! UPBDFLX_O3 begins here!
      !=================================================================

      ! Dynamic timestep [s]
      DTDYN = GET_TS_DYN() * 60d0

      !=================================================================
      ! Compute the proper release rate of O3 coming down from the 
      ! stratosphere for the different GEOS model grids. 
      ! (bey, djj, rvm, bmy, 12/5/00).
      !
      ! PO3_vmr is the O3 release rate constant [v/v/s] that will yield 
      ! a strat-to-trop flux of 475 [Tg O3/yr].  Different TPCORE flags 
      ! create different amounts of ozone in the stratosphere.  Flags 
      ! 337F are currently preferred (bey, djj, rvm).  
      !  
      ! For now, provide values for PO3_vmr for two TPCORE flag settings:
      !  (1) IORD = 3, JORD = 3, KORD = 7  (preferred, assumed to 
      !                                     be the default)
      !  (2) IORD = 5, JORD = 5, KORD = 7 
      !=================================================================
#if   defined( GEOS_1 )

      ! These are only guesses.  As of 12/5/00, the GEOS1 flux has not 
      ! been tested in TPCORE v7.1.  Since the GEOS1 stratosphere is 
      ! only 6/7 as large as the GEOS_STRAT stratosphere, the ozone 
      ! production per unit mass should be larger in GEOS1 (bey, rvm, qli)
      PO3_vmr = 4.9d-14                                  ! 3,3,7

#elif defined( GEOS_STRAT )

      ! The O3 flux has been tested better in GEOS-STRAT (bmy, 12/5/00)
      PO3_vmr = 4.2d-14                                  ! 3,3,7
      IF ( IORD + JORD + KORD == 17 ) PO3_vmr = 4.07d-14 ! 5,5,7

#elif defined( GEOS_2 )
#error "Not running full chem yet with GEOS-2 data..."

#elif defined( GEOS_3 )

      ! Use the GEOS-STRAT ozone flux just to get going (bmy, 7/3/01)
      PO3_vmr = 4.2d-14                                  ! 3,3,7
      IF ( IORD + JORD + KORD == 17 ) PO3_vmr = 4.07d-14 ! 5,5,7

#endif

      !=================================================================
      ! Select the proper tracer number to store O3 into, depending on
      ! whether this is a full chemistry run or a single tracer Ox run
      !=================================================================
      SELECT CASE ( NSRCX )

         ! Full chemistry
         CASE ( 3 )
            NTRACER = IDTOX
          
         ! Single or multi-tracer Ox
         CASE ( 6 )
            NTRACER = 1

            ! For multi-tracer Ox: change to carry 12 tracers for O3
            ! tagged run of which strat O3 is tracer 11. (amf, 7/3/01)
            IF ( LSPLIT ) NTRACE2 = 11

         ! All other simulations don't use O3...print error message
         CASE DEFAULT
            CALL ERROR_STOP( 'This simulation does not use O3!',
     &                       'UPBDFLX_O3 (upbdflx_mod.f)' )

      END SELECT

      !=================================================================
      ! Loop over latitude/longtitude locations (I,J)
      ! Define window offsets IREF = I + I0, JREF = J + J0
      !=================================================================
      DO J = J30S, J30N 
         DO I = 1, IIPAR

            !===========================================================
            ! L70mb is the 1st layer where pressure is equal to
            ! or smaller than 70 mb 
            !
            ! P1 = pressure [ mb ] at the sigma center     of level L70mb - 1
            ! P3 = pressure [ mb ] at the lower sigma edge of level L70mb
            ! P2 = pressure [ mb ] at the sigma center     of level L70mb
            !===========================================================
            DO L = 1, LLPAR
               P2 = GET_PCENTER(I,J,L) 
               
               IF ( P2 < P70mb ) THEN
                  L70mb = L
                  EXIT
               ENDIF
            ENDDO
            
            P1 = GET_PCENTER(I,J,L70mb-1) 
            P3 = GET_PEDGE(I,J,L70mb) 
 
            !===========================================================
            ! T2 = temperature (K)  at the sigma center of level L70mb
            ! T1 = temperature (K)  at the sigma center of level L70mb-1
            !
            ! DZ is the height from the sigma center of level L70mb-1 
            ! to 70mb.  Therefore, DZ may be found in either the 
            ! (L70mb)th sigma layer or the (L70mb-1)th sigma layer.  
            !
            ! ZUP is the height from the sigma center of the 
            ! (L70mb-1)th layer
            !=========================================================== 
            T2   = T(I,J,L70mb  )
            T1   = T(I,J,L70mb-1)        

            DZ   = Rdg0 * ( (T1 + T2) / 2d0 ) * LOG( P1 / P70mb ) 
            ZUP  = Rdg0 * T1 * LOG( P1 /P3 )

            !===========================================================       
            ! H70mb is height between 70mb and the upper edge of the 
            ! level where DZ is.
            !  
            ! If DZ >= ZUP then DZ is already in level L70mb.
            ! If DZ <  ZUP then DZ is in level L70mb-1.
            !===========================================================       
            IF ( DZ >= ZUP ) THEN
               H70mb = BXHEIGHT(I,J,L70mb) - ( DZ - ZUP )
            ELSE
               L70mb = L70mb - 1
               H70mb = ZUP - DZ
            ENDIF

            !=========================================================== 
            ! Distribute O3 into the region (30S-30N, 70mb-10mb)
            !=========================================================== 
            DO L = L70mb, LLPAR 

               ! Convert O3 in grid box (I,J,L) from v/v/s to kg/box
               PO3 = PO3_vmr * DTDYN 

               ! For both 2 x 2.5 and 4 x 5 GEOS grids, 30S and 30 N are
               ! grid box centers.  However, the O3 release region is 
               ! edged by 30S and 30N.  Therefore, if we are at the 30S
               ! or 30N grid boxes, divide the O3 flux by 2.
               IF ( J == J30S .or. J == J30N ) THEN
                  PO3 = PO3 / 2d0
               ENDIF 

               ! If we are in the lower level, compute the fraction
               ! of this level that lies above 70 mb, and scale 
               ! the O3 flux accordingly.
               IF ( L == L70mb ) THEN
                  PO3 = PO3 * H70mb / BXHEIGHT(I,J,L) 
               ENDIF
            
               ! Store O3 flux in the proper tracer number
               STT(I,J,L,NTRACER) = STT(I,J,L,NTRACER) + PO3 

               ! Store strat O3 flux into tracer #11 for
               ! multi-tracer Ox run (amf, bmy, 7/3/01)
               IF ( LSPLIT .and. NSRCX == 6 ) THEN
                  STT(I,J,L,NTRACE2) = STT(I,J,L,NTRACE2) + PO3
               ENDIF
               
            ENDDO   
         ENDDO
      ENDDO

      ! Return to calling program
      END SUBROUTINE UPBDFLX_O3 

!------------------------------------------------------------------------------

      SUBROUTINE UPBDFLX_NOY( IFLAG )
!
!******************************************************************************
!  Subroutine UPBDFLX_NOY imposes NOy (NOx + HNO3) upper boundary condition
!  in the stratosphere. The production rates for NOy are provided by Dylan
!  Jones, along with NOx and HNO3 concentrations. 
!  (qli, rvm, mje, bmy, 12/22/99, 4/14/03)
!
!  Arguments as input:
!  ===========================================================================
!  (1) IFLAG : IFLAG=1 will partition    [NOy] before transport
!              IFLAG=2 will re-partition [NOy] after  transport
!
!  NOTES:
!  (1 ) Use READ_BPCH2 to read data from disk in binary punch file format.
!  (2 ) Now partition total [NOy] into [NOx] and [HNO3], instead of
!        partitioning P(NOy) into P(NOx) and P(HNO3).  (qli, bmy, 12/22/1999)
!  (3 ) Also echo back to the user when reading data from disk.  This 
!        allows the user to trace I/O errors more easily. (bmy, 2/1/00)
!  (4 ) Cosmetic changes, updated comments (bmy, 3/17/00)
!  (5 ) Reference F90 module "bpch2_mod" which contains routine "read_bpch2"
!        for reading data from binary punch files (bmy, 6/28/00)
!  (6 ) Only add P(NOy) above 10mb (archived in files "pnoy_above_10mb.*) 
!        into the top layer of the GEOS-1 and GEOS-STRAT grids.  The GEOS-2
!        and GEOS-3 grids extend well above 10mb and so they will contain
!        all of the P(NOy) up there (bmy, 6/29/00)
!  (7 ) Now use function GET_TAU0 (from "bpch2_mod.f") to return the TAU0 
!        value used to index the binary punch file. (bmy, 7/20/00)
!  (8 ) Only dump P(NOy) above 10mb for GEOS-1 grid.  The GEOS-STRAT grid
!        will already have this contribution, since it extends to 0.1 mb.
!        Also fix regridding error in P(NOy) data file.  Add parallel
!        processor DO-loops. (rvm, qli, bmy, 12/6/00)
!  (9 ) Now scale P(NOy) by 0.7 for TPCORE flags 337, in order to prevent
!        excess NOy from building up in the stratosphere. (rvm, bmy, 12/12/00)
!  (10) Now read properly regridded P(NOy) files from the pnoy_200106/
!        subdirectory of DATA_DIR.  Also updated comments and made a
!        few cosmetic changes. (mje, bmy, 6/28/01)
!  (11) Now use 3 arguments (M/D/Y) in call to GET_TAU0.  ARRAY needs to be 
!        of size (1,JGLOB,LGLOB).  Use JGLOB,LGLOB in calls to READ_BPCH2.
!        Use TRANSFER_ZONAL (from "transfer_mod.f") to cast from REAL*4 to
!        REAL*8 and resize arrays to (JJPAR,LLPAR) (bmy, 9/27/01)
!  (12) Removed obsolete commented out code from 9/01 (bmy, 10/24/01)
!  (13) Now write file name to stdout (bmy, 4/3/02)
!  (14) Now reference ERROR_STOP from "error_mod.f".  Also references IDTNOX
!        and IDTHNO3 from "tracerid_mod.f". (bmy, 11/6/02)
!  (15) Rename MONTHSAVE to LASTMONTH.  Now use functions GET_TS_DYN and
!        GET_MONTH from "time_mod.f".  Now call READ_BPCH2 with QUIET=.TRUE.
!        to suppress printing of extra info.  Cosmetic changes.  Now references
!        AD from "dao_mod.f" for GEOS-1 (bmy, 4/14/03)
!******************************************************************************
!      
      ! References to F90 modules
      USE BPCH2_MOD
      USE DAO_MOD,      ONLY : AD
      USE ERROR_MOD,    ONLY : ERROR_STOP
      USE TRACERID_MOD, ONLY : IDTNOX,     IDTHNO3
      USE TIME_MOD,     ONLY : GET_TS_DYN, GET_MONTH
      USE TRANSFER_MOD, ONLY : TRANSFER_ZONAL

#     include "CMN_SIZE"   ! Size parameters
#     include "CMN"        ! STT 
#     include "CMN_O3"     ! XNUMOLAIR
#     include "CMN_SETUP"  ! DATA_DIR

      ! Arguments
      INTEGER, INTENT(IN)  :: IFLAG

      ! Local variables
      INTEGER              :: I, J, L
      INTEGER, SAVE        :: LASTMONTH

      REAL*4               :: DTDYN, AIRDENS, PNOY
      REAL*4               :: ARRAY(1,JGLOB,LGLOB)

      ! Ratio of ( [NO] + [NO2] ) / [NOy] 
      REAL*4, SAVE         :: XRATIO(JJPAR,LLPAR) 

      ! Arrays for P(NOY), NO, NO2, and HNO3 concentrations
      REAL*4, SAVE         :: STRATPNOY(JJPAR,LLPAR)
      REAL*4, SAVE         :: STRATNO(JJPAR,LLPAR)
      REAL*4, SAVE         :: STRATNO2(JJPAR,LLPAR)
      REAL*4, SAVE         :: STRATHNO3(JJPAR,LLPAR)

      ! For P(NOy) above 10 mb
      REAL*4, SAVE         :: SPNOY10mb(JJPAR)

      ! TAU values for indexing the punch file 
      REAL*8               :: XTAU

      ! File Names
      CHARACTER (LEN=255)  :: FILENAME
      CHARACTER (LEN=255)  :: FILENAME2

      ! External functions
      REAL*8,  EXTERNAL    :: BOXVL

      !=================================================================
      ! UPBDFLX_NOY begins here! 
      !=================================================================

      ! Dynamic timestep [s]
      DTDYN = GET_TS_DYN() * 60d0

      !=================================================================
      ! IFLAG = 1: Before transport
      !
      ! If we have entered into a new month, read P(NOy), HNO3,
      ! NO, and NO2 from disk (binary punch file format).
      !=================================================================
      IF ( IFLAG == 1 ) THEN

         IF ( GET_MONTH() /= LASTMONTH ) THEN

            ! Save the current month 
            LASTMONTH = GET_MONTH()

            ! TAU value corresponding to the beginning of this month
            XTAU = GET_TAU0( GET_MONTH(), 1, 1985 ) 

            ! File containing P(NOy), NOx, HNO3 concentrations
            ! Now read corrected file from pnoy_200106/ subdir (bmy, 6/28/01)
            FILENAME  = TRIM( DATA_DIR )             // 
     &                  'pnoy_200106/pnoy_nox_hno3.' //
     &                  GET_NAME_EXT()   // '.'      // GET_RES_EXT()

            !===========================================================
            !### KLUDGE -- for now, read properly regridded P(NOy) 
            !### file for GEOS-STRAT grid.  This will become part
            !### of the standard code later. (qli, rvm, bmy, 12/6/00)
            !===========================================================
#if   defined( GEOS_STRAT )
            FILENAME = TRIM( FILENAME ) // '.rvm'
#endif

            ! Echo filename to stdout
            WRITE( 6, 100 ) TRIM( FILENAME )
 100        FORMAT( '     - UPBDFLX_NOY: Reading ', a )
            
            ! P(NOy) in [v/v/s] is stored as tracer #1
            CALL READ_BPCH2( FILENAME, 'PNOY-L=$', 1,     
     &                       XTAU,      1,         JGLOB,     
     &                       LGLOB,     ARRAY,     QUIET=.TRUE. )

            ! Cast from REAL*4 to REAL*8 and resize to (JJPAR,LLPAR)
            CALL TRANSFER_ZONAL( ARRAY(1,:,:), STRATPNOY )

            !===========================================================
            ! rvm (12/02/00) scale PNOy for new tpcore flags 337F for
            ! the GEOS-STRAT model.  This is a crude fix to prevent too 
            ! much NOy flux from mass generation in the stratosphere.
            ! This fix should yield a NOy flux of 0.55 Tg N/yr.
            !
            ! CAVEAT: The flux might not be correct for the 2 x 2.5
            ! model, should test this later on. (rvm, bmy, 12/12/00)
            !===========================================================
#if   defined( GEOS_STRAT )
            STRATPNOY(:,:) = STRATPNOY(:,:) * 0.7e0 
#endif
            
            ! [HNO3] in [v/v] is stored as tracer #2
            CALL READ_BPCH2( FILENAME, 'PNOY-L=$', 2,     
     &                       XTAU,      1,         JGLOB,     
     &                       LGLOB,     ARRAY,     QUIET=.TRUE. )

            ! Cast from REAL*4 to REAL*8 and resize to (JJPAR,LLPAR)
            CALL TRANSFER_ZONAL( ARRAY(1,:,:), STRATHNO3 )
         
            ! [NO] in [v/v] is stored as tracer #4
            CALL READ_BPCH2( FILENAME, 'PNOY-L=$', 4,     
     &                       XTAU,      1,         JGLOB,     
     &                       LGLOB,     ARRAY,     QUIET=.TRUE. )

            ! Cast from REAL*4 to REAL*8 and resize to (JJPAR,LLPAR)
            CALL TRANSFER_ZONAL( ARRAY(1,:,:), STRATNO )

            ! [NO2] in [v/v] is stored as tracer #5
            CALL READ_BPCH2( FILENAME, 'PNOY-L=$', 5,     
     &                       XTAU,      1,         JGLOB,     
     &                       LGLOB,     ARRAY,     QUIET=.TRUE. )

            ! Cast from REAL*4 to REAL*8 and resize to (JJPAR,LLPAR)
            CALL TRANSFER_ZONAL( ARRAY(1,:,:), STRATNO2 )

            !===========================================================
            ! P(NOy) above 10mb is stored in a separate file for 
            ! GEOS-1 grid.  Do not add this for the GEOS-STRAT grid.
            ! (qli, rvm, bmy, 12/6/00)
            !===========================================================
#if   defined( GEOS_1 )

            ! File containing P(NOy) above 10 mb
            ! Now read corrected file from pnoy_200106/ subdir (bmy, 6/28/01)
            FILENAME2 = TRIM( DATA_DIR )               // 
     &                  'pnoy_200106/pnoy_above_10mb.' //
     &                  GET_NAME_EXT()   // '.'        // GET_RES_EXT()

            ! Write file name to stdout
            WRITE( 6, 100 ) TRIM( FILENAME )

            ! P(NOy) in [molec/cm3/s] above 10mb 
            ! is stored as tracer #6 in a separate file
            CALL READ_BPCH2( FILENAME2, 'PNOY-L=$', 6, 
     &                       XTAU,       1,         JGLOB,     
     &                       1,          ARRAY,     QUIET=.TRUE. )

            SPNOY10mb(:) = ARRAY(1,:,1)

#endif

         ENDIF

         !==============================================================
         ! Initial partitioning of [NOy] to [NOx] and [HNO3], before 
         ! transport
         ! 
         ! We use zonal mean values for stratospheric P(NOy), [NO], 
         ! [NO2], and [HNO3] taken from Dylan Jones' & Hans Schneider's 
         ! 2-D model.  
         !
         ! Since P(NOy) above 10mb accounts for almost 50% of the total 
         ! stratospheric production, we also dump P(NOy) above 10 mb 
         ! into the top layer of the model.  These values are also 
         ! supplied to us by Dylan Jones.
         !
         ! We make the following assumptions:
         !
         !    (1) [NOx] = [NO] + [NO2] 
         !    (2) [NOy] = [NO] + [NO2] + [HNO3] = [NOx] + [HNO3]  
         !
         ! Therefore, in order to obtain [NOx] and [HNO3] from [NOy], 
         ! we must do the partitioning as follows:
         !
         !    (1) [NOy]   = P(NOy) + [NOx] + [HNO3]  
         !                = Production of NOy plus current 
         !                  concentrations of NOx and HNO3 in the 
         !                  given grid box
         !
         !    (2) XRATIO  = ( [NO] + [NO2] ) / [NOy]
         !
         !    (3) P(NOx)  = P(NOy) * XRATIO
         !  
         !    (4) P(HNO3) = P(NOy) * ( 1 - XRATIO ) 
         !
         ! XRATIO = ( [NO] + [NO2] ) / [NOy] approximates the true 
         ! ratio of [NOx] / [NOy], but is itself not the true ratio, 
         ! since our formulation of [NOy] neglects some additional 
         ! species (e.g. PAN, HNO4, N2O5, R4N2, PPN, PMN).
         !
         ! At some future point we may take the additional constituents 
         ! of [NOy] into account.  For now we proceed as outlined above.
         !==============================================================

         !print*, 'In upbdflx_noy, before imposing NOx upbd flux '

!! Deactivate parallel processor DO-loops for now (bmy, 12/12/00)
!!$OMP PARALLEL DO
!!$OMP+DEFAULT( SHARED )
!!$OMP+SCHEDULE( DYNAMIC )
!!$OMP+PRIVATE( I, J, L, PNOY )
!#if   defined( GEOS_1 )
!!$OMP+PRIVATE( AIRDENS )
!#endif
         DO L = MINVAL( LPAUSE ), LLPAR
         DO J = 1, JJPAR
         DO I = 1, IIPAR

            ! Skip over tropospheric boxes
            IF ( L < LPAUSE(I,J) ) CYCLE

            ! PNOY = P(NOy) converted from [v/v/s] to [v/v] 
            PNOY = STRATPNOY(J,L) * DTDYN 

#if   defined( GEOS_1 )
            !===========================================================
            ! Top level -- dump P(NOy) above 10 mb here
            ! AIRDENS   has units of [molec/cm3  ]
            ! SPNOY10mb has units of [molec/cm3/s]
            ! Convert SPNOY10mb to [v/v] and add to PNOY
            !
            ! NOTE: We only have to do this for GEOS-1 grid, since 
            ! GEOS-STRAT, GEOS-2 and GEOS-3 grids extend well above 
            ! 10 mb. (qli, rvm, bmy, 12/6/00)
            !===========================================================
            IF ( L == LLPAR ) THEN 
               AIRDENS = AD(I,J,L) * XNUMOLAIR / BOXVL(I,J,L)
               PNOY    = PNOY + ( SPNOY10mb(J) * DTDYN / AIRDENS )
            ENDIF
#endif

            ! Add [NOx] and [HNO3] to PNOY.
            ! PNOY is now the total [NOy] concentration
            PNOY = PNOY + STT(I,J,L,IDTNOX) + STT(I,J,L,IDTHNO3)

            ! XRATIO is the ratio ( [NO] + [NO2] ) / [NOy],
            ! which is needed for the partitioning.
            ! XRATIO will be the same for a given month
            XRATIO(J,L) = ( STRATNO(J,L) + STRATNO2(J,L) ) /
     &                    ( STRATNO(J,L) + STRATNO2(J,L) + 
     &                      STRATHNO3(J,L) ) 

            ! Partition total [NOy] to [NOx], units are [v/v]
            STT(I,J,L,IDTNOX)  = PNOY * XRATIO(J,L) 

            ! Partition total [NOy] to [HNO3], units are [v/v]
            STT(I,J,L,IDTHNO3) = PNOY * ( 1d0 - XRATIO(J,L) ) 
         ENDDO   
         ENDDO
         ENDDO
!!$OMP END PARALLEL DO

      !=================================================================
      ! IFLAG = 2: After transport
      !
      ! Repartition [NOy] after transport into [NOx] + [HNO3]
      !
      ! This repartitioning is necessary to avoid performing chemistry
      ! between the [NO2] and [HNO3] species.
      !
      ! The concentrations [NOx] and [HNO3] will have changed due to 
      ! transport, but the ratio used to partition them will be the 
      ! same.
      !
      ! Add parallel processor DO-loop (bmy, 12/6/00)
      !=================================================================
      ELSE IF ( IFLAG == 2 ) THEN

! Deactivate parallel processor DO-loops for now (bmy, 12/12/00)
!!$OMP PARALLEL DO
!!$OMP+DEFAULT( SHARED )
!!$OMP+PRIVATE( I, J, L, PNOY )
!!$OMP+SCHEDULE( DYNAMIC )
         DO L = MINVAL( LPAUSE ), LLPAR
         DO J = 1, JJPAR
         DO I = 1, IIPAR

            ! Skip over tropospheric boxes
            IF ( L < LPAUSE(I,J) ) CYCLE
            
            ! Compute the new total [NOy] by summing up [NOx] + [HNO3]
            PNOY = STT(I,J,L,IDTNOX) + STT(I,J,L,IDTHNO3)

            ! Partition total [NOy] to [NOx], units are [v/v]
            STT(I,J,L,IDTNOX)  = PNOY * XRATIO(J,L) 

            ! Partition total [NOy] to [HNO3], units are [v/v]
            STT(I,J,L,IDTHNO3) = PNOY * ( 1d0 - XRATIO(J,L) ) 
         ENDDO   
         ENDDO
         ENDDO   
!!$OMP END PARALLEL DO      


      ELSE 

         ! If IFLAG /= 1 or IFLAG /= 2, print an error message and stop
         CALL ERROR_STOP( 'IFLAG must be 1 or 2!',
     &                    'UPBDFLX_NOY (upbdflx_mod.f)' )

      ENDIF

      ! Return to calling program
      END SUBROUTINE UPBDFLX_NOY 

!------------------------------------------------------------------------------

      END MODULE UPBDFLX_MOD
