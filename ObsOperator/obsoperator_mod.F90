MODULE ObsOperator_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod
  USE ObsOperator_Output_Mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: ObsOperator_Init
  PUBLIC :: ObsOperator_Cleanup
  PUBLIC :: ObsOperator_Sample

  INTEGER, PARAMETER :: MAX_OPERATOR_ENTRIES = 100000

  TYPE(ObsOperatorEntry), ALLOCATABLE, TARGET :: OperatorEntries(:)
  CHARACTER(LEN=255) :: PreviousInputPath
  CHARACTER(LEN=255) :: PreviousOutputPath

  TYPE(ObsOperatorOutputFile) :: CurrentOutputFile
CONTAINS

  SUBROUTINE ObsOperator_Init(Input_Opt, State_Chm, State_Grid, RC)
    USE ErrCode_Mod
    USE File_Mod, ONLY : FILE_EXISTS
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState
    USE Time_Mod, ONLY : Expand_Date, Get_Nymd, Get_Nhms
    USE ObsOperator_Input_Alt_Mod, ONLY : Read_ObsOperator_Input

    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC

    LOGICAL :: isPrintLog, isOpen
    CHARACTER(LEN=255) :: errorMessage, thisLocation, currentInputPath, currentOutputPath

    INTEGER :: nActiveEntries, I, ioStat

    RC =  GC_SUCCESS
    errorMessage = ''
    thisLocation = ' -> at ObsOperator_Init (in module ObsOperator/obsoperator_mod.F90)'
    isPrintLog = (Input_Opt%amIRoot .and. Input_Opt%ObsOperator_Verbose )

    IF (.NOT. ALLOCATED(OperatorEntries)) THEN
      ALLOCATE(OperatorEntries(MAX_OPERATOR_ENTRIES))
    END IF

    currentInputPath = Input_Opt%ObsOperator_InputFile
    CALL Expand_Date(currentInputPath, Get_Nymd(), Get_Nhms())

    currentOutputPath = Input_Opt%ObsOperator_OutputFile
    CALL Expand_Date(currentOutputPath, Get_Nymd(), Get_Nhms())

    IF ( currentInputPath == PreviousInputPath ) THEN
      ! The input file has not changed, so we can skip reading it
      RETURN
    ENDIF
    PreviousInputPath = currentInputPath

    IF ( FILE_EXISTS(currentInputPath) ) THEN
      CALL Read_ObsOperator_Input(currentInputPath, Input_Opt, State_Chm, State_Grid, OperatorEntries, RC)
      IF (RC /= GC_SUCCESS) THEN
        RETURN
      END IF
    END IF

    nActiveEntries = 0
    DO I = 1, MAX_OPERATOR_ENTRIES
      IF (OperatorEntries(I)%IsActive) THEN
        nActiveEntries = nActiveEntries + 1
      END IF
    END DO

    IF ( currentOutputPath /= PreviousOutputPath .AND. nActiveEntries > 0 ) THEN
      IF (CurrentOutputFile%FileId >= 0) THEN
        CALL Finalize_ObsOperator_Output(CurrentOutputFile)
      END IF
      ! File will be created on write sample, if any occur
      CurrentOutputFile%Path = currentOutputPath
    END IF
    PreviousOutputPath = currentOutputPath

    IF ( isPrintLog ) THEN

       ! Print info
      WRITE( 6, '(/,a)' ) REPEAT( '=', 79 )
      WRITE( 6, 100     ) Get_Nymd()
      WRITE( 6, 110     ) TRIM( currentInputPath  )
      WRITE( 6, 130     ) nActiveEntries
      WRITE( 6, '(a,/)' ) REPEAT( '=', 79 )

      ! FORMAT statements
100   FORMAT( 'ObsOperator for date ',  i8  )
110   FORMAT( '-> Input file      : ', a   )
130   FORMAT( '-> # active entries: ', i10 )
    END IF
  END SUBROUTINE ObsOperator_Init

  SUBROUTINE ObsOperator_Cleanup(State_Chm)
    USE State_Chm_Mod, ONLY : ChmState

    TYPE(ChmState), INTENT(IN) :: State_Chm

    INTEGER :: I

    ! Write out any unwritten entries
    DO I = 1, MAX_OPERATOR_ENTRIES
      IF (OperatorEntries(I)%IsActive) THEN
        CALL ObsOperator_Finalize_Entry(OperatorEntries(I), State_Chm)
      END IF
    END DO

    IF (ALLOCATED(OperatorEntries)) THEN
      DEALLOCATE(OperatorEntries)
    END IF

    IF (CurrentOutputFile%FileId >= 0) THEN
      CALL Finalize_ObsOperator_Output(CurrentOutputFile)
    END IF
  END SUBROUTINE ObsOperator_Cleanup

  ! Loop through each active entry and sample the value
  SUBROUTINE ObsOperator_Sample(Input_Opt, State_Chm, State_Grid, State_Met, RC)
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState
    USE State_Met_Mod, ONLY : MetState
    USE Time_Mod, ONLY : GET_ELAPSED_SEC, GET_TS_DYN
    USE ObsOperator_Sample_Mod, ONLY : Sample_ObsOperatorEntry

    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    TYPE(MetState), INTENT(IN) :: State_Met
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: I
    INTEGER :: J
    INTEGER :: currentTime
    INTEGER :: maxOperatorTimeIndex
    INTEGER :: ioStat
    LOGICAL :: isPrintLog

    isPrintLog = (Input_Opt%amIRoot .and. Input_Opt%ObsOperator_Verbose )
    currentTime = INT( GET_ELAPSED_SEC() / GET_TS_DYN() )

    IF (isPrintLog) THEN
      WRITE(*, '(A, I0)') '--------------------------------'
      WRITE(*, '(A, I0)') 'ObsOperator sampling time index = ', currentTime
    END IF

    DO I = 1, MAX_OPERATOR_ENTRIES
      IF (.NOT. OperatorEntries(I)%IsActive) THEN
        CYCLE
      END IF

      CALL Sample_ObsOperatorEntry( &
        OperatorEntries(I), &
        currentTime, &
        Input_Opt, &
        State_Chm, &
        State_Grid, &
        State_Met &
      )

      maxOperatorTimeIndex = MAXVAL(OperatorEntries(I)%TimeIndices)
      IF (currentTime >= maxOperatorTimeIndex) THEN
        CALL ObsOperator_Finalize_Entry(OperatorEntries(I), State_Chm)
      END IF
    END DO

    IF (isPrintLog) THEN
      WRITE(*, '(A, I0)') '--------------------------------'
    ENDIF
  END SUBROUTINE ObsOperator_Sample

  SUBROUTINE ObsOperator_Finalize_Entry(Entry, State_Chm)
    USE ErrCode_Mod
    USE State_Chm_Mod, ONLY : ChmState
    USE Error_Mod, ONLY : Error_Stop
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(ChmState), INTENT(IN) :: State_Chm

    CALL Write_ObsOperator_Entry(CurrentOutputFile, Entry)
    CALL Deactivate_ObsOperatorEntry(Entry)
  END SUBROUTINE ObsOperator_Finalize_Entry
END MODULE ObsOperator_Mod

