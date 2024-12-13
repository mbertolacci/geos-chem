MODULE ObsOperator_Input_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Read_ObsOperator_Input

  PRIVATE :: Trim_Comment
  PRIVATE :: String_Count
  PRIVATE :: Read_Species
  PRIVATE :: Read_Time_Operator
  PRIVATE :: Read_Horizontal_Operator
  PRIVATE :: Read_Vertical_Operator

  CONTAINS

  SUBROUTINE Read_ObsOperator_Input(Path, Input_Opt, State_Chm, State_Grid, ObsOperator_Entries, RC)
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState

    CHARACTER(LEN=*), INTENT(IN) :: Path
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ObsOperator_Entries(:)
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: errorMessage, thisLocation

    WRITE(*, '(A)') 'First pass starting'
    CALL Read_ObsOperator_Input_First_Pass(Path, ObsOperator_Entries, RC)
    IF (RC /= GC_SUCCESS) THEN
      RETURN
    END IF

    WRITE(*, '(A)') 'Second pass starting'
    CALL Read_ObsOperator_Input_Second_Pass(Path, Input_Opt, State_Chm, State_Grid, ObsOperator_Entries, RC)
    IF (RC /= GC_SUCCESS) THEN
      RETURN
    END IF
  END SUBROUTINE Read_ObsOperator_Input

  SUBROUTINE Read_ObsOperator_Input_First_Pass(Path, ObsOperator_Entries, RC)
    USE ErrCode_Mod
    USE inquireMod, ONLY : findFreeLUN
    USE File_Mod, ONLY : IOERROR

    CHARACTER(LEN=*), INTENT(IN) :: Path
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ObsOperator_Entries(:)
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: errorMessage, thisLocation

    INTEGER :: maxEntries, currentIdLength, colonIndex, currentEntryIndex, inputFile, ioStat
    CHARACTER(LEN=511) :: line

    inputFile = findFreeLUN()
    OPEN( inputFile, FILE=Path, IOSTAT=ioStat )
    IF (ioStat /= 0) THEN
      CALL IOERROR(ioStat, inputFile, 'Read_ObsOperator_Input_First_Pass:1')
    END IF

    maxEntries = SIZE(ObsOperator_Entries)

    currentEntryIndex = 1
    DO
      READ(inputFile, '(A511)', IOSTAT=ioStat, END=999) line
      IF (ioStat /= 0) THEN
        CALL IOERROR(ioStat, inputFile, 'Read_ObsOperator_Input_First_Pass:2')
      END IF

      CALL Trim_Comment(line, '#;')

      IF ( line == "" ) CYCLE

      IF ( line(1:1) /= " " ) THEN
        ! This is a new entry
        DO WHILE (ObsOperator_Entries(currentEntryIndex)%IsActive)
          currentEntryIndex = currentEntryIndex + 1
          IF (currentEntryIndex > maxEntries) THEN
            errorMessage = 'Error: too many operator entries. You can increase this in obsoperator_mod.F90'
            CALL GC_Error(errorMessage, RC, thisLocation)
            RETURN
          END IF
        END DO

        colonIndex = INDEX(line, ":")
        IF (colonIndex == 0) THEN
          errorMessage = 'Error: invalid format for operator entry'
          CALL GC_Error(errorMessage, RC, thisLocation)
          RETURN
        END IF

        currentIdLength = LEN_TRIM(line(1:colonIndex-1))
        ALLOCATE(CHARACTER(LEN=currentIdLength) :: ObsOperator_Entries(currentEntryIndex)%Id)

        ObsOperator_Entries(currentEntryIndex)%Id = line(1:currentIdLength)
        ObsOperator_Entries(currentEntryIndex)%IsActive = .TRUE.
        ObsOperator_Entries(currentEntryIndex)%IsFilled = .FALSE.
      END IF
    END DO

999 CLOSE(inputFile)
  END SUBROUTINE Read_ObsOperator_Input_First_Pass

  SUBROUTINE Read_ObsOperator_Input_Second_Pass(Path, Input_Opt, State_Chm, State_Grid, ObsOperator_Entries, RC)
    USE ErrCode_Mod
    USE QFYAML_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState

    CHARACTER(LEN=*), INTENT(IN) :: Path
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ObsOperator_Entries(:)
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: errorMessage
    CHARACTER(LEN=255) :: thisLocation

    INTEGER :: I, currentIdLength
    CHARACTER(LEN=511) :: line
    TYPE(QFYAML_t) :: yml
    TYPE(QFYAML_t) :: yml_anchored

    INTEGER :: startClock, endClock, clockRate
    REAL :: elapsedTime

    thisLocation = " -> at Read_ObsOperator_Input_Second_Pass (in module ObsOperator/obsoperator_input_mod.F90)"

    CALL SYSTEM_CLOCK(startClock, clockRate)
    WRITE(*, '(A)') 'QFYAML_Init starting'
    CALL QFYAML_Init( Path, yml, yml_anchored, RC )
    IF ( RC /= GC_SUCCESS ) THEN
      errorMessage = "Error reading from " // Path
      CALL GC_Error( errorMessage, RC, thisLocation )
      RETURN
    ENDIF
    WRITE(*, '(A)') 'QFYAML_Init finished'
    CALL SYSTEM_CLOCK(endClock)
    elapsedTime = REAL(endClock - startClock) / REAL(clockRate)
    WRITE(*, '(A, F6.3, A)') 'QFYAML_Init finished in ', elapsedTime, ' seconds'

    CALL SYSTEM_CLOCK(startClock, clockRate)
    WRITE(*, '(A)') 'Reading operators'
    DO I = 1, SIZE(ObsOperator_Entries)
      IF (.NOT. ObsOperator_Entries(I)%IsActive .OR. ObsOperator_Entries(I)%IsFilled) THEN
        CYCLE
      END IF

      CALL Read_Species(ObsOperator_Entries(I), yml, State_Chm, RC)
      IF ( RC /= GC_SUCCESS ) THEN
        errorMessage = 'Error reading species for operator entry ' // TRIM(ObsOperator_Entries(I)%Id)
        CALL GC_Error( errorMessage, RC, thisLocation )
        RETURN
      ENDIF

      CALL Read_Time_Operator(ObsOperator_Entries(I), yml, Input_Opt, RC)
      IF ( RC /= GC_SUCCESS ) THEN
        errorMessage = 'Error reading time operator for operator entry ' // TRIM(ObsOperator_Entries(I)%Id)
        CALL GC_Error( errorMessage, RC, thisLocation )
        RETURN
      ENDIF

      CALL Read_Horizontal_Operator(ObsOperator_Entries(I), yml, State_Grid, RC)
      IF ( RC /= GC_SUCCESS ) THEN
        errorMessage = 'Error reading horizontal operator for operator entry ' // TRIM(ObsOperator_Entries(I)%Id)
        CALL GC_Error( errorMessage, RC, thisLocation )
        RETURN
      ENDIF

      CALL Read_Vertical_Operator(ObsOperator_Entries(I), yml, RC)
      IF ( RC /= GC_SUCCESS ) THEN
        errorMessage = 'Error reading vertical operator for operator entry ' // TRIM(ObsOperator_Entries(I)%Id)
        CALL GC_Error( errorMessage, RC, thisLocation )
        RETURN
      ENDIF

      ObsOperator_Entries(I)%IsFilled = .TRUE.
    END DO
    WRITE(*, '(A)') 'Reading operators finished'
    CALL SYSTEM_CLOCK(endClock)
    elapsedTime = REAL(endClock - startClock) / REAL(clockRate)
    WRITE(*, '(A, F6.3, A)') 'Reading operators finished in ', elapsedTime, ' seconds'

    WRITE(*, '(A)') 'QFYAML_CleanUp starting'
    CALL SYSTEM_CLOCK(startClock, clockRate)
    CALL QFYAML_CleanUp(yml_anchored)
    CALL QFYAML_CleanUp(yml)
    CALL SYSTEM_CLOCK(endClock)
    elapsedTime = REAL(endClock - startClock) / REAL(clockRate)
    WRITE(*, '(A, F6.3, A)') 'QFYAML_CleanUp finished in ', elapsedTime, ' seconds'
  END SUBROUTINE Read_ObsOperator_Input_Second_Pass

  SUBROUTINE Read_Species(Entry, yml, State_Chm, RC)
    USE QFYAML_Mod
    USE State_Chm_Mod,  ONLY : ChmState

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    TYPE(ChmState), INTENT(IN) :: State_Chm
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=QFYAML_StrLen) :: a_str(QFYAML_MaxArr)
    INTEGER :: nSpecies
    INTEGER :: I
    INTEGER :: J

    a_str = MISSING_STR
    CALL QFYAML_Add_Get( &
      yml, &
      Entry%Id // "%species", &
      a_str, &
      "", &
      RC, &
      dynamic_size=.TRUE. &
    )
    IF (RC /= QFYAML_Success) THEN
      RETURN
    END IF

    nSpecies = String_Count(a_str)
    ALLOCATE(Entry%SpeciesIndex(nSpecies))
    ALLOCATE(Entry%SpeciesValue(nSpecies))
    DO I = 1, nSpecies
      DO J = 1, State_Chm%nSpecies
        IF (TRIM(State_Chm%SpcData(J)%Info%Name) == TRIM(a_str(I))) THEN
          Entry%SpeciesIndex(I) = J
          EXIT
        END IF
      END DO
      Entry%SpeciesValue(I) = 0.0_f8
    END DO
  END SUBROUTINE Read_Species

  SUBROUTINE Read_Time_Operator(Entry, yml, Input_Opt, RC)
    USE QFYAML_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE Error_Mod, ONLY : Error_Stop

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    INTEGER, INTENT(OUT) :: RC

    INTEGER, PARAMETER :: TIME_OPERATOR_RANGE = 0
    INTEGER, PARAMETER :: TIME_OPERATOR_POINT = 1
    INTEGER, PARAMETER :: TIME_OPERATOR_EXACT = 2

    CHARACTER(LEN=QFYAML_StrLen) :: v_str
    INTEGER :: timeOperatorType
    LOGICAL :: isUnitTimeIndex
    LOGICAL :: normalizeWeights
    INTEGER :: I, J
    INTEGER :: timeIndex1
    INTEGER :: timeIndex2
    INTEGER :: nTimeIndices

    ! Read type
    CALL QFYAML_Add_Get( &
      yml, &
      Entry%Id // "%time_operator%type", &
      v_str, &
      "", &
      RC &
    )
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading time operator type", "" )
      RETURN
    END IF

    IF (TRIM(v_str) == "range") THEN
      timeOperatorType = TIME_OPERATOR_RANGE
    ELSE IF (TRIM(v_str) == "point") THEN
      timeOperatorType = TIME_OPERATOR_POINT
    ELSE IF (TRIM(v_str) == "exact") THEN
      timeOperatorType = TIME_OPERATOR_EXACT
    ELSE
      CALL Error_Stop( "Invalid time operator type", "" )
      RETURN
    END IF

    ! Read unit
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( &
      yml, &
      Entry%Id // "%time_operator%unit", &
      v_str, &
      "", &
      RC &
    )
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading time operator unit", "" )
      RETURN
    END IF

    isUnitTimeIndex = .FALSE.
    IF (v_str /= MISSING_STR) THEN
      IF (TRIM(v_str) == "time_index") THEN
        isUnitTimeIndex = .TRUE.
      ELSE IF (TRIM(v_str) == "time") THEN
        isUnitTimeIndex = .FALSE.
      ELSE
        CALL Error_Stop( "Invalid time operator unit", "" )
        RETURN
      END IF
    END IF

    IF (timeOperatorType == TIME_OPERATOR_RANGE) THEN
      IF (isUnitTimeIndex) THEN
        CALL QFYAML_Add_Get(yml, Entry%Id // "%time_operator%start", timeIndex1, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator start", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%time_operator%end", timeIndex2, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator end", "" )
          RETURN
        END IF
      ELSE
        CALL Read_Time_Entry(yml, Entry%Id // "%time_operator%start", Input_Opt, RC, timeIndex1)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator start", "" )
          RETURN
        END IF
        CALL Read_Time_Entry(yml, Entry%Id // "%time_operator%end", Input_Opt, RC, timeIndex2)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator end", "" )
          RETURN
        END IF
      END IF

      IF (timeIndex1 > timeIndex2) THEN
        CALL Error_Stop( "Time operator start is greater than end", "" )
        RETURN
      END IF

      v_str = MISSING_STR
      CALL QFYAML_Add_Get(yml, Entry%Id // "%time_operator%weights", v_str, "", RC)
      IF (RC /= QFYAML_Success) THEN
        CALL Error_Stop( "Error reading time operator weights", "" )
        RETURN
      END IF

      normalizeWeights = .TRUE.
      IF (v_str /= MISSING_STR) THEN
        IF (v_str == "equal") THEN
          normalizeWeights = .FALSE.
        ELSE IF (v_str == "normalized") THEN
          normalizeWeights = .TRUE.
        ELSE
          CALL Error_Stop( "Invalid time operator weighting type", "" )
          RETURN
        END IF
      END IF

      nTimeIndices = timeIndex2 - timeIndex1 + 1
      ALLOCATE(Entry%TimeIndices(nTimeIndices))
      ALLOCATE(Entry%TimeWeights(nTimeIndices))
      DO J = 1, nTimeIndices
        Entry%TimeIndices(J) = timeIndex1 + J - 1
        IF (normalizeWeights) THEN
          Entry%TimeWeights(J) = 1.0_f8 / nTimeIndices
        ELSE
          Entry%TimeWeights(J) = 1.0_f8
        END IF
      END DO
    ELSE IF (timeOperatorType == TIME_OPERATOR_POINT) THEN
      IF (isUnitTimeIndex) THEN
        CALL QFYAML_Add_Get(yml, Entry%Id // "%time_operator%time", timeIndex1, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator index", "" )
          RETURN
        END IF
      ELSE
        CALL Read_Time_Entry(yml, Entry%Id // "%time_operator%time", Input_Opt, RC, timeIndex1)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading time operator time", "" )
          RETURN
        END IF
      END IF

      ALLOCATE(Entry%TimeIndices(1))
      ALLOCATE(Entry%TimeWeights(1))
      Entry%TimeIndices(1) = timeIndex1
      Entry%TimeWeights(1) = 1.0_f8
    ELSE IF (timeOperatorType == TIME_OPERATOR_EXACT) THEN
      CALL Error_Stop( "Exact time operator is not implemented", "" )
      RETURN
    ELSE
      CALL Error_Stop( "Invalid time operator type", "" )
      RETURN
    END IF
  END SUBROUTINE Read_Time_Operator

  SUBROUTINE Read_Horizontal_Operator(Entry, yml, State_Grid, RC)
    USE QFYAML_Mod
    USE State_Grid_Mod, ONLY : GrdState
    USE Error_Mod, ONLY : Error_Stop

    INTEGER, PARAMETER :: HORIZONTAL_OPERATOR_BOX = 0
    INTEGER, PARAMETER :: HORIZONTAL_OPERATOR_POINT = 1
    INTEGER, PARAMETER :: HORIZONTAL_OPERATOR_EXACT = 2

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=QFYAML_StrLen) :: v_str
    LOGICAL :: isUnitGridIndex
    INTEGER :: horizontalOperatorType
    INTEGER :: boxLowerI, boxUpperI, boxLowerJ, boxUpperJ
    LOGICAL :: isAreaWeights
    LOGICAL :: isNormalizedWeights
    INTEGER :: nHorizontalIndices
    INTEGER :: I, J, currentIndex

    CALL QFYAML_Add_Get( &
      yml, &
      Entry%Id // "%horizontal_operator%type", &
      v_str, &
      "", &
      RC &
    )
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading horizontal operator type", "" )
      RETURN
    END IF

    IF (TRIM(v_str) == "box") THEN
      horizontalOperatorType = HORIZONTAL_OPERATOR_BOX
    ELSE IF (TRIM(v_str) == "point") THEN
      horizontalOperatorType = HORIZONTAL_OPERATOR_POINT
    ELSE IF (TRIM(v_str) == "exact") THEN
      horizontalOperatorType = HORIZONTAL_OPERATOR_EXACT
    ELSE
      CALL Error_Stop( "Invalid horizontal operator type", "" )
      RETURN
    END IF

    ! Read unit
    v_str = MISSING_STR
    CALL QFYAML_Add_Get( &
      yml, &
      Entry%Id // "%horizontal_operator%unit", &
      v_str, &
      "", &
      RC &
    )
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading horizontal operator unit", "" )
      RETURN
    END IF

    isUnitGridIndex = .FALSE.
    IF (v_str /= MISSING_STR) THEN
      IF (TRIM(v_str) == "grid_index") THEN
        isUnitGridIndex = .TRUE.
      ELSE IF (TRIM(v_str) == "degrees") THEN
        isUnitGridIndex = .FALSE.
      ELSE
        CALL Error_Stop( "Invalid horizontal operator unit", "" )
        RETURN
      END IF
    END IF

    IF (horizontalOperatorType == HORIZONTAL_OPERATOR_BOX) THEN
      IF (isUnitGridIndex) THEN
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%longitude_start", boxLowerI, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude start", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%longitude_end", boxUpperI, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude end", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%latitude_start", boxLowerJ, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude start", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%latitude_end", boxUpperJ, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude end", "" )
          RETURN
        END IF
      ELSE
        CALL Read_Longitude_Entry(yml, Entry%Id // "%horizontal_operator%longitude_start", State_Grid, RC, boxLowerI)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude start", "" )
          RETURN
        END IF
        CALL Read_Longitude_Entry(yml, Entry%Id // "%horizontal_operator%longitude_end", State_Grid, RC, boxUpperI)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude end", "" )
          RETURN
        END IF
        CALL Read_Latitude_Entry(yml, Entry%Id // "%horizontal_operator%latitude_start", State_Grid, RC, boxLowerJ)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude start", "" )
          RETURN
        END IF
        CALL Read_Latitude_Entry(yml, Entry%Id // "%horizontal_operator%latitude_end", State_Grid, RC, boxUpperJ)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude end", "" )
          RETURN
        END IF
      END IF

      v_str = MISSING_STR
      CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%weights", v_str, "", RC)
      IF (RC /= QFYAML_Success) THEN
        CALL Error_Stop( "Error reading horizontal operator weights", "" )
        RETURN
      END IF

      isAreaWeights = .TRUE.
      isNormalizedWeights = .TRUE.
      IF (v_str == "area") THEN
        isAreaWeights = .TRUE.
        isNormalizedWeights = .FALSE.
      ELSE IF (v_str == "normalized_area") THEN
        isAreaWeights = .TRUE.
        isNormalizedWeights = .TRUE.
      ELSE IF (v_str == "normalized") THEN
        isAreaWeights = .FALSE.
        isNormalizedWeights = .TRUE.
      ELSE IF (v_str == "equal") THEN
        isAreaWeights = .FALSE.
        isNormalizedWeights = .FALSE.
      ELSE IF (v_str /= MISSING_STR) THEN
        CALL Error_Stop( "Invalid horizontal operator weights", "" )
        RETURN
      END IF

      IF (boxLowerI > boxUpperI .OR. boxLowerJ > boxUpperJ) THEN
        CALL Error_Stop( "Invalid horizontal operator box", "" )
        RETURN
      END IF

      nHorizontalIndices = (boxUpperI - boxLowerI + 1) * (boxUpperJ - boxLowerJ + 1)
      ALLOCATE(Entry%HorizontalIndices(nHorizontalIndices, 2))
      ALLOCATE(Entry%HorizontalWeights(nHorizontalIndices))
      currentIndex = 1
      DO I = boxLowerI, boxUpperI
        DO J = boxLowerJ, boxUpperJ
          Entry%HorizontalIndices(currentIndex, 1) = I
          Entry%HorizontalIndices(currentIndex, 2) = J
          IF (isAreaWeights) THEN
            Entry%HorizontalWeights(currentIndex) = State_Grid%AREA_M2(I, J)
          ELSE
            Entry%HorizontalWeights(currentIndex) = 1.0_f8
          END IF
          currentIndex = currentIndex + 1
        END DO
      END DO

      IF (isNormalizedWeights) THEN
        Entry%HorizontalWeights = Entry%HorizontalWeights / SUM(Entry%HorizontalWeights)
      END IF
    ELSE IF (horizontalOperatorType == HORIZONTAL_OPERATOR_POINT) THEN
      IF (isUnitGridIndex) THEN
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%longitude", boxLowerI, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%horizontal_operator%latitude", boxLowerJ, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude", "" )
          RETURN
        END IF
      ELSE
        CALL Read_Longitude_Entry(yml, Entry%Id // "%horizontal_operator%longitude", State_Grid, RC, boxLowerI)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator longitude", "" )
          RETURN
        END IF
        CALL Read_Latitude_Entry(yml, Entry%Id // "%horizontal_operator%latitude", State_Grid, RC, boxLowerJ)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading horizontal operator latitude", "" )
          RETURN
        END IF
      END IF

      ALLOCATE(Entry%HorizontalIndices(1, 2))
      ALLOCATE(Entry%HorizontalWeights(1))
      Entry%HorizontalIndices(1, 1) = boxLowerI
      Entry%HorizontalIndices(1, 2) = boxLowerJ
      Entry%HorizontalWeights(1) = 1.0_f8
    ELSE IF (horizontalOperatorType == HORIZONTAL_OPERATOR_EXACT) THEN
      CALL Error_Stop( "Exact horizontal operator is not implemented", "" )
      RETURN
    END IF
  END SUBROUTINE Read_Horizontal_Operator

  SUBROUTINE Read_Vertical_Operator(Entry, yml, RC)
    USE QFYAML_Mod
    USE Error_Mod, ONLY : Error_Stop

    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_POINT = 0
    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_RANGE = 1
    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_EXACT = 2

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=QFYAML_StrLen) :: v_str
    REAL(yp) :: v_real
    INTEGER :: v_int
    INTEGER :: a_int(QFYAML_MaxArr)
    REAL(yp) :: a_real(QFYAML_MaxArr)

    INTEGER :: verticalOperatorType
    INTEGER :: verticalOperatorUnit
    INTEGER :: nVerticalExact
    INTEGER :: nVerticalWeights

    ! Read type
    v_str = MISSING_STR
    CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%type", v_str, "", RC)
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading vertical operator type", "" )
      RETURN
    END IF

    IF (v_str == "point") THEN
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_POINT
    ELSE IF (v_str == "range") THEN
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_RANGE
    ELSE IF (v_str == "exact") THEN
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_EXACT
    ELSE
      CALL Error_Stop( "Invalid vertical operator type", "" )
      RETURN
    END IF

    ! Read unit
    v_str = MISSING_STR
    CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%unit", v_str, "", RC)
    IF (RC /= QFYAML_Success) THEN
      CALL Error_Stop( "Error reading vertical operator unit", "" )
      RETURN
    END IF

    IF (v_str == "pressure") THEN
      verticalOperatorUnit = VERTICAL_UNIT_PRESSURE
    ELSE IF (v_str == "altitude") THEN
      verticalOperatorUnit = VERTICAL_UNIT_ALTITUDE
    ELSE IF (v_str == "pressure_level") THEN
      verticalOperatorUnit = VERTICAL_UNIT_PRESSURE_LEVEL
    ELSE
      CALL Error_Stop( "Invalid vertical operator unit", "" )
      RETURN
    END IF

    Entry%VerticalUnit = verticalOperatorUnit

    IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_RANGE) THEN
      Entry%VerticalOperatorType = VERTICAL_OPERATOR_TYPE_RANGE

      v_str = MISSING_STR
      CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%weights", v_str, "", RC)
      IF (RC /= QFYAML_Success) THEN
        CALL Error_Stop( "Error reading vertical operator range start", "" )
        RETURN
      END IF

      Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE
      IF (v_str == "normalized_pressure") THEN
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE
      ELSE IF (v_str == "pressure") THEN
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_PRESSURE
      ELSE IF (v_str == "normalized") THEN
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_NORMALIZED
      ELSE IF (v_str == "equal") THEN
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_EQUAL
      ELSE IF (v_str /= MISSING_STR) THEN
        CALL Error_Stop( "Invalid vertical operator weights", "" )
        RETURN
      END IF

      IF (verticalOperatorUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
        CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%start", Entry%VerticalRangeStartIndex, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading vertical operator range start", "" )
          RETURN
        END IF
        CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%end", Entry%VerticalRangeEndIndex, "", RC)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading vertical operator range end", "" )
          RETURN
        END IF

        IF (Entry%VerticalRangeStartIndex > Entry%VerticalRangeEndIndex) THEN
          CALL Error_Stop( "Vertical operator range start is greater than end", "" )
          RETURN
        END IF
      ELSE
        v_real = MISSING_REAL
        CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%start", v_real, "", RC)
        IF (RC /= QFYAML_Success .OR. v_real == MISSING_REAL) THEN
          CALL Error_Stop( "Error reading vertical operator range start", "" )
          RETURN
        END IF
        Entry%VerticalRangeStart = DBLE(v_real)
        v_real = MISSING_REAL
        CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%end", v_real, "", RC)
        IF (RC /= QFYAML_Success .OR. v_real == MISSING_REAL) THEN
          CALL Error_Stop( "Error reading vertical operator range end", "" )
          RETURN
        END IF
        Entry%VerticalRangeEnd = DBLE(v_real)

        IF (Entry%VerticalRangeStart > Entry%VerticalRangeEnd) THEN
          CALL Error_Stop( "Vertical operator range start is greater than end", "" )
          RETURN
        END IF
      END IF
    ELSE
      Entry%VerticalOperatorType = VERTICAL_OPERATOR_TYPE_EXACT

      IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_POINT) THEN
        IF (verticalOperatorUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
          v_int = MISSING_INT
          CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%value", v_int, "", RC)
          IF (RC /= QFYAML_Success .OR. v_int == MISSING_INT) THEN
            CALL Error_Stop( "Error reading vertical operator value", "" )
            RETURN
          END IF

          ALLOCATE(Entry%VerticalExactIndex(1))
          Entry%VerticalExactIndex(1) = v_int
        ELSE
          v_real = MISSING_REAL
          CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%value", v_real, "", RC)
          IF (RC /= QFYAML_Success .OR. v_real == MISSING_REAL) THEN
            CALL Error_Stop( "Error reading vertical operator value", "" )
            RETURN
          END IF

          ALLOCATE(Entry%VerticalExact(1))
          Entry%VerticalExact(1) = DBLE(v_real)
        END IF
        ALLOCATE(Entry%VerticalExactWeight(1))
        Entry%VerticalExactWeight(1) = 1.0_f8
      ELSE IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_EXACT) THEN
        IF (verticalOperatorUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
          a_int = MISSING_INT
          CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%values", a_int, "", RC, dynamic_size=.TRUE.)
          IF (RC /= QFYAML_Success) THEN
            CALL Error_Stop( "Error reading vertical operator values", "" )
            RETURN
          END IF

          nVerticalExact = Find_Location_Integer(a_int, MISSING_INT) - 1
          IF (nVerticalExact == 0) THEN
            CALL Error_Stop( "No vertical operator values", "" )
            RETURN
          END IF

          ALLOCATE(Entry%VerticalExactIndex(nVerticalExact))
          Entry%VerticalExactIndex = a_int(1:nVerticalExact)
        ELSE
          a_real = MISSING_REAL
          CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%values", a_real, "", RC, dynamic_size=.TRUE.)
          IF (RC /= QFYAML_Success) THEN
            CALL Error_Stop( "Error reading vertical operator values", "" )
            RETURN
          END IF

          nVerticalExact = Find_Location_Real(a_real, MISSING_REAL) - 1
          IF (nVerticalExact == 0) THEN
            CALL Error_Stop( "No vertical operator values", "" )
            RETURN
          END IF

          ALLOCATE(Entry%VerticalExact(nVerticalExact))
          Entry%VerticalExact = a_real(1:nVerticalExact)
        END IF

        a_real = MISSING_REAL
        CALL QFYAML_Add_Get(yml, Entry%Id // "%vertical_operator%weights", a_real, "", RC, dynamic_size=.TRUE.)
        IF (RC /= QFYAML_Success) THEN
          CALL Error_Stop( "Error reading vertical operator weights", "" )
          RETURN
        END IF

        nVerticalWeights = Find_Location_Real(a_real, MISSING_REAL) - 1
        IF (nVerticalWeights /= nVerticalExact) THEN
          CALL Error_Stop( "Number of vertical operator weights does not match number of values", "" )
          RETURN
        END IF

        ALLOCATE(Entry%VerticalExactWeight(nVerticalWeights))
        Entry%VerticalExactWeight = a_real(1:nVerticalWeights)
      END IF
    END IF
  END SUBROUTINE Read_Vertical_Operator

  SUBROUTINE Read_Time_Entry(yml, key, Input_Opt, RC, timeIndex)
    USE QFYAML_Mod
    USE Input_Opt_Mod, ONLY : OptInput

    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    CHARACTER(LEN=*), INTENT(IN) :: key
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    INTEGER, INTENT(OUT) :: RC
    INTEGER, INTENT(OUT) :: timeIndex

    INTEGER :: time(2)

    time = MISSING_INT
    CALL QFYAML_Add_Get(yml, key, time, "", RC)
    IF (RC /= QFYAML_Success .OR. ANY(time == MISSING_INT)) THEN
      RETURN
    END IF

    CALL Convert_Time_To_Index(time, Input_Opt, timeIndex)
  END SUBROUTINE Read_Time_Entry

  SUBROUTINE Read_Longitude_Entry(yml, key, State_Grid, RC, longitudeIndex)
    USE QFYAML_Mod
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    CHARACTER(LEN=*), INTENT(IN) :: key
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC
    INTEGER, INTENT(OUT) :: longitudeIndex

    REAL(yp) :: longitude

    longitude = MISSING_REAL
    CALL QFYAML_Add_Get(yml, key, longitude, "", RC)
    IF (RC /= QFYAML_Success .OR. longitude == MISSING_REAL) THEN
      RETURN
    END IF

    CALL Convert_Longitude_To_Index(DBLE(longitude), State_Grid, longitudeIndex)
  END SUBROUTINE Read_Longitude_Entry

  SUBROUTINE Read_Latitude_Entry(yml, key, State_Grid, RC, latitudeIndex)
    USE QFYAML_Mod
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(QFYAML_t), INTENT(INOUT) :: yml
    CHARACTER(LEN=*), INTENT(IN) :: key
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC
    INTEGER, INTENT(OUT) :: latitudeIndex

    REAL(yp) :: latitude

    latitude = MISSING_REAL
    CALL QFYAML_Add_Get(yml, key, latitude, "", RC)
    IF (RC /= QFYAML_Success .OR. latitude == MISSING_REAL) THEN
      RETURN
    END IF

    CALL Convert_Latitude_To_Index(DBLE(latitude), State_Grid, latitudeIndex)
  END SUBROUTINE Read_Latitude_Entry

  SUBROUTINE Convert_Time_To_Index(time, Input_Opt, timeIndex)
    USE Input_Opt_Mod, ONLY : OptInput
    USE Time_Mod, ONLY : GET_JD, GET_TS_DYN

    INTEGER, INTENT(IN) :: time(2)
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    INTEGER, INTENT(OUT) :: timeIndex
    REAL(fp) :: simStartJulian, timeJulian

    simStartJulian = GET_JD(Input_Opt%NymdB, Input_Opt%NhmsB)
    timeJulian = GET_JD(time(1), 100 * time(2))
    timeIndex = FLOOR((timeJulian - simStartJulian) * 86400_f8 / GET_TS_DYN())
  END SUBROUTINE Convert_Time_To_Index

  SUBROUTINE Convert_Longitude_To_Index(longitude, State_Grid, gridIndex)
    USE State_Grid_Mod, ONLY : GrdState

    REAL(fp), INTENT(IN) :: longitude
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: gridIndex

    gridIndex = INT( ( longitude + 180e+0_fp - &
      ( State_Grid%XMinOffset * State_Grid%DX ) ) / State_Grid%DX + 1.5e+0_fp )

    IF (gridIndex > State_Grid%NX) THEN
      gridIndex = gridIndex - State_Grid%NX
    END IF
  END SUBROUTINE Convert_Longitude_To_index

  SUBROUTINE Convert_Latitude_To_Index(latitude, State_Grid, gridIndex)
    USE State_Grid_Mod, ONLY : GrdState

    REAL(fp), INTENT(IN) :: latitude
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: gridIndex

    gridIndex = INT( ( latitude + 90e+0_fp - &
      ( State_Grid%YMinOffset * State_Grid%DY ) ) / State_Grid%DY + 1.5e+0_fp )
  END SUBROUTINE Convert_Latitude_To_index

  FUNCTION Find_Location_Integer(a, b) RESULT(index)
    INTEGER, INTENT(IN) :: a(:)
    INTEGER, INTENT(IN) :: b
    INTEGER :: index

    DO index = 1, SIZE(a)
      IF (a(index) == b) THEN
        RETURN
      END IF
    END DO
  END FUNCTION Find_Location_Integer

  FUNCTION Find_Location_Real(a, b) RESULT(index)
    USE QFYAML_Mod

    REAL(yp), INTENT(IN) :: a(:)
    REAL(yp), INTENT(IN) :: b
    INTEGER :: index

    DO index = 1, SIZE(a)
      IF (a(index) == b) THEN
        RETURN
      END IF
    END DO
  END FUNCTION Find_Location_Real

  FUNCTION String_Count( a_str ) RESULT( first_non_missing )
    CHARACTER(LEN=*), INTENT(IN) :: a_str(:)
    INTEGER                      :: first_non_missing
    INTEGER :: N
    
    first_non_missing = 0
    DO N = 1, SIZE( a_str )
        IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT
        first_non_missing = N
    ENDDO
  END FUNCTION String_Count

  SUBROUTINE Trim_Comment(line, comment_chars)
    CHARACTER(LEN=*), INTENT(IN)    :: comment_chars
    CHARACTER(LEN=*), INTENT(INOUT) :: line
    INTEGER          :: n
    
    CHARACTER(LEN=1) :: current_char
    CHARACTER(LEN=1) :: need_char
    
    need_char = ""
    DO n = 1, LEN(line)
      current_char = line(n:n)

      IF (need_char == "") THEN
        IF (current_char == "'") THEN
          need_char = "'"    ! Open string
        ELSE IF (current_char == '"') THEN
          need_char = '"'    ! Open string
        ELSE IF (INDEX(comment_chars, current_char) /= 0) THEN
          line = line(1:n-1) ! Trim line up to comment character
          EXIT
        ENDIF
      ELSE IF (current_char == need_char) THEN
        need_char = ""        ! Close string
      ENDIF
    ENDDO
  END SUBROUTINE Trim_Comment
END MODULE ObsOperator_Input_Mod
