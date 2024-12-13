MODULE ObsOperator_Input_Alt_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod
  USE Yaml_Node_Mod
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Read_ObsOperator_Input

  CONTAINS

  SUBROUTINE Read_ObsOperator_Input(Path, Input_Opt, State_Chm, State_Grid, ObsOperator_Entries, RC)
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState
    USE inquireMod, ONLY : findFreeLUN
    USE File_Mod, ONLY : IOERROR
    USE Yaml_Document_Mod

    CHARACTER(LEN=*), INTENT(IN) :: Path
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ObsOperator_Entries(:)
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: thisLocation

    INTEGER :: inputFile, ioStat
    INTEGER :: currentEntryIndex
    INTEGER :: currentIdLength
    TYPE(YamlDocument) :: Document

    thisLocation = " -> at Read_ObsOperator_Input (in module ObsOperator/obsoperator_input_alt_mod.F90)"

    inputFile = findFreeLUN()
    OPEN( inputFile, FILE=Path, IOSTAT=ioStat )
    IF (ioStat /= 0) THEN
      CALL IOERROR(ioStat, inputFile, 'Read_ObsOperator_Input:1')
    END IF
    CALL Setup_YAML_Document(Document, 32)

    CALL Parse_YAML_File(Document, inputFile, RC)
    IF (RC /= GC_SUCCESS) THEN
      WRITE(*, '(A, I0)') 'Error parsing YAML file: ', RC
      RETURN
    END IF

    CALL Read_Entries(Document, ObsOperator_Entries, Input_Opt, State_Chm, State_Grid, RC)
    IF (RC /= GC_SUCCESS) THEN
      CALL GC_Error('Error reading entries from YAML file', RC, thisLocation)
      RETURN
    END IF

    CALL Free_YAML_Document(Document)
  END SUBROUTINE Read_ObsOperator_Input

  SUBROUTINE Read_Entries(Document, ObsOperator_Entries, Input_Opt, State_Chm, State_Grid, RC)
    USE ErrCode_Mod
    USE Yaml_Document_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(YamlDocument), INTENT(IN) :: Document
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ObsOperator_Entries(:)
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: thisLocation

    TYPE(YamlNode), POINTER :: entriesNode
    TYPE(YamlNodeListEntry), POINTER :: currentEntryEntry

    INTEGER :: currentEntryIndex

    thisLocation = " -> at Read_Entries (in module ObsOperator/obsoperator_input_alt_mod.F90)"

    CALL Get_YAML_Key(Document%RootNode, 'entries', entriesNode, RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing entries key', RC, thisLocation)
      RETURN
    END IF

    IF (EntriesNode%NodeType /= YAML_NODE_TYPE_SEQUENCE) THEN
      CALL GC_Error('entries key is not a sequence', RC, thisLocation)
      RETURN
    END IF

    currentEntryEntry => EntriesNode%FirstEntry
    currentEntryIndex = 1
    DO WHILE (ASSOCIATED(currentEntryEntry))
      CALL Advance_Entry_Index(currentEntryIndex, ObsOperator_Entries, RC)
      IF (RC /= 0) THEN
        CALL GC_Error('Error advancing entry index', RC, thisLocation)
        RETURN
      END IF
      CALL Fill_Entry(ObsOperator_Entries(currentEntryIndex), currentEntryEntry%Node, Input_Opt, State_Chm, State_Grid, RC)
      IF (RC /= 0) THEN
        CALL GC_Error('Error filling entry', RC, thisLocation)
        RETURN
      END IF
      currentEntryEntry => currentEntryEntry%Next
    END DO
  END SUBROUTINE Read_Entries

  SUBROUTINE Advance_Entry_Index(currentEntryIndex, ObsOperator_Entries, RC)
    INTEGER, INTENT(INOUT) :: currentEntryIndex
    TYPE(ObsOperatorEntry), INTENT(IN) :: ObsOperator_Entries(:)
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: maxEntries

    maxEntries = SIZE(ObsOperator_Entries)

    IF (currentEntryIndex == 0) THEN
      currentEntryIndex = 1
    END IF

    DO WHILE (ObsOperator_Entries(currentEntryIndex)%IsActive)
      currentEntryIndex = currentEntryIndex + 1
      IF (currentEntryIndex > maxEntries) THEN
        RC = 1
        RETURN
      END IF
    END DO
  END SUBROUTINE Advance_Entry_Index

  SUBROUTINE Fill_Entry(Entry, Node, Input_Opt, State_Chm, State_Grid, RC)
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(YamlNode), INTENT(IN) :: Node
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: id
    TYPE(YamlNode), POINTER :: subNode

    CHARACTER(LEN=255) :: errorMessage, thisLocation

    thisLocation = " -> at Fill_Entry (in module ObsOperator/obsoperator_input_alt_mod.F90)"

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'id', id, RC=RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing id key', RC, thisLocation)
      RETURN
    END IF
    ALLOCATE(CHARACTER(LEN=LEN_TRIM(id)) :: Entry%Id)
    Entry%Id = id(1:LEN_TRIM(id))

    CALL Get_YAML_Key(Node, 'species', subNode, RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing species key', RC, thisLocation)
      RETURN
    END IF

    CALL Fill_Species(Entry, subNode, State_Chm, RC)
    IF ( RC /= GC_SUCCESS ) THEN
      errorMessage = 'Error reading species for operator entry ' // TRIM(Entry%Id)
      CALL GC_Error( errorMessage, RC, thisLocation )
      RETURN
    ENDIF

    CALL Get_YAML_Key(Node, 'time_operator', subNode, RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing time_operator key', RC, thisLocation)
      RETURN
    END IF

    CALL Fill_Time_Operator(Entry, subNode, Input_Opt, RC)
    IF ( RC /= GC_SUCCESS ) THEN
      errorMessage = 'Error reading time operator for operator entry ' // TRIM(Entry%Id)
      CALL GC_Error( errorMessage, RC, thisLocation )
      RETURN
    ENDIF

    CALL Get_YAML_Key(Node, 'horizontal_operator', subNode, RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing horizontal_operator key', RC, thisLocation)
      RETURN
    END IF

    CALL Fill_Horizontal_Operator(Entry, subNode, State_Grid, RC)
    IF ( RC /= GC_SUCCESS ) THEN
      errorMessage = 'Error reading horizontal operator for operator entry ' // TRIM(Entry%Id)
      CALL GC_Error( errorMessage, RC, thisLocation )
      RETURN
    ENDIF

    CALL Get_YAML_Key(Node, 'vertical_operator', subNode, RC)
    IF (RC /= 0) THEN
      CALL GC_Error('YAML file missing vertical_operator key', RC, thisLocation)
      RETURN
    END IF

    CALL Fill_Vertical_Operator(Entry, subNode, RC)
    IF ( RC /= GC_SUCCESS ) THEN
      errorMessage = 'Error reading vertical operator for operator entry ' // TRIM(Entry%Id)
      CALL GC_Error( errorMessage, RC, thisLocation )
      RETURN
    ENDIF

    Entry%IsActive = .TRUE.
  END SUBROUTINE Fill_Entry

  SUBROUTINE Fill_Species(Entry, Node, State_Chm, RC)
    USE ErrCode_Mod
    USE State_Chm_Mod, ONLY : ChmState

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(YamlNode), INTENT(IN) :: Node
    TYPE(ChmState), INTENT(IN) :: State_Chm
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: nSpecies, i, j
    CHARACTER(LEN=255), ALLOCATABLE :: speciesNames(:)

    CALL Get_YAML_Scalars_Size(Node, nSpecies, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    ALLOCATE(speciesNames(nSpecies))
    CALL Get_YAML_Char_Scalars(Node, speciesNames, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    ALLOCATE(Entry%SpeciesIndex(nSpecies))
    ALLOCATE(Entry%SpeciesValue(nSpecies))
    DO i = 1, nSpecies
      Entry%SpeciesIndex(i) = 0
      DO j = 1, State_Chm%nSpecies
        IF (TRIM(State_Chm%SpcData(j)%Info%Name) == TRIM(speciesNames(i))) THEN
          Entry%SpeciesIndex(i) = j
          EXIT
        END IF
      END DO
      IF (Entry%SpeciesIndex(i) == 0) THEN
        RC = 1
        RETURN
      END IF
      Entry%SpeciesValue(i) = 0.0_f8
    END DO

    DEALLOCATE(speciesNames)
  END SUBROUTINE Fill_Species

  SUBROUTINE Fill_Time_Operator(Entry, Node, Input_Opt, RC)
    USE ErrCode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE Error_Mod, ONLY : Error_Stop

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(YamlNode), INTENT(IN) :: Node
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    INTEGER, INTENT(OUT) :: RC

    INTEGER, PARAMETER :: TIME_OPERATOR_RANGE = 0
    INTEGER, PARAMETER :: TIME_OPERATOR_POINT = 1
    INTEGER, PARAMETER :: TIME_OPERATOR_EXACT = 2

    TYPE(YamlNode), POINTER :: subNode
    CHARACTER(LEN=255) :: vStr

    INTEGER :: timeOperatorType
    LOGICAL :: isUnitTimeIndex
    LOGICAL :: normalizeWeights
    INTEGER :: I, J
    INTEGER :: timeIndex1
    INTEGER :: timeIndex2
    INTEGER :: nTimeIndices

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'type', vStr, RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    SELECT CASE (TRIM(vStr))
    CASE ("range")
      timeOperatorType = TIME_OPERATOR_RANGE
    CASE ("point")
      timeOperatorType = TIME_OPERATOR_POINT
    CASE DEFAULT
      CALL Error_Stop( "Invalid time operator type", "" )
      RETURN
    END SELECT

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'unit', vStr, RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    isUnitTimeIndex = .FALSE.
    SELECT CASE (TRIM(vStr))
    CASE ("time_index")
      isUnitTimeIndex = .TRUE.
    CASE ("time")
      isUnitTimeIndex = .FALSE.
    CASE DEFAULT
      CALL Error_Stop( "Invalid time operator unit", "" )
      RETURN
    END SELECT

    CALL Get_YAML_Char_Scalar_At_Key(Node, "weights", vStr, "normalized", RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    normalizeWeights = .TRUE.
    SELECT CASE (TRIM(vStr))
    CASE ("equal")
      normalizeWeights = .FALSE.
    CASE ("normalized")
      normalizeWeights = .TRUE.
    CASE DEFAULT
      CALL Error_Stop( "Invalid time operator weighting type", "" )
      RETURN
    END SELECT

    IF (timeOperatorType == TIME_OPERATOR_RANGE) THEN
      CALL Read_Time_Entry(Node, "start", isUnitTimeIndex, Input_Opt, timeIndex1, RC)
      CALL Read_Time_Entry(Node, "end", isUnitTimeIndex, Input_Opt, timeIndex2, RC)
    ELSE IF (timeOperatorType == TIME_OPERATOR_POINT) THEN
      CALL Read_Time_Entry(Node, "time", isUnitTimeIndex, Input_Opt, timeIndex1, RC)
      timeIndex2 = timeIndex1
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
  END SUBROUTINE Fill_Time_Operator

  SUBROUTINE Fill_Horizontal_Operator(Entry, Node, State_Grid, RC)
    USE ErrCode_Mod
    USE State_Grid_Mod, ONLY : GrdState
    USE Error_Mod, ONLY : Error_Stop

    INTEGER, PARAMETER :: HORIZONTAL_OPERATOR_BOX = 0
    INTEGER, PARAMETER :: HORIZONTAL_OPERATOR_POINT = 1

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(YamlNode), INTENT(IN) :: Node
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC


    CHARACTER(LEN=255) :: vStr
    LOGICAL :: isUnitGridIndex
    INTEGER :: horizontalOperatorType
    INTEGER :: boxLowerI, boxUpperI, boxLowerJ, boxUpperJ
    LOGICAL :: isAreaWeights
    LOGICAL :: isNormalizedWeights
    INTEGER :: nHorizontalIndices
    INTEGER :: I, J, currentIndex

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'type', vStr, RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    SELECT CASE (TRIM(vStr))
    CASE ("box")
      horizontalOperatorType = HORIZONTAL_OPERATOR_BOX
    CASE ("point")
      horizontalOperatorType = HORIZONTAL_OPERATOR_POINT
    CASE DEFAULT
      CALL Error_Stop( "Invalid horizontal operator type", "" )
      RETURN
    END SELECT

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'unit', vStr, 'degrees', RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    isUnitGridIndex = .FALSE.
    SELECT CASE (TRIM(vStr))
    CASE ("grid_index")
      isUnitGridIndex = .TRUE.
    CASE ("degrees")
      isUnitGridIndex = .FALSE.
    CASE DEFAULT
      CALL Error_Stop( "Invalid horizontal operator unit", "" )
      RETURN
    END SELECT

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'weights', vStr, 'normalized_area', RC=RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    SELECT CASE (TRIM(vStr))
    CASE ("area")
      isAreaWeights = .TRUE.
      isNormalizedWeights = .FALSE.
    CASE ("normalized_area")
      isAreaWeights = .TRUE.
      isNormalizedWeights = .TRUE.
    CASE ("normalized")
      isAreaWeights = .FALSE.
      isNormalizedWeights = .TRUE.
    CASE ("equal")
      isAreaWeights = .FALSE.
      isNormalizedWeights = .FALSE.
    CASE DEFAULT
      CALL Error_Stop( "Invalid horizontal operator weights", "" )
      RETURN
    END SELECT

    IF (horizontalOperatorType == HORIZONTAL_OPERATOR_BOX) THEN
      CALL Read_Longitude_Entry(Node, "longitude_start", isUnitGridIndex, State_Grid, RC, boxLowerI)
      CALL Read_Longitude_Entry(Node, "longitude_end", isUnitGridIndex, State_Grid, RC, boxUpperI)
      CALL Read_Latitude_Entry(Node, "latitude_start", isUnitGridIndex, State_Grid, RC, boxLowerJ)
      CALL Read_Latitude_Entry(Node, "latitude_end", isUnitGridIndex, State_Grid, RC, boxUpperJ)
    ELSE IF (horizontalOperatorType == HORIZONTAL_OPERATOR_POINT) THEN
      CALL Read_Longitude_Entry(Node, "longitude", isUnitGridIndex, State_Grid, RC, boxLowerI)
      CALL Read_Latitude_Entry(Node, "latitude", isUnitGridIndex, State_Grid, RC, boxLowerJ)
      boxUpperI = boxLowerI
      boxUpperJ = boxLowerJ
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
  END SUBROUTINE Fill_Horizontal_Operator

  SUBROUTINE Fill_Vertical_Operator(Entry, Node, RC)
    USE ErrCode_Mod
    USE Error_Mod, ONLY : Error_Stop

    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_POINT = 0
    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_RANGE = 1
    INTEGER, PARAMETER :: VERTICAL_OPERATOR_INTERNAL_EXACT = 2

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    TYPE(YamlNode), INTENT(IN) :: Node
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=255) :: vStr
    REAL(fp) :: vReal

    INTEGER :: verticalOperatorType
    INTEGER :: nVerticalExact
    INTEGER :: nVerticalWeights

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'type', vStr, RC=RC)
    IF (RC /= 0) THEN
      CALL Error_Stop( "Error reading vertical operator type", "" )
      RETURN
    END IF
    SELECT CASE (TRIM(vStr))
    CASE ("point")
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_POINT
    CASE ("range")
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_RANGE
    CASE ("exact")
      verticalOperatorType = VERTICAL_OPERATOR_INTERNAL_EXACT
    CASE DEFAULT
      CALL Error_Stop( "Invalid vertical operator type", "" )
      RETURN
    END SELECT

    CALL Get_YAML_Char_Scalar_At_Key(Node, 'unit', vStr, 'pressure', RC=RC)
    IF (RC /= 0) THEN
      CALL Error_Stop( "Error reading vertical operator unit", "" )
      RETURN
    END IF
    SELECT CASE (TRIM(vStr))
    CASE ("pressure")
      Entry%VerticalUnit = VERTICAL_UNIT_PRESSURE
    CASE ("altitude")
      Entry%VerticalUnit = VERTICAL_UNIT_ALTITUDE
    CASE ("pressure_level")
      Entry%VerticalUnit = VERTICAL_UNIT_PRESSURE_LEVEL
    CASE DEFAULT
      CALL Error_Stop( "Invalid vertical operator unit", "" )
      RETURN
    END SELECT

    IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_EXACT) THEN
      Entry%VerticalOperatorType = VERTICAL_OPERATOR_TYPE_EXACT

      CALL Get_YAML_Scalars_Size_At_Key(Node, "values", nVerticalExact, RC)
      IF (RC /= 0) THEN
        CALL Error_Stop( "Error reading vertical operator exact values size", "" )
        RETURN
      END IF

      CALL Get_YAML_Scalars_Size_At_Key(Node, "weights", nVerticalWeights, RC)
      IF (RC /= 0) THEN
        CALL Error_Stop( "Error reading vertical operator exact weights size", "" )
        RETURN
      END IF

      IF (nVerticalExact /= nVerticalWeights) THEN
        CALL Error_Stop( "Vertical operator exact values and weights do not match", "" )
        RETURN
      END IF

      IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
        ALLOCATE(Entry%VerticalExactIndex(nVerticalExact))
        CALL Get_YAML_Int_Scalars_At_Key(Node, "values", Entry%VerticalExactIndex, RC)
        IF (RC /= 0) THEN
          CALL Error_Stop( "Error reading vertical operator exact values", "" )
          RETURN
        END IF
      ELSE
        ALLOCATE(Entry%VerticalExact(nVerticalExact))
        CALL Get_YAML_Real_Scalars_At_Key(Node, "values", Entry%VerticalExact, RC)
        IF (RC /= 0) THEN
          CALL Error_Stop( "Error reading vertical operator exact values", "" )
          RETURN
        END IF
      END IF

      ALLOCATE(Entry%VerticalExactWeight(nVerticalExact))
      CALL Get_YAML_Real_Scalars_At_Key(Node, "weights", Entry%VerticalExactWeight, RC)
      IF (RC /= 0) THEN
        CALL Error_Stop( "Error reading vertical operator exact weights", "" )
        RETURN
      END IF
    ELSE
      Entry%VerticalOperatorType = VERTICAL_OPERATOR_TYPE_RANGE

      CALL Get_YAML_Char_Scalar_At_Key(Node, 'weights', vStr, 'normalized_pressure', RC=RC)
      IF (RC /= 0) THEN
        CALL Error_Stop( "Error reading vertical operator weights", "" )
        RETURN
      END IF
      SELECT CASE (TRIM(vStr))
      CASE ("normalized_pressure")
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE
      CASE ("pressure")
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_PRESSURE
      CASE ("normalized")
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_NORMALIZED
      CASE ("equal")
        Entry%VerticalOperatorWeight = VERTICAL_OPERATOR_WEIGHT_EQUAL
      CASE DEFAULT
        CALL Error_Stop( "Invalid vertical operator weights", "" )
        RETURN
      END SELECT  

      IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_POINT) THEN
        IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
          CALL Get_YAML_Int_Scalar_At_Key(Node, 'value', Entry%VerticalRangeStartIndex, RC=RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator point value", "" )
            RETURN
          END IF
          Entry%VerticalRangeEndIndex = Entry%VerticalRangeStartIndex
        ELSE
          CALL Get_YAML_Real_Scalar_At_Key(Node, 'value', Entry%VerticalRangeStart, RC=RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator point value", "" )
            RETURN
          END IF
          Entry%VerticalRangeEnd = Entry%VerticalRangeStart
        END IF
      ELSE IF (verticalOperatorType == VERTICAL_OPERATOR_INTERNAL_RANGE) THEN
        IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
          CALL Get_YAML_Int_Scalar_At_Key(Node, 'start', Entry%VerticalRangeStartIndex, Entry%VerticalRangeEndIndex, RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator range start", "" )
            RETURN
          END IF
          CALL Get_YAML_Int_Scalar_At_Key(Node, 'end', Entry%VerticalRangeEndIndex, RC=RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator range end", "" )
            RETURN
          END IF
        ELSE
          CALL Get_YAML_Real_Scalar_At_Key(Node, 'start', Entry%VerticalRangeStart, Entry%VerticalRangeEnd, RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator range start", "" )
            RETURN
          END IF
          CALL Get_YAML_Real_Scalar_At_Key(Node, 'end', Entry%VerticalRangeEnd, RC=RC)
          IF (RC /= 0) THEN
            CALL Error_Stop( "Error reading vertical operator range end", "" )
            RETURN
          END IF
        END IF
      END IF

      IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE_LEVEL) THEN
        IF (Entry%VerticalRangeStartIndex > Entry%VerticalRangeEndIndex) THEN
          CALL Error_Stop( "Vertical operator range start is greater than end", "" )
          RETURN
        END IF
      ELSE
        IF (Entry%VerticalRangeStart > Entry%VerticalRangeEnd) THEN
          CALL Error_Stop( "Vertical operator range start is greater than end", "" )
          RETURN
        END IF
      END IF
    END IF
  END SUBROUTINE Fill_Vertical_Operator

  SUBROUTINE Read_Time_Entry(Node, Key, IsUnitTimeIndex, Input_Opt, Output, RC)
    USE Input_Opt_Mod, ONLY : OptInput

    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    LOGICAL, INTENT(IN) :: IsUnitTimeIndex
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    INTEGER, INTENT(OUT) :: Output
    INTEGER, INTENT(OUT) :: RC
    
    INTEGER :: time(2)

    IF (IsUnitTimeIndex) THEN
      CALL Get_YAML_Int_Scalar_At_Key(Node, Key, Output, RC=RC)
    ELSE
      CALL Get_YAML_Int_Scalars_At_Key(Node, Key, time, RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
      CALL Convert_Time_To_Index(time, Input_Opt, Output)
    END IF
  END SUBROUTINE Read_Time_Entry

  SUBROUTINE Read_Longitude_Entry(Node, Key, IsUnitGridIndex, State_Grid, RC, Output)
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    LOGICAL, INTENT(IN) :: IsUnitGridIndex
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC
    INTEGER, INTENT(OUT) :: Output

    REAL(fp) :: longitude

    IF (IsUnitGridIndex) THEN
      CALL Get_YAML_Int_Scalar_At_Key(Node, Key, Output, RC=RC)
    ELSE
      CALL Get_YAML_Real_Scalar_At_Key(Node, Key, longitude, RC=RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
      CALL Convert_Longitude_To_Index(DBLE(longitude), State_Grid, Output)
    END IF
  END SUBROUTINE Read_Longitude_Entry

  SUBROUTINE Read_Latitude_Entry(Node, Key, IsUnitGridIndex, State_Grid, RC, Output)
    USE State_Grid_Mod, ONLY : GrdState

    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    LOGICAL, INTENT(IN) :: IsUnitGridIndex
    TYPE(GrdState), INTENT(IN) :: State_Grid
    INTEGER, INTENT(OUT) :: RC
    INTEGER, INTENT(OUT) :: Output

    REAL(fp) :: latitude

    IF (IsUnitGridIndex) THEN
      CALL Get_YAML_Int_Scalar_At_Key(Node, Key, Output, RC=RC)
    ELSE
      CALL Get_YAML_Real_Scalar_At_Key(Node, Key, latitude, RC=RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
      CALL Convert_Latitude_To_Index(DBLE(latitude), State_Grid, Output)
    END IF
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
END MODULE ObsOperator_Input_Alt_Mod