MODULE ObsOperator_Entry_Mod
  USE PRECISION_MOD

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Deactivate_ObsOperatorEntry

  INTEGER, PUBLIC, PARAMETER :: MAX_ENTRY_ID_LENGTH = 255
  INTEGER, PUBLIC, PARAMETER :: MAX_ENTRY_FIELD_NAME_LENGTH = 64

  INTEGER, PUBLIC, PARAMETER :: VERTICAL_UNIT_PRESSURE = 1
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_UNIT_ALTITUDE = 2
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_UNIT_PRESSURE_LEVEL = 3
  
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_TYPE_RANGE = 1
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_TYPE_EXACT = 2
  
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE = 1
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_WEIGHT_PRESSURE = 2
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_WEIGHT_NORMALIZED = 3
  INTEGER, PUBLIC, PARAMETER :: VERTICAL_OPERATOR_WEIGHT_EQUAL = 4
  
  TYPE, PUBLIC :: ObsOperatorEntry
    LOGICAL :: IsActive = .FALSE.

    CHARACTER(LEN=:), ALLOCATABLE :: Id

    CHARACTER(LEN=MAX_ENTRY_FIELD_NAME_LENGTH), ALLOCATABLE :: FieldName(:)
    INTEGER, ALLOCATABLE :: FieldSpeciesIndex(:)
    REAL(f8), ALLOCATABLE :: FieldValue(:)

    INTEGER, ALLOCATABLE :: TimeIndices(:)
    REAL(f8), ALLOCATABLE :: TimeWeights(:)

    INTEGER, ALLOCATABLE :: HorizontalIndices(:,:)
    REAL(f8), ALLOCATABLE :: HorizontalWeights(:)

    INTEGER :: VerticalUnit
    INTEGER :: VerticalOperatorType
    ! Vertical operator range
    REAL(f8) :: VerticalRangeStart
    REAL(f8) :: VerticalRangeEnd
    INTEGER :: VerticalRangeStartIndex
    INTEGER :: VerticalRangeEndIndex
    INTEGER :: VerticalOperatorWeight
    ! Vertical operator exact
    REAL(f8), ALLOCATABLE :: VerticalExact(:)
    INTEGER, ALLOCATABLE :: VerticalExactIndex(:)
    REAL(f8), ALLOCATABLE :: VerticalExactWeight(:)
  END TYPE ObsOperatorEntry

  CONTAINS

  SUBROUTINE Deactivate_ObsOperatorEntry(ThisEntry)
    TYPE(ObsOperatorEntry), INTENT(INOUT) :: ThisEntry

    ThisEntry%IsActive = .FALSE.
    IF (ALLOCATED(ThisEntry%Id)) THEN
      DEALLOCATE(ThisEntry%Id)
    END IF
    IF (ALLOCATED(ThisEntry%FieldName)) THEN
      DEALLOCATE(ThisEntry%FieldName)
    END IF
    IF (ALLOCATED(ThisEntry%FieldSpeciesIndex)) THEN
      DEALLOCATE(ThisEntry%FieldSpeciesIndex)
    END IF
    IF (ALLOCATED(ThisEntry%FieldValue)) THEN
      DEALLOCATE(ThisEntry%FieldValue)
    END IF
    IF (ALLOCATED(ThisEntry%TimeIndices)) THEN
      DEALLOCATE(ThisEntry%TimeIndices)
    END IF
    IF (ALLOCATED(ThisEntry%TimeWeights)) THEN
      DEALLOCATE(ThisEntry%TimeWeights)
    END IF
    IF (ALLOCATED(ThisEntry%HorizontalIndices)) THEN
      DEALLOCATE(ThisEntry%HorizontalIndices)
    END IF
    IF (ALLOCATED(ThisEntry%HorizontalWeights)) THEN
      DEALLOCATE(ThisEntry%HorizontalWeights)
    END IF
    IF (ALLOCATED(ThisEntry%VerticalExact)) THEN
      DEALLOCATE(ThisEntry%VerticalExact)
    END IF
    IF (ALLOCATED(ThisEntry%VerticalExactIndex)) THEN
      DEALLOCATE(ThisEntry%VerticalExactIndex)
    END IF
    IF (ALLOCATED(ThisEntry%VerticalExactWeight)) THEN
      DEALLOCATE(ThisEntry%VerticalExactWeight)
    END IF
  END SUBROUTINE Deactivate_ObsOperatorEntry
END MODULE ObsOperator_Entry_Mod
