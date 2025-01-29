MODULE YAML_Document_Mod
  USE YAML_Parser_Mod

  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: MAX_KEY_LENGTH = 255

  TYPE, PUBLIC :: YamlDocument
    INTEGER :: MaxKeys
    INTEGER :: MaxKeyDepth

    CHARACTER(LEN=MAX_KEY_LENGTH), ALLOCATABLE :: Keys(:, :)
    LOGICAL, ALLOCATABLE :: IsSequence(:, :)
    INTEGER, ALLOCATABLE :: SequenceIndex(:, :)
    INTEGER, ALLOCATABLE :: KeyDepths(:)
    CHARACTER(LEN=MAX_KEY_LENGTH), ALLOCATABLE :: Values(:)
    INTEGER :: ValueIndex

    CHARACTER(LEN=MAX_KEY_LENGTH), ALLOCATABLE :: CurrentKey(:)
    LOGICAL, ALLOCATABLE :: CurrentIsSequence(:)
    INTEGER, ALLOCATABLE :: CurrentSequenceIndex(:)
    INTEGER :: CurrentKeyDepth
  END TYPE YamlDocument

  PUBLIC :: Setup_YAML_Document
  PUBLIC :: Free_YAML_Document
  PUBLIC :: Push_YAML_Parser_Event
  PUBLIC :: Print_YAML_Document
  CONTAINS

  SUBROUTINE Setup_YAML_Document(Document, MaxKeys, MaxKeyDepth)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    INTEGER, INTENT(IN) :: MaxKeys
    INTEGER, INTENT(IN) :: MaxKeyDepth

    Document%MaxKeys = MaxKeys
    Document%MaxKeyDepth = MaxKeyDepth

    Document%ValueIndex = 0
    Document%CurrentKeyDepth = 0

    ALLOCATE(Document%Keys(Document%MaxKeys, Document%MaxKeyDepth))
    ALLOCATE(Document%IsSequence(Document%MaxKeys, Document%MaxKeyDepth))
    ALLOCATE(Document%SequenceIndex(Document%MaxKeys, Document%MaxKeyDepth))
    ALLOCATE(Document%KeyDepths(Document%MaxKeys))
    ALLOCATE(Document%Values(Document%MaxKeys))

    ALLOCATE(Document%CurrentKey(Document%MaxKeyDepth))
    ALLOCATE(Document%CurrentIsSequence(Document%MaxKeyDepth))
    ALLOCATE(Document%CurrentSequenceIndex(Document%MaxKeyDepth))
  END SUBROUTINE Setup_YAML_Document

  SUBROUTINE Free_YAML_Document(Document)
    TYPE(YamlDocument), INTENT(INOUT) :: Document

    DEALLOCATE(Document%Keys)
    DEALLOCATE(Document%IsSequence)
    DEALLOCATE(Document%SequenceIndex)
    DEALLOCATE(Document%KeyDepths)
    DEALLOCATE(Document%Values)

    DEALLOCATE(Document%CurrentKey)
    DEALLOCATE(Document%CurrentIsSequence)
    DEALLOCATE(Document%CurrentSequenceIndex)
  END SUBROUTINE Free_YAML_Document

  SUBROUTINE Push_YAML_Parser_Event(Document, Event, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    TYPE(YamlParserEvent), INTENT(IN) :: Event
    INTEGER, INTENT(OUT) :: RC

    SELECT CASE (Event%EventType)
    CASE (YAML_PARSER_EVENT_TYPE_KEY)
      Document%CurrentKey(Document%CurrentKeyDepth) = Event%Value
    CASE (YAML_PARSER_EVENT_TYPE_SCALAR)
      ! Scalars are the leaf nodes of the YAML document
      CALL Increment_Index_If_Sequence(Document)
      CALL Push_Value(Document, Event%Value, RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_START)
      CALL Increment_Index_If_Sequence(Document)
      CALL Push_Level(Document, IsSequence=.TRUE., RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_END)
      Document%CurrentKeyDepth = Document%CurrentKeyDepth - 1
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_START)
      CALL Increment_Index_If_Sequence(Document)
      CALL Push_Level(Document, IsSequence=.FALSE., RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_END)
      Document%CurrentKeyDepth = Document%CurrentKeyDepth - 1
    CASE DEFAULT
      RC = 1
    END SELECT
  END SUBROUTINE Push_YAML_Parser_Event

  SUBROUTINE Print_YAML_Document(Document)
    TYPE(YamlDocument), INTENT(IN) :: Document
    INTEGER :: I
    CHARACTER(LEN=512) :: EntryKey
    DO I = 1, Document%ValueIndex
      CALL Make_Entry_Key(Document, I, EntryKey)
      PRINT '(A, A, A)', TRIM(EntryKey), ' = ', TRIM(Document%Values(I))
    END DO
  END SUBROUTINE Print_YAML_Document

  SUBROUTINE Make_Entry_Key(Document, Index, EntryKey)
    TYPE(YamlDocument), INTENT(IN) :: Document
    INTEGER, INTENT(IN) :: Index
    CHARACTER(LEN=*), INTENT(INOUT) :: EntryKey

    INTEGER :: i
    CHARACTER(LEN=16) :: sequenceIndex

    EntryKey = '$root'
    DO i = 1, Document%KeyDepths(Index)
      IF (Document%IsSequence(Index, i)) THEN
        WRITE(sequenceIndex, '(I0)') Document%SequenceIndex(Index, i)
        EntryKey = TRIM(EntryKey) // '[' // TRIM(sequenceIndex) // ']'
      ELSE
        EntryKey = TRIM(EntryKey) // '.' // TRIM(Document%Keys(Index, i))
      END IF
    END DO
  END SUBROUTINE Make_Entry_Key

  SUBROUTINE Increment_Index_If_Sequence(Document)
    TYPE(YamlDocument), INTENT(INOUT) :: Document

    IF (Document%CurrentIsSequence(Document%CurrentKeyDepth)) THEN
      Document%CurrentSequenceIndex(Document%CurrentKeyDepth) = Document%CurrentSequenceIndex(Document%CurrentKeyDepth) + 1
    END IF
  END SUBROUTINE Increment_Index_If_Sequence

  SUBROUTINE Push_Value(Document, NewValue, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    CHARACTER(LEN=*), INTENT(IN) :: NewValue
    INTEGER, INTENT(OUT) :: RC

    IF (Document%ValueIndex >= Document%MaxKeys) THEN
      RC = 1
      RETURN
    END IF

    Document%ValueIndex = Document%ValueIndex + 1

    ! Copy all current entries to the new index
    Document%Keys(Document%ValueIndex, 1:Document%CurrentKeyDepth) = Document%CurrentKey(1:Document%CurrentKeyDepth)
    Document%IsSequence(Document%ValueIndex, 1:Document%CurrentKeyDepth) = Document%CurrentIsSequence(1:Document%CurrentKeyDepth)
    Document%SequenceIndex(Document%ValueIndex, 1:Document%CurrentKeyDepth) = Document%CurrentSequenceIndex(1:Document%CurrentKeyDepth)
    Document%KeyDepths(Document%ValueIndex) = Document%CurrentKeyDepth
    Document%Values(Document%ValueIndex) = NewValue
  END SUBROUTINE Push_Value

  SUBROUTINE Push_Level(Document, IsSequence, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    LOGICAL, INTENT(IN) :: IsSequence
    INTEGER, INTENT(OUT) :: RC

    IF (Document%CurrentKeyDepth >= Document%MaxKeyDepth) THEN
      RC = 1
      RETURN
    END IF

    Document%CurrentKeyDepth = Document%CurrentKeyDepth + 1
    Document%CurrentIsSequence(Document%CurrentKeyDepth) = IsSequence
    Document%CurrentSequenceIndex(Document%CurrentKeyDepth) = 0
  END SUBROUTINE Push_Level
END MODULE YAML_Document_Mod