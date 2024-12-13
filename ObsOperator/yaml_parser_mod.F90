MODULE YAML_Parser_Mod
  USE YAML_Scanner_Mod

  IMPLICIT NONE
  PRIVATE

  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_KEY = 1
  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_SCALAR = 2
  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_SEQUENCE_START = 3
  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_SEQUENCE_END = 4
  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_MAPPING_START = 5
  INTEGER, PUBLIC, PARAMETER :: YAML_PARSER_EVENT_TYPE_MAPPING_END = 6

  TYPE, PUBLIC :: YamlParserEvent
    INTEGER :: EventType
    CHARACTER(LEN=255) :: Value
  END TYPE YamlParserEvent

  TYPE, PUBLIC :: YamlParser
    TYPE(YamlScanner) :: Scanner

    LOGICAL :: IsSequence(255)
    LOGICAL :: IsIndentlessSequence(255)
    LOGICAL :: IsFlowSequence(255)
    INTEGER :: CurrentDepth

    TYPE(YamlParserEvent) :: EventBuffer(255)
    INTEGER :: EventBufferIndex
  END TYPE YamlParser

  PUBLIC :: Setup_YAML_Parser
  PUBLIC :: Parse_YAML_Line
  PUBLIC :: Finalize_YAML_Parser
  PUBLIC :: Print_YAML_Parser_Event
  CONTAINS

  SUBROUTINE Setup_YAML_Parser(Parser)
    TYPE(YamlParser), INTENT(INOUT) :: Parser

    CALL Setup_YAML_Scanner(Parser%Scanner)
    Parser%CurrentDepth = 0
    Parser%EventBufferIndex = 0
  END SUBROUTINE Setup_YAML_Parser

  SUBROUTINE Parse_YAML_Line(Parser, Line, RC)
    TYPE(YamlParser), INTENT(INOUT) :: Parser
    CHARACTER(LEN=*), INTENT(IN) :: Line
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: i
    INTEGER :: currentTokenType

    CALL Scan_YAML_Line(Line, Parser%Scanner, RC)
    IF (RC /= 0) THEN
      PRINT '(A, I0)', 'Error scanning line: ', RC
      RETURN
    END IF

    DO i = 1, Parser%Scanner%TokenBufferIndex
      currentTokenType = Parser%Scanner%TokenBuffer(i)%TokenType

      IF (Parser%CurrentDepth > 0 .AND. Parser%IsFlowSequence(Parser%CurrentDepth)) THEN
        IF (currentTokenType /= YAML_TOKEN_FLOW_SEQUENCE_END .AND. currentTokenType /= YAML_TOKEN_SCALAR) THEN
          RC = 1
          RETURN
        END IF
      END IF

      SELECT CASE (currentTokenType)
      CASE (YAML_TOKEN_BLOCK_END)
        IF (Parser%IsIndentlessSequence(Parser%CurrentDepth)) THEN
          ! Pop an extra depth level because we're done with the indentless sequence
          CALL Pop_Depth(Parser, RC)
          IF (RC /= 0) THEN
            RETURN
          END IF
        END IF
        CALL Pop_Depth(Parser, RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_BLOCK_MAPPING_START)
        Parser%CurrentDepth = Parser%CurrentDepth + 1
        Parser%IsSequence(Parser%CurrentDepth) = .FALSE.
        Parser%IsIndentlessSequence(Parser%CurrentDepth) = .FALSE.
        Parser%IsFlowSequence(Parser%CurrentDepth) = .FALSE.
        CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_MAPPING_START, RC=RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_BLOCK_SEQUENCE_START)
        Parser%CurrentDepth = Parser%CurrentDepth + 1
        Parser%IsSequence(Parser%CurrentDepth) = .TRUE.
        Parser%IsIndentlessSequence(Parser%CurrentDepth) = .FALSE.
        Parser%IsFlowSequence(Parser%CurrentDepth) = .FALSE.
        CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_SEQUENCE_START, RC=RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_FLOW_SEQUENCE_START)
        Parser%CurrentDepth = Parser%CurrentDepth + 1
        Parser%IsSequence(Parser%CurrentDepth) = .TRUE.
        Parser%IsIndentlessSequence(Parser%CurrentDepth) = .FALSE.
        Parser%IsFlowSequence(Parser%CurrentDepth) = .TRUE.
        CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_SEQUENCE_START, RC=RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_FLOW_SEQUENCE_END)
        CALL Pop_Depth(Parser, RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_KEY)
        IF (Parser%IsIndentlessSequence(Parser%CurrentDepth)) THEN
          ! A new key means we're done with the current indentless sequence
          CALL Pop_Depth(Parser, RC)
          IF (RC /= 0) THEN
            RETURN
          END IF
        END IF
        CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_KEY, Parser%Scanner%TokenBuffer(i)%TokenValue, RC=RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      CASE (YAML_TOKEN_BLOCK_ENTRY)
        IF (.NOT. Parser%IsSequence(Parser%CurrentDepth)) THEN
          ! Sequence blocks can be indentless, in which case we won't have received a BLOCK_SEQUENCE_START token
          Parser%CurrentDepth = Parser%CurrentDepth + 1
          Parser%IsSequence(Parser%CurrentDepth) = .TRUE.
          Parser%IsIndentlessSequence(Parser%CurrentDepth) = .TRUE.
          Parser%IsFlowSequence(Parser%CurrentDepth) = .FALSE.
          CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_SEQUENCE_START, RC=RC)
          IF (RC /= 0) THEN
            RETURN
          END IF
        END IF
      CASE (YAML_TOKEN_SCALAR)
        CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_SCALAR, Parser%Scanner%TokenBuffer(i)%TokenValue, RC=RC)
        IF (RC /= 0) THEN
          RETURN
        END IF
      END SELECT
    END DO
    Parser%Scanner%TokenBufferIndex = 0
  END SUBROUTINE Parse_YAML_Line

  SUBROUTINE Finalize_YAML_Parser(Parser, RC)
    TYPE(YamlParser), INTENT(INOUT) :: Parser
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: currentDepth
    INTEGER :: i

    currentDepth = Parser%CurrentDepth
    DO i = 1, currentDepth
      CALL Pop_Depth(Parser, RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
    END DO
  END SUBROUTINE Finalize_YAML_Parser

  SUBROUTINE Pop_Depth(Parser, RC)
    TYPE(YamlParser), INTENT(INOUT) :: Parser
    INTEGER, INTENT(OUT) :: RC

    IF (Parser%CurrentDepth == 0) THEN
      RC = 1
      RETURN
    END IF

    IF (Parser%IsSequence(Parser%CurrentDepth)) THEN
      CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_SEQUENCE_END, RC=RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
    ELSE
      CALL Push_YAML_Parser_Event(Parser, YAML_PARSER_EVENT_TYPE_MAPPING_END, RC=RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
    END IF
    Parser%CurrentDepth = Parser%CurrentDepth - 1
  END SUBROUTINE Pop_Depth

  SUBROUTINE Push_YAML_Parser_Event(Parser, EventType, EventValue, RC)
    TYPE(YamlParser), INTENT(INOUT) :: Parser
    INTEGER, INTENT(IN) :: EventType
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: EventValue
    INTEGER, INTENT(OUT) :: RC

    IF (Parser%EventBufferIndex >= SIZE(Parser%EventBuffer)) THEN
      RC = 1
      RETURN
    END IF

    Parser%EventBufferIndex = Parser%EventBufferIndex + 1
    Parser%EventBuffer(Parser%EventBufferIndex)%EventType = EventType
    IF (PRESENT(EventValue)) THEN
      Parser%EventBuffer(Parser%EventBufferIndex)%Value = EventValue
    END IF
  END SUBROUTINE Push_YAML_Parser_Event

  SUBROUTINE Print_YAML_Parser_Event(Event)
    TYPE(YamlParserEvent), INTENT(IN) :: Event

    SELECT CASE (Event%EventType)
    CASE (YAML_PARSER_EVENT_TYPE_KEY)
      PRINT '(A, A, A)', 'KEY(', TRIM(Event%Value), ')'
    CASE (YAML_PARSER_EVENT_TYPE_SCALAR)
      PRINT '(A, A, A)', 'SCALAR(', TRIM(Event%Value), ')'
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_START)
      PRINT '(A)', 'SEQUENCE_START'
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_END)
      PRINT '(A)', 'SEQUENCE_END'
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_START)
      PRINT '(A)', 'MAPPING_START'
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_END)
      PRINT '(A)', 'MAPPING_END'
    END SELECT
  END SUBROUTINE Print_YAML_Parser_Event
END MODULE YAML_Parser_Mod

