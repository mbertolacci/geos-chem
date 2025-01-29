! Lexes a subset of YAML
! - No flow mappings
! - Flow sequence only allowed to contain scalars and not allowed to span lines
! - Simple keys that are scalars only
! - No quoted scalars
! - Strings are not allowed to span lines
! - No anchors or aliases
! - No tags
MODULE YAML_Scanner_Mod
  IMPLICIT NONE
  PRIVATE

  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_BLOCK_SEQUENCE_START = 0
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_BLOCK_MAPPING_START = 1
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_BLOCK_END = 2
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_BLOCK_ENTRY = 3
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_FLOW_SEQUENCE_START = 4
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_FLOW_SEQUENCE_END = 5
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_KEY = 6
  INTEGER, PUBLIC, PARAMETER :: YAML_TOKEN_SCALAR = 7

  TYPE, PUBLIC :: YamlToken
    INTEGER :: TokenType
    CHARACTER(LEN=255) :: TokenValue
  END TYPE YamlToken

  TYPE, PUBLIC :: YamlScanner
    INTEGER :: IndentationDepth(16)
    INTEGER :: IndentationDepthIndex

    TYPE(YamlToken) :: TokenBuffer(255)
    INTEGER :: TokenBufferIndex
  END TYPE YamlScanner

  PUBLIC :: Setup_YAML_Scanner
  PUBLIC :: Print_YAML_Token
  PUBLIC :: Scan_YAML_Line
  CONTAINS

  SUBROUTINE Setup_YAML_Scanner(Scanner)
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner

    Scanner%IndentationDepthIndex = 1
    Scanner%IndentationDepth(1) = -1
    Scanner%TokenBufferIndex = 0
  END SUBROUTINE Setup_YAML_Scanner

  SUBROUTINE Print_YAML_Token(Token)
    TYPE(YamlToken), INTENT(IN) :: Token

    IF (Token%TokenType == YAML_TOKEN_BLOCK_SEQUENCE_START) THEN
      PRINT '(A)', "BLOCK_SEQUENCE_START"
    ELSE IF (Token%TokenType == YAML_TOKEN_BLOCK_MAPPING_START) THEN
      PRINT '(A)', "BLOCK_MAPPING_START"
    ELSE IF (Token%TokenType == YAML_TOKEN_BLOCK_END) THEN
      PRINT '(A)', "BLOCK_END"
    ELSE IF (Token%TokenType == YAML_TOKEN_BLOCK_ENTRY) THEN
      PRINT '(A)', "BLOCK_ENTRY"
    ELSE IF (Token%TokenType == YAML_TOKEN_KEY) THEN
      PRINT '(A)', "KEY"
      PRINT '(A)', Token%TokenValue
    ELSE IF (Token%TokenType == YAML_TOKEN_SCALAR) THEN
      PRINT '(A)', "SCALAR"
      PRINT '(A)', Token%TokenValue
    END IF
  END SUBROUTINE Print_YAML_Token

  SUBROUTINE Scan_YAML_Line(Line, Scanner, RC)
    CHARACTER(LEN=*), INTENT(IN) :: Line
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: lineIndex
    INTEGER :: lineEndIndex
    INTEGER :: lineIndent
    INTEGER :: currentIndent
    INTEGER :: colonIndex
    LOGICAL :: addedIndent

    CALL Line_End_Index(Line, lineEndIndex)

    IF (lineEndIndex == 0) THEN
      ! Skip blank lines
      RETURN
    END IF

    lineIndex = 1
    CALL Advance_While_Space(Line, lineIndex)
    lineIndent = lineIndex - 1


    currentIndent = Scanner%IndentationDepth(Scanner%IndentationDepthIndex)

    addedIndent = .FALSE.
    IF (lineIndent > currentIndent) THEN
      CALL Push_Indent(Scanner, lineIndent, RC)
      IF (RC /= 0) THEN
        PRINT '(A, I0)', "Error pushing indent: ", RC
        RETURN
      END IF
      addedIndent = .TRUE.
    ELSE IF (lineIndent < currentIndent) THEN
      ! Dropped down indentation; keep popping until we match the current indentation
      DO WHILE (lineIndent < currentIndent)
        CALL Pop_Indent(Scanner, currentIndent, RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error popping indent: ", RC
          RETURN
        END IF

        CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_END, RC = RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing block end: ", RC
          RETURN
        END IF  
      END DO

      IF (lineIndent /= currentIndent) THEN
        RC = 1
        RETURN
      END IF
    END IF

    ! Check for a block entry
    IF (Line(lineIndex:lineIndex) == "-") THEN
      IF (addedIndent) THEN
        CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_SEQUENCE_START, RC = RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing block sequence start: ", RC
          RETURN
        END IF
      END IF

      CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_ENTRY, RC = RC)
      IF (RC /= 0) THEN
        PRINT '(A, I0)', "Error pushing block entry: ", RC
        RETURN
      END IF

      lineIndex = lineIndex + 1

      DO WHILE (lineIndex <= lineEndIndex)
        CALL Advance_While_Space(Line, lineIndex)

        IF (lineIndex > lineEndIndex) THEN
          ! End of line reached
          RETURN
        END IF

        IF (Line(lineIndex:lineIndex) == "-") THEN
          ! Nested block sequence
          CALL Push_Indent(Scanner, lineIndex - 1, RC)
          IF (RC /= 0) THEN
            PRINT '(A, I0)', "Error pushing indent in nested block sequence: ", RC
            RETURN
          END IF

          CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_SEQUENCE_START, RC = RC)
          IF (RC /= 0) THEN
            PRINT '(A, I0)', "Error pushing block sequence start: ", RC
            RETURN
          END IF

          CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_ENTRY, RC = RC)
          IF (RC /= 0) THEN
            PRINT '(A, I0)', "Error pushing block entry: ", RC
            RETURN
          END IF
        ELSE
          EXIT
        END IF
      END DO

      ! If the rest of the line contains anything, it is either "key: value" or just a scalar
      colonIndex = INDEX(Line(1:lineEndIndex), ":")
      IF (colonIndex > 0) THEN
        ! Add indent and push a block mapping start
        CALL Push_Indent(Scanner, lineIndex - 1, RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing indent in block mapping: ", RC
          RETURN
        END IF

        CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_MAPPING_START, RC = RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing block mapping start: ", RC
          RETURN
        END IF

        ! Must be either "key:" or "key: value", where value can be either a scalar or [scalar, ..., scalar]
        CALL Scan_YAML_Key_Value(Line, lineIndex, lineEndIndex, Scanner, RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error parsing key value: ", RC
          RETURN
        END IF
      ELSE
        ! Must be a value, where value can be either a scalar or [scalar, ..., scalar]
        CALL Scan_YAML_Value(Line, lineIndex, lineEndIndex, Scanner, RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing scalar: ", RC
          RETURN
        END IF
      END IF
    ELSE
      IF (addedIndent) THEN
        CALL Push_Token(Scanner, YAML_TOKEN_BLOCK_MAPPING_START, RC = RC)
        IF (RC /= 0) THEN
          PRINT '(A, I0)', "Error pushing block mapping start: ", RC
          RETURN
        END IF
      END IF

      ! Must be either "key:" or "key: value", where value can be either a scalar or [scalar, ..., scalar]
      CALL Scan_YAML_Key_Value(Line, lineIndex, lineEndIndex, Scanner, RC)
      IF (RC /= 0) THEN
        PRINT '(A, I0)', "Error parsing key value: ", RC
        RETURN
      END IF
    END IF
  END SUBROUTINE Scan_YAML_Line

  SUBROUTINE Scan_YAML_Key_Value(Line, LineStartIndex, LineEndIndex, Scanner, RC)
    CHARACTER(LEN=*), INTENT(IN) :: Line
    INTEGER, INTENT(IN) :: LineStartIndex
    INTEGER, INTENT(IN) :: LineEndIndex
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: colonIndex
    INTEGER :: lineIndex

    lineIndex = LineStartIndex
    colonIndex = INDEX(Line(lineIndex:LineEndIndex), ":")
    IF (colonIndex == 0) THEN
      RC = 1
      RETURN
    END IF
    colonIndex = colonIndex + lineIndex - 1

    CALL Push_Token(Scanner, YAML_TOKEN_KEY, Line(lineIndex:colonIndex-1), RC = RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    lineIndex = colonIndex + 1
    CALL Advance_While_Space(Line, lineIndex)
    IF (lineIndex > LineEndIndex) THEN
      ! No value
      RETURN
    END IF

    CALL Scan_YAML_Value(Line, lineIndex, LineEndIndex, Scanner, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
  END SUBROUTINE Scan_YAML_Key_Value

  SUBROUTINE Scan_YAML_Value(Line, LineStartIndex, LineEndIndex, Scanner, RC)
    CHARACTER(LEN=*), INTENT(IN) :: Line
    INTEGER, INTENT(IN) :: LineStartIndex
    INTEGER, INTENT(IN) :: LineEndIndex
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: lineIndex, valueEndIndex, endBracketIndex

    lineIndex = LineStartIndex

    IF (Line(lineIndex:lineIndex) == "[") THEN
      CALL Push_Token(Scanner, YAML_TOKEN_FLOW_SEQUENCE_START, RC = RC)
      IF (RC /= 0) THEN
        RETURN
      END IF

      endBracketIndex = INDEX(Line(lineIndex:LineEndIndex), "]")
      IF (endBracketIndex == 0) THEN
        RC = 1
        RETURN
      END IF
      endBracketIndex = endBracketIndex + lineIndex - 1

      lineIndex = lineIndex + 1
      CALL Advance_While_Space(Line, lineIndex)

      DO WHILE (lineIndex < endBracketIndex)
        valueEndIndex = INDEX(Line(lineIndex:endBracketIndex), ",") - 1
        IF (valueEndIndex == -1) THEN
          valueEndIndex = endBracketIndex - 1
        ELSE
          valueEndIndex = valueEndIndex + lineIndex - 1
        END IF

        CALL Push_Token(Scanner, YAML_TOKEN_SCALAR, TRIM(Line(lineIndex:valueEndIndex)), RC = RC)
        IF (RC /= 0) THEN
          RETURN
        END IF

        lineIndex = valueEndIndex + 2
        IF (lineIndex < endBracketIndex) THEN
          CALL Advance_While_Space(Line, lineIndex)
        END IF
      END DO

      CALL Push_Token(Scanner, YAML_TOKEN_FLOW_SEQUENCE_END, RC = RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
    ELSE
      CALL Push_Token(Scanner, YAML_TOKEN_SCALAR, Line(lineIndex:LineEndIndex), RC = RC)
      IF (RC /= 0) THEN
        RETURN
      END IF
    END IF
  END SUBROUTINE Scan_YAML_Value

  SUBROUTINE Push_Indent(Scanner, NewIndent, RC)
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(IN) :: NewIndent
    INTEGER, INTENT(OUT) :: RC

    IF (Scanner%IndentationDepthIndex < SIZE(Scanner%IndentationDepth)) THEN
      Scanner%IndentationDepthIndex = Scanner%IndentationDepthIndex + 1
      Scanner%IndentationDepth(Scanner%IndentationDepthIndex) = NewIndent
    ELSE
      RC = 1
    END IF
  END SUBROUTINE Push_Indent

  SUBROUTINE Pop_Indent(Scanner, NewIndent, RC)
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(OUT) :: NewIndent
    INTEGER, INTENT(OUT) :: RC

    IF (Scanner%IndentationDepthIndex > 1) THEN
      Scanner%IndentationDepthIndex = Scanner%IndentationDepthIndex - 1
      NewIndent = Scanner%IndentationDepth(Scanner%IndentationDepthIndex)
    ELSE
      RC = 1
    END IF
  END SUBROUTINE Pop_Indent

  SUBROUTINE Push_Token(Scanner, TokenType, TokenValue, RC)
    TYPE(YamlScanner), INTENT(INOUT) :: Scanner
    INTEGER, INTENT(IN) :: TokenType
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: TokenValue
    INTEGER, INTENT(OUT) :: RC

    IF (Scanner%TokenBufferIndex == SIZE(Scanner%TokenBuffer)) THEN
      RC = 1
      RETURN
    END IF

    Scanner%TokenBufferIndex = Scanner%TokenBufferIndex + 1
    Scanner%TokenBuffer(Scanner%TokenBufferIndex)%TokenType = TokenType
    IF (PRESENT(TokenValue)) THEN
      Scanner%TokenBuffer(Scanner%TokenBufferIndex)%TokenValue = TRIM(TokenValue)
    END IF
  END SUBROUTINE Push_Token

  SUBROUTINE Line_End_Index(Line, Index)
    CHARACTER(LEN=*), INTENT(IN) :: Line
    INTEGER, INTENT(OUT) :: Index

    INTEGER :: i

    Index = 0
    DO i = 1, LEN(Line)
      IF (Line(i:i) == "#") THEN
        RETURN
      END IF

      IF (Line(i:i) == "\n" .OR. Line(i:i) == " ") THEN
        CYCLE
      END IF

      Index = i
    END DO
  END SUBROUTINE Line_End_Index

  SUBROUTINE Advance_While_Space(Line, Index)
    CHARACTER(LEN=*), INTENT(IN) :: Line
    INTEGER, INTENT(INOUT) :: Index

    INTEGER :: i

    DO i = Index, LEN(Line)
      IF (Line(i:i) /= " ") THEN
        Index = i
        RETURN
      END IF
    END DO
  END SUBROUTINE Advance_While_Space
END MODULE YAML_Scanner_Mod
