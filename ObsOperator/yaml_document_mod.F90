MODULE YAML_Document_Mod
  USE YAML_Parser_Mod
  USE YAML_Node_Mod

  IMPLICIT NONE
  PRIVATE

  TYPE :: NodePointer
    TYPE(YamlNode), POINTER :: Node
  END TYPE NodePointer

  TYPE, PUBLIC :: YamlDocument
    TYPE(YamlNode), POINTER :: RootNode
    TYPE(NodePointer), ALLOCATABLE :: NodeStack(:)
    INTEGER :: NodeStackIndex
    INTEGER :: NodeStackSize
  END TYPE YamlDocument

  PUBLIC :: Setup_YAML_Document
  PUBLIC :: Free_YAML_Document
  PUBLIC :: Parse_YAML_File
  PUBLIC :: Push_YAML_Parser_Event
  PUBLIC :: Print_YAML_Document

  CONTAINS

  SUBROUTINE Setup_YAML_Document(Document, NodeStackSize)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    INTEGER, INTENT(IN) :: NodeStackSize

    INTEGER :: i

    ALLOCATE(Document%RootNode)
    ALLOCATE(Document%NodeStack(NodeStackSize))

    CALL Setup_Blank_YAML_Node(Document%RootNode)

    Document%NodeStackIndex = 1
    Document%NodeStackSize = NodeStackSize
    Document%NodeStack(1)%Node => Document%RootNode
  END SUBROUTINE Setup_YAML_Document

  SUBROUTINE Free_YAML_Document(Document)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    INTEGER :: i

    CALL Free_YAML_Node(Document%RootNode)
    DEALLOCATE(Document%NodeStack)
  END SUBROUTINE Free_YAML_Document

  SUBROUTINE Parse_YAML_File(Document, InputFile, RC)
    USE File_Mod, ONLY : IOERROR

    TYPE(YamlDocument), INTENT(INOUT) :: Document
    INTEGER, INTENT(IN) :: InputFile
    INTEGER, INTENT(OUT) :: RC

    CHARACTER(LEN=511) :: line
    TYPE(YamlParser) :: Parser

    INTEGER :: i, ioState

    CALL Setup_YAML_Parser(Parser)

    DO
      READ(InputFile, '(A511)', IOSTAT=ioState, ERR=998, END=999) line

      CALL Parse_YAML_Line(Parser, line, RC)
      IF (RC /= 0) THEN
        WRITE(*, '(A, I0)') 'Error parsing line: ', RC
        RETURN
      END IF

      DO i = 1, Parser%EventBufferIndex
        CALL Push_YAML_Parser_Event(Document, Parser%EventBuffer(i), RC)
        IF (RC /= 0) THEN
          WRITE(*, '(A, I0)') 'Error pushing event: ', RC
          RETURN
        END IF
      END DO
      Parser%EventBufferIndex = 0
    END DO

    998 CALL IOERROR(ioState, InputFile, 'Parse_YAML_File:1')

    999 CLOSE(inputFile)

    CALL Finalize_YAML_Parser(Parser, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF
    DO i = 1, Parser%EventBufferIndex
      CALL Push_YAML_Parser_Event(Document, Parser%EventBuffer(i), RC)
      IF (RC /= 0) THEN
        WRITE(*, '(A, I0)') 'Error pushing event: ', RC
        RETURN
      END IF
    END DO
  END SUBROUTINE Parse_YAML_File

  SUBROUTINE Push_YAML_Parser_Event(Document, Event, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    TYPE(YamlParserEvent), INTENT(IN) :: Event
    INTEGER, INTENT(OUT) :: RC
    TYPE(YamlNode), POINTER :: CurrentNode, NewNode

    CurrentNode => Document%NodeStack(Document%NodeStackIndex)%Node

    SELECT CASE (Event%EventType)
    CASE (YAML_PARSER_EVENT_TYPE_KEY)
      CALL Append_YAML_Node(CurrentNode, Key=Event%Value, NewNode=NewNode)
      CALL Push_Node(Document, NewNode, RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_SCALAR)
      IF (CurrentNode%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
        CALL Append_YAML_Node(CurrentNode, NewNode=NewNode)
        CALL Push_Node(Document, NewNode, RC=RC)
        CurrentNode => NewNode
      END IF
      CurrentNode%NodeType = YAML_NODE_TYPE_SCALAR
      CurrentNode%ScalarValue = Event%Value
      CALL Pop_Node(Document, RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_START)
      IF (CurrentNode%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
        CALL Append_YAML_Node(CurrentNode, NewNode=NewNode)
        CALL Push_Node(Document, NewNode, RC=RC)
        CurrentNode => NewNode
      END IF
      CurrentNode%NodeType = YAML_NODE_TYPE_SEQUENCE
    CASE (YAML_PARSER_EVENT_TYPE_SEQUENCE_END)
      CALL Pop_Node(Document, RC=RC)
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_START)
      IF (CurrentNode%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
        CALL Append_YAML_Node(CurrentNode, NewNode=NewNode)
        CALL Push_Node(Document, NewNode, RC=RC)
        CurrentNode => NewNode
      END IF
      CurrentNode%NodeType = YAML_NODE_TYPE_MAPPING
    CASE (YAML_PARSER_EVENT_TYPE_MAPPING_END)
      CALL Pop_Node(Document, RC=RC)
    CASE DEFAULT
      RC = 1
    END SELECT
  END SUBROUTINE Push_YAML_Parser_Event

  SUBROUTINE Print_YAML_Document(Document)
    TYPE(YamlDocument), INTENT(IN) :: Document
    TYPE(YamlNode), POINTER :: Node
    TYPE(YamlNodeListEntry), POINTER :: Entry

    CALL Print_YAML_Node(Document%RootNode, '')
  END SUBROUTINE Print_YAML_Document

  SUBROUTINE Push_Node(Document, Node, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    TYPE(YamlNode), POINTER, INTENT(IN) :: Node
    INTEGER, INTENT(OUT) :: RC

    IF (Document%NodeStackIndex >= Document%NodeStackSize) THEN
      WRITE(*, '(A, I0)') 'error in push', Document%NodeStackIndex
      RC = 1
      RETURN
    END IF

    Document%NodeStackIndex = Document%NodeStackIndex + 1
    Document%NodeStack(Document%NodeStackIndex)%Node => Node
  END SUBROUTINE Push_Node

  SUBROUTINE Pop_Node(Document, RC)
    TYPE(YamlDocument), INTENT(INOUT) :: Document
    INTEGER, INTENT(OUT) :: RC

    IF (Document%NodeStackIndex < 1) THEN
      WRITE(*, '(A, I0)') 'error in pop', Document%NodeStackIndex
      RC = 1
      RETURN
    END IF

    Document%NodeStackIndex = Document%NodeStackIndex - 1
  END SUBROUTINE Pop_Node
END MODULE YAML_Document_Mod
