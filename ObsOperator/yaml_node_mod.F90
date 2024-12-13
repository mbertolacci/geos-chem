MODULE YAML_Node_Mod
  USE Precision_Mod

  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: YAML_MAX_SCALAR_LENGTH = 255

  INTEGER, PARAMETER :: YAML_NODE_TYPE_BLANK = 0
  INTEGER, PARAMETER :: YAML_NODE_TYPE_SCALAR = 1
  INTEGER, PARAMETER :: YAML_NODE_TYPE_SEQUENCE = 2
  INTEGER, PARAMETER :: YAML_NODE_TYPE_MAPPING = 3

  TYPE, PUBLIC :: YamlNode
    INTEGER :: NodeType
    TYPE(YamlNodeListEntry), POINTER :: FirstEntry
    TYPE(YamlNodeListEntry), POINTER :: LastEntry
    INTEGER :: SequenceLength
    CHARACTER(LEN=YAML_MAX_SCALAR_LENGTH) :: ScalarValue
  END TYPE YamlNode

  TYPE, PUBLIC :: YamlNodeListEntry
    CHARACTER(LEN=YAML_MAX_SCALAR_LENGTH) :: Key
    TYPE(YamlNode), POINTER :: Node
    TYPE(YamlNodeListEntry), POINTER :: Next
  END TYPE YamlNodeListEntry

  PUBLIC :: YAML_NODE_TYPE_BLANK
  PUBLIC :: YAML_NODE_TYPE_SCALAR
  PUBLIC :: YAML_NODE_TYPE_SEQUENCE
  PUBLIC :: YAML_NODE_TYPE_MAPPING
  PUBLIC :: Setup_Blank_YAML_Node
  PUBLIC :: Append_YAML_Node
  PUBLIC :: Free_YAML_Node
  PUBLIC :: Print_YAML_Node
  PUBLIC :: Get_YAML_Key
  PUBLIC :: Get_YAML_Scalars_Size_At_Key
  PUBLIC :: Get_YAML_Scalars_Size
  PUBLIC :: Get_YAML_Char_Scalars
  PUBLIC :: Get_YAML_Char_Scalar_At_Key
  PUBLIC :: Get_YAML_Real_Scalar_At_Key
  PUBLIC :: Get_YAML_Real_Scalars_At_Key
  PUBLIC :: Get_YAML_Int_Scalars_At_Key
  PUBLIC :: Get_YAML_Int_Scalar_At_Key
  CONTAINS

  SUBROUTINE Setup_Blank_YAML_Node(Node)
    TYPE(YamlNode), INTENT(INOUT) :: Node

    Node%NodeType = YAML_NODE_TYPE_BLANK
    Node%SequenceLength = 0
    NULLIFY(Node%FirstEntry)
    NULLIFY(Node%LastEntry)
  END SUBROUTINE Setup_Blank_YAML_Node

  SUBROUTINE Append_YAML_Node(Node, Key, NewNode)
    TYPE(YamlNode), INTENT(INOUT) :: Node
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: Key
    TYPE(YamlNode), POINTER, INTENT(OUT) :: NewNode
    TYPE(YamlNodeListEntry), POINTER :: NewEntry

    ALLOCATE(NewNode)
    CALL Setup_Blank_YAML_Node(NewNode)

    ALLOCATE(NewEntry)
    IF (PRESENT(Key)) THEN
      NewEntry%Key = Key
    END IF
    NewEntry%Node => NewNode
    
    IF (ASSOCIATED(Node%LastEntry)) THEN
      Node%LastEntry%Next => NewEntry
      Node%LastEntry => NewEntry
    ELSE
      Node%FirstEntry => NewEntry
      Node%LastEntry => NewEntry
    END IF

    Node%SequenceLength = Node%SequenceLength + 1
  END SUBROUTINE Append_YAML_Node

  RECURSIVE SUBROUTINE Free_YAML_Node(Node)
    TYPE(YamlNode), INTENT(INOUT) :: Node
    TYPE(YamlNodeListEntry), POINTER :: Entry
    TYPE(YamlNodeListEntry), POINTER :: NextEntry

    Entry => Node%FirstEntry
    DO WHILE (ASSOCIATED(Entry))
      CALL Free_YAML_Node(Entry%Node)
      NextEntry => Entry%Next
      DEALLOCATE(Entry%Node)
      DEALLOCATE(Entry)
      Entry => NextEntry
    END DO
  END SUBROUTINE Free_YAML_Node

  RECURSIVE SUBROUTINE Print_YAML_Node(Node, DepthString)
    TYPE(YamlNode), POINTER :: Node
    CHARACTER(LEN=*), INTENT(IN) :: DepthString
    TYPE(YamlNodeListEntry), POINTER :: Entry

    IF (Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
      PRINT *, DepthString, Node%ScalarValue
      RETURN
    END IF

    Entry => Node%FirstEntry
    DO WHILE (ASSOCIATED(Entry))
      IF (Node%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
        IF (Entry%Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
          PRINT *, DepthString, '- ', TRIM(Entry%Node%ScalarValue)
        ELSE
          PRINT *, DepthString, '- '
          CALL Print_YAML_Node(Entry%Node, DepthString // '  ')
        END IF
      ELSE IF (Node%NodeType == YAML_NODE_TYPE_MAPPING) THEN
        IF (Entry%Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
          PRINT *, DepthString, TRIM(Entry%Key), ': ', TRIM(Entry%Node%ScalarValue)
        ELSE
          PRINT *, DepthString, TRIM(Entry%Key), ':'
          CALL Print_YAML_Node(Entry%Node, DepthString // '  ')
        END IF
      END IF
      Entry => Entry%Next
    END DO
  END SUBROUTINE Print_YAML_Node

  SUBROUTINE Get_YAML_Key(Node, Key, OutputNode, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    TYPE(YamlNode), POINTER, INTENT(OUT) :: OutputNode
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNodeListEntry), POINTER :: Entry

    IF (Node%NodeType /= YAML_NODE_TYPE_MAPPING) THEN
      RC = 1
      RETURN
    END IF

    Entry => Node%FirstEntry
    DO WHILE (ASSOCIATED(Entry))
      IF (Entry%Key == Key) THEN
        OutputNode => Entry%Node
        RC = 0
        RETURN
      END IF
      Entry => Entry%Next
    END DO

    RC = 1
  END SUBROUTINE Get_YAML_Key

  SUBROUTINE Get_YAML_Scalars_Size_At_Key(Node, Key, OutputSize, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    INTEGER, INTENT(OUT) :: OutputSize
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    CALL Get_YAML_Scalars_Size(subNode, OutputSize, RC)
  END SUBROUTINE Get_YAML_Scalars_Size_At_Key

  SUBROUTINE Get_YAML_Scalars_Size(Node, OutputSize, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    INTEGER, INTENT(OUT) :: OutputSize
    INTEGER, INTENT(OUT) :: RC

    IF (Node%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
      OutputSize = Node%SequenceLength
    ELSE IF (Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
      OutputSize = 1
    ELSE
      RC = 1
    END IF
  END SUBROUTINE Get_YAML_Scalars_Size

  SUBROUTINE Get_YAML_Int_Scalar_At_Key(Node, Key, Output, DefaultValue, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    INTEGER, INTENT(OUT) :: Output
    INTEGER, OPTIONAL, INTENT(IN) :: DefaultValue
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      IF (PRESENT(DefaultValue)) THEN
        Output = DefaultValue
        RC = 0
      ELSE
        RETURN
      END IF
    END IF

    CALL Get_YAML_Int_Scalar(subNode, Output, RC)
  END SUBROUTINE Get_YAML_Int_Scalar_At_Key

  SUBROUTINE Get_YAML_Int_Scalars_At_Key(Node, Key, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    INTEGER, INTENT(OUT) :: Output(:)
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    CALL Get_YAML_Int_Scalars(subNode, Output, RC)
  END SUBROUTINE Get_YAML_Int_Scalars_At_Key

  SUBROUTINE Get_YAML_Int_Scalar(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    INTEGER, INTENT(OUT) :: Output
    INTEGER, INTENT(OUT) :: RC

    IF (Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
      RC = 1
      RETURN
    END IF

    READ(Node%ScalarValue, *) Output
  END SUBROUTINE Get_YAML_Int_Scalar

  SUBROUTINE Get_YAML_Int_Scalars(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    INTEGER, INTENT(OUT) :: Output(:)
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: outputSize
    TYPE(YamlNodeListEntry), POINTER :: Entry
    INTEGER :: i

    IF (Node%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
      IF (Node%SequenceLength > SIZE(Output)) THEN
        RC = 1
        RETURN
      END IF

      Entry => Node%FirstEntry
      i = 1
      DO WHILE (ASSOCIATED(Entry))
        IF (Entry%Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
          RC = 1
          RETURN
        END IF

        READ(Entry%Node%ScalarValue, *) Output(i)
        i = i + 1
        Entry => Entry%Next
      END DO
    ELSE IF (Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
      IF (SIZE(Output) /= 1) THEN
        RC = 1
        RETURN
      END IF

      READ(Node%ScalarValue, *) Output(1)
    ELSE
      RETURN
    END IF
  END SUBROUTINE Get_YAML_Int_Scalars

  SUBROUTINE Get_YAML_Real_Scalar_At_Key(Node, Key, Output, DefaultValue, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    REAL(fp), INTENT(OUT) :: Output
    REAL(fp), OPTIONAL, INTENT(IN) :: DefaultValue
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      IF (PRESENT(DefaultValue)) THEN
        Output = DefaultValue
        RC = 0
      ELSE
        RETURN
      END IF
    END IF

    CALL Get_YAML_Real_Scalar(subNode, Output, RC)
  END SUBROUTINE Get_YAML_Real_Scalar_At_Key

  SUBROUTINE Get_YAML_Real_Scalars_At_Key(Node, Key, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    REAL(fp), INTENT(OUT) :: Output(:)
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      RETURN
    END IF

    CALL Get_YAML_Real_Scalars(subNode, Output, RC)
  END SUBROUTINE Get_YAML_Real_Scalars_At_Key

  SUBROUTINE Get_YAML_Real_Scalar(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    REAL(fp), INTENT(OUT) :: Output
    INTEGER, INTENT(OUT) :: RC

    IF (Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
      RC = 1
      RETURN
    END IF

    READ(Node%ScalarValue, *) Output
  END SUBROUTINE Get_YAML_Real_Scalar

  SUBROUTINE Get_YAML_Real_Scalars(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    REAL(fp), INTENT(OUT) :: Output(:)
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: outputSize
    TYPE(YamlNodeListEntry), POINTER :: Entry
    INTEGER :: i

    IF (Node%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
      IF (Node%SequenceLength > SIZE(Output)) THEN
        RC = 1
        RETURN
      END IF

      Entry => Node%FirstEntry
      i = 1
      DO WHILE (ASSOCIATED(Entry))
        IF (Entry%Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
          RC = 1
          RETURN
        END IF

        READ(Entry%Node%ScalarValue, *) Output(i)
        i = i + 1
        Entry => Entry%Next
      END DO
    ELSE IF (Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
      IF (SIZE(Output) /= 1) THEN
        RC = 1
        RETURN
      END IF

      READ(Node%ScalarValue, *) Output(1)
    ELSE
      RETURN
    END IF
  END SUBROUTINE Get_YAML_Real_Scalars

  SUBROUTINE Get_YAML_Char_Scalar_At_Key(Node, Key, Output, DefaultValue, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(IN) :: Key
    CHARACTER(LEN=*), INTENT(OUT) :: Output
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: DefaultValue
    INTEGER, INTENT(OUT) :: RC

    TYPE(YamlNode), POINTER :: subNode

    CALL Get_YAML_Key(Node, Key, subNode, RC)
    IF (RC /= 0) THEN
      IF (PRESENT(DefaultValue)) THEN
        Output = DefaultValue
        RC = 0
      END IF
      RETURN
    END IF

    CALL Get_YAML_Char_Scalar(subNode, Output, RC)
  END SUBROUTINE Get_YAML_Char_Scalar_At_Key

  SUBROUTINE Get_YAML_Char_Scalar(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(OUT) :: Output
    INTEGER, INTENT(OUT) :: RC

    IF (Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
      RC = 1
      RETURN
    END IF

    Output = Node%ScalarValue
  END SUBROUTINE Get_YAML_Char_Scalar

  SUBROUTINE Get_YAML_Char_Scalars(Node, Output, RC)
    TYPE(YamlNode), INTENT(IN) :: Node
    CHARACTER(LEN=*), INTENT(OUT) :: Output(:)
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: outputSize
    TYPE(YamlNodeListEntry), POINTER :: Entry
    INTEGER :: i

    IF (Node%NodeType == YAML_NODE_TYPE_SEQUENCE) THEN
      IF (Node%SequenceLength > SIZE(Output)) THEN
        RC = 1
        RETURN
      END IF

      Entry => Node%FirstEntry
      i = 1
      DO WHILE (ASSOCIATED(Entry))
        IF (Entry%Node%NodeType /= YAML_NODE_TYPE_SCALAR) THEN
          RC = 1
          RETURN
        END IF

        Output(i) = Entry%Node%ScalarValue
        i = i + 1
        Entry => Entry%Next
      END DO
    ELSE IF (Node%NodeType == YAML_NODE_TYPE_SCALAR) THEN
      IF (SIZE(Output) /= 1) THEN
        RC = 1
        RETURN
      END IF

      Output(1) = Node%ScalarValue
    ELSE
      RETURN
    END IF
  END SUBROUTINE Get_YAML_Char_Scalars
END MODULE YAML_Node_Mod


