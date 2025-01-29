MODULE ObsOperator_Output_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Write_ObsOperator_Entry
  PUBLIC :: Finalize_ObsOperator_Output

  INTEGER, PARAMETER :: MAX_FIELDS = 255

  TYPE, PUBLIC :: ObsOperatorOutputFile
    CHARACTER(LEN=511) :: Path
    INTEGER :: FileId = -1

    INTEGER :: CurrentIdIndex = 1
    INTEGER :: CurrentSampleIndex = 1
    CHARACTER(LEN=MAX_ENTRY_FIELD_NAME_LENGTH) :: IncludedFieldNames(MAX_FIELDS)
    INTEGER :: IncludedFieldsIndex = 0
  END TYPE ObsOperatorOutputFile

  CONTAINS

  SUBROUTINE Write_ObsOperator_Entry(OutputFile, Entry)
    USE ErrCode_Mod
    USE m_netcdf_io_write
    USE ObsOperator_Entry_Mod, ONLY : ObsOperatorEntry, MAX_ENTRY_ID_LENGTH, MAX_ENTRY_FIELD_NAME_LENGTH

    TYPE(ObsOperatorOutputFile), INTENT(INOUT) :: OutputFile
    TYPE(ObsOperatorEntry), INTENT(IN) :: Entry

    INTEGER :: i, j
    INTEGER :: nFields
    CHARACTER(LEN=1) :: outputId(MAX_ENTRY_ID_LENGTH, 1)
    INTEGER, ALLOCATABLE :: outputIdIndex(:), outputFieldIndex(:)

    ! Create output file on first write
    IF (OutputFile%FileId < 0) THEN
      CALL Create_ObsOperator_Output(OutputFile)
    END IF

    ! Convert id to character array
    outputId = ACHAR(0)
    DO i = 1, LEN_TRIM(Entry%Id)
      outputId(i, 1) = Entry%Id(i:i)
    END DO

    nFields = SIZE(Entry%FieldSpeciesIndex)

    ! Add ids to the id index
    ALLOCATE(outputIdIndex(nFields))
    outputIdIndex = OutputFile%CurrentIdIndex

    ! Add fields to the output file and index into the list
    ALLOCATE(outputFieldIndex(nFields))
    DO i = 1, nFields
      DO j = 1, OutputFile%IncludedFieldsIndex
        IF (OutputFile%IncludedFieldNames(j) == Entry%FieldName(i)) THEN
          outputFieldIndex(i) = j
          EXIT
        END IF
      END DO
      IF (j > OutputFile%IncludedFieldsIndex) THEN
        OutputFile%IncludedFieldsIndex = OutputFile%IncludedFieldsIndex + 1
        OutputFile%IncludedFieldNames(OutputFile%IncludedFieldsIndex) = Entry%FieldName(i)
        outputFieldIndex(i) = OutputFile%IncludedFieldsIndex
      END IF
    END DO


    CALL NcWr(outputId, OutputFile%FileId, 'id', (/ 1, OutputFile%CurrentIdIndex /), (/ MAX_ENTRY_ID_LENGTH, 1 /))
    CALL NcWr(outputIdIndex, OutputFile%FileId, 'id_index', (/ OutputFile%CurrentSampleIndex /), (/ nFields /))
    CALL NcWr(outputFieldIndex, OutputFile%FileId, 'field_index', (/ OutputFile%CurrentSampleIndex /), (/ nFields /))
    CALL NcWr(Entry%FieldValue, OutputFile%FileId, 'sample', (/ OutputFile%CurrentSampleIndex /), (/ nFields /))

    DEALLOCATE(outputIdIndex)
    DEALLOCATE(outputFieldIndex)

    OutputFile%CurrentIdIndex = OutputFile%CurrentIdIndex + 1
    OutputFile%CurrentSampleIndex = OutputFile%CurrentSampleIndex + nFields
  END SUBROUTINE Write_ObsOperator_Entry

  SUBROUTINE Finalize_ObsOperator_Output(OutputFile)
    USE m_netcdf_io_write
    USE m_netcdf_io_close

    TYPE(ObsOperatorOutputFile), INTENT(INOUT) :: OutputFile

    CHARACTER(LEN=1), ALLOCATABLE :: outputFieldNames(:, :)
    INTEGER :: i, j, nFields

    IF (OutputFile%FileId < 0) THEN
      RETURN
    END IF

    nFields = OutputFile%IncludedFieldsIndex

    ALLOCATE(outputFieldNames(MAX_ENTRY_FIELD_NAME_LENGTH, nFields))
    outputFieldNames = ACHAR(0)
    DO i = 1, nFields
      DO j = 1, LEN_TRIM(OutputFile%IncludedFieldNames(i))
        outputFieldNames(j, i) = OutputFile%IncludedFieldNames(i)(j:j)
      END DO
    END DO
    CALL NcWr(outputFieldNames, OutputFile%FileId, 'field', (/ 1, 1 /), (/ MAX_ENTRY_FIELD_NAME_LENGTH, nFields /))

    CALL NcCl( OutputFile%FileId )
    OutputFile%FileId = -1
  END SUBROUTINE Finalize_ObsOperator_Output

  SUBROUTINE Create_ObsOperator_Output(OutputFile)
    USE ErrCode_Mod
    USE Error_Mod, ONLY : Error_Stop
    USE NetCDF
    USE m_netcdf_io_define

    TYPE(ObsOperatorOutputFile), INTENT(INOUT) :: OutputFile

    INTEGER :: ierr
    INTEGER :: tmp
    ! Number of entries and how many characters needed to store the id
    INTEGER :: dimensionIdEntries
    INTEGER :: dimensionIdIdChars
    ! Number of fields and how many characters needed to store the field name
    INTEGER :: dimensionIdFields
    INTEGER :: dimensionIdFieldChars
    INTEGER :: currentVariableId
    ! Number of samples (equal to number of entries * number of fields)
    INTEGER :: dimensionIdSamples

    OutputFile%FileId = -1
    ! NOTE: Using this instead of NcCr_Wr because the latter does not support
    ! multiple unlimited dimensions
    ierr = NF90_Create(OutputFile%Path, NF90_NETCDF4, OutputFile%FileId)
    IF (ierr /= NF90_NOERR .OR. OutputFile%FileId < 0) THEN
      CALL Error_Stop( "Error creating netCDF file", "" )
    END IF

    CALL NcSetFill(OutputFile%FileId, NF90_NOFILL, tmp)

    CALL NcDef_Dimension(OutputFile%FileId, 'entries', NF90_UNLIMITED, dimensionIdEntries)
    CALL NcDef_Dimension(OutputFile%FileId, 'id_chars', MAX_ENTRY_ID_LENGTH, dimensionIdIdChars)
    CALL NcDef_Dimension(OutputFile%FileId, 'fields', NF90_UNLIMITED, dimensionIdFields)
    CALL NcDef_Dimension(OutputFile%FileId, 'field_chars', MAX_ENTRY_FIELD_NAME_LENGTH, dimensionIdFieldChars)
    CALL NcDef_Dimension(OutputFile%FileId, 'samples', NF90_UNLIMITED, dimensionIdSamples)

    CALL NcDef_Variable(OutputFile%FileId, 'id', NF90_CHAR, 2, (/dimensionIdIdChars, dimensionIdEntries/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'ids')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'description', 'id')

    CALL NcDef_Variable(OutputFile%FileId, 'field', NF90_CHAR, 2, (/dimensionIdFieldChars, dimensionIdFields/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'fields')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'description', 'field name')

    CALL NcDef_Variable(OutputFile%FileId, 'id_index', NF90_INT, 1, (/dimensionIdSamples/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'id_index')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'description', 'index of the id in the id list')

    CALL NcDef_Variable(OutputFile%FileId, 'field_index', NF90_INT, 1, (/dimensionIdSamples/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'field_index')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'description', 'index of the field in the field list')

    CALL NcDef_Variable(OutputFile%FileId, 'sample', NF90_FLOAT, 1, (/dimensionIdSamples/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'samples')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'description', 'sample of the id and field')

    CALL NcEnd_def(OutputFile%FileId)
  END SUBROUTINE Create_ObsOperator_Output
END MODULE ObsOperator_Output_Mod
