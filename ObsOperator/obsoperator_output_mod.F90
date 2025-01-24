MODULE ObsOperator_Output_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Create_ObsOperator_Output
  PUBLIC :: Write_ObsOperator_Entry
  PUBLIC :: Close_ObsOperator_Output

  TYPE, PUBLIC :: ObsOperatorOutputFile
    INTEGER :: FileId = -1
    INTEGER :: CurrentIndex = 1
  END TYPE ObsOperatorOutputFile

  CONTAINS

  SUBROUTINE Create_ObsOperator_Output(Path, OutputFile, RC)
    USE ErrCode_Mod
    USE Error_Mod, ONLY : Error_Stop
    USE NetCDF
    USE m_netcdf_io_define
    USE m_netcdf_io_create
    USE ObsOperator_Entry_Mod, ONLY : MAX_ENTRY_ID_LENGTH, MAX_ENTRY_FIELD_NAME_LENGTH
    CHARACTER(LEN=*), INTENT(IN) :: Path
    TYPE(ObsOperatorOutputFile), INTENT(OUT) :: OutputFile
    INTEGER, INTENT(OUT) :: RC

    INTEGER :: tmp
    INTEGER :: dimensionIdValues
    INTEGER :: dimensionIdIds
    INTEGER :: dimensionIdFields
    INTEGER :: currentVariableId

    OutputFile%CurrentIndex = 1

    OutputFile%FileId = -1
    CALL NcCr_Wr( OutputFile%FileId, Path, WRITE_NC4=.TRUE. )
    IF ( OutputFile%FileId < 0 ) THEN
      CALL Error_Stop( "Invalid netCDF file id", "" )
      RETURN
    ENDIF

    CALL NcSetFill(OutputFile%FileId, NF90_NOFILL, tmp)

    CALL NcDef_Dimension(OutputFile%FileId, 'values', NF90_UNLIMITED, dimensionIdValues)
    CALL NcDef_Dimension(OutputFile%FileId, 'ids', MAX_ENTRY_ID_LENGTH, dimensionIdIds)
    CALL NcDef_Dimension(OutputFile%FileId, 'fields', MAX_ENTRY_FIELD_NAME_LENGTH, dimensionIdFields)

    CALL NcDef_Variable(OutputFile%FileId, 'value', NF90_FLOAT, 1, (/dimensionIdValues/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'values')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', 'v/v')

    CALL NcDef_Variable(OutputFile%FileId, 'id', NF90_CHAR, 2, (/dimensionIdIds, dimensionIdValues/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'ids')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')

    CALL NcDef_Variable(OutputFile%FileId, 'field', NF90_CHAR, 2, (/dimensionIdFields, dimensionIdValues/), currentVariableId, compress=.TRUE.)
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'long_name', 'fields')
    CALL NcDef_Var_Attributes(OutputFile%FileId, currentVariableId, 'units', '1')

    CALL NcEnd_def(OutputFile%FileId)
  END SUBROUTINE Create_ObsOperator_Output

  SUBROUTINE Write_ObsOperator_Entry(OutputFile, Entry)
    USE m_netcdf_io_write
    USE ObsOperator_Entry_Mod, ONLY : ObsOperatorEntry, MAX_ENTRY_ID_LENGTH, MAX_ENTRY_FIELD_NAME_LENGTH

    TYPE(ObsOperatorOutputFile), INTENT(INOUT) :: OutputFile
    TYPE(ObsOperatorEntry), INTENT(IN) :: Entry

    INTEGER :: i, j
    INTEGER :: nOutputs
    INTEGER :: length
    CHARACTER(LEN=1), ALLOCATABLE :: outputIds(:, :)
    CHARACTER(LEN=1), ALLOCATABLE :: outputFieldNames(:, :)

    nOutputs = SIZE(Entry%FieldSpeciesIndex)

    ! Convert strings to character arrays
    ALLOCATE(outputIds(MAX_ENTRY_ID_LENGTH, nOutputs))
    ALLOCATE(outputFieldNames(MAX_ENTRY_FIELD_NAME_LENGTH, nOutputs))
    outputIds = ACHAR(0)
    outputFieldNames = ACHAR(0)
    DO i = 1, nOutputs
      length = LEN_TRIM(Entry%Id)
      DO j = 1, length
        outputIds(j, i) = Entry%Id(j:j)
      END DO
      length = LEN_TRIM(Entry%FieldName(i))
      DO j = 1, length
        outputFieldNames(j, i) = Entry%FieldName(i)(j:j)
      END DO
    END DO

    CALL NcWr(outputIds, OutputFile%FileId, 'id', (/ 1, OutputFile%CurrentIndex /), (/ MAX_ENTRY_ID_LENGTH, nOutputs /))
    CALL NcWr(outputFieldNames, OutputFile%FileId, 'field', (/ 1, OutputFile%CurrentIndex /), (/ MAX_ENTRY_FIELD_NAME_LENGTH, nOutputs /))
    CALL NcWr(Entry%FieldValue, OutputFile%FileId, 'value', (/ OutputFile%CurrentIndex /), (/ nOutputs /))

    DEALLOCATE(outputIds)
    DEALLOCATE(outputFieldNames)

    OutputFile%CurrentIndex = OutputFile%CurrentIndex + nOutputs 
  END SUBROUTINE Write_ObsOperator_Entry

  SUBROUTINE Close_ObsOperator_Output(OutputFile)
    USE m_netcdf_io_close
    TYPE(ObsOperatorOutputFile), INTENT(INOUT) :: OutputFile

    CALL NcCl( OutputFile%FileId )
  END SUBROUTINE Close_ObsOperator_Output
END MODULE ObsOperator_Output_Mod
