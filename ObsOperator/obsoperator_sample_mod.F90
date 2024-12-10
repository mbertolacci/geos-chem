MODULE ObsOperator_Sample_Mod
  USE PRECISION_MOD
  USE ObsOperator_Entry_Mod

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: Sample_ObsOperatorEntry

  CONTAINS

  SUBROUTINE Sample_ObsOperatorEntry(Entry, Current_Time_Index, Input_Opt, State_Chm, State_Grid, State_Met)
    USE Input_Opt_Mod, ONLY : OptInput
    USE State_Chm_Mod, ONLY : ChmState
    USE State_Grid_Mod, ONLY : GrdState
    USE State_Met_Mod, ONLY : MetState
    USE PhysConstants, ONLY : AIRMW

    TYPE(ObsOperatorEntry), INTENT(INOUT) :: Entry
    INTEGER, INTENT(IN) :: Current_Time_Index
    TYPE(OptInput), INTENT(IN) :: Input_Opt
    TYPE(ChmState), INTENT(IN) :: State_Chm
    TYPE(GrdState), INTENT(IN) :: State_Grid
    TYPE(MetState), INTENT(IN) :: State_Met

    INTEGER :: I
    INTEGER :: J
    INTEGER :: K
    INTEGER :: nLevelIndices
    INTEGER :: matchingTimePoint
    REAL(f8) :: currentSpeciesValue
    REAL(f8) :: currentCellValue
    REAL(f8) :: currentVerticalWeight
    REAL(f8) :: verticalWeightDenominator
    INTEGER :: currentSpeciesIndex
    INTEGER :: currentHorizI
    INTEGER :: currentHorizJ
    INTEGER :: currentLevelIndex
    INTEGER :: verticalRangeStart, verticalRangeEnd
    LOGICAL :: isPrintLog

    DO matchingTimePoint = 1, SIZE(Entry%TimeIndices)
      IF (Entry%TimeIndices(matchingTimePoint) == Current_Time_Index) THEN
        EXIT
      END IF
    END DO

    IF (matchingTimePoint > SIZE(Entry%TimeIndices)) THEN
      RETURN
    END IF

    isPrintLog = (Input_Opt%amIRoot .and. Input_Opt%ObsOperator_Verbose )

    IF (isPrintLog) THEN
      WRITE(*, '(A, A)') '- Sampling id = ', Entry%Id
    END IF

    ! Loop through each species
    DO I = 1, SIZE(Entry%SpeciesIndex)
      currentSpeciesValue = 0.0_f8
      currentSpeciesIndex = Entry%SpeciesIndex(I)

      IF (isPrintLog) THEN
        WRITE(*, '(A, A)') '  + Species = ', State_Chm%SpcData(currentSpeciesIndex)%Info%Name
      END IF

      DO J = 1, SIZE(Entry%HorizontalIndices, 1)
        currentHorizI = Entry%HorizontalIndices(J, 1)
        currentHorizJ = Entry%HorizontalIndices(J, 2)        
        currentCellValue = 0.0_f8

        ! Do vertical averaging
        IF (Entry%VerticalOperatorType == VERTICAL_OPERATOR_TYPE_RANGE) THEN
          IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE) THEN
            ! NOTE(mgnb): end of pressure range => lower index
            CALL Vertical_Index_From_Pressure(Entry%VerticalRangeEnd, currentHorizI, currentHorizJ, State_Grid, State_Met, verticalRangeStart)
            CALL Vertical_Index_From_Pressure(Entry%VerticalRangeStart, currentHorizI, currentHorizJ, State_Grid, State_Met, verticalRangeEnd)
          ELSE IF (Entry%VerticalUnit == VERTICAL_UNIT_ALTITUDE) THEN
            CALL Vertical_Index_From_Altitude(Entry%VerticalRangeStart, currentHorizI, currentHorizJ, State_Grid, State_Met, verticalRangeStart)
            CALL Vertical_Index_From_Altitude(Entry%VerticalRangeEnd, currentHorizI, currentHorizJ, State_Grid, State_Met, verticalRangeEnd)
          ELSE
            verticalRangeStart = Entry%VerticalRangeStartIndex
            verticalRangeEnd = Entry%VerticalRangeEndIndex
          ENDIF

          IF ( &
            Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_NORMALIZED &
            .OR. Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE &
          ) THEN
            verticalWeightDenominator = 0.0_f8
          ELSE
            verticalWeightDenominator = 1.0_f8
          END IF

          DO currentLevelIndex = verticalRangeStart, verticalRangeEnd
            IF ( &
              Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE &
              .OR. Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_PRESSURE &
            ) THEN
              currentVerticalWeight = State_Met%DELP(currentHorizI, currentHorizJ, currentLevelIndex)
            ELSE
              currentVerticalWeight = 1.0_f8
            END IF

            IF ( &
              Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_NORMALIZED &
              .OR. Entry%VerticalOperatorWeight == VERTICAL_OPERATOR_WEIGHT_NORMALIZED_PRESSURE &
            ) THEN
              verticalWeightDenominator = verticalWeightDenominator + currentVerticalWeight
            END IF

            IF ( isPrintLog ) THEN
              WRITE(*, '(A, I0, A, I0, A, I0, A)') '    + Querying (I, J, K) = (', currentHorizI, ', ', currentHorizJ, ', ', currentLevelIndex, ')'
            END IF

            currentCellValue = currentCellValue + currentVerticalWeight * State_Chm%Species( &
              currentSpeciesIndex &
            )%Conc( &
              currentHorizI, &
              currentHorizJ, &
              currentLevelIndex &
            )
          END DO

          currentCellValue = currentCellValue / verticalWeightDenominator
        ELSE IF (Entry%VerticalOperatorType == VERTICAL_OPERATOR_TYPE_EXACT) THEN
          nLevelIndices = SIZE(Entry%VerticalExactWeight)
          DO K = 1, nLevelIndices
            IF (Entry%VerticalUnit == VERTICAL_UNIT_PRESSURE) THEN
              CALL Vertical_Index_From_Pressure(Entry%VerticalExact(K), currentHorizI, currentHorizJ, State_Grid, State_Met, currentLevelIndex)
            ELSE IF (Entry%VerticalUnit == VERTICAL_UNIT_ALTITUDE) THEN
              CALL Vertical_Index_From_Altitude(Entry%VerticalExact(K), currentHorizI, currentHorizJ, State_Grid, State_Met, currentLevelIndex)
            ELSE
              currentLevelIndex = Entry%VerticalExactIndex(K)
            END IF
  
            IF ( isPrintLog ) THEN
              WRITE(*, '(A, I0, A, I0, A, I0, A)') '    + Querying (I, J, K) = (', currentHorizI, ', ', currentHorizJ, ', ', currentLevelIndex, ')'
            END IF

            currentVerticalWeight = Entry%VerticalExactWeight(K)
            currentCellValue = currentCellValue + currentVerticalWeight * State_Chm%Species( &
              currentSpeciesIndex &
            )%Conc( &
              currentHorizI, &
              currentHorizJ, &
              currentLevelIndex &
            )
          END DO
        END IF

        currentSpeciesValue = ( &
          currentSpeciesValue &
          + Entry%HorizontalWeights(J) * currentCellValue &
        )
      END DO

      currentSpeciesValue = currentSpeciesValue * (AIRMW / State_Chm%SpcData(currentSpeciesIndex)%Info%MW_g)

      Entry%SpeciesValue(I) = ( &
        Entry%SpeciesValue(I) &
        + Entry%TimeWeights(matchingTimePoint) * currentSpeciesValue &
      )
    END DO
  END SUBROUTINE Sample_ObsOperatorEntry



  SUBROUTINE Vertical_Index_From_Altitude(Altitude, I, J, State_Grid, State_Met, Index)
    USE State_Grid_Mod, ONLY : GrdState
    USE State_Met_Mod,  ONLY : MetState

    REAL(f8), INTENT(IN) :: Altitude
    INTEGER, INTENT(IN) :: I
    INTEGER, INTENT(IN) :: J
    TYPE(GrdState), INTENT(IN) :: State_Grid  ! Grid State object
    TYPE(MetState), INTENT(IN) :: State_Met   ! Meteorology State object
    INTEGER, INTENT(OUT) :: Index
    REAL(f8) :: Z

    Z = 0.0_f8
    DO Index = 1, State_Grid%NZ
       Z = Z + State_Met%BXHEIGHT(I,J,Index)
       IF ( Z >= Altitude ) RETURN
    ENDDO
  END SUBROUTINE Vertical_Index_From_Altitude

  SUBROUTINE Vertical_Index_From_Pressure(Pressure, I, J, State_Grid, State_Met, Index)
    USE State_Grid_Mod, ONLY : GrdState
    USE State_Met_Mod,  ONLY : MetState

    REAL(f8), INTENT(IN) :: Pressure
    INTEGER, INTENT(IN) :: I
    INTEGER, INTENT(IN) :: J
    TYPE(GrdState), INTENT(IN) :: State_Grid  ! Grid State object
    TYPE(MetState), INTENT(IN) :: State_Met   ! Meteorology State object
    INTEGER, INTENT(OUT) :: Index

    INTEGER :: K

    Index = State_Grid%NZ
    DO K = 1, State_Grid%NZ
       IF (State_Met%PEDGE(I,J,K) <= Pressure) THEN
          Index = MAX(K - 1, 1)
          EXIT
       ENDIF
    ENDDO
  END SUBROUTINE Vertical_Index_From_Pressure
END MODULE ObsOperator_Sample_Mod
