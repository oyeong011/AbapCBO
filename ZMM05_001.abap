*&---------------------------------------------------------------------*
*& Report ZMM05_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZMM05_001 MESSAGE-ID ZMED05.

INCLUDE ZMM05_001_TOP.
INCLUDE ZMM05_001_SCR.
INCLUDE ZMM05_001_F01.
INCLUDE ZMM05_001_PBO.
INCLUDE ZMM05_001_PAI.


INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.
  PERFORM SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM CHECK_PARAM.
  IF P_CRE = C_X.
    PERFORM PREPARE_CREATE_MODE.
    CALL SCREEN 100.
  ELSEIF P_VIEW = C_X.
    PERFORM GET_DATA.
    IF GV_LIFNR IS NOT INITIAL.
      CALL SCREEN 100.
    ELSE.
      MESSAGE I001.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&  Include           ZMM05_001_TOP
*&---------------------------------------------------------------------*

TABLES : ZEDT05_LFA1, ZEDT05_LFB1, ZEDT05_LFM1.

CONSTANTS: C_X           TYPE C VALUE 'X'.
DATA : OK_CODE TYPE SY-UCOMM.
" 1. Key Fields
DATA : GV_LIFNR LIKE ZEDT05_LFA1-LIFNR,  " 구매처 번호 (채번 및 조회용)
       GV_BUKRS LIKE ZEDT05_LFB1-BUKRS,  " 회사 코드
       GV_KTOKK LIKE ZEDT05_LFA1-KTOKK,  " 계정 그룹 (화면 제어용 필수)
" 2. LFA1 (일반 데이터)
       GV_NAME1 LIKE ZEDT05_LFA1-NAME1,  " 구매처명
       GV_LAND1 LIKE ZEDT05_LFA1-LAND1,  " 국가
       GV_STCD1 LIKE ZEDT05_LFA1-STCD1,  " 개인번호 (3000 그룹용)
       GV_STCD2 LIKE ZEDT05_LFA1-STCD2,  " 사업자번호
       GV_STRAS LIKE ZEDT05_LFA1-STRAS,  " 주소
" 3. LFB1 (회사코드 데이터)
       GV_LOEVM LIKE ZEDT05_LFB1-LOEVM,  " 삭제지시자
       GV_AKONT LIKE ZEDT05_LFB1-AKONT,  " 조정계정
       GV_ZTERM LIKE ZEDT05_LFB1-ZTERM,  " 지급조건
" 4. LFM1 (구매 데이터)
       GV_EKORG LIKE ZEDT05_LFM1-EKORG,  " 구매조직
       GV_EKGRP LIKE ZEDT05_LFM1-EKGRP,  " 구매그룹
       GV_WAERS LIKE ZEDT05_LFM1-WAERS,  " 구매오더통화
       GV_MWSKZ LIKE ZEDT05_LFM1-MWSKZ.  " 세금코드


DATA : BEGIN OF GS_ALV,
      LIFNR LIKE Z23LFA1-LIFNR,  "구매처코드
      BUKRS LIKE Z23LFB1-BUKRS, "회사코드
      NAME1 LIKE Z23LFA1-NAME1,  "구매처명
      LAND1 LIKE Z23LFA1-LAND1,   "국가
      KTOKK LIKE Z23LFA1-KTOKK,"구매처그룹
      STCD1 LIKE Z23LFA1-STCD1,    "개인번호 / 구매처그룹이 3000일 때만
      STCD2 LIKE Z23LFA1-STCD2,    "사업자번호
      STRAS LIKE Z23LFA1-STRAS,    "주소

      AKONT LIKE Z23LFB1-AKONT,  "계정
      ZTERM LIKE Z23LFB1-ZTERM,  "지급조건
      EKORG LIKE Z23LFM1-EKORG,  "구매조직
      EKGRP LIKE Z23LFM1-EKGRP,  "구매그룹
      WAERS LIKE Z23LFM1-WAERS,  "구매오더통화
      MWSKZ LIKE Z23LFM1-MWSKZ,"세금코드
  END OF GS_ALV.
  DATA : GT_ALV LIKE TABLE OF GS_ALV.



SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  PARAMETERS : P_LIFNR LIKE ZEDT05_LFA1-LIFNR MODIF ID M2.
  PARAMETERS : P_BUKRS TYPE ZEDT05_LFB1-BUKRS.
  PARAMETERS : P_KTOKK LIKE ZEDT05_LFA1-KTOKK MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (5) TEXT-001 FOR FIELD P_CRE.
  PARAMETERS : P_CRE RADIOBUTTON GROUP R1 DEFAULT 'X' USER-COMMAND UC1.
  SELECTION-SCREEN COMMENT 20(5) TEXT-002 FOR FIELD P_VIEW.
  PARAMETERS : P_VIEW RADIOBUTTON GROUP R1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.


*&---------------------------------------------------------------------*
*&  Include           ZMM05_001_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM SELECTION_SCREEN .
  LOOP AT SCREEN.
    IF P_CRE = C_X.
      IF SCREEN-GROUP1 = 'M2'. " 조회조건(LIFNR) 숨김
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF P_VIEW = C_X.
      IF SCREEN-GROUP1 = 'M1'. " 생성조건(KTOKK) 숨김
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_CREATE_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form PREPARE_CREATE_MODE .
  CLEAR : GV_LIFNR, GV_NAME1, GV_LAND1, GV_STCD1, GV_STCD2, GV_STRAS,
          GV_AKONT, GV_ZTERM, GV_EKORG, GV_EKGRP, GV_WAERS, GV_MWSKZ.
  DATA : LV_MAX_LIFNR TYPE ZEDT05_LFA1-LIFNR.
  GV_KTOKK = P_KTOKK.
  GV_BUKRS = P_BUKRS.

  SELECT MAX( LIFNR ) INTO LV_MAX_LIFNR
    FROM ZEDT05_LFA1.

  IF LV_MAX_LIFNR IS INITIAL.
    GV_LIFNR = '1000000000'.
  ELSE.
    GV_LIFNR = LV_MAX_LIFNR + 1.
  ENDIF.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_DATA .
  SELECT SINGLE NAME1 LAND1 KTOKK STCD1 STCD2 STRAS
    INTO (GV_NAME1, GV_LAND1, GV_KTOKK, GV_STCD1, GV_STCD2, GV_STRAS)
    FROM ZEDT05_LFA1
  WHERE LIFNR = P_LIFNR.

  IF SY-SUBRC = 0.
    GV_LIFNR = P_LIFNR.
    SELECT SINGLE AKONT ZTERM
      INTO (GV_AKONT, GV_ZTERM)
      FROM ZEDT05_LFB1
      WHERE LIFNR = P_LIFNR
      AND BUKRS = P_BUKRS.

    SELECT SINGLE EKORG EKGRP WAERS MWSKZ
      INTO (GV_EKORG, GV_EKGRP, GV_WAERS, GV_MWSKZ)
      FROM ZEDT05_LFM1
     WHERE LIFNR = P_LIFNR.

   ELSE.
     MESSAGE '해당 구매처가 존재하지 않습니다.' TYPE 'S' DISPLAY LIKE 'E'.
   ENDIF.
endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CHECK_PARAM .
  IF P_BUKRS IS INITIAL.
    MESSAGE I000.
    STOP.
  ELSEIF P_KTOKK IS INITIAL and P_LIFNR IS INITIAL.
    MESSAGE I000.
    STOP.
  ENDIF.
endform.

*&---------------------------------------------------------------------*
*&  Include           ZMM05_001_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.


  LOOP AT SCREEN.
    IF P_VIEW = C_X.
      IF SCREEN-NAME = 'GV_LIFNR' OR SCREEN-NAME = 'GV_BUKRS'.
        SCREEN-INPUT = 0.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME CS 'STCD1'.
        SCREEN-ACTIVE = 0.
    ENDIF.


    IF GV_KTOKK = '2000'.
      IF SCREEN-NAME CS 'STCD1' OR SCREEN-NAME CS 'STCD2'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF GV_KTOKK = '3000'.
      IF SCREEN-NAME CS 'STCD1' OR SCREEN-NAME CS 'STCD2'.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
endmodule.


*&---------------------------------------------------------------------*
*&  Include           ZMM05_001_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  CASE OK_CODE.
    WHEN 'SAVE'.
      IF GV_NAME1 IS INITIAL.
        MESSAGE '구매처명을 입력하세요' TYPE 'E'.
      ENDIF.
      IF GV_KTOKK IS INITIAL.
        MESSAGE '계정그룹을 입력하세요' TYPE 'E'.
      ENDIF.

      ZEDT05_LFA1-LIFNR = GV_LIFNR.
      ZEDT05_LFA1-NAME1 = GV_NAME1.
      ZEDT05_LFA1-LAND1 = GV_LAND1.
      ZEDT05_LFA1-KTOKK = GV_KTOKK.
      ZEDT05_LFA1-STCD1 = GV_STCD1.
      ZEDT05_LFA1-STCD2 = GV_STCD2.
      ZEDT05_LFA1-STRAS = GV_STRAS.

      " [LFB1 데이터 매핑]
      ZEDT05_LFB1-LIFNR = GV_LIFNR.
      ZEDT05_LFB1-BUKRS = GV_BUKRS.
      ZEDT05_LFB1-AKONT = GV_AKONT.
      ZEDT05_LFB1-ZTERM = GV_ZTERM.

      " [LFM1 데이터 매핑]
      ZEDT05_LFM1-LIFNR = GV_LIFNR.
      ZEDT05_LFM1-EKORG = GV_EKORG.
      ZEDT05_LFM1-EKGRP = GV_EKGRP.
      ZEDT05_LFM1-WAERS = GV_WAERS.
      ZEDT05_LFM1-MWSKZ = GV_MWSKZ.

      IF P_CRE = C_X.
        INSERT ZEDT05_LFA1.
        INSERT ZEDT05_LFB1.
        INSERT ZEDT05_LFM1.

        IF SY-SUBRC = 0.
          MESSAGE '구매처가 생성되었습니다.' TYPE 'S'.
          COMMIT WORK.
          LEAVE TO SCREEN 0. " 초기 화면으로 복귀
        ELSE.
          MESSAGE '저장 실패' TYPE 'E'.
          ROLLBACK WORK.
        ENDIF.
      ELSE.
        UPDATE ZEDT05_LFA1.
        UPDATE ZEDT05_LFB1.
        UPDATE ZEDT05_LFM1.
        COMMIT WORK.
        MESSAGE '수정되었습니다' TYPE 'S'.
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    ENDCASE.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module EXIT_COMMAND input.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
endmodule.
