*&---------------------------------------------------------------------*
*& Report ZMM05_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM05_002 MESSAGE-ID ZMED05.

INCLUDE ZMM05_002_CLS.
INCLUDE ZMM05_002_TOP.
INCLUDE ZMM05_002_SCR.
INCLUDE ZMM05_002_F01.
INCLUDE ZMM05_002_PBO.
INCLUDE ZMM05_002_PAI.



AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

START-OF-SELECTION.
  PERFORM CHECK_P.

  IF R_CRE = 'X'.
    " 생성 모드
    PERFORM CHECK_VENDOR_VALID.
    PERFORM PREPARE_CREATE_MODE.
    CALL SCREEN 100.

  ELSEIF R_VIEW = 'X'.
    " 조회 모드
    PERFORM GET_PO_DATA.

    IF GT_ITEM IS INITIAL.
      MESSAGE '조회된 데이터가 없습니다.' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      CALL SCREEN 100.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&  Include           ZMM05_002_CLS
*&---------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS : HANDLE_DATA_CHANGED
              FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.
ENDCLASS.

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DATA_CHANGED.
    PERFORM CHECK_ALV_DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.
ENDCLASS.


TABLES : ZEDT05_LFA1,  " 구매처 마스터
         ZEDT05_LFM1,
         ZMARA05,      " 자재 마스터
         ZEKKO_05,     " PO 헤더
         ZEKPO_05,     " PO 아이템
         MAKT.         " 자재 내역

CONSTANTS : GC_SAVE TYPE SY-UCOMM VALUE 'SAVE',
            GC_BACK TYPE SY-UCOMM VALUE 'BACK',
            GC_EXIT TYPE SY-UCOMM VALUE 'EXIT',
            GC_CANC TYPE SY-UCOMM VALUE 'CANC'.

DATA : BEGIN OF GS_ITEM,
         ICON(4)       TYPE C,              " 상태 아이콘
         EBELP         TYPE ZEKPO_05-EBELP, " 품목번호
         MATNR         TYPE ZEKPO_05-MATNR, " 자재번호
         MAKTX         TYPE ZEKPO_05-MAKTX, " 자재내역
         MENGE         TYPE ZEKPO_05-MENGE, " 수량
         STPRS         TYPE ZEKPO_05-STPRS, " 단가 (금액)
         MEINS         TYPE ZEKPO_05-MEINS, " 기본 단위
         WAERS         TYPE ZEKKO_05-WAERS, " 통화 (헤더 참조 or 아이템)
         MWSKZ         TYPE MWSKZ,          " 세금코드 (추가됨!)
         PRDAT         TYPE ZEKPO_05-PRDAT, " 납품일
         WERKS         TYPE ZEKPO_05-WERKS, " 플랜트
         LGORT         TYPE ZEKPO_05-LGORT, " 저장위치
         BPRME         TYPE ZEKPO_05-BPRME, " 가격 단위 (EA)
         CELLTAB       TYPE LVC_T_STYL,     " 셀 제어용
       END OF GS_ITEM.

DATA : GT_ITEM LIKE TABLE OF GS_ITEM.

" 헤더 정보
DATA : BEGIN OF GS_HEADER,
         BUKRS TYPE ZEKKO_05-BUKRS,
         LIFNR TYPE ZEKKO_05-LIFNR,
         EKORG TYPE ZEKKO_05-EKORG,
         EKGRP TYPE ZEKKO_05-EKGRP,
         BEDAT TYPE ZEKKO_05-BEDAT,
         WAERS TYPE ZEKKO_05-WAERS,
         MWSKZ TYPE ZEDT05_LFM1-MWSKZ,
         EBELN TYPE EBELN,
       END OF GS_HEADER.

" ALV Object
DATA : GO_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GO_GRID      TYPE REF TO CL_GUI_ALV_GRID.

" Layout & Field Catalog
DATA : GS_LAYOUT  TYPE LVC_S_LAYO,
       GT_FCAT    TYPE LVC_T_FCAT,
       GS_FCAT    TYPE LVC_S_FCAT.

" Common Variables
DATA : OK_CODE   TYPE SY-UCOMM,
       GV_OKCODE TYPE SY-UCOMM.
DATA : GO_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.



*&---------------------------------------------------------------------*
*&  Include           ZMM05_002_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_BUKRS TYPE BUKRS MODIF ID M1 DEFAULT '1000'.
  PARAMETERS : P_LIFNR1 TYPE LIFNR MODIF ID M1 DEFAULT '1000000000'.
  PARAMETERS : P_BEDAT TYPE ZEKKO_05-BEDAT MODIF ID M1 DEFAULT SY-DATUM.

  PARAMETERS : P_LIFNR2 TYPE LIFNR MODIF ID M2 DEFAULT '1000000000'.
  PARAMETERS : P_BUKRS2 TYPE BUKRS MODIF ID M2 DEFAULT '1000'.
  PARAMETERS : P_EBLEN TYPE EBELN MODIF ID M2.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
  PARAMETERS : R_CRE RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND UCOM.
  PARAMETERS : R_VIEW RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B2.


*&---------------------------------------------------------------------*
*&  Include           ZMM05_002_F01
*&---------------------------------------------------------------------*

FORM MODIFY_SCREEN.
  LOOP AT SCREEN.
    IF R_CRE = 'X'.
      IF SCREEN-GROUP1 = 'M2'.
        SCREEN-ACTIVE = '0'.
      ENDIF.
    ELSEIF R_VIEW = 'X'.
      IF SCREEN-GROUP1 = 'M1'.
        SCREEN-ACTIVE = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

FORM CHECK_P.
  IF R_CRE = 'X'.
    IF P_LIFNR1 IS INITIAL OR P_BUKRS IS INITIAL OR P_BEDAT IS INITIAL.
      MESSAGE I000.
      STOP.
    ENDIF.
  ELSEIF R_VIEW = 'X'.
    IF P_LIFNR2 IS INITIAL OR P_BUKRS2 IS INITIAL OR P_EBLEN IS INITIAL.
      MESSAGE I000.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.

FORM CHECK_VENDOR_VALID.
  SELECT SINGLE A~LIFNR A~LAND1
    INTO (GS_HEADER-LIFNR, GS_HEADER-WAERS)
    FROM ZEDT05_LFA1 AS A
    WHERE A~LIFNR = P_LIFNR1.

  IF SY-SUBRC <> 0.
    MESSAGE I002.
    STOP.
  ENDIF.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF GS_HEADER
    FROM ZEDT05_LFM1
    WHERE LIFNR = P_LIFNR1.

  IF SY-SUBRC <> 0.
    " LFM1에 데이터가 없으면 기본값 설정
    GS_HEADER-WAERS = 'KRW'.
    GS_HEADER-MWSKZ = 'V0'.  " 기본 세금코드
  ENDIF.

  GS_HEADER-BUKRS = P_BUKRS.
  GS_HEADER-BEDAT = P_BEDAT.

  IF P_LIFNR2 IS INITIAL.
    GS_HEADER-LIFNR = P_LIFNR1.
  ELSEIF P_LIFNR1 IS INITIAL.
    GS_HEADER-LIFNR = P_LIFNR2.
  ENDIF.
ENDFORM.

FORM PREPARE_CREATE_MODE.
  REFRESH GT_ITEM.
  CLEAR GS_ITEM.
ENDFORM.


FORM SET_ALV_LATOUT.
  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.
ENDFORM.

FORM SET_ALV_FIELDCAT .
  DATA: LV_EDIT TYPE C LENGTH 1.

  CLEAR GT_FCAT.

  " 조회 모드면 편집 불가
  IF R_VIEW = 'X'.
    LV_EDIT = ''.
  ELSE.
    LV_EDIT = 'X'.
  ENDIF.

  PERFORM APPEND_FCAT USING :
    'EBELP' '품목'      ''       '',
    'MATNR' '자재번호'   LV_EDIT  '',
    'MAKTX' '자재내역'   ''       '',
    'MENGE' 'PO수량'     LV_EDIT  'MEINS',
    'STPRS' '단가(금액)' LV_EDIT  '',
    'MEINS' '단위'      ''       '',
    'BPRME' '통화'      ''       '',
    'MWSKZ' '세금코드'   LV_EDIT  '',
    'EINDT' '납품일'     LV_EDIT  '',
    'WERKS' '플랜트'     LV_EDIT  '',
    'LGORT' '저장위치'   LV_EDIT  ''.
ENDFORM.

FORM APPEND_FCAT USING P_FIELD P_TEXT P_EDIT P_QFIELD.
  CLEAR GS_FCAT.
  GS_FCAT-FIELDNAME = P_FIELD.
  GS_FCAT-COLTEXT   = P_TEXT.
  GS_FCAT-EDIT      = P_EDIT.
  GS_FCAT-QFIELDNAME = P_QFIELD. " 수량/통화 단위 참조 필드


  IF P_FIELD = 'MENGE'.
    GS_FCAT-REF_TABLE = 'ZEKPO_05'. " 본인이 만든 테이블명
    GS_FCAT-REF_FIELD = 'MENGE'.
  ENDIF.

  IF P_FIELD = 'EBELP'.
    GS_FCAT-REF_TABLE = 'ZEKPO_05'. " 본인 테이블명
    GS_FCAT-REF_FIELD = 'EBELP'.    " 필드명
    GS_FCAT-OUTPUTLEN = 6.          " 길이 6자리 정도면 충분
  ENDIF.

  IF P_FIELD = 'STPRS'.
    GS_FCAT-REF_TABLE = 'ZEKPO_05'.
    GS_FCAT-REF_FIELD = 'STPRS'.
    GS_FCAT-DATATYPE = 'CURR'.
  ENDIF.

  IF P_FIELD = 'EINDT'.
    GS_FCAT-DATATYPE = 'DATS'.
  ENDIF.


  APPEND GS_FCAT TO GT_FCAT.
ENDFORM.
* ZMM05_002_F01

FORM BTN_ADD_ROW .
  DATA : LV_MAX_INT TYPE I.             " 정수 변수
  DATA : LS_CELLTAB TYPE LVC_S_STYL.

  LOOP AT GT_ITEM INTO GS_ITEM.
    IF GS_ITEM-EBELP > LV_MAX_INT.
      LV_MAX_INT = GS_ITEM-EBELP.
    ENDIF.
  ENDLOOP.

  IF LV_MAX_INT IS INITIAL.
    LV_MAX_INT = 10.
  ELSE.
    LV_MAX_INT = LV_MAX_INT + 10.
  ENDIF.

  " 2. 신규 라인 세팅
  CLEAR GS_ITEM.

  GS_ITEM-EBELP = LV_MAX_INT.
  GS_ITEM-WAERS = GS_HEADER-WAERS.
  GS_ITEM-MWSKZ = GS_HEADER-MWSKZ.

  " 3. 입력 가능 필드 열어주기
  LS_CELLTAB-FIELDNAME = 'MATNR'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'MENGE'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'WERKS'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'LGORT'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'STPRS'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'MWSKZ'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.

  LS_CELLTAB-FIELDNAME = 'EINDT'.
  LS_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT LS_CELLTAB INTO TABLE GS_ITEM-CELLTAB.


  " 4. 테이블 추가
  APPEND GS_ITEM TO GT_ITEM.


  IF GO_GRID IS BOUND.
    DATA : LS_STABLE TYPE LVC_S_STBL.

    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = LS_STABLE.
  ENDIF.
ENDFORM.



FORM GET_PO_DATA.
  DATA: LT_EKPO_TEMP TYPE TABLE OF ZEKPO_05.

  " 1. 헤더 조회
  SELECT SINGLE * FROM ZEKKO_05
    INTO CORRESPONDING FIELDS OF GS_HEADER
    WHERE EBELN = P_EBLEN
      AND BUKRS = P_BUKRS2
      AND LIFNR = P_LIFNR2.

  IF SY-SUBRC <> 0.
    MESSAGE '해당 PO 데이터가 없습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 2. 아이템 조회
  SELECT * FROM ZEKPO_05
    INTO TABLE LT_EKPO_TEMP
    WHERE EBELN = P_EBLEN
    ORDER BY EBELP.

  IF SY-SUBRC <> 0.
    MESSAGE '해당 PO의 아이템이 없습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " 3. 내부 테이블로 데이터 이동
  REFRESH GT_ITEM.
  LOOP AT LT_EKPO_TEMP INTO DATA(LS_EKPO_TEMP).
    CLEAR GS_ITEM.
    MOVE-CORRESPONDING LS_EKPO_TEMP TO GS_ITEM.
    APPEND GS_ITEM TO GT_ITEM.
  ENDLOOP.

ENDFORM.
FORM CHECK_ALV_DATA_CHANGED USING P_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: LS_MODI  TYPE LVC_S_MODI,
        LS_ZMARA TYPE ZMARA05,
        LS_LFM1  TYPE ZEDT05_LFM1.    " 구매처-회사코드 정보

  DATA: LV_MATNR TYPE ZMARA05-ZMATNR,
        LV_WERKS TYPE WERKS_D,
        LV_LGORT TYPE LGORT_D,
        LV_DATE  TYPE DATUM,
        LV_WAERS TYPE WAERS,           " 통화
        LV_MWSKZ TYPE MWSKZ.           " 세금코드

  LOOP AT P_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MODI.

    CASE LS_MODI-FIELDNAME.

      WHEN 'MATNR'.
        " 빈 값은 스킵
        IF LS_MODI-VALUE IS INITIAL.
          CONTINUE.
        ENDIF.

        CLEAR LV_MATNR.
        LV_MATNR = LS_MODI-VALUE.
        IF STRLEN( LV_MATNR ) < 10.
          SHIFT LV_MATNR RIGHT DELETING TRAILING SPACE.
          OVERLAY LV_MATNR WITH '0000000000'.  " 10개의 0
        ENDIF.

        " 1) ZMARA05 테이블 조회 (자재 정보)
        SELECT SINGLE * FROM ZMARA05 INTO LS_ZMARA
          WHERE ZMATNR = LV_MATNR.

        IF SY-SUBRC <> 0.
          PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' 'ZMARA05에 존재하지 않는 자재입니다.'.
        ELSE.
          " 2) 구매처별 통화/세금코드 조회 (ZEDT05_LFM1)
          SELECT SINGLE WAERS MWSKZ
            INTO (LV_WAERS, LV_MWSKZ)
            FROM ZEDT05_LFM1
            WHERE LIFNR = GS_HEADER-LIFNR.

          IF SY-SUBRC <> 0.
            " LFM1에 데이터가 없으면 기본값
            LV_WAERS = 'KRW'.
            LV_MWSKZ = 'V0'.
          ENDIF.

          " 3) 화면(ALV)에 자동 입력

          " [자재내역]
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'MAKTX' I_VALUE = LS_ZMARA-ZMATNAME.

          " [단위]
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'MEINS' I_VALUE = LS_ZMARA-MEINS.

          " [가격단위]
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'BPRME' I_VALUE = LS_ZMARA-MEINS.

          " [단가]
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'STPRS' I_VALUE = LS_ZMARA-STPRS.

          " [플랜트]
          IF LS_ZMARA-ZWERKS IS NOT INITIAL.
            CALL METHOD P_DATA_CHANGED->MODIFY_CELL
              EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'WERKS' I_VALUE = LS_ZMARA-ZWERKS.
          ENDIF.

          " [저장위치]
          IF LS_ZMARA-ZLGORT IS NOT INITIAL.
            CALL METHOD P_DATA_CHANGED->MODIFY_CELL
              EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'LGORT' I_VALUE = LS_ZMARA-ZLGORT.
          ENDIF.

          " [통화] - 구매처별로 다름!
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'WAERS' I_VALUE = LV_WAERS.

          " [세금코드] - 구매처별로 다름!
          CALL METHOD P_DATA_CHANGED->MODIFY_CELL
            EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'MWSKZ' I_VALUE = LV_MWSKZ.

        ENDIF.

      " =========================================================
      " 2. 플랜트 (WERKS) 정합성 체크
      " =========================================================
      WHEN 'WERKS'.

        CALL METHOD P_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING I_ROW_ID = LS_MODI-ROW_ID
                    I_FIELDNAME = 'MATNR'
          IMPORTING E_VALUE = LV_MATNR.

        IF LV_MATNR IS NOT INITIAL.
          IF STRLEN( LV_MATNR ) < 10.
            SHIFT LV_MATNR RIGHT DELETING TRAILING SPACE.
            OVERLAY LV_MATNR WITH '0000000000'.
          ENDIF.
        ENDIF.

        IF LS_MODI-VALUE IS INITIAL.
          CONTINUE.
        ENDIF.

        LV_WERKS = LS_MODI-VALUE.
        SELECT SINGLE ZWERKS FROM ZMARA05
          INTO LV_WERKS
          WHERE ZMATNR = LV_MATNR
            AND ZWERKS = LV_WERKS.
        IF SY-SUBRC <> 0.
          PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' '존재하지 않는 플랜트입니다.'.
        ENDIF.

      " =========================================================
      " 3. 저장위치 (LGORT) 정합성 체크
      " =========================================================
      WHEN 'LGORT'.
        IF LS_MODI-VALUE IS INITIAL.
          CONTINUE.
        ENDIF.

        LV_LGORT = LS_MODI-VALUE.

        CALL METHOD P_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING I_ROW_ID = LS_MODI-ROW_ID
                    I_FIELDNAME = 'MATNR'
          IMPORTING E_VALUE = LV_MATNR.

        IF LV_MATNR IS NOT INITIAL.
          IF STRLEN( LV_MATNR ) < 10.
            SHIFT LV_MATNR RIGHT DELETING TRAILING SPACE.
            OVERLAY LV_MATNR WITH '0000000000'.
          ENDIF.
        ENDIF.

        " 현재 행의 플랜트 값 읽기
        CALL METHOD P_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING I_ROW_ID = LS_MODI-ROW_ID I_FIELDNAME = 'WERKS'
          IMPORTING E_VALUE  = LV_WERKS.

        IF LV_WERKS IS INITIAL.
          PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' '플랜트를 먼저 입력하세요.'.
        ELSE.
          " 표준 저장위치 테이블 확인
          SELECT SINGLE COUNT(*) FROM ZMARA05
            WHERE ZMATNR = LV_MATNR
              AND ZLGORT = LV_LGORT.
          IF SY-SUBRC <> 0.
            PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' '해당 자재에 없는 저장위치입니다.'.
          ENDIF.
        ENDIF.

      " =========================================================
      " 4. 납품일
      " =========================================================
      WHEN 'PRDAT'.
        LV_DATE = LS_MODI-VALUE.

        IF LV_DATE IS INITIAL OR LV_DATE = '00000000'.
          PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' '납품일을 입력하세요.'.
        ENDIF.

        " 헤더의 증빙일(BEDAT)과 비교
        IF LV_DATE < GS_HEADER-BEDAT.
          PERFORM ADD_PROTOCOL USING P_DATA_CHANGED LS_MODI 'E' '납품일은 증빙일 이후여야 합니다.'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_PROTOCOL (PDF 참조: 에러 메시지 출력)
*&---------------------------------------------------------------------*
FORM ADD_PROTOCOL USING P_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                        P_MODI         TYPE LVC_S_MODI
                        P_TYPE         TYPE CHAR1
                        P_MSG          TYPE STRING.

  CALL METHOD P_DATA_CHANGED->ADD_PROTOCOL_ENTRY
    EXPORTING
      I_MSGID     = '00'       " 표준 메시지 클래스
      I_MSGNO     = '010'      " 표준 메시지 번호 (&1 &2 &3 &4)
      I_MSGTY     = P_TYPE
      I_MSGV1     = P_MSG
      I_FIELDNAME = P_MODI-FIELDNAME
      I_ROW_ID    = P_MODI-ROW_ID.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REGIST_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REGIST_EVENT.
  IF GO_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GO_EVENT_RECEIVER.
    SET HANDLER GO_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR GO_GRID.

    " Enter 키 입력 시
    CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    " Modified 이벤트
    CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BTN_REMOVE_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form BTN_REMOVE_ROW .
  DATA : LT_ROWS TYPE LVC_T_ROW,
         LS_ROW TYPE LVC_S_ROW.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.
  IF LT_ROWS IS INITIAL.
    MESSAGE '삭제할 라인을 선택하세요.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SORT LT_ROWS BY INDEX DESCENDING.

  LOOP AT LT_ROWS INTO LS_ROW.
    DELETE GT_ITEM INDEX LS_ROW-INDEX.
  ENDLOOP.

  IF GO_GRID IS BOUND.
    DATA : LS_STABLE TYPE LVC_S_STBL.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = LS_STABLE.
  ENDIF.

  MESSAGE '삭제되었습니다.' TYPE 'S'.
endform.
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .
  DATA : LS_EKKO TYPE ZEKKO_05,        " 헤더 저장용 구조
         LS_EKPO TYPE ZEKPO_05,        " 아이템 저장용 구조
         LT_EKPO TYPE TABLE OF ZEKPO_05.
  DATA : LV_EBELN TYPE EBELN.          " 채번 변수
  DATA : LV_ANSWER TYPE C.

  " 0. 데이터 변경사항 반영
  CALL METHOD GO_GRID->CHECK_CHANGED_DATA.

  " 1. 저장 팝업
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = '저장 확인'
      TEXT_QUESTION         = '구매오더를 저장하시겠습니까?'
      TEXT_BUTTON_1         = '예'
      TEXT_BUTTON_2         = '아니오'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = LV_ANSWER.

  IF LV_ANSWER <> '1'.
    EXIT.
  ENDIF.

  IF GT_ITEM IS INITIAL.
    MESSAGE '저장할 아이템이 없습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " 2. PO 번호 채번
  IF R_CRE = 'X'.
    " DB에서 가장 큰 번호 조회 (테이블에 EBELN이 있어야 문법 오류가 안 납니다)
    SELECT SINGLE MAX( EBELN ) FROM ZEKKO_05 INTO LV_EBELN.

    IF LV_EBELN IS INITIAL.
      LV_EBELN = '4500000001'.
    ELSE.
      LV_EBELN = LV_EBELN + 1.
    ENDIF.
    GS_HEADER-EBELN = LV_EBELN.
  ELSE.
    " 조회 모드일 경우 화면의 번호 사용
    IF GS_HEADER-EBELN IS INITIAL.
       GS_HEADER-EBELN = P_EBLEN.
    ENDIF.
  ENDIF.

  " 3. 헤더 데이터 매핑
  MOVE-CORRESPONDING GS_HEADER TO LS_EKKO.

  " [삭제함] ERNAM, ERDAT 관련 코드는 테이블에 없으므로 주석 처리/삭제
  " LS_EKKO-ERNAM = SY-UNAME.
  " LS_EKKO-ERDAT = SY-DATUM.

  " 헤더 저장
  MODIFY ZEKKO_05 FROM LS_EKKO.

  " 4. 아이템 데이터 매핑
  LOOP AT GT_ITEM INTO GS_ITEM.
    CLEAR LS_EKPO.
    MOVE-CORRESPONDING GS_ITEM TO LS_EKPO.

    " [중요] 헤더와 아이템 연결 (EBELN)
    LS_EKPO-EBELN = GS_HEADER-EBELN.
    LS_EKPO-MANDT = SY-MANDT.

    APPEND LS_EKPO TO LT_EKPO.
  ENDLOOP.

  " 기존 아이템 삭제 후 재생성
  DELETE FROM ZEKPO_05 WHERE EBELN = GS_HEADER-EBELN.
  MODIFY ZEKPO_05 FROM TABLE LT_EKPO.

  " 5. 커밋 및 완료
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH '저장 완료. 번호:' GS_HEADER-EBELN.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '저장 실패' TYPE 'E'.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&  Include           ZMM05_002_PBO
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
    IF SCREEN-NAME CS 'GS_HEADER'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CREATE_ALV_OBJECT output.
  IF GO_CONTAINER IS INITIAL.
    CREATE OBJECT GO_CONTAINER
      EXPORTING
          CONTAINER_NAME = 'CC_ALV'.

    CREATE OBJECT GO_GRID
      EXPORTING
        I_PARENT = GO_CONTAINER.
    PERFORM SET_ALV_LATOUT.
    PERFORM SET_ALV_FIELDCAT.
    PERFORM REGIST_EVENT.


    CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT = GS_LAYOUT
      CHANGING
        IT_OUTTAB = GT_ITEM
        IT_FIELDCATALOG = GT_FCAT.
    CALL METHOD GO_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ELSE.
    CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
endmodule.


*&---------------------------------------------------------------------*
*&  Include           ZMM05_002_PAI
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  GV_OKCODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OKCODE.
    WHEN 'ADD'.  " GUI Status에서 만든 기능코드
      PERFORM BTN_ADD_ROW.

    WHEN 'REMOVE'.
      PERFORM BTN_REMOVE_ROW.
    WHEN GC_SAVE.    " 저장 (필요시 구현)
       PERFORM SAVE_DATA.

  ENDCASE.

ENDMODULE.
