*----------------------------------------------------------------------*
*                        Yara Brasil                                   *
*                     Date: 23/11/2020                                 *
*-----------------  --------------------  -----------------------------*
*----------------  -------------------   ------------------------------*
* Program name:     ZHYD0006_MM       Author: Kamylla Oliveira         *
* Demanda:          EST.810           Author: Alessandro Ketelhut(FUNC)*
* Transport Number: EDCK900218                                         *
* Object - Title 1: Gestão de materiais - Apurar nível de atendimento  *
*                   de reservas                                        *
* The program is a copy from R3QCSAP, program: YMR469V0                *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*-----       CHANGE  LOG                             ------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Transport:  Date:       DEMAND   ID      Name                        *
*----------------------------------------------------------------------*
* EDCK900218  23/11/2020  EST.810  C053054 Kamylla Calazans Oliveira   *
* Object of change:       Versão inicial                               *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Report ZHYD0006_MM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhyd0006_mm MESSAGE-ID zgen.

*-------------------------------------------------------------------*
* Definição da(s) Tabela(s) Transparente(s)
*-------------------------------------------------------------------*
TABLES: resb, mkpf, mseg, mard, zz005_mm, marc,
        zz014_mm, zz007_mm.

*-------------------------------------------------------------------*
* TYPE-POOLS
*-------------------------------------------------------------------*
TYPE-POOLS: slis. " Tipos globais para ALV

*-------------------------------------------------------------------*
* Tipos
*-------------------------------------------------------------------*
TYPES: BEGIN OF ty_resb.
    INCLUDE STRUCTURE resb.
TYPES: situ TYPE c.
TYPES: END OF ty_resb.

TYPES: BEGIN OF ty_rkpf,
         rsnum      TYPE rkpf-rsnum,
         kzver      TYPE rkpf-kzver,
         kostl      TYPE rkpf-kostl,                        "+TI 43934
         ps_psp_pnr TYPE rkpf-ps_psp_pnr,
         nplnr      TYPE rkpf-nplnr,
       END OF ty_rkpf.

TYPES: BEGIN OF ty_caufv,
         aufnr   TYPE caufv-aufnr,
         autyp   TYPE caufv-autyp,                          "+TI 43934
         no_disp TYPE caufv-no_disp,
       END OF ty_caufv.

TYPES: BEGIN OF ty_mkpf,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
         budat TYPE mkpf-budat,
       END OF ty_mkpf.

TYPES: BEGIN OF ty_mseg,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mseg-mjahr,
         zeile      TYPE mseg-zeile,
         bwart      TYPE mseg-bwart,
         matnr      TYPE mseg-matnr,
         werks      TYPE mseg-werks,
         lgort      TYPE mseg-lgort,
         sobkz      TYPE mseg-sobkz,                         "TOP 4404386 - Luana Prates - 08/11/16
         menge      TYPE mseg-menge,
         kostl      TYPE mseg-kostl,                        "+TI 43934
         aufnr      TYPE mseg-aufnr,                        "+TI 43934
         rsnum      TYPE mseg-rsnum,
         rspos      TYPE mseg-rspos,
         mat_pspnr  TYPE mseg-mat_pspnr,
         ps_psp_pnr TYPE mseg-ps_psp_pnr,
         nplnr      TYPE mseg-nplnr,
         sjahr      TYPE mseg-sjahr,
         smbln      TYPE mseg-smbln,
         smblp      TYPE mseg-smblp,
       END OF ty_mseg.

TYPES: BEGIN OF ty_mat_rsab,
         matnr TYPE resb-matnr,
         werks TYPE resb-werks,
         lgort TYPE resb-lgort,
         pspel TYPE resb-pspel,
       END OF ty_mat_rsab.

TYPES: BEGIN OF ty_rs_pnd_mt,
         rsnum    TYPE resb-rsnum,
         rspos    TYPE resb-rspos,
         matnr    TYPE resb-matnr,
         werks    TYPE resb-werks,
         lgort    TYPE resb-lgort,
         bdter    TYPE resb-bdter,
         bdmng    TYPE resb-bdmng,
         enmng    TYPE resb-enmng,
         bwart    TYPE resb-bwart,
         aufnr    TYPE resb-aufnr,
         pspel    TYPE resb-pspel,
         atdm     TYPE c,
         atmcq    TYPE c,
         ym_sldvr TYPE zz005_mm-ym_sldvr,
         ym_sldre TYPE zz005_mm-ym_sldre,
       END OF ty_rs_pnd_mt.

TYPES: BEGIN OF ty_mard,
         matnr TYPE mard-matnr,
         werks TYPE mard-werks,
         lgort TYPE mard-lgort,
         labst TYPE mard-labst,
         insme TYPE mard-insme,
       END OF ty_mard.

TYPES: BEGIN OF ty_qbew,
         matnr TYPE qbew-matnr,
         bwkey TYPE qbew-bwkey,
         pspnr TYPE qbew-pspnr,
         lbkum TYPE qbew-lbkum,
       END OF ty_qbew.

* - +TI 43934 - Início - Vinícius Melo - 05/05/2014
TYPES: BEGIN OF y_aufk,
         aufnr TYPE aufk-aufnr,
         autyp TYPE aufk-autyp,
       END   OF y_aufk.

TYPES: BEGIN OF y_eban,
         banfn    TYPE eban-banfn,
         bnfpo    TYPE eban-bnfpo,
         loekz    TYPE eban-loekz,
         matnr    TYPE eban-matnr,
         werks    TYPE eban-werks,
         badat    TYPE eban-badat,
         arsnr    TYPE eban-arsnr,
         arsps    TYPE eban-arsps,
         knttp    TYPE knttp,       "TOP 4404386 - Luana Prates - 11/10/16
         prio_urg TYPE prio_urg,    "TOP 4404386 - Luana Prates - 11/10/16
       END   OF y_eban.

TYPES: BEGIN OF y_ebkn,
         banfn      TYPE ebkn-banfn,
         bnfpo      TYPE ebkn-bnfpo,
         zebkn      TYPE ebkn-zebkn,
         loekz      TYPE ebkn-loekz,
         aufnr      TYPE ebkn-aufnr,
         kostl      TYPE kostl,      "TOP 4404386 - Luana Prates - 11/10/16
         ps_psp_pnr TYPE ps_psp_pnr, "TOP 4404386 - Luana Prates - 11/10/16
         nplnr      TYPE nplnr,      "TOP 4404386 - Luana Prates - 11/10/16
       END   OF y_ebkn.

TYPES: BEGIN OF y_mat_rsv.
    INCLUDE TYPE zz005_mm.
TYPES: autyp      TYPE aufk-autyp,
       objectclas TYPE cdhdr-objectclas,
       objectid   TYPE cdhdr-objectid,
       tabname    TYPE cdpos-tabname,
       tabkey     TYPE cdpos-tabkey.
TYPES: END   OF y_mat_rsv.

TYPES: BEGIN OF y_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         dismm TYPE marc-dismm,
       END   OF y_marc,
* - +TI 43934 - Fim    - Vinícius Melo - 05/05/2014

* - Início - TOP 4404386 - Luana Prates - 11/10/16
       BEGIN OF y_mvt_per,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mseg-mjahr,
         zeile      TYPE mseg-zeile,
         matnr      TYPE bsim-matnr,
         werks      TYPE mseg-werks,
         lgort      TYPE mseg-lgort,
         shkzg      TYPE mseg-shkzg,
         dmbtr      TYPE mseg-dmbtr,
         bwtar      TYPE mseg-bwtar,
         menge      TYPE mseg-menge,
         meins      TYPE mseg-meins,
         ps_psp_pnr TYPE mseg-ps_psp_pnr,
         blart      TYPE mkpf-blart,
         bldat      TYPE mkpf-bldat,
         budat      TYPE mkpf-budat,
       END OF y_mvt_per,

       BEGIN OF y_estq_data,
         matnr      TYPE mard-matnr,
         werks      TYPE mard-werks,
         lgort      TYPE mard-lgort,
         ps_psp_pnr TYPE ps_psp_pnr,
         estqm      TYPE lbkum,
       END OF y_estq_data,

       BEGIN OF y_mvmt_mat,
         matnr      TYPE mard-matnr,
         werks      TYPE mard-werks,
         lgort      TYPE mard-lgort,
         ps_psp_pnr TYPE ps_psp_pnr,
         qtd_h      TYPE menge_d,
         qtd_s      TYPE menge_d,
       END OF y_mvmt_mat,

       BEGIN OF y_t001w,
         werks      TYPE t001w-werks,
         bbranch    TYPE t001w-j_1bbranch,
         ytp_centro TYPE t001w-ytp_centro,
       END OF y_t001w,

       BEGIN OF ty_rs_dev,
         rsnum TYPE resb-rsnum,
         rspos TYPE resb-rspos,
         matnr TYPE resb-matnr,
         werks TYPE resb-werks,
         bdmng TYPE resb-bdmng,
         enmng TYPE resb-enmng,
       END OF ty_rs_dev,

       BEGIN OF y_aux,
         chave TYPE mseg-aufnr,
       END OF y_aux.

*-------------------------------------------------------------------*
* Definição da(s) Tabela(s) Interna(s)
*-------------------------------------------------------------------*
DATA: it_resb       TYPE TABLE OF ty_resb       WITH HEADER LINE,
      it_rkpf       TYPE TABLE OF ty_rkpf       WITH HEADER LINE,
      it_caufv      TYPE TABLE OF ty_caufv      WITH HEADER LINE,
      it_mkpf       TYPE TABLE OF ty_mkpf       WITH HEADER LINE,
      it_mseg       TYPE TABLE OF ty_mseg       WITH HEADER LINE,
      it_mat_rsab   TYPE TABLE OF ty_mat_rsab   WITH HEADER LINE,
      it_mat_rsv    TYPE TABLE OF y_mat_rsv     WITH HEADER LINE,
      it_zz014_mm   TYPE STANDARD TABLE OF      zz014_mm,
      t_marc        TYPE TABLE OF y_marc,
      t_aufk        TYPE TABLE OF y_aufk,
      t_eban        TYPE TABLE OF y_eban,
      t_ebkn        TYPE TABLE OF y_ebkn,
      it_rs_pnd_mt  TYPE TABLE OF ty_rs_pnd_mt  WITH HEADER LINE,
      it_mard       TYPE TABLE OF ty_mard       WITH HEADER LINE,
      it_qbew       TYPE TABLE OF ty_qbew       WITH HEADER LINE,
      it_dados      TYPE TABLE OF zz005_mm   WITH HEADER LINE,
      t_mvt_per     TYPE TABLE OF y_mvt_per     WITH HEADER LINE,
      t_sld_mat_mvt TYPE TABLE OF ty_mard       WITH HEADER LINE,
      t_sld_prj_mvt TYPE TABLE OF ty_qbew       WITH HEADER LINE,
      t_estq_data   TYPE TABLE OF y_estq_data   WITH HEADER LINE,
      t_t001w       TYPE TABLE OF y_t001w       WITH HEADER LINE,
      t_rs_dev      TYPE TABLE OF ty_rs_dev     WITH HEADER LINE.

DATA: it_fieldcat       TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: e_layout          TYPE slis_layout_alv.

*-------------------------------------------------------------------*
* Variáveis
*-------------------------------------------------------------------*
DATA: vgn_estoque TYPE mard-labst,
      v_stq_tot   TYPE mard-labst.

*-------------------------------------------------------------------*
* Constantes
*-------------------------------------------------------------------*
CONSTANTS: c_a           TYPE c    VALUE 'A',
           c_b           TYPE c    VALUE 'B',
           c_c           TYPE c    VALUE 'C',
           c_f(01)       TYPE c    VALUE 'F',
           c_i(01)       TYPE c    VALUE 'I',
           c_k(01)       TYPE c    VALUE 'K',
           c_key(03)     TYPE c    VALUE 'KEY',
           c_s           TYPE c    VALUE 'S',
           c_x           TYPE c    VALUE 'X',
           c_l           TYPE c    VALUE 'L',
           c_m           TYPE c    VALUE 'M',
           c_n           TYPE c    VALUE 'N',
           c_p           TYPE c    VALUE 'P',
           c_q           TYPE c    VALUE 'Q',
           c_r           TYPE c    VALUE 'R',
           c_h           TYPE c    VALUE 'H',
           c_d           TYPE c    VALUE 'D',
           c_02(02)      TYPE c    VALUE '02',
           c_eq(02)      TYPE c    VALUE 'EQ',
           c_261(03)     TYPE c    VALUE '261',
           c_281(03)     TYPE c    VALUE '281',
           c_ctm(03)     TYPE c    VALUE 'CTM',
           c_ctmd(04)    TYPE c    VALUE 'CTMD',
           c_0001(04)    TYPE n    VALUE '0001',
           c_order(05)   TYPE c    VALUE 'ORDER',
           c_resb(04)    TYPE c    VALUE 'RESB',
           c_ym_resb(07) TYPE c    VALUE 'YM_RESB'.

*-------------------------------------------------------------------*
* Ranges
*-------------------------------------------------------------------*
RANGES:  r_wrk   FOR t001w-werks,
         r_wrk_d FOR t001w-werks.

* ------------------------------------------------------------------*
* Definição da Tela de Seleção
* ------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_bdter FOR resb-bdter NO INTERVALS NO-EXTENSION  OBLIGATORY,
                s_vgart FOR mkpf-vgart OBLIGATORY,
                s_blart FOR mkpf-blart OBLIGATORY,
                s_bwart FOR resb-bwart OBLIGATORY,
                s_matnr FOR mseg-matnr,
                s_werks FOR mseg-werks,
                s_lgort FOR mard-lgort OBLIGATORY,
                s_dismm FOR marc-dismm.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2.
SELECTION-SCREEN COMMENT /01(83) TEXT-002.
SELECTION-SCREEN COMMENT /01(83) TEXT-003.
SELECTION-SCREEN COMMENT /01(83) TEXT-004.
SELECTION-SCREEN COMMENT /01(83) TEXT-005.
SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------
* LÓGICA DE PROCESSAMENTO
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Selecionar dados
  PERFORM yf_selecao_dados.

END-OF-SELECTION.
* Processamento dos dados
  PERFORM yf_processa_dados.

  PERFORM yf_analisar_tempo_rc.                             "+TI 43934

* Atualizar tabela Y
  PERFORM yf_atualizar_tab.

  PERFORM yf_alv.

************************************************************************

*&---------------------------------------------------------------------*
*&      Form  YF_SELECAO_DADOS
*&---------------------------------------------------------------------*
FORM yf_selecao_dados.

  DATA: t_mseg_aux TYPE TABLE OF ty_mseg,
        t_resb_aux TYPE TABLE OF ty_resb,
        t_aux      TYPE TABLE OF y_aux  WITH HEADER LINE,
        e_mseg     TYPE ty_mseg.

  DATA: l_tabix  TYPE sy-tabix.

  CLEAR: it_resb, it_caufv, it_mkpf, it_mseg.
  REFRESH: it_resb, it_caufv, it_mkpf, it_mseg.
  FREE t_marc[].

  SELECT *
  FROM zz014_mm
  INTO TABLE it_zz014_mm
  WHERE ym_werks IN s_werks.
  IF sy-subrc IS INITIAL.
    SORT it_zz014_mm BY ym_werks.
  ENDIF.

** Buscar dados da tabela RESB
  SELECT *
  FROM resb
  INTO TABLE it_resb
  WHERE bdter   IN s_bdter
    AND bwart   IN s_bwart
    AND matnr   IN s_matnr
    AND werks   IN s_werks
    AND lgort   IN s_lgort
    AND ( postp EQ c_l OR
          rssta EQ c_m )
    AND no_disp EQ space
    AND flgex   EQ space.

  DELETE it_resb WHERE bdmng = 0.


  IF it_resb[] IS NOT INITIAL.

*** Buscar dados na tabela CAUFV
    SELECT aufnr autyp no_disp
    FROM caufv
    INTO TABLE it_caufv
    FOR ALL ENTRIES IN it_resb
    WHERE aufnr EQ it_resb-aufnr.

*   Complementar informações da reserva
    SELECT rsnum kzver kostl ps_psp_pnr nplnr
      INTO TABLE it_rkpf
        FROM rkpf
          FOR ALL ENTRIES IN it_resb
          WHERE rsnum EQ it_resb-rsnum.
  ENDIF.

*** Buscar dados na tabela MKPF
  SELECT mblnr mjahr budat
  FROM mkpf
  INTO TABLE it_mkpf
  WHERE budat IN s_bdter
    AND vgart IN s_vgart
    AND blart IN s_blart.

  IF sy-subrc EQ 0.
*** Buscar dados na tabela MSEG
    SELECT mblnr mjahr zeile bwart matnr werks
           lgort sobkz menge kostl aufnr rsnum
           rspos mat_pspnr   ps_psp_pnr  nplnr
           sjahr smbln smblp
    FROM mseg
    INTO TABLE it_mseg
    FOR ALL ENTRIES IN it_mkpf
    WHERE mblnr EQ it_mkpf-mblnr
      AND mjahr EQ it_mkpf-mjahr
      AND bwart IN s_bwart
      AND matnr IN s_matnr
      AND werks IN s_werks.

    IF sy-subrc EQ 0. " se encontrou movimentos
*     Verificar e desconsiderar estornados
      PERFORM yf_verif_desconsid_estornos.

*     Desconsiderar movimentos com quantidade movimentada zero
      DELETE it_mseg WHERE menge EQ 0.
    ENDIF.

  ENDIF.

  IF it_resb[] IS INITIAL AND
     it_mseg[] IS INITIAL.
    "Não há movimento registrado na data de processamento informada.
    MESSAGE s083 WITH TEXT-e01 TEXT-e02 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT: it_resb  BY rsnum rspos,
        it_rkpf  BY rsnum,
        it_caufv BY aufnr,
        it_mkpf  BY mblnr mjahr,
        it_mseg  BY mblnr mjahr zeile.

  IF NOT it_resb[] IS INITIAL.
    t_resb_aux[] = it_resb[].
    SORT t_resb_aux BY matnr werks.
    DELETE ADJACENT DUPLICATES FROM t_resb_aux COMPARING matnr werks.
    DELETE t_resb_aux WHERE matnr IS INITIAL.
    CHECK NOT t_resb_aux[] IS INITIAL.

    SELECT matnr werks dismm
      FROM marc
      INTO TABLE t_marc
       FOR ALL ENTRIES IN t_resb_aux
     WHERE matnr = t_resb_aux-matnr
       AND werks = t_resb_aux-werks.

  ENDIF.

  IF NOT it_mseg[] IS INITIAL.
    t_mseg_aux[] = it_mseg[].
    SORT t_mseg_aux BY matnr werks.
    DELETE ADJACENT DUPLICATES FROM t_mseg_aux COMPARING matnr werks.
    DELETE t_mseg_aux WHERE matnr IS INITIAL.

    IF NOT t_mseg_aux[] IS INITIAL.
      SELECT matnr werks dismm
        FROM marc
        APPENDING TABLE t_marc
         FOR ALL ENTRIES IN t_mseg_aux
       WHERE matnr = t_mseg_aux-matnr
         AND werks = t_mseg_aux-werks.
    ENDIF.

    t_mseg_aux[] = it_mseg[].
    SORT t_mseg_aux BY aufnr.
    DELETE ADJACENT DUPLICATES FROM t_mseg_aux COMPARING aufnr.
    DELETE t_mseg_aux WHERE aufnr IS INITIAL AND
                            nplnr IS INITIAL.
    CLEAR t_aux[].
    LOOP AT t_mseg_aux INTO e_mseg.
      IF e_mseg-aufnr IS NOT INITIAL.
        t_aux-chave = e_mseg-aufnr.
      ELSEIF e_mseg-nplnr IS NOT INITIAL.
        t_aux-chave = e_mseg-nplnr.
      ENDIF.
      APPEND t_aux.
      CLEAR t_aux.
    ENDLOOP.

    SORT t_aux BY chave.
    DELETE ADJACENT DUPLICATES FROM t_aux COMPARING chave.

    IF NOT t_aux[] IS INITIAL.
      SELECT aufnr autyp
        FROM aufk
        INTO TABLE t_aufk
        FOR ALL ENTRIES IN t_aux
        WHERE aufnr = t_aux-chave.
    ENDIF.
  ENDIF.

  DELETE t_marc WHERE dismm NOT IN s_dismm.

  IF NOT t_marc[] IS INITIAL.
    SORT t_marc BY matnr werks.
    DELETE ADJACENT DUPLICATES FROM t_marc COMPARING matnr werks.
  ENDIF.

  IF it_mseg[] IS NOT INITIAL.

    SELECT m~mblnr m~mjahr m~zeile m~matnr m~werks m~lgort m~shkzg
           m~dmbtr m~bwtar m~menge m~meins m~ps_psp_pnr
           k~blart k~bldat k~budat
           INTO TABLE t_mvt_per
           FROM mseg AS m
           INNER JOIN mkpf AS k
           ON m~mblnr = k~mblnr AND
              m~mjahr = k~mjahr
           FOR ALL ENTRIES IN it_mseg
           WHERE m~matnr EQ it_mseg-matnr
             AND m~werks EQ it_mseg-werks
             AND k~budat GE s_bdter-low.


    IF t_mvt_per[] IS NOT INITIAL.

      SELECT matnr werks lgort labst FROM mard
             INTO TABLE t_sld_mat_mvt
             FOR ALL ENTRIES IN t_mvt_per
             WHERE matnr = t_mvt_per-matnr
               AND werks = t_mvt_per-werks
               AND lgort = t_mvt_per-lgort.

      SELECT matnr bwkey pspnr lbkum FROM qbew
             INTO TABLE t_sld_prj_mvt
             FOR ALL ENTRIES IN t_mvt_per
             WHERE matnr = t_mvt_per-matnr
               AND bwkey = t_mvt_per-werks
               AND pspnr = t_mvt_per-ps_psp_pnr.

    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SELECAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  YF_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM yf_processa_dados.

  DATA: e_aufk  TYPE y_aufk,
        e_cdhdr TYPE cdhdr,
        e_cdpos TYPE cdpos.

  DATA: vlc_bdter   TYPE resb-bdter,
        vln_bdmng   TYPE resb-bdmng,
        vln_enmng   TYPE resb-enmng,
        vlc_postp   TYPE resb-postp,
        vlc_kzver   TYPE rkpf-kzver,
        vln_tabix   TYPE sy-tabix,
        l_qtd_bx    TYPE mseg-menge,
        l_estq      TYPE lbkum,
        l_wrk_dev   TYPE t001w-werks,
        l_est_rsv_d TYPE resb-bdmng,
        l_centro_op TYPE c.

  DATA: t_mseg_aux TYPE TABLE OF ty_mseg WITH HEADER LINE,
        t_mard_d   TYPE TABLE OF ty_mard WITH HEADER LINE.

  PERFORM yf_compor_estq_data.

  t_mseg_aux[] = it_mseg[].

  SORT t_mseg_aux BY matnr werks lgort mblnr mjahr zeile.

  CLEAR: it_mat_rsab, it_mat_rsv.
  REFRESH: it_mat_rsab, it_mat_rsv.

  SORT: t_aufk  BY aufnr,
        it_caufv BY aufnr.

*** Apurar movimentos de baixa de reserva, registrados
*** na data de processamento
  LOOP AT it_mseg.

    CLEAR: vlc_bdter, vln_bdmng, vln_enmng, vlc_postp, vlc_kzver.

    SELECT i~bdter i~bdmng i~enmng i~postp
           c~kzver
    INTO (vlc_bdter, vln_bdmng, vln_enmng, vlc_postp,
          vlc_kzver)
    FROM resb AS i
      INNER JOIN rkpf AS c
        ON i~rsnum EQ c~rsnum
          UP TO 1 ROWS
    WHERE i~rsnum EQ it_mseg-rsnum
      AND i~rspos EQ it_mseg-rspos.
    ENDSELECT.

*   Verificar se reserva é por dentro da ordem e de item não estocável
    IF NOT ( vlc_kzver IS INITIAL ) AND " reserva por dentro da ordem...
       vlc_postp EQ c_n.                "...e item não estocável
*     Ignorar o movimento pois a medição será feita no atendimento
*     da RC associada à reserva
      CONTINUE.
    ENDIF.


    it_mat_rsv-ym_rsnum = it_mseg-rsnum.
    it_mat_rsv-ym_rspos = it_mseg-rspos.
    it_mat_rsv-ym_matnr = it_mseg-matnr.
    it_mat_rsv-ym_werks = it_mseg-werks.
    it_mat_rsv-ym_lgort = it_mseg-lgort.
    it_mat_rsv-ym_dtprc = s_bdter-low.
    it_mat_rsv-ym_strsv = c_b.
    it_mat_rsv-ym_bdter = vlc_bdter.

    IF NOT ( it_mseg-mat_pspnr  IS INITIAL ).
      it_mat_rsv-ym_knttp = c_q.
      it_mat_rsv-ym_pspel = it_mseg-mat_pspnr.

      READ TABLE t_aufk INTO e_aufk WITH KEY aufnr = it_mseg-nplnr.
      IF sy-subrc EQ 0.
        it_mat_rsv-autyp = e_aufk-autyp.
      ENDIF.

    ELSEIF NOT ( it_mseg-ps_psp_pnr IS INITIAL ).
      it_mat_rsv-ym_knttp = c_p.
      it_mat_rsv-ym_elpep = it_mseg-ps_psp_pnr.

    ELSEIF NOT ( it_mseg-nplnr IS INITIAL ).
      it_mat_rsv-ym_knttp = c_n.
      it_mat_rsv-ym_nplnr = it_mseg-nplnr.

      READ TABLE t_aufk INTO e_aufk WITH KEY aufnr = it_mseg-nplnr.
      IF sy-subrc EQ 0.
        it_mat_rsv-autyp = e_aufk-autyp.
      ENDIF.

    ELSEIF NOT ( it_mseg-kostl IS INITIAL ).
      it_mat_rsv-ym_knttp = c_k.
      it_mat_rsv-ym_kostl = it_mseg-kostl.

    ELSEIF NOT ( it_mseg-aufnr IS INITIAL ).
      it_mat_rsv-ym_knttp = c_f.
      it_mat_rsv-ym_aufnr = it_mseg-aufnr.

      READ TABLE t_aufk INTO e_aufk
                     WITH KEY aufnr = it_mseg-aufnr
                                      BINARY SEARCH.
      IF sy-subrc = 0.
        it_mat_rsv-autyp = e_aufk-autyp.
      ENDIF.
* - +TI 43934 - Fim    - Vinícius Melo - 05/05/2014

    ENDIF.

    IF it_mseg-sobkz EQ c_q.
      it_mat_rsv-ym_knttp = c_q.
    ENDIF.

* - Início - TOP 4404386 - Luana Prates - 11/10/16
    LOOP AT t_mseg_aux WHERE matnr = it_mseg-matnr
                         AND werks = it_mseg-werks
                         AND lgort = it_mseg-lgort
                         AND mblnr < it_mseg-mblnr.
      IF it_mseg-ps_psp_pnr IS NOT INITIAL.

        CHECK it_mseg-ps_psp_pnr = t_mseg_aux-ps_psp_pnr.

      ENDIF.

      l_qtd_bx = l_qtd_bx + t_mseg_aux-menge.

    ENDLOOP.

    IF vlc_bdter > s_bdter-low.
      it_mat_rsv-ym_nvatm = c_s.
    ELSE.
      IF it_mseg-menge < vln_bdmng.
        CLEAR l_estq.

        READ TABLE t_estq_data WITH KEY matnr = it_mseg-matnr
                                        werks = it_mseg-werks
                                        lgort = it_mseg-lgort
                                        ps_psp_pnr = it_mseg-ps_psp_pnr.
        IF sy-subrc EQ 0.
          l_estq = t_estq_data-estqm - l_qtd_bx.
        ENDIF.

        IF l_estq < vln_bdmng.
          it_mat_rsv-ym_nvatm = c_p.
        ELSE.
          it_mat_rsv-ym_nvatm = c_s.
        ENDIF.
      ELSE.
        it_mat_rsv-ym_nvatm = c_s.
      ENDIF.
    ENDIF.

    it_mat_rsv-ym_bdmng = vln_bdmng.
    it_mat_rsv-ym_enmng = vln_enmng.
    it_mat_rsv-ym_mblnr = it_mseg-mblnr.
    it_mat_rsv-ym_mjahr = it_mseg-mjahr.
    it_mat_rsv-ym_zeile = it_mseg-zeile.
    it_mat_rsv-ym_kzver = vlc_kzver.
    it_mat_rsv-ym_bwart = it_mseg-bwart.

    IF it_mat_rsv-ym_kzver IS INITIAL.
      it_mat_rsv-objectclas = c_ym_resb.
      CONCATENATE sy-mandt it_mat_rsv-ym_rsnum
             INTO it_mat_rsv-objectid.
    ELSE.
      it_mat_rsv-objectclas = c_order.

      IF it_mseg-nplnr IS NOT INITIAL.
        CONCATENATE sy-mandt it_mat_rsv-autyp it_mseg-nplnr
               INTO it_mat_rsv-objectid.
      ELSE.

        CONCATENATE sy-mandt it_mat_rsv-autyp it_mat_rsv-ym_aufnr
               INTO it_mat_rsv-objectid.
      ENDIF.
    ENDIF.

    it_mat_rsv-tabname = c_resb.
    CONCATENATE sy-mandt it_mat_rsv-ym_rsnum it_mat_rsv-ym_rspos
           INTO it_mat_rsv-tabkey.

    it_mat_rsv-ym_strsv_rc = it_mat_rsv-ym_strsv.

    APPEND it_mat_rsv.
    CLEAR: it_mat_rsv.
  ENDLOOP.

*** Analisar reservas que deveriam ter sido baixadas
*** na data de processamento
  LOOP AT it_resb.
    vln_tabix = sy-tabix.

    CLEAR: it_caufv, it_rkpf.

*   Verificar se reserva é criada por dentro da ordem...
*   ...de item não estocável
    READ TABLE it_rkpf
      WITH KEY rsnum = it_resb-rsnum BINARY SEARCH.

    IF NOT ( it_rkpf-kzver IS INITIAL ) AND " criada por ordem e...
       it_resb-postp EQ c_n.                " ...item não estocável
*     Ignorar a reserva pois nesse caso o que deve ser medido é o nível
*     de serviço da requisição de compra associada a essa reserva
      DELETE it_resb INDEX vln_tabix.
      CONTINUE.
    ENDIF.

    IF NOT it_resb-aufnr IS INITIAL.
      READ TABLE it_caufv WITH KEY aufnr = it_resb-aufnr
                                           BINARY SEARCH.
      IF sy-subrc         EQ 0 AND
         it_caufv-no_disp EQ c_x.
        DELETE it_resb INDEX vln_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF it_resb-xloek EQ c_x.
      "Reserva  cancelada
      MOVE c_c TO it_resb-situ.
    ELSEIF
           it_resb-enmng GE it_resb-bdmng.
      "Reserva baixada
      MOVE c_b TO it_resb-situ.

    ELSEIF it_resb-kzear EQ c_x OR
           it_resb-enmng EQ 0.
      "Reserva cancelada
      MOVE c_c TO it_resb-situ.

    ELSE.
      "Reserva em aberto
      MOVE c_a TO it_resb-situ.

      it_mat_rsab-matnr = it_resb-matnr.
      it_mat_rsab-werks = it_resb-werks.
      it_mat_rsab-lgort = it_resb-lgort.
      it_mat_rsab-pspel = it_resb-pspel.

      IF it_mat_rsab-lgort IS INITIAL.
        it_mat_rsab-lgort = c_0001.
      ENDIF.

      APPEND it_mat_rsab.
      CLEAR: it_mat_rsab.
    ENDIF.

    MODIFY it_resb INDEX vln_tabix TRANSPORTING situ.
  ENDLOOP.

*** Analisar capacidade de atendimento das reservas que deveriam ser
*** atendidas na data de processamento, mas, que continuam em aberto
  CLEAR: it_rs_pnd_mt, it_mard, it_qbew.
  REFRESH: it_rs_pnd_mt, it_mard, it_qbew.

  IF NOT it_mat_rsab[] IS INITIAL.

*** Selectionar RESB
    SELECT rsnum rspos matnr werks lgort bdter
           bdmng enmng bwart aufnr pspel
    FROM resb
    INTO TABLE it_rs_pnd_mt
    FOR ALL ENTRIES IN it_mat_rsab
    WHERE matnr EQ it_mat_rsab-matnr
      AND werks EQ it_mat_rsab-werks
      AND xloek NE c_x
*      AND kzear EQ space
      AND bdter LE s_bdter-low
      AND no_disp EQ space
      AND ( postp EQ c_l OR
            rssta EQ c_m )
      AND flgex   EQ space.
    LOOP AT it_rs_pnd_mt.

      IF it_rs_pnd_mt-lgort IS INITIAL.
        it_rs_pnd_mt-lgort = c_0001.
        MODIFY it_rs_pnd_mt INDEX sy-tabix.
      ENDIF.

    ENDLOOP.

* - Seleciona centros operacionais e devolução
    SELECT werks j_1bbranch ytp_centro FROM t001w
          INTO TABLE t_t001w
          FOR ALL ENTRIES IN it_mat_rsab
          WHERE j_1bbranch EQ it_mat_rsab-werks
            AND ( ytp_centro = space OR
                  ytp_centro = c_d ).
    LOOP AT t_t001w.
      IF t_t001w-ytp_centro = c_d.
        r_wrk_d-sign   = c_i.
        r_wrk_d-option = c_eq.
        r_wrk_d-low    = t_t001w-werks.
        APPEND r_wrk_d.
      ENDIF.
    ENDLOOP.

*** Selecionar MARD
    SELECT matnr werks lgort labst insme
    FROM mard
    INTO TABLE it_mard
    FOR ALL ENTRIES IN it_mat_rsab
    WHERE matnr EQ it_mat_rsab-matnr
      AND werks EQ it_mat_rsab-werks.
*      AND lgort EQ it_mat_rsab-lgort.

*** Selecionar MARD para Devolução (todos os depósitos)
    IF r_wrk_d[] IS NOT INITIAL.
      SELECT matnr werks lgort labst insme
      FROM mard
      INTO TABLE t_mard_d
      FOR ALL ENTRIES IN it_mat_rsab
      WHERE matnr EQ it_mat_rsab-matnr
        AND werks IN r_wrk_d.
    ENDIF.

*** Selecionar QBEW
    SELECT matnr bwkey pspnr lbkum
    FROM qbew
    INTO TABLE it_qbew
    FOR ALL ENTRIES IN it_mat_rsab
    WHERE matnr EQ it_mat_rsab-matnr
      AND bwkey EQ it_mat_rsab-werks
      AND pspnr EQ it_mat_rsab-pspel.

    SORT: it_mard  BY matnr werks,
          it_qbew  BY matnr bwkey pspnr,
          t_mard_d BY matnr werks.

*** Selecionar RESB para Devolução
    IF r_wrk_d[] IS NOT INITIAL.
      SELECT rsnum rspos matnr werks bdmng enmng
            FROM resb
            INTO TABLE t_rs_dev
            FOR ALL ENTRIES IN it_mat_rsab
            WHERE matnr EQ it_mat_rsab-matnr
              AND werks IN r_wrk_d
              AND xloek NE c_x
              AND bdter LE s_bdter-low
              AND no_disp EQ space
              AND ( postp EQ c_l OR
                    rssta EQ c_m )
              AND flgex   EQ space.
    ENDIF.
  ENDIF.

  SORT: it_mat_rsab  BY matnr werks lgort,
        it_rs_pnd_mt BY matnr werks lgort bdter.

  CLEAR: vln_tabix, vgn_estoque.

*** Leitura dos dados da tabela IT_MAT_RSAB
  LOOP AT it_mat_rsab WHERE pspel IS INITIAL.
    CLEAR: it_mard, v_stq_tot, vgn_estoque.

    LOOP AT it_mard WHERE matnr = it_mat_rsab-matnr
                      AND werks = it_mat_rsab-werks.
      vgn_estoque = vgn_estoque + it_mard-labst.
      v_stq_tot   = v_stq_tot + it_mard-labst + it_mard-insme.
    ENDLOOP.

* - Soma saldo do centro de devolução
    CLEAR: l_est_rsv_d, it_mard, l_centro_op.

    READ TABLE t_t001w WITH KEY werks = it_mat_rsab-werks.
    IF sy-subrc EQ 0 AND
       t_t001w-ytp_centro EQ space.
      l_centro_op = c_x.
    ENDIF.

    IF l_centro_op EQ c_x.                 "Somar saldo centro de devolução apenas para centro operacional

      IF it_mat_rsab-werks EQ c_ctm.
        l_wrk_dev = c_ctmd.
      ELSE.
        READ TABLE t_t001w WITH KEY bbranch    = it_mat_rsab-werks
                                    ytp_centro = c_d.
        IF sy-subrc EQ 0.
          l_wrk_dev = t_t001w-werks.
        ENDIF.
      ENDIF.

      IF it_mat_rsab-werks NE l_wrk_dev.

        LOOP AT t_rs_dev WHERE matnr = it_mat_rsab-matnr
                           AND werks = l_wrk_dev.
          l_est_rsv_d = ( t_rs_dev-bdmng - t_rs_dev-enmng ) + l_est_rsv_d.
        ENDLOOP.

        READ TABLE t_mard_d WITH KEY matnr = it_mat_rsab-matnr
                                     werks = l_wrk_dev
                                     BINARY SEARCH.
        IF sy-subrc EQ 0.
          vgn_estoque = ( t_mard_d-labst - l_est_rsv_d ) + vgn_estoque.
          v_stq_tot   = ( t_mard_d-labst - l_est_rsv_d ) + t_mard_d-insme + v_stq_tot.
        ENDIF.

      ENDIF.

    ENDIF.

    LOOP AT it_rs_pnd_mt WHERE matnr EQ it_mat_rsab-matnr
                           AND werks EQ it_mat_rsab-werks
                           AND lgort EQ it_mat_rsab-lgort
                           AND pspel IS INITIAL.

      vln_tabix = sy-tabix.

      "MOVE: vgn_estoque   TO it_rs_pnd_mt-ym_sldvr.

      it_rs_pnd_mt-ym_sldvr = it_rs_pnd_mt-bdmng = it_rs_pnd_mt-bdmng - it_rs_pnd_mt-enmng.

      IF vgn_estoque LE 0.
        "Reserva não pode ser atendida
        MOVE: c_n TO it_rs_pnd_mt-atdm.
      ELSE.

        IF it_rs_pnd_mt-bdmng LT 0.
          MOVE: 0 TO it_rs_pnd_mt-bdmng.
        ELSEIF it_rs_pnd_mt-bdmng GT vgn_estoque. "it_mard-labst.
          "Reserva atendida parcialmente
          MOVE: c_p TO it_rs_pnd_mt-atdm.
        ELSEIF it_rs_pnd_mt-bdmng LE vgn_estoque. "it_mard-labst.
          "Reserva atendida totalmente
          MOVE: c_s TO it_rs_pnd_mt-atdm.
        ENDIF.

*        it_mard-labst = it_mard-labst - it_rs_pnd_mt-bdmng.
        vgn_estoque = vgn_estoque - it_rs_pnd_mt-bdmng.
*        IF it_mard-labst LT 0.
        IF vgn_estoque LT 0.
          MOVE 0 TO vgn_estoque. "it_mard-labst.
        ENDIF.
      ENDIF.

      IF v_stq_tot NE it_rs_pnd_mt-ym_sldvr.    "Se o saldo c/ controle qualidade for igual ao saldo livre não apurar controle qualidade
        IF v_stq_tot LE 0.
          "Reserva não pode ser atendida
          MOVE: c_n TO it_rs_pnd_mt-atmcq.
        ELSE.

          IF it_rs_pnd_mt-bdmng GT v_stq_tot.     "it_mard-labst.
            "Reserva atendida parcialmente
            MOVE: c_p TO it_rs_pnd_mt-atmcq.
          ELSEIF it_rs_pnd_mt-bdmng LE v_stq_tot.     "it_mard-labst.
            "Reserva atendida totalmente
            MOVE: c_s TO it_rs_pnd_mt-atmcq.
          ENDIF.

          v_stq_tot = v_stq_tot - it_rs_pnd_mt-bdmng.
          IF v_stq_tot LT 0.
            MOVE 0 TO v_stq_tot.
          ENDIF.
        ENDIF.
      ENDIF.

      MOVE: vgn_estoque   TO it_rs_pnd_mt-ym_sldre.

      MODIFY it_rs_pnd_mt INDEX vln_tabix TRANSPORTING atdm atmcq ym_sldvr ym_sldre.

    ENDLOOP. "IT_RS_PND_MT
  ENDLOOP. "it_mat_rsab

  SORT: it_mat_rsab  BY matnr werks pspel,
        it_rs_pnd_mt BY matnr werks pspel bdter.

  CLEAR: vln_tabix, vgn_estoque.

*** Leitura dos dados da tabela IT_MAT_RSAB
  LOOP AT it_mat_rsab WHERE pspel NE space.
    CLEAR: it_qbew.

    READ TABLE it_qbew WITH KEY matnr = it_mat_rsab-matnr
                                bwkey = it_mat_rsab-werks
                                pspnr = it_mat_rsab-pspel
                                BINARY SEARCH.

    vgn_estoque = it_qbew-lbkum.

    LOOP AT it_rs_pnd_mt WHERE matnr EQ it_mat_rsab-matnr
                           AND werks EQ it_mat_rsab-werks
                           AND lgort EQ it_mat_rsab-lgort
                           AND pspel EQ it_mat_rsab-pspel.

      vln_tabix = sy-tabix.

      MOVE: it_qbew-lbkum TO it_rs_pnd_mt-ym_sldvr,
            vgn_estoque   TO it_rs_pnd_mt-ym_sldre.

      IF it_qbew-lbkum LE 0.
        "Reserva não pode ser atendida
        MOVE: c_n TO it_rs_pnd_mt-atdm.

      ELSEIF it_qbew-lbkum GT 0.
        it_rs_pnd_mt-bdmng = it_rs_pnd_mt-bdmng - it_rs_pnd_mt-enmng.

        IF it_rs_pnd_mt-bdmng LT 0.
          MOVE: 0 TO it_rs_pnd_mt-bdmng.
        ELSEIF it_rs_pnd_mt-bdmng GT it_qbew-lbkum.
          "Reserva pode ser alterada parcialmente
          MOVE: c_p TO it_rs_pnd_mt-atdm.
        ELSEIF it_rs_pnd_mt-bdmng LE it_qbew-lbkum.
          "Reserva pode ser atendida totalmente
          MOVE: c_s TO it_rs_pnd_mt-atdm.
        ENDIF.

        it_qbew-lbkum = it_qbew-lbkum - it_rs_pnd_mt-bdmng.

        IF it_qbew-lbkum LT 0.
          MOVE: 0 TO it_qbew-lbkum.
        ENDIF.
      ENDIF.

      MODIFY it_rs_pnd_mt INDEX vln_tabix TRANSPORTING atdm ym_sldvr ym_sldre.

    ENDLOOP. "it_rs_pnd_mt
  ENDLOOP. "it_mat_rsab

  SORT it_rs_pnd_mt BY rsnum rspos.

*** Apurar dados de reservas que deveriam ser baixadas na
*** data de processamento
  LOOP AT it_resb.
    CLEAR: it_mat_rsv, it_rs_pnd_mt, it_rkpf.

*   Posicionar nos dados de cabeçalho da reserva
    READ TABLE it_rkpf
      WITH KEY rsnum = it_resb-rsnum BINARY SEARCH.

    READ TABLE it_mat_rsv ASSIGNING FIELD-SYMBOL(<fs_mat_rsv>) WITH KEY ym_rsnum = it_resb-rsnum
                                                                        ym_rspos = it_resb-rspos.

    IF sy-subrc IS INITIAL. "Atualizar os valores de quantidade reserva

      READ TABLE it_rs_pnd_mt WITH KEY rsnum = it_resb-rsnum
                                 rspos = it_resb-rspos
                                 BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF it_resb-situ EQ c_a.
          <fs_mat_rsv>-ym_sldvr = it_rs_pnd_mt-ym_sldvr.
          <fs_mat_rsv>-ym_sldre = it_rs_pnd_mt-ym_sldre.
        ENDIF.

      ENDIF.
    ELSE.
      READ TABLE it_rs_pnd_mt WITH KEY rsnum = it_resb-rsnum
                                       rspos = it_resb-rspos
                                       BINARY SEARCH.
*      IF sy-subrc EQ 0.
      it_mat_rsv-ym_rsnum = it_resb-rsnum.
      it_mat_rsv-ym_rspos = it_resb-rspos.
      it_mat_rsv-ym_matnr = it_resb-matnr.
      it_mat_rsv-ym_werks = it_resb-werks.
      it_mat_rsv-ym_lgort = it_resb-lgort.
      it_mat_rsv-ym_dtprc = s_bdter-low.
      it_mat_rsv-ym_strsv = it_resb-situ.
      it_mat_rsv-ym_bdter = it_resb-bdter.
      it_mat_rsv-ym_bdmng = it_resb-bdmng.
      it_mat_rsv-ym_enmng = it_resb-enmng.

      IF NOT ( it_resb-pspel IS INITIAL ).
        it_mat_rsv-ym_knttp = c_q.
        it_mat_rsv-ym_pspel = it_resb-pspel.

        READ TABLE it_caufv WITH KEY aufnr = it_resb-aufnr  BINARY SEARCH.
        IF sy-subrc = 0.
          it_mat_rsv-autyp = it_caufv-autyp.
        ENDIF.

      ELSEIF NOT ( it_rkpf-ps_psp_pnr IS INITIAL ).
        it_mat_rsv-ym_knttp = c_p.
        it_mat_rsv-ym_elpep = it_rkpf-ps_psp_pnr.

      ELSEIF NOT ( it_rkpf-nplnr IS INITIAL ).
        it_mat_rsv-ym_knttp = c_n.
        it_mat_rsv-ym_nplnr = it_rkpf-nplnr.

        READ TABLE it_caufv WITH KEY aufnr = it_resb-aufnr  BINARY SEARCH.
        IF sy-subrc = 0.
          it_mat_rsv-autyp = it_caufv-autyp.
        ENDIF.

      ELSEIF NOT ( it_rkpf-kostl IS INITIAL ).
        it_mat_rsv-ym_knttp = c_k.
        it_mat_rsv-ym_kostl = it_rkpf-kostl.

      ELSEIF NOT ( it_resb-aufnr IS INITIAL ).
        it_mat_rsv-ym_knttp = c_f.
        it_mat_rsv-ym_aufnr = it_resb-aufnr.

        CLEAR it_caufv.
        READ TABLE it_caufv WITH KEY aufnr = it_resb-aufnr
                                             BINARY SEARCH.
        IF sy-subrc = 0.
          it_mat_rsv-autyp = it_caufv-autyp.
        ENDIF.

      ENDIF.

      IF it_resb-sobkz EQ c_q.
        it_mat_rsv-ym_knttp = c_q.
      ENDIF.

      IF it_resb-situ EQ c_a.
        it_mat_rsv-ym_nvatm = it_rs_pnd_mt-atdm.
        it_mat_rsv-ym_atmcq = it_rs_pnd_mt-atmcq.
        it_mat_rsv-ym_sldvr = it_rs_pnd_mt-ym_sldvr.
        it_mat_rsv-ym_sldre = it_rs_pnd_mt-ym_sldre.
      ELSE.
        it_mat_rsv-ym_nvatm = c_s.
      ENDIF.

      it_mat_rsv-ym_kzver = it_rkpf-kzver.
      it_mat_rsv-ym_bwart = it_resb-bwart.

      IF it_mat_rsv-ym_kzver IS INITIAL.
        it_mat_rsv-objectclas = c_ym_resb.
        CONCATENATE sy-mandt it_mat_rsv-ym_rsnum
               INTO it_mat_rsv-objectid.
      ELSE.
        it_mat_rsv-objectclas = c_order.

        IF it_mat_rsv-ym_nplnr IS NOT INITIAL.
          CONCATENATE sy-mandt it_mat_rsv-autyp it_mat_rsv-ym_nplnr
                 INTO it_mat_rsv-objectid.
        ELSE.
          CONCATENATE sy-mandt it_mat_rsv-autyp it_mat_rsv-ym_aufnr
                 INTO it_mat_rsv-objectid.
        ENDIF.
      ENDIF.

      it_mat_rsv-tabname = c_resb.
      CONCATENATE sy-mandt it_mat_rsv-ym_rsnum it_mat_rsv-ym_rspos
             INTO it_mat_rsv-tabkey.

      it_mat_rsv-ym_strsv_rc = it_mat_rsv-ym_strsv.

      APPEND it_mat_rsv.
    ENDIF.
  ENDLOOP. "it_resb

ENDFORM.                    " YF_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  YF_ATUALIZAR_TAB
*&---------------------------------------------------------------------*
FORM yf_atualizar_tab .

*** Atualizar a tabela transparente ZZ005_MM
  LOOP AT it_mat_rsv.

* Verificar se o registro já existe na tabela ZZ005_MM
    SELECT *
    FROM zz005_mm UP TO 1 ROWS
    INTO zz005_mm
    WHERE ym_rsnum EQ it_mat_rsv-ym_rsnum
      AND ym_rspos EQ it_mat_rsv-ym_rspos.
    ENDSELECT.

    IF sy-subrc NE 0.
      MOVE-CORRESPONDING it_mat_rsv TO zz005_mm.
      INSERT zz005_mm.
      COMMIT WORK.
    ENDIF.
    APPEND zz005_mm TO it_dados.
  ENDLOOP.

  "Execução finalizada com sucesso...
  MESSAGE s083 WITH TEXT-s01.
  EXIT.

ENDFORM.                    " YF_ATUALIZAR_TAB

*&---------------------------------------------------------------------*
*&      Form  YF_VERIF_DESCONSID_ESTORNOS
*&---------------------------------------------------------------------*
FORM yf_verif_desconsid_estornos .
  DATA: BEGIN OF it_t156 OCCURS 0,
          bwart TYPE t156-bwart,
          shkzg TYPE t156-shkzg,
          rstyp TYPE t156-rstyp,
          xstbw TYPE t156-xstbw,
        END OF it_t156,

        wa_mseg     LIKE it_mseg,
        wa_mseg_aux LIKE it_mseg,
        wa_t156     LIKE it_t156.

* Buscar dados cadastrais de tipos de movimento
  SELECT bwart shkzg rstyp xstbw
    INTO TABLE it_t156
    FROM t156
    WHERE bwart IN s_bwart.

  SORT it_t156 BY bwart.

  LOOP AT it_t156 INTO wa_t156
    WHERE xstbw EQ c_x.

*   Ler o movimento de estorno pelo tipo de movimento (de estorno)
    LOOP AT it_mseg INTO wa_mseg
      WHERE bwart EQ wa_t156-bwart
        AND smbln NE space.

*     Eliminar o movimento de estorno
      DELETE it_mseg INDEX sy-tabix.

*     Localizar o movimento estornado
      CLEAR wa_mseg_aux.

      READ TABLE it_mseg INTO wa_mseg_aux
        WITH KEY mblnr = wa_mseg-smbln
                 mjahr = wa_mseg-sjahr
                 zeile = wa_mseg-smblp.
      IF sy-subrc EQ 0.
*       Se encontrar o movimento estornado, eliminar
        DELETE it_mseg INDEX sy-tabix.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

* Desconsiderar movimentos que não sejam para reservas
  DELETE it_mseg WHERE rsnum IS INITIAL.
ENDFORM.                    " YF_VERIF_DESCONSID_ESTORNOS
*&---------------------------------------------------------------------*
*&      Form  YF_ALV
*&---------------------------------------------------------------------*
FORM yf_alv .

*Formata o Header das colunas
  PERFORM yf_monta_header_column.

* Formata Layout
  PERFORM yf_formata_layout.

* Executa função ALV
  PERFORM yf_call_function_alv.


ENDFORM.                    " YF_ALV
*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_HEADER_COLUMN
*&---------------------------------------------------------------------*
FORM yf_monta_header_column .

  DATA: l_tabname TYPE slis_tabname VALUE 'IT_DADOS'.       "+TI 43934

  PERFORM yf_colum_header USING:
       'YM_RSNUM'    l_tabname  TEXT-t01 '' '',
       'YM_RSPOS'    l_tabname  TEXT-t02 '' '' ,
       'YM_MATNR'    l_tabname  TEXT-t03 'MATNR' 'MARA',
       'YM_WERKS'    l_tabname  TEXT-t04 '' '' ,
       'YM_LGORT'    l_tabname  TEXT-t05 '' '',
       'YM_STRSV'    l_tabname  TEXT-t06 '' '' ,
       'YM_BDTER'    l_tabname  TEXT-t07 '' '' ,
       'YM_NVATM'    l_tabname  TEXT-t08 '' '' ,
       'YM_ATMCQ'    l_tabname  TEXT-t11 '' '' ,
       'YM_BWART'    l_tabname  space    'BWART' 'MSEG',    "+TI 43934
       'YM_KZVER'    l_tabname  space    'KZVER' 'RKPF'.    "+TI 43934

ENDFORM.                    " YF_MONTA_HEADER_COLUMN
*&---------------------------------------------------------------------*
*&      Form  YF_COLUM_HEADER
*&---------------------------------------------------------------------*
FORM yf_colum_header  USING p_fieldname     LIKE it_fieldcat-fieldname
                            p_tabname       LIKE it_fieldcat-tabname
                            p_seltext_l     LIKE it_fieldcat-seltext_l
                            p_ref_fieldname LIKE it_fieldcat-ref_fieldname
                            p_ref_tabname   LIKE it_fieldcat-ref_tabname.

  it_fieldcat-fieldname     =  p_fieldname.
  it_fieldcat-tabname       =  p_tabname.
  it_fieldcat-seltext_l     =  p_seltext_l.
  it_fieldcat-ref_fieldname =  p_ref_fieldname.
  it_fieldcat-ref_tabname   =  p_ref_tabname.
  APPEND it_fieldcat.
  CLEAR  it_fieldcat.

ENDFORM.                    " YF_COLUM_HEADER
*&---------------------------------------------------------------------*
*&      Form  YF_CALL_FUNCTION_ALV
*&---------------------------------------------------------------------*
FORM yf_call_function_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = it_fieldcat[]
      i_default          = 'X'
      i_save             = 'A'
      is_layout          = e_layout
    TABLES
      t_outtab           = it_dados
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc NE 0.
    MESSAGE w252(s#) WITH TEXT-015. " Erro na geração do relatório ALV
    STOP.
  ENDIF.


ENDFORM.                    " YF_CALL_FUNCTION_ALV
*&---------------------------------------------------------------------*
*&      Form  YF_FORMATA_LAYOUT
*&---------------------------------------------------------------------*
FORM yf_formata_layout .

  e_layout-colwidth_optimize = 'X'.
  e_layout-zebra             = 'X'.

ENDFORM.                    " YF_FORMATA_LAYOUT
* - +TI 43934 - Início - Vinícius Melo - 05/05/2014
*&---------------------------------------------------------------------*
*&      Form  YF_ANALISAR_TEMPO_RC
*&---------------------------------------------------------------------*
*       Analisar Tempo RCs
*----------------------------------------------------------------------*
FORM yf_analisar_tempo_rc .

  DATA: e_eban TYPE y_eban,
        e_ebkn TYPE y_ebkn,
        e_marc TYPE y_marc.

  DATA: l_date_from  TYPE ltak-bdatu,
        l_date_to    TYPE ltak-bdatu,
        l_delta_time TYPE mcwmit-be_ae,
        l_delta_timp TYPE mcwmit-be_ae,
        l_delta_unit TYPE mcwmit-lzeit,
        l_time_from  TYPE ltak-bzeit,
        l_time_to    TYPE ltak-bzeit,
        l_existe_rc  TYPE c.                "TOP 4404386 - Luana Prates - 11/10/16

  FIELD-SYMBOLS: <f_mat_rsv> TYPE y_mat_rsv.

  CONSTANTS: c_urg TYPE rvari_vnam   VALUE 'BSART_EMERG',
             c_obj TYPE rvari_vnam   VALUE 'ZCUBS_MM-810'.

  DATA o_tvarv TYPE REF TO zcl_glit_tvarv.

  DATA r_urg TYPE RANGE OF eban-bsart.

  CREATE OBJECT o_tvarv
    EXPORTING
      im_id = c_obj.

  IF o_tvarv IS BOUND.
    CALL METHOD o_tvarv->get_multiple
      EXPORTING
        im_param = c_urg
      IMPORTING
        ex_value = r_urg.
  ENDIF.

  SORT: t_eban  BY matnr werks knttp ASCENDING badat DESCENDING,
        t_marc  BY matnr werks.


  LOOP AT it_mat_rsv ASSIGNING <f_mat_rsv>.

    PERFORM yf_busca_data_hora USING    <f_mat_rsv>-objectclas
                                        <f_mat_rsv>-objectid
                               CHANGING l_date_from
                                        l_time_from.

    READ TABLE t_marc INTO e_marc
                           WITH KEY matnr = <f_mat_rsv>-ym_matnr
                                    werks = <f_mat_rsv>-ym_werks
                                                  BINARY SEARCH.
    CHECK sy-subrc = 0.

    READ TABLE it_zz014_mm ASSIGNING FIELD-SYMBOL(<fs_zz014_mm>) WITH KEY ym_werks = <f_mat_rsv>-ym_werks BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      l_delta_timp = <fs_zz014_mm>-ym_rcers * 60.
    ENDIF.

    IF <fs_zz014_mm> IS ASSIGNED.
      IF e_marc-dismm NE <fs_zz014_mm>-ym_tpmrp.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    CLEAR l_existe_rc.

    REFRESH: t_ebkn, t_eban.

    IF <f_mat_rsv>-ym_knttp EQ c_f.

      SELECT banfn bnfpo zebkn loekz aufnr
             kostl ps_psp_pnr  nplnr
             FROM ebkn
             INTO TABLE t_ebkn
             WHERE aufnr = <f_mat_rsv>-ym_aufnr.

    ELSEIF <f_mat_rsv>-ym_knttp EQ c_q.

      SELECT banfn bnfpo zebkn loekz aufnr
             kostl ps_psp_pnr  nplnr
             FROM ebkn
             INTO TABLE t_ebkn
             WHERE aufnr = <f_mat_rsv>-ym_pspel.

    ELSEIF <f_mat_rsv>-ym_knttp EQ c_p.

      SELECT banfn bnfpo zebkn loekz aufnr
             kostl ps_psp_pnr  nplnr
             FROM ebkn
             INTO TABLE t_ebkn
             WHERE ps_psp_pnr = <f_mat_rsv>-ym_elpep.

    ELSEIF <f_mat_rsv>-ym_knttp EQ c_n.

      SELECT banfn bnfpo zebkn loekz aufnr
             kostl ps_psp_pnr  nplnr
             FROM ebkn
             INTO TABLE t_ebkn
             WHERE nplnr = <f_mat_rsv>-ym_nplnr.

    ELSEIF <f_mat_rsv>-ym_knttp EQ c_k.

      SELECT banfn bnfpo zebkn loekz aufnr
             kostl ps_psp_pnr  nplnr
             FROM ebkn
             INTO TABLE t_ebkn
             WHERE kostl = <f_mat_rsv>-ym_kostl.

    ENDIF.

    IF t_ebkn[] IS NOT INITIAL.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
             FROM eban
             INTO TABLE t_eban
             FOR ALL ENTRIES IN t_ebkn
             WHERE banfn    EQ t_ebkn-banfn
               AND bnfpo    EQ t_ebkn-bnfpo
               AND matnr    IN s_matnr
               AND werks    IN s_werks
               AND matnr    EQ <f_mat_rsv>-ym_matnr         "HD 5060530
               AND werks    EQ <f_mat_rsv>-ym_werks         "HD 5060530
               AND prio_urg IN r_urg.

      SORT t_eban BY badat DESCENDING.                      "HD 5060530

    ENDIF.

    LOOP AT t_eban INTO e_eban.

      READ TABLE t_ebkn INTO e_ebkn WITH KEY banfn = e_eban-banfn
                                             bnfpo = e_eban-bnfpo.
      IF sy-subrc EQ 0.
        IF it_mat_rsv-ym_knttp EQ c_q AND
           e_ebkn-ps_psp_pnr   EQ <f_mat_rsv>-ym_pspel.
          l_existe_rc = c_x.
          EXIT.
        ELSEIF it_mat_rsv-ym_knttp EQ c_p AND
               e_ebkn-ps_psp_pnr   EQ <f_mat_rsv>-ym_elpep.
          l_existe_rc = c_x.
          EXIT.
        ELSEIF it_mat_rsv-ym_knttp EQ c_n AND
               e_ebkn-nplnr        EQ <f_mat_rsv>-ym_nplnr.
          l_existe_rc = c_x.
          EXIT.
        ELSEIF it_mat_rsv-ym_knttp EQ c_k AND
               e_ebkn-kostl        EQ <f_mat_rsv>-ym_kostl.
          l_existe_rc = c_x.
          EXIT.
        ELSEIF it_mat_rsv-ym_knttp EQ c_f AND
               e_ebkn-aufnr        EQ <f_mat_rsv>-ym_aufnr.
          l_existe_rc = c_x.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_existe_rc EQ c_x.

      CLEAR: l_date_from,
             l_date_to,
             l_time_from,
             l_time_to,
             l_delta_time,
             l_delta_unit.

* - Início - TOP 4404386 - Luana Prates - 11/10/16
      IF <f_mat_rsv>-ym_strsv = c_r.
        SELECT SINGLE * FROM zz007_mm
               WHERE ym_banfn = e_eban-banfn
                 AND ym_bnfpo = e_eban-bnfpo.
        IF sy-subrc NE 0.
          zz007_mm-ym_banfn = e_eban-banfn.
          zz007_mm-ym_bnfpo = e_eban-bnfpo.
          zz007_mm-ym_rsnum = <f_mat_rsv>-ym_rsnum.
          zz007_mm-ym_rspos = <f_mat_rsv>-ym_rspos.
          INSERT zz007_mm.
          COMMIT WORK.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " YF_ANALISAR_TEMPO_RC
*&---------------------------------------------------------------------*
*&      Form  YF_SELECIONA_RC
*&---------------------------------------------------------------------*
*       Seleciona RCs
*----------------------------------------------------------------------*
FORM yf_seleciona_rc .

  DATA: t_mat_rsv_aux TYPE TABLE OF y_mat_rsv,
        e_mat_rsv     TYPE y_mat_rsv.

  LOOP AT it_mat_rsv WHERE ym_knttp = c_f.

    e_mat_rsv = it_mat_rsv.

    APPEND e_mat_rsv TO t_mat_rsv_aux.

  ENDLOOP.

  IF t_mat_rsv_aux[] IS NOT INITIAL.

    SELECT banfn bnfpo zebkn loekz aufnr
           kostl ps_psp_pnr  nplnr
      FROM ebkn
      INTO TABLE t_ebkn
       FOR ALL ENTRIES IN t_mat_rsv_aux
     WHERE aufnr = t_mat_rsv_aux-ym_aufnr.

    IF sy-subrc = 0.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
        FROM eban
        INTO TABLE t_eban
         FOR ALL ENTRIES IN t_ebkn
       WHERE banfn = t_ebkn-banfn
         AND bnfpo = t_ebkn-bnfpo.
    ENDIF.
  ENDIF.

* - Selecionar RCs para atendimento a estoque de projeto
  LOOP AT it_mat_rsv WHERE ym_knttp = c_q.

    e_mat_rsv = it_mat_rsv.

    APPEND e_mat_rsv TO t_mat_rsv_aux.

  ENDLOOP.

  IF t_mat_rsv_aux[] IS NOT INITIAL.

    SELECT banfn bnfpo zebkn loekz aufnr
           kostl ps_psp_pnr  nplnr
      FROM ebkn
      APPENDING TABLE t_ebkn
      FOR ALL ENTRIES IN t_mat_rsv_aux
      WHERE ps_psp_pnr = t_mat_rsv_aux-ym_pspel.

    IF t_ebkn[] IS NOT INITIAL.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
        FROM eban
        APPENDING TABLE t_eban
        FOR ALL ENTRIES IN t_ebkn
        WHERE banfn = t_ebkn-banfn
          AND bnfpo = t_ebkn-bnfpo
          AND matnr IN s_matnr
          AND werks IN s_werks.
    ENDIF.

  ENDIF.
  CLEAR t_mat_rsv_aux[].

* - Selecionar RCs para atendimento a elemento PEP
  LOOP AT it_mat_rsv WHERE ym_knttp = c_p.

    e_mat_rsv = it_mat_rsv.

    APPEND e_mat_rsv TO t_mat_rsv_aux.

  ENDLOOP.

  IF t_mat_rsv_aux[] IS NOT INITIAL.

    SELECT banfn bnfpo zebkn loekz aufnr
           kostl ps_psp_pnr  nplnr
      FROM ebkn
      APPENDING TABLE t_ebkn
      FOR ALL ENTRIES IN t_mat_rsv_aux
      WHERE ps_psp_pnr = t_mat_rsv_aux-ym_elpep.

    IF t_ebkn[] IS NOT INITIAL.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
        FROM eban
        APPENDING TABLE t_eban
        FOR ALL ENTRIES IN t_ebkn
        WHERE banfn = t_ebkn-banfn
          AND bnfpo = t_ebkn-bnfpo
          AND matnr IN s_matnr
          AND werks IN s_werks.
    ENDIF.

  ENDIF.
  CLEAR t_mat_rsv_aux[].

* - Selecionar RCs para atendimento a diagrama de rede
  LOOP AT it_mat_rsv WHERE ym_knttp = c_n.

    e_mat_rsv = it_mat_rsv.

    APPEND e_mat_rsv TO t_mat_rsv_aux.

  ENDLOOP.

  IF t_mat_rsv_aux[] IS NOT INITIAL.

    SELECT banfn bnfpo zebkn loekz aufnr
           kostl ps_psp_pnr  nplnr
      FROM ebkn
      APPENDING TABLE t_ebkn
      FOR ALL ENTRIES IN t_mat_rsv_aux
      WHERE nplnr = t_mat_rsv_aux-ym_nplnr.

    IF t_ebkn[] IS NOT INITIAL.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
        FROM eban
        APPENDING TABLE t_eban
        FOR ALL ENTRIES IN t_ebkn
        WHERE banfn = t_ebkn-banfn
          AND bnfpo = t_ebkn-bnfpo
          AND matnr IN s_matnr
          AND werks IN s_werks.
    ENDIF.

  ENDIF.
  CLEAR t_mat_rsv_aux[].

* - Selecionar RCs para atendimento a centro de custo
  LOOP AT it_mat_rsv WHERE ym_knttp = c_k.

    e_mat_rsv = it_mat_rsv.

    APPEND e_mat_rsv TO t_mat_rsv_aux.

  ENDLOOP.

  IF t_mat_rsv_aux[] IS NOT INITIAL.

    SELECT banfn bnfpo zebkn loekz aufnr
           kostl ps_psp_pnr  nplnr
      FROM ebkn
      APPENDING TABLE t_ebkn
      FOR ALL ENTRIES IN t_mat_rsv_aux
      WHERE kostl = t_mat_rsv_aux-ym_kostl.

    IF t_ebkn[] IS NOT INITIAL.
      SELECT banfn bnfpo loekz matnr werks badat arsnr arsps knttp prio_urg
        FROM eban
        APPENDING TABLE t_eban
        FOR ALL ENTRIES IN t_ebkn
        WHERE banfn = t_ebkn-banfn
          AND bnfpo = t_ebkn-bnfpo
          AND matnr IN s_matnr
          AND werks IN s_werks.
    ENDIF.

  ENDIF.
  CLEAR t_mat_rsv_aux[].
ENDFORM.                    " YF_SELECIONA_RC
* - +TI 43934 - Fim    - Vinícius Melo - 05/05/2014
*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DATA_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MAT_RSV_OBJECTCLAS  text
*      -->P_IT_MAT_RSV_OBJECTID  text
*      <--P_L_DATE_FROM  text
*      <--P_L_DATE_TO  text
*----------------------------------------------------------------------*
FORM yf_busca_data_hora USING    us_objectclas TYPE cdhdr-objectclas
                                 us_objectid   TYPE cdhdr-objectid
                        CHANGING ch_udate      TYPE ltak-bdatu
                                 ch_utime      TYPE ltak-bzeit.

  DATA: l_changenr  TYPE cdpos-changenr.

  CLEAR: ch_udate,
         ch_utime.

  SELECT SINGLE changenr FROM cdpos
         INTO l_changenr
         WHERE objectclas = it_mat_rsv-objectclas
           AND objectid   = it_mat_rsv-objectid
           AND tabname    = it_mat_rsv-tabname
           AND tabkey     = it_mat_rsv-tabkey
           AND fname      = c_key
           AND chngind    = c_i.

  SELECT SINGLE udate utime FROM cdhdr
         INTO (ch_udate, ch_utime)
         WHERE objectclas = it_mat_rsv-objectclas
           AND objectid   = it_mat_rsv-objectid
           AND changenr   = l_changenr.

ENDFORM.                    " YF_BUSCA_DATA_HORA
*&---------------------------------------------------------------------*
*&      Form  YF_COMPOR_ESTQ_DATA
*&---------------------------------------------------------------------*
FORM yf_compor_estq_data .

  DATA: t_mvmt_mat  TYPE TABLE OF y_mvmt_mat  WITH HEADER LINE.

  LOOP AT t_mvt_per.

    CLEAR t_mvmt_mat.

    MOVE: t_mvt_per-matnr      TO t_mvmt_mat-matnr,
          t_mvt_per-werks      TO t_mvmt_mat-werks,
          t_mvt_per-lgort      TO t_mvmt_mat-lgort,
          t_mvt_per-ps_psp_pnr TO t_mvmt_mat-ps_psp_pnr.

    IF t_mvt_per-shkzg EQ c_h.
      t_mvmt_mat-qtd_h = t_mvt_per-menge.
      t_mvmt_mat-qtd_s = 0.
    ELSE.
      t_mvmt_mat-qtd_h = 0.
      t_mvmt_mat-qtd_s = t_mvt_per-menge.
    ENDIF.

    COLLECT t_mvmt_mat.
  ENDLOOP.

  LOOP AT t_sld_mat_mvt.

    CLEAR: t_estq_data, t_mvmt_mat.

    READ TABLE t_mvmt_mat WITH KEY matnr = t_sld_mat_mvt-matnr
                                   werks = t_sld_mat_mvt-werks
                                   lgort = t_sld_mat_mvt-lgort.
    IF sy-subrc EQ 0.

      t_estq_data-matnr = t_sld_mat_mvt-matnr.
      t_estq_data-werks = t_sld_mat_mvt-werks.
      t_estq_data-lgort = t_sld_mat_mvt-lgort.

      t_estq_data-estqm = t_sld_mat_mvt-labst - t_mvmt_mat-qtd_s + t_mvmt_mat-qtd_h.

      APPEND t_estq_data.

    ENDIF.
  ENDLOOP.

  LOOP AT t_sld_prj_mvt.

    CLEAR: t_estq_data, t_mvmt_mat.

    READ TABLE t_mvmt_mat WITH KEY matnr      = t_sld_prj_mvt-matnr
                                   werks      = t_sld_prj_mvt-bwkey
                                   ps_psp_pnr = t_sld_prj_mvt-pspnr.
    IF sy-subrc EQ 0.

      t_estq_data-matnr      = t_sld_prj_mvt-matnr.
      t_estq_data-werks      = t_sld_prj_mvt-bwkey.
      t_estq_data-lgort      = t_mvmt_mat-lgort.
      t_estq_data-ps_psp_pnr = t_sld_prj_mvt-pspnr.

      t_estq_data-estqm = t_sld_prj_mvt-lbkum - t_mvmt_mat-qtd_s + t_mvmt_mat-qtd_h.

      APPEND t_estq_data.

    ENDIF.
  ENDLOOP.

  SORT t_estq_data BY matnr werks lgort ps_psp_pnr.

ENDFORM.                    " YF_COMPOR_ESTQ_DATA
