*----------------------------------------------------------------------*
*       CLASS ZCL_LOGGER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_LOGGER definition
  public
  create private .

public section.
  type-pools ABAP .

*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!
  data HEADER type BAL_S_LOG read-only .
  data HANDLE type BALLOGHNDL read-only .
  data DB_NUMBER type BALOGNR read-only .

  class-methods NEW
    importing
      !OBJECT type CSEQUENCE optional
      !SUBOBJECT type CSEQUENCE optional
      !DESC type CSEQUENCE optional
      !CONTEXT type SIMPLE optional
      !AUTO_SAVE type ABAP_BOOL optional
      !SECOND_DB_CONN type ABAP_BOOL default ABAP_TRUE
    returning
      value(LOG) type ref to ZCL_LOGGER .
  class-methods OPEN
    importing
      !OBJECT type CSEQUENCE
      !SUBOBJECT type CSEQUENCE
      !DESC type CSEQUENCE optional
      !CREATE_IF_DOES_NOT_EXIST type ABAP_BOOL default ABAP_FALSE
      !AUTO_SAVE type ABAP_BOOL optional
    returning
      value(LOG) type ref to ZCL_LOGGER .
  methods ADD
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !TYPE type SYMSGTY optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods A
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods E
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods W
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods I
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods S
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods POPUP .
  methods FULLSCREEN .
  methods EXPORT_TO_TABLE
    returning
      value(BAPIRETTAB) type BAPIRETTAB .
  methods GET_AUTOSAVE
    returning
      value(AUTO_SAVE) type ABAP_BOOL .
  methods SET_AUTOSAVE
    importing
      !AUTO_SAVE type ABAP_BOOL .
  methods SAVE .
  PROTECTED SECTION.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  PRIVATE SECTION.

* Local type for hrpad_message as it is not available in an ABAP Development System
    TYPES: BEGIN OF hrpad_message_field_list_alike,
             scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike.

    TYPES: BEGIN OF hrpad_message_alike,
             cause(32)    TYPE c,              "original: hrpad_message_cause
             detail_level TYPE ballevel.
        INCLUDE TYPE symsg .
    TYPES: field_list TYPE STANDARD TABLE OF hrpad_message_field_list_alike
           WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF hrpad_message_alike.

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    DATA auto_save TYPE abap_bool .
    DATA sec_connection TYPE abap_bool .
    DATA sec_connect_commit TYPE abap_bool .
ENDCLASS.



CLASS ZCL_LOGGER IMPLEMENTATION.


  METHOD a.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'A'
      importance    = importance ).
  ENDMETHOD.


  METHOD add.

    DATA: detailed_msg      TYPE bal_s_msg,
          free_text_msg     TYPE char200,
          ctx_type          TYPE REF TO cl_abap_typedescr,
          ctx_ddic_header   TYPE x030l,
          msg_type          TYPE REF TO cl_abap_typedescr,
          msg_table_type    TYPE REF TO cl_abap_tabledescr,
          exception_data    TYPE bal_s_exc,
          log_numbers       TYPE bal_t_lgnm,
          log_handles       TYPE bal_t_logh,
          log_number        TYPE bal_s_lgnm,
          formatted_context TYPE bal_s_cont,
          formatted_params  TYPE bal_s_parm.

    FIELD-SYMBOLS: <table_of_messages> TYPE ANY TABLE,
                   <message_line>      TYPE any,
                   <bapi_msg>          TYPE bapiret2,
                   <bdc_msg>           TYPE bdcmsgcoll,
                   <hrpad_msg>         TYPE hrpad_message_alike,
                   <context_val>       TYPE any.

    IF context IS NOT INITIAL.
      ASSIGN context TO <context_val>.
      formatted_context-value = <context_val>.
      ctx_type = cl_abap_typedescr=>describe_by_data( context ).

      CALL METHOD ctx_type->get_ddic_header
        RECEIVING
          p_header     = ctx_ddic_header
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
    ELSEIF callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
    ENDIF.

    msg_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    IF obj_to_log IS INITIAL.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      exception_data-exception = obj_to_log.
      exception_data-msgty = type.
      exception_data-probclass = importance.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        add( obj_to_log     = <message_line>
             context        = context
             callback_form  = callback_form
             callback_prog  = callback_prog
             callback_fm    = callback_fm
             type           = type
             importance     = importance ).
      ENDLOOP.
      RETURN.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET2'.
      ASSIGN obj_to_log TO <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN obj_to_log TO <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
      ASSIGN obj_to_log TO <hrpad_msg>.
      detailed_msg-msgty = <hrpad_msg>-msgty.
      detailed_msg-msgid = <hrpad_msg>-msgid.
      detailed_msg-msgno = <hrpad_msg>-msgno.
      detailed_msg-msgv1 = <hrpad_msg>-msgv1.
      detailed_msg-msgv2 = <hrpad_msg>-msgv2.
      detailed_msg-msgv3 = <hrpad_msg>-msgv3.
      detailed_msg-msgv4 = <hrpad_msg>-msgv4.
    ELSE.
      free_text_msg = obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = me->handle
          i_msgty      = type
          i_probclass  = importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_exc      = exception_data.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context = formatted_context.
      detailed_msg-params = formatted_params.
      detailed_msg-probclass = importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF auto_save = abap_true.
      APPEND me->handle TO log_handles.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle       = log_handles
          i_2th_connection     = me->sec_connection
          i_2th_connect_commit = me->sec_connect_commit
        IMPORTING
          e_new_lognumbers     = log_numbers.
      IF me->db_number IS INITIAL.
        READ TABLE log_numbers INDEX 1 INTO log_number.
        me->db_number = log_number-lognumber.
      ENDIF.
    ENDIF.

    self = me.
  ENDMETHOD.


  METHOD e.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'E'
      importance    = importance ).
  ENDMETHOD.


  METHOD export_to_table.
    DATA: log_handle      TYPE bal_t_logh,
          message_handles TYPE bal_t_msgh,
          message         TYPE bal_s_msg,
          bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    INSERT handle INTO TABLE log_handle.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handle
      IMPORTING
        e_t_msg_handle = message_handles
      EXCEPTIONS
        msg_not_found  = 0.

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID message-msgid
                TYPE message-msgty
                NUMBER message-msgno
                INTO bapiret2-message
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.

        bapiret2-type          = message-msgty.
        bapiret2-id            = message-msgid.
        bapiret2-number        = message-msgno.
        bapiret2-log_no        = <msg_handle>-log_handle. "last 2 chars missing!!
        bapiret2-log_msg_no    = <msg_handle>-msgnumber.
        bapiret2-message_v1    = message-msgv1.
        bapiret2-message_v2    = message-msgv2.
        bapiret2-message_v3    = message-msgv3.
        bapiret2-message_v4    = message-msgv4.
        bapiret2-system        = sy-sysid.
        APPEND bapiret2 TO bapirettab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fullscreen.

    DATA:
      profile     TYPE bal_s_prof,
      log_handles TYPE bal_t_logh.

    APPEND me->handle TO log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = log_handles.

  ENDMETHOD.


  METHOD get_autosave.

    auto_save = me->auto_save.

  ENDMETHOD.


  METHOD i.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'I'
      importance    = importance ).
  ENDMETHOD.


  METHOD new.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    FIELD-SYMBOLS <context_val> TYPE c.

    CREATE OBJECT log.
    log->header-object = object.
    log->header-subobject = subobject.
    log->header-extnumber = desc.

*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    IF auto_save IS SUPPLIED.
      log->auto_save = auto_save.
    ELSE.
      IF object IS NOT INITIAL AND subobject IS NOT INITIAL.
        log->auto_save = abap_true.
      ENDIF.
    ENDIF.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    IF second_db_conn = abap_true.
      log->sec_connection = abap_true.
      log->sec_connect_commit = abap_true.
    ENDIF.

    IF context IS SUPPLIED AND context IS NOT INITIAL.
      log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      ASSIGN context TO <context_val> CASTING.
      log->header-context-value = <context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = log->header
      IMPORTING
        e_log_handle = log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = log->handle
      IMPORTING
        e_s_log      = log->header.

  ENDMETHOD.


  METHOD open.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    DATA: filter             TYPE bal_s_lfil,
          desc_filter        TYPE bal_s_extn,
          obj_filter         TYPE bal_s_obj,
          subobj_filter      TYPE bal_s_sub,

          found_headers      TYPE balhdr_t,
          most_recent_header TYPE balhdr,
          handles_loaded     TYPE bal_t_logh.

    desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
    desc_filter-sign = subobj_filter-sign = obj_filter-sign = 'I'.

    obj_filter-low = object.
    APPEND obj_filter TO filter-object.
    subobj_filter-low = subobject.
    APPEND subobj_filter TO filter-subobject.
    IF desc IS SUPPLIED.
      desc_filter-low = desc.
      APPEND desc_filter TO filter-extnumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = found_headers
      EXCEPTIONS
        log_not_found  = 1.

    IF sy-subrc = 1.
      IF create_if_does_not_exist = abap_true.
        log = zcl_logger=>new( object    = object
                               subobject = subobject
                               desc      = desc ).
      ENDIF.
      RETURN.
    ENDIF.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    CREATE OBJECT log.
*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    IF auto_save IS NOT SUPPLIED.
      log->auto_save = abap_true.
    ELSE.
      log->auto_save = auto_save.
    ENDIF.

    log->db_number = most_recent_header-lognumber.
    log->handle = most_recent_header-log_handle.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = log->handle
      IMPORTING
        e_s_log      = log->header.

  ENDMETHOD.


  METHOD popup.
* See SBAL_DEMO_04_POPUP for ideas

    DATA:
      profile     TYPE bal_s_prof,
      log_handles TYPE bal_t_logh.

    APPEND me->handle TO log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = log_handles.

  ENDMETHOD.


  METHOD s.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'S'
      importance    = importance ).
  ENDMETHOD.


  METHOD save.
*--------------------------------------------------------------------*
* Method to save the log on demand.  Intended to be called at the    *
*  end of the log processing so that logs can be saved depending     *
*  on other criteria, like the existance of error messages.          *
*  If there are no error messages, it may not be desireable to save  *
*  a log                                                             *
*--------------------------------------------------------------------*


    DATA:
      log_handles TYPE bal_t_logh,
      log_numbers TYPE bal_t_lgnm,
      log_number  TYPE bal_s_lgnm.

    CHECK auto_save = abap_false.

    APPEND me->handle TO log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = log_handles
        i_2th_connection     = me->sec_connection
        i_2th_connect_commit = me->sec_connect_commit
      IMPORTING
        e_new_lognumbers     = log_numbers.
    IF me->db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      me->db_number = log_number-lognumber.
    ENDIF.

  ENDMETHOD.


  METHOD set_autosave.

    me->auto_save = auto_save.

  ENDMETHOD.


  METHOD w.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'W'
      importance    = importance ).
  ENDMETHOD.
ENDCLASS.
