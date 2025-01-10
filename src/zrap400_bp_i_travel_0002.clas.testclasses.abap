*"* use this source file for your ABAP unit test classes

**************************************************************
*  Local class to test read-only behavior implementations    *
**************************************************************
"! @testing BDEF:ZRAP400_I_Travel_0002
CLASS ltcl_readonly_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_travel,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      validate_overall_status FOR TESTING.
ENDCLASS.


CLASS ltcl_readonly_methods IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.

    cds_test_environment = cl_cds_test_environment=>create(
      i_for_entity = 'ZRAP400_I_Travel_0002'
    ).
    sql_test_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #( ( '/DMO/CUSTOMER' ) )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD validate_overall_status.
    DATA travel_mock_data TYPE STANDARD TABLE OF zrap400_trav0002.
    travel_mock_data = VALUE #( ( travel_id = 43 overall_status = 'B' )
                                ( traveL_id = 44 overall_status = 'O' ) ).

    cds_test_environment->insert_test_data( i_data = travel_mock_data ).


    TYPES: BEGIN OF ty_entity_key,
             travelID TYPE /dmo/travel_id,
           END OF ty_entity_key.
    DATA: failed      TYPE RESPONSE FOR FAILED LATE zrap400_i_travel_0002,
          reported    TYPE RESPONSE FOR REPORTED LATE zrap400_i_travel_0002,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    entity_keys = VALUE #( ( travelID = 42 )
                           ( travelID = 43 )
                           ( travelID = 44 ) ).


    class_under_test->validateStatus(
      EXPORTING
        keys = CORRESPONDING #( entity_keys )
      CHANGING
        failed = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'failed-travel-id' act = failed-travel[ 1 ]-TravelID exp = 43 ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).
    DATA(reported_travel) = reported-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( msg = 'reported-travel-id' act = reported_travel-TravelID  exp = 43 ).
    cl_abap_unit_assert=>assert_equals( msg = 'reported-%element'  act = reported_travel-%element-OverallStatus  exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_bound(  msg = 'reported-%msg'      act = reported_travel-%msg ).
  ENDMETHOD.

ENDCLASS.

**************************************************************
*  Local class to test modifying behavior implementations    *
**************************************************************
"! @testing BDEF:ZRAP400_I_Travel_0002
CLASS ltcl_writing_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_travel,
      cds_test_environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      set_status_to_accepted FOR TESTING RAISING cx_static_check,
      set_status_to_rejected FOR TESTING RAISING cx_static_check,
      set_status_to_open     FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_writing_methods IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.

    cds_test_environment = cl_cds_test_environment=>create( i_for_entity = 'ZRAP400_I_TRAVEL_0002' ).
    cds_test_environment->enable_double_redirection( ).
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD set_status_to_accepted.
    DATA travel_mock_data TYPE STANDARD TABLE OF zrap400_trav0002.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = 'O' )
                                ( travel_id = 44 overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    TYPES:
      BEGIN OF ty_entity_key,
        travelID TYPE /dmo/travel_id,
      END OF ty_entity_key.
    DATA: result      TYPE TABLE FOR ACTION RESULT zrap400_i_travel_0002\\travel~acceptTravel,
          mapped      TYPE RESPONSE FOR MAPPED EARLY zrap400_i_travel_0002,
          failed      TYPE RESPONSE FOR FAILED EARLY zrap400_i_travel_0002,
          reported    TYPE RESPONSE FOR REPORTED EARLY zrap400_i_travel_0002,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    entity_keys = VALUE #( ( travelID = 42 )
                           ( travelID = 43 )
                           ( travelID = 44 ) ).
    class_under_test->accepttravel(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_initial( msg = 'mapped'   act = mapped ).
    cl_abap_unit_assert=>assert_initial( msg = 'failed'   act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    DATA exp LIKE result.
    exp = VALUE #( ( TravelID = 42 %param-TravelID = 42 %param-OverallStatus = 'A' )
                   ( TravelID = 43 %param-TravelID = 43 %param-OverallStatus = 'A' )
                   ( TravelID = 44 %param-TravelID = 44 %param-OverallStatus = 'A' ) ).

    DATA act LIKE result.
    act = CORRESPONDING #( result MAPPING TravelID = TravelID
                                    ( %param = %param MAPPING TravelID = TravelID
                                                              OverallStatus = OverallStatus
                                                              EXCEPT *  )
                                    EXCEPT * ).
    SORT act ASCENDING BY TravelID.
    cl_abap_unit_assert=>assert_equals( msg = 'action result' exp = exp act = act ).

    READ ENTITY zrap400_i_travel_0002
      FIELDS ( TravelID OverallStatus )
      WITH CORRESPONDING #( entity_keys )
      RESULT DATA(read_result).
    act = VALUE #( FOR travel IN read_result
                    ( TravelID = travel-TravelID
                      %param-TravelID = travel-TravelID
                      %param-OverallStatus = travel-OverallStatus )
                  ).
    SORT act BY TravelID ASCENDING.
    cl_abap_unit_assert=>assert_equals( msg = 'read result' exp = exp act = act ).
  ENDMETHOD.

  METHOD set_status_to_open.
    DATA travel_mock_data TYPE STANDARD TABLE OF zrap400_trav0002.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = ''  )
                                ( travel_id = 44 overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    TYPES: BEGIN OF ty_entity_key,
             travelID TYPE /dmo/travel_id,
           END OF ty_entity_key.
    DATA: reported    TYPE RESPONSE FOR REPORTED LATE zrap400_i_travel_0002,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.
    entity_keys = VALUE #( ( travelID = 42 )
                           ( travelID = 43 )
                           ( travelID = 44 ) ).
    class_under_test->setstatustoopen(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        reported = reported
    ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    READ ENTITY ZRAP400_I_Travel_0002
      FIELDS ( TravelID OverallStatus )
      WITH CORRESPONDING #( entity_keys )
      RESULT DATA(read_result).

    DATA act LIKE read_result.
    act = CORRESPONDING #( read_result MAPPING TravelID = TravelID
                                               OverallStatus = OverallStatus
                                               EXCEPT * ).
    SORT act ASCENDING BY TravelID.

    DATA exp LIKE read_result.
    exp = VALUE #( ( TravelID = 42 OverallStatus = 'A' )
                   ( TravelID = 43 OverallStatus = 'O' )
                   ( TravelID = 44 OverallStatus = 'X' ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'read result' exp = exp act = act ).
  ENDMETHOD.

  METHOD set_status_to_rejected.
    DATA travel_mock_data TYPE STANDARD TABLE OF zrap400_trav0002.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = ''  )
                                ( travel_id = 44 overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    TYPES: BEGIN OF ty_entity_key,
             travelID TYPE /dmo/travel_id,
           END OF ty_entity_key.

    DATA entity_keys TYPE STANDARD TABLE OF ty_entity_key.
    entity_keys = VALUE #( ( travelID = 42 )
                           ( travelID = 43 )
                           ( travelID = 44 ) ).

    DATA: reported TYPE RESPONSE FOR REPORTED EARLY zrap400_i_travel_0002,
          result   TYPE TABLE FOR ACTION RESULT zrap400_i_travel_0002\\travel~rejectTravel,
          mapped   TYPE RESPONSE FOR MAPPED EARLY zrap400_i_travel_0002,
          failed   TYPE RESPONSE FOR FAILED EARLY zrap400_i_travel_0002.
    class_under_test->rejecttravel(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        result   = result
        mapped   = mapped
        reported = reported
        failed   = failed
    ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'mapped' act = mapped ).

    READ ENTITY ZRAP400_I_Travel_0002
      FIELDS ( TravelID OverallStatus )
      WITH CORRESPONDING #( entity_keys )
      RESULT DATA(read_result).

    DATA act LIKE read_result.
    act = CORRESPONDING #( read_result MAPPING TravelID = TravelID
                                               OverallStatus = OverallStatus
                                               EXCEPT * ).
    SORT act ASCENDING BY TravelID.

    DATA exp LIKE read_result.
    exp = VALUE #( ( TravelID = 42 OverallStatus = 'X' )
                   ( TravelID = 43 OverallStatus = 'X' )
                   ( TravelID = 44 OverallStatus = 'X' ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'read result' exp = exp act = act ).
  ENDMETHOD.

ENDCLASS.
