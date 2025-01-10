CLASS ltc_helper DEFINITION FOR TESTING CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      client_proxy         TYPE REF TO /iwbep/if_cp_client_proxy,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment,
      agency_mock_data     TYPE STANDARD TABLE OF /dmo/agency,
      customer_mock_data   TYPE STANDARD TABLE OF /dmo/customer,
      carrier_mock_data    TYPE STANDARD TABLE OF /dmo/carrier,
      flight_mock_data     TYPE STANDARD TABLE OF /dmo/flight,
      travel_mock_data     TYPE STANDARD TABLE OF ZRAP400_Trav0002,
      booking_mock_data    TYPE STANDARD TABLE OF ZRAP400_Book0002,
      begin_date           TYPE /dmo/begin_date,
      end_date             TYPE /dmo/end_date.

    CLASS-METHODS:
      helper_class_setup RAISING cx_static_check,
      helper_class_teardown,
      helper_setup,
      helper_teardown.
ENDCLASS.

CLASS ltc_helper IMPLEMENTATION.

  METHOD helper_class_setup.
    client_proxy = cl_web_odata_client_factory=>create_v2_local_proxy(
      VALUE #( service_id = 'ZRAP400_UI_TRAVEL_0002_O2'
               service_version = '0001' )
    ).

    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
      i_for_entities = VALUE #( ( i_for_entity = 'ZRAP400_C_TRAVEL_0002' i_select_base_dependencies = abap_true )
                                ( i_for_entity = 'ZRAP400_C_BOOK_0002' i_select_base_dependencies = abap_true ) )
    ).

    sql_test_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #( ( '/DMO/AGENCY' )
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/CARRIER' )
                                   ( '/DMO/FLIGHT' ) )
    ).

    agency_mock_data   = VALUE #( ( agency_id = '987654' name = 'Agency 987654' ) ).
    customer_mock_data = VALUE #( ( customer_id = '987653' last_name = 'customer 987653' ) ).
    carrier_mock_data  = VALUE #( ( carrier_id = '123' name = 'carrier 123' ) ).
    flight_mock_data   = VALUE #( ( carrier_id = '123' connection_id = '9876' flight_date = begin_date
                                    price = '2000' currency_code = 'EUR' ) ).
    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date = cl_abap_context_info=>get_system_date( ) + 30.

    travel_mock_data = VALUE #( ( travel_id = '101'
                                  agency_id = agency_mock_data[ 1 ]-agency_id
                                  customer_id = customer_mock_data[ 1 ]-customer_id
                                  begin_date = begin_date
                                  end_date = end_date
                                  booking_fee = '20'
                                  currency_code = 'EUR'
                                  description = 'Mock Travel'
                                  overall_status = 'O' ) ).
    booking_mock_data = VALUE #( ( travel_id = '101'
                                   booking_id = '2001'
                                   customer_id = customer_mock_data[ 1 ]-customer_id
                                   carrier_id = flight_mock_data[ 1 ]-carrier_id
                                   connection_id = flight_mock_data[ 1 ]-connection_id
                                   flight_date = flight_mock_data[ 1 ]-flight_date
                                   flight_price = flight_mock_data[ 1 ]-price
                                   currency_code = flight_mock_data[ 1 ]-currency_code
                                   booking_status = 'N' ) ).
  ENDMETHOD.

  METHOD helper_class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD helper_setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    cds_test_environment->insert_test_data( travel_mock_data ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    sql_test_environment->insert_test_data( agency_mock_data ).
    sql_test_environment->insert_test_data( customer_mock_data ).
    sql_test_environment->insert_test_data( carrier_mock_data ).
    sql_test_environment->insert_test_data( flight_mock_data ).
  ENDMETHOD.

  METHOD helper_teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZRAP400_UI_TRAVEL_0002_O2
CLASS ltc_CREATE DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      setup,
      teardown,
      create FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_CREATE IMPLEMENTATION.


  METHOD create.
    DATA(business_data) = VALUE ZRAP400_C_Travel_0002(
      agencyid = ltc_helper=>agency_mock_data[ 1 ]-agency_id
      customerid = ltc_helper=>customer_mock_data[ 1 ]-customer_id
      begindate = ltc_helper=>begin_date
      enddate = ltc_helper=>end_date
      bookingfee = '10.50'
      CurrencyCode = 'EUR'
      Description = 'TestTravel 1'
    ).

    DATA(request) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_create( ).
    request->set_business_data( business_data ).

    DATA(response) = request->execute( ).
    cl_abap_unit_assert=>assert_not_initial( response ).

    DATA response_data TYPE ZRAP400_C_Travel_0002.
    response->get_business_data( IMPORTING es_business_data = response_data ).

    cl_abap_unit_assert=>assert_equals( msg = 'description from response' act = response_data-Description exp = business_data-Description ).
    cl_abap_unit_assert=>assert_equals( msg = 'overall status from response' act = response_data-OverallStatus exp = 'O' ).

    READ ENTITIES OF ZRAP400_C_Travel_0002
      ENTITY Travel
        FIELDS ( Description OverallStatus )
          WITH VALUE #( ( TravelID = response_data-TravelID ) )
      RESULT DATA(travels_read)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_initial( msg = 'failed travel from read' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported travel from read' act = reported ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'travels from read' act = travels_read ).
    cl_abap_unit_assert=>assert_equals( msg = 'description from read' act = travels_read[ 1 ]-Description exp = business_data-Description ).
    cl_abap_unit_assert=>assert_equals( msg = 'overall status from read' act = travels_read[ 1 ]-OverallStatus exp = 'O' ).


    SELECT *
      FROM zrap400_trav0002
      WHERE travel_id = @response_data-TravelID
      INTO TABLE @DATA(travels_db).

    cl_abap_unit_assert=>assert_not_initial( msg = 'travels from db' act = travels_db ).
    cl_abap_unit_assert=>assert_equals( msg = 'description from db' act = travels_db[ 1 ]-description exp = business_data-Description ).
    cl_abap_unit_assert=>assert_equals( msg = 'overall status from db' act = travels_db[ 1 ]-overall_status exp = 'O' ).
  ENDMETHOD.

  METHOD setup.
    ltc_helper=>helper_setup( ).
  ENDMETHOD.

  METHOD class_setup.
    ltc_helper=>helper_class_setup( ).
  ENDMETHOD.

  METHOD class_teardown.
    ltc_helper=>helper_class_teardown( ).
  ENDMETHOD.

  METHOD teardown.
    ltc_helper=>helper_teardown( ).
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZRAP400_UI_TRAVEL_0002_O2
CLASS ltc_DEEP_CREATE DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      setup,
      teardown,
      deep_create FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_DEEP_CREATE IMPLEMENTATION.


  METHOD setup.
    ltc_helper=>helper_setup( ).
  ENDMETHOD.

  METHOD class_setup.
    ltc_helper=>helper_class_setup( ).
  ENDMETHOD.

  METHOD class_teardown.
    ltc_helper=>helper_class_teardown( ).
  ENDMETHOD.

  METHOD teardown.
    ltc_helper=>helper_teardown( ).
  ENDMETHOD.

  METHOD deep_create.
    TYPES BEGIN OF ty_travel_and_booking.
    INCLUDE TYPE ZRAP400_C_Travel_0002.
    TYPES to_booking TYPE STANDARD TABLE OF zrap400_c_book_0002 WITH DEFAULT KEY.
    TYPES END OF ty_travel_and_booking.

    DATA(business_data) = VALUE ty_travel_and_booking(
                                        agencyid        = ltc_helper=>agency_mock_data[ 1 ]-agency_id
                                        customerid      = ltc_helper=>customer_mock_data[ 1 ]-customer_id
                                        begindate       = ltc_helper=>begin_date
                                        enddate         = ltc_helper=>end_date
                                        bookingfee      = '21'
                                        currencycode    = 'USD'
                                        description     = 'TestTravel 2'
                                        to_booking = VALUE #( ( CustomerID    = ltc_helper=>customer_mock_data[ 1 ]-customer_id
                                                                CarrierID     = ltc_helper=>flight_mock_data[ 1 ]-carrier_id
                                                                ConnectionID  = ltc_helper=>flight_mock_data[ 1 ]-connection_id
                                                                FlightDate    = ltc_helper=>flight_mock_data[ 1 ]-flight_date
                                                                FlightPrice   = ltc_helper=>flight_mock_data[ 1 ]-price
                                                                CurrencyCode  = ltc_helper=>flight_mock_data[ 1 ]-currency_code
                                                                BookingStatus = 'N'  ) ) ).
    TRY.
        DATA(request) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_create( ).
        DATA(data_description_node) = request->create_data_descripton_node( ).
        data_description_node->add_child( 'TO_BOOKING' ). "Prefix 'TO_' required

        request->set_deep_business_data(
          is_business_data            = business_data
          io_data_description         = data_description_node
        ).

        DATA(response) = request->execute( ).

        DATA response_data TYPE ty_travel_and_booking.
        response->get_business_data( IMPORTING es_business_data = response_data ).

        READ ENTITIES OF ZRAP400_C_Travel_0002
          ENTITY Travel
            FIELDS ( Description ) WITH VALUE #( ( TravelID = response_data-TravelID ) )
            RESULT DATA(travel_read)
          BY \_Booking
            FIELDS ( CarrierID ) WITH VALUE #( ( TravelID = response_data-TravelID ) )
            RESULT DATA(bookings_read)
          FAILED DATA(failed)
          REPORTED DATA(reported).

        cl_abap_unit_assert=>assert_equals( msg = 'description from read' exp = 'TestTravel 2' act = travel_read[ 1 ]-Description ).
        cl_abap_unit_assert=>assert_equals( msg = 'carrier-id from read' exp = ltc_helper=>flight_mock_data[ 1 ]-carrier_id act = bookings_read[ 1 ]-CarrierID ).

        SELECT *
         FROM zrap400_trav0002
         WHERE travel_id = @response_data-TravelID
         INTO TABLE @DATA(travels_db).

        cl_abap_unit_assert=>assert_not_initial( msg = 'travel from db' act = travels_db ).
        cl_abap_unit_assert=>assert_not_initial( msg = 'travel-id from db' act = travels_db[ 1 ]-travel_id ).
        cl_abap_unit_assert=>assert_equals( msg = 'description from db' exp = 'TestTravel 2' act = travels_db[ 1 ]-description ).

        SELECT *
         FROM zrap400_book0002
         WHERE travel_id = @response_data-TravelID
         INTO TABLE @DATA(bookings_db).

        cl_abap_unit_assert=>assert_not_initial( msg = 'booking from db' act = bookings_db ).
        cl_abap_unit_assert=>assert_not_initial( msg = 'booking-id from db' act = bookings_db[ 1 ]-booking_id ).
        cl_abap_unit_assert=>assert_equals( msg = 'carrier-id from db' exp = ltc_helper=>flight_mock_data[ 1 ]-carrier_id act = bookings_db[ 1 ]-carrier_id ).
      CATCH /iwbep/cx_cp_remote /iwbep/cx_gateway INTO DATA(exception).
        cl_abap_unit_assert=>fail( msg = exception->get_longtext( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZRAP400_UI_TRAVEL_0002_O2
CLASS ltc_READ_LIST DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      setup,
      teardown,
      read_list FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_READ_LIST IMPLEMENTATION.


  METHOD class_setup.
    ltc_helper=>helper_class_setup( ).
  ENDMETHOD.

  METHOD class_teardown.
    ltc_helper=>helper_class_teardown( ).
  ENDMETHOD.

  METHOD setup.
    ltc_helper=>helper_setup( ).
  ENDMETHOD.

  METHOD teardown.
    ltc_helper=>helper_teardown( ).
  ENDMETHOD.

  METHOD read_list.
    DATA range_agencyid TYPE RANGE OF /dmo/agency_id.
    range_agencyid = VALUE #( ( low = ltc_helper=>agency_mock_data[ 1 ]-agency_id
                                option = 'EQ'
                                sign = 'I' ) ).
    DATA range_customerid TYPE RANGE OF /dmo/customer_id.
    range_customerid = VALUE #( ( low = ltc_helper=>customer_mock_data[ 1 ]-customer_id
                                  option = 'EQ'
                                  sign = 'I' ) ).

    DATA(request) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_read( ).
    DATA(filter_factory) = request->create_filter_factory( ).
    DATA(filter_node_agency) = filter_factory->create_by_range( iv_property_path = 'AGENCYID' it_range = range_agencyid ).
    DATA(filter_node_customer) = filter_factory->create_by_range( iv_property_path = 'CUSTOMERID' it_range = range_customerid ).
    DATA(filter_node_root) = filter_node_agency->and( filter_node_customer ).
    request->set_filter( filter_node_root ).
    request->set_top( 50 )->set_skip( 0 ).

    DATA(response) = request->execute( ).
    DATA business_data TYPE STANDARD TABLE OF ZRAP400_C_Travel_0002.
    response->get_business_data( IMPORTING et_business_data = business_data ).

    SELECT *
      FROM zrap400_trav0002
      WHERE agency_id IN @range_agencyid
            AND customer_id IN @range_customerid
      INTO TABLE @DATA(travels).

    cl_abap_unit_assert=>assert_equals( msg = 'query result equal count' exp = lines( travels ) act = lines( business_data ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'query result equal travelid' exp = travels[ 1 ]-travel_id act = business_data[ 1 ]-TravelID ).

  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZRAP400_UI_TRAVEL_0002_O2
CLASS ltc_UPDATE DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      setup,
      teardown,
      update FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_UPDATE IMPLEMENTATION.


  METHOD update.
    TYPES: BEGIN OF ty_entity_key,
             travelID TYPE /dmo/travel_id,
           END OF ty_entity_key.

    DATA business_data TYPE ZRAP400_C_Travel_0002.
    business_data-Description = 'Travel updated'.
    business_data-BookingFee = '31.40'.

    DATA(entity_key) = VALUE ty_entity_key( travelid = ltc_helper=>travel_mock_data[ 1 ]-travel_id ).
    DATA(resource) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( entity_key ).
    DATA(response_read) = resource->create_request_for_read( )->execute( ).

    DATA(request) = resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-patch ).
    request->set_business_data(
      is_business_data = business_data
      it_provided_property = VALUE #( ( `DESCRIPTION` ) ( `BOOKINGFEE` ) )
    ).
    request->set_if_match( response_read->get_etag( ) ).
    DATA(response) = request->execute( ).
    response->get_business_data( IMPORTING es_business_data = business_data ).

    READ ENTITIES OF ZRAP400_C_Travel_0002
      ENTITY Travel
      FIELDS ( BookingFee Description )
      WITH VALUE #( ( TravelID = ltc_helper=>travel_mock_data[ 1 ]-travel_id ) )
    RESULT DATA(travels)
    FAILED DATA(failed)
    REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).
    cl_abap_unit_assert=>assert_equals( msg = 'Travel description updated' act = travels[ 1 ]-Description exp = 'Travel updated' ).
    cl_abap_unit_assert=>assert_equals( msg = 'Booking fees updated' act = travels[ 1 ]-BookingFee exp = '31.40' ).
  ENDMETHOD.

  METHOD class_setup.
    ltc_helper=>helper_class_setup( ).
  ENDMETHOD.

  METHOD class_teardown.
    ltc_Helper=>helper_class_teardown( ).
  ENDMETHOD.

  METHOD setup.
    ltc_helper=>helper_setup( ).
  ENDMETHOD.

  METHOD teardown.
    ltc_helper=>helper_teardown( ).
  ENDMETHOD.

*  METHOD reject_travel.
*    TYPES: BEGIN OF ty_entity_key,
*             travelid TYPE /DMO/travel_id,
*           END OF ty_entity_key.
*    DATA(entity_key) = VALUE ty_entity_key( travelid = ltc_helper=>travel_mock_data[ 1 ]-travel_id ).
*
*    DATA(resource) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( entity_key ).
*    DATA(action_resource) = resource->bind_action( 'REJECT_TRAVEL' ).
*
*    DATA(response_read) = resource->create_request_for_read( )->execute( ).
*    DATA(action_request) = action_resource->create_request( )->set_if_match( response_read->get_etag( ) ).
*    action_request->execute( ).
*
*
*    READ ENTITIES OF ZRAP400_C_Travel_0002
*      ENTITY Travel
*      FIELDS ( OverallStatus )
*      WITH VALUE #( ( TravelID = entity_key-travelid ) )
*    RESULT DATA(travels)
*    FAILED DATA(failed)
*    REPORTED DATA(reported).
*
*    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
*    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).
*    cl_abap_unit_assert=>assert_equals( msg = 'Travel status rejected' act = travels[ 1 ]-OverallStatus exp = 'X' ).
*  ENDMETHOD.

ENDCLASS.

"!@testing SRVB:ZRAP400_UI_TRAVEL_0002_O2
CLASS ltc_DELETE_ENTITY DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      setup,
      teardown,
      delete FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_DELETE_ENTITY IMPLEMENTATION.

  METHOD class_setup.
    ltc_helper=>helper_class_setup( ).
  ENDMETHOD.

  METHOD class_teardown.
    ltc_helper=>helper_class_teardown( ).
  ENDMETHOD.

  METHOD setup.
    ltc_helper=>helper_setup( ).
  ENDMETHOD.

  METHOD teardown.
    ltc_helper=>helper_teardown( ).
  ENDMETHOD.

  METHOD delete.
    TYPES: BEGIN OF ty_entity_key,
             travelid TYPE /dmo/travel_id,
           END OF ty_entity_key.
    DATA(entity_key) = VALUE ty_entity_key( travelid = ltc_helper=>travel_mock_data[ 1 ]-travel_id ).
    DATA(resource) = ltc_helper=>client_proxy->create_resource_for_entity_set( 'Travel' )->navigate_with_key( entity_key ).
    DATA(response_read) = resource->create_request_for_read( )->execute( ).

    "Check if test entity exists in the environment
    SELECT SINGLE *
      FROM zrap400_trav0002
      WHERE travel_id EQ @entity_key-travelid
      INTO @DATA(travel).

    cl_abap_unit_assert=>assert_equals( msg = 'initial check' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'description check' exp = ltc_helper=>travel_mock_data[ 1 ]-description act = travel-description ).

    DATA(request) = resource->create_request_for_delete( ).
    request->set_if_match( response_read->get_etag( ) ).
    request->execute( ).

    SELECT SINGLE *
      FROM zrap400_trav0002
      WHERE travel_id EQ @entity_key-travelid
      INTO @travel.

    cl_abap_unit_assert=>assert_equals( msg = 'db check after deletion' exp = 4 act = sy-subrc ).

    READ ENTITIES OF ZRAP400_C_Travel_0002
      ENTITY Travel
        FIELDS ( description )
        WITH VALUE #( ( TravelID = entity_key-travelid ) )
      RESULT DATA(travels_read)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_not_initial( msg = 'read check after deletion' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).
    cl_abap_unit_assert=>assert_initial( msg = 'read travels' act = travels_read ).
  ENDMETHOD.

ENDCLASS.
