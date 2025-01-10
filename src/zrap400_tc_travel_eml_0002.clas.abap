"! @testing BDEF:ZRAP400_I_Travel_0002
CLASS zrap400_tc_travel_eml_0002 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment,
      begin_date           TYPE /dmo/begin_date,
      end_date             TYPE /dmo/end_date,
      agency_mock_data     TYPE STANDARD TABLE OF /dmo/agency,
      customer_mock_data   TYPE STANDARD TABLE OF /dmo/customer,
      carrier_mock_data    TYPE STANDARD TABLE OF /dmo/carrier,
      flight_mock_data     TYPE STANDARD TABLE OF /dmo/flight.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      deep_create_with_action FOR TESTING RAISING cx_static_check.
ENDCLASS.



CLASS zrap400_tc_travel_eml_0002 IMPLEMENTATION.
  METHOD class_setup.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
      i_for_entities = VALUE #( ( i_for_entity = 'ZRAP400_I_Travel_0002' )
                                ( i_for_entity = 'ZRAP400_I_Book_0002' ) )
    ).

    sql_test_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #( ( '/DMO/AGENCY' )
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/CARRIER' )
                                   ( '/DMO/FLIGHT' ) )
    ).

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date = cl_abap_context_info=>get_system_date( ) + 30.

    agency_mock_data = VALUE #( ( agency_id = '987654' name = 'Agency 987654' ) ).
    customer_mock_data = VALUE #( ( customer_Id = '987653' last_name = 'Customer 987653' ) ).
    carrier_mock_data = VALUE #( ( carrier_id = '123' name = 'Carrier 123' ) ).
    flight_mock_data = VALUE #( ( carrier_id = '123' connection_id = '9876' flight_date = begin_date price = '2000' currency_code = 'EUR' ) ).
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD deep_create_with_action.
    MODIFY ENTITIES OF ZRAP400_I_Travel_0002
      ENTITY Travel
        CREATE SET FIELDS WITH
          VALUE #( ( %cid = 'ROOT1'
                     AgencyID = agency_mock_data[ 1 ]-agency_id
                     CustomerID = customer_mock_data[ 1 ]-customer_id
                     BeginDate = begin_date
                     EndDate = end_date
                     Description = 'TestTravel 1'
                     BookingFee = '10.5'
                     CurrencyCode = 'EUR'
                     OverallStatus = 'O'
                 ) )
        CREATE BY \_Booking SET FIELDS WITH
          VALUE #( ( %cid_ref = 'ROOT1'
                     %target = VALUE #( ( %cid = 'BOOKING1'
                                          BookingDate = begin_date
                                          CustomerID = customer_mock_data[ 1 ]-customer_id
                                          CarrierID = flight_mock_data[ 1 ]-carrier_id
                                          ConnectionID = flight_mock_data[ 1 ]-connection_id
                                          FlightDate = flight_mock_data[ 1 ]-flight_date
                                          FlightPrice = flight_mock_data[ 1 ]-price
                                          CurrencyCode = flight_mock_data[ 1 ]-currency_code
                                          BookingStatus = 'N'
                                      ) )
                 ) )
      ENTITY Travel
        EXECUTE acceptTravel
          FROM VALUE #( ( %cid_ref = 'ROOT1' ) )

      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'mapped-travel' act = mapped-travel ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'mapped-booking' act = mapped-booking ).

    COMMIT ENTITIES RESPONSES
      FAILED DATA(commit_failed)
      REPORTED DATA(commit_reported).

    cl_abap_unit_assert=>assert_initial( msg = 'commit failed' act = commit_failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'commit reported' act = commit_reported ).

    SELECT *
      FROM zrap400_i_travel_0002
      INTO TABLE @DATA(travels).

    cl_abap_unit_assert=>assert_not_initial( msg = 'travel from db' act = travels ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'travel ID' act = travels[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'overall status' exp = 'A' act = travels[ 1 ]-OverallStatus ).
    cl_abap_unit_assert=>assert_equals( msg = 'total price incl. booking fee' exp = '2010.50' act = travels[ 1 ]-TotalPrice ).

    SELECT *
      FROM ZRAP400_I_Book_0002
      INTO TABLE @DATA(bookings).

    cl_abap_unit_assert=>assert_not_initial( msg = 'booking from db' act = bookings ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'booking ID' act = bookings[ 1 ]-BookingID ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    sql_test_environment->insert_test_data( agency_mock_data ).
    sql_test_environment->insert_test_data( customer_Mock_data ).
    sql_test_environment->insert_test_data( carrier_mock_data ).
    sql_test_environment->insert_test_data( flight_mock_data ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

ENDCLASS.
