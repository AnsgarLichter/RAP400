@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View forBooking'
@ObjectModel.semanticKey: [ 'BookingID' ]
@Search.searchable: true
define view entity ZRAP400_C_Book_0002
  as projection on ZRAP400_I_Book_0002 as Booking
{
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key TravelID,
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key BookingID,
  BookingDate,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: '/DMO/I_Customer', 
      element: 'CustomerID'
    }
  } ]
  CustomerID,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: '/DMO/I_Carrier', 
      element: 'AirlineID'
    }
  } ]
  CarrierID,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: '/DMO/I_Flight', 
      element: 'ConnectionID'
    }, 
    additionalBinding: [ {
      localElement: 'FlightDate', 
      element: 'FlightDate'
    }, {
      localElement: 'CarrierID', 
      element: 'AirlineID'
    }, {
      localElement: 'FlightPrice', 
      element: 'Price'
    }, {
      localElement: 'CurrencyCode', 
      element: 'CurrencyCode'
    } ]
  } ]
  ConnectionID,
  FlightDate,
  BookingStatus,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  FlightPrice,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: 'I_Currency', 
      element: 'Currency'
    }
  } ]
  CurrencyCode,
  LastChangedAt,
  _Travel : redirected to parent ZRAP400_C_Travel_0002,
  _Connection,
  _Flight,
  _Carrier,
  _Currency,
  _Customer
  
}
