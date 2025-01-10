@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View forTravel'
@ObjectModel.semanticKey: [ 'TravelID' ]
@Search.searchable: true
define root view entity ZRAP400_C_Travel_0002
  provider contract TRANSACTIONAL_QUERY
  as projection on ZRAP400_I_Travel_0002 as Travel
{
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key TravelID,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: '/DMO/I_Agency', 
      element: 'AgencyID'
    }
  } ]
  AgencyID,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: '/DMO/I_Customer', 
      element: 'CustomerID'
    }
  } ]
  CustomerID,
  BeginDate,
  EndDate,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  BookingFee,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  TotalPrice,
  @Consumption.valueHelpDefinition: [ {
    entity: {
      name: 'I_Currency', 
      element: 'Currency'
    }
  } ]
  CurrencyCode,
  Description,
  OverallStatus,
  LastChangedAt,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  _Booking : redirected to composition child ZRAP400_C_Book_0002,
  _Agency,
  _Currency,
  _Customer
  
}
