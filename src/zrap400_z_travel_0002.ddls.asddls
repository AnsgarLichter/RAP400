@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Projection View forTravel'
define root view entity ZRAP400_Z_Travel_0002
  provider contract TRANSACTIONAL_INTERFACE
  as projection on ZRAP400_I_Travel_0002 as Travel
{
  key TravelID,
  AgencyID,
  CustomerID,
  BeginDate,
  EndDate,
  BookingFee,
  TotalPrice,
  CurrencyCode,
  Description,
  OverallStatus,
  LastChangedAt,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  _Booking : redirected to composition child ZRAP400_Z_Book_0002,
  _Agency,
  _Currency,
  _Customer
  
}
