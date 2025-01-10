@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'CDS View forTravel'
@ObjectModel.sapObjectNodeType.name: 'ZRAP400_Travel_0002'
define root view entity ZRAP400_I_Travel_0002
  as select from ZRAP400_TRAV0002 as Travel
  association [0..1] to /DMO/I_Agency as _Agency on $projection.AgencyID = _Agency.AgencyID
  association [0..1] to I_Currency as _Currency on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to /DMO/I_Customer as _Customer on $projection.CustomerID = _Customer.CustomerID
  composition [0..*] of ZRAP400_I_Book_0002 as _Booking
{
  key TRAVEL_ID as TravelID,
  AGENCY_ID as AgencyID,
  CUSTOMER_ID as CustomerID,
  BEGIN_DATE as BeginDate,
  END_DATE as EndDate,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  BOOKING_FEE as BookingFee,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  TOTAL_PRICE as TotalPrice,
  CURRENCY_CODE as CurrencyCode,
  DESCRIPTION as Description,
  OVERALL_STATUS as OverallStatus,
  LAST_CHANGED_AT as LastChangedAt,
  CREATED_BY as CreatedBy,
  CREATED_AT as CreatedAt,
  LAST_CHANGED_BY as LastChangedBy,
  _Booking,
  _Agency,
  _Currency,
  _Customer
  
}
