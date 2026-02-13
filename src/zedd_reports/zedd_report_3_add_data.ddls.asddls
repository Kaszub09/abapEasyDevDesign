@AbapCatalog.sqlViewName: 'ZEDD_REPORT_3AD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 5 example additional data'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_report_3_add_data
  as select from ZEDD_REPORT_3_ADD_DATA_BASE
{
  sales_order,
  sales_order_pos,
  vrkme,
  sum(lfimg)      as lfimg,
  sum(amount_car) as amount_car
}

group by
  sales_order,
  sales_order_pos,
  vrkme
