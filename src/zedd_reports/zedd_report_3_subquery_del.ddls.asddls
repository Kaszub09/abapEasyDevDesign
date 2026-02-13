@AbapCatalog.sqlViewName: 'ZEDD_REPORT_3SD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 3 example subquery deliveries'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_report_3_subquery_del
  as select from lips
{
  cast( vgbel as vbeln) as sales_order,
  cast( vgpos as posnr) as sales_order_pos,
  vbeln,
  posnr,
  lips.lfimg,
  lips.vrkme
}
