@AbapCatalog.sqlViewName: 'zedd_report_3SDO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 3 example subquery all docs'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_report_3_subquery_docs
  as select from vbfa
{
  cast(  vbelv as vbeln) as sales_order,
  cast( posnv as posnr)  as sales_order_pos,
  vbtyp_n,
  vbeln,
  posnn
}
