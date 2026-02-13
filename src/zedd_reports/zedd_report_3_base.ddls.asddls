@AbapCatalog.sqlViewName: 'ZEDD_REPORT_3B'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 3 example base'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_report_3_base
  as select from  vbap
  left outer join makt on  makt.spras = $session.system_language
                       and makt.matnr = vbap.matnr
{
  key vbeln as sales_order,
  key posnr as sales_order_pos,
      vbap.matnr,
      maktx,
      kwmeng,
      vrkme
}
