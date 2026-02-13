@AbapCatalog.sqlViewName: 'ZEDD_REPORT_3ADB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Additional data base'
@Metadata.ignorePropagatedAnnotations: true
define view ZEDD_REPORT_3_ADD_DATA_BASE
  as select from  lips
  left outer join marm on  marm.matnr = lips.matnr
                       and marm.meinh = 'CAR'
{
  cast( vgbel as vbeln)                            as sales_order,
  cast( vgpos as posnr)                            as sales_order_pos,
  vrkme,
  lips.lfimg,
  division( lips.lgmng * marm.umren, marm.umrez,3) as amount_car
}
