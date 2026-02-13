@AbapCatalog.sqlViewName: 'ZEDD_REPORT_1S'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 1 example'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_report_1
  as select from  mara
  left outer join makt on  makt.spras = $session.system_language
                       and makt.matnr = mara.matnr
{
  mara.matnr,
  ersda,
  ernam,
  maktx
}
