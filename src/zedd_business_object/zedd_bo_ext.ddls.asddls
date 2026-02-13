@AbapCatalog.sqlViewName: 'zedd_bo_exts'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Extneded BO view'
@Metadata.ignorePropagatedAnnotations: true
define view zedd_bo_ext
  as select from  zedd_bo

  left outer join makt on  makt.spras = $session.system_language
                       and makt.matnr = zedd_bo.matnr
{
  bo_id,
  zedd_bo.matnr,
  is_active,
  some_comment,
  created_at_date,
  last_changed_at_date,
  maktx
}
