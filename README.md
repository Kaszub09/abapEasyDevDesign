# abapEasyDevDesign
 Easy ABAP development - design examples

Examples may require https://github.com/Kaszub09/abapEasyDev and https://github.com/Kaszub09/abapEasyALV.

Various ABAP design examples:
- [How to write reports](src/zedd_reports/zedd_report_1_simple.prog.abap) - few generic ways/templates to approach the problem
- [Business object](src/zedd_business_object/zif_edd_bo.intf.abap) - how to design business object - so that model is not tied to view, and provides ease of extensibility and maintainability.
- [Program](src/zedd_program/zedd_program_mvc_bo.prog.abap) - how to design program with multiple screens and navigation between them, with focus on avoiding global data and code scattered across multiple modules, called who knows when.
- [Buffer](src/zedd_buffer/zcl_edd_buffer_example.clas.abap) - how to write simple buffer to avoid unnecessary trips to database.
- [Bypass single use](/src/zedd_bypass_single_use/zcl_edd_bypass_single_use_base.clas.abap]) - how to design around single use stuff, e.g. BADI, when multiple implemntations are needed
- [Interface with callback](src/zedd_interfaces/zif_edd_1.intf.abap) - extend interface so implementer can interact with surrounding program
