program timesheet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, timesheet_main, dm, auxiliary, editing;

{$R *.res}

begin
  Application.Initialize;
    Application.CreateForm(Ttimesheet_main_form, timesheet_main_form);
    Application.CreateForm(Tdm_main, dm_main);
  Application.CreateForm(Tediting_form, editing_form);
  Application.Run;
end.

