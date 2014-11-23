program timesheet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, timesheet_main, dm, auxiliary, editing, weekreportu;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TeditingForm, editingForm);
  Application.CreateForm(TweekReport, weekReport);
  Application.Run;
end.

