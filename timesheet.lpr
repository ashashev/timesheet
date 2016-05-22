program timesheet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils, Dialogs, Forms, datetimectrls, timesheet_main, dm, auxiliary,
  editing, weekreportu, config, popupcalendar, versioninfo;

{$R *.res}

begin
  Application.Title:='Timesheet';
  Application.Initialize;
  try
    if Application.HasOption('version') then
    begin
      ShowMessage('Timesheet' + Version.Get);
    end
    else
    begin
      Application.CreateForm(TdmMain, dmMain);
      Application.CreateForm(TmainForm, mainForm);
      Application.CreateForm(TeditingForm, editingForm);
      Application.CreateForm(TweekReport, weekReport);
      Application.CreateForm(TPopupCalendar, PopupCalendarForm);
      Application.Run;
    end;
  except
    On E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end.

