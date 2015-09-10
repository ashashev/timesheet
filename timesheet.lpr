program timesheet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils, Dialogs, Forms, datetimectrls, timesheet_main, dm, auxiliary,
  editing, weekreportu, config, popupcalendar;

{$R *.res}

begin
  Application.Initialize;
  try
    Application.CreateForm(TdmMain, dmMain);
    Application.CreateForm(TmainForm, mainForm);
    Application.CreateForm(TeditingForm, editingForm);
    Application.CreateForm(TweekReport, weekReport);
    Application.CreateForm(TPopupCalendar, PopupCalendarForm);
    Application.Run;
  except
    On E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end.

