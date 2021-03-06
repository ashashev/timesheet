unit timesheet_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, FileUtil, DateTimePicker, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, DBGrids, ComCtrls, ActnList, dm, db, Grids,
  Menus, Buttons;

type

  { TmainForm }

  TmainForm = class(TForm)
    curDate: TDateTimePicker;
    dsTimesheet: TDataSource;
    grid: TDBGrid;
    mainMenu: TMainMenu;
    meEdit: TMenuItem;
    miWeekReport: TMenuItem;
    miReports: TMenuItem;
    miDelete: TMenuItem;
    miCopySelectedRecord: TMenuItem;
    miEdit: TMenuItem;
    miNew: TMenuItem;
    miQuit: TMenuItem;
    miFile: TMenuItem;
    panel1: TPanel;
    status: TStatusBar;
    toolButton1: TToolButton;
    toolButton2: TToolButton;
    toolButton3: TToolButton;
    toolButton4: TToolButton;
    toolButton5: TToolButton;
    toolBar: TToolBar;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure curDateChange(Sender: TObject);
    procedure curDateEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridCellClick({%H-}Column: TColumn);
    procedure gridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure gridKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure miQuitClick(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
  private
    { private declarations }
    notCallUpdateDate: Boolean;
    procedure updateDate;
  public
    { public declarations }
    procedure updateTotalElapsed;
    procedure updateSelectElapsed;
  end;

var
  mainForm: TmainForm;

implementation

uses auxiliary, dateutils, popupcalendar, versioninfo;

const
  panelWeek = 1;
  panelTotalElapsed = 3;
  panelSelectElapsed = 5;

{$R *.lfm}

function CalcColorForCurrentRow(selected: Boolean): TColor;
var
  data: TDataSet;
  isBreak: Boolean;
  timeIsCorrect: Boolean;
begin
  Result := clNone;
  data := mainForm.grid.DataSource.DataSet;
  if (not data.FieldByName('date').IsNull)
     and (StrToDate(data.FieldByName('date').AsString,
                    dm.dbDateFormatStr,
                    dm.dbDateSeparator) < mainForm.curDate.Date) then
  begin
    Result := clSilver;
  end
  else
  begin
    if not selected then
    begin

      isBreak := data.FieldByName('used').IsNull
                 or (data.FieldByName('used').AsInteger = 0);
      timeIsCorrect := (not data.FieldByName('time').IsNull)
                       and (data.FieldByName('time').AsInteger > 0);

      if isBreak then
        Result := clTeal
      else if not timeIsCorrect then
        Result := clRed;

    end;
  end;
end;

{ TmainForm }


procedure TmainForm.curDateChange(Sender: TObject);
begin
  if (not notCallUpdateDate) and (not curDate.Focused) then
    updateDate;
end;

procedure TmainForm.curDateEditingDone(Sender: TObject);
begin
  if not notCallUpdateDate then
    updateDate;
end;

procedure TmainForm.FormCreate(Sender: TObject);
begin
  Caption := 'Timesheet';
  Caption := Caption + ' (' + Version.Get + ')';
  notCallUpdateDate := True;
  curDate.Date := Now;
end;

procedure TmainForm.FormShow(Sender: TObject);
begin
  notCallUpdateDate:=False;;
  updateDate;
end;

procedure TmainForm.gridCellClick(Column: TColumn);
begin
  updateSelectElapsed;
end;

procedure TmainForm.gridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  wish: TColor;
begin
  wish := CalcColorForCurrentRow(gdSelected in State);
  if clNone <> wish then
    grid.Canvas.Font.Color := wish;
  grid.Canvas.FillRect(Rect);
  grid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TmainForm.gridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  updateSelectElapsed;
end;

procedure TmainForm.miQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TmainForm.ToolButton8Click(Sender: TObject);
begin
  PopupCalendarForm.initialize(curDate);
end;

procedure TmainForm.updateDate;
begin
  Assert(not notCallUpdateDate, 'Flag notCallUpdateDate was set.');

  notCallUpdateDate := True;
  with curDate do
  begin
    Date := firstDayOfWeek(Date);
    dmMain.showTimesheetOnWeek(Date);
    status.Panels.Items[panelWeek].Text := IntToStr(WeekOf(Date));
  end;
  notCallUpdateDate := False;
end;

procedure TmainForm.updateTotalElapsed;
var
  bookmark: TBookmark;
  elapsed: Longint;
begin
  bookmark := grid.DataSource.DataSet.GetBookmark;
  grid.DataSource.DataSet.DisableControls;
  with grid.DataSource.DataSet do
  begin
    First;
    elapsed := 0;
    while not EOF do
    begin
      if (StrToDate(FieldByName('date').AsString,
                    dm.dbDateFormatStr,
                    dm.dbDateSeparator) >= curDate.Date) and
         (not FieldByName('used').IsNull) and
         (FieldByName('used').AsInteger <> 0) and
         (not FieldByName('time').IsNull) and
         (FieldByName('time').AsInteger > 0) then
        elapsed := elapsed + FieldByName('time').AsInteger;
      Next;
    end;
    status.Panels.Items[panelTotalElapsed].Text := auxiliary.minutesToString(elapsed);
  end;
  grid.DataSource.DataSet.GotoBookmark(bookmark);
  grid.DataSource.DataSet.FreeBookmark(bookmark);
  grid.DataSource.DataSet.EnableControls;
end;

procedure TmainForm.updateSelectElapsed;
var
  i: Integer;
  bookmark: TBookmark;
  elapsed: Longint;
begin
  bookmark := grid.DataSource.DataSet.GetBookmark;
  grid.DataSource.DataSet.DisableControls;
  elapsed := 0;
  with grid.DataSource.DataSet do
  begin
    for i := 0 to (grid.SelectedRows.Count - 1) do
    begin
      GotoBookmark(grid.SelectedRows.Items[i]);
      if (not FieldByName('used').IsNull) and
         (FieldByName('used').AsInteger <> 0) and
         (not FieldByName('time').IsNull) and
         (FieldByName('time').AsInteger > 0) then
        elapsed := elapsed + FieldByName('time').AsInteger;
    end;
  end;
  grid.DataSource.DataSet.GotoBookmark(bookmark);
  grid.DataSource.DataSet.FreeBookmark(bookmark);
  grid.DataSource.DataSet.EnableControls;
  status.Panels.Items[panelSelectElapsed].Text := auxiliary.minutesToString(elapsed);
end;

end.

