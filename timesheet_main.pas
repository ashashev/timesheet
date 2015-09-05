unit timesheet_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, ComCtrls, ActnList, EditBtn, dm, db, Grids;

type

  { TmainForm }

  TmainForm = class(TForm)
    curDate: TDateEdit;
    dsTimesheet: TDataSource;
    grid: TDBGrid;
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
    procedure curDateChange(Sender: TObject);
    procedure curDateEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridCellClick({%H-}Column: TColumn);
    procedure gridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure gridKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
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

uses auxiliary, dateutils;

const
  panelWeek = 1;
  panelTotalElapsed = 3;
  panelSelectElapsed = 5;

{$R *.lfm}

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
  curDate.Button.Flat := True;
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
begin
  if not (gdSelected in State) then
  begin
    with grid.DataSource.DataSet do
    begin
      if FieldByName('used').IsNull
         or (FieldByName('used').AsInteger = 0) then
      begin
        grid.Canvas.Font.Color := clGray;
      end
      else if FieldByName('time').IsNull
              or (FieldByName('time').AsInteger = 0) then
      begin
        grid.Canvas.Font.Color := clRed;
      end;
    end;
  end;
  grid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TmainForm.gridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  updateSelectElapsed;
end;

procedure TmainForm.updateDate;
var
  curday: Integer;
begin
  Assert(not notCallUpdateDate, 'Flag notCallUpdateDate was set.');

  notCallUpdateDate := True;
  with curDate do
  begin
    curday := DayOfWeek(Date);

    if curday = sunday then
      Date := Date - (daysPerWeek - 1)
    else
      Date := Date - (curday - monday);

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
      if (not FieldByName('used').IsNull) and
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

