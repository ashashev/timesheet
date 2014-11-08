unit timesheet_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, ComCtrls, StdCtrls, ActnList, Calendar, EditBtn, dm, db,
  editing;

type

  { Ttimesheet_main_form }

  Ttimesheet_main_form = class(TForm)
    cur_date: TDateEdit;
    ds_timesheet: TDataSource;
    grid: TDBGrid;
    Panel1: TPanel;
    status: TStatusBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    tool_bar: TToolBar;
    procedure cur_dateChange(Sender: TObject);
    procedure cur_dateEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridCellClick(Column: TColumn);
    procedure gridEditingDone(Sender: TObject);
    procedure gridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    procedure update_date;
  public
    { public declarations }
    procedure update_total_elapsed;
    procedure update_select_elapsed;
  end;

var
  timesheet_main_form: Ttimesheet_main_form;

implementation

uses auxiliary, dateutils;

const
  panel_week = 1;
  panel_total_elapsed = 3;
  panel_select_elapsed = 5;

{$R *.lfm}

{ Ttimesheet_main_form }


procedure Ttimesheet_main_form.cur_dateChange(Sender: TObject);
begin
  if not cur_date.Focused then
    update_date;
end;

procedure Ttimesheet_main_form.cur_dateEditingDone(Sender: TObject);
begin
  update_date;
end;

procedure Ttimesheet_main_form.FormCreate(Sender: TObject);
begin
  Caption := 'Timesheet';
  cur_date.Button.Flat := True;
  cur_date.Date := Now;
end;

procedure Ttimesheet_main_form.FormShow(Sender: TObject);
begin
  update_date;
end;

procedure Ttimesheet_main_form.gridCellClick(Column: TColumn);
begin
  update_select_elapsed;
end;

procedure Ttimesheet_main_form.gridEditingDone(Sender: TObject);
begin

end;

procedure Ttimesheet_main_form.gridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  update_select_elapsed;
end;

procedure Ttimesheet_main_form.update_date;
var
  curday: Integer;
begin
  with cur_date do
  begin
    curday := DayOfWeek(Date);
    if curday = sunday then
      Date := Date - (days_per_week - 1)
    else
      Date := Date - (curday - monday);
    dm_main.show_timesheet_on_week(Date);
    status.Panels.Items[panel_week].Text := IntToStr(WeekOf(Date));
  end;
end;

procedure Ttimesheet_main_form.update_total_elapsed;
var
  bookmark: TBookmark;
  elapsed: Longint;
begin
  if not grid.DataSource.DataSet.Active then
    grid.DataSource.DataSet.Open;

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
         (FieldByName('time').AsInteger > 0) then
        elapsed := elapsed + FieldByName('time').AsInteger;
      Next;
    end;
    status.Panels.Items[panel_total_elapsed].Text := auxiliary.minutes_to_string(elapsed);
  end;
  grid.DataSource.DataSet.GotoBookmark(bookmark);
  grid.DataSource.DataSet.EnableControls;
end;

procedure Ttimesheet_main_form.update_select_elapsed;
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
         (FieldByName('time').AsInteger > 0) then
        elapsed := elapsed + FieldByName('time').AsInteger;
    end;
  end;
  grid.DataSource.DataSet.GotoBookmark(bookmark);
  grid.DataSource.DataSet.EnableControls;
  status.Panels.Items[panel_select_elapsed].Text := auxiliary.minutes_to_string(elapsed);
end;

end.

