unit dm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, ActnList, Controls;

const
  date_format_str = 'yyyy-mm-dd';
  date_separator = '-';

type

  { Tdm_main }

  Tdm_main = class(TDataModule)
    actions: TActionList;
    act_delete: TAction;
    act_edit: TAction;
    act_new: TAction;
      db_con: TSQLite3Connection;
      images: TImageList;
      sql_delete: TSQLQuery;
      sql_new: TSQLQuery;
      sql_edit: TSQLQuery;
      sql_categories: TSQLQuery;
      sql_categoriesid: TLongintField;
      sql_categoriesname: TMemoField;
      sql_categoriesused: TLongintField;
      sql_timesheetcategory: TLongintField;
      sql_timesheetcomment: TMemoField;
      sql_timesheetdate: TMemoField;
      sql_timesheetid: TLongintField;
      sql_timesheettask: TStringField;
      sql_timesheettask_code: TMemoField;
      sql_timesheettask_description: TMemoField;
      sql_timesheettime: TLargeintField;
      sql_timesheettime_from: TLongintField;
      sql_timesheettime_to: TLongintField;
      sql_timesheetused: TLongintField;
      sql_tran: TSQLTransaction;
      sql_timesheet: TSQLQuery;
      procedure act_deleteExecute(Sender: TObject);
      procedure act_editExecute(Sender: TObject);
      procedure act_newExecute(Sender: TObject);
      procedure DataModuleCreate(Sender: TObject);
      procedure sql_categoriesnameGetText(Sender: TField; var aText: string;
        DisplayText: Boolean);
      procedure sql_timesheetAfterOpen(DataSet: TDataSet);
      procedure sql_timesheetdateGetText(Sender: TField; var aText: string;
          DisplayText: Boolean);
      procedure sql_timesheettimeGetText(Sender: TField; var aText: string;
          DisplayText: Boolean);
      procedure sql_timesheettime_fromGetText(Sender: TField;
          var aText: string; DisplayText: Boolean);
      procedure sql_timesheettime_toGetText(Sender: TField; var aText: string;
          DisplayText: Boolean);
      procedure show_timesheet_on_week(start_week: TDateTime);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dm_main: Tdm_main;

implementation

uses auxiliary, editing, timesheet_main, dialogs;

{$R *.lfm}

{ Tdm_main }

procedure Tdm_main.sql_timesheetdateGetText(Sender: TField; var aText: string;
    DisplayText: Boolean);
begin
  aText := sql_timesheetdate.AsString;
end;

procedure Tdm_main.DataModuleCreate(Sender: TObject);
begin
  sql_timesheet.Open;
end;

procedure Tdm_main.act_newExecute(Sender: TObject);
begin
  editing_form.editing_mode := em_new;
  editing_form.ShowModal;
end;

procedure Tdm_main.act_editExecute(Sender: TObject);
begin
  editing_form.editing_mode := em_edit;
  editing_form.ShowModal;
end;

procedure Tdm_main.act_deleteExecute(Sender: TObject);
var
  confirm_msg: String;
begin
  confirm_msg := 'Are you sure you want to delete this record?' + #13#10 +
    'Date: ' + sql_timesheet.FieldByName('date').AsString + #13#10 +
    'Task: ' + sql_timesheet.FieldByName('task').AsString + #13#10 +
    'Elapsed: ' +
    auxiliary.minutes_to_string(sql_timesheet.FieldByName('time').Value);

  if MessageDlg(
      'Confirm',
      confirm_msg,
      mtConfirmation,
      [mbYes, mbCancel],
      0,
      mbCancel
    ) = mrYes then
  begin
    sql_delete.ParamByName('id').Value := sql_timesheet.FieldByName('id').Value;
    sql_timesheet.Close;
    sql_delete.ExecSQL;
    if sql_delete.RowsAffected = 1 then
      sql_tran.Commit
    else
      sql_tran.Rollback;
    sql_timesheet.Open;
  end;
end;

procedure Tdm_main.sql_categoriesnameGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := sql_categoriesname.AsString;
end;

procedure Tdm_main.sql_timesheetAfterOpen(DataSet: TDataSet);
begin
  timesheet_main_form.update_total_elapsed;
  timesheet_main_form.update_select_elapsed;
end;

procedure Tdm_main.sql_timesheettimeGetText(Sender: TField; var aText: string;
    DisplayText: Boolean);
begin
  aText:= auxiliary.minutes_to_string(sql_timesheettime.Value);
end;

procedure Tdm_main.sql_timesheettime_fromGetText(Sender: TField;
    var aText: string; DisplayText: Boolean);
begin
    aText:= auxiliary.minutes_to_string(sql_timesheettime_from.Value);
end;

procedure Tdm_main.sql_timesheettime_toGetText(Sender: TField;
    var aText: string; DisplayText: Boolean);
begin
  aText:= auxiliary.minutes_to_string(sql_timesheettime_to.Value);
end;

procedure Tdm_main.show_timesheet_on_week(start_week: TDateTime);
begin
  sql_timesheet.Close;
  sql_timesheet.ParamByName('datefrom').Value :=
      FormatDateTime(date_format_str, start_week );
  sql_timesheet.ParamByName('dateto').Value :=
      FormatDateTime(date_format_str, start_week + (days_per_week - 1));
  sql_timesheet.Open;
end;

end.

