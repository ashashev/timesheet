unit dm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, ActnList, Controls;

const
  dbDateFormatStr = 'yyyy-mm-dd';
  dbDateSeparator = '-';

type

  { TdmMain }

  TdmMain = class(TDataModule)
    actShowWeekReport: TAction;
    actNewFromSel: TAction;
    actions: TActionList;
    actDelete: TAction;
    actEdit: TAction;
    actNew: TAction;
    dbCon: TSQLite3Connection;
    images: TImageList;
    sqlWeekReport: TSQLQuery;
    sqlTimeCrosscup: TSQLQuery;
    sqlDelete: TSQLQuery;
    sqlNew: TSQLQuery;
    sqlEdit: TSQLQuery;
    sqlCategories: TSQLQuery;
    sqlCategoriesid: TLongintField;
    sqlCategoriesname: TMemoField;
    sqlCategoriesused: TLongintField;
    sqlTimesheetCategory: TLongintField;
    sqlTimesheetComment: TMemoField;
    sqlTimesheetdate: TMemoField;
    sqlTimesheetid: TLongintField;
    sqlTimesheetTask: TStringField;
    sqlTimesheetTaskCode: TMemoField;
    sqlTimesheetTaskDescription: TMemoField;
    sqlTimesheetTime: TLargeintField;
    sqlTimesheetTimeFrom:TLongintField;
    sqlTimesheetTimeTo: TLongintField;
    sqlTimesheetUsed: TLongintField;
    sqlTran: TSQLTransaction;
    sqlTimesheet: TSQLQuery;
    sqlWeekReportmonth: TStringField;
    sqlWeekReporttask: TStringField;
    sqlWeekReporttime: TLargeintField;
    procedure actNewFromSelExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actShowWeekReportExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure sqlCategoriesnameGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlTimesheetAfterOpen(DataSet: TDataSet);
    procedure sqlTimesheetdateGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlTimesheetTaskGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlTimesheetTimeGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlTimesheetTimeFromGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure sqlTimesheetTimeToGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlWeekReportmonthGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure sqlWeekReporttimeGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    procedure showTimesheetOnWeek(startWeek: TDateTime);
    procedure showWeekReport(startWeek: TDateTime);
    function makeMsgBodyForCurRow: String;
    function makeMsgBodyForTimeCrosscup: String;
  end;

var
  dmMain: TdmMain;

implementation

uses auxiliary, editing, timesheet_main, dialogs, weekreportu;

{$R *.lfm}

{ TdmMain }

procedure TdmMain.sqlTimesheetdateGetText(Sender: TField; var aText: string;
    DisplayText: Boolean);
var
  curDate: TDateTime;
  strDate: String;
begin
  try
    curDate := StrToDate(sqlTimesheetdate.AsString,dbDateFormatStr,dbDateSeparator);
    aText := DateToStr(curDate);
  except
    aText := '';
  end;
end;

procedure TdmMain.sqlTimesheetTaskGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := Copy(sqlTimesheetTask.AsString,1,255);
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  DateSeparator := '.';
  ShortDateFormat := 'dd.mm.yyyy';
  dbCon.Close(True);
  dbCon.Open;
end;

procedure TdmMain.actNewExecute(Sender: TObject);
begin
  editingForm.editingMode := emNew;
  editingForm.ShowModal;
end;

procedure TdmMain.actShowWeekReportExecute(Sender: TObject);
begin
  showWeekReport(mainForm.curDate.Date);
end;

procedure TdmMain.actEditExecute(Sender: TObject);
begin
  editingForm.editingMode := emEdit;
  editingForm.ShowModal;
end;

procedure TdmMain.actDeleteExecute(Sender: TObject);
var
  confirm_msg: String;
begin
  confirm_msg := 'Are you sure you want to delete this record?' + #13#10 +
    dmMain.makeMsgBodyForCurRow();

  if MessageDlg(
      'Confirm',
      confirm_msg,
      mtConfirmation,
      [mbYes, mbCancel],
      0,
      mbCancel
    ) = mrYes then
  begin
    sqlDelete.ParamByName('id').Value := sqlTimesheet.FieldByName('id').Value;
    sqlTimesheet.Close;
    sqlDelete.ExecSQL;
    if sqlDelete.RowsAffected = 1 then
      sqlTran.Commit
    else
      sqlTran.Rollback;
    sqlTimesheet.Open;
  end;
end;

procedure TdmMain.actNewFromSelExecute(Sender: TObject);
begin
  editingForm.editingMode := emNewFormSel;
  editingForm.ShowModal;
end;

procedure TdmMain.sqlCategoriesnameGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := sqlCategoriesname.AsString;
end;

procedure TdmMain.sqlTimesheetAfterOpen(DataSet: TDataSet);
begin
  mainForm.updateTotalElapsed;
  mainForm.updateSelectElapsed;
end;

procedure TdmMain.sqlTimesheetTimeGetText(Sender: TField; var aText: string;
    DisplayText: Boolean);
begin
  if not sqlTimesheetTime.IsNull then
    aText:= auxiliary.minutesToString(sqlTimesheetTime.Value)
  else
    aText := '';
end;

procedure TdmMain.sqlTimesheetTimeFromGetText(Sender: TField;
    var aText: string; DisplayText: Boolean);
begin
  if not sqlTimesheetTimeFrom.IsNull then
    aText := auxiliary.minutesToString(sqlTimesheetTimeFrom.Value)
  else
    aText := '';
end;

procedure TdmMain.sqlTimesheetTimeToGetText(Sender: TField;
    var aText: string; DisplayText: Boolean);
begin
  if not sqlTimesheetTimeTo.IsNull then
    aText:= auxiliary.minutesToString(sqlTimesheetTimeTo.Value)
  else
    aText := '';
end;

procedure TdmMain.sqlWeekReportmonthGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if not sqlWeekReportMonth.IsNull then
    aText := FormatDateTime('mmm',StrToDate(sqlWeekReportMonth.Value,'m'))
  else
    aText := '';
end;

procedure TdmMain.sqlWeekReporttimeGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if not sqlWeekReportTime.IsNull then
    aText:= auxiliary.minutesToString(sqlWeekReportTime.Value)
  else
    aText := '';
end;

procedure TdmMain.showTimesheetOnWeek(startWeek: TDateTime);
begin
  sqlTimesheet.Close;
  sqlTimesheet.ParamByName('datefrom').Value :=
      FormatDateTime(dbDateFormatStr, startWeek );
  sqlTimesheet.ParamByName('dateto').Value :=
      FormatDateTime(dbDateFormatStr, startWeek + (daysPerWeek - 1));
  sqlTimesheet.Open;
end;

procedure TdmMain.showWeekReport(startWeek: TDateTime);
begin
  with sqlWeekReport do
  begin
    Close;
    ParamByName('datefrom').Value :=
        FormatDateTime(dbDateFormatStr, startWeek );
    ParamByName('dateto').Value :=
        FormatDateTime(dbDateFormatStr, startWeek + (daysPerWeek - 1));
    Open;
    weekReport.lblFrom.Caption := DateToStr(startWeek);
    weekReport.lblTo.Caption   := DateToStr(startWeek + (daysPerWeek - 1));
    weekReport.ShowModal;
  end;
end;

function TdmMain.makeMsgBodyForCurRow(): String;
var
  date: TDateTime;
begin
  with sqlTimesheet do
  begin
    date := StrToDate(FieldByName('date').AsString, dbDateFormatStr, dbDateSeparator);
    Result := 'Date: ' + DateToStr(date) + #13#10 +
      'Time: from ' +
       auxiliary.minutesVariantToString(FieldByName('time_from').Value, '-') +
      ' to ' +
       auxiliary.minutesVariantToString(FieldByName('time_to').Value, '-') +
       #13#10 +
      'Task: ' + FieldByName('task').AsString + #13#10 +
      'Elapsed: ' +
       auxiliary.minutesVariantToString(FieldByName('time').Value, '-');
  end;
end;

function TdmMain.makeMsgBodyForTimeCrosscup: String;
begin
  Result := '';
  with sqlTimeCrosscup do
  begin
    First;
    while not Eof do
    begin
      Result := Result +
        'Time: from ' +
        auxiliary.minutesVariantToString(FieldByName('time_from').Value, '-') +
        ' to ' +
        auxiliary.minutesVariantToString(FieldByName('time_to').Value, '-') +
        #13#10 +
        'Task: ' + FieldByName('task').AsString + #13#10;
      Next;
    end;
  end;
end;

end.

