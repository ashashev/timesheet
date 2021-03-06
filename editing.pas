unit editing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, DbCtrls, Buttons, maskedit, ComCtrls,
  dm, sqldb;

type

  EditingMode = ( emNew, emEdit, emNewFormSel );

  { TeditingForm }

  TeditingForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbUseCurDate: TCheckBox;
    eDate: TDateTimePicker;
    dsCategories: TDataSource;
    eCategory: TDBLookupComboBox;
    eCode: TEdit;
    eTask: TEdit;
    eComment: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    eFrom: TMaskEdit;
    eTo: TMaskEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    ToolBar1: TToolBar;
    BtnShowCalendar: TToolButton;
    procedure btnOkClick(Sender: TObject);
    procedure cbUseCurDateChange(Sender: TObject);
    procedure eFromEditingDone(Sender: TObject);
    procedure eToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnShowCalendarClick(Sender: TObject);
  private
    { private declarations }
    fEditingMode: EditingMode;
    useCurDate: Boolean;

    function validate_time( obj: TMaskEdit ): Boolean;
    function validate_time(): Boolean;
    function ValidateCrossCup(var ErrorMessage: String): Boolean;
    function getDate(): TDateTime;
    procedure fill_query_params(query: TSQLQuery);
    procedure updateAvailableEDate;
    procedure selectStartedFocus;
  public
    { public declarations }
    property editingMode: EditingMode read fEditingMode write fEditingMode;
  end;

var
  editingForm: TeditingForm;

implementation

uses auxiliary, popupcalendar;

{$R *.lfm}

{ TeditingForm }

procedure TeditingForm.FormCreate(Sender: TObject);
begin
  fEditingMode := emNew;
  eDate.Date := Now;
  useCurDate := true;
end;

procedure TeditingForm.FormShow(Sender: TObject);
begin
  if not eCategory.ListSource.DataSet.Active then
    eCategory.ListSource.DataSet.Open;

  eCategory.ItemIndex := -1;
  eCategory.KeyValue := Null;
  eCategory.Text := '';

  with dmMain.sqlTimesheet do
  begin
    case fEditingMode of
      emNew, emNewFormSel: begin
        Caption := 'New Task';
        cbUseCurDate.Enabled := true;
        cbUseCurDate.Checked := useCurDate;
        if fEditingMode = emNewFormSel then
        begin
          eCategory.KeyValue := FieldByName('category').Value;
          eCode.Text := FieldByName('task_code').AsString;
          eTask.Text := FieldByName('task_description').AsString;
          eComment.Text := FieldByName('comment').AsString;
        end
        else
        begin
          eCode.Text := '';
          eTask.Text := '';
          eComment.Text := '';
        end;
        eFrom.Text := '';
        eTo.Text := '';
      end;//emNew, emNewFormSel
      emEdit: begin
        Caption := 'Editing';
        cbUseCurDate.Enabled := false;
        cbUseCurDate.Checked := false;
        eDate.Date := StrToDate( FieldByName('date').AsString, dbDateFormatStr, dbDateSeparator);
        eFrom.Text := auxiliary.minutesVariantToString(FieldByName('time_from').Value);
        eTo.Text := auxiliary.minutesVariantToString(FieldByName('time_to').Value);
        eCategory.KeyValue := FieldByName('category').Value;
        eCode.Text := FieldByName('task_code').AsString;
        eTask.Text := FieldByName('task_description').AsString;
        eComment.Text := FieldByName('comment').AsString;
      end;//emEdit
    end;//case fEditingMode of
  end;//with dmMain.sqlTimesheet do
  updateAvailableEDate;
  selectStartedFocus;
end;

procedure TeditingForm.BtnShowCalendarClick(Sender: TObject);
begin
  PopupCalendarForm.initialize(eDate);
end;

procedure TeditingForm.btnOkClick(Sender: TObject);
var
  query: TSQLQuery;
  confirmMsg: String;
begin
  if not validate_time then
    ModalResult:=0
  else
  begin
    case fEditingMode of
    emNew, emNewFormSel:begin
      query := dmMain.sqlNew;
      end;
    emEdit:
      with dmMain.sqlTimesheet do
      begin
        confirmMsg := 'Are you sure you want to change record?' + #13#10 +
          'It was:' + #13#10 + dmMain.makeMsgBodyForCurRow;
        if MessageDlg('Changing Confirm', confirmMsg,
           mtConfirmation,[mbYes,mbCancel],0,mbCancel) <> mrYes then
        begin
          ModalResult := 0;
          Exit;
        end;
        query := dmMain.sqlEdit;
        query.ParamByName('id').Value := FieldByName('id').Value;
      end;
    end;

    fill_query_params(query);

    dmMain.sqlTimesheet.Close;
    query.ExecSQL;

    if query.RowsAffected = 1 then
      dmMain.sqlTran.Commit
    else
      dmMain.sqlTran.Rollback;
    dmMain.sqlTimesheet.Open;
  end;
end;

procedure TeditingForm.cbUseCurDateChange(Sender: TObject);
begin
  if cbUseCurDate.Enabled then
    useCurDate := cbUseCurDate.Checked;
  updateAvailableEDate;
end;

procedure TeditingForm.eFromEditingDone(Sender: TObject);
begin
  validate_time(eFrom);
end;

procedure TeditingForm.eToEditingDone(Sender: TObject);
begin
  validate_time(eTo);
end;

function TeditingForm.validate_time( obj: TMaskEdit ): Boolean;
begin
  Result := true;
  if obj.Text <> emptyTimeStr then
  begin
    obj.Text := StringReplace( obj.Text, ' ', '0', [rfReplaceAll] );
    if not auxiliary.validateTimeString(obj.Text) then
    begin
      ShowMessage('Invalid time!' + #13#10 + errorStrTimeMustBe);
      obj.SetFocus;
      Result := false;
    end;
  end;
end;

function TeditingForm.validate_time: Boolean;
var
  ErrorMessage: String;
begin
  Result := False;

  if not validate_time(eFrom) then
  begin
    Result := False;
    Exit;
  end;

  if not validate_time(eTo) then
  begin
    Result := False;
    Exit;
  end;

  if (eFrom.Text = emptyTimeStr) or (eTo.Text = emptyTimeStr) then
  begin
    Result := True;
    Exit;
  end;

  if stringToMinutes(eFrom.Text) > stringToMinutes(eTo.Text) then
  begin
    ShowMessage('Invalid time!' + #13#10 +
                'The value "Time To" must be more than the value "Time From"');
    Result := False;
    Exit;
  end;

  if not ValidateCrossCup(ErrorMessage) then
  begin
    Result := False;
    ShowMessage(ErrorMessage);
    Exit;
  end;

  Result := True;
end;

function TeditingForm.ValidateCrossCup(var ErrorMessage: String): Boolean;
begin
  ErrorMessage := '';
  Result := False;
  with dmMain.sqlTimeCrosscup do
  begin
    Close;
    ParamByName('date').Value := FormatDateTime(dbDateFormatStr,getDate);
    ParamByName('time_from').Value := auxiliary.stringToMinutesVariant(eFrom.Text);
    ParamByName('time_to').Value := auxiliary.stringToMinutesVariant(eTo.Text);

    if fEditingMode <> emEdit then
      ParamByName('id').Value := Null
    else
      ParamByName('id').Value := dmMain.sqlTimesheet.FieldByName('id').Value;

    Open;
    First;
    if not Eof then
    begin
      ErrorMessage := 'There are time crosscups on date ' +
                      DateToStr(getDate) + '!' + #13#10 +
                      dmMain.makeMsgBodyForTimeCrosscup;
      Result := false;
    end
    else
    begin
      Result := true;
    end;
    Close;
  end;
end;

function TeditingForm.getDate(): TDateTime;
begin
  Result := Now;
  if not cbUseCurDate.Checked then
    Result := eDate.Date;
end;

procedure TeditingForm.fill_query_params(query: TSQLQuery);
begin
  with query do
  begin
    ParamByName('date').Value := FormatDateTime(dbDateFormatStr,getDate);
    ParamByName('time_from').Value := auxiliary.stringToMinutesVariant(eFrom.Text);
    ParamByName('time_to').Value := auxiliary.stringToMinutesVariant(eTo.Text);
    ParamByName('category').Value := eCategory.KeyValue;
    ParamByName('task_code').Value := eCode.Text;
    ParamByName('task_description').Value := eTask.Text;
    ParamByName('comment').Value := eComment.Text;
  end;
end;

procedure TeditingForm.updateAvailableEDate();
begin
  eDate.Enabled := not cbUseCurDate.Checked;
  BtnShowCalendar.Enabled := not cbUseCurDate.Checked;
end;

procedure TeditingForm.selectStartedFocus;
begin
  if cbUseCurDate.Checked then
  begin
    case fEditingMode of
      emNew: begin
        eCategory.SetFocus;
      end;
      emNewFormSel: begin
        eFrom.SetFocus;
      end;
    end;
  end
  else
  begin
    if fEditingMode = emEdit then
    begin
      if eDate.DateIsNull then
        eDate.SetFocus
      else if eCategory.KeyValue = Null then
        eCategory.SetFocus
      else if eFrom.Text = emptyTimeStr then
        eFrom.SetFocus
      else if eTo.Text = emptyTimeStr then
        eTo.SetFocus
      else if eCode.Text = '' then
        eCode.SetFocus
      else if eTask.Text = '' then
        eTask.SetFocus
      else if eComment.Text = '' then
        eComment.SetFocus
      else
        eDate.SetFocus;
    end
    else
    begin
      eDate.SetFocus;
    end;
  end;
end;

end.

