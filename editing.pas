unit editing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, DbCtrls, Buttons, maskedit, dm, sqldb;

type

  EditingMode = ( emNew, emEdit, emNewFormSel );

  { TeditingForm }

  TeditingForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    eDate: TDateEdit;
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
    procedure btnOkClick(Sender: TObject);
    procedure eCategoryChange(Sender: TObject);
    procedure eFromEditingDone(Sender: TObject);
    procedure eToEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fEditingMode: EditingMode;

    function validate_time( obj: TMaskEdit ): Boolean;
    function validate_time(): Boolean;
    procedure fill_query_params(query: TSQLQuery);
  public
    { public declarations }
    property editingMode: EditingMode read fEditingMode write fEditingMode;
  end;

var
  editingForm: TeditingForm;

implementation

uses auxiliary;

{$R *.lfm}

{ TeditingForm }

procedure TeditingForm.FormCreate(Sender: TObject);
begin
  fEditingMode := emNew;
  eDate.Button.Flat := true;
  eDate.Date := Now;
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
      end;
    emEdit: begin
      Caption := 'Editing';
      eDate.Date := StrToDate( FieldByName('date').AsString, dbDateFormatStr, dbDateSeparator);
      eFrom.Text := auxiliary.minutesVariantToString(FieldByName('time_from').Value);
      eTo.Text := auxiliary.minutesVariantToString(FieldByName('time_to').Value);
      eCategory.KeyValue := FieldByName('category').Value;
      eCode.Text := FieldByName('task_code').AsString;
      eTask.Text := FieldByName('task_description').AsString;
      eComment.Text := FieldByName('comment').AsString;
      end;
    end;
  end;
end;

procedure TeditingForm.eCategoryChange(Sender: TObject);
begin

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
        if MessageDlg('Confirm', confirmMsg,
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
begin
  Result := true;
  if validate_time(eFrom) and validate_time(eTo) then
  begin
    if (eFrom.Text <> emptyTimeStr) and (eTo.Text <> emptyTimeStr) then
      if stringToMinutes(eFrom.Text) > stringToMinutes(eTo.Text) then
      begin
        ShowMessage('Invalid time!' + #13#10 +
          'The value "Time To" must be more than the value "Time From"');
        Result := false;
      end
      else
      begin
        with dmMain.sqlTimeCrosscup do
        begin
          Close;
          ParamByName('date').Value := FormatDateTime(dbDateFormatStr,eDate.Date);
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
            ShowMessage('There are time crosscups on date ' +
                DateToStr(eDate.Date) + '!' + #13#10 +
                dmMain.makeMsgBodyForTimeCrosscup
              );
            Result := false;
          end;
          Close;
        end;
      end;
  end;
end;

procedure TeditingForm.fill_query_params(query: TSQLQuery);
begin
  with query do
  begin
    ParamByName('date').Value := FormatDateTime(dbDateFormatStr,eDate.Date);
    ParamByName('time_from').Value := auxiliary.stringToMinutesVariant(eFrom.Text);
    ParamByName('time_to').Value := auxiliary.stringToMinutesVariant(eTo.Text);
    ParamByName('category').Value := eCategory.KeyValue;
    ParamByName('task_code').Value := eCode.Text;
    ParamByName('task_description').Value := eTask.Text;
    ParamByName('comment').Value := eComment.Text;
  end;
end;

end.

