unit editing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, DbCtrls, Buttons, maskedit, dm, sqldb;

type

  EditingMode = ( em_new, em_edit, em_new_form_sel );

  { Tediting_form }

  Tediting_form = class(TForm)
    btn_ok: TBitBtn;
    btn_cancel: TBitBtn;
    e_date: TDateEdit;
    ds_categories: TDataSource;
    e_category: TDBLookupComboBox;
    e_code: TEdit;
    e_task: TEdit;
    e_comment: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    e_from: TMaskEdit;
    e_to: TMaskEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure btn_okClick(Sender: TObject);
    procedure e_categoryChange(Sender: TObject);
    procedure e_fromEditingDone(Sender: TObject);
    procedure e_toEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    f_editing_mode: EditingMode;

    function validate_time( obj: TMaskEdit ): Boolean;
    function validate_time(): Boolean;
    procedure fill_query_params(query: TSQLQuery);
  public
    { public declarations }
    property editing_mode: EditingMode read f_editing_mode write f_editing_mode;
  end;

var
  editing_form: Tediting_form;

implementation

uses auxiliary;

{$R *.lfm}

{ Tediting_form }

procedure Tediting_form.FormCreate(Sender: TObject);
begin
  f_editing_mode := em_new;
  e_date.Button.Flat := true;
  e_date.Date := Now;
end;

procedure Tediting_form.FormShow(Sender: TObject);
begin
  if not e_category.ListSource.DataSet.Active then
    e_category.ListSource.DataSet.Open;

  e_category.ItemIndex := -1;
  e_category.KeyValue := Null;
  e_category.Text := '';

  with dm_main.sql_timesheet do
  begin
    case f_editing_mode of
    em_new, em_new_form_sel: begin
      Caption := 'New Task';
      if f_editing_mode = em_new_form_sel then
      begin
        e_category.KeyValue := FieldByName('category').Value;
        e_code.Text := FieldByName('task_code').AsString;
        e_task.Text := FieldByName('task_description').AsString;
        e_comment.Text := FieldByName('comment').AsString;
      end
      else
      begin
        e_code.Text := '';
        e_task.Text := '';
        e_comment.Text := '';
      end;
      e_from.Text := '';
      e_to.Text := '';
      end;
    em_edit: begin
      Caption := 'Editing';
      e_date.Date := StrToDate( FieldByName('date').AsString, date_format_str, date_separator);
      e_from.Text := auxiliary.minutes_variant_to_string(FieldByName('time_from').Value);
      e_to.Text := auxiliary.minutes_variant_to_string(FieldByName('time_to').Value);
      e_category.KeyValue := FieldByName('category').Value;
      e_code.Text := FieldByName('task_code').AsString;
      e_task.Text := FieldByName('task_description').AsString;
      e_comment.Text := FieldByName('comment').AsString;
      end;
    end;
  end;
end;

procedure Tediting_form.e_categoryChange(Sender: TObject);
begin

end;

procedure Tediting_form.btn_okClick(Sender: TObject);
var
  query: TSQLQuery;
  confirm_msg: String;
begin
  if not validate_time then
    ModalResult:=0
  else
  begin
    case f_editing_mode of
    em_new, em_new_form_sel:begin
      query := dm_main.sql_new;
      end;
    em_edit:
      with dm_main.sql_timesheet do
      begin
        confirm_msg := 'Are you sure you want to change record?' + #13#10 +
          'It was:' + #13#10 + dm_main.make_msg_body_for_cur_row();
        if MessageDlg('Confirm', confirm_msg,
           mtConfirmation,[mbYes,mbCancel],0,mbCancel) <> mrYes then
        begin
          ModalResult := 0;
          Exit;
        end;
        query := dm_main.sql_edit;
        query.ParamByName('id').Value := FieldByName('id').Value;
      end;
    end;

    fill_query_params(query);

    dm_main.sql_timesheet.Close;
    query.ExecSQL;

    if query.RowsAffected = 1 then
      dm_main.sql_tran.Commit
    else
      dm_main.sql_tran.Rollback;
    dm_main.sql_timesheet.Open;
  end;
end;

procedure Tediting_form.e_fromEditingDone(Sender: TObject);
begin
  validate_time(e_from);
end;

procedure Tediting_form.e_toEditingDone(Sender: TObject);
begin
  validate_time(e_to);
end;

function Tediting_form.validate_time( obj: TMaskEdit ): Boolean;
begin
  Result := true;
  if obj.Text <> empty_time_str then
  begin
    obj.Text := StringReplace( obj.Text, ' ', '0', [rfReplaceAll] );
    if not auxiliary.validate_time_string(obj.Text) then
    begin
      ShowMessage('Invalid time!' + #13#10 + error_str_time_must_be);
      obj.SetFocus;
      Result := false;
    end;
  end;
end;

function Tediting_form.validate_time: Boolean;
begin
  Result := true;
  if validate_time(e_from) and validate_time(e_to) then
  begin
    if (e_from.Text <> empty_time_str) and (e_to.Text <> empty_time_str) then
      if string_to_minutes(e_from.Text) > string_to_minutes(e_to.Text) then
      begin
        ShowMessage('Invalid time!' + #13#10 +
          'The value "Time To" must be more than the value "Time From"');
        Result := false;
      end;
  end;
end;

procedure Tediting_form.fill_query_params(query: TSQLQuery);
begin
  with query do
  begin
    ParamByName('date').Value := FormatDateTime(date_format_str,e_date.Date);
    ParamByName('time_from').Value := auxiliary.string_to_minutes_variant(e_from.Text);
    ParamByName('time_to').Value := auxiliary.string_to_minutes_variant(e_to.Text);
    ParamByName('category').Value := e_category.KeyValue;
    ParamByName('task_code').Value := e_code.Text;
    ParamByName('task_description').Value := e_task.Text;
    ParamByName('comment').Value := e_comment.Text;
  end;
end;

end.

