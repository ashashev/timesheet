unit weekreportu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls;

type

  { TweekReport }

  TweekReport = class(TForm)
    dsWeekReport: TDataSource;
    griid: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    lblTo: TLabel;
    lblFrom: TLabel;
    Panel1: TPanel;
    procedure lblFromClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  weekReport: TweekReport;

implementation

{$R *.lfm}

{ TweekReport }

procedure TweekReport.lblFromClick(Sender: TObject);
begin

end;

end.

