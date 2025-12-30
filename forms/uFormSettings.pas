unit uFormSettings;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, uFormDialogBase, uFramePatch;

type
  TFormSettings = class(TFormDialogBase)
    frmLastOpen: TFrame1;
    frmLastSave: TFrame1;
    frmLogFile: TFrame1;
    pnlMaxCountFileHistopy: TPanel;
    lblCurrentDirOpen: TLabel;
    edtMaxCountFileHistopy: TEdit;
    procedure edtMaxCountFileHistopyKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

procedure TFormSettings.edtMaxCountFileHistopyKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;

  if not (CharInSet(Key, ['0'..'9']) or (Key = #8)) then
      Key := #0;
end;

end.
