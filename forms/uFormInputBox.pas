unit uFormInputBox;

interface

uses
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  uFormDialogBase;

type
  TFormInputBox = class(TFormDialogBase)
    lblTextLabel: TLabel;
    edtText: TEdit;
  private
    { Private declarations }
  public
    class function Show(const aCaption, aPrompt, aDefault: string; out aText: string): boolean;
  end;

var
  FormInputBox: TFormInputBox;

implementation

{$R *.dfm}

uses
  System.UITypes;

{ TFormInputBox }

class function TFormInputBox.Show(const aCaption, aPrompt,
  aDefault: string; out aText: string): boolean;
var
  vForm: TFormInputBox;
begin
  Result := False;
  aText := aDefault;
  vForm := TFormInputBox.Create(nil);
  try
    vForm.Caption := aCaption;
    vForm.lblTextLabel.Caption := aPrompt;
    vForm.edtText.Text := aDefault;

    if vForm.ShowModal = mrOk then
    begin
      aText := vForm.edtText.Text;
      Result := True;
    end;
  finally
    vForm.Free;
  end;
end;

end.
