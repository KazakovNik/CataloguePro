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
    class function Show(const ACaption, APrompt, ADefault: string; out Text: string): boolean;
  end;

var
  FormInputBox: TFormInputBox;

implementation

{$R *.dfm}

uses
  System.UITypes;

{ TFormInputBox }

class function TFormInputBox.Show(const ACaption, APrompt,
  ADefault: string; out Text: string): boolean;
var
  Form: TFormInputBox;
begin
  Result := False;
  Text := ADefault;
  Form := TFormInputBox.Create(nil);
  try
    Form.Caption := ACaption;
    Form.lblTextLabel.Caption := APrompt;
    Form.edtText.Text := ADefault;

    if Form.ShowModal = mrOk then
    begin
      Text := Form.edtText.Text;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

end.
