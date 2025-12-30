unit uFormInputBox;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFormDialogBase, Vcl.StdCtrls,
  Vcl.ExtCtrls;

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
