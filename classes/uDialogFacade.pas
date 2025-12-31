unit uDialogFacade;

interface

uses
  System.UITypes;

type
  TDialogFacade = class
  private
    function MessageDialog(aMsg: String; aTitle: String; aDlgType: TMsgDlgType;
      aButtons: TMsgDlgButtons; aCodAjuda: Integer) : TModalResult;
  public
    function MessageInfoDialogOkCancel(aMsg: String; aTitle: String): Boolean;
    function CreateInputDialog(const aCaption, aPrompt, aDefault: string;
      out aText: string): Boolean;
    procedure MessageError(aMsg: String);
  end;

implementation

uses
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Forms, uFormInputBox, uRsControls;

{ TDialogFacade }

function TDialogFacade.CreateInputDialog(const aCaption, aPrompt, aDefault: string;
  out aText: string): Boolean;
begin
  Result := TFormInputBox.Show(aCaption, aPrompt, aDefault, aText);
end;

function TDialogFacade.MessageDialog(aMsg, aTitle: String; aDlgType: TMsgDlgType;
  aButtons: TMsgDlgButtons; aCodAjuda: Integer): TModalResult;
var
  i: Integer;
begin
  with CreateMessageDialog(aMsg, aDlgType, aButtons) Do
  begin
    try
      Caption := aTitle;
      HelpContext := aCodAjuda;
      Left:= Screen.ActiveForm.Left + (Screen.ActiveForm.Width - Width) div 2;
      Top := Screen.ActiveForm.Top + (Screen.ActiveForm.Height - Height) div 2;

      for i := 0 to ComponentCount - 1 Do
      begin
        if not (Components[i] is TButton) then
          Continue;

        case (Components[i] As TButton).ModalResult of
          mrNone: (Components[i] As TButton).Caption := resDialogNone;
          mrAbort: (Components[i] As TButton).Caption := resDialogAbort;
          mrAll: (Components[i] As TButton).Caption := resDialogAll;
          mrCancel: (Components[i] As TButton).Caption := resDialogCancel;
          mrIgnore: (Components[i] As TButton).Caption := resDialogIgnore;
          mrNo: (Components[i] As TButton).Caption := resDialogNo;
          mrNoToAll: (Components[i] As TButton).Caption := resDialogNoToAll;
          mrOk: (Components[i] As TButton).Caption := resDialogOk;
          mrRetry: (Components[i] As TButton).Caption := resDialogRetry;
          mrYes: (Components[i] As TButton).Caption := resDialogYes;
          mrYesToAll: (Components[i] As TButton).Caption := resDialogYesToAll;
        end;
      end;

      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TDialogFacade.MessageError(aMsg: String);
begin
  MessageDialog(aMsg, resDialogErrorTitle, TMsgDlgType.mtError, [mbOk], 0)
end;

function TDialogFacade.MessageInfoDialogOkCancel(aMsg, aTitle: String): Boolean;
begin
  Result :=
    MessageDialog(aMsg, aTitle, TMsgDlgType.mtInformation, [mbOk, mbCancel], 0) = mrOk;
end;

end.
