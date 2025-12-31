unit uDialogFacade;

interface

uses
  System.UITypes;

type
  TDialogFacade = class
  private
    function MessageDialog(Msg: String; Title: String; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; Cod_Ajuda: Integer) : TModalResult;
  public
    function MessageInfoDialogOkCancel(Msg: String; Title: String): Boolean;
    function CreateInputDialog(const ACaption, APrompt, ADefault: string;
      out Text: string): Boolean;
    procedure MessageError(Msg: String);
  end;

implementation

uses
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Forms, uFormInputBox;

{ TDialogFacade }

function TDialogFacade.CreateInputDialog(const ACaption, APrompt, ADefault: string;
  out Text: string): Boolean;
begin
  Result := TFormInputBox.Show(ACaption, APrompt, ADefault, Text);
end;

function TDialogFacade.MessageDialog(Msg, Title: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Cod_Ajuda: Integer): TModalResult;
var
  i: Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) Do
  begin
    try
      Caption := Title;
      HelpContext := Cod_Ajuda;
      Left:= Screen.ActiveForm.Left + (Screen.ActiveForm.Width - Width) div 2;
      Top := Screen.ActiveForm.Top + (Screen.ActiveForm.Height - Height) div 2;

      for i := 0 to ComponentCount - 1 Do
      begin
        if not (Components[i] is TButton) then
          Continue;

        case (Components[i] As TButton).ModalResult of
          mrNone: (Components[i] As TButton).Caption := '&Help';
          mrAbort: (Components[i] As TButton).Caption := 'Прервать';
          mrAll: (Components[i] As TButton).Caption := '&Все';
          mrCancel: (Components[i] As TButton).Caption := '&Отмена';
          mrIgnore: (Components[i] As TButton).Caption := '&Игнорировать';
          mrNo: (Components[i] As TButton).Caption := '&Нет';
          mrNoToAll: (Components[i] As TButton).Caption := 'Нет всему';
          mrOk: (Components[i] As TButton).Caption := '&Ok';
          mrRetry: (Components[i] As TButton).Caption := '&Повторить попытку';
          mrYes: (Components[i] As TButton).Caption := '&Да';
          mrYesToAll: (Components[i] As TButton).Caption := 'Да всем';
        end;
      end;

      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TDialogFacade.MessageError(Msg: String);
begin
  MessageDialog(Msg, 'Ошибка', TMsgDlgType.mtError, [mbOk], 0)
end;

function TDialogFacade.MessageInfoDialogOkCancel(Msg, Title: String): Boolean;
begin
  Result :=
    MessageDialog(Msg, Title, TMsgDlgType.mtInformation, [mbOk, mbCancel], 0) = mrOk;
end;

end.
