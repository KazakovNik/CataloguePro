unit uFormDialogBase;

interface

uses
  Winapi.Windows, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormDialogBase = class(TForm)
    pnlBtnPanel: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDialogBase: TFormDialogBase;

implementation

{$R *.dfm}

procedure TFormDialogBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
