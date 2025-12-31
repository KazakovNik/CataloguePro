unit uFormAbout;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, uFormDialogBase;

type
  TFormAbout = class(TFormDialogBase)
    lblProgramName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblUrl: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'О программе';
  lblProgramName.Caption := Application.MainForm.Caption;
  lblVersion.Caption := 'Версия: 1.0';
  lblCopyright.Caption := 'Казаков Николай Вадимович';
  lblUrl.Caption := 'https://github.com/KazakovNik/CataloguePro';
end;

end.
