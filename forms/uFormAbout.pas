unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFormDialogBase, Vcl.StdCtrls,
  Vcl.ExtCtrls;

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
    procedure lblUrlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

uses
  ShellAPI;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'О программе';
  lblProgramName.Caption := Application.MainForm.Caption;
  lblVersion.Caption := 'Версия: 1.0';
  lblCopyright.Caption := 'Казакон Николай Вадимочик';
  lblUrl.Caption := 'https://github.com/KazakovNik/CataloguePro';
end;

procedure OpenInDefaultBrowser(const AURL: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormAbout.lblUrlClick(Sender: TObject);
begin
  inherited;
  OpenInDefaultBrowser(lblUrl.Caption);
end;

end.
