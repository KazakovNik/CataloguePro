unit uFormSettings;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, uFormDialogBase, uFramePatch;

type
  TOpenEvent = procedure(aPatch: string) of object;

  TFormSettings = class(TFormDialogBase)
    frmLastOpen: TFramePatch;
    frmLastSave: TFramePatch;
    frmLogFile: TFramePatch;
    pnlMaxCountFileHistopy: TPanel;
    lblCurrentDirOpen: TLabel;
    edtMaxCountFileHistopy: TEdit;
    procedure edtMaxCountFileHistopyKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    FOnLastFolderOpen: TOpenEvent;
    FOnLastFolderSaveOpen: TOpenEvent;
    FOnLogFileOpen: TOpenEvent;
  private
    procedure DoOpenLastOpen(Sender: TObject);
    procedure DoLastFolderSaveOpen(Sender: TObject);
    procedure DoLogFileOpen(Sender: TObject);
  public
    property OnLastFolderOpen: TOpenEvent read FOnLastFolderOpen write FOnLastFolderOpen;
    property OnLastFolderSaveOpen: TOpenEvent read FOnLastFolderSaveOpen write FOnLastFolderSaveOpen;
    property OnLogFileOpen: TOpenEvent read FOnLogFileOpen write FOnLogFileOpen;
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

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  inherited;

  frmLastOpen.btnOpenDir.OnClick := DoOpenLastOpen;
  frmLastSave.btnOpenDir.OnClick := DoLastFolderSaveOpen;
  frmLogFile.btnOpenDir.OnClick := DoLogFileOpen;
end;

procedure TFormSettings.DoLogFileOpen(Sender: TObject);
begin
  if Assigned(OnLogFileOpen) then
    OnLogFileOpen(frmLogFile.edtPatch.Text);
end;

procedure TFormSettings.DoOpenLastOpen(Sender: TObject);
begin
  if Assigned(OnLastFolderOpen) then
    OnLastFolderOpen(frmLastOpen.edtPatch.Text);
end;

procedure TFormSettings.DoLastFolderSaveOpen(Sender: TObject);
begin
  if Assigned(OnLastFolderSaveOpen) then
    OnLastFolderSaveOpen(frmLastSave.edtPatch.Text);
end;

end.
