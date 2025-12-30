unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  uFileContentController, uSettingsController;

type
  TFormMain = class(TForm)
    ImageList1: TImageList;
    ActionManager1: TActionManager;
    ActionToolBar1: TActionToolBar;
    actLoadFile: TAction;
    OpenDialog: TOpenDialog;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    lbHeap: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actLoadFileExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFileLoader: TFileContentController;
    FSettings: TSettingsController;
  public
    procedure LoadFile(filename: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.actLoadFileExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  if OpenDialog.Execute then
    LoadFile(OpenDialog.FileName);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSettings.MainFormHeight := Self.ClientHeight;
  FSettings.MainFormWidth := Self.ClientWidth;
  FSettings.MainFormLeft := Self.Left;
  FSettings.MainFormTop := Self.Top;
  FSettings.HeapWidth := lbHeap.Width;
  FSettings.Save;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  settingsFN: string;
begin
  FFileLoader := TFileContentController.Create();
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFileLoader.Free;
  FSettings.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  settingsFN: string;
begin
  settingsFN := ExtractFilePath(Application.ExeName) + '\settings.ini';
  FSettings := TSettingsController.Create(settingsFN);
  if not FileExists(settingsFN) then
  begin
    Self.Position := poDesktopCenter;
    Self.ClientHeight := Round(Screen.Height / 100 * 50);
    Self.ClientWidth := Round(Screen.Width / 100 * 50);
  end
  else
  begin
    Self.Position := poDefault;
    Self.ClientHeight := FSettings.MainFormHeight;
    Self.ClientWidth := FSettings.MainFormWidth;
    Self.Left := FSettings.MainFormLeft;
    Self.Top := FSettings.MainFormTop;
    lbHeap.Width := FSettings.HeapWidth;
  end;
end;

procedure TFormMain.LoadFile(filename: string);
begin
  FFileLoader.FilePath := filename;
  lbHeap.Items.BeginUpdate;
  try
    lbHeap.Items.Clear;
    try
      lbHeap.Items.Text := FFileLoader.Content;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    lbHeap.Items.EndUpdate;
  end;
end;

end.
