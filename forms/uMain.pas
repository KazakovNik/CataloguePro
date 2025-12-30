unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  uFileContentController, uSettingsController, uTreeController, Vcl.Menus;

type
  TFormMain = class(TForm)
    ImageList1: TImageList;
    ActionManager1: TActionManager;
    ActionToolBar1: TActionToolBar;
    actLoadFile: TAction;
    OpenDialog: TOpenDialog;
    TreeView: TTreeView;
    Splitter: TSplitter;
    lbHeap: TListBox;
    actAddNode: TAction;
    actAddRootNode: TAction;
    actEditNode: TAction;
    actDeleteNode: TAction;
    actSaveTreeToFile: TAction;
    dlgSaveTextFile: TSaveDialog;
    pmTree: TPopupMenu;
    actAddNode1: TMenuItem;
    actAddRootNode1: TMenuItem;
    actDeleteNode1: TMenuItem;
    actEditNode1: TMenuItem;
    actSaveTreeToFile1: TMenuItem;
    N1: TMenuItem;
    actInsertData: TAction;
    actReturnBack: TAction;
    pnlСenter: TPanel;
    pnlСenterBtn: TPanel;
    btnInsert: TButton;
    btnReturnBack: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actLoadFileExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAddNodeExecute(Sender: TObject);
    procedure actAddRootNodeExecute(Sender: TObject);
    procedure actEditNodeExecute(Sender: TObject);
    procedure actDeleteNodeExecute(Sender: TObject);
    procedure actSaveTreeToFileExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFileLoader: TFileContentController;
    FSettings: TSettingsController;
    FTreeController: TTreeController;
  public
    procedure LoadFile(filename: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.actAddNodeExecute(Sender: TObject);
begin
  FTreeController.AddNode(TreeView.Selected,
    InputBox('Новый элемент', 'Название:', Format('Новая элемент (%d)', [TreeView.Items.Count])));
end;

procedure TFormMain.actAddRootNodeExecute(Sender: TObject);
begin
  FTreeController.AddNode(nil,
    InputBox('Новый элемент', 'Название:', Format('Новая элемент (%d)', [TreeView.Items.Count])));
end;

procedure TFormMain.actDeleteNodeExecute(Sender: TObject);
begin
  if Assigned(TreeView.Selected) then
    FTreeController.DeleteNode(TreeView.Selected);
end;

procedure TFormMain.actEditNodeExecute(Sender: TObject);
begin
  if Assigned(TreeView.Selected) then
    FTreeController.EditNode(TreeView.Selected,
      InputBox('Редактирование', 'Измените название:', TreeView.Selected.Text));
end;

procedure TFormMain.actLoadFileExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := FSettings.FileOpenDirectory;
  if OpenDialog.Execute then
  begin
    FSettings.FileOpenDirectory := ExtractFilePath(OpenDialog.FileName);
    LoadFile(OpenDialog.FileName);
  end;
end;

procedure TFormMain.actSaveTreeToFileExecute(Sender: TObject);
begin
  dlgSaveTextFile.InitialDir := FSettings.FileSaveDirectory;
  if dlgSaveTextFile.Execute then
  begin
    FSettings.FileSaveDirectory := ExtractFilePath(dlgSaveTextFile.FileName);
    FTreeController.SaveToFile(ChangeFileExt(dlgSaveTextFile.FileName, '.txt'));
  end;
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
  FTreeController := TTreeController.Create(TreeView);

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

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFileLoader.Free;
  FSettings.Free;
  FTreeController.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnlСenterBtn.Top := (pnlСenter.Height - pnlСenterBtn.Height) div 2;
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
