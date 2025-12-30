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
    pnlÑenter: TPanel;
    pnlÑenterBtn: TPanel;
    btnInsert: TButton;
    btnReturnBack: TButton;
    actSettings: TAction;
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
    procedure actInsertDataUpdate(Sender: TObject);
    procedure actInsertDataExecute(Sender: TObject);
    procedure actReturnBackExecute(Sender: TObject);
    procedure actReturnBackUpdate(Sender: TObject);
    procedure lbHeapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FFileLoader: TFileContentController;
    FSettings: TSettingsController;
    FTreeController: TTreeController;
  private
    procedure DoReturn(Text: string);
    procedure InitSettings;
    procedure SaveSettings;
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
    InputBox('Íîâûé ıëåìåíò', 'Íàçâàíèå:', Format('Íîâàÿ ıëåìåíò (%d)', [TreeView.Items.Count])));
end;

procedure TFormMain.actAddRootNodeExecute(Sender: TObject);
begin
  FTreeController.AddNode(nil,
    InputBox('Íîâûé ıëåìåíò', 'Íàçâàíèå:', Format('Íîâàÿ ıëåìåíò (%d)', [TreeView.Items.Count])));
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
      InputBox('Ğåäàêòèğîâàíèå', 'Èçìåíèòå íàçâàíèå:', TreeView.Selected.Text));
end;

procedure TFormMain.actInsertDataExecute(Sender: TObject);
var
  index: integer;
begin
  FTreeController.InsertNode(lbHeap.Items[lbHeap.ItemIndex]);
  index := lbHeap.ItemIndex;
  lbHeap.Items.Delete(lbHeap.ItemIndex);
  if index > 0 then
    index := index - 1;
  if index <= lbHeap.Count - 1 then
    lbHeap.ItemIndex := index;
end;

procedure TFormMain.actInsertDataUpdate(Sender: TObject);
begin
  actInsertData.Enabled :=
    (lbHeap.ItemIndex <> -1) and
    (TreeView.Items.Count > 0) and
    Assigned(TreeView.Selected) and
    FTreeController.SelectedIsFolder(TreeView.Selected);
end;

procedure TFormMain.actReturnBackUpdate(Sender: TObject);
begin
  actReturnBack.Enabled :=
    (TreeView.Items.Count > 0) and
    Assigned(TreeView.Selected) and
    (FTreeController.SelectedIsItem(TreeView.Selected) or
    (FTreeController.SelectedIsFolder(TreeView.Selected) and TreeView.Selected.HasChildren));
end;

procedure TFormMain.actLoadFileExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := FSettings.FileOpenDirectory;
  if OpenDialog.Execute then
  begin
    FSettings.FileOpenDirectory := ExtractFilePath(OpenDialog.FileName);
    LoadFile(OpenDialog.FileName);
    if lbHeap.Count > 0 then
      lbHeap.ItemIndex := 0;
  end;
end;

procedure TFormMain.actReturnBackExecute(Sender: TObject);
begin
  FTreeController.DeleteNode(TreeView.Selected);
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

procedure TFormMain.DoReturn(Text: string);
begin
  lbHeap.Items.Add(Text);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FFileLoader := TFileContentController.Create();
  FTreeController := TTreeController.Create(TreeView);
  FTreeController.OnReturn := DoReturn;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFileLoader.Free;
  FSettings.Free;
  FTreeController.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnlÑenterBtn.Top := (pnlÑenter.Height - pnlÑenterBtn.Height) div 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  InitSettings;
end;

procedure TFormMain.lbHeapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not ((Sender is TTreeView) and (Source is TListBox)) then
    Exit;
  Accept := true;
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

procedure TFormMain.SaveSettings;
begin
  FSettings.MainFormHeight := Self.ClientHeight;
  FSettings.MainFormWidth := Self.ClientWidth;
  FSettings.MainFormLeft := Self.Left;
  FSettings.MainFormTop := Self.Top;
  FSettings.HeapWidth := lbHeap.Width;
  FSettings.Save;
end;

procedure TFormMain.InitSettings;
var
  settingsFN: string;
begin
  settingsFN := ExtractFilePath(Application.ExeName) + '\settings.ini';
  FSettings := TSettingsController.Create(settingsFN);
  Self.ClientHeight := FSettings.MainFormHeight;
  Self.ClientWidth := FSettings.MainFormWidth;
  Self.Left := FSettings.MainFormLeft;
  Self.Top := FSettings.MainFormTop;
  lbHeap.Width := FSettings.HeapWidth;
end;

procedure TFormMain.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  index: integer;
  tmpNode: TTreeNode;
begin
  if not ((Source is TListBox) or ((Sender as TTreeView).Items.Count > 0)) then
    Exit;
  tmpNode := (Sender as TTreeView).GetNodeAt(X, Y);

  FTreeController.SelectNode(tmpNode);
  FTreeController.InsertNode(lbHeap.Items[lbHeap.ItemIndex]);

  index := lbHeap.ItemIndex;
  lbHeap.Items.Delete(lbHeap.ItemIndex);
  if index > 0 then
    index := index - 1;
  if index <= lbHeap.Count - 1 then
    lbHeap.ItemIndex := index;
end;

procedure TFormMain.TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not (Source is TListBox) then
    Exit;
  Accept := true;
end;

end.
