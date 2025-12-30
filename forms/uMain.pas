unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, Vcl.ButtonGroup,
  uHeapController, uSettingsController, uTreeController;

type
  TFormMain = class(TForm)
    ImageList1: TImageList;
    ActionManager1: TActionManager;
    actLoadFile: TAction;
    OpenDialog: TOpenDialog;
    TreeView: TTreeView;
    Splitter: TSplitter;
    lbHeap: TListBox;
    actAddNode: TAction;
    actAddRootNode: TAction;
    actEditNode: TAction;
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
    actSettings: TAction;
    actExpandAll: TAction;
    actCollapseAll: TAction;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    pnlHeap: TPanel;
    pnlTree: TPanel;
    pnlTreeUnils: TPanel;
    pnlHeaputils: TPanel;
    btnLoadFile: TButton;
    actDeleteNode: TAction;
    N5: TMenuItem;
    mmMain: TMainMenu;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    actAbout: TAction;
    N21: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actLoadFileExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAddNodeExecute(Sender: TObject);
    procedure actAddRootNodeExecute(Sender: TObject);
    procedure actEditNodeExecute(Sender: TObject);
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
    procedure actDeleteNodeUpdate(Sender: TObject);
    procedure actDeleteNodeExecute(Sender: TObject);
  private
    FHeap: THeapController;
    FSettings: TSettingsController;
    FTreeController: TTreeController;
  private
    procedure DoReturn(Text: string);
    procedure InitSettings;
    procedure SaveSettings;
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

procedure TFormMain.actDeleteNodeUpdate(Sender: TObject);
begin
  actDeleteNode.Enabled :=
    (TreeView.Items.Count > 0) and
    Assigned(TreeView.Selected) and
    FTreeController.SelectedIsFolder(TreeView.Selected) and
    (not TreeView.Selected.HasChildren);
end;

procedure TFormMain.actEditNodeExecute(Sender: TObject);
begin
  if Assigned(TreeView.Selected) then
    FTreeController.EditNode(TreeView.Selected,
      InputBox('Редактирование', 'Измените название:', TreeView.Selected.Text));
end;

procedure TFormMain.actInsertDataExecute(Sender: TObject);
begin
  FTreeController.InsertItem(lbHeap.Items[lbHeap.ItemIndex]);
  FHeap.Delete(lbHeap.ItemIndex);
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
    (FTreeController.SelectedIsFolder(TreeView.Selected) and
     TreeView.Selected.HasChildren));
end;

procedure TFormMain.actLoadFileExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := FSettings.FileOpenDirectory;
  if not OpenDialog.Execute then
    Exit;

  FSettings.FileOpenDirectory := ExtractFilePath(OpenDialog.FileName);
  try
    FHeap.LoadFile(OpenDialog.FileName);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFormMain.actReturnBackExecute(Sender: TObject);
begin
  FTreeController.DeleteNode(TreeView.Selected);
end;

procedure TFormMain.actSaveTreeToFileExecute(Sender: TObject);
begin
  dlgSaveTextFile.InitialDir := FSettings.FileSaveDirectory;
  if not dlgSaveTextFile.Execute then
    Exit;

  FSettings.FileSaveDirectory := ExtractFilePath(dlgSaveTextFile.FileName);
  FTreeController.SaveToFile(ChangeFileExt(dlgSaveTextFile.FileName, '.txt'));
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
  FHeap := THeapController.Create(lbHeap);
  FTreeController := TTreeController.Create(TreeView);
  FTreeController.OnReturn := DoReturn;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FHeap.Free;
  FSettings.Free;
  FTreeController.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnlСenterBtn.Top := (pnlСenter.Height - pnlСenterBtn.Height) div 2;
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
  tmpNode: TTreeNode;
begin
  if not ((Source is TListBox) or ((Sender as TTreeView).Items.Count > 0)) then
    Exit;

  tmpNode := (Sender as TTreeView).GetNodeAt(X, Y);
  FTreeController.SelectNode(tmpNode);
  FTreeController.InsertItem(lbHeap.Items[lbHeap.ItemIndex]);
  FHeap.Delete(lbHeap.ItemIndex);
end;

procedure TFormMain.TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not (Source is TListBox) then
    Exit;
  Accept := true;
end;

end.
