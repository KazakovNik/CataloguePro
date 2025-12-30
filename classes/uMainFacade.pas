unit uMainFacade;

interface

uses
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.Menus,
  uHeapController, uSettingsController, uTreeController, uRecentFilesController;

type
  TMainFacade = class
  private
    FTree: TTreeView;
    FHeap: TListBox;
    FHeapController: THeapController;
    FSettingsController: TSettingsController;
    FTreeController: TTreeController;
    FRecentFilesController: TRecentFilesController;
  private
    function ShowGialogNewNode: string;
    function ShowGialogEditNode: string;
    function GetFileOpenDirectory: string;
    function GetFileSaveDirectory: string;
    procedure DoReturn(Text: string);
    procedure DoLoadFile(FileName: string);
    procedure DoUpdateRecentFiles(History: TStringList);
    procedure DoUpdateSettings(Sender: TSettingsController);
    procedure OpenFileFromMenu(Sender: TObject);
  public
    constructor Create(Tree: TTreeView; Heap: TListBox);
    destructor Destroy; override;

    procedure AddNode;
    procedure AddNodeRoot;
    procedure DeleteCurrentNode;
    procedure DeleteAllNode;
    procedure EditCurrentNode;
    procedure InsertCurrentItem;
    procedure HeapLoadFile(filename: string);
    procedure TreeSaveToFile(filename: string);
    procedure SaveSettings;
    procedure InitSettings;
    procedure InsertTextIntoNode(Node: TTreeNode);
    procedure ShowmAbout;
    procedure ShowmSettings;
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure UpdateSubmenuRecentFiles(mmMain: TMainMenu; mniRecentFiles: TMenuItem);

    function CountNode: integer;
    function DeleteNodeEnabled: Boolean;
    function InsertCurrentItemEnabled: Boolean;
    function ReturnBackCurrentItemEnabled: Boolean;
    function TreeDragOverAccept(Sender, Source: TObject; X, Y: Integer): Boolean;
//    function GetDragControlTree: TControl;
    function RecentFilesEnabled: Boolean;
  public
    property FileOpenDirectory: string read GetFileOpenDirectory;
    property FileSaveDirectory: string read GetFileSaveDirectory;
  end;

implementation

uses
  Vcl.Dialogs, System.SysUtils, Vcl.Forms, uNodeModel;

{ TMainFacade }

procedure TMainFacade.AddNode;
begin
  FTreeController.AddNode(FTree.Selected, ShowGialogNewNode);
end;

procedure TMainFacade.AddNodeRoot;
begin
  FTreeController.AddNode(nil, ShowGialogNewNode);
end;

function TMainFacade.CountNode: integer;
begin
  Result := FTree.Items.Count;
end;

procedure TMainFacade.DoUpdateSettings(Sender: TSettingsController);
begin
  FRecentFilesController.SetMaxCountFile(Sender.MaxCountFileHistopy);
end;

constructor TMainFacade.Create(Tree: TTreeView; Heap: TListBox);
var
  settingsFN: string;
begin
  FTree := Tree;
  FHeap := Heap;

  settingsFN := ExtractFilePath(Application.ExeName) + '\settings.ini';
  FSettingsController := TSettingsController.Create(settingsFN);
  FSettingsController.OnUpdate := DoUpdateSettings;

  FRecentFilesController := TRecentFilesController.Create();
  FRecentFilesController.OnUpdate := DoUpdateRecentFiles;
  FRecentFilesController.LoadHistory(FSettingsController.RecentFiles);

  FHeapController := THeapController.Create(FHeap);
  FHeapController.OnLoadFile := DoLoadFile;
  FTreeController := TTreeController.Create(FTree);
  FTreeController.OnReturn := DoReturn;
end;

destructor TMainFacade.Destroy;
begin
  FSettingsController.OnUpdate := nil;
  FRecentFilesController.OnUpdate := nil;
  FHeapController.OnLoadFile := nil;
  FTreeController.OnReturn := nil;

  FHeapController.Free;
  FSettingsController.Free;
  FTreeController.Free;
  FRecentFilesController.Free;

  inherited;
end;

procedure TMainFacade.DoUpdateRecentFiles(History: TStringList);
begin
  FSettingsController.RecentFiles := StringReplace(History.Text, #13#10, ';', [rfReplaceAll]);
end;

procedure TMainFacade.DoLoadFile(FileName: string);
begin
  FRecentFilesController.OpenFile(FileName);
end;

procedure TMainFacade.DoReturn(Text: string);
begin
  FHeap.Items.Add(Text);
end;

procedure TMainFacade.DeleteAllNode;
var
  I: Integer;
  RootNode: TTreeNode;
begin
  for I := FTree.Items.Count - 1 downto 0 do
  begin
    RootNode := FTree.Items[I];
    if not Assigned(RootNode.Parent) then
      FTreeController.DeleteNode(RootNode);
  end;
end;

procedure TMainFacade.DeleteCurrentNode;
begin
  if Assigned(FTree.Selected) then
    FTreeController.DeleteNode(FTree.Selected);
end;

function TMainFacade.DeleteNodeEnabled: Boolean;
begin
  Result := (FTree.Items.Count > 0) and Assigned(FTree.Selected);
end;

procedure TMainFacade.EditCurrentNode;
begin
  if Assigned(FTree.Selected) then
    FTreeController.EditNode(FTree.Selected, ShowGialogEditNode());
end;

//function TMainFacade.GetDragControlTree: TControl;
//begin
//  Result := FTreeController.GetDragControlTree();
//end;

function TMainFacade.GetFileOpenDirectory: string;
begin
  Result := FSettingsController.FileOpenDirectory;
end;

function TMainFacade.GetFileSaveDirectory: string;
begin
  Result := FSettingsController.FileSaveDirectory;
end;

procedure TMainFacade.HeapLoadFile(filename: string);
begin
  FSettingsController.FileOpenDirectory := ExtractFilePath(filename);
  try
    FHeapController.LoadFile(filename);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMainFacade.InitSettings;
begin
  Application.MainForm.ClientHeight := FSettingsController.MainFormHeight;
  Application.MainForm.ClientWidth := FSettingsController.MainFormWidth;
  Application.MainForm.Left := FSettingsController.MainFormLeft;
  Application.MainForm.Top := FSettingsController.MainFormTop;
  FHeap.Width := FSettingsController.HeapWidth;
end;

procedure TMainFacade.InsertCurrentItem;
begin
  FTreeController.InsertItem(FHeap.Items[FHeap.ItemIndex]);
  FHeapController.Delete(FHeap.ItemIndex);
end;

function TMainFacade.InsertCurrentItemEnabled: Boolean;
begin
  Result :=
    (FHeap.ItemIndex <> -1) and
    (FTree.Items.Count > 0) and
    Assigned(FTree.Selected) and
    FTreeController.SelectedIsFolder(FTree.Selected);
end;

procedure TMainFacade.InsertTextIntoNode(Node: TTreeNode);
begin
  FTreeController.SelectNode(Node);
  FTreeController.InsertItem(FHeap.Items[FHeap.ItemIndex]);
  FHeapController.Delete(FHeap.ItemIndex);
end;

function TMainFacade.RecentFilesEnabled: Boolean;
begin
  Result := FRecentFilesController.RecentFilesCount() > 0;
end;

function TMainFacade.ReturnBackCurrentItemEnabled: Boolean;
begin
  Result :=
    (FTree.Items.Count > 0) and
    Assigned(FTree.Selected) and
    (FTreeController.SelectedIsItem(FTree.Selected) or
    (FTreeController.SelectedIsFolder(FTree.Selected) and
     FTree.Selected.HasChildren));
end;

procedure TMainFacade.TreeSaveToFile(filename: string);
begin
  FSettingsController.FileSaveDirectory := ExtractFilePath(filename);
  FTreeController.SaveToFile(ChangeFileExt(filename, '.txt'));
end;

procedure TMainFacade.UpdateSubmenuRecentFiles(mmMain: TMainMenu; mniRecentFiles: TMenuItem);
var
  i: Integer;
  item: TMenuItem;
begin
  mniRecentFiles.Clear;

  for i := 0 to FRecentFilesController.RecentFilesCount() - 1 do
  begin
    item := TMenuItem.Create(Application.MainForm);
    item.Caption := FRecentFilesController.GetFileHistory(i);
    item.Visible := True;
    item.Tag := i;
    item.AutoCheck := True;
    item.OnClick := OpenFileFromMenu; // назначаем событие клика
    mniRecentFiles.Add(item);
  end;
end;

procedure TMainFacade.OpenFileFromMenu(Sender: TObject);
var
  FileName: string;
begin
  FileName := TMenuItem(Sender).Caption;
  Delete(FileName, 1, 1);
  HeapLoadFile(FileName);
end;

procedure TMainFacade.SaveSettings;
begin
  FSettingsController.MainFormHeight := Application.MainForm.ClientHeight;
  FSettingsController.MainFormWidth := Application.MainForm.ClientWidth;
  FSettingsController.MainFormLeft := Application.MainForm.Left;
  FSettingsController.MainFormTop := Application.MainForm.Top;
  FSettingsController.HeapWidth := FHeap.Width;
  FSettingsController.Save;
end;

function TMainFacade.TreeDragOverAccept(Sender, Source: TObject; X, Y: Integer): Boolean;
var
  tmpNode: TTreeNode;
begin
//  if (Source is TDragControlObject)
//    and ((Source as TDragControlObject).Control.ClassName = 'TModeDragNode') then
//      Exit(True);

  if not ((Source is TListBox) and (Sender is TTreeView)) then
    Exit(False);
  tmpNode := (Sender as TTreeView).GetNodeAt(X, Y);

  Result := Assigned(tmpNode) and FTreeController.SelectedIsFolder(tmpNode);
end;

procedure TMainFacade.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DstNode: TTreeNode;
//  modelcontrol: TControl;
//  model: TModeDragNode;
//SrcNode: TTreeNode;
begin
//  if (Source is TDragControlObject)
//    and ((Source as TDragControlObject).Control.ClassName = 'TModeDragNode') then
//  begin
//    modelcontrol := (Source as TDragControlObject).Control;
//    try
//      model := TModeDragNode(modelcontrol);
//      SrcNode := model.Node;
//      DstNode := (Sender as TTreeView).GetNodeAt(X, Y);
//
//      if DstNode = nil then
//        SrcNode.MoveTo(nil, naAdd)
//      else
//      begin
//        if FTreeController.SelectedIsFolder(DstNode) then
//          SrcNode.MoveTo(DstNode, naAdd)
//        else
//          SrcNode.MoveTo(DstNode.Parent, naAdd);
//      end;
//    finally
//      modelcontrol.Free;
//      Source.Free;
//    end;
//    Exit;
//  end;

  if not ((Source is TListBox) or ((Sender as TTreeView).Items.Count > 0)) then
    Exit;
  DstNode := (Sender as TTreeView).GetNodeAt(X, Y);
  if Assigned(DstNode) then
    InsertTextIntoNode(DstNode);
end;


function TMainFacade.ShowGialogEditNode: string;
begin
  Result := InputBox('Редактирование', 'Измените название:', FTree.Selected.Text);
end;

function TMainFacade.ShowGialogNewNode: string;
begin
  Result :=
    InputBox('Новый элемент', 'Название:',
      Format('Новая элемент (%d)', [CountNode]));
end;

procedure TMainFacade.ShowmAbout;
begin

end;

procedure TMainFacade.ShowmSettings;
begin

end;

end.
