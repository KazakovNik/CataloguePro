unit uMainFacade;

interface

uses
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.Menus,
  uHeapController, uSettingsController, uTreeController, uRecentFilesController,
  uLogger, uILogger, uUserInfo;

type
  TMainFacade = class
  private
    FTree: TTreeView;
    FHeap: TListBox;
    FStatusBar: TStatusBar;
    FHeapController: THeapController;
    FSettingsController: TSettingsController;
    FTreeController: TTreeController;
    FRecentFilesController: TRecentFilesController;
    FLogger: ILogger;
    FUserInfo: TUserInfo;
  private
    function ShowGialogNewNode: string;
    function ShowGialogEditNode: string;
    function GetFileOpenDirectory: string;
    function GetFileSaveDirectory: string;
    procedure DoReturn(Text: string);
    procedure DoLoadFile(FileName: string);
    procedure DoUpdateRecentFiles(History: TStringList);
    procedure DoUpdateSettings(Sender: TSettingsController);
    procedure DoUpdateStatus;
    procedure OpenFileFromMenu(Sender: TObject);
  public
    constructor Create(Tree: TTreeView; Heap: TListBox; StatusBar: TStatusBar);
    destructor Destroy; override;

    procedure AddNode;
    procedure AddNodeRoot;
    procedure DeleteCurrentNode;
    procedure DeleteAllNode;
    procedure EditCurrentNode;
    procedure InsertCurrentItem;
    procedure HeapLoadFile(filename: string);
    procedure LoadTreeFile(filename: string);
    procedure TreeSaveToFile(filename: string);
    procedure SaveSettings;
    procedure InitSettings;
    procedure InsertTextIntoNode(Node: TTreeNode);
    procedure ShowmAbout;
    procedure ShowmSettings;
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure UpdateSubmenuRecentFiles(mmMain: TMainMenu; mniRecentFiles: TMenuItem);

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
  Vcl.Dialogs, System.SysUtils, Vcl.Forms, System.UITypes,
  uNodeModel, uAboutFacade, uSettingsFacade;

{ TMainFacade }

procedure TMainFacade.AddNode;
begin
  FTreeController.AddNode(FTree.Selected, ShowGialogNewNode);
  DoUpdateStatus;
end;

procedure TMainFacade.AddNodeRoot;
begin
  FTreeController.AddNode(nil, ShowGialogNewNode);
  DoUpdateStatus;
end;

procedure TMainFacade.DoUpdateSettings(Sender: TSettingsController);
begin
  FRecentFilesController.SetMaxCountFile(Sender.MaxCountFileHistopy);
end;

procedure TMainFacade.DoUpdateStatus;
begin
  FStatusBar.Panels[0].Text := FUserInfo.UserName;
  FStatusBar.Panels[1].Text := FUserInfo.ComputerName;
  if (FHeap.Items.Count = 0) and (FTreeController.CountNode = 0) then
  begin
    FStatusBar.Panels[2].Text := '';
    FStatusBar.Panels[3].Text := '';
  end
  else
  begin
    FStatusBar.Panels[2].Text := 'Строк в куче: ' + IntToStr(FHeap.Items.Count);
    FStatusBar.Panels[3].Text := 'Записей в дереве: ' + IntToStr(FTreeController.CountNode);
  end;
end;

constructor TMainFacade.Create(Tree: TTreeView; Heap: TListBox; StatusBar: TStatusBar);
var
  settingsFN: string;
begin
  FTree := Tree;
  FHeap := Heap;
  FStatusBar := StatusBar;

  FUserInfo := TUserInfo.Create;

  settingsFN := ExtractFilePath(Application.ExeName) + 'settings.ini';
  FSettingsController := TSettingsController.Create(settingsFN);
  FSettingsController.OnUpdate := DoUpdateSettings;

  FLogger := TLogger.Create(FSettingsController.LoggerFileName, FUserInfo);
  FLogger.AddInfo('Открытие программы');

  FRecentFilesController := TRecentFilesController.Create(FLogger);
  FRecentFilesController.OnUpdate := DoUpdateRecentFiles;
  FRecentFilesController.LoadHistory(FSettingsController.RecentFiles);

  FHeapController := THeapController.Create(FHeap, FLogger);
  FTreeController := TTreeController.Create(FTree, FLogger);
  FTreeController.OnReturn := DoReturn;

  DoUpdateStatus;
end;

destructor TMainFacade.Destroy;
begin
  FSettingsController.OnUpdate := nil;
  FRecentFilesController.OnUpdate := nil;
  FTreeController.OnReturn := nil;

  FHeapController.Free;
  FSettingsController.Free;
  FTreeController.Free;
  FRecentFilesController.Free;

  FLogger.AddInfo('Закрытие программы');
  FUserInfo.Free;

  inherited;
end;

procedure TMainFacade.DoUpdateRecentFiles(History: TStringList);
begin
  FSettingsController.RecentFiles := StringReplace(History.Text, #13#10, ';', [rfReplaceAll]);
end;

procedure TMainFacade.DoLoadFile(FileName: string);
begin
  FRecentFilesController.OpenFile(FileName);
  DoUpdateStatus;
end;

procedure TMainFacade.DoReturn(Text: string);
begin
  FHeapController.Add(Text);
  DoUpdateStatus;
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
  DoUpdateStatus;
end;

procedure TMainFacade.DeleteCurrentNode;
begin
  if Assigned(FTree.Selected) then
    FTreeController.DeleteNode(FTree.Selected);
  DoUpdateStatus;
end;

function TMainFacade.DeleteNodeEnabled: Boolean;
begin
  Result := (FTree.Items.Count > 0) and Assigned(FTree.Selected);
end;

procedure TMainFacade.EditCurrentNode;
begin
  if Assigned(FTree.Selected) then
    FTreeController.EditNode(FTree.Selected, ShowGialogEditNode());
  DoUpdateStatus;
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
    DoLoadFile(filename);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  DoUpdateStatus;
end;

procedure TMainFacade.InsertCurrentItem;
begin
  FLogger.AddInfo('Переносим в дерево текущюю запись из кучи');
  FTreeController.InsertItem(FHeapController.GetCurrentItem());
  FHeapController.DeleteCurrent;
  DoUpdateStatus;
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
  FLogger.AddInfo('Переносим в дерево запись из кучи: ' + Node.Text);
  FTreeController.SelectNode(Node);
  FTreeController.InsertItem(FHeapController.GetCurrentItem());
  FHeapController.DeleteCurrent;
  DoUpdateStatus;
end;

procedure TMainFacade.LoadTreeFile(filename: string);
begin
  FSettingsController.FileOpenDirectory := ExtractFilePath(filename);
  try
    FTreeController.LoadTreeFile(filename);
    FTreeController.ExpandAll();
    DoLoadFile(filename);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  DoUpdateStatus;
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
  DoUpdateStatus;
end;

procedure TMainFacade.UpdateSubmenuRecentFiles(mmMain: TMainMenu;
  mniRecentFiles: TMenuItem);
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
    item.OnClick := OpenFileFromMenu;
    mniRecentFiles.Add(item);
  end;
end;

procedure TMainFacade.OpenFileFromMenu(Sender: TObject);
var
  FileName: string;
begin
  //TODO: невнятная проблема с TMenuItem, в некоторых случаях добавляется лишний символ
  FileName := TMenuItem(Sender).Caption;
//  Delete(FileName, 1, 1);
  HeapLoadFile(FileName);
end;

procedure TMainFacade.InitSettings;
begin
  Application.MainForm.ClientHeight := FSettingsController.MainFormHeight;
  Application.MainForm.ClientWidth := FSettingsController.MainFormWidth;
  Application.MainForm.Left := FSettingsController.MainFormLeft;
  Application.MainForm.Top := FSettingsController.MainFormTop;
  FHeap.Width := FSettingsController.HeapWidth;
  Application.MainForm.WindowState := TWindowState(FSettingsController.WindowState);
end;

procedure TMainFacade.SaveSettings;
begin
  case Application.MainForm.WindowState of
    wsNormal:
      begin
        FSettingsController.MainFormHeight := Application.MainForm.ClientHeight;
        FSettingsController.MainFormWidth := Application.MainForm.ClientWidth;
        FSettingsController.MainFormLeft := Application.MainForm.Left;
        FSettingsController.MainFormTop := Application.MainForm.Top;
      end;
  end;
  FSettingsController.WindowState := Ord(Application.MainForm.WindowState);
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
  DoUpdateStatus;
end;

function TMainFacade.ShowGialogEditNode: string;
begin
  Result := InputBox('Редактирование', 'Измените название:', FTree.Selected.Text);
end;

function TMainFacade.ShowGialogNewNode: string;
begin
  Result :=
    InputBox('Новый элемент', 'Название:',
      Format('Новая элемент (%d)', [FTreeController.CountNode]));
end;

procedure TMainFacade.ShowmAbout;
var
  Facade: TAboutFacade;
begin
  Facade := TAboutFacade.Create;
  try
    Facade.ShowModal;
  finally
    Facade.Free;
  end;
end;

procedure TMainFacade.ShowmSettings;
var
  Facade: TSettingsFacade;
begin
  Facade := TSettingsFacade.Create(FSettingsController);
  try
    Facade.ShowModal;
  finally
    Facade.Free;
  end;
end;

end.
