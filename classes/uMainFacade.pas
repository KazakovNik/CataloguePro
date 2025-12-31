unit uMainFacade;

interface

uses
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.Menus, System.SysUtils,
  uHeapController, uSettingsController, uTreeController, uRecentFilesController,
  uLogger, uILogger, uUserInfo, uDialogFacade;

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
    FDialogFacade: TDialogFacade;
  private
    function GetFileOpenDirectory: string;
    function GetFileSaveDirectory: string;
    procedure DoReturn(aText: string);
    procedure DoLoadFile(aFileName: string);
    procedure DoUpdateRecentFiles(aHistory: TStringList);
    procedure DoUpdateSettings(aSender: TSettingsController);
    procedure DoUpdateStatus;
    procedure OpenFileFromMenu(aSender: TObject);
    procedure MoveNode(aTargetNode, aSourceNode: TTreeNode);
    procedure DoOpenPatch(Patch: string);
    procedure DoException(aSender: TObject; E: Exception);
  public
    constructor Create(aTree: TTreeView; aHeap: TListBox; aStatusBar: TStatusBar);
    destructor Destroy; override;

    procedure AddNode;
    procedure AddNodeRoot;
    procedure DeleteCurrentNode;
    procedure DeleteAllNode;
    procedure EditCurrentNode;
    procedure InsertCurrentItem;
    procedure InsertCurrentItemToRoot;
    procedure HeapLoadFile(aFileName: string);
    procedure LoadTreeFile(aFileName: string);
    procedure TreeSaveToFile(aFileName: string);
    procedure SaveSettings;
    procedure InitSettings;
    procedure InsertTextIntoNode(aNode: TTreeNode);
    procedure ShowmAbout;
    procedure ShowmSettings;
    procedure TreeDragDrop(aSender, aSource: TObject; X, Y: Integer);
    procedure UpdateSubmenuRecentFiles(aMainMenu: TMainMenu; aHistoryMenu: TMenuItem);
    procedure ExpandAll;
    procedure CollapseAll;
    procedure SelectNodeUnderMouse;

    function DeleteNodeEnabled: Boolean;
    function InsertCurrentItemEnabled: Boolean;
    function ReturnBackCurrentItemEnabled: Boolean;
    function TreeDragOverAccept(aSender, aSource: TObject; X, Y: Integer): Boolean;
    function RecentFilesEnabled: Boolean;

    function ClearTreeEnabled: Boolean;
    function ClearHeapEnabled: Boolean;
    procedure ClearHeap;
    procedure ClearTree;
  public
    property FileOpenDirectory: string read GetFileOpenDirectory;
    property FileSaveDirectory: string read GetFileSaveDirectory;
  end;

implementation

uses
  Vcl.Dialogs, Vcl.Forms, System.UITypes, System.Types, Winapi.Windows,
  uAboutFacade, uSettingsFacade, uObjectOpener, uRsControls;

const
  cImgFile = 13;

{ TMainFacade }

procedure TMainFacade.AddNode;
var
  vText: string;
begin
  if FDialogFacade.CreateInputDialog(resMainAddNodeCaption, resMainAddNodeTitle,
    Format(resMainAddNodeText, [FTreeController.CountNode]), vText) then
  begin
    FTreeController.AddNode(FTree.Selected, vText);
    DoUpdateStatus;
  end;
end;

procedure TMainFacade.AddNodeRoot;
var
  vText: string;
begin
  if FDialogFacade.CreateInputDialog(resMainAddNodeCaption, resMainAddNodeTitle,
    Format(resMainAddNodeText, [FTreeController.CountNode]), vText) then
  begin
    FTreeController.AddNode(nil, vText);
    DoUpdateStatus;
  end;
end;

procedure TMainFacade.DoUpdateSettings(aSender: TSettingsController);
begin
  FRecentFilesController.SetMaxCountFile(aSender.MaxCountFileHistopy);
end;

procedure TMainFacade.DoUpdateStatus;
begin
  FStatusBar.Panels[0].Text := FUserInfo.UserName;
  FStatusBar.Panels[1].Text := FUserInfo.ComputerName;
  if (FHeap.Items.Count = 0) and (FTreeController.CountNode = 0) then
  begin
    FStatusBar.Panels[2].Text := EmptyStr;
    FStatusBar.Panels[3].Text := EmptyStr;
  end
  else
  begin
    FStatusBar.Panels[2].Text := resMainStatusHeap + IntToStr(FHeap.Items.Count);
    FStatusBar.Panels[3].Text := resMainStatusTree + IntToStr(FTreeController.CountNode);
  end;
end;

procedure TMainFacade.ClearHeap;
begin
  FHeapController.Clear;
end;

procedure TMainFacade.ClearTree;
begin
  if FDialogFacade.MessageInfoDialogOkCancel(resMainClearTreeCaption, resMainClearTreeTitle) then
    FTreeController.Clear;
end;

function TMainFacade.ClearHeapEnabled: Boolean;
begin
  Result := not FHeapController.IsEmpty;
end;

function TMainFacade.ClearTreeEnabled: Boolean;
begin
  Result := not FTreeController.IsEmpty;
end;

procedure TMainFacade.CollapseAll;
begin
  FTreeController.CollapseAll;
end;

constructor TMainFacade.Create(aTree: TTreeView; aHeap: TListBox; aStatusBar: TStatusBar);
var
  vSettingsFN: string;
begin
  Application.OnException := DoException;

  FTree := aTree;
  FHeap := aHeap;
  FStatusBar := aStatusBar;

  FUserInfo := TUserInfo.Create;
  FDialogFacade := TDialogFacade.Create;

  vSettingsFN := ExtractFilePath(Application.ExeName) + cSettingsFileName;
  FSettingsController := TSettingsController.Create(vSettingsFN);
  FSettingsController.OnUpdate := DoUpdateSettings;

  FLogger := TLogger.Create(FSettingsController.LoggerFileName, FUserInfo);
  FLogger.AddInfo(resMainOpenApp);

  FRecentFilesController := TRecentFilesController.Create(FLogger);
  FRecentFilesController.OnUpdate := DoUpdateRecentFiles;
  FRecentFilesController.LoadHistory(FSettingsController.RecentFiles);

  FHeapController := THeapController.Create(FHeap, FLogger);
  FTreeController := TTreeController.Create(FTree, FLogger);
  FTreeController.OnReturn := DoReturn;
  DoUpdateStatus;
end;

procedure TMainFacade.DoException(aSender: TObject; E: Exception);
begin
  if Assigned(FLogger) then
    FLogger.AddError(e.Message);

  if Assigned(FDialogFacade) then
    FDialogFacade.MessageError(e.Message);
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

  FLogger.AddInfo(resMainCloseApp);
  FUserInfo.Free;
  FDialogFacade.Free;

  inherited;
end;

procedure TMainFacade.DoUpdateRecentFiles(aHistory: TStringList);
begin
  FSettingsController.RecentFiles := StringReplace(aHistory.Text, #13#10, ';', [rfReplaceAll]);
end;

procedure TMainFacade.DoLoadFile(aFileName: string);
begin
  FRecentFilesController.OpenFile(aFileName);
  DoUpdateStatus;
end;

procedure TMainFacade.DoReturn(aText: string);
begin
  FHeapController.Add(aText);
  DoUpdateStatus;
end;

procedure TMainFacade.DeleteAllNode;
begin
  FLogger.AddInfo(resMainDeleteAllNodeLog);
  FTreeController.DeleteAllNode();
  DoUpdateStatus;
end;

procedure TMainFacade.DeleteCurrentNode;
begin
  FLogger.AddInfo(resMainDeleteCurrentNodeLog);
  if Assigned(FTree.Selected) then
    FTreeController.DeleteNode(FTree.Selected);
  DoUpdateStatus;
end;

function TMainFacade.DeleteNodeEnabled: Boolean;
begin
  Result := (FTree.Items.Count > 0) and Assigned(FTree.Selected);
end;

procedure TMainFacade.EditCurrentNode;
var
  vText: string;
begin
  if not Assigned(FTree.Selected) then
    Exit;

  if FDialogFacade.CreateInputDialog(resMainEditCurrentnodeTitle, resMainEditCurrentnodeCaption,
    FTree.Selected.Text, vText) then
  begin
    FTreeController.EditNode(FTree.Selected, vText);
    DoUpdateStatus;
  end;
end;

procedure TMainFacade.ExpandAll;
begin
  FTreeController.ExpandAll;
end;

function TMainFacade.GetFileOpenDirectory: string;
begin
  Result := FSettingsController.FileOpenDirectory;
end;

function TMainFacade.GetFileSaveDirectory: string;
begin
  Result := FSettingsController.FileSaveDirectory;
end;

procedure TMainFacade.HeapLoadFile(aFileName: string);
begin
  if not FileExists(aFileName) then
  begin
    FLogger.AddError(resMainFailedLoadFile + aFileName);
    if FDialogFacade.MessageInfoDialogOkCancel(resMainFileNotFoundDelete, resMainFileNotFoundDeleteTitle) then
      FRecentFilesController.DeleteByName(aFileName);

    Exit;
  end;

  FSettingsController.FileOpenDirectory := ExtractFilePath(aFileName);
  try
    FHeapController.LoadFile(aFileName);
    DoLoadFile(aFileName);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  DoUpdateStatus;
end;

procedure TMainFacade.InsertCurrentItem;
begin
  FLogger.AddInfo(resMainInserCurrentItem);
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

procedure TMainFacade.InsertCurrentItemToRoot;
begin
  FLogger.AddInfo(resMainInserCurrentItemRoot);
  FTreeController.InsertItemToRoot(FHeapController.GetCurrentItem());
  FHeapController.DeleteCurrent;
  DoUpdateStatus;
end;

procedure TMainFacade.InsertTextIntoNode(aNode: TTreeNode);
begin
  FLogger.AddInfo(resMainInserCurrentItemLog + aNode.Text);
  FTreeController.SelectNode(aNode);
  FTreeController.InsertItem(FHeapController.GetCurrentItem());
  FHeapController.DeleteCurrent;
  DoUpdateStatus;
end;

procedure TMainFacade.LoadTreeFile(aFileName: string);
begin
  FSettingsController.FileOpenDirectory := ExtractFilePath(aFileName);
  try
    FTreeController.LoadTreeFile(aFileName);
    FTreeController.ExpandAll();
    DoLoadFile(aFileName);
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

procedure TMainFacade.TreeSaveToFile(aFileName: string);
var
  vFileName: string;
begin
  FSettingsController.FileSaveDirectory := ExtractFilePath(aFileName);
  vFileName := ChangeFileExt(aFileName, '.txt');
  if FileExists(vFileName)
    and (not FDialogFacade.MessageInfoDialogOkCancel(
        resMainTreeSaveToFileReRecord, resMainTreeSaveToFileReRecordTitle)) then
      Exit;

  FTreeController.SaveToFile(vFileName);
  DoUpdateStatus;
end;

procedure TMainFacade.UpdateSubmenuRecentFiles(aMainMenu: TMainMenu;
  aHistoryMenu: TMenuItem);
var
  i: Integer;
  vItem: TMenuItem;
begin
  aHistoryMenu.Clear;
  for i := 0 to FRecentFilesController.RecentFilesCount() - 1 do
  begin
    vItem := TMenuItem.Create(Application.MainForm);
    vItem.AutoHotkeys := TMenuItemAutoFlag.maManual;
    vItem.ShortCut := 0;
    vItem.Caption := FRecentFilesController.GetFileHistory(i);
    vItem.Visible := True;
    vItem.Tag := i;
    vItem.OnClick := OpenFileFromMenu;
    vItem.ImageIndex := cImgFile;

    aHistoryMenu.Add(vItem);
  end;
end;

procedure TMainFacade.OpenFileFromMenu(aSender: TObject);
begin
  HeapLoadFile(FRecentFilesController.GetFileHistory(TMenuItem(aSender).Tag));
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

procedure TMainFacade.SelectNodeUnderMouse;
var
  vMousePosition, vTreePos: TPoint;
  vNode: TTreeNode;
begin
  GetCursorPos(vMousePosition);
  vTreePos := FTree.ScreenToClient(vMousePosition);
  vNode := FTree.GetNodeAt(vTreePos.X, vTreePos.Y);
  FTreeController.SelectNode(vNode);
end;

function TMainFacade.TreeDragOverAccept(aSender, aSource: TObject; X, Y: Integer): Boolean;
var
  vNode: TTreeNode;
begin
  if (aSource is TTreeView) then
    Exit(True);

  if not ((aSource is TListBox) and (aSender is TTreeView)) then
    Exit(False);
  vNode := (aSender as TTreeView).GetNodeAt(X, Y);

  Result := Assigned(vNode) and FTreeController.SelectedIsFolder(vNode);
end;

procedure TMainFacade.MoveNode(aTargetNode, aSourceNode : TTreeNode);
var
  vNode : TTreeNode;
  i : Integer;
begin
  with FTree do
  begin
    vNode := FTreeController.CloneItem(aSourceNode, aTargetNode);

    for i := 0 to aSourceNode.Count - 1 do
      MoveNode(vNode, aSourceNode.Item[i]);
  end;
end;

procedure TMainFacade.TreeDragDrop(aSender, aSource: TObject; X, Y: Integer);
var
  vTargetNode, vSourceNode : TTreeNode;
begin
  if (aSource is TTreeView) then
  begin
    with FTree do
    begin
      vTargetNode := GetNodeAt(X, Y);

      if FTreeController.SelectedIsItem(vTargetNode) then
      begin
        if vTargetNode.Parent = nil then
          vTargetNode := nil
        else
          vTargetNode := vTargetNode.Parent;
      end;

      vSourceNode := Selected;
      MoveNode(vTargetNode, vSourceNode);
      vSourceNode.Free;
    end;
    Exit;
  end;

  if not ((aSource is TListBox) or ((aSender as TTreeView).Items.Count > 0)) then
    Exit;
  vTargetNode := (aSender as TTreeView).GetNodeAt(X, Y);
  if Assigned(vTargetNode) then
    InsertTextIntoNode(vTargetNode);
  DoUpdateStatus;
end;

procedure TMainFacade.ShowmAbout;
var
  vFacade: TAboutFacade;
begin
  vFacade := TAboutFacade.Create;
  try
    vFacade.ShowModal;
  finally
    vFacade.Free;
  end;
end;

procedure TMainFacade.ShowmSettings;
var
  vFacade: TSettingsFacade;
begin
  vFacade := TSettingsFacade.Create(FSettingsController);
  try
    vFacade.OnOpen := DoOpenPatch;
    vFacade.ShowModal;
  finally
    vFacade.Free;
  end;
end;

procedure TMainFacade.DoOpenPatch(Patch: string);
begin
  if FileExists(Patch) then
  begin
    FLogger.AddInfo(resMainSettingsloadFile + patch);
    TObjectOpener.OpenFile(Patch);
  end
  else
  begin
    FLogger.AddInfo(resMainSettingsLoadFolder + patch);
    TObjectOpener.OpenFolder(Patch);
  end;
end;

end.
