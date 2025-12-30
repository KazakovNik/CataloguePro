unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, Vcl.ButtonGroup,
  uMainFacade,
  uHeapController, uSettingsController, uTreeController;

type
  TFormMain = class(TForm)
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    TreeView: TTreeView;
    Splitter: TSplitter;
    lbHeap: TListBox;
    dlgSaveTextFile: TSaveDialog;
    pmTree: TPopupMenu;
    mniAddNode: TMenuItem;
    mniAddRootNode: TMenuItem;
    mniEditNode: TMenuItem;
    mniSaveTreeToFile: TMenuItem;
    N1: TMenuItem;
    pnl—enter: TPanel;
    pnl—enterBtn: TPanel;
    btnInsert: TButton;
    btnReturnBack: TButton;
    N2: TMenuItem;
    mniCollapseAll: TMenuItem;
    mniExpandAll: TMenuItem;
    pnlHeap: TPanel;
    pnlTree: TPanel;
    pnlTreeUnils: TPanel;
    pnlHeaputils: TPanel;
    btnLoadFile: TButton;
    mniDeleteNode: TMenuItem;
    mmMain: TMainMenu;
    N6: TMenuItem;
    mniLoadFile: TMenuItem;
    N8: TMenuItem;
    mniAddNode1: TMenuItem;
    mniAddRootNode1: TMenuItem;
    mniEditNode1: TMenuItem;
    mniDeleteNode1: TMenuItem;
    mniSaveTreeToFile1: TMenuItem;
    N15: TMenuItem;
    mniExpandAll1: TMenuItem;
    mniCollapseAll1: TMenuItem;
    N18: TMenuItem;
    mniSettings: TMenuItem;
    N20: TMenuItem;
    mniAbout: TMenuItem;
    mniDeleteAllNode: TMenuItem;
    statStatusBar: TStatusBar;
    mniRecentFiles: TMenuItem;
    mniLoadTree: TMenuItem;
    actlst: TActionList;
    actLoadFile: TAction;
    actAddNode: TAction;
    actAddRootNode: TAction;
    actEditNode: TAction;
    actSaveTreeToFile: TAction;
    actLoadTree: TAction;
    actInsertData: TAction;
    actReturnBack: TAction;
    actSettings: TAction;
    actExpandAll: TAction;
    actCollapseAll: TAction;
    actDeleteNode: TAction;
    actAbout: TAction;
    actDeleteAllNode: TAction;
    actRecentFiles: TAction;
    mniDeleteAllNode1: TMenuItem;
    actInsertDataRoot: TAction;
    btnInsertDataRoot: TButton;
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
    procedure actAddNodeUpdate(Sender: TObject);
    procedure actEditNodeUpdate(Sender: TObject);
    procedure actSaveTreeToFileUpdate(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actExpandAllUpdate(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actCollapseAllUpdate(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure TreeViewStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure actDeleteAllNodeUpdate(Sender: TObject);
    procedure actDeleteAllNodeExecute(Sender: TObject);
    procedure actRecentFilesUpdate(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure actRecentFilesExecute(Sender: TObject);
    procedure actLoadTreeExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure actInsertDataRootExecute(Sender: TObject);
    procedure actInsertDataRootUpdate(Sender: TObject);
  private
    FFacade: TMainFacade;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.actAboutExecute(Sender: TObject);
begin
  FFacade.ShowmAbout;
end;

procedure TFormMain.actAddNodeExecute(Sender: TObject);
begin
  FFacade.AddNode();
end;

procedure TFormMain.actAddNodeUpdate(Sender: TObject);
begin
  actAddNode.Enabled := Assigned(TreeView.Selected);
end;

procedure TFormMain.actAddRootNodeExecute(Sender: TObject);
begin
  FFacade.AddNodeRoot();
end;

procedure TFormMain.actCollapseAllExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.actCollapseAllUpdate(Sender: TObject);
begin
//
end;

procedure TFormMain.actDeleteAllNodeExecute(Sender: TObject);
begin
  FFacade.DeleteAllNode();
end;

procedure TFormMain.actDeleteAllNodeUpdate(Sender: TObject);
begin
  actDeleteAllNode.Enabled := FFacade.DeleteNodeEnabled();
end;

procedure TFormMain.actDeleteNodeExecute(Sender: TObject);
begin
  FFacade.DeleteCurrentNode();
end;

procedure TFormMain.actDeleteNodeUpdate(Sender: TObject);
begin
  actDeleteNode.Enabled := FFacade.DeleteNodeEnabled();
end;

procedure TFormMain.actEditNodeExecute(Sender: TObject);
begin
  FFacade.EditCurrentNode();
end;

procedure TFormMain.actEditNodeUpdate(Sender: TObject);
begin
//
end;

procedure TFormMain.actExpandAllExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.actExpandAllUpdate(Sender: TObject);
begin
//
end;

procedure TFormMain.actInsertDataExecute(Sender: TObject);
begin
  FFacade.InsertCurrentItem();
end;

procedure TFormMain.actInsertDataRootExecute(Sender: TObject);
begin
///
end;

procedure TFormMain.actInsertDataRootUpdate(Sender: TObject);
begin
///
end;

procedure TFormMain.actInsertDataUpdate(Sender: TObject);
begin
  actInsertData.Enabled := FFacade.InsertCurrentItemEnabled();
end;

procedure TFormMain.actReturnBackUpdate(Sender: TObject);
begin
  actReturnBack.Enabled := FFacade.ReturnBackCurrentItemEnabled();
end;

procedure TFormMain.actLoadFileExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := FFacade.FileOpenDirectory;
  if OpenDialog.Execute then
    FFacade.HeapLoadFile(OpenDialog.FileName);
end;

procedure TFormMain.actLoadTreeExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := FFacade.FileOpenDirectory;
  if OpenDialog.Execute then
    FFacade.LoadTreeFile(OpenDialog.FileName);
end;

procedure TFormMain.actRecentFilesExecute(Sender: TObject);
begin
  // Á‡„ÎÛ¯Í‡ ‰Îˇ ActionManager
end;

procedure TFormMain.actRecentFilesUpdate(Sender: TObject);
begin
  actRecentFiles.Enabled := FFacade.RecentFilesEnabled();
end;

procedure TFormMain.actReturnBackExecute(Sender: TObject);
begin
  FFacade.DeleteCurrentNode();
end;

procedure TFormMain.actSaveTreeToFileExecute(Sender: TObject);
begin
  dlgSaveTextFile.InitialDir := FFacade.FileSaveDirectory;
  if dlgSaveTextFile.Execute then
    FFacade.TreeSaveToFile(dlgSaveTextFile.FileName);
end;

procedure TFormMain.actSaveTreeToFileUpdate(Sender: TObject);
begin
//
end;

procedure TFormMain.actSettingsExecute(Sender: TObject);
begin
  FFacade.ShowmSettings;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFacade.SaveSettings();
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFacade.Free;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnl—enterBtn.Top := (pnl—enter.Height - pnl—enterBtn.Height) div 2;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FFacade := TMainFacade.Create(TreeView, lbHeap, statStatusBar);
  FFacade.InitSettings();
end;

procedure TFormMain.lbHeapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not ((Sender is TTreeView) and (Source is TListBox)) then
    Exit;
  Accept := true;
end;

procedure TFormMain.N6Click(Sender: TObject);
begin
  FFacade.UpdateSubmenuRecentFiles(mmMain, mniRecentFiles);
end;

procedure TFormMain.pmTreePopup(Sender: TObject);
var
  MousePosition, TreePos: TPoint;
  Node: TTreeNode;
begin
  GetCursorPos(MousePosition);
  TreePos := TreeView.ScreenToClient(MousePosition);
  Node := TreeView.GetNodeAt(TreePos.X, TreePos.Y);
  if Assigned(Node) then
    TreeView.Selected := Node;
end;

procedure TFormMain.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  FFacade.TreeDragDrop(Sender, Source, X, Y);
end;

procedure TFormMain.TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := FFacade.TreeDragOverAccept(Sender, Source, X, Y);
end;

procedure TFormMain.TreeViewStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
//  DragObject := TDragControlObject.Create(FFacade.GetDragControlTree());
end;

end.
