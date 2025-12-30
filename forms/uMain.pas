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
    actEditNode1: TMenuItem;
    actSaveTreeToFile1: TMenuItem;
    N1: TMenuItem;
    actInsertData: TAction;
    actReturnBack: TAction;
    pnl—enter: TPanel;
    pnl—enterBtn: TPanel;
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
    actDeleteAllNode: TAction;
    N22: TMenuItem;
    statStatusBar: TStatusBar;
    mniRecentFiles: TMenuItem;
    actRecentFiles: TAction;
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FFacade := TMainFacade.Create(TreeView, lbHeap);
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
