unit uMainFacade;

interface

uses
  Vcl.ComCtrls, Vcl.StdCtrls,
  uHeapController, uSettingsController, uTreeController;

type
  TMainFacade = class
  private
    FTree: TTreeView;
    FHeap: TListBox;
    FHeapController: THeapController;
    FSettingsController: TSettingsController;
    FTreeController: TTreeController;
  private
    FFileSaveDirectory: string;

    function ShowGialogNewNode: string;
    function ShowGialogEditNode: string;
    function GetFileOpenDirectory: string;
    function GetFileSaveDirectory: string;
    procedure DoReturn(Text: string);
  public
    constructor Create(Tree: TTreeView; Heap: TListBox);
    destructor Destroy; override;

    procedure AddNode;
    procedure AddNodeRoot;
    procedure DeleteCurrentNode;
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

    function CountNode: integer;
    function DeleteNodeEnabled: Boolean;
    function InsertCurrentItemEnabled: Boolean;
    function ReturnBackCurrentItemEnabled: Boolean;
    function TreeDragOverAccept(Sender, Source: TObject; X, Y: Integer): Boolean;
  public
    property FileOpenDirectory: string read GetFileOpenDirectory;
    property FileSaveDirectory: string read GetFileSaveDirectory;
  end;

implementation

uses
  Vcl.Dialogs, System.SysUtils, Vcl.Forms;

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

constructor TMainFacade.Create(Tree: TTreeView; Heap: TListBox);
var
  settingsFN: string;
begin
  FTree := Tree;
  FHeap := Heap;

  settingsFN := ExtractFilePath(Application.ExeName) + '\settings.ini';
  FSettingsController := TSettingsController.Create(settingsFN);

  FHeapController := THeapController.Create(FHeap);
  FTreeController := TTreeController.Create(FTree);
  FTreeController.OnReturn := DoReturn;
end;

destructor TMainFacade.Destroy;
begin
  FHeapController.Free;
  FSettingsController.Free;
  FTreeController.Free;

  inherited;
end;

procedure TMainFacade.DoReturn(Text: string);
begin
  FHeap.Items.Add(Text);
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

function TMainFacade.ReturnBackCurrentItemEnabled: Boolean;
begin
  Result :=
    (FTree.Items.Count > 0) and
    Assigned(FTree.Selected) and
    (FTreeController.SelectedIsItem(FTree.Selected) or
    (FTreeController.SelectedIsFolder(FTree.Selected) and
     FTree.Selected.HasChildren));
end;

procedure TMainFacade.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  tmpNode: TTreeNode;
begin
  if not ((Source is TListBox) or ((Sender as TTreeView).Items.Count > 0)) then
    Exit;

  tmpNode := (Sender as TTreeView).GetNodeAt(X, Y);
  if Assigned(tmpNode) then
    InsertTextIntoNode(tmpNode);
end;

procedure TMainFacade.TreeSaveToFile(filename: string);
begin
  FSettingsController.FileSaveDirectory := ExtractFilePath(filename);
  FTreeController.SaveToFile(ChangeFileExt(filename, '.txt'));
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
  if not (Source is TListBox) then
    Exit;
  tmpNode := (Sender as TTreeView).GetNodeAt(X, Y);

  Result := Assigned(tmpNode) and FTreeController.SelectedIsFolder(tmpNode);
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
