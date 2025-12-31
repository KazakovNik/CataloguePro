unit uTreeController;

interface

uses
  SysUtils, Classes,
  ComCtrls, uNodeModel, uILogger;

type
  TReturnEvent = procedure(Text: string) of object;

  TTreeController = class(TObject)
  private
    FTView: TTreeView;
    FOnReturn: TReturnEvent;
    FLogger: ILogger;
  private
    procedure GenerateTreeData(ParentNode: TTreeNode;
      Stream: TStringStream; path: string);
    procedure DoReturn(Text: string);
    procedure ForceNode(nodeList: TStringList);
    function GetNodeFromParentByName(Parent: TTreeNode;
      const NameNode: string): TTreeNode;
    procedure Expand(Node: TTreeNode);
    function AddItem(ParentNode: TTreeNode; Text: string): TTreeNode;
    procedure FreeNode(var Node: TTreeNode);
    function AddFolder(ParentNode: TTreeNode; Text: string): TTreeNode;
    procedure ClearData;
  public
    constructor Create(View: TTreeView; Logger: ILogger);
    destructor Destroy; override;

    procedure InsertItem(Text: string);
    function CloneItem(SourceNode, ParentNode: TTreeNode): TTreeNode;
    procedure InsertItemToRoot(Text: string);
    procedure DeleteNode(Node: TTreeNode);
    procedure EditNode(Node: TTreeNode; NewText: string);
    procedure SelectNode(Node: TTreeNode);
    procedure ExpandAll;
    procedure CollapseAll;
    procedure SaveToFile(FileName: string);
    procedure LoadTreeFile(filename: string);
    procedure Clear;

    function SelectedIsItem(Node: TTreeNode): Boolean;
    function SelectedIsFolder(Node: TTreeNode): Boolean;
    function AddNode(ParentNode: TTreeNode; Text: string): TTreeNode;
    function CountNode: integer;
    function IsEmpty: Boolean;

    property OnReturn: TReturnEvent read FOnReturn write FOnReturn;
  end;

implementation

uses
  uRsControls;

const
  cSeparatorTree = '\';

constructor TTreeController.Create(View: TTreeView; Logger: ILogger);
begin
  inherited Create;

  FTView := View;
  FLogger := Logger;
end;

destructor TTreeController.Destroy;
begin
  ClearData;

  inherited;
end;

procedure TTreeController.ClearData;
var
  i: Integer;
begin
  for i := 0 to FTView.Items.Count - 1 do
    if Assigned(FTView.Items[i].Data) then
      TObject(FTView.Items[i].Data).Free;
end;

function TTreeController.CountNode: integer;
begin
  Result := FTView.Items.Count;
end;

procedure TTreeController.DoReturn(Text: string);
begin
  if Assigned(OnReturn) then
    OnReturn(Text);
end;

procedure TTreeController.LoadTreeFile(filename: string);
var
  sl: TStringList;
  slt: TStringList;
  i: Integer;
begin
  FLogger.AddInfo(resTreeLoadFile + filename);
  if not FileExists(filename) then
  begin
    FLogger.AddError(resTreeFileNotFound + filename);
    raise Exception.Create(resTreeFileNotFound + #13#10 + filename);
  end;

  sl := TStringList.Create;
  slt:= TStringList.Create;
  try
    sl.LoadFromFile(filename);

    for i := 0 to sl.Count - 1 do
    begin
      slt.Text := StringReplace(sl[i], cSeparatorTree, #13#10, [rfReplaceAll]);
      ForceNode(slt);
    end;
  finally
    sl.Free;
    slt.Free;
  end;
end;

function TTreeController.GetNodeFromParentByName(Parent: TTreeNode;
  const NameNode: string): TTreeNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTView.Items.Count - 1 do
  begin
    if FTView.Items[I].Parent = Parent then
    begin
      if SameText(FTView.Items[I].Text, NameNode) then
        Exit(FTView.Items[I]);
    end;
  end;
end;

procedure TTreeController.ForceNode(nodeList: TStringList);
var
  i: integer;
  Node: TTreeNode;
  ParentNode: TTreeNode;
begin
  Node := nil;
  for i := 0 to nodeList.Count - 1 do
  begin
    ParentNode := Node;
    if i = nodeList.Count - 1 then
      Break;
    if not Assigned(ParentNode) then
    begin
      Node := GetNodeFromParentByName(nil, nodeList[i]);
      if not Assigned(Node) then
        Node := AddNode(nil, nodeList[i]);
    end
    else
    begin
      Node := GetNodeFromParentByName(ParentNode, nodeList[i]);
      if not Assigned(Node) then
        Node := AddNode(ParentNode, nodeList[i]);
    end;
  end;

  AddItem(Node, nodeList[nodeList.Count - 1]);
end;

procedure TTreeController.SaveToFile(FileName: string);
var
  StrStream: TStringStream;
  i: Integer;
begin
  FLogger.AddInfo(resTreeSaveToFile + filename);
  StrStream := TStringStream.Create(EmptyStr);
  try
    for i := 0 to FTView.Items.Count - 1 do
    begin
      if not Assigned(FTView.Items[i].Parent) then
        GenerateTreeData(FTView.Items[I], StrStream, EmptyStr)
      else
        StrStream.WriteString(FTView.Items[i].Text + #13#10);
    end;
    StrStream.SaveToFile(FileName);
  finally
    StrStream.Free;
  end;
end;

procedure TTreeController.GenerateTreeData(ParentNode: TTreeNode;
  Stream: TStringStream; path: string);
var
  I: Integer;
  Node: TTreeNode;
begin
  if path <> EmptyStr then
    path := path + '\';
  path := path + ParentNode.Text;

  for I := 0 to ParentNode.Count - 1 do
  begin
    Node := ParentNode.Item[I];

    if Node.HasChildren then
      GenerateTreeData(Node, Stream, path + cSeparatorTree + Node.Text)
    else
      Stream.WriteString(path + cSeparatorTree + Node.Text + #13#10);
  end;
end;

procedure TTreeController.InsertItem(Text: string);
var
  ParentNode: TTreeNode;
begin
  ParentNode := FTView.Selected;
  if Assigned(ParentNode) then
    FLogger.AddInfo(resTreeAddItem + ParentNode.Text + '\' + Text)
  else
    FLogger.AddInfo(resTreeAddItem + Text);

  AddItem(ParentNode, Text);
end;

procedure TTreeController.InsertItemToRoot(Text: string);
begin
  FLogger.AddInfo(resTreeAddItemRoot + Text);

  AddItem(nil, Text);
end;

function TTreeController.IsEmpty: Boolean;
begin
  Result := CountNode = 0;
end;

function TTreeController.AddItem(ParentNode: TTreeNode; Text: string): TTreeNode;
begin
  Result := FTView.Items.AddChild(ParentNode, Text);
  Result.Data := TModelItem.Create();
  Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
  Result.SelectedIndex := Result.ImageIndex;

  Expand(ParentNode);
end;

function TTreeController.AddFolder(ParentNode: TTreeNode; Text: string): TTreeNode;
begin
  Result := FTView.Items.AddChild(ParentNode, Text);
  Result.Data := TModelDir.Create();
  Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
  Result.SelectedIndex := Result.ImageIndex;

  Expand(ParentNode);
end;

function TTreeController.AddNode(ParentNode: TTreeNode; Text: string): TTreeNode;
var
  NewNode: TTreeNode;
begin
  if Assigned(ParentNode) then
    FLogger.AddInfo(resTreeAddGroup + ParentNode.Text + '\' + Text)
  else
    FLogger.AddInfo(resTreeAddGroup + Text);

  NewNode := AddFolder(ParentNode, Text);
  FTView.Selected := NewNode;
  Result := NewNode;
end;

procedure TTreeController.FreeNode(var Node: TTreeNode);
begin
  TObject(Node.Data).Free;
  Node.Free;
end;

procedure TTreeController.DeleteNode(Node: TTreeNode);
var
  i: integer;
begin
  if not Assigned(Node) then
    Exit;

  if SelectedIsFolder(Node) and (not Node.HasChildren)  then
  begin
    if Assigned(Node) then
      FLogger.AddInfo(resTreeDelGroup + Node.Text);
    FreeNode(Node);
    Exit;
  end;

  if not Node.HasChildren then
  begin
    if Assigned(Node) then
      FLogger.AddInfo(resTreeDelitem + Node.Text);
    DoReturn(Node.Text);
    FreeNode(Node);
  end
  else
  begin
    if Assigned(Node) then
      FLogger.AddInfo(resTreeDelGroup + Node.Text);
    for i := Node.Count - 1 downto 0 do
      DeleteNode(Node.Item[i]);
    FreeNode(Node);
  end;
end;

procedure TTreeController.EditNode(Node: TTreeNode; NewText: string);
var
  oldText: string;
begin
  if Assigned(Node) then
  begin
    oldText := Node.Text;
    Node.Text := NewText;
    FLogger.AddInfo(Format(resTreeEdit, [oldText, Node.Text]));
  end;
end;

function TTreeController.SelectedIsFolder(Node: TTreeNode): Boolean;
begin
  Result := not SelectedIsItem(Node);
end;

function TTreeController.SelectedIsItem(Node: TTreeNode): Boolean;
begin
  Result :=
    Assigned(Node) and
    Assigned(Node.Data) and
    (TObject(Node.Data).ClassName = TModelItem.ClassName);
end;

procedure TTreeController.SelectNode(Node: TTreeNode);
begin
  FTView.Selected := Node;
end;

procedure TTreeController.Expand(Node: TTreeNode);
var
  I: integer;
begin
  if (not Assigned(Node)) or (not Assigned(Node.GetFirstChild())) then
    Exit;

  for I := 0 to Node.Count - 1 do
    Expand(Node.Item[I]);
  Node.Expanded := True;
end;

procedure TTreeController.ExpandAll;
begin
  FTView.FullExpand;
  if FTView.Items.Count > 0 then
    FTView.Selected := FTView.Items[0];
end;

procedure TTreeController.Clear;
begin
  FLogger.AddInfo(resTreeClear);
  ClearData;
  FTView.Items.Clear;
end;

function TTreeController.CloneItem(SourceNode, ParentNode: TTreeNode): TTreeNode;
begin
  with FTView do
  begin
    Result := Items.AddChild(ParentNode, SourceNode.Text);
    Result.Data := SourceNode.Data;
    Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
    Result.SelectedIndex := Result.ImageIndex;

    Expand(ParentNode);
  end;
end;

procedure TTreeController.CollapseAll;
begin
  FTView.FullCollapse;
end;

end.
