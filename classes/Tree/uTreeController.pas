unit uTreeController;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, uNodeModel;

type
  TReturnEvent = procedure(Text: string) of object;

  TTreeController = class(TObject)
  private
    FTView: TTreeView;
    FOnReturn: TReturnEvent;
  private
    procedure GenerateTreeData(ParentNode: TTreeNode;
      Stream: TStringStream; path: string);
    procedure DoReturn(Text: string);
    procedure ForceNode(nodeList: TStringList);
    function GetNodeFromParentByName(Parent: TTreeNode;
      const NameNode: string): TTreeNode;
    procedure Expand(Node: TTreeNode);
  public
    constructor Create(View: TTreeView);
    destructor Destroy; override;

    procedure InsertItem(Text: string);
    procedure DeleteNode(Node: TTreeNode);
    procedure EditNode(Node: TTreeNode; NewText: string);
    procedure SelectNode(Node: TTreeNode);
    procedure ExpandAll;
    procedure CollapseAll;
    procedure SaveToFile(FileName: string);
    procedure LoadTreeFile(filename: string);

    function SelectedIsItem(Node: TTreeNode): Boolean;
    function SelectedIsFolder(Node: TTreeNode): Boolean;
    //function GetDragControlTree: TControl;
    function AddNode(ParentNode: TTreeNode; Text: string): TTreeNode;

    property OnReturn: TReturnEvent read FOnReturn write FOnReturn;
  end;

implementation

constructor TTreeController.Create(View: TTreeView);
begin
  inherited Create;

  FTView := View;
end;

destructor TTreeController.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTView.Items.Count - 1 do
    if Assigned(FTView.Items[i].Data) then
      TObject(FTView.Items[i].Data).Free;

  inherited;
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
  if not FileExists(filename) then
    raise Exception.Create('Файл не найден:'#13#10 + filename);

  sl := TStringList.Create;
  slt:= TStringList.Create;
  try
    sl.LoadFromFile(filename);

    for i := 0 to sl.Count - 1 do
    begin
      slt.Text := StringReplace(sl[i], '\', #13#10, [rfReplaceAll]);
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
  I, j: Integer;
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
  NewNode, ParentNode: TTreeNode;
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

  NewNode := FTView.Items.AddChild(Node, nodeList[nodeList.Count - 1]);
  NewNode.Data := TModelItem.Create();
end;

procedure TTreeController.SaveToFile(FileName: string);
var
  StrStream: TStringStream;
  I: Integer;
begin
  StrStream := TStringStream.Create('');
  try
    for I := 0 to FTView.Items.Count - 1 do
    begin
      if not Assigned(FTView.Items[i].Parent) then
        GenerateTreeData(FTView.Items[I], StrStream, EmptyStr);
    end;
    StrStream.SaveToFile(FileName);
  finally
    StrStream.Free;
  end;
end;

procedure TTreeController.GenerateTreeData(ParentNode: TTreeNode; Stream: TStringStream; path: string);
var
  I: Integer;
  Node: TTreeNode;
  Indentation: string;
begin
  if path <> EmptyStr then
    path := path + '\';
  path := path + ParentNode.Text;

  for I := 0 to ParentNode.Count - 1 do
  begin
    Node := ParentNode.Item[I];

    if Node.HasChildren then
      GenerateTreeData(Node, Stream, path + '\' + Node.Text)
    else
      Stream.WriteString(path + '\' + Node.Text + #13#10);
  end;
end;

//function TTreeController.GetDragControlTree: TControl;
//var
//  model: TModeDragNode;
//begin
//  model := TModeDragNode.Create(nil);
//  model.Node := FTView.Selected;
//  Result := model;
//end;

procedure TTreeController.InsertItem(Text: string);
var
  NewNode, ParentNode: TTreeNode;
begin
  ParentNode := FTView.Selected;
  NewNode := FTView.Items.AddChild(ParentNode, Text);
  NewNode.Data := TModelItem.Create();
  Expand(ParentNode);
end;

function TTreeController.AddNode(ParentNode: TTreeNode; Text: string): TTreeNode;
var
  NewNode: TTreeNode;
begin
  NewNode := FTView.Items.AddChild(ParentNode, Text);
  NewNode.Data := TModelDir.Create();
  Expand(ParentNode);
  FTView.Selected := NewNode;
  Result := NewNode;
end;

procedure TTreeController.DeleteNode(Node: TTreeNode);
var
  i: integer;
begin
  if not Assigned(Node) then
    Exit;

  if SelectedIsFolder(Node) and (not Node.HasChildren)  then
  begin
    Node.Free;
    Exit;
  end;

  if not Node.HasChildren then
  begin
    DoReturn(Node.Text);
    Node.Free;
  end
  else
  begin
    for i := Node.Count - 1 downto 0 do
      DeleteNode(Node.Item[i]);
    Node.Free;
  end;
end;

procedure TTreeController.EditNode(Node: TTreeNode; NewText: string);
begin
  Node.Text := NewText;
end;

function TTreeController.SelectedIsFolder(Node: TTreeNode): Boolean;
begin
  Result := not SelectedIsItem(Node);
end;

function TTreeController.SelectedIsItem(Node: TTreeNode): Boolean;
begin
  Result := Assigned(Node) and Assigned(Node.Data) and (TObject(Node.Data).ClassName = 'TModelItem');
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

procedure TTreeController.CollapseAll;
begin
  FTView.FullCollapse;
end;

end.
