unit uTreeController;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, uNodeModel, uILogger;

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
  public
    constructor Create(View: TTreeView; Logger: ILogger);
    destructor Destroy; override;

    procedure InsertItem(Text: string);
    procedure InsertItemToRoot(Text: string);
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
    function CountNode: integer;

    property OnReturn: TReturnEvent read FOnReturn write FOnReturn;
  end;

implementation

constructor TTreeController.Create(View: TTreeView; Logger: ILogger);
begin
  inherited Create;

  FTView := View;
  FLogger := Logger;
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
  FLogger.AddInfo('Загружаем файл дерева: ' + filename);
  if not FileExists(filename) then
  begin
    FLogger.AddError('Файл не найден:' + filename);
    raise Exception.Create('Файл не найден:'#13#10 + filename);
  end;

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

//  NewNode := FTView.Items.AddChild(Node, nodeList[nodeList.Count - 1]);
//  NewNode.Data := TModelItem.Create();
  AddItem(Node, nodeList[nodeList.Count - 1]);
end;

procedure TTreeController.SaveToFile(FileName: string);
var
  StrStream: TStringStream;
  I: Integer;
begin
  FLogger.AddInfo('Сохраняем дерево в файл: ' + filename);
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
  ParentNode: TTreeNode;
begin
  ParentNode := FTView.Selected;
  if Assigned(ParentNode) then
    FLogger.AddInfo('Добавили в дерево эелемент: ' + ParentNode.Text + '\' + Text)
  else
    FLogger.AddInfo('Добавили в дерево эелемент: ' + Text);

  AddItem(ParentNode, Text);
end;

procedure TTreeController.InsertItemToRoot(Text: string);
begin
  FLogger.AddInfo('Добавили в корень дерева эелемент: ' + Text);

  AddItem(nil, Text);
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
    FLogger.AddInfo('Добавили в дерево группу: ' + ParentNode.Text + '\' + Text)
  else
    FLogger.AddInfo('Добавили в дерево группу: ' + Text);

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
      FLogger.AddInfo('Удалили каталог: ' + Node.Text);
    FreeNode(Node);
    Exit;
  end;

  if not Node.HasChildren then
  begin
    if Assigned(Node) then
      FLogger.AddInfo('Удалили элемент: ' + Node.Text);
    DoReturn(Node.Text);
    FreeNode(Node);
  end
  else
  begin
    if Assigned(Node) then
      FLogger.AddInfo('Удалили каталог: ' + Node.Text);
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
    FLogger.AddInfo(Format('Изменили текст элемента. Было: %s. Cтало: %s', [oldText, Node.Text]));
  end;
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
