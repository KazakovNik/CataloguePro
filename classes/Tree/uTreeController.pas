unit uTreeController;

interface

uses
  SysUtils, Classes,
  ComCtrls, uNodeModel, uILogger;

type
  TReturnEvent = procedure(aText: string) of object;

  TTreeController = class(TObject)
  private
    FTree: TTreeView;
    FOnReturn: TReturnEvent;
    FLogger: ILogger;
  private
    procedure GenerateTreeData(aParentNode: TTreeNode;
      aStream: TStringStream; aPath: string);
    procedure DoReturn(aText: string);
    procedure ForceNode(aNodeList: TStringList);
    function GetNodeFromParentByName(aParent: TTreeNode;
      const aNameNode: string): TTreeNode;
    procedure Expand(aNode: TTreeNode);
    function AddItem(aParentNode: TTreeNode; aText: string): TTreeNode;
    procedure FreeNode(var aNode: TTreeNode);
    function AddFolder(aParentNode: TTreeNode; aText: string): TTreeNode;
    procedure ClearData;
  public
    constructor Create(aView: TTreeView; aLogger: ILogger);
    destructor Destroy; override;

    procedure InsertItem(aText: string);
    function CloneItem(aSourceNode, aParentNode: TTreeNode): TTreeNode;
    procedure InsertItemToRoot(aText: string);
    procedure DeleteNode(aNode: TTreeNode);
    procedure DeleteAllNode;
    procedure EditNode(aNode: TTreeNode; aNewText: string);
    procedure SelectNode(aNode: TTreeNode);
    procedure ExpandAll;
    procedure CollapseAll;
    procedure SaveToFile(aFileName: string);
    procedure LoadTreeFile(aFileName: string);
    procedure Clear;

    function SelectedIsItem(aNode: TTreeNode): Boolean;
    function SelectedIsFolder(aNode: TTreeNode): Boolean;
    function AddNode(aParentNode: TTreeNode; aText: string): TTreeNode;
    function CountNode: integer;
    function IsEmpty: Boolean;

    property OnReturn: TReturnEvent read FOnReturn write FOnReturn;
  end;

implementation

uses
  uRsControls;

const
  cSeparatorTree = '\';

constructor TTreeController.Create(aView: TTreeView; aLogger: ILogger);
begin
  inherited Create;

  FTree := aView;
  FLogger := aLogger;
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
  for i := 0 to FTree.Items.Count - 1 do
    if Assigned(FTree.Items[i].Data) then
      TObject(FTree.Items[i].Data).Free;
end;

function TTreeController.CountNode: integer;
begin
  Result := FTree.Items.Count;
end;

procedure TTreeController.DoReturn(aText: string);
begin
  if Assigned(OnReturn) then
    OnReturn(aText);
end;

procedure TTreeController.LoadTreeFile(aFileName: string);
var
  vFile: TStringList;
  vTempList: TStringList;
  i: Integer;
begin
  FLogger.AddInfo(resTreeLoadFile + aFileName);
  if not FileExists(aFileName) then
  begin
    FLogger.AddError(resTreeFileNotFound + aFileName);
    raise Exception.Create(resTreeFileNotFound + #13#10 + aFileName);
  end;

  vFile := TStringList.Create;
  vTempList:= TStringList.Create;
  try
    vFile.LoadFromFile(aFileName);

    for i := 0 to vFile.Count - 1 do
    begin
      vTempList.Text := StringReplace(vFile[i], cSeparatorTree, #13#10, [rfReplaceAll]);
      ForceNode(vTempList);
    end;
  finally
    vFile.Free;
    vTempList.Free;
  end;
end;

function TTreeController.GetNodeFromParentByName(aParent: TTreeNode;
  const aNameNode: string): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTree.Items.Count - 1 do
  begin
    if FTree.Items[i].Parent = aParent then
    begin
      if SameText(FTree.Items[i].Text, aNameNode) then
        Exit(FTree.Items[i]);
    end;
  end;
end;

procedure TTreeController.ForceNode(aNodeList: TStringList);
var
  i: integer;
  vNode: TTreeNode;
  vParentNode: TTreeNode;
begin
  vNode := nil;
  for i := 0 to aNodeList.Count - 1 do
  begin
    vParentNode := vNode;
    if i = aNodeList.Count - 1 then
      Break;
    if not Assigned(vParentNode) then
    begin
      vNode := GetNodeFromParentByName(nil, aNodeList[i]);
      if not Assigned(vNode) then
        vNode := AddNode(nil, aNodeList[i]);
    end
    else
    begin
      vNode := GetNodeFromParentByName(vParentNode, aNodeList[i]);
      if not Assigned(vNode) then
        vNode := AddNode(vParentNode, aNodeList[i]);
    end;
  end;

  AddItem(vNode, aNodeList[aNodeList.Count - 1]);
end;

procedure TTreeController.SaveToFile(aFileName: string);
var
  vStrStream: TStringStream;
  i: Integer;
begin
  FLogger.AddInfo(resTreeSaveToFile + aFileName);
  vStrStream := TStringStream.Create(EmptyStr);
  try
    for i := 0 to FTree.Items.Count - 1 do
    begin
      if not Assigned(FTree.Items[i].Parent) then
        GenerateTreeData(FTree.Items[I], vStrStream, EmptyStr)
      else
        vStrStream.WriteString(FTree.Items[i].Text + #13#10);
    end;
    vStrStream.SaveToFile(aFileName);
  finally
    vStrStream.Free;
  end;
end;

procedure TTreeController.GenerateTreeData(aParentNode: TTreeNode;
  aStream: TStringStream; aPath: string);
var
  i: Integer;
  vNode: TTreeNode;
begin
  if aPath <> EmptyStr then
    aPath := aPath + '\';
  aPath := aPath + aParentNode.Text;

  for i := 0 to aParentNode.Count - 1 do
  begin
    vNode := aParentNode.Item[i];

    if vNode.HasChildren then
      GenerateTreeData(vNode, aStream, aPath + cSeparatorTree + vNode.Text)
    else
      aStream.WriteString(aPath + cSeparatorTree + vNode.Text + #13#10);
  end;
end;

procedure TTreeController.InsertItem(aText: string);
var
  vParentNode: TTreeNode;
begin
  vParentNode := FTree.Selected;
  if Assigned(vParentNode) then
    FLogger.AddInfo(resTreeAddItem + vParentNode.Text + '\' + aText)
  else
    FLogger.AddInfo(resTreeAddItem + aText);

  AddItem(vParentNode, aText);
end;

procedure TTreeController.InsertItemToRoot(aText: string);
begin
  FLogger.AddInfo(resTreeAddItemRoot + aText);

  AddItem(nil, aText);
end;

function TTreeController.IsEmpty: Boolean;
begin
  Result := CountNode = 0;
end;

function TTreeController.AddItem(aParentNode: TTreeNode; aText: string): TTreeNode;
begin
  Result := FTree.Items.AddChild(aParentNode, aText);
  Result.Data := TModelItem.Create();
  Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
  Result.SelectedIndex := Result.ImageIndex;

  Expand(aParentNode);
end;

function TTreeController.AddFolder(aParentNode: TTreeNode; aText: string): TTreeNode;
begin
  Result := FTree.Items.AddChild(aParentNode, aText);
  Result.Data := TModelDir.Create();
  Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
  Result.SelectedIndex := Result.ImageIndex;

  Expand(aParentNode);
end;

function TTreeController.AddNode(aParentNode: TTreeNode; aText: string): TTreeNode;
var
  vNewNode: TTreeNode;
begin
  if Assigned(aParentNode) then
    FLogger.AddInfo(resTreeAddGroup + aParentNode.Text + '\' + aText)
  else
    FLogger.AddInfo(resTreeAddGroup + aText);

  vNewNode := AddFolder(aParentNode, aText);
  FTree.Selected := vNewNode;
  Result := vNewNode;
end;

procedure TTreeController.FreeNode(var aNode: TTreeNode);
begin
  TObject(aNode.Data).Free;
  aNode.Free;
end;

procedure TTreeController.DeleteAllNode;
var
  I: Integer;
  vRootNode: TTreeNode;
begin
  for I := FTree.Items.Count - 1 downto 0 do
  begin
    vRootNode := FTree.Items[I];
    if not Assigned(vRootNode.Parent) then
      DeleteNode(vRootNode);
  end;
end;

procedure TTreeController.DeleteNode(aNode: TTreeNode);
var
  i: integer;
begin
  if not Assigned(aNode) then
    Exit;

  if SelectedIsFolder(aNode) and (not aNode.HasChildren)  then
  begin
    if Assigned(aNode) then
      FLogger.AddInfo(resTreeDelGroup + aNode.Text);
    FreeNode(aNode);
    Exit;
  end;

  if not aNode.HasChildren then
  begin
    if Assigned(aNode) then
      FLogger.AddInfo(resTreeDelitem + aNode.Text);
    DoReturn(aNode.Text);
    FreeNode(aNode);
  end
  else
  begin
    if Assigned(aNode) then
      FLogger.AddInfo(resTreeDelGroup + aNode.Text);
    for i := aNode.Count - 1 downto 0 do
      DeleteNode(aNode.Item[i]);
    FreeNode(aNode);
  end;
end;

procedure TTreeController.EditNode(aNode: TTreeNode; aNewText: string);
var
  vOldText: string;
begin
  if Assigned(aNode) then
  begin
    vOldText := aNode.Text;
    aNode.Text := aNewText;
    FLogger.AddInfo(Format(resTreeEdit, [vOldText, aNode.Text]));
  end;
end;

function TTreeController.SelectedIsFolder(aNode: TTreeNode): Boolean;
begin
  Result := not SelectedIsItem(aNode);
end;

function TTreeController.SelectedIsItem(aNode: TTreeNode): Boolean;
begin
  Result :=
    Assigned(aNode) and
    Assigned(aNode.Data) and
    (TObject(aNode.Data).ClassName = TModelItem.ClassName);
end;

procedure TTreeController.SelectNode(aNode: TTreeNode);
begin
  FTree.Selected := aNode;
end;

procedure TTreeController.Expand(aNode: TTreeNode);
var
  i: integer;
begin
  if (not Assigned(aNode)) or (not Assigned(aNode.GetFirstChild())) then
    Exit;

  for i := 0 to aNode.Count - 1 do
    Expand(aNode.Item[i]);
  aNode.Expanded := True;
end;

procedure TTreeController.ExpandAll;
begin
  FTree.FullExpand;
  if FTree.Items.Count > 0 then
    FTree.Selected := FTree.Items[0];
end;

procedure TTreeController.Clear;
begin
  FLogger.AddInfo(resTreeClear);
  ClearData;
  FTree.Items.Clear;
end;

function TTreeController.CloneItem(aSourceNode, aParentNode: TTreeNode): TTreeNode;
begin
  with FTree do
  begin
    Result := Items.AddChild(aParentNode, aSourceNode.Text);
    Result.Data := aSourceNode.Data;
    Result.ImageIndex := TModelBase(Result.Data).ImageIndex;
    Result.SelectedIndex := Result.ImageIndex;

    Expand(aParentNode);
  end;
end;

procedure TTreeController.CollapseAll;
begin
  FTree.FullCollapse;
end;

end.
