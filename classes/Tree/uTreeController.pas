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
    procedure GenerateTreeData(ParentNode: TTreeNode; Stream: TStringStream; path: string);
    procedure DoReturn(Text: string);
    procedure Expand(Node: TTreeNode);
    procedure Collapse(Node: TTreeNode);
  public
    constructor Create(View: TTreeView);
    destructor Destroy; override;

    procedure AddNode(ParentNode: TTreeNode; Text: string);
    procedure InsertItem(Text: string);
    procedure DeleteNode(Node: TTreeNode);
    procedure EditNode(Node: TTreeNode; NewText: string);
    procedure SelectNode(Node: TTreeNode);

    function SelectedIsItem(Node: TTreeNode): Boolean;
    function SelectedIsFolder(Node: TTreeNode): Boolean;
    //function GetDragControlTree: TControl;

    procedure ExpandAll;
    procedure CollapseAll;

    procedure SaveToFile(FileName: string);

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

procedure TTreeController.AddNode(ParentNode: TTreeNode; Text: string);
var
  NewNode: TTreeNode;
  vModel: TModelDir;
begin
  NewNode := FTView.Items.AddChild(ParentNode, Text);
  NewNode.Data := TModelDir.Create();
  Expand(ParentNode);
  FTView.Selected := NewNode;
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

procedure TTreeController.Collapse(Node: TTreeNode);
var I: integer;
begin
  if (not Assigned(Node)) or (not Assigned(Node.GetFirstChild())) then
    Exit;
  for I := 0 to Node.Count - 1 do
    Collapse(Node.Item[I]);
  Node.Expanded := False;
end;

procedure TTreeController.ExpandAll;
begin
  Expand(nil);
end;

procedure TTreeController.CollapseAll;
begin
  Collapse(nil);
end;

end.
