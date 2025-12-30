unit uTreeController;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, uNodeModel;

type
  TTreeController = class(TObject)
  private
    FModel: TNodeModel;
    FTView: TTreeView;
    procedure OnChange(Sender: TObject; Node: TTreeNode);
    procedure SaveSelected(Node: TTreeNode);
    procedure RestoreSelected(Node: TTreeNode);
    procedure ExpandAll(Node: TTreeNode);
    procedure CollapseAll(Node: TTreeNode);
    procedure GenerateTreeData(ParentNode: TTreeNode; Stream: TStringStream; path: string);
  public
    constructor Create(View: TTreeView);
    destructor Destroy; override;

    procedure AddNode(ParentNode: TTreeNode; Text: string);
    procedure DeleteNode(Node: TTreeNode);
    procedure EditNode(Node: TTreeNode; NewText: string);
    procedure SelectNode(Node: TTreeNode);

    procedure SaveToFile(FileName: string);
  end;

implementation

constructor TTreeController.Create(View: TTreeView);
begin
  inherited Create;

  FModel := TNodeModel.Create;
  FTView := View;
  FTView.OnChange := OnChange;
end;

destructor TTreeController.Destroy;
begin
  FTView.OnChange := nil;
  FModel.Free;
  inherited;
end;

procedure TTreeController.SaveSelected(Node: TTreeNode);
var
  Idx, i: Integer;
begin
  for i := 0 to FTView.Items.Count - 1 do
  begin
    if FTView.Items[i] = Node then
    begin
      FModel.Nodes[i] := TNodeState.Create;
      FModel.Nodes[i].Expanded := Node.Expanded;
      FModel.Nodes[i].Selected := True;
      Exit;
    end;
  end;
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

procedure TTreeController.RestoreSelected(Node: TTreeNode);
var
  Idx, i: Integer;
begin
  for i := 0 to FTView.Items.Count - 1 do
  begin
    if FTView.Items[i] = Node then
    begin
      FModel.Nodes[i].Expanded := Node.Expanded;
      FModel.Nodes[i].Selected := True;

      Node.Selected := FModel.Nodes[i].Selected;
      Node.Expanded := FModel.Nodes[i].Expanded;
      Exit;
    end;
  end;
end;

procedure TTreeController.OnChange(Sender: TObject; Node: TTreeNode);
begin
  SaveSelected(Node);
end;

procedure TTreeController.AddNode(ParentNode: TTreeNode; Text: string);
var
  NewNode: TTreeNode;
begin
  NewNode := FTView.Items.AddChild(ParentNode, Text);
  ExpandAll(ParentNode);
  SaveSelected(NewNode);
end;

procedure TTreeController.DeleteNode(Node: TTreeNode);
begin
  if Assigned(Node) then
    Node.Free;
end;

procedure TTreeController.EditNode(Node: TTreeNode; NewText: string);
begin
  Node.Text := NewText;
  SaveSelected(Node);
end;

procedure TTreeController.SelectNode(Node: TTreeNode);
begin
  RestoreSelected(Node);
end;

procedure TTreeController.ExpandAll(Node: TTreeNode);
var
  I: integer;
begin
  if (not Assigned(Node)) or (not Assigned(Node.GetFirstChild())) then
    Exit;

  for I := 0 to Node.Count - 1 do
    ExpandAll(Node.Item[I]);
  Node.Expanded := True;
end;

procedure TTreeController.CollapseAll(Node: TTreeNode);
var I: integer;
begin
  for I := 0 to Node.Count - 1 do
    CollapseAll(Node.Item[I]);
  Node.Expanded := False;
end;

end.
