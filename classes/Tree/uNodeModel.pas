unit uNodeModel;

interface

uses
  Classes, Generics.Collections, System.SysUtils;

type
  TNodeData = class
  public
    Expanded: Boolean;
    Selected: Boolean;
    isFolder: Boolean;
  end;

  TNodeModel = class(TObject)
  private
    FNodes: TList<TNodeData>;
  private
    procedure SetNodeState(Index: Integer; const Value: TNodeData);
    function GetNodeState(Index: Integer): TNodeData;
  public
    constructor Create;
    destructor Destroy; override;

    property Nodes[Index: Integer]: TNodeData read GetNodeState write SetNodeState; default;
  end;

implementation

constructor TNodeModel.Create;
begin
  inherited;
  FNodes := TList<TNodeData>.Create;
end;

destructor TNodeModel.Destroy;
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    if Assigned(FNodes.Items[i]) then
      FNodes.Items[i].Free;
  FNodes.Clear;

  FreeAndNil(FNodes);
  inherited;
end;

procedure TNodeModel.SetNodeState(Index: Integer; const Value: TNodeData);
begin
  if Index >= 0 then begin
    while FNodes.Count <= Index do
      FNodes.Add(Default(TNodeData));
    FNodes[Index] := Value;
  end;
end;

function TNodeModel.GetNodeState(Index: Integer): TNodeData;
begin
  Result := Default(TNodeData);
  if (Index >= 0) and (FNodes.Count > Index) then
    Result := FNodes[Index];
end;

end.
