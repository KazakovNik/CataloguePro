unit uNodeModel;

interface

uses
  Classes, Generics.Collections, System.SysUtils;

type
  TNodeState = class
    Expanded: Boolean;
    Selected: Boolean;
  end;

  TNodeModel = class(TObject)
  private
    FNodes: TList<TNodeState>;
  private
    procedure SetNodeState(Index: Integer; const Value: TNodeState);
    function GetNodeState(Index: Integer): TNodeState;
  public
    constructor Create;
    destructor Destroy; override;

    property Nodes[Index: Integer]: TNodeState read GetNodeState write SetNodeState; default;
  end;

implementation

constructor TNodeModel.Create;
begin
  inherited;
  FNodes := TList<TNodeState>.Create;
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

procedure TNodeModel.SetNodeState(Index: Integer; const Value: TNodeState);
begin
  if Index >= 0 then begin
    while FNodes.Count <= Index do
      FNodes.Add(Default(TNodeState));
    FNodes[Index] := Value;
  end;
end;

function TNodeModel.GetNodeState(Index: Integer): TNodeState;
begin
  Result := Default(TNodeState);
  if (Index >= 0) and (FNodes.Count > Index) then
    Result := FNodes[Index];
end;

end.
