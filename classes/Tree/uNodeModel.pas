unit uNodeModel;

interface

uses
  Vcl.Controls, Vcl.ComCtrls;

type
  TModelBase = class
  private
    FImageIndex: Integer;
  public
    property ImageIndex: Integer read FImageIndex;
  end;

  TModelItem = class(TModelBase)
  public
    constructor Create();
  end;

  TModelDir = class(TModelBase)
    constructor Create();
  end;

//  TModeDragNode = class(TControl)
//  private
//    FNode: TTreeNode;
//  public
//    property Node: TTreeNode read FNode write FNode;
//  end;

implementation

{ TModelDir }

constructor TModelDir.Create;
begin
  FImageIndex := 14;
end;

{ TModelItem }

constructor TModelItem.Create;
begin
  FImageIndex := 13;
end;

end.
