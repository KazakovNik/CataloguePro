unit uNodeModel;

interface

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
