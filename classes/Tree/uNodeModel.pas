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

const
  cImageFolder = 14;
  cImageFile = 13;

{ TModelDir }

constructor TModelDir.Create;
begin
  FImageIndex := cImageFolder;
end;

{ TModelItem }

constructor TModelItem.Create;
begin
  FImageIndex := cImageFile;
end;

end.
