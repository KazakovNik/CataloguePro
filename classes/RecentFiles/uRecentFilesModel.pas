unit uRecentFilesModel;

interface

uses
  System.Classes;

type
  TRecentFilesModel = class
  private
    FFileHistory: TStringList;
    FMaxCountFile: integer;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Insert(aFileName: string);
    procedure SetMaxCountFile(const aValue: Integer);
    procedure LoadHistory(aHistory: string);
    procedure DeleteByName(aFileName: string);

    property FileHistory: TStringList read FFileHistory;
  end;

implementation

{ TRecentFilesModel }

procedure TRecentFilesModel.Insert(aFileName: string);
begin
  if FFileHistory.IndexOf(aFileName) = -1 then
    FileHistory.Insert(0, aFileName)
  else
  begin
    FileHistory.Delete(FileHistory.IndexOf(aFileName));
    FileHistory.Insert(0, aFileName);
  end;
  SetMaxCountFile(FMaxCountFile);
end;

procedure TRecentFilesModel.LoadHistory(aHistory: string);
begin
  FFileHistory.Text := aHistory;
  SetMaxCountFile(FMaxCountFile);
end;

procedure TRecentFilesModel.SetMaxCountFile(const aValue: Integer);
var
  i: integer;
begin
  FMaxCountFile := aValue;
  for i := FFileHistory.Count - 1 downto aValue do
    FFileHistory.Delete(i);
end;

constructor TRecentFilesModel.Create;
begin
  FMaxCountFile := 10;
  FFileHistory := TStringList.Create;
end;

procedure TRecentFilesModel.DeleteByName(aFileName: string);
var
  vIndex: integer;
begin
  vIndex := FFileHistory.IndexOf(aFileName);
  if vIndex <> -1 then
    FFileHistory.Delete(vIndex);
end;

destructor TRecentFilesModel.Destroy;
begin
  FFileHistory.Free;

  inherited;
end;

end.
