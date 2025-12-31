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

    procedure Insert(FileName: string);
    procedure SetMaxCountFile(const Value: Integer);
    procedure LoadHistory(History: string);
    procedure DeleteByName(FileName: string);

    property FileHistory: TStringList read FFileHistory;
  end;

implementation

{ TRecentFilesModel }

procedure TRecentFilesModel.Insert(FileName: string);
begin
  if FFileHistory.IndexOf(FileName) = -1 then
    FileHistory.Insert(0, Filename)
  else
  begin
    FileHistory.Delete(FileHistory.IndexOf(Filename));
    FileHistory.Insert(0, Filename);
  end;
  SetMaxCountFile(FMaxCountFile);
end;

procedure TRecentFilesModel.LoadHistory(History: string);
begin
  FFileHistory.Text := History;
  SetMaxCountFile(FMaxCountFile);
end;

procedure TRecentFilesModel.SetMaxCountFile(const Value: Integer);
var
  i: integer;
begin
  FMaxCountFile := Value;
  for i := FFileHistory.Count - 1 downto Value do
    FFileHistory.Delete(i);
end;

constructor TRecentFilesModel.Create;
begin
  FMaxCountFile := 10;
  FFileHistory := TStringList.Create;
end;

procedure TRecentFilesModel.DeleteByName(FileName: string);
var
  index: integer;
begin
  index := FFileHistory.IndexOf(FileName);
  if index <> -1 then
    FFileHistory.Delete(index);
end;

destructor TRecentFilesModel.Destroy;
begin
  FFileHistory.Free;

  inherited;
end;

end.
