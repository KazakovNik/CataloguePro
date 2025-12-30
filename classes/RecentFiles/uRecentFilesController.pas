unit uRecentFilesController;

interface

uses
  System.Classes, uRecentFilesModel;

type
  TUpdateEvent = procedure(History: TStringList) of object;

  TRecentFilesController = class
  private
    FModel: TRecentFilesModel;
    FOnUpdate: TUpdateEvent;
  private
    procedure DoUpdate(History: TStringList);

  public
    constructor Create();
    destructor Destroy; override;

    procedure LoadHistory(History: string);
    procedure OpenFile(FileName: string);
    procedure SetMaxCountFile(const Value: Integer);
    function RecentFilesCount: Integer;
    function GetFileHistory(index: integer): string;

    property OnUpdate: TUpdateEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses
  System.AnsiStrings, System.SysUtils;

{ TRecentFilesController }

constructor TRecentFilesController.Create;
begin
  FModel := TRecentFilesModel.Create;
  SetMaxCountFile(10);
end;

destructor TRecentFilesController.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TRecentFilesController.DoUpdate(History: TStringList);
begin
  if Assigned(OnUpdate) then
    OnUpdate(History);
end;

function TRecentFilesController.GetFileHistory(index: integer): string;
begin
  Result := EmptyStr;
  if (index >= 0) and (index < FModel.FileHistory.Count) then
    Result := FModel.FileHistory[index];
end;

procedure TRecentFilesController.LoadHistory(History: string);
begin
  FModel.LoadHistory(StringReplace(History, ';', #13#10, [rfReplaceAll]));
end;

procedure TRecentFilesController.OpenFile(FileName: string);
begin
  FModel.Insert(FileName);
  DoUpdate(FModel.FileHistory);
end;

function TRecentFilesController.RecentFilesCount: Integer;
begin
  Result := FModel.FileHistory.Count;
end;

procedure TRecentFilesController.SetMaxCountFile(const Value: Integer);
begin
  FModel.SetMaxCountFile(Value);
end;

end.
