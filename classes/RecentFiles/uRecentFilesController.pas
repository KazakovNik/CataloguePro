unit uRecentFilesController;

interface

uses
  System.Classes, uRecentFilesModel, uILogger;

type
  TUpdateEvent = procedure(aHistory: TStringList) of object;

  TRecentFilesController = class
  private
    FModel: TRecentFilesModel;
    FOnUpdate: TUpdateEvent;
    FLogger: ILogger;
  private
    procedure DoUpdate(aHistory: TStringList);
  public
    constructor Create(aLogger: ILogger);
    destructor Destroy; override;

    procedure LoadHistory(aHistory: string);
    procedure OpenFile(aFileName: string);
    procedure SetMaxCountFile(const aValue: Integer);
    function RecentFilesCount: Integer;
    function GetFileHistory(aIndex: integer): string;
    procedure DeleteByName(aFileName: string);

    property OnUpdate: TUpdateEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses
  System.AnsiStrings, System.SysUtils, uRsControls;

{ TRecentFilesController }

constructor TRecentFilesController.Create(aLogger: ILogger);
begin
  FLogger := aLogger;
  FModel := TRecentFilesModel.Create;
  SetMaxCountFile(10);
end;

procedure TRecentFilesController.DeleteByName(aFileName: string);
begin
  FLogger.AddInfo(resRecentFilesDelFile + #13#10 + aFileName);
  FModel.DeleteByName(aFileName);
  DoUpdate(FModel.FileHistory);
end;

destructor TRecentFilesController.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TRecentFilesController.DoUpdate(aHistory: TStringList);
begin
  if Assigned(OnUpdate) then
    OnUpdate(aHistory);
end;

function TRecentFilesController.GetFileHistory(aIndex: integer): string;
begin
  Result := EmptyStr;
  if (aIndex >= 0) and (aIndex < FModel.FileHistory.Count) then
    Result := FModel.FileHistory[aIndex];
end;

procedure TRecentFilesController.LoadHistory(aHistory: string);
begin
  FModel.LoadHistory(StringReplace(aHistory, ';', #13#10, [rfReplaceAll]));
end;

procedure TRecentFilesController.OpenFile(aFileName: string);
begin
  FModel.Insert(aFileName);
  DoUpdate(FModel.FileHistory);
end;

function TRecentFilesController.RecentFilesCount: Integer;
begin
  Result := FModel.FileHistory.Count;
end;

procedure TRecentFilesController.SetMaxCountFile(const aValue: Integer);
begin
  FModel.SetMaxCountFile(aValue);
end;

end.
