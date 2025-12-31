unit uLogger;

interface

uses
  SysUtils, System.IOUtils,
  uILogger, uUserInfo;

type
  TLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
    FUserInfo: TUserInfo;
  private
    procedure WriteLog(aTypeMsg, aMsg: string);
  public
    constructor Create(const aFileName: string; aUserInfo: TUserInfo);

    procedure AddError(const aMsg: string);
    procedure AddInfo(const aMsg: string);
    procedure AddAction(const aMsg: string);
  end;

implementation

procedure TLogger.AddAction(const aMsg: string);
begin
  WriteLog('ACTION', aMsg);
end;

procedure TLogger.AddError(const aMsg: string);
begin
  WriteLog('ERROR', aMsg);
end;

procedure TLogger.AddInfo(const aMsg: string);
begin
  WriteLog('INFO', aMsg);
end;

constructor TLogger.Create(const aFileName: string; aUserInfo: TUserInfo);
begin
  inherited Create;
  FFileName := aFileName;
  FUserInfo := aUserInfo;
end;

procedure TLogger.WriteLog(aTypeMsg, aMsg: string);
var
  vMsg: string;
begin
  vMsg :=
    FormatDateTime('yyyy-mm-dd hh:mm:ss', Now()) + #$9
    + FUserInfo.UserName + #$9
    + FUserInfo.ComputerName + #$9
    + aTypeMsg + #$9
    + aMsg + #13#10;

  TFile.AppendAllText(FFileName, vMsg);
end;

end.
