unit uLogger;

interface

uses
  Classes, SysUtils, System.IOUtils,
  uILogger, uUserInfo;

type
  TLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
    FUserInfo: TUserInfo;
  private
    procedure WriteLog(typeMsg, Msg: string);
  public
    constructor Create(const AFileName: string; UserInfo: TUserInfo);

    procedure AddError(const Msg: string);
    procedure AddInfo(const Msg: string);
    procedure AddAction(const Msg: string);
  end;

implementation

procedure TLogger.AddAction(const Msg: string);
begin
  WriteLog('ACTION', Msg);
end;

procedure TLogger.AddError(const Msg: string);
begin
  WriteLog('ERROR', Msg);
end;

procedure TLogger.AddInfo(const Msg: string);
begin
  WriteLog('INFO', Msg);
end;

constructor TLogger.Create(const AFileName: string; UserInfo: TUserInfo);
begin
  inherited Create;
  FFileName := AFileName;
  FUserInfo := UserInfo;
end;

procedure TLogger.WriteLog(typeMsg, Msg: string);
var
  text: string;
begin
  text :=
    FormatDateTime('yyyy-mm-dd hh:mm:ss', Now()) + #$9
    + FUserInfo.UserName + #$9
    + FUserInfo.ComputerName + #$9
    + typeMsg + #$9
    + Msg + #13#10;

  TFile.AppendAllText(FFileName, text);
end;

end.
