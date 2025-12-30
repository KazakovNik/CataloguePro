unit uUserInfo;

interface

uses
  Winapi.Windows, System.SysUtils;

type
  TUserInfo = class
  private
    function GetUserName: string;
    function GetComputerName: string;
  public
    property UserName: string read GetUserName;
    property ComputerName: string read GetComputerName;
  end;

implementation

{ TUserInfo }

function TUserInfo.GetComputerName: string;
begin
  Result := GetEnvironmentVariable('COMPUTERNAME');
end;

function TUserInfo.GetUserName: string;
begin
  Result := GetEnvironmentVariable('USERNAME');
end;

end.
