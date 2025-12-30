unit uILogger;

interface

type
  ILogger = interface
    ['{A13C9C6B-02DA-42E0-9F34-A5DEA1CD6637}']

    procedure AddError(const Msg: string);
    procedure AddInfo(const Msg: string);
    procedure AddAction(const Msg: string);
  end;

implementation

end.
