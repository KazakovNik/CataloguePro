unit uFramePatch;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

type
  TFrame1 = class(TFrame)
    lblCurrentDirOpen: TLabel;
    edtPatch: TEdit;
    btnOpenDir: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
