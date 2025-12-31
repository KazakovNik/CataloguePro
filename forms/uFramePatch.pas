unit uFramePatch;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

type
  TFramePatch = class(TFrame)
    lblPatch: TLabel;
    edtPatch: TEdit;
    btnOpenDir: TButton;
  end;

implementation

{$R *.dfm}

end.
