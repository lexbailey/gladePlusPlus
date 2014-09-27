unit defParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type TDefParser = class(TObject)

    private
      FSigs: array of string;
      function GetSigniature(signalName: string):string;

      function LexDefs(defString: string): TStrings;
      procedure ParseDefs(defStrings: TStrings);
    public
      property Signiatures[signalName: string] : string read GetSigniature; default;
      constructor Create(fileName: string);
      destructor Destroy();

  end;

implementation

function TDefParser.getSigniature(signalName: string): string;
begin

end;

function TDefParser.LexDefs(defString: string): TStrings;
var comment, identifier, value, whitespace : TRegExpr;
  output: TStrings;
  done: boolean = false;
  defStringNoCom: string;
begin
  output := TStringList.Create;
  comment := TRegExpr.Create; identifier := TRegExpr.Create; value := TRegExpr.Create; whitespace := TRegExpr.Create;
  comment.Expression:='^;;[^\n]*'; comment.ModifierM := true;
  identifier.Expression:='^[a-zA-Z_\-]+'; identifier.ModifierM := true;
  value.Expression:='^"?[a-zA-Z_\- #\.]+"?'; value.ModifierM := true;
  whitespace.Expression:='^[ \n\t]*'; whitespace.ModifierM := true;

  //Remove all comments
  defStringNoCom := comment.Replace(defString, '', false);

  //writeln(defStringNoCom);

  //TODO
  {
  repeat
    if comment.Exec(defstring) then
    begin

    end;
    done := true;
  until done;
   }

  comment.Free; identifier.Free; value.Free; whitespace.Free;
end;

procedure TDefParser.ParseDefs(defStrings: TStrings);
begin
  //foo
end;

constructor TDefParser.create(fileName: string);
var input: TStrings;
begin
  inherited create;
  input := TStringList.Create;
  input.LoadFromFile(fileName);
  self.ParseDefs(LexDefs(input.Text));
  input.Free;
end;

Destructor TDefParser.Destroy;
begin

end;

end.

