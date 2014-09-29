unit defParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM,
  XMLRead;

type

  TSignalSsigniature = record
    FName: string;
    FSigniature: string;
  end;

  TDefParser = class(TObject)

    private
      FSigs: array of TSignalSsigniature;
      function GetSigniature(signalName: string):string;

    public
      property Signiatures[signalName: string] : string read GetSigniature; default;
      constructor Create(fileName: string);
      destructor Destroy();

  end;

implementation

function DashToUnderscore(s:string):string;
begin
  //Replace all dashes with underscores
  result := StringReplace(s, '-', '_', [rfReplaceAll]);
end;

function TDefParser.getSigniature(signalName: string): string;
var i: integer;
begin
  result := '#error signal definition not found for ' + signalName;
  for i := 0 to length(FSigs)-1 do begin
    if FSigs[i].FName = signalName then begin
      result := FSigs[i].FSigniature;
      break;
    end;
  end;
end;


constructor TDefParser.create(fileName: string);
var inputDoc: TXMLDocument;
  signals, thisParams: TDOMNodeList;
  thisSignal: TDOMNode;
  i, j : integer;
  ofobject, name, returnType, signiature: string;
  paramName, paramType: string;
begin
  inherited create;

  try
    //Load the defs file
    ReadXMLFile(inputDoc, fileName);
    //Get the signals-node's children
    signals := inputDoc.DocumentElement.ChildNodes;
    //Make space
    setLength(FSigs, signals.Length);
    //Loop over all signals
    for i := 0 to signals.Length - 1 do begin
      //Get current signal
      thisSignal := signals[i];
      //Get information
      ofobject := thisSignal.Attributes.GetNamedItem('of-object').TextContent;
      name := thisSignal.Attributes.GetNamedItem('name').TextContent;
      returnType := thisSignal.Attributes.GetNamedItem('return-type').TextContent;
      //Add name to list:
      //FSigs[i].FName:= DashToUnderscore(ofobject + '.' + name);
      FSigs[i].FName:= DashToUnderscore(name);
      //Get params
      thisParams := thisSignal.ChildNodes;
      //generate param list
      signiature:= returnType + ' %s(';
      for j := 0 to thisParams.Length - 1 do begin
        paramType:= thisParams[j].Attributes.GetNamedItem('type').TextContent;
        paramName:= thisParams[j].TextContent;
        signiature := signiature + paramType + ' ' + paramName;
        //Generate a semicolon only if this is not the last item
        if (j < thisParams.Length - 1) then
          signiature := signiature + ';';
      end;
      signiature := signiature + ');';

      FSigs[i].FSigniature:=signiature;

    end;
  finally
    inputDoc.Free;
  end;

end;

Destructor TDefParser.Destroy;
begin

end;

end.

