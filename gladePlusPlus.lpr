{%RunCommand $MakeExe($(EdFile)) -i ./../samurai/samuraigui/samgui3_10.glade}
program gladePlusPlus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  DOM,
  XMLRead, templates, unixutil;

type

  { TGladePlusPlus }

  TGladePlusPlus = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGladePlusPlus }

const
  topLevels: array [0..3] of string = ('GtkWindow', 'GtkAssistant', 'GtkDialog', 'GtkFileChooserDialog');

  fileExtensionHeader = '.gpp.hpp';
  fileExtensionSource = '.gpp.cpp';

var
  mainOutputFileName : string;
  outputHeader, outputSource, cleanup: TStrings;
  outputFolder, inputFileName, projName : string;
  signalsHeader: TStrings;

function StringIsIn(const s: string; const a: array of string):boolean;
var  len, i: integer;
begin
  len := length(a);
  result := false;
  for i := 0 to len-1 do begin
    writeln('Testing ' + s + ' agaist ' + a[i]);
    if a[i] = s then begin
      result := true;
      break;
    end;
  end;
end;

function DashToUnderscore(s:string):string;
begin
  result := StringReplace(s, '-', '_', [rfReplaceAll]);
end;

function defineInstance(objectClass: string; objectName: string):string;
begin
  objectClass := copy(objectClass,4,length(objectClass));
  result := format(#9+#9+#9+'Gtk::%s *%s;', [objectClass, objectName]);
end;

function defineSubInstance(parentName: string; objectName: string; objectClass: string):string;
begin
  objectClass := copy(objectClass,4,length(objectClass));
  result := format(#9+#9+#9+'GladePlusPlus%s_%s *%s_widgets;'+#10+#9+#9+#9'Gtk::%s *%s;', [parentName, objectName, objectName, objectClass, objectName]);
end;

function initInstance(objectName: string):string;
begin
result := format(#9+'refBuilder->get_widget("%s", this->%s);', [objectName, objectName]);
end;

function initSubInstance(objectName: string; parentName: string):string;
begin
result := format(#9+#9+'this->%s_widgets = new GladePlusPlus%s_%s(refBuilder);'+#10+#9+#9+'refBuilder->get_widget("%s", this->%s);', [objectName, parentName, objectName, objectName, objectName]);
end;

function deleteInstance(objectName: string):string;
begin
result := format(#9+'delete this->%s;', [objectName]);
end;

function declareSignal(signalHandler, signalArgs: string):string;
begin
  result := format('void %s(%s);', [signalHandler, signalArgs]);
end;

function connectSignal(objectName, signalName, signalHandler: string):string;
begin
  result := format(#9+'this->%s->signal_%s().connect(sigc::ptr_fun(&%s));', [objectName, signalName, signalHandler]);
end;

procedure recurseChildren(const TLItems: TDOMNodeList;
  var thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup: TStrings);
var
  ObjectName: string;
  ObjectClass: string;
  SignalHandler, SignalName: string;
  i,j: integer;
  ThisNode: TDOMNode;
  ThisChild: TDOMNode;
  Children: TDOMNodeList;
begin
  for i := 0 to TLItems.Count-1 do begin
          ThisNode := TLItems[i];
          writeln('Top level child ' + inttostr(i) + ' is "' + ThisNode.NodeName + '"');
          if (ThisNode.NodeName = 'child')then begin
            ThisNode := ThisNode.FirstChild;
            if (ThisNode.NodeName = 'object') then begin
              writeln(ThisNode.TextContent);
              ObjectClass := (ThisNode.Attributes.GetNamedItem('class').TextContent);
              ObjectName := DashToUnderscore(ThisNode.Attributes.GetNamedItem('id').TextContent);
              writeln('This node is an object of class "' + ObjectClass + '"');
              thisTLOutHeader.Append(defineInstance(ObjectClass, ObjectName));
              thisTLOutSource.Append(initInstance(ObjectName));
              thisTLSourceCleanup.Append(deleteInstance(ObjectName));
              children := ThisNode.ChildNodes;
              for j := 0 to children.Count-1 do begin
                ThisChild := Children[j];
                if ThisChild.NodeName = 'signal' then begin
                  SignalHandler := (ThisChild.Attributes.GetNamedItem('handler').TextContent);
                  SignalName := DashToUnderscore(ThisChild.Attributes.GetNamedItem('name').TextContent);
                  thisTLOutSource.Append(connectSignal(objectName, signalName, signalHandler));
                  signalsHeader.Append(declareSignal(signalHandler, ''));
                end;
              end;

              recurseChildren(ThisNode.ChildNodes, thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup);

            end;
          end else
          begin

            writeln('skipping non-object node');
          end;
        end;
end;

procedure handleTopLevelObject(tlo: TDOMNode; var initCode, declareCode: TStringList);
var objClass, objName: string;

  thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup: TStrings;

  thisTLOutHeaderName, thisTLOutSourceName: string;
  thisTLOutHeaderFileName, thisTLOutSourceFileName: string;

  TLItems: TDOMNodeList;



begin
  objClass := tlo.Attributes.GetNamedItem('class').TextContent;
  objName := DashToUnderscore(tlo.Attributes.GetNamedItem('id').TextContent);
  declareCode.Append(defineSubInstance(projName, objName, objClass));

  initCode.Append(initSubInstance(objName, projName));

  cleanup.Append(deleteInstance(objName));

  thisTLOutHeaderName := projName + '_' + objName;
  thisTLOutSourceName := projName + '_' + objName;

  thisTLOutHeaderFileName := outputFolder + thisTLOutHeaderName + fileExtensionHeader;
  thisTLOutSourceFileName := outputFolder + thisTLOutSourceName + fileExtensionSource;




  thisTLOutHeader := TStringList.create;
  thisTLOutSource := TStringList.create;
  thisTLSourceCleanup := TStringList.Create;

  thisTLOutHeader.Append(format(outputTLHeader, [thisTLOutHeaderName,thisTLOutHeaderName,projName,thisTLOutHeaderName]));
  thisTLOutSource.Append(format(outputTLSourceHeader, [thisTLOutHeaderName + fileExtensionHeader,thisTLOutHeaderName,thisTLOutHeaderName]));
  //stuff

  TLItems := tlo.ChildNodes;
  writeln('This top level has ' + inttostr(TLItems.Count) + ' items');


  recurseChildren(TLItems, thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup);

  //End stuff
  thisTLOutHeader.Append(format(outputTLFooter, [thisTLOutHeaderName,thisTLOutHeaderName]));
  thisTLOutHeader.SaveToFile(thisTLOutHeaderFileName);
  thisTLOutSource.Append(format(outputTLSourceMiddle, [thisTLOutHeaderName,thisTLOutHeaderName]));
  thisTLOutSource.AddStrings(thisTLSourceCleanup);
  thisTLOutSource.Append(format(outputTLSourceFooter, []));
  thisTLOutSource.SaveToFile(thisTLOutSourceFileName);



end;

function includeHeader(fileName: string):string;
begin
  result := format('#include "%s.gpp.hpp"', [fileName]);
end;

procedure TGladePlusPlus.DoRun;
var
  ErrorMsg, ObjectClass, ObjectName, xmlPath: String;
  InterfaceNode, ThisNode: TDOMNode;
  InterfaceItems: TDOMNodeList;
  Doc: TXMLDocument;
  i: integer;
  includes, initCode, declareCode: TStringList;
  thisSignalHeaderName, thisSignalHeaderFileName:string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hiox','help,input,outputFolder,xmlPath');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('i','input') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  outputFolder := './';
  if HasOption('o','outputFolder') then begin
    outputFolder := GetOptionValue('o','outputFolder');
  end;


  inputFileName := GetOptionValue('i','input');

  xmlPath := inputFileName;
  if HasOption('x','xmlPath') then begin
    xmlPath := GetOptionValue('x','xmlPath');
  end;

  projName := Basename(inputFileName,'.glade');
  mainOutputFileName := outputFolder + projName;

  writeln('Main output file header: ' + mainOutputFileName + fileExtensionHeader);
  writeln('Main output file source: ' + mainOutputFileName + fileExtensionSource);

  try
    // Read in xml file from disk
    writeln('Input file: ' + inputFileName);
    ReadXMLFile(Doc, inputFileName);
    writeln('File read');
    // Retrieve the "password" node
    InterfaceNode := Doc.DocumentElement;
    if (interfaceNode <> nil) and (InterfaceNode.NodeName = 'interface') then
    begin
      writeln('Found interface section');
      InterfaceItems := InterfaceNode.ChildNodes;
      writeln('Iterface has ' + inttostr(InterfaceItems.Count) + ' items');

      //Create output buffers
      outputHeader := TStringList.create;
      outputSource := TStringList.create;
      cleanup := TStringList.create;
      includes := TStringList.Create;
      initCode := TStringList.Create;
      declareCode := TStringList.Create;
      signalsHeader := TStringList.Create;


      thisSignalHeaderName := projName+'_signals';
      thisSignalHeaderFileName := outputFolder + thisSignalHeaderName + fileExtensionHeader;
      signalsHeader.Append(format(templates.signalsHeader, [thisSignalHeaderName, thisSignalHeaderName]));

      outputHeader.Append(Format(templates.topLevelHeader, [projName, projName, projName]));
      outputSource.Append(Format(templates.topLevelSourceHeader, [projName + fileExtensionHeader, projName, projName, projName, xmlPath]));



      for i := 0 to InterfaceItems.Count-1 do begin
        ThisNode := InterfaceItems[i];
        writeln('Object ' + inttostr(i) + ' is "' + ThisNode.NodeName + '"');
        if ThisNode.NodeName = 'object' then begin
          ObjectClass := (ThisNode.Attributes.GetNamedItem('class').TextContent);
          ObjectName := DashToUnderscore(ThisNode.Attributes.GetNamedItem('id').TextContent);

          writeln('This node is an object of class "' + ObjectClass + '"');
          if StringIsIn(ObjectClass, topLevels) then begin
            writeln('This is a top level object');
            includes.Append(includeHeader(projName+'_'+ObjectName));
            handleTopLevelObject(ThisNode, initCode, declareCode);
          end;
        end else
        begin
          writeln('skipping non-object node');
        end;
      end;
      outputHeader.AddStrings(includes);
      outputHeader.Append(Format(templates.topLevelPostInclude, [projName]));
      outputHeader.AddStrings(declareCode);
      outputSource.AddStrings(initCode);
      outputSource.Append(Format(templates.topLevelSourceMiddle, [projName, projName]));
      outputSource.AddStrings(cleanup);
      outputSource.Append(Format(templates.topLevelSourceFooter, [projName, projName]));

      outputHeader.Append(Format(templates.topLevelFooter, [projName, projName]));
      outputHeader.SaveToFile(mainOutputFileName + fileExtensionHeader);
      outputSource.SaveToFile(mainOutputFileName + fileExtensionSource);

      signalsHeader.Append(format(templates.signalsFooter, []));
      signalsHeader.SaveToFile(thisSignalHeaderFileName);

      outputHeader.Free;
      outputSource.Free;
    end else
    begin
      ShowException(Exception.Create('Error: Invalid input. couldn''t find interface section'));
    end;
  finally
    // finally, free the document
    Doc.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TGladePlusPlus.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGladePlusPlus.Destroy;
begin
  inherited Destroy;
end;

procedure TGladePlusPlus.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' [options] -i <input-file-name>');
  writeln('    Options:');
  writeln('        -o, --outputFolder <folder-name>    Specify the output folder name. Default is ''.''');
end;

var
  Application: TGladePlusPlus;
begin
  Application:=TGladePlusPlus.Create(nil);
  Application.Title:='Glade Plus Plus';
  Application.Run;
  Application.Free;
end.

