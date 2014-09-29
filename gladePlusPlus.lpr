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
  XMLRead, templates, unixutil, RegExpr, defParser;

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
  sigDefs: TDefParser;

function StringIsIn(const s: string; const a: array of string):boolean;
var  len, i: integer;
begin
  //Return true if an array of strings contains the specified string. False otherwise
  len := length(a);
  result := false;
  for i := 0 to len-1 do begin
    if a[i] = s then begin
      result := true;
      break;
    end;
  end;
end;

function DashToUnderscore(s:string):string;
begin
  //Replace all dashes with underscores
  result := StringReplace(s, '-', '_', [rfReplaceAll]);
end;

function defineInstance(objectClass: string; objectName: string):string;
begin
  //Produce a line of C for defining an instance of a GTK object
  objectClass := copy(objectClass,4,length(objectClass));
  result := format(#9+#9+#9+'Gtk::%s *%s;', [objectClass, objectName]);
end;

function defineSubInstance(parentName: string; objectName: string; objectClass: string):string;
begin
  //Produce a line of C for defining an instance of a class that represents the widgets of a GTK top level
  objectClass := copy(objectClass,4,length(objectClass));
  result := format(#9+#9+#9+'GladePlusPlus%s_%s *%s_widgets;'+#10+#9+#9+#9'Gtk::%s *%s;', [parentName, objectName, objectName, objectClass, objectName]);
end;

function initInstance(objectName: string):string;
begin
  //Produce a line of C for initialising an instance of a GTK object
  result := format(#9+'refBuilder->get_widget("%s", this->%s);', [objectName, objectName]);
end;

function initSubInstance(objectName: string; parentName: string):string;
begin
  //Produce a line of C for initialising an instance of a class that represents the widgets of a GTK top level
  result := format(#9+#9+'this->%s_widgets = new GladePlusPlus%s_%s(refBuilder);'+#10+#9+#9+'refBuilder->get_widget("%s", this->%s);', [objectName, parentName, objectName, objectName, objectName]);
end;

function deleteInstance(objectName: string):string;
begin
  //Produce a line of C for freeing an instance of a GTK object
  result := format(#9+'delete this->%s;', [objectName]);
end;

function declareSignal(signalHandler, signalArgs: string):string;
begin
  //Produce a line of C that declares the signature of a signal handler
  result := format(signalArgs, [signalHandler]);
end;

function connectSignal(objectName, signalName, signalHandler: string):string;
begin
  //Produce a line of C that connects a signal to a signal handler
  result := format(#9+'this->%s->signal_%s().connect(sigc::ptr_fun(&%s));', [objectName, signalName, signalHandler]);
end;

function includeHeader(fileName: string):string;
begin
  //Generate code that includes a header file in a C++ source
  result := format('#include "%s.gpp.hpp"', [fileName]);
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
  //Loop through all items in the top level or parent node
  for i := 0 to TLItems.Count-1 do begin
    //Get the current item
    ThisNode := TLItems[i];
    //Check if this node is a valid child node
    if (ThisNode.NodeName = 'child') and (ThisNode.Attributes.GetNamedItem('internal-child') = nil)then begin
      //Get the first child of the child node, this should be an obect node
      ThisNode := ThisNode.FirstChild;
      if (ThisNode.NodeName = 'object') then begin
        //Get the class of the object node (the widget type)
        ObjectClass := (ThisNode.Attributes.GetNamedItem('class').TextContent);
        //Get the name of the object node (ID)
        ObjectName := DashToUnderscore(ThisNode.Attributes.GetNamedItem('id').TextContent);
        //Generate code to define this widget as a member of this toplevel's C++ class
        thisTLOutHeader.Append(defineInstance(ObjectClass, ObjectName));
        //Generate code to initialise this widget
        thisTLOutSource.Append(initInstance(ObjectName));
        //Generate code to free this widget when the C++ class is deleted
        thisTLSourceCleanup.Append(deleteInstance(ObjectName));
        //Get all of the children of this widget/object node
        children := ThisNode.ChildNodes;
        //Loop trough the,
        for j := 0 to children.Count-1 do begin
          ThisChild := Children[j];
          //Find all signals that need connecting
          if ThisChild.NodeName = 'signal' then begin
            //Get the name of the handler function
            SignalHandler := (ThisChild.Attributes.GetNamedItem('handler').TextContent);
            //Get the name of the signal that is to be connected
            SignalName := DashToUnderscore(ThisChild.Attributes.GetNamedItem('name').TextContent);
            //Generate code to connect this signal
            thisTLOutSource.Append(connectSignal(objectName, signalName, signalHandler));
            //Generate a function signiature for the signal handler
            //signalsHeader.Append(declareSignal(signalHandler, sigDefs.Signiatures[DashToUnderscore(ObjectClass + '.' + SignalName)]));
            signalsHeader.Append(declareSignal(signalHandler, sigDefs.Signiatures[DashToUnderscore(SignalName)]));
          end;
        end;
        //Now search this child for all of its children.
        recurseChildren(children, thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup);
      end;
    end;
  end;
end;

procedure handleTopLevelObject(tlo: TDOMNode; var initCode, declareCode: TStringList);
var
  objClass, objName: string;
  thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup: TStrings;
  thisTLOutHeaderName, thisTLOutSourceName: string;
  thisTLOutHeaderFileName, thisTLOutSourceFileName: string;
  TLItems: TDOMNodeList;
begin
  //This function parses a top level (window, dialog, assistant, etc...)

  //Get the class of the top level
  objClass := tlo.Attributes.GetNamedItem('class').TextContent;
  //Get the name of the top level
  objName := DashToUnderscore(tlo.Attributes.GetNamedItem('id').TextContent);
  //Generate code to declare an instance of the C++ class that represents this top level
  declareCode.Append(defineSubInstance(projName, objName, objClass));
  //Generate code to initialise an instance of the C++ class that represents this top level
  initCode.Append(initSubInstance(objName, projName));
  //Generate code to free an instance of the C++ class that represents this top level
  cleanup.Append(deleteInstance(objName));

  //Generate [unique] names for the header and source files for this C++ class
  thisTLOutHeaderName := projName + '_' + objName;
  thisTLOutSourceName := projName + '_' + objName;

  //Construct the full path to the file
  thisTLOutHeaderFileName := outputFolder + thisTLOutHeaderName + fileExtensionHeader;
  thisTLOutSourceFileName := outputFolder + thisTLOutSourceName + fileExtensionSource;

  //Initialise string lists for generated code
  thisTLOutHeader := TStringList.create;
  thisTLOutSource := TStringList.create;
  thisTLSourceCleanup := TStringList.Create;

  //Generate boiler plate starting section for the header and the source files
  thisTLOutHeader.Append(format(outputTLHeader, [thisTLOutHeaderName,thisTLOutHeaderName,projName,thisTLOutHeaderName]));
  thisTLOutSource.Append(format(outputTLSourceHeader, [thisTLOutHeaderName + fileExtensionHeader,thisTLOutHeaderName,thisTLOutHeaderName]));

  //Get all child nodes
  TLItems := tlo.ChildNodes;
  //Recurse through the children, generate code for handling widgets
  recurseChildren(TLItems, thisTLOutHeader, thisTLOutSource, thisTLSourceCleanup);

  //Generate boiler plate ending section for the header and source files
  thisTLOutHeader.Append(format(outputTLFooter, [thisTLOutHeaderName,thisTLOutHeaderName]));
  thisTLOutHeader.SaveToFile(thisTLOutHeaderFileName);
  thisTLOutSource.Append(format(outputTLSourceMiddle, [thisTLOutHeaderName,thisTLOutHeaderName]));
  thisTLOutSource.AddStrings(thisTLSourceCleanup);
  thisTLOutSource.Append(format(outputTLSourceFooter, []));
  thisTLOutSource.SaveToFile(thisTLOutSourceFileName);

  //Free string lists for generated code
  thisTLOutHeader.Free;
  thisTLOutSource.Free;
  thisTLSourceCleanup.Free;
end;

procedure TGladePlusPlus.DoRun;
var
  ErrorMsg, ObjectClass, ObjectName, xmlPath: String;
  InterfaceNode, ThisNode: TDOMNode;
  InterfaceItems: TDOMNodeList;
  Doc: TXMLDocument;
  i: integer;
  includes, initCode, declareCode: TStringList;
  thisSignalHeaderName, thisSignalHeaderFileName, defsFile:string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hioxd','help,input,outputFolder,xmlPath,signalDefsFile');
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

  //Input must be set
  if not HasOption('i','input') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  //Get the input file name
  inputFileName := GetOptionValue('i','input');

  //Default output folder is this folder
  outputFolder := './';
  if HasOption('o','outputFolder') then begin
    outputFolder := GetOptionValue('o','outputFolder');
  end;

  //XML path defaults to the input file name but can be different
  xmlPath := inputFileName;
  if HasOption('x','xmlPath') then begin
    xmlPath := GetOptionValue('x','xmlPath');
  end;

  //Get the projet name from the input file name
  projName := Basename(inputFileName,'.glade');
  //Generate the name for the main output file name
  mainOutputFileName := outputFolder + projName;

  //Before parsing the glade file, load in the defs file
  defsFile := 'gtk_signals.xml';
  if HasOption('d','signalDefsFile') then begin
    defsFile := GetOptionValue('d','signalDefsFile');
  end;
  writeln('loading defs');
  sigDefs := TDefParser.Create(defsFile);
  writeln('loaded defs');

  try
    //Load the glade file
    ReadXMLFile(Doc, inputFileName);
    //Get the interface node (document/root level node)
    InterfaceNode := Doc.DocumentElement;
    //Quick sanity check
    if (interfaceNode <> nil) and (InterfaceNode.NodeName = 'interface') then
    begin
      //Get all children
      InterfaceItems := InterfaceNode.ChildNodes;

      //Create output buffers
      outputHeader := TStringList.create;
      outputSource := TStringList.create;
      cleanup := TStringList.create;
      includes := TStringList.Create;
      initCode := TStringList.Create;
      declareCode := TStringList.Create;
      signalsHeader := TStringList.Create;

      //Generate name for signal handler source files
      thisSignalHeaderName := projName+'_signals';
      thisSignalHeaderFileName := outputFolder + thisSignalHeaderName + fileExtensionHeader;

      //Generate boiler plate for signal handler code
      signalsHeader.Append(format(templates.signalsHeader, [thisSignalHeaderName, thisSignalHeaderName]));

      //Generate boiler plate for main header and source files
      outputHeader.Append(Format(templates.topLevelHeader, [projName, projName, projName]));
      outputSource.Append(Format(templates.topLevelSourceHeader, [projName + fileExtensionHeader, projName, projName, projName, xmlPath]));

      //Loop through all children
      for i := 0 to InterfaceItems.Count-1 do begin
        //Get this node
        ThisNode := InterfaceItems[i];
        //For any object nodes... (These should all be top level widgets)
        if ThisNode.NodeName = 'object' then begin
          //Get the object class (eg GtkWindow)
          ObjectClass := (ThisNode.Attributes.GetNamedItem('class').TextContent);
          //Get the ID for this top level
          ObjectName := DashToUnderscore(ThisNode.Attributes.GetNamedItem('id').TextContent);
          //Confirm that this is a top level
          if StringIsIn(ObjectClass, topLevels) then begin
            //Generate code to include the source for this top level
            includes.Append(includeHeader(projName+'_'+ObjectName));
            //Generate all code associated with the top level
            handleTopLevelObject(ThisNode, initCode, declareCode);
          end;
        end;
      end;
      //More boiler plate generation
      outputHeader.AddStrings(includes);
      outputHeader.Append(Format(templates.topLevelPostInclude, [projName]));
      outputHeader.AddStrings(declareCode);
      outputSource.AddStrings(initCode);
      outputSource.Append(Format(templates.topLevelSourceMiddle, [projName, projName]));
      outputSource.AddStrings(cleanup);
      outputSource.Append(Format(templates.topLevelSourceFooter, [projName, projName]));
      outputHeader.Append(Format(templates.topLevelFooter, [projName, projName]));

      //Save outputs
      outputHeader.SaveToFile(mainOutputFileName + fileExtensionHeader);
      outputSource.SaveToFile(mainOutputFileName + fileExtensionSource);

      signalsHeader.Append(format(templates.signalsFooter, []));
      signalsHeader.SaveToFile(thisSignalHeaderFileName);

      //Free things
      outputHeader.Free;
      outputSource.Free;
      cleanup.Free;
      includes.Free;
      initCode.Free;
      declareCode.Free;
      signalsHeader.Free;
    end else
    begin
      // if the interface section cannot be found then the file is not valid.
      ShowException(Exception.Create('Error: Invalid input. couldn''t find interface section'));
    end;
  finally
    // finally, free the glade file
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

